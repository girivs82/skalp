/**
 * SKALP DAP Debug Session — translates DAP protocol ↔ skalp-debug JSON lines.
 *
 * DAP Concept Mapping:
 *   Thread       → Single thread (Thread 1 = simulation)
 *   Step Over    → Advance 1 clock cycle
 *   Step In      → Step 1 half-cycle
 *   Continue     → Run until breakpoint or max cycles
 *   Pause        → Stop after current cycle
 *   Stack Frame  → Module hierarchy
 *   Scopes       → Inputs / Outputs / Registers / Signals
 *   Breakpoint   → Signal watchpoint (line → signal via source map)
 */

import {
    DebugSession,
    InitializedEvent,
    StoppedEvent,
    TerminatedEvent,
    Thread,
    StackFrame,
    Scope,
    Variable,
    Source,
    OutputEvent,
    Breakpoint,
} from '@vscode/debugadapter';
import { DebugProtocol } from '@vscode/debugprotocol';
import * as vscode from 'vscode';
import * as path from 'path';
import { SkalpDebugServer, DebugServerEvent } from './server-connection';
import { InlineValueDecorator } from './inline-values';

// Variable reference IDs for scopes
const SCOPE_INPUTS = 1;
const SCOPE_OUTPUTS = 2;
const SCOPE_REGISTERS = 3;
const SCOPE_SIGNALS = 4;

const SCOPE_MAP: Record<number, string> = {
    [SCOPE_INPUTS]: 'inputs',
    [SCOPE_OUTPUTS]: 'outputs',
    [SCOPE_REGISTERS]: 'registers',
    [SCOPE_SIGNALS]: 'signals',
};

interface LaunchRequestArguments extends DebugProtocol.LaunchRequestArguments {
    program: string;
    stopOnEntry?: boolean;
    level?: string;
    maxCycles?: number;
    topModule?: string;
}

export class SkalpDebugSession extends DebugSession {
    private server: SkalpDebugServer | null = null;
    private sourceFile: string = '';
    private currentCycle: number = 0;
    private variableCache: Map<string, any[]> = new Map();
    private extensionPath: string = '';
    private inlineDecorator: InlineValueDecorator = new InlineValueDecorator();
    // Callback to post messages to the waveform viewer (set by adapter-factory)
    private waveformPostMessage: ((message: any) => void) | null = null;
    // Source map: line number → signal name (for breakpoint resolution)
    private sourceMap: Map<number, string> = new Map();
    // Reverse source map: signal name → first line (for highlight on breakpoint hit)
    private signalToLine: Map<string, number> = new Map();
    // Active gutter breakpoints: line → server breakpoint ID (for removal)
    private activeBreakpointIds: Map<number, number> = new Map();
    // Reverse: server breakpoint ID → line (for highlight on hit)
    private bpIdToLine: Map<number, number> = new Map();
    // Line of the entity declaration (neutral highlight anchor)
    private entityLine: number = 1;
    // Current highlight line (updated on breakpoint hit)
    private highlightLine: number = 1;

    public constructor() {
        super();
        this.setDebuggerColumnsStartAt1(true);
        this.setDebuggerLinesStartAt1(true);
    }

    public setExtensionPath(p: string): void {
        this.extensionPath = p;
    }

    public setWaveformCallback(callback: ((message: any) => void) | null): void {
        this.waveformPostMessage = callback;
    }

    // -----------------------------------------------------------------
    // DAP: Initialize
    // -----------------------------------------------------------------

    protected initializeRequest(
        response: DebugProtocol.InitializeResponse,
        _args: DebugProtocol.InitializeRequestArguments
    ): void {
        response.body = response.body || {};
        response.body.supportsConfigurationDoneRequest = true;
        response.body.supportsEvaluateForHovers = true;
        response.body.supportsStepBack = true;
        response.body.supportsSetVariable = false;
        response.body.supportsRestartRequest = false;
        response.body.supportTerminateDebuggee = true;
        response.body.supportsBreakpointLocationsRequest = false;
        response.body.supportsConditionalBreakpoints = true;

        this.sendResponse(response);
        // NOTE: InitializedEvent is sent in launchRequest AFTER the source map
        // is populated, so that setBreakpoints sees a populated sourceMap.
    }

    // -----------------------------------------------------------------
    // DAP: Launch
    // -----------------------------------------------------------------

    protected async launchRequest(
        response: DebugProtocol.LaunchResponse,
        args: LaunchRequestArguments
    ): Promise<void> {
        console.log('[SKALP Debug] launchRequest:', args.program, 'level:', args.level);
        this.sourceFile = args.program;

        // Start the skalp-debug subprocess
        this.server = new SkalpDebugServer(this.extensionPath);
        const workingDir = path.dirname(this.sourceFile);

        this.server.on('event', (event: DebugServerEvent) => {
            this.handleServerEvent(event);
        });

        this.server.on('log', (msg: string) => {
            this.sendEvent(new OutputEvent(msg, 'console'));
        });

        this.server.on('exit', (code: number | null) => {
            this.sendEvent(new TerminatedEvent());
        });

        this.server.on('error', (err: Error) => {
            this.sendEvent(new OutputEvent(`Debug server error: ${err.message}\n`, 'stderr'));
            this.sendEvent(new TerminatedEvent());
        });

        this.server.start(workingDir);

        // Send launch command
        try {
            this.server.sendCommand({
                cmd: 'launch',
                file: this.sourceFile,
                level: args.level || 'behavioral',
                top_module: args.topModule || null,
                max_cycles: args.maxCycles || 100000,
            });

            // Wait for initialized event
            const initEvent = await this.server.waitForEvent('initialized', 60000);

            // Populate source map from initialized event (before VSCode sends setBreakPointsRequest)
            if (initEvent.source_map) {
                this.sourceMap.clear();
                this.signalToLine.clear();
                for (const m of initEvent.source_map) {
                    this.sourceMap.set(m.line, m.signal);
                    // Reverse map: keep first occurrence (declaration line)
                    if (!this.signalToLine.has(m.signal)) {
                        this.signalToLine.set(m.signal, m.line);
                    }
                }
            }

            // Entity declaration line (for neutral highlight)
            if (initEvent.entity_line) {
                this.entityLine = initEvent.entity_line;
            }
            this.highlightLine = this.entityLine;

            // Capture signal names and widths for waveform viewer initialization
            this.signalWidths = initEvent.signal_widths || {};
            if (Object.keys(this.signalWidths).length === 0 && initEvent.signals) {
                // Fallback: if server didn't send widths, use names with width 1
                for (const name of initEvent.signals) {
                    this.signalWidths[name] = 1;
                }
            }

            this.sendEvent(new OutputEvent(
                `Design loaded: ${initEvent.inputs?.length || 0} inputs, ` +
                `${initEvent.outputs?.length || 0} outputs, ` +
                `${initEvent.signals?.length || 0} signals, ` +
                `${this.sourceMap.size} breakpoint locations\n`,
                'console'
            ));
        } catch (e: any) {
            this.sendEvent(new OutputEvent(`Launch failed: ${e.message}\n`, 'stderr'));
            this.sendEvent(new TerminatedEvent());
        }

        this.sendResponse(response);

        // Signal VSCode that we're ready to accept breakpoints.
        // Sent here (after source map is populated) so setBreakPointsRequest
        // can resolve line→signal mappings.
        this.sendEvent(new InitializedEvent());

        // Auto-open waveform viewer alongside the source during debug
        this.openDebugWaveform();

        // If stopOnEntry, emit a stopped event so the debugger pauses
        if (args.stopOnEntry !== false) {
            this.sendEvent(new StoppedEvent('entry', 1));
        }
    }

    // -----------------------------------------------------------------
    // DAP: Configuration Done
    // -----------------------------------------------------------------

    protected configurationDoneRequest(
        response: DebugProtocol.ConfigurationDoneResponse,
        _args: DebugProtocol.ConfigurationDoneArguments
    ): void {
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Threads
    // -----------------------------------------------------------------

    protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {
        response.body = {
            threads: [new Thread(1, 'Simulation')],
        };
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Stack Trace (module hierarchy as frames)
    // -----------------------------------------------------------------

    protected stackTraceRequest(
        response: DebugProtocol.StackTraceResponse,
        _args: DebugProtocol.StackTraceArguments
    ): void {
        const source = new Source(
            path.basename(this.sourceFile),
            this.sourceFile
        );

        response.body = {
            stackFrames: [
                new StackFrame(
                    0,
                    `cycle ${this.currentCycle}`,
                    source,
                    this.highlightLine
                ),
            ],
            totalFrames: 1,
        };
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Scopes
    // -----------------------------------------------------------------

    protected scopesRequest(
        response: DebugProtocol.ScopesResponse,
        _args: DebugProtocol.ScopesArguments
    ): void {
        response.body = {
            scopes: [
                new Scope('Inputs', SCOPE_INPUTS, false),
                new Scope('Outputs', SCOPE_OUTPUTS, false),
                new Scope('Registers', SCOPE_REGISTERS, false),
                new Scope('Signals', SCOPE_SIGNALS, false),
            ],
        };
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Variables
    // -----------------------------------------------------------------

    protected async variablesRequest(
        response: DebugProtocol.VariablesResponse,
        args: DebugProtocol.VariablesArguments
    ): Promise<void> {
        const scopeName = SCOPE_MAP[args.variablesReference];
        if (!scopeName || !this.server) {
            response.body = { variables: [] };
            this.sendResponse(response);
            return;
        }

        try {
            this.server.sendCommand({ cmd: 'get_variables', scope: scopeName });
            const event = await this.server.waitForEvent('variables', 5000);

            const variables: Variable[] = (event.vars || []).map((v: any) => {
                return new Variable(v.name, v.value, 0);
            });

            response.body = { variables };
        } catch {
            response.body = { variables: [] };
        }

        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Continue
    // -----------------------------------------------------------------

    protected continueRequest(
        response: DebugProtocol.ContinueResponse,
        _args: DebugProtocol.ContinueArguments
    ): void {
        if (this.server) {
            this.server.sendCommand({ cmd: 'continue' });
        }
        response.body = { allThreadsContinued: true };
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Next (step over = 1 cycle)
    // -----------------------------------------------------------------

    protected nextRequest(
        response: DebugProtocol.NextResponse,
        _args: DebugProtocol.NextArguments
    ): void {
        if (this.server) {
            this.server.sendCommand({ cmd: 'step', granularity: 'cycle' });
        }
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Step In (= half cycle)
    // -----------------------------------------------------------------

    protected stepInRequest(
        response: DebugProtocol.StepInResponse,
        _args: DebugProtocol.StepInArguments
    ): void {
        if (this.server) {
            this.server.sendCommand({ cmd: 'step', granularity: 'half_cycle' });
        }
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Pause
    // -----------------------------------------------------------------

    protected pauseRequest(
        response: DebugProtocol.PauseResponse,
        _args: DebugProtocol.PauseArguments
    ): void {
        if (this.server) {
            this.server.sendCommand({ cmd: 'pause' });
        }
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Set Breakpoints
    // -----------------------------------------------------------------

    protected async setBreakPointsRequest(
        response: DebugProtocol.SetBreakpointsResponse,
        args: DebugProtocol.SetBreakpointsArguments
    ): Promise<void> {
        const breakpoints: Breakpoint[] = [];
        const requestedLines = new Set<number>(
            (args.breakpoints || []).map(bp => bp.line)
        );

        // Remove all existing breakpoints — DAP sends the full set each time,
        // and conditions may have changed, so always recreate from scratch
        if (this.server) {
            for (const [_line, bpId] of this.activeBreakpointIds) {
                this.server.sendCommand({
                    cmd: 'remove_breakpoint',
                    id: bpId,
                });
            }
        }

        // Track which lines are now active
        const newActiveIds = new Map<number, number>();

        if (this.server && args.breakpoints) {
            for (const bp of args.breakpoints) {
                // Try to resolve line → signal via source map
                const signalName = this.sourceMap.get(bp.line);

                if (signalName) {
                    // Parse condition string from gutter breakpoint
                    const parsed = this.parseBreakpointCondition(bp.condition);

                    this.server.sendCommand({
                        cmd: 'set_breakpoint',
                        signal: signalName,
                        condition: parsed.condition,
                        value: parsed.value,
                    });

                    try {
                        const event = await this.server.waitForEvent('breakpoint_set', 3000);
                        const bpId = event.id as number;
                        newActiveIds.set(bp.line, bpId);
                        const dbp = new Breakpoint(true, bp.line);
                        dbp.setId(bpId);
                        breakpoints.push(dbp);
                    } catch {
                        breakpoints.push(new Breakpoint(false, bp.line));
                    }
                } else {
                    // No source map entry — mark as unverified
                    // The user can set signal breakpoints via the debug console
                    breakpoints.push(new Breakpoint(false, bp.line));
                }
            }
        }

        this.activeBreakpointIds = newActiveIds;
        // Build reverse map: BP ID → line (for highlight on hit)
        this.bpIdToLine.clear();
        for (const [line, bpId] of newActiveIds) {
            this.bpIdToLine.set(bpId, line);
        }
        response.body = { breakpoints };
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Evaluate (debug console + hover)
    // -----------------------------------------------------------------

    protected async evaluateRequest(
        response: DebugProtocol.EvaluateResponse,
        args: DebugProtocol.EvaluateArguments
    ): Promise<void> {
        if (!this.server) {
            response.body = { result: 'No debug session', variablesReference: 0 };
            this.sendResponse(response);
            return;
        }

        const expr = args.expression.trim();

        // Handle "break <signal> [condition]" command in debug console
        if (expr.startsWith('break ')) {
            const parts = expr.substring(6).trim().split(/\s+/);
            const signal = parts[0];
            const condition = parts[1] || 'any_change';
            const value = parts[2] ? parseInt(parts[2], 10) : undefined;

            this.server.sendCommand({
                cmd: 'set_breakpoint',
                signal,
                condition,
                value,
            });

            try {
                const event = await this.server.waitForEvent('breakpoint_set', 3000);
                response.body = {
                    result: `Breakpoint ${event.id} set on ${signal} (${condition})`,
                    variablesReference: 0,
                };
            } catch {
                response.body = { result: `Failed to set breakpoint on ${signal}`, variablesReference: 0 };
            }

            this.sendResponse(response);
            return;
        }

        // Handle "set <signal> <value>" command
        if (expr.startsWith('set ')) {
            const parts = expr.substring(4).trim().split(/\s+/);
            if (parts.length >= 2) {
                const name = parts[0];
                const value = parseInt(parts[1], 10);
                this.server.sendCommand({ cmd: 'set_input', name, value });
                response.body = { result: `Set ${name} = ${value}`, variablesReference: 0 };
                this.sendResponse(response);
                return;
            }
        }

        // Default: evaluate as signal name lookup
        this.server.sendCommand({ cmd: 'evaluate', expr });
        try {
            const event = await this.server.waitForEvent('evaluate', 3000);
            const result = event.result;

            if (typeof result === 'object' && result.type !== 'error') {
                // Rich result from server — format for hover vs console
                const isHover = args.context === 'hover';
                if (isHover) {
                    // Multi-line hover tooltip
                    const lines: string[] = [];
                    lines.push(`${result.result}`);
                    if (result.width !== undefined) {
                        lines.push(`  width: ${result.width}`);
                    }
                    if (result.decimal !== undefined) {
                        lines.push(`  dec: ${result.decimal}`);
                    }
                    if (result.binary !== undefined) {
                        // Truncate long binary strings
                        const bin = result.binary.length > 32
                            ? result.binary.substring(0, 32) + '...'
                            : result.binary;
                        lines.push(`  bin: ${bin}`);
                    }
                    if (result.changed && result.previous !== undefined) {
                        lines.push(`  prev: ${result.previous}`);
                    }
                    response.body = {
                        result: lines.join('\n'),
                        variablesReference: 0,
                    };
                } else {
                    // Console: single-line
                    response.body = {
                        result: result.result || JSON.stringify(result),
                        variablesReference: 0,
                    };
                }
            } else {
                response.body = {
                    result: typeof result === 'object' ? result.result || JSON.stringify(result) : String(result),
                    variablesReference: 0,
                };
            }
        } catch {
            response.body = { result: 'evaluation timeout', variablesReference: 0 };
        }

        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Step Back
    // -----------------------------------------------------------------

    protected stepBackRequest(
        response: DebugProtocol.StepBackResponse,
        _args: DebugProtocol.StepBackArguments
    ): void {
        if (this.server) {
            this.server.sendCommand({ cmd: 'step_back' });
        }
        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // Breakpoint condition parsing
    // -----------------------------------------------------------------

    /**
     * Parse a user-supplied breakpoint condition string into the server's
     * condition/value format.
     *
     * Supported formats:
     *   rising_edge / rising   → { condition: 'rising_edge' }
     *   falling_edge / falling → { condition: 'falling_edge' }
     *   == N                   → { condition: 'equals', value: N }
     *   != N                   → { condition: 'not_equals', value: N }
     *   > N                    → { condition: 'greater_than', value: N }
     *   < N                    → { condition: 'less_than', value: N }
     *   empty / absent         → { condition: 'any_change' }
     */
    private parseBreakpointCondition(conditionStr?: string): { condition: string; value?: number } {
        if (!conditionStr || conditionStr.trim() === '') {
            return { condition: 'any_change' };
        }

        const cond = conditionStr.trim().toLowerCase();

        if (cond === 'rising_edge' || cond === 'rising') {
            return { condition: 'rising_edge' };
        }
        if (cond === 'falling_edge' || cond === 'falling') {
            return { condition: 'falling_edge' };
        }
        if (cond === 'any_change' || cond === 'change') {
            return { condition: 'any_change' };
        }
        if (cond === 'non_zero' || cond === 'nonzero') {
            return { condition: 'non_zero' };
        }

        // Comparison operators: == != > <
        let m: RegExpMatchArray | null;
        m = conditionStr.trim().match(/^==\s*(\d+)$/);
        if (m) { return { condition: 'equals', value: parseInt(m[1], 10) }; }

        m = conditionStr.trim().match(/^!=\s*(\d+)$/);
        if (m) { return { condition: 'not_equals', value: parseInt(m[1], 10) }; }

        m = conditionStr.trim().match(/^>\s*(\d+)$/);
        if (m) { return { condition: 'greater_than', value: parseInt(m[1], 10) }; }

        m = conditionStr.trim().match(/^<\s*(\d+)$/);
        if (m) { return { condition: 'less_than', value: parseInt(m[1], 10) }; }

        // Hex values: == 0xFF
        m = conditionStr.trim().match(/^==\s*0x([0-9a-fA-F]+)$/);
        if (m) { return { condition: 'equals', value: parseInt(m[1], 16) }; }

        m = conditionStr.trim().match(/^!=\s*0x([0-9a-fA-F]+)$/);
        if (m) { return { condition: 'not_equals', value: parseInt(m[1], 16) }; }

        m = conditionStr.trim().match(/^>\s*0x([0-9a-fA-F]+)$/);
        if (m) { return { condition: 'greater_than', value: parseInt(m[1], 16) }; }

        m = conditionStr.trim().match(/^<\s*0x([0-9a-fA-F]+)$/);
        if (m) { return { condition: 'less_than', value: parseInt(m[1], 16) }; }

        // Fallback: pass as-is (the server's BreakpointCondition::parse handles it)
        return { condition: conditionStr.trim() };
    }

    // -----------------------------------------------------------------
    // Server event handler
    // -----------------------------------------------------------------

    private handleServerEvent(event: DebugServerEvent): void {
        switch (event.event) {
            case 'stopped': {
                this.currentCycle = event.cycle || 0;
                this.variableCache.clear();

                let reason = event.reason || 'step';
                // Map server reasons to DAP stop reasons
                if (reason === 'max_cycles') { reason = 'step'; }

                // Collect all hit breakpoint IDs (for DAP hitBreakpointIds)
                const allHits: Array<{bp_id: number; signal: string; value: string}> =
                    event.hits || [];
                const hitBpIds: number[] = [];
                for (const h of allHits) {
                    if (h.bp_id !== undefined) {
                        hitBpIds.push(h.bp_id);
                    }
                }

                if (reason === 'breakpoint' && allHits.length > 0) {
                    // Continue stopped by breakpoint: highlight the first hit's line
                    const firstBpId = allHits[0].bp_id;
                    if (firstBpId !== undefined) {
                        const line = this.bpIdToLine.get(firstBpId);
                        if (line !== undefined) {
                            this.highlightLine = line;
                        }
                    }
                    // Fallback for console-set breakpoints
                    if (this.highlightLine === this.entityLine && event.hit?.signal) {
                        const sigLine = this.signalToLine.get(event.hit.signal);
                        if (sigLine !== undefined) {
                            this.highlightLine = sigLine;
                        }
                    }
                } else {
                    // Step/pause/max_cycles: neutral anchor at entity declaration
                    this.highlightLine = this.entityLine;
                }

                const stoppedEvent = new StoppedEvent(reason, 1);
                // Tell VSCode which breakpoints were hit (highlights them in gutter)
                if (reason === 'breakpoint' && hitBpIds.length > 0) {
                    (stoppedEvent as DebugProtocol.StoppedEvent).body.hitBreakpointIds = hitBpIds;
                }
                this.sendEvent(stoppedEvent);

                // Console output
                if (allHits.length > 0) {
                    const parts = allHits.map(h => `${h.signal} = ${h.value}`);
                    const prefix = reason === 'breakpoint' ? 'breakpoint' : 'step';
                    this.sendEvent(new OutputEvent(
                        `[cycle ${this.currentCycle}] ${prefix} — ${parts.join(', ')}\n`,
                        'console'
                    ));
                } else {
                    this.sendEvent(new OutputEvent(
                        `[cycle ${this.currentCycle}] ${reason}\n`,
                        'console'
                    ));
                }

                // Request inline values for decoration
                this.requestInlineValues();

                // Sync waveform viewer to current cycle
                this.syncWaveformToCycle(event);
                break;
            }

            case 'continued':
                // Clear inline decorations when simulation resumes
                this.inlineDecorator.clear();
                break;

            case 'terminated':
                this.inlineDecorator.clear();
                this.sendEvent(new TerminatedEvent());
                break;

            case 'error':
                this.sendEvent(new OutputEvent(
                    `Error: ${event.message}\n`,
                    'stderr'
                ));
                break;

            case 'source_map':
                // Update source map from server
                if (event.mappings) {
                    this.sourceMap.clear();
                    for (const m of event.mappings) {
                        this.sourceMap.set(m.line, m.signal);
                    }
                }
                break;

            case 'inline_values':
                // Apply inline value decorations from server response
                if (event.file && event.values) {
                    this.inlineDecorator.applyValues(event.file, event.values);
                }
                break;
        }
    }

    // -----------------------------------------------------------------
    // Inline value decorations
    // -----------------------------------------------------------------

    private requestInlineValues(): void {
        if (!this.server || !this.sourceFile) { return; }
        this.server.sendCommand({
            cmd: 'get_inline_values',
            file: this.sourceFile,
        });
        // Response handled asynchronously via 'inline_values' event in handleServerEvent
    }

    // -----------------------------------------------------------------
    // Live waveform sync
    // -----------------------------------------------------------------

    private syncWaveformToCycle(event: DebugServerEvent): void {
        if (!this.waveformPostMessage) { return; }

        const waveformTime = event.waveform_time ?? this.currentCycle;

        // Send incremental waveform data (signal changes at this time point)
        if (event.waveform_changes) {
            // Use signal_widths from stopped event if available, else fall back
            const widths = event.signal_widths || this.signalWidths;
            // Also update our cached widths from server
            if (event.signal_widths) {
                this.signalWidths = event.signal_widths;
            }
            this.waveformPostMessage({
                type: 'addChanges',
                changes: event.waveform_changes,
                time: waveformTime,
                signalWidths: widths,
            });
        }

        // Jump waveform cursor to current cycle's waveform time
        this.waveformPostMessage({
            type: 'jumpToCycle',
            cycle: waveformTime,
        });

        // If this was a breakpoint hit, add a marker
        if (event.reason === 'breakpoint' && event.hit) {
            this.waveformPostMessage({
                type: 'addBreakpointMarker',
                cycle: waveformTime,
                label: `BP: ${event.hit.signal}`,
            });
        }
    }

    // -----------------------------------------------------------------
    // Waveform viewer auto-open
    // -----------------------------------------------------------------

    // Track known signal names and widths for waveform initialization
    private signalWidths: Record<string, number> = {};

    private openDebugWaveform(): void {
        // Create a minimal stub .debug.skw file so the waveform viewer can open it.
        // Live data will be streamed via addChanges messages on each stopped event.
        const skwPath = this.sourceFile.replace(/\.(sk|skalp)$/, '.debug.skw');
        const fs = require('fs');

        // Build empty waveform structure from signal names + widths
        const signals = Object.entries(this.signalWidths).map(([name, width]) => ({
            name,
            width,
            type: width === 1 ? 'bit' : 'nat',
        }));
        const changes: Record<string, any[]> = {};
        for (const name of Object.keys(this.signalWidths)) {
            changes[name] = [];
        }
        const stubData = {
            version: 1,
            design: path.basename(this.sourceFile, path.extname(this.sourceFile)),
            timescale: '1ns',
            endTime: 100,
            simCycles: 0,
            signals,
            changes,
            annotations: [],
            displayConfig: { defaultRadix: 'hex', signalGroups: [] },
        };

        try {
            fs.writeFileSync(skwPath, JSON.stringify(stubData));
        } catch {
            return; // Can't write stub file — skip waveform
        }

        const uri = vscode.Uri.file(skwPath);
        vscode.commands.executeCommand(
            'vscode.openWith',
            uri,
            'skalp.waveformViewer',
            vscode.ViewColumn.Beside
        ).then(undefined, () => {
            // Silently ignore if waveform viewer can't open
        });
    }

    // -----------------------------------------------------------------
    // Cleanup
    // -----------------------------------------------------------------

    protected disconnectRequest(
        response: DebugProtocol.DisconnectResponse,
        _args: DebugProtocol.DisconnectArguments
    ): void {
        this.inlineDecorator.dispose();
        if (this.server) {
            this.server.stop();
            this.server = null;
        }
        this.sendResponse(response);
    }
}

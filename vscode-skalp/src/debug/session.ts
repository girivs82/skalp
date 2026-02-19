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
import * as path from 'path';
import { SkalpDebugServer, DebugServerEvent } from './server-connection';

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
    // Source map: line number → signal name (for breakpoint resolution)
    private sourceMap: Map<number, string> = new Map();
    // Active gutter breakpoints: line → server breakpoint ID (for removal)
    private activeBreakpointIds: Map<number, number> = new Map();

    public constructor() {
        super();
        this.setDebuggerColumnsStartAt1(true);
        this.setDebuggerLinesStartAt1(true);
    }

    public setExtensionPath(p: string): void {
        this.extensionPath = p;
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
        response.body.supportsStepBack = false;
        response.body.supportsSetVariable = false;
        response.body.supportsRestartRequest = false;
        response.body.supportTerminateDebuggee = true;
        response.body.supportsBreakpointLocationsRequest = false;

        this.sendResponse(response);
        this.sendEvent(new InitializedEvent());
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
                for (const m of initEvent.source_map) {
                    this.sourceMap.set(m.line, m.signal);
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
                    1  // line 1 (we don't have exact line mapping yet)
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

        // Remove breakpoints that are no longer in the active set
        if (this.server) {
            for (const [line, bpId] of this.activeBreakpointIds) {
                if (!requestedLines.has(line)) {
                    this.server.sendCommand({
                        cmd: 'remove_breakpoint',
                        id: bpId,
                    });
                }
            }
        }

        // Track which lines are now active
        const newActiveIds = new Map<number, number>();

        if (this.server && args.breakpoints) {
            for (const bp of args.breakpoints) {
                // If this line already has an active breakpoint, keep it
                const existingId = this.activeBreakpointIds.get(bp.line);
                if (existingId !== undefined && requestedLines.has(bp.line)) {
                    newActiveIds.set(bp.line, existingId);
                    breakpoints.push(new Breakpoint(true, bp.line));
                    continue;
                }

                // Try to resolve line → signal via source map
                const signalName = this.sourceMap.get(bp.line);

                if (signalName) {
                    // Register AnyChange watchpoint on the resolved signal
                    this.server.sendCommand({
                        cmd: 'set_breakpoint',
                        signal: signalName,
                        condition: 'any_change',
                    });

                    try {
                        const event = await this.server.waitForEvent('breakpoint_set', 3000);
                        const bpId = event.id as number;
                        newActiveIds.set(bp.line, bpId);
                        breakpoints.push(new Breakpoint(true, bp.line));
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
            response.body = {
                result: typeof result === 'object' ? result.result || JSON.stringify(result) : String(result),
                variablesReference: 0,
            };
        } catch {
            response.body = { result: 'evaluation timeout', variablesReference: 0 };
        }

        this.sendResponse(response);
    }

    // -----------------------------------------------------------------
    // DAP: Disconnect
    // -----------------------------------------------------------------

    protected disconnectRequest(
        response: DebugProtocol.DisconnectResponse,
        _args: DebugProtocol.DisconnectArguments
    ): void {
        if (this.server) {
            this.server.stop();
            this.server = null;
        }
        this.sendResponse(response);
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

                const stoppedEvent = new StoppedEvent(reason, 1);
                this.sendEvent(stoppedEvent);

                // Output cycle info to debug console
                const hitInfo = event.hit
                    ? ` — ${event.hit.signal} = ${event.hit.value}`
                    : '';
                this.sendEvent(new OutputEvent(
                    `[cycle ${this.currentCycle}] ${reason}${hitInfo}\n`,
                    'console'
                ));
                break;
            }

            case 'continued':
                // Nothing to do — DAP already knows we're running
                break;

            case 'terminated':
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
        }
    }
}

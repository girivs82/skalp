import * as vscode from 'vscode';
import * as path from 'path';

export class SchematicViewerProvider {
    private context: vscode.ExtensionContext;
    private panel: vscode.WebviewPanel | null = null;
    private currentFilePath: string = '';

    constructor(context: vscode.ExtensionContext) {
        this.context = context;
    }

    show(): void {
        const editor = vscode.window.activeTextEditor;
        if (!editor || !editor.document.fileName.endsWith('.sk')) {
            vscode.window.showWarningMessage('Open a .sk file to view its schematic.');
            return;
        }

        if (this.panel) {
            this.panel.reveal(vscode.ViewColumn.Beside);
        } else {
            this.panel = vscode.window.createWebviewPanel(
                'skalpSchematic',
                'SKALP Schematic',
                vscode.ViewColumn.Beside,
                {
                    enableScripts: true,
                    retainContextWhenHidden: true,
                    localResourceRoots: [
                        vscode.Uri.file(path.join(this.context.extensionPath, 'media')),
                        vscode.Uri.file(path.join(this.context.extensionPath, 'node_modules', 'elkjs', 'lib'))
                    ]
                }
            );

            this.panel.webview.html = this.getHtml(this.panel.webview);

            this.panel.webview.onDidReceiveMessage((msg) => {
                if (msg.type === 'navigateToEntity') {
                    this.navigateToEntity(msg.entityType);
                } else if (msg.type === 'navigateToLine') {
                    this.navigateToLine(msg.filePath, msg.line);
                }
            });

            this.panel.onDidDispose(() => {
                this.panel = null;
            });
        }

        this.currentFilePath = editor.document.fileName;
        this.updateSchematic(editor);

        // Update on cursor move (debounced)
        let timeout: ReturnType<typeof setTimeout> | null = null;
        const disposable = vscode.window.onDidChangeTextEditorSelection((e) => {
            if (e.textEditor.document.fileName.endsWith('.sk')) {
                this.currentFilePath = e.textEditor.document.fileName;
                if (timeout) { clearTimeout(timeout); }
                timeout = setTimeout(() => this.updateSchematic(e.textEditor), 150);
            }
        });

        this.panel.onDidDispose(() => {
            disposable.dispose();
            if (timeout) { clearTimeout(timeout); }
        });
    }

    private async navigateToEntity(entityType: string): Promise<void> {
        if (!this.currentFilePath) { return; }

        const pattern = new RegExp(`^\\s*(pub\\s+)?entity\\s+${entityType}\\b`);

        // Helper: search a document for the entity, navigate if found
        const searchAndNavigate = async (uri: vscode.Uri): Promise<boolean> => {
            try {
                const doc = await vscode.workspace.openTextDocument(uri);
                const text = doc.getText();
                const docLines = text.split('\n');
                for (let i = 0; i < docLines.length; i++) {
                    if (pattern.test(docLines[i])) {
                        const ed = await vscode.window.showTextDocument(doc, vscode.ViewColumn.One);
                        const pos = new vscode.Position(i, 0);
                        ed.selection = new vscode.Selection(pos, pos);
                        ed.revealRange(new vscode.Range(pos, pos), vscode.TextEditorRevealType.InCenter);
                        return true;
                    }
                }
            } catch { /* skip unreadable files */ }
            return false;
        };

        // Helper: recursively collect all .sk files under a directory
        const collectSkFiles = async (dirUri: vscode.Uri, depth: number): Promise<vscode.Uri[]> => {
            if (depth <= 0) { return []; }
            const results: vscode.Uri[] = [];
            try {
                const entries = await vscode.workspace.fs.readDirectory(dirUri);
                for (const [name, type] of entries) {
                    const childUri = vscode.Uri.joinPath(dirUri, name);
                    if (type === vscode.FileType.File && name.endsWith('.sk')) {
                        results.push(childUri);
                    } else if (type === vscode.FileType.Directory && !name.startsWith('.') && name !== 'build' && name !== 'target') {
                        results.push(...await collectSkFiles(childUri, depth - 1));
                    }
                }
            } catch { /* skip inaccessible dirs */ }
            return results;
        };

        // 1. Search the current file
        if (await searchAndNavigate(vscode.Uri.file(this.currentFilePath))) { return; }

        // 2. Walk up the directory tree (up to 4 levels), searching all .sk files at each level
        //    This finds entities in sibling modules (e.g., src/lib/protection.sk from src/battery_dcdc/main.sk)
        const searched = new Set<string>();
        searched.add(this.currentFilePath);

        let dir = this.currentFilePath.substring(0, this.currentFilePath.lastIndexOf('/'));
        for (let level = 0; level < 4; level++) {
            const dirUri = vscode.Uri.file(dir);
            const skFiles = await collectSkFiles(dirUri, 3);
            for (const file of skFiles) {
                if (searched.has(file.fsPath)) { continue; }
                searched.add(file.fsPath);
                if (await searchAndNavigate(file)) { return; }
            }
            // Go up one level
            const parent = dir.substring(0, dir.lastIndexOf('/'));
            if (parent === dir) { break; } // reached root
            dir = parent;
        }
    }

    private navigateToLine(filePath: string, line: number): void {
        if (!filePath || line < 0) { return; }
        const uri = vscode.Uri.file(filePath);
        vscode.workspace.openTextDocument(uri).then(doc => {
            vscode.window.showTextDocument(doc, vscode.ViewColumn.One).then(ed => {
                const pos = new vscode.Position(line, 0);
                ed.selection = new vscode.Selection(pos, pos);
                ed.revealRange(new vscode.Range(pos, pos), vscode.TextEditorRevealType.InCenter);
            });
        });
    }

    private updateSchematic(editor: vscode.TextEditor): void {
        if (!this.panel) { return; }

        const source = editor.document.getText();
        const cursorLine = editor.selection.active.line;

        const schematicData = this.parseEntityAtCursor(source, cursorLine);
        if (schematicData) {
            this.panel.webview.postMessage({ type: 'updateSchematic', data: schematicData });
        }
    }

    private parseEntityAtCursor(source: string, cursorLine: number): SchematicData | null {
        const lines = source.split('\n');

        // Find all entity declarations with their line ranges
        const entities: { name: string; start: number; end: number }[] = [];
        let eStart = -1;
        let eName = '';
        let braceDepth = 0;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const entityMatch = line.match(/^\s*entity\s+(\w+)/);
            if (entityMatch && eStart < 0) {
                eStart = i;
                eName = entityMatch[1];
                braceDepth = 0;
            }
            if (eStart >= 0) {
                for (const ch of line) {
                    if (ch === '{') { braceDepth++; }
                    if (ch === '}') {
                        braceDepth--;
                        if (braceDepth === 0) {
                            entities.push({ name: eName, start: eStart, end: i });
                            eStart = -1;
                            break;
                        }
                    }
                }
            }
        }

        // Also find all impl blocks and their line ranges
        const implBlocks: { name: string; start: number; end: number }[] = [];
        let iStart = -1;
        let iName = '';
        braceDepth = 0;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const implMatch = line.match(/^\s*impl\s+(\w+)/);
            if (implMatch && iStart < 0) {
                iStart = i;
                iName = implMatch[1];
                braceDepth = 0;
            }
            if (iStart >= 0) {
                for (const ch of line) {
                    if (ch === '{') { braceDepth++; }
                    if (ch === '}') {
                        braceDepth--;
                        if (braceDepth === 0) {
                            implBlocks.push({ name: iName, start: iStart, end: i });
                            iStart = -1;
                            break;
                        }
                    }
                }
            }
        }

        // Determine which entity to show:
        // 1. Entity whose entity decl or impl block contains the cursor
        // 2. First entity if cursor not in any
        let targetEntity: string | null = null;
        let targetEntityStart = -1;
        let targetEntityEnd = -1;

        for (const ent of entities) {
            if (cursorLine >= ent.start && cursorLine <= ent.end) {
                targetEntity = ent.name;
                targetEntityStart = ent.start;
                targetEntityEnd = ent.end;
                break;
            }
        }

        if (!targetEntity) {
            for (const impl of implBlocks) {
                if (cursorLine >= impl.start && cursorLine <= impl.end) {
                    targetEntity = impl.name;
                    // Find the entity declaration for this impl
                    const entDecl = entities.find(e => e.name === impl.name);
                    if (entDecl) {
                        targetEntityStart = entDecl.start;
                        targetEntityEnd = entDecl.end;
                    }
                    break;
                }
            }
        }

        if (!targetEntity && entities.length > 0) {
            targetEntity = entities[0].name;
            targetEntityStart = entities[0].start;
            targetEntityEnd = entities[0].end;
        }

        if (!targetEntity || targetEntityStart < 0) { return null; }

        // Find the matching impl block
        const implBlock = implBlocks.find(ib => ib.name === targetEntity);

        return this.extractSchematic(targetEntity, lines, targetEntityStart, targetEntityEnd, implBlock || null);
    }

    private extractSchematic(
        entityName: string,
        lines: string[],
        entityStart: number,
        entityEnd: number,
        implBlock: { name: string; start: number; end: number } | null
    ): SchematicData {
        const ports: SchematicPort[] = [];
        const signals: SchematicSignal[] = [];
        const instances: SchematicInstance[] = [];
        const assignments: SchematicAssignment[] = [];

        // --- Parse ports from entity declaration ---
        for (let i = entityStart; i <= entityEnd; i++) {
            const line = lines[i].trim();

            // Skip comments
            if (line.startsWith('//')) { continue; }
            // Remove inline comments
            const cleanLine = line.replace(/\/\/.*$/, '').trim();

            const portMatch = cleanLine.match(/^\s*(in|out|inout)\s+(\w+)\s*:\s*(.+?)[\s,;]*$/);
            if (portMatch) {
                const width = this.inferWidth(portMatch[3]);
                ports.push({
                    name: portMatch[2],
                    direction: portMatch[1] as 'in' | 'out' | 'inout',
                    type: portMatch[3].trim(),
                    width,
                    line: i
                });
            }
        }

        // --- Parse impl block for signals, instances, assignments ---
        let onBlockInputPorts = new Set<string>();
        let onBlockOutputPorts = new Set<string>();
        if (implBlock) {
            const result = this.parseImplBlock(lines, implBlock.start, implBlock.end, ports, signals, instances, assignments);
            onBlockInputPorts = result.onBlockInputPorts;
            onBlockOutputPorts = result.onBlockOutputPorts;
        }

        // Also scan combinational assignments for field-access input port references
        // (e.g., lockstep_rx.phase_shift — not caught by on-block scan or instance matching)
        const inputPortNames = new Set(ports.filter(p => p.direction === 'in').map(p => p.name));
        for (const a of assignments) {
            for (const portName of inputPortNames) {
                if (a.rhs.includes(portName + '.')) {
                    onBlockInputPorts.add(portName);
                }
            }
        }

        // Collect on-block output ports AND complex-assignment output ports
        const outputPortNames = new Set(ports.filter(p => p.direction === 'out').map(p => p.name));
        const logicOutputPorts = new Set(onBlockOutputPorts);
        for (const a of assignments) {
            if (!a.isOutputPort) { continue; }
            const outName = a.lhs.split('.')[0];
            if (logicOutputPorts.has(outName)) { continue; }
            const rhs = a.rhs.trim();
            if (!/^\w+$/.test(rhs)) {
                logicOutputPorts.add(outName);
            }
        }

        // Build connection graph (two-phase: first without Logic, then add Logic for uncovered ports)
        const nets = this.buildConnectionGraph(
            entityName, ports, signals, instances, assignments,
            onBlockInputPorts, logicOutputPorts
        );

        return {
            entity_name: entityName,
            ports,
            signals,
            instances,
            assignments,
            nets,
            filePath: this.currentFilePath,
            entityLine: entityStart,
            implLine: implBlock ? implBlock.start : -1
        };
    }

    private parseImplBlock(
        lines: string[],
        implStart: number,
        implEnd: number,
        ports: SchematicPort[],
        signals: SchematicSignal[],
        instances: SchematicInstance[],
        assignments: SchematicAssignment[]
    ): { onBlockInputPorts: Set<string>; onBlockOutputPorts: Set<string> } {
        const portNames = new Set(ports.map(p => p.name));
        const inputPortNames = new Set(ports.filter(p => p.direction === 'in').map(p => p.name));
        const outputPortNames = new Set(ports.filter(p => p.direction === 'out').map(p => p.name));

        // Track which line ranges are inside instance blocks (to skip them for assignment parsing)
        const instanceRanges: { start: number; end: number }[] = [];

        // Track which line ranges are inside on() blocks
        const onBlockRanges: { start: number; end: number }[] = [];

        // Ports referenced/assigned inside on() blocks (sequential logic)
        const onBlockInputPorts = new Set<string>();
        const onBlockOutputPorts = new Set<string>();

        // First pass: find on(...) blocks so we don't parse assignments inside them as combinational
        let i = implStart + 1; // skip the `impl Name {` line
        while (i <= implEnd) {
            const line = lines[i].trim();
            if (line.match(/^\s*on\s*\(/)) {
                const blockStart = i;
                let depth = 0;
                for (let j = i; j <= implEnd; j++) {
                    for (const ch of lines[j]) {
                        if (ch === '{') { depth++; }
                        if (ch === '}') {
                            depth--;
                            if (depth === 0) {
                                onBlockRanges.push({ start: blockStart, end: j });
                                i = j;
                                break;
                            }
                        }
                    }
                    if (depth === 0) { break; }
                }
            }
            i++;
        }

        // Scan on() block contents for port references
        for (const range of onBlockRanges) {
            for (let j = range.start; j <= range.end; j++) {
                const line = lines[j].replace(/\/\/.*$/, '').trim();
                if (!line) { continue; }

                // Check for output port assignments (LHS of `=`)
                const assignMatch = line.match(/^(\w+)\s*=/);
                if (assignMatch && outputPortNames.has(assignMatch[1])) {
                    onBlockOutputPorts.add(assignMatch[1]);
                }

                // Check for input port references (any token matching an input port)
                const tokens: string[] = line.match(/\b\w+\b/g) || [];
                for (const tok of tokens) {
                    if (inputPortNames.has(tok)) {
                        onBlockInputPorts.add(tok);
                    }
                }
            }
        }

        // Second pass: find instances (multi-line brace-delimited blocks)
        i = implStart + 1;
        while (i <= implEnd) {
            const line = lines[i];
            const trimmed = line.trim();

            // Skip comments
            if (trimmed.startsWith('//')) { i++; continue; }

            // Check for instance declaration: let name = EntityType::<generics> {
            const instMatch = trimmed.match(
                /^\s*let\s+(\w+)\s*=\s*(\w+)(?:::<[^>]*>)?\s*\{/
            );
            if (instMatch) {
                const instName = instMatch[1];
                const instType = instMatch[2];
                const connections: SchematicConnection[] = [];
                const instStartLine = i;

                // Track braces to find end of instance block
                let depth = 0;
                let foundClose = false;
                for (let j = i; j <= implEnd && !foundClose; j++) {
                    const jLine = lines[j];
                    for (let k = 0; k < jLine.length; k++) {
                        if (jLine[k] === '{') { depth++; }
                        if (jLine[k] === '}') {
                            depth--;
                            if (depth === 0) {
                                instanceRanges.push({ start: i, end: j });
                                i = j;
                                foundClose = true;
                                break;
                            }
                        }
                    }
                }

                // Parse connections from lines within the instance block
                for (let j = instStartLine; j <= (instanceRanges.length > 0 ? instanceRanges[instanceRanges.length - 1].end : i); j++) {
                    const connLine = lines[j].replace(/\/\/.*$/, '').trim();
                    // Match port: expression or port = expression
                    // The port name is an identifier, the expression is everything after : or = until , or end
                    const connMatch = connLine.match(/^\s*(\w+)\s*[:=]\s*(.+?)\s*[,}]?\s*$/);
                    if (connMatch) {
                        const connPort = connMatch[1];
                        let connExpr = connMatch[2].trim();
                        // Strip trailing comma
                        connExpr = connExpr.replace(/,\s*$/, '').trim();
                        // Skip the `let name = Type {` line itself
                        if (connPort === 'let' || connPort === instType) { continue; }
                        connections.push({ port: connPort, signal: connExpr });
                    }
                }

                instances.push({
                    name: instName,
                    entity_type: instType,
                    connections,
                    line: instStartLine
                });
            }

            i++;
        }

        // Third pass: signals and assignments (outside instance blocks and on() blocks)
        for (let li = implStart + 1; li < implEnd; li++) {
            // Skip if inside an instance block
            if (instanceRanges.some(r => li >= r.start && li <= r.end)) { continue; }
            // Skip if inside an on() block
            if (onBlockRanges.some(r => li >= r.start && li <= r.end)) { continue; }

            const line = lines[li].replace(/\/\/.*$/, '').trim();
            if (!line || line === '{' || line === '}') { continue; }

            // Signal declarations
            const sigMatch = line.match(/^\s*signal\s+(\w+)\s*:\s*(.+?)[\s;]*$/);
            if (sigMatch) {
                const width = this.inferWidth(sigMatch[2]);
                signals.push({ name: sigMatch[1], type: sigMatch[2].trim(), width, line: li });
                continue;
            }

            // Combinational assignments: lhs = rhs (but not `let`, `signal`, `on`, `if`, `match`, etc.)
            if (line.startsWith('let ') || line.startsWith('signal ') || line.startsWith('on') ||
                line.startsWith('if ') || line.startsWith('match ') || line.startsWith('generate ') ||
                line.startsWith('impl ') || line.startsWith('entity ') || line.startsWith('}') ||
                line.startsWith('{') || line.startsWith('else')) { continue; }

            const assignMatch = line.match(/^(\w+(?:\.\w+)*)\s*=\s*(.+)$/);
            if (assignMatch) {
                const lhs = assignMatch[1].trim();
                let rhs = assignMatch[2].trim();
                // Remove trailing semicolons/commas if any
                rhs = rhs.replace(/[;,]\s*$/, '').trim();

                assignments.push({
                    lhs,
                    rhs,
                    line: li,
                    isOutputPort: portNames.has(lhs.split('.')[0])
                });
            }
        }

        return { onBlockInputPorts, onBlockOutputPorts };
    }

    private buildConnectionGraph(
        _entityName: string,
        ports: SchematicPort[],
        signals: SchematicSignal[],
        instances: SchematicInstance[],
        assignments: SchematicAssignment[],
        onBlockInputPorts: Set<string>,
        logicOutputPorts: Set<string>
    ): SchematicNet[] {
        const nets: SchematicNet[] = [];
        const inputPortNames = new Set(ports.filter(p => p.direction === 'in').map(p => p.name));
        const outputPortNames = new Set(ports.filter(p => p.direction === 'out').map(p => p.name));

        // Set of signals driven by combinational assignments (LHS of `sig = expr`)
        const drivenSignals = new Set<string>();
        for (const a of assignments) {
            drivenSignals.add(a.lhs.split('.')[0]);
        }

        // Direction inference: for each instance connection, determine if it's input or output
        // Key heuristic: if a signal is a simple identifier AND is NOT driven by any
        // combinational assignment AND is NOT an entity input port, then the instance
        // must be producing it (it's an output of the instance).
        for (const inst of instances) {
            for (const conn of inst.connections) {
                const sig = conn.signal;
                const isSimpleIdent = /^\w+$/.test(sig);

                let direction: 'in' | 'out' = 'in';

                if (!isSimpleIdent) {
                    // Complex expression (cast, function call, field access, etc.) → always input
                    direction = 'in';
                } else if (inputPortNames.has(sig)) {
                    // Entity input port feeds into instance
                    direction = 'in';
                } else if (drivenSignals.has(sig)) {
                    // Signal is driven by a combinational assignment → input to instance
                    direction = 'in';
                } else if (outputPortNames.has(sig)) {
                    // Direct connection to entity output port name → instance drives it
                    direction = 'out';
                } else {
                    // Signal is NOT driven by anything else → must be produced by this instance
                    direction = 'out';
                }

                conn.direction = direction;
            }
        }

        // ── Build nets ──

        // 1. Entity input ports → instance inputs that reference them
        //    Matches both exact name (e.g., `clk`) and field access (e.g., `config.protection`)
        for (const port of ports) {
            if (port.direction !== 'in') { continue; }
            const sinks: SchematicEndpoint[] = [];
            for (const inst of instances) {
                for (const conn of inst.connections) {
                    if (conn.direction !== 'in') { continue; }
                    // Exact match or field access prefix (config.field, bms.field)
                    if (conn.signal === port.name ||
                        conn.signal.startsWith(port.name + '.')) {
                        sinks.push({ type: 'instance', name: inst.name, port: conn.port });
                    }
                }
            }
            if (sinks.length > 0) {
                nets.push({
                    name: port.name,
                    width: port.width,
                    driver: { type: 'entity_port', name: port.name, port: port.name },
                    sinks
                });
            }
        }

        // 2. Instance outputs → entity output ports and/or other instance inputs
        for (const inst of instances) {
            for (const conn of inst.connections) {
                if (conn.direction !== 'out') { continue; }
                const sig = conn.signal;
                const sinks: SchematicEndpoint[] = [];

                // Check if directly connected to an entity output port
                if (outputPortNames.has(sig)) {
                    sinks.push({ type: 'entity_port', name: sig, port: sig });
                }

                // Check if assigned to an entity output port (e.g., `charge_phase = cc_cv_phase`)
                for (const a of assignments) {
                    if (a.isOutputPort && a.rhs === sig) {
                        const outName = a.lhs.split('.')[0];
                        if (!sinks.find(s => s.type === 'entity_port' && s.name === outName)) {
                            sinks.push({ type: 'entity_port', name: outName, port: outName });
                        }
                    }
                }

                // Check if consumed by other instances as input
                for (const otherInst of instances) {
                    if (otherInst.name === inst.name) { continue; }
                    for (const otherConn of otherInst.connections) {
                        if (otherConn.signal === sig && otherConn.direction === 'in') {
                            sinks.push({ type: 'instance', name: otherInst.name, port: otherConn.port });
                        }
                    }
                }

                if (sinks.length > 0) {
                    const sigInfo = signals.find(s => s.name === sig);
                    nets.push({
                        name: sig,
                        width: sigInfo ? sigInfo.width : 1,
                        driver: { type: 'instance', name: inst.name, port: conn.port },
                        sinks
                    });
                }
            }
        }

        // Track which signals/ports already have nets to avoid duplicates
        const coveredSignals = new Set<string>();
        for (const n of nets) {
            coveredSignals.add(n.name);
        }

        // 3. Input port → assignment chain → instance (multi-hop)
        //    e.g., adc_temp_pri → temp_pri → temp_max → protection.temperature
        //    Follows assignment chains transitively (up to 4 hops).
        for (const port of ports) {
            if (port.direction !== 'in') { continue; }
            if (coveredSignals.has(port.name)) { continue; }

            // Find all assignments whose RHS references this input port (including field access)
            const allDerived = new Set<string>();
            for (const a of assignments) {
                if (a.isOutputPort) { continue; }
                const rhsTokens: string[] = a.rhs.match(/\b\w+\b/g) || [];
                if (rhsTokens.includes(port.name) || a.rhs.includes(port.name + '.')) {
                    allDerived.add(a.lhs.split('.')[0]);
                }
            }

            // Follow assignment chains transitively (multi-hop)
            for (let hop = 0; hop < 4; hop++) {
                const newDerived: string[] = [];
                for (const a of assignments) {
                    if (a.isOutputPort) { continue; }
                    const lhsSig = a.lhs.split('.')[0];
                    if (allDerived.has(lhsSig)) { continue; } // already found
                    const rhsTokens: string[] = a.rhs.match(/\b\w+\b/g) || [];
                    for (const d of allDerived) {
                        if (rhsTokens.includes(d)) {
                            newDerived.push(lhsSig);
                            break;
                        }
                    }
                }
                if (newDerived.length === 0) { break; }
                for (const d of newDerived) { allDerived.add(d); }
            }

            // Check all derived signals for instance consumption
            const sinks: SchematicEndpoint[] = [];
            for (const sig of allDerived) {
                for (const inst of instances) {
                    for (const conn of inst.connections) {
                        if (conn.signal === sig && conn.direction === 'in') {
                            sinks.push({ type: 'instance', name: inst.name, port: conn.port });
                        }
                    }
                }
            }

            if (sinks.length > 0) {
                nets.push({
                    name: port.name,
                    width: port.width,
                    driver: { type: 'entity_port', name: port.name, port: port.name },
                    sinks
                });
                coveredSignals.add(port.name);
            }
        }

        // 4. Instance output → signal → assignment → output port chains
        //    e.g., charge_ctrl outputs cc_cv_output → `phase_raw = (cc_cv_output >> 8) as int[10]`
        //    → `phase_limited = ...` → `phase_actual = phase_limited`
        //    Already partially covered in step 2. Here we extend: find instance outputs
        //    whose signal is used in assignment chains leading to output ports.
        //    (Step 2 only checks direct `output_port = sig` assignments)

        // 5. Output port assignments from internal signals (not instance outputs)
        //    Handles both simple RHS (`state = state_reg`) and complex RHS
        //    (`fan_pwm = fan_counter < fan_duty`, `faults = FaultFlags { ... }`).
        for (const a of assignments) {
            if (!a.isOutputPort) { continue; }
            const outPortName = a.lhs.split('.')[0];
            if (coveredSignals.has(outPortName)) { continue; }

            const outPort = ports.find(p => p.name === outPortName && p.direction === 'out');
            if (!outPort) { continue; }

            const rhs = a.rhs.trim();
            const rhsIsSimple = /^\w+$/.test(rhs);
            const sink: SchematicEndpoint = { type: 'entity_port', name: outPortName, port: outPortName };

            // For simple RHS, check if it's an instance output (already handled in step 2)
            if (rhsIsSimple) {
                let isInstOutput = false;
                for (const inst of instances) {
                    for (const conn of inst.connections) {
                        if (conn.signal === rhs && conn.direction === 'out') {
                            isInstOutput = true;
                        }
                    }
                }
                if (isInstOutput) { continue; } // already covered in step 2
            }

            // Try to trace the source: instance output, input port, or internal logic
            const rhsTokens: string[] = rhs.match(/\b\w+\b/g) || [];
            const sigInfo = rhsIsSimple ? signals.find(s => s.name === rhs) : undefined;

            // Check if any RHS token (or derived signal) comes from an instance output
            let sourceInst: { instName: string; port: string } | undefined;
            for (const inst of instances) {
                for (const conn of inst.connections) {
                    if (conn.direction === 'out') {
                        // Direct reference: rhs contains the instance output signal
                        if (rhsTokens.includes(conn.signal)) {
                            sourceInst = { instName: inst.name, port: conn.port };
                            break;
                        }
                        // One-hop: rhs references a signal derived from instance output
                        if (rhsIsSimple) {
                            for (const innerA of assignments) {
                                if (innerA.lhs === rhs) {
                                    const innerToks: string[] = innerA.rhs.match(/\b\w+\b/g) || [];
                                    if (innerToks.includes(conn.signal)) {
                                        sourceInst = { instName: inst.name, port: conn.port };
                                    }
                                }
                            }
                        }
                    }
                    if (sourceInst) { break; }
                }
                if (sourceInst) { break; }
            }

            // Check if any RHS token comes from an input port
            let sourcePort: SchematicPort | undefined;
            if (!sourceInst) {
                for (const tok of rhsTokens) {
                    const p = ports.find(pp => pp.name === tok && pp.direction === 'in');
                    if (p) { sourcePort = p; break; }
                }
                // Also check one-hop: simple RHS signal derived from input port
                if (!sourcePort && rhsIsSimple) {
                    for (const innerA of assignments) {
                        if (innerA.lhs === rhs) {
                            const innerToks: string[] = innerA.rhs.match(/\b\w+\b/g) || [];
                            for (const tok of innerToks) {
                                const p = ports.find(pp => pp.name === tok && pp.direction === 'in');
                                if (p) { sourcePort = p; break; }
                            }
                        }
                        if (sourcePort) { break; }
                    }
                }
            }

            if (sourceInst) {
                nets.push({
                    name: rhsIsSimple ? rhs : outPortName,
                    width: sigInfo ? sigInfo.width : outPort.width,
                    driver: { type: 'instance', name: sourceInst.instName, port: sourceInst.port },
                    sinks: [sink]
                });
            } else if (sourcePort) {
                nets.push({
                    name: rhsIsSimple ? rhs : outPortName,
                    width: sourcePort.width,
                    driver: { type: 'entity_port', name: sourcePort.name, port: sourcePort.name },
                    sinks: [sink]
                });
            } else {
                // Driven by internal logic — will be handled by step 7 (Logic instance)
                // if it's in logicOutputPorts; skip here to avoid duplicate nets
                if (logicOutputPorts.has(outPortName)) { continue; }
                nets.push({
                    name: rhsIsSimple ? rhs : outPortName,
                    width: sigInfo ? sigInfo.width : outPort.width,
                    driver: { type: 'entity_port', name: rhsIsSimple ? rhs : outPortName, port: rhsIsSimple ? rhs : outPortName },
                    sinks: [sink]
                });
            }
            coveredSignals.add(outPortName);
        }

        // 6. Signal-mediated connections: assignment drives a signal that instances consume
        //    (not covered by steps 1-5). These are internal signals driven by combinational
        //    logic, so they are routed through the synthetic "Logic" instance.
        const logicSignalOutputs: { sig: string; sinks: SchematicEndpoint[]; width: number }[] = [];
        for (const a of assignments) {
            if (a.isOutputPort) { continue; }
            const sig = a.lhs.split('.')[0];
            if (coveredSignals.has(sig)) { continue; }

            const sinks: SchematicEndpoint[] = [];
            for (const inst of instances) {
                for (const conn of inst.connections) {
                    if (conn.signal === sig && conn.direction === 'in') {
                        sinks.push({ type: 'instance', name: inst.name, port: conn.port });
                    }
                }
            }
            for (const oa of assignments) {
                if (oa.isOutputPort && oa.rhs === sig) {
                    const outName = oa.lhs.split('.')[0];
                    sinks.push({ type: 'entity_port', name: outName, port: outName });
                }
            }

            if (sinks.length > 0) {
                // Check if this signal is produced by an instance output (already in step 2)
                let isInstOutput = false;
                for (const inst of instances) {
                    for (const conn of inst.connections) {
                        if (conn.signal === sig && conn.direction === 'out') {
                            isInstOutput = true;
                        }
                    }
                }
                if (!isInstOutput) {
                    const sigInfo = signals.find(s => s.name === sig);
                    logicSignalOutputs.push({
                        sig,
                        sinks,
                        width: sigInfo ? sigInfo.width : 1
                    });
                    coveredSignals.add(sig);
                }
            }
        }

        // 7. Create synthetic "Logic" instance for ports used in on() blocks / complex assignments
        //    that are still uncovered after steps 1-6, plus internal signal outputs from step 6.
        const logicConnections: SchematicConnection[] = [];

        // Uncovered input ports referenced in on() blocks or field-access assignments
        for (const portName of onBlockInputPorts) {
            if (coveredSignals.has(portName)) { continue; }
            logicConnections.push({ port: portName, signal: portName, direction: 'in' });
        }

        // Uncovered output ports driven by on() blocks or complex assignments
        for (const portName of logicOutputPorts) {
            if (coveredSignals.has(portName)) { continue; }
            logicConnections.push({ port: portName, signal: portName, direction: 'out' });
        }

        // Add step 6 internal signals as Logic outputs
        for (const lso of logicSignalOutputs) {
            logicConnections.push({ port: lso.sig, signal: lso.sig, direction: 'out' });
        }

        if (logicConnections.length > 0) {
            const logicInst: SchematicInstance = {
                name: 'logic',
                entity_type: 'Logic',
                connections: logicConnections,
                line: -1
            };
            instances.push(logicInst);

            // Create nets for Logic instance connections
            for (const conn of logicConnections) {
                if (conn.direction === 'in') {
                    const port = ports.find(p => p.name === conn.port && p.direction === 'in');
                    if (port) {
                        nets.push({
                            name: port.name,
                            width: port.width,
                            driver: { type: 'entity_port', name: port.name, port: port.name },
                            sinks: [{ type: 'instance', name: 'logic', port: conn.port }]
                        });
                        coveredSignals.add(port.name);
                    }
                } else if (conn.direction === 'out') {
                    const port = ports.find(p => p.name === conn.port && p.direction === 'out');
                    if (port) {
                        nets.push({
                            name: port.name,
                            width: port.width,
                            driver: { type: 'instance', name: 'logic', port: conn.port },
                            sinks: [{ type: 'entity_port', name: port.name, port: port.name }]
                        });
                        coveredSignals.add(port.name);
                    }
                }
            }

            // Create nets for step 6 internal signal outputs from Logic
            for (const lso of logicSignalOutputs) {
                nets.push({
                    name: lso.sig,
                    width: lso.width,
                    driver: { type: 'instance', name: 'logic', port: lso.sig },
                    sinks: lso.sinks
                });
            }
        }

        return nets;
    }

    private inferWidth(typeStr: string): number {
        typeStr = typeStr.trim();
        if (typeStr === 'bit' || typeStr === 'bool' || typeStr === 'clock' || typeStr === 'reset'
            || typeStr.startsWith('reset(')) { return 1; }
        const bitMatch = typeStr.match(/bit<(\d+)>/);
        if (bitMatch) { return parseInt(bitMatch[1]); }
        const natMatch = typeStr.match(/nat<(\d+)>|nat\[(\d+)\]/);
        if (natMatch) { return parseInt(natMatch[1] || natMatch[2]); }
        const intMatch = typeStr.match(/int<(\d+)>|int\[(\d+)\]/);
        if (intMatch) { return parseInt(intMatch[1] || intMatch[2]); }
        if (typeStr === 'fp32') { return 32; }
        if (typeStr === 'fp16') { return 16; }
        if (typeStr === 'fp64') { return 64; }
        // Custom types — assume width > 1 (will be drawn as bus)
        if (/^[A-Z]\w+$/.test(typeStr)) { return 8; }
        return 1;
    }

    private getHtml(webview: vscode.Webview): string {
        const elkUri = webview.asWebviewUri(
            vscode.Uri.file(path.join(this.context.extensionPath, 'node_modules', 'elkjs', 'lib', 'elk.bundled.js'))
        );
        const scriptUri = webview.asWebviewUri(
            vscode.Uri.file(path.join(this.context.extensionPath, 'media', 'schematic.js'))
        );

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>SKALP Schematic</title>
    <style>
        html, body {
            margin: 0;
            padding: 0;
            overflow: hidden;
            width: 100%;
            height: 100%;
            background: var(--vscode-editor-background);
            color: var(--vscode-editor-foreground);
            font-family: var(--vscode-font-family);
            display: flex;
            flex-direction: column;
        }
        #toolbar {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 4px 8px;
            background: var(--vscode-titleBar-activeBackground);
            border-bottom: 1px solid var(--vscode-panel-border);
            font-size: 12px;
            flex-shrink: 0;
        }
        #toolbar button {
            background: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 4px 8px;
            cursor: pointer;
            font-size: 12px;
        }
        #toolbar button:hover {
            background: var(--vscode-button-hoverBackground);
        }
        #entity-name {
            font-weight: bold;
        }
        #stats {
            color: var(--vscode-descriptionForeground);
            font-size: 11px;
        }
        canvas {
            display: block;
            flex: 1;
            min-height: 0;
        }
    </style>
</head>
<body>
    <div id="toolbar">
        <span id="entity-name">No entity</span>
        <span id="stats"></span>
        <span>|</span>
        <button id="btn-zoom-in">+</button>
        <button id="btn-zoom-out">-</button>
        <button id="btn-zoom-fit">Fit</button>
    </div>
    <canvas id="schematic-canvas"></canvas>
    <script src="${elkUri}"></script>
    <script src="${scriptUri}"></script>
</body>
</html>`;
    }
}

interface SchematicPort {
    name: string;
    direction: 'in' | 'out' | 'inout';
    type: string;
    width: number;
    line: number;
}

interface SchematicSignal {
    name: string;
    type: string;
    width: number;
    line: number;
}

interface SchematicConnection {
    port: string;
    signal: string;
    direction?: 'in' | 'out';
}

interface SchematicInstance {
    name: string;
    entity_type: string;
    connections: SchematicConnection[];
    line: number;
}

interface SchematicAssignment {
    lhs: string;
    rhs: string;
    line: number;
    isOutputPort: boolean;
}

interface SchematicEndpoint {
    type: 'entity_port' | 'instance';
    name: string;
    port: string;
}

interface SchematicNet {
    name: string;
    width: number;
    driver: SchematicEndpoint;
    sinks: SchematicEndpoint[];
}

interface SchematicData {
    entity_name: string;
    ports: SchematicPort[];
    signals: SchematicSignal[];
    instances: SchematicInstance[];
    assignments: SchematicAssignment[];
    nets: SchematicNet[];
    filePath: string;
    entityLine: number;
    implLine: number;
}

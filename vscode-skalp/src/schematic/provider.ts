import * as vscode from 'vscode';

export class SchematicViewerProvider {
    private context: vscode.ExtensionContext;
    private panel: vscode.WebviewPanel | null = null;

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
                { enableScripts: true, retainContextWhenHidden: true }
            );

            const scriptUri = this.panel.webview.asWebviewUri(
                vscode.Uri.joinPath(this.context.extensionUri, 'media', 'schematic.js')
            );

            this.panel.webview.html = this.getHtml(scriptUri);

            this.panel.onDidDispose(() => {
                this.panel = null;
            });
        }

        this.updateSchematic(editor);

        // Update on cursor move (debounced)
        let timeout: ReturnType<typeof setTimeout> | null = null;
        const disposable = vscode.window.onDidChangeTextEditorSelection((e) => {
            if (e.textEditor.document.fileName.endsWith('.sk')) {
                if (timeout) { clearTimeout(timeout); }
                timeout = setTimeout(() => this.updateSchematic(e.textEditor), 150);
            }
        });

        this.panel.onDidDispose(() => {
            disposable.dispose();
            if (timeout) { clearTimeout(timeout); }
        });
    }

    private updateSchematic(editor: vscode.TextEditor): void {
        if (!this.panel) { return; }

        const source = editor.document.getText();
        const cursorLine = editor.selection.active.line;

        // Parse entity at cursor from source text (lightweight client-side parsing)
        const schematicData = this.parseEntityAtCursor(source, cursorLine);
        if (schematicData) {
            this.panel.webview.postMessage({ type: 'updateSchematic', data: schematicData });
        }
    }

    private parseEntityAtCursor(source: string, cursorLine: number): SchematicData | null {
        const lines = source.split('\n');

        // Find the entity block containing cursor
        let entityStart = -1;
        let entityName = '';
        let braceDepth = 0;
        let entityEnd = -1;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const entityMatch = line.match(/^\s*entity\s+(\w+)/);
            if (entityMatch) {
                entityStart = i;
                entityName = entityMatch[1];
                braceDepth = 0;
            }
            if (entityStart >= 0) {
                for (const ch of line) {
                    if (ch === '{') { braceDepth++; }
                    if (ch === '}') {
                        braceDepth--;
                        if (braceDepth === 0) {
                            entityEnd = i;
                            if (cursorLine >= entityStart && cursorLine <= entityEnd) {
                                return this.extractSchematic(entityName, lines, entityStart, entityEnd);
                            }
                            entityStart = -1;
                        }
                    }
                }
            }
        }

        // If cursor not in any entity, find the first one
        entityStart = -1;
        braceDepth = 0;
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const entityMatch = line.match(/^\s*entity\s+(\w+)/);
            if (entityMatch) {
                entityStart = i;
                entityName = entityMatch[1];
                braceDepth = 0;
            }
            if (entityStart >= 0) {
                for (const ch of line) {
                    if (ch === '{') { braceDepth++; }
                    if (ch === '}') {
                        braceDepth--;
                        if (braceDepth === 0) {
                            return this.extractSchematic(entityName, lines, entityStart, i);
                        }
                    }
                }
            }
        }

        return null;
    }

    private extractSchematic(
        entityName: string,
        lines: string[],
        start: number,
        end: number
    ): SchematicData {
        const ports: SchematicPort[] = [];
        const signals: SchematicSignal[] = [];
        const instances: SchematicInstance[] = [];

        for (let i = start; i <= end; i++) {
            const line = lines[i].trim();

            // Parse ports: in/out name: type
            const portMatch = line.match(/^\s*(in|out|inout)\s+(\w+)\s*:\s*(.+?)[\s,;{]*$/);
            if (portMatch) {
                const width = this.inferWidth(portMatch[3]);
                ports.push({
                    name: portMatch[2],
                    direction: portMatch[1] as 'in' | 'out' | 'inout',
                    type: portMatch[3],
                    width
                });
            }

            // Parse signals: signal name: type
            const sigMatch = line.match(/^\s*signal\s+(\w+)\s*:\s*(.+?)[\s;]*$/);
            if (sigMatch) {
                const width = this.inferWidth(sigMatch[2]);
                signals.push({ name: sigMatch[1], type: sigMatch[2], width });
            }

            // Parse instances: let name = Entity(...)
            const instMatch = line.match(/^\s*let\s+(\w+)\s*=\s*(\w+)\s*\(/);
            if (instMatch) {
                instances.push({
                    name: instMatch[1],
                    entity_type: instMatch[2],
                    connections: []
                });
            }

            // Parse instance declarations: inst name: Entity
            const instDeclMatch = line.match(/^\s*inst\s+(\w+)\s*:\s*(\w+)/);
            if (instDeclMatch) {
                instances.push({
                    name: instDeclMatch[1],
                    entity_type: instDeclMatch[2],
                    connections: []
                });
            }
        }

        return { entity_name: entityName, ports, signals, instances };
    }

    private inferWidth(typeStr: string): number {
        typeStr = typeStr.trim();
        if (typeStr === 'bit' || typeStr === 'bool' || typeStr === 'clock' || typeStr === 'reset') { return 1; }
        const bitMatch = typeStr.match(/bit<(\d+)>/);
        if (bitMatch) { return parseInt(bitMatch[1]); }
        const natMatch = typeStr.match(/nat<(\d+)>/);
        if (natMatch) { return parseInt(natMatch[1]); }
        const intMatch = typeStr.match(/int<(\d+)>/);
        if (intMatch) { return parseInt(intMatch[1]); }
        if (typeStr === 'fp32') { return 32; }
        if (typeStr === 'fp16') { return 16; }
        if (typeStr === 'fp64') { return 64; }
        return 1;
    }

    private getHtml(scriptUri: vscode.Uri): string {
        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>SKALP Schematic</title>
    <style>
        body {
            margin: 0;
            padding: 0;
            overflow: hidden;
            background: var(--vscode-editor-background);
            color: var(--vscode-editor-foreground);
            font-family: var(--vscode-font-family);
        }
        #toolbar {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 4px 8px;
            background: var(--vscode-titleBar-activeBackground);
            border-bottom: 1px solid var(--vscode-panel-border);
            font-size: 12px;
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
        canvas {
            display: block;
        }
    </style>
</head>
<body>
    <div id="toolbar">
        <span id="entity-name">No entity</span>
        <span>|</span>
        <button id="btn-zoom-in">+</button>
        <button id="btn-zoom-out">-</button>
        <button id="btn-zoom-fit">Fit</button>
    </div>
    <canvas id="schematic-canvas"></canvas>
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
}

interface SchematicSignal {
    name: string;
    type: string;
    width: number;
}

interface SchematicInstance {
    name: string;
    entity_type: string;
    connections: { port: string; signal: string }[];
}

interface SchematicData {
    entity_name: string;
    ports: SchematicPort[];
    signals: SchematicSignal[];
    instances: SchematicInstance[];
}

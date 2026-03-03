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
        const fn = editor?.document.fileName || '';
        if (!editor || !(fn.endsWith('.sk') || fn.endsWith('.skalp') || fn.endsWith('.vhd') || fn.endsWith('.vhdl'))) {
            vscode.window.showWarningMessage('Open a .sk or VHDL file to view its schematic.');
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
            const efn = e.textEditor.document.fileName;
            if (efn.endsWith('.sk') || efn.endsWith('.skalp') || efn.endsWith('.vhd') || efn.endsWith('.vhdl')) {
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

        // Match skalp-style `entity Name` and VHDL-style `entity Name is`
        const pattern = new RegExp(`^\\s*(pub\\s+)?entity\\s+${entityType}(\\s+is)?\\b`, 'i');

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

        // Helper: recursively collect all HDL files under a directory
        const collectHdlFiles = async (dirUri: vscode.Uri, depth: number): Promise<vscode.Uri[]> => {
            if (depth <= 0) { return []; }
            const results: vscode.Uri[] = [];
            try {
                const entries = await vscode.workspace.fs.readDirectory(dirUri);
                for (const [name, type] of entries) {
                    const childUri = vscode.Uri.joinPath(dirUri, name);
                    if (type === vscode.FileType.File && (name.endsWith('.sk') || name.endsWith('.skalp') || name.endsWith('.vhd') || name.endsWith('.vhdl'))) {
                        results.push(childUri);
                    } else if (type === vscode.FileType.Directory && !name.startsWith('.') && name !== 'build' && name !== 'target') {
                        results.push(...await collectHdlFiles(childUri, depth - 1));
                    }
                }
            } catch { /* skip inaccessible dirs */ }
            return results;
        };

        // 1. Search the current file
        if (await searchAndNavigate(vscode.Uri.file(this.currentFilePath))) { return; }

        // 2. Walk up the directory tree (up to 4 levels), searching all HDL files at each level
        const searched = new Set<string>();
        searched.add(this.currentFilePath);

        let dir = this.currentFilePath.substring(0, this.currentFilePath.lastIndexOf('/'));
        for (let level = 0; level < 4; level++) {
            const dirUri = vscode.Uri.file(dir);
            const hdlFiles = await collectHdlFiles(dirUri, 3);
            for (const file of hdlFiles) {
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

    private async updateSchematic(editor: vscode.TextEditor): Promise<void> {
        if (!this.panel) { return; }

        const uri = editor.document.uri.toString();
        const cursorLine = editor.selection.active.line;

        // Call LSP command for HIR-based schematic extraction
        try {
            const schematicData = await vscode.commands.executeCommand<SchematicData>(
                'skalp.getSchematic',
                uri,
                cursorLine
            );

            if (schematicData) {
                // Add filePath for navigation (not provided by LSP)
                schematicData.filePath = editor.document.fileName;
                this.panel.webview.postMessage({ type: 'updateSchematic', data: schematicData });
            }
        } catch {
            // LSP command not available — silently ignore
        }
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

interface SchematicData {
    entity_name: string;
    ports: any[];
    signals: any[];
    instances: any[];
    assignments: any[];
    nets: any[];
    filePath: string;
    entityLine: number;
    implLine: number;
}

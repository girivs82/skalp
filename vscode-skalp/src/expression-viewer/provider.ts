import * as vscode from 'vscode';
import * as path from 'path';

// --- Data types sent to webview (for documentation; JSON comes from LSP) ---

interface CircuitNode {
    id: string;
    type: string;
    label: string;
    inputLabels?: string[];
}

interface CircuitWire {
    id: string;
    label?: string;
    fromNode: string;
    toNode: string;
    toPort: number;
}

interface CircuitInput {
    id: string;
    name: string;
    isConstant: boolean;
}

interface ExpressionData {
    targetName: string;
    nodes: CircuitNode[];
    wires: CircuitWire[];
    inputs: CircuitInput[];
    sourceRange: [number, number];
}

// --- Provider ---

export class ExpressionViewerProvider {
    private context: vscode.ExtensionContext;
    private panel: vscode.WebviewPanel | null = null;
    private debounceTimer: ReturnType<typeof setTimeout> | null = null;

    constructor(context: vscode.ExtensionContext) {
        this.context = context;
    }

    show(): void {
        const editor = vscode.window.activeTextEditor;
        if (!editor || (!editor.document.fileName.endsWith('.sk') && !editor.document.fileName.endsWith('.skalp'))) {
            vscode.window.showWarningMessage('Open a SKALP file first.');
            return;
        }

        if (this.panel) {
            this.panel.reveal(vscode.ViewColumn.Beside);
            this.updateExpression(editor);
            return;
        }

        this.panel = vscode.window.createWebviewPanel(
            'skalpExpressionViewer',
            'Expression Viewer',
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

        this.panel.onDidDispose(() => {
            this.panel = null;
        });

        // Cursor tracking with 100ms debounce
        this.context.subscriptions.push(
            vscode.window.onDidChangeTextEditorSelection(() => {
                this.scheduleUpdate();
            }),
            vscode.workspace.onDidChangeTextDocument(() => {
                this.scheduleUpdate();
            })
        );

        this.updateExpression(editor);
    }

    private scheduleUpdate(): void {
        if (this.debounceTimer) {
            clearTimeout(this.debounceTimer);
        }
        this.debounceTimer = setTimeout(() => {
            const editor = vscode.window.activeTextEditor;
            if (editor && this.panel) {
                this.updateExpression(editor);
            }
        }, 100);
    }

    private async updateExpression(editor: vscode.TextEditor): Promise<void> {
        if (!this.panel) { return; }

        const doc = editor.document;
        if (!doc.fileName.endsWith('.sk') && !doc.fileName.endsWith('.skalp')) {
            this.panel.webview.postMessage({ type: 'clearExpression' });
            return;
        }

        const pos = editor.selection.active;
        try {
            const result = await vscode.commands.executeCommand<ExpressionData | null>(
                'skalp.getExpressionCircuit',
                doc.uri.toString(), pos.line, pos.character
            );
            if (result && this.panel) {
                this.panel.webview.postMessage({ type: 'updateExpression', data: result });
            } else if (this.panel) {
                this.panel.webview.postMessage({ type: 'clearExpression' });
            }
        } catch {
            if (this.panel) {
                this.panel.webview.postMessage({ type: 'clearExpression' });
            }
        }
    }

    // --- HTML ---

    private getHtml(webview: vscode.Webview): string {
        const elkUri = webview.asWebviewUri(
            vscode.Uri.file(path.join(this.context.extensionPath, 'node_modules', 'elkjs', 'lib', 'elk.bundled.js'))
        );
        const scriptUri = webview.asWebviewUri(
            vscode.Uri.file(path.join(this.context.extensionPath, 'media', 'expression-viewer.js'))
        );

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
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
        #toolbar span {
            white-space: nowrap;
        }
        #target-name {
            font-weight: bold;
            color: var(--vscode-textLink-foreground);
        }
        #toolbar button {
            background: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
            border: none;
            padding: 2px 8px;
            cursor: pointer;
            border-radius: 2px;
            font-size: 12px;
        }
        #toolbar button:hover {
            background: var(--vscode-button-secondaryHoverBackground);
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
        <span id="target-name">No expression</span>
        <span id="stats"></span>
        <span>|</span>
        <button id="btn-zoom-in">+</button>
        <button id="btn-zoom-out">&minus;</button>
        <button id="btn-zoom-fit">Fit</button>
    </div>
    <canvas id="expr-canvas"></canvas>
    <script src="${elkUri}"></script>
    <script src="${scriptUri}"></script>
</body>
</html>`;
    }
}

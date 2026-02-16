import * as vscode from 'vscode';

export class WaveformViewerProvider implements vscode.CustomReadonlyEditorProvider {
    private context: vscode.ExtensionContext;

    constructor(context: vscode.ExtensionContext) {
        this.context = context;
    }

    async openCustomDocument(
        uri: vscode.Uri,
        _openContext: vscode.CustomDocumentOpenContext,
        _token: vscode.CancellationToken
    ): Promise<vscode.CustomDocument> {
        return { uri, dispose: () => {} };
    }

    async resolveCustomEditor(
        document: vscode.CustomDocument,
        webviewPanel: vscode.WebviewPanel,
        _token: vscode.CancellationToken
    ): Promise<void> {
        webviewPanel.webview.options = { enableScripts: true };

        const scriptUri = webviewPanel.webview.asWebviewUri(
            vscode.Uri.joinPath(this.context.extensionUri, 'media', 'waveform.js')
        );

        webviewPanel.webview.html = this.getHtml(scriptUri);

        const fileData = await vscode.workspace.fs.readFile(document.uri);
        let text = new TextDecoder().decode(fileData);

        // Handle gzip: if the file starts with gzip magic bytes, decompress
        if (fileData[0] === 0x1f && fileData[1] === 0x8b) {
            const ds = new DecompressionStream('gzip');
            const writer = ds.writable.getWriter();
            writer.write(fileData);
            writer.close();
            const reader = ds.readable.getReader();
            const chunks: Uint8Array[] = [];
            let done = false;
            while (!done) {
                const result = await reader.read();
                if (result.value) { chunks.push(result.value); }
                done = result.done;
            }
            const totalLen = chunks.reduce((a, c) => a + c.length, 0);
            const merged = new Uint8Array(totalLen);
            let offset = 0;
            for (const c of chunks) {
                merged.set(c, offset);
                offset += c.length;
            }
            text = new TextDecoder().decode(merged);
        }

        const skwData = JSON.parse(text);
        webviewPanel.webview.postMessage({ type: 'loadWaveform', data: skwData });

        webviewPanel.webview.onDidReceiveMessage((msg) => {
            if (msg.type === 'requestData') {
                webviewPanel.webview.postMessage({ type: 'loadWaveform', data: skwData });
            }
        });
    }

    private getHtml(scriptUri: vscode.Uri): string {
        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>SKALP Waveform Viewer</title>
    <style>
        body {
            margin: 0;
            padding: 0;
            overflow: hidden;
            background: var(--vscode-editor-background);
            color: var(--vscode-editor-foreground);
            font-family: var(--vscode-font-family);
            font-size: var(--vscode-font-size);
        }
        #toolbar {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 4px 8px;
            background: var(--vscode-titleBar-activeBackground);
            border-bottom: 1px solid var(--vscode-panel-border);
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
        #toolbar select, #toolbar input {
            background: var(--vscode-input-background);
            color: var(--vscode-input-foreground);
            border: 1px solid var(--vscode-input-border);
            padding: 2px 4px;
            font-size: 12px;
        }
        #search {
            width: 180px;
        }
        #container {
            display: flex;
            height: calc(100vh - 32px);
        }
        #signal-list {
            width: 250px;
            min-width: 150px;
            overflow-y: auto;
            border-right: 1px solid var(--vscode-panel-border);
            user-select: none;
        }
        .signal-row {
            display: flex;
            align-items: center;
            padding: 2px 8px;
            cursor: pointer;
            white-space: nowrap;
            height: 24px;
            border-bottom: 1px solid var(--vscode-panel-border, rgba(128,128,128,0.2));
        }
        .signal-row:hover {
            background: var(--vscode-list-hoverBackground);
        }
        .signal-row.selected {
            background: var(--vscode-list-activeSelectionBackground);
            color: var(--vscode-list-activeSelectionForeground);
        }
        .signal-name {
            flex: 1;
            overflow: hidden;
            text-overflow: ellipsis;
            font-size: 11px;
        }
        .signal-value {
            font-family: var(--vscode-editor-fontFamily, monospace);
            font-size: 11px;
            margin-left: 8px;
            color: var(--vscode-descriptionForeground);
        }
        .group-header {
            font-weight: bold;
            padding: 4px 8px;
            background: var(--vscode-sideBarSectionHeader-background);
            cursor: pointer;
        }
        .group-header::before {
            content: '\\25BC ';
            font-size: 10px;
        }
        .group-header.collapsed::before {
            content: '\\25B6 ';
        }
        #waveform-canvas {
            flex: 1;
        }
        #cursor-readout {
            position: absolute;
            bottom: 4px;
            right: 8px;
            background: var(--vscode-editorWidget-background);
            border: 1px solid var(--vscode-editorWidget-border);
            padding: 2px 8px;
            font-family: var(--vscode-editor-fontFamily, monospace);
            font-size: 11px;
        }
    </style>
</head>
<body>
    <div id="toolbar">
        <button id="btn-zoom-in" title="Zoom In">+</button>
        <button id="btn-zoom-out" title="Zoom Out">-</button>
        <button id="btn-zoom-fit" title="Zoom to Fit">Fit</button>
        <span>|</span>
        <label>Radix:</label>
        <select id="radix-select">
            <option value="hex">Hex</option>
            <option value="binary">Binary</option>
            <option value="decimal">Decimal</option>
            <option value="signed">Signed</option>
            <option value="float">Float</option>
        </select>
        <span>|</span>
        <input id="search" type="text" placeholder="Filter signals..." />
        <span id="info"></span>
    </div>
    <div id="container">
        <div id="signal-list"></div>
        <canvas id="waveform-canvas"></canvas>
    </div>
    <div id="cursor-readout"></div>
    <script src="${scriptUri}"></script>
</body>
</html>`;
    }
}

import * as vscode from 'vscode';

export class WaveformViewerProvider implements vscode.CustomReadonlyEditorProvider {
    private context: vscode.ExtensionContext;
    private activePanel: vscode.WebviewPanel | undefined;

    constructor(context: vscode.ExtensionContext) {
        this.context = context;
    }

    /**
     * Get the currently active waveform webview panel (if any).
     */
    public getActivePanel(): vscode.WebviewPanel | undefined {
        return this.activePanel;
    }

    /**
     * Post a message to the active waveform webview.
     * Used by the debug adapter to scroll to breakpoint cycles, add markers, etc.
     */
    public postToWaveform(message: any): void {
        if (this.activePanel) {
            this.activePanel.webview.postMessage(message);
        }
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
        this.activePanel = webviewPanel;
        webviewPanel.onDidDispose(() => {
            if (this.activePanel === webviewPanel) {
                this.activePanel = undefined;
            }
        });

        webviewPanel.webview.options = { enableScripts: true };

        const scriptUri = webviewPanel.webview.asWebviewUri(
            vscode.Uri.joinPath(this.context.extensionUri, 'media', 'waveform.js')
        );

        const fontSize = vscode.workspace.getConfiguration('skalp.waveform').get<number>('fontSize', 12);
        webviewPanel.webview.html = this.getHtml(scriptUri, fontSize);

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

    private getHtml(scriptUri: vscode.Uri, fontSize: number): string {
        const rowHeight = Math.round(fontSize * 2.2);
        const headerHeight = Math.round(fontSize * 1.7);
        const smallFontSize = Math.max(8, fontSize - 1);
        const tinyFontSize = Math.max(7, fontSize - 2);
        const toolbarFontSize = Math.max(10, fontSize);
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
            font-size: ${toolbarFontSize}px;
        }
        #toolbar button:hover {
            background: var(--vscode-button-hoverBackground);
        }
        #toolbar select, #toolbar input {
            background: var(--vscode-input-background);
            color: var(--vscode-input-foreground);
            border: 1px solid var(--vscode-input-border);
            padding: 2px 4px;
            font-size: ${toolbarFontSize}px;
        }
        #search {
            width: 180px;
        }
        #container {
            display: flex;
            height: calc(100vh - 32px);
        }
        #signal-list {
            width: ${Math.round(fontSize * 22)}px;
            min-width: 150px;
            overflow-y: auto;
            border-right: 1px solid var(--vscode-panel-border);
            user-select: none;
            padding-top: ${headerHeight}px;
        }
        .signal-row {
            display: flex;
            align-items: center;
            padding: 0 8px;
            cursor: pointer;
            white-space: nowrap;
            height: ${rowHeight}px;
            box-sizing: border-box;
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
            font-size: ${fontSize}px;
        }
        .signal-value {
            font-family: var(--vscode-editor-fontFamily, monospace);
            font-size: ${fontSize}px;
            margin-left: 8px;
            color: var(--vscode-descriptionForeground);
        }
        .group-header {
            font-weight: bold;
            padding: 0 8px;
            height: ${rowHeight}px;
            box-sizing: border-box;
            line-height: ${rowHeight}px;
            background: var(--vscode-sideBarSectionHeader-background);
            border-bottom: 1px solid var(--vscode-panel-border, rgba(128,128,128,0.2));
            cursor: pointer;
            font-size: ${fontSize}px;
        }
        .group-header::before {
            content: '\\25BC ';
            font-size: ${tinyFontSize}px;
        }
        .group-header.collapsed::before {
            content: '\\25B6 ';
        }
        .group-count {
            font-weight: normal;
            opacity: 0.5;
            font-size: ${tinyFontSize}px;
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
            font-size: ${smallFontSize}px;
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
        <label>Input:</label>
        <select id="input-mode" title="Trackpad: pinch=zoom, swipe=pan. Mouse: wheel=zoom, drag=pan.">
            <option value="auto">Auto</option>
            <option value="trackpad">Trackpad</option>
            <option value="mouse">Mouse</option>
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
    <script>
        window.skalpConfig = {
            fontSize: ${fontSize},
            rowHeight: ${rowHeight},
            headerHeight: ${headerHeight},
            signalListWidth: ${Math.round(fontSize * 22)}
        };
    </script>
    <script src="${scriptUri}"></script>
</body>
</html>`;
    }
}

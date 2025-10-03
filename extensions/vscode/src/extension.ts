import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    console.log('SKALP Language Support is now active!');

    // Get LSP server path from configuration
    const config = vscode.workspace.getConfiguration('skalp');
    const serverPath = config.get<string>('lsp.path', 'skalp-lsp');

    // Server options
    const serverOptions: ServerOptions = {
        run: { command: serverPath, transport: TransportKind.stdio },
        debug: { command: serverPath, transport: TransportKind.stdio }
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'skalp' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.sk')
        }
    };

    // Create and start the language client
    client = new LanguageClient(
        'skalp-lsp',
        'SKALP Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client (this will also launch the server)
    client.start();

    // Register commands
    const disposable = vscode.commands.registerCommand('skalp.helloWorld', () => {
        vscode.window.showInformationMessage('Hello from SKALP Language Support!');
    });

    context.subscriptions.push(disposable);
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
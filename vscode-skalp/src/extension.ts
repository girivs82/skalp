import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    // Get the server path from configuration
    const config = vscode.workspace.getConfiguration('skalp');
    const serverPath = config.get<string>('serverPath') || 'skalp-lsp';

    // Server options
    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio,
            options: {
                env: {
                    ...process.env,
                    RUST_LOG: 'skalp_lsp=debug'
                }
            }
        }
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        // Register the server for SKALP documents
        documentSelector: [{ scheme: 'file', language: 'skalp' }],
        synchronize: {
            // Notify the server about file changes to .sk and .skalp files
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{sk,skalp}')
        }
    };

    // Create the language client and start it
    client = new LanguageClient(
        'skalp',
        'SKALP Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client
    client.start();

    // Register additional commands
    context.subscriptions.push(
        vscode.commands.registerCommand('skalp.restart', async () => {
            await client.stop();
            await client.start();
            vscode.window.showInformationMessage('SKALP Language Server restarted');
        })
    );

    vscode.window.showInformationMessage('SKALP Language Support activated');
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
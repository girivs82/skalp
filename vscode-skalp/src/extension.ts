import * as vscode from 'vscode';
import * as path from 'path';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';
import { CliRunner } from './cli/runner';
import { WaveformViewerProvider } from './waveform/provider';
import { EcResultTreeProvider } from './ec-dashboard/tree-provider';
import { SchematicViewerProvider } from './schematic/provider';
import { ExpressionViewerProvider } from './expression-viewer/provider';
import { SkalpTaskProvider } from './tasks/provider';
import { SkalpTestController } from './testing/controller';
import { SkalpDebugAdapterFactory } from './debug/adapter-factory';

let client: LanguageClient;
let cliRunner: CliRunner;
let statusBarItem: vscode.StatusBarItem;
let testController: SkalpTestController;

export function activate(context: vscode.ExtensionContext) {
    const outputChannel = vscode.window.createOutputChannel('SKALP');
    cliRunner = new CliRunner(outputChannel, context.extensionPath);

    // --- Status Bar ---
    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 50);
    statusBarItem.text = '$(circuit-board) SKALP';
    statusBarItem.tooltip = 'SKALP — click for commands';
    statusBarItem.command = 'skalp.showMenu';
    statusBarItem.show();
    context.subscriptions.push(statusBarItem);

    // --- LSP Client ---
    const config = vscode.workspace.getConfiguration('skalp');
    const serverPath = resolveBinaryPath(
        'skalp-lsp',
        config.get<string>('serverPath') || 'skalp-lsp',
        context.extensionPath
    );

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

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'skalp' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{sk,skalp}')
        }
    };

    client = new LanguageClient(
        'skalp',
        'SKALP Language Server',
        serverOptions,
        clientOptions
    );

    // Start LSP in background — don't block extension activation
    client.start().catch((err: any) => {
        outputChannel.appendLine(`LSP server failed to start: ${err.message}`);
    });

    // --- EC Dashboard ---
    const ecProvider = new EcResultTreeProvider();
    vscode.window.registerTreeDataProvider('skalpEc', ecProvider);

    // --- Waveform Viewer ---
    const waveformProvider = new WaveformViewerProvider(context);
    context.subscriptions.push(
        vscode.window.registerCustomEditorProvider(
            'skalp.waveformViewer',
            waveformProvider,
            { webviewOptions: { retainContextWhenHidden: true } }
        )
    );

    // --- Schematic Viewer ---
    const schematicProvider = new SchematicViewerProvider(context);

    // --- Expression Viewer ---
    const expressionProvider = new ExpressionViewerProvider(context);

    // --- Task Provider ---
    context.subscriptions.push(
        vscode.tasks.registerTaskProvider(SkalpTaskProvider.type, new SkalpTaskProvider())
    );

    // --- Test Controller ---
    testController = new SkalpTestController(outputChannel);
    context.subscriptions.push({ dispose: () => testController.dispose() });

    // --- Debug Adapter ---
    const debugAdapterFactory = new SkalpDebugAdapterFactory(context.extensionPath);
    // Wire waveform provider into debug adapter for live waveform sync
    debugAdapterFactory.setWaveformCallback((msg: any) => waveformProvider.postToWaveform(msg));
    context.subscriptions.push(
        vscode.debug.registerDebugAdapterDescriptorFactory('skalp', debugAdapterFactory)
    );

    // --- Commands ---
    context.subscriptions.push(
        vscode.commands.registerCommand('skalp.showMenu', async () => {
            const items: vscode.QuickPickItem[] = [
                { label: '$(gear) Build', description: 'Build to SystemVerilog' },
                { label: '$(play) Simulate', description: 'Run simulation' },
                { label: '$(chip) Synthesize', description: 'Synthesize for FPGA' },
                { label: '$(verified) Equivalence Check', description: 'Verify RTL vs gates' },
                { label: '$(edit) Format', description: 'Format source file' },
                { label: '$(search) Analyze', description: 'Gate-level analysis' },
                { label: '$(graph-line) Open Waveform', description: 'Open waveform viewer' },
                { label: '$(circuit-board) Show Schematic', description: 'Show schematic view' },
                { label: '$(symbol-operator) Expression Viewer', description: 'Show expression circuit diagram' },
            ];
            const pick = await vscode.window.showQuickPick(items, { placeHolder: 'Select SKALP command' });
            if (!pick) { return; }
            const commandMap: Record<string, string> = {
                '$(gear) Build': 'skalp.build',
                '$(play) Simulate': 'skalp.simulate',
                '$(chip) Synthesize': 'skalp.synthesize',
                '$(verified) Equivalence Check': 'skalp.ec',
                '$(edit) Format': 'skalp.format',
                '$(search) Analyze': 'skalp.analyze',
                '$(graph-line) Open Waveform': 'skalp.showWaveform',
                '$(circuit-board) Show Schematic': 'skalp.showSchematic',
                '$(symbol-operator) Expression Viewer': 'skalp.showExpressionViewer',
            };
            const cmd = commandMap[pick.label];
            if (cmd) { vscode.commands.executeCommand(cmd); }
        }),

        vscode.commands.registerCommand('skalp.restart', async () => {
            await client.stop();
            await client.start();
            vscode.window.showInformationMessage('SKALP Language Server restarted');
        }),

        vscode.commands.registerCommand('skalp.build', async (uri?: vscode.Uri) => {
            const filePath = resolveFilePath(uri);
            if (!filePath) { return; }

            statusBarItem.text = '$(loading~spin) Building...';
            const result = await cliRunner.runBuild(filePath);
            statusBarItem.text = result.exitCode === 0
                ? '$(check) Build OK'
                : '$(error) Build Failed';
            setTimeout(() => { statusBarItem.text = '$(circuit-board) SKALP'; }, 5000);

            if (result.exitCode !== 0) {
                vscode.window.showErrorMessage('SKALP build failed. See output for details.');
            }
        }),

        vscode.commands.registerCommand('skalp.simulate', async (uri?: vscode.Uri) => {
            const filePath = resolveFilePath(uri);
            if (!filePath) { return; }

            statusBarItem.text = '$(loading~spin) Simulating...';
            const result = await cliRunner.runSimulate(filePath);
            statusBarItem.text = result.exitCode === 0
                ? '$(check) Sim OK'
                : '$(error) Sim Failed';
            setTimeout(() => { statusBarItem.text = '$(circuit-board) SKALP'; }, 5000);

            if (result.exitCode === 0 && result.outputPath) {
                const openWaveform = await vscode.window.showInformationMessage(
                    'Simulation complete.',
                    'Open Waveform'
                );
                if (openWaveform === 'Open Waveform') {
                    const skwUri = vscode.Uri.file(result.outputPath);
                    vscode.commands.executeCommand('vscode.openWith', skwUri, 'skalp.waveformViewer');
                }
            } else if (result.exitCode !== 0) {
                vscode.window.showErrorMessage('SKALP simulation failed. See output for details.');
            }
        }),

        vscode.commands.registerCommand('skalp.ec', async (uri?: vscode.Uri) => {
            const filePath = resolveFilePath(uri);
            if (!filePath) { return; }

            statusBarItem.text = '$(loading~spin) EC running...';
            const result = await cliRunner.runEc(filePath);
            statusBarItem.text = result.exitCode === 0
                ? '$(verified) EC PASS'
                : '$(error) EC FAIL';
            setTimeout(() => { statusBarItem.text = '$(circuit-board) SKALP'; }, 10000);

            if (result.exitCode === 0 && result.outputPath) {
                ecProvider.loadReport(result.outputPath);
            } else if (result.exitCode !== 0) {
                vscode.window.showErrorMessage('Equivalence check failed. See output for details.');
            }
        }),

        vscode.commands.registerCommand('skalp.synthesize', async (uri?: vscode.Uri) => {
            const filePath = resolveFilePath(uri);
            if (!filePath) { return; }

            statusBarItem.text = '$(loading~spin) Synthesizing...';
            const result = await cliRunner.runSynth(filePath);
            statusBarItem.text = result.exitCode === 0
                ? '$(check) Synth OK'
                : '$(error) Synth Failed';
            setTimeout(() => { statusBarItem.text = '$(circuit-board) SKALP'; }, 5000);

            if (result.exitCode !== 0) {
                vscode.window.showErrorMessage('SKALP synthesis failed. See output for details.');
            }
        }),

        vscode.commands.registerCommand('skalp.format', async (uri?: vscode.Uri) => {
            const filePath = resolveFilePath(uri);
            if (!filePath) { return; }

            const result = await cliRunner.runFormat(filePath);
            if (result.exitCode === 0) {
                vscode.window.showInformationMessage('SKALP: File formatted.');
            } else {
                vscode.window.showErrorMessage('SKALP format failed. See output for details.');
            }
        }),

        vscode.commands.registerCommand('skalp.analyze', async (uri?: vscode.Uri) => {
            const filePath = resolveFilePath(uri);
            if (!filePath) { return; }

            statusBarItem.text = '$(loading~spin) Analyzing...';
            const result = await cliRunner.runAnalyze(filePath);
            statusBarItem.text = '$(circuit-board) SKALP';

            if (result.exitCode !== 0) {
                vscode.window.showErrorMessage('SKALP analysis failed. See output for details.');
            }
        }),

        vscode.commands.registerCommand('skalp.showWaveform', async () => {
            const files = await vscode.window.showOpenDialog({
                filters: { 'Waveform': ['skw', 'skw.gz', 'vcd'] },
                canSelectMany: false
            });
            if (files && files.length > 0) {
                vscode.commands.executeCommand('vscode.openWith', files[0], 'skalp.waveformViewer');
            }
        }),

        vscode.commands.registerCommand('skalp.showSchematic', async () => {
            schematicProvider.show();
        }),

        vscode.commands.registerCommand('skalp.showExpressionViewer', async () => {
            expressionProvider.show();
        })
    );

    // --- Build on Save ---
    context.subscriptions.push(
        vscode.workspace.onDidSaveTextDocument(async (doc) => {
            const buildOnSave = vscode.workspace.getConfiguration('skalp').get<boolean>('buildOnSave');
            if (buildOnSave && (doc.fileName.endsWith('.sk') || doc.fileName.endsWith('.skalp'))) {
                vscode.commands.executeCommand('skalp.build', vscode.Uri.file(doc.fileName));
            }
        })
    );
}

function resolveFilePath(uri?: vscode.Uri): string | undefined {
    if (uri) {
        return uri.fsPath;
    }
    const editor = vscode.window.activeTextEditor;
    if (editor && (editor.document.fileName.endsWith('.sk') || editor.document.fileName.endsWith('.skalp'))) {
        return editor.document.fileName;
    }
    vscode.window.showWarningMessage('No SKALP file selected.');
    return undefined;
}

/**
 * Resolve a SKALP binary path with the following priority:
 * 1. User-configured explicit path (if not the default name)
 * 2. Bundled binary inside extension (bin/)
 * 3. PATH lookup (covers cargo install, homebrew, etc.)
 * 4. Dev mode: repo-relative (../target/release/ or ../target/debug/)
 */
export function resolveBinaryPath(
    binaryName: string,
    configured: string,
    extensionPath: string
): string {
    const fs = require('fs');

    // 1. User set an explicit path — use it directly
    if (configured !== binaryName) {
        return configured;
    }

    // 2. Bundled binary inside extension
    const bundled = path.join(extensionPath, 'bin', binaryName);
    if (fs.existsSync(bundled)) {
        return bundled;
    }

    // 3. PATH lookup — check if the binary is available on PATH
    const { execFileSync } = require('child_process');
    try {
        const which = process.platform === 'win32' ? 'where' : 'which';
        const result = execFileSync(which, [binaryName], { encoding: 'utf8', timeout: 3000 }).trim();
        if (result) {
            return result.split('\n')[0];
        }
    } catch {
        // not on PATH
    }

    // 4. Dev mode: repo-relative
    const repoRoot = path.resolve(extensionPath, '..');
    const releaseBin = path.join(repoRoot, 'target', 'release', binaryName);
    if (fs.existsSync(releaseBin)) {
        return releaseBin;
    }
    const debugBin = path.join(repoRoot, 'target', 'debug', binaryName);
    if (fs.existsSync(debugBin)) {
        return debugBin;
    }

    // Fallback: hope it's on PATH (will fail with a clear error if not)
    return binaryName;
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

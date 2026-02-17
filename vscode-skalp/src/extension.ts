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
import { SkalpTaskProvider } from './tasks/provider';
import { SkalpTestController } from './testing/controller';

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
    let serverPath = config.get<string>('serverPath') || 'skalp-lsp';

    // Auto-detect: if default 'skalp-lsp' and running from repo, use the built binary
    if (serverPath === 'skalp-lsp') {
        const repoRoot = path.resolve(context.extensionPath, '..');
        const releaseBin = path.join(repoRoot, 'target', 'release', 'skalp-lsp');
        const debugBin = path.join(repoRoot, 'target', 'debug', 'skalp-lsp');
        const fs = require('fs');
        if (fs.existsSync(releaseBin)) {
            serverPath = releaseBin;
        } else if (fs.existsSync(debugBin)) {
            serverPath = debugBin;
        }
    }

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

    client.start();

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

    // --- Task Provider ---
    context.subscriptions.push(
        vscode.tasks.registerTaskProvider(SkalpTaskProvider.type, new SkalpTaskProvider())
    );

    // --- Test Controller ---
    testController = new SkalpTestController(outputChannel);
    context.subscriptions.push({ dispose: () => testController.dispose() });

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

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

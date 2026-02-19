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
import { resolveBinaryPath } from './utils/resolve-binary';
import { parseEntities } from './scaffolding/entity-parser';
import { generateTestbench } from './scaffolding/testbench-generator';

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
                { label: '$(file-directory-create) New Project', description: 'Create a new SKALP project' },
                { label: '$(beaker) Generate Testbench', description: 'Generate Rust test from entity' },
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
                '$(file-directory-create) New Project': 'skalp.newProject',
                '$(beaker) Generate Testbench': 'skalp.genTestbench',
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
        }),

        // --- New Project ---
        vscode.commands.registerCommand('skalp.newProject', async () => {
            const name = await vscode.window.showInputBox({
                prompt: 'Project name',
                placeHolder: 'my-design',
                validateInput: (v) => /^[a-zA-Z][a-zA-Z0-9_-]*$/.test(v) ? null : 'Invalid project name (letters, digits, hyphens, underscores)',
            });
            if (!name) { return; }

            const folders = await vscode.window.showOpenDialog({
                canSelectFiles: false,
                canSelectFolders: true,
                canSelectMany: false,
                openLabel: 'Select parent directory',
                defaultUri: vscode.workspace.workspaceFolders?.[0]?.uri,
            });
            if (!folders || folders.length === 0) { return; }
            const parentDir = folders[0].fsPath;

            statusBarItem.text = '$(loading~spin) Creating project...';
            const result = await cliRunner.runNewProject(name, parentDir);
            statusBarItem.text = '$(circuit-board) SKALP';

            if (result.exitCode === 0) {
                const projectUri = vscode.Uri.file(path.join(parentDir, name));
                const open = await vscode.window.showInformationMessage(
                    `Project '${name}' created.`,
                    'Open Project'
                );
                if (open === 'Open Project') {
                    vscode.commands.executeCommand('vscode.openFolder', projectUri);
                }
            } else {
                vscode.window.showErrorMessage(`Failed to create project. See output for details.`);
            }
        }),

        // --- Generate Testbench ---
        vscode.commands.registerCommand('skalp.genTestbench', async (uri?: vscode.Uri) => {
            // Get the .sk file content
            let filePath: string | undefined;
            let source: string;

            if (uri) {
                filePath = uri.fsPath;
                const doc = await vscode.workspace.openTextDocument(uri);
                source = doc.getText();
            } else {
                const editor = vscode.window.activeTextEditor;
                if (!editor || !(editor.document.fileName.endsWith('.sk') || editor.document.fileName.endsWith('.skalp'))) {
                    vscode.window.showWarningMessage('No SKALP file selected.');
                    return;
                }
                filePath = editor.document.fileName;
                source = editor.document.getText();
            }

            const entities = parseEntities(source);
            if (entities.length === 0) {
                vscode.window.showWarningMessage('No entity declarations found in this file.');
                return;
            }

            // Pick entity if multiple
            let entity = entities[0];
            if (entities.length > 1) {
                const pick = await vscode.window.showQuickPick(
                    entities.map(e => ({ label: e.name, description: `${e.ports.length} ports`, entity: e })),
                    { placeHolder: 'Select entity for testbench' }
                );
                if (!pick) { return; }
                entity = (pick as any).entity;
            }

            // Find Cargo project root (walk up to find Cargo.toml)
            let projectRoot = path.dirname(filePath);
            let foundCargo = false;
            for (let i = 0; i < 5; i++) {
                const cargoPath = path.join(projectRoot, 'Cargo.toml');
                try {
                    await vscode.workspace.fs.stat(vscode.Uri.file(cargoPath));
                    foundCargo = true;
                    break;
                } catch {
                    const parent = path.dirname(projectRoot);
                    if (parent === projectRoot) { break; }
                    projectRoot = parent;
                }
            }

            if (!foundCargo) {
                // Use the file's directory as project root
                projectRoot = path.dirname(filePath);
            }

            // Compute relative source path
            const sourceRelPath = path.relative(projectRoot, filePath).replace(/\\/g, '/');

            // Generate testbench content
            const snakeName = entity.name
                .replace(/([A-Z]+)([A-Z][a-z])/g, '$1_$2')
                .replace(/([a-z\d])([A-Z])/g, '$1_$2')
                .toLowerCase();
            const content = generateTestbench(entity, sourceRelPath);

            // Write to tests/ directory
            const testsDir = path.join(projectRoot, 'tests');
            const outPath = path.join(testsDir, `test_${snakeName}.rs`);

            // Check if file already exists
            try {
                await vscode.workspace.fs.stat(vscode.Uri.file(outPath));
                const overwrite = await vscode.window.showWarningMessage(
                    `tests/test_${snakeName}.rs already exists. Overwrite?`,
                    'Overwrite', 'Cancel'
                );
                if (overwrite !== 'Overwrite') { return; }
            } catch {
                // File doesn't exist — good
            }

            // Ensure tests/ directory exists
            await vscode.workspace.fs.createDirectory(vscode.Uri.file(testsDir));

            // Write file
            await vscode.workspace.fs.writeFile(
                vscode.Uri.file(outPath),
                Buffer.from(content, 'utf-8')
            );

            // Check if Cargo.toml needs skalp-testing dev-dependency
            if (foundCargo) {
                const cargoPath = path.join(projectRoot, 'Cargo.toml');
                const cargoDoc = await vscode.workspace.openTextDocument(cargoPath);
                const cargoText = cargoDoc.getText();
                if (!cargoText.includes('skalp-testing')) {
                    const addDep = await vscode.window.showInformationMessage(
                        'Cargo.toml is missing skalp-testing dev-dependency. Add it?',
                        'Add', 'Skip'
                    );
                    if (addDep === 'Add') {
                        // Resolve skalp crates path from CLI binary location
                        const cliPath = cliRunner.getResolvedCliPath();
                        let cratesPath = '';
                        if (cliPath) {
                            // Binary is at <hls>/target/{debug,release}/skalp
                            const candidate = path.join(path.dirname(cliPath), '..', '..', 'crates', 'skalp-testing');
                            try {
                                await vscode.workspace.fs.stat(vscode.Uri.file(candidate));
                                cratesPath = path.join(path.dirname(cliPath), '..', '..', 'crates');
                                cratesPath = path.resolve(cratesPath);
                            } catch {
                                // Not found — will use TODO placeholder
                            }
                        }

                        const testingDep = cratesPath
                            ? `skalp-testing = { path = "${cratesPath}/skalp-testing" }`
                            : `# TODO: set path to your skalp-testing crate\n# skalp-testing = { path = "/path/to/hls/crates/skalp-testing" }`;
                        const tokioDep = 'tokio = { version = "1", features = ["full", "test-util"] }';

                        if (cargoText.includes('[dev-dependencies]')) {
                            const edit = new vscode.WorkspaceEdit();
                            const idx = cargoText.indexOf('[dev-dependencies]');
                            const lineNum = cargoText.substring(0, idx).split('\n').length - 1;
                            edit.insert(vscode.Uri.file(cargoPath), new vscode.Position(lineNum + 1, 0),
                                `${tokioDep}\n${testingDep}\n`);
                            await vscode.workspace.applyEdit(edit);
                            await cargoDoc.save();
                        } else {
                            const edit = new vscode.WorkspaceEdit();
                            const lastLine = cargoDoc.lineCount;
                            edit.insert(vscode.Uri.file(cargoPath), new vscode.Position(lastLine, 0),
                                `\n[dev-dependencies]\n${tokioDep}\n${testingDep}\n`);
                            await vscode.workspace.applyEdit(edit);
                            await cargoDoc.save();
                        }
                    }
                }
            }

            // Open the generated file
            const doc = await vscode.workspace.openTextDocument(outPath);
            await vscode.window.showTextDocument(doc);
            vscode.window.showInformationMessage(`Generated testbench: tests/test_${snakeName}.rs`);
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

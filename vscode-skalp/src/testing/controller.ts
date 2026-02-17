import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as cp from 'child_process';

export class SkalpTestController {
    private controller: vscode.TestController;
    private disposables: vscode.Disposable[] = [];
    private watcher: vscode.FileSystemWatcher | undefined;
    private log: vscode.OutputChannel;

    constructor(outputChannel: vscode.OutputChannel) {
        this.log = outputChannel;
        this.log.appendLine('[TestController] Initializing...');
        this.controller = vscode.tests.createTestController('skalpTests', 'SKALP Tests');

        this.controller.resolveHandler = async (item) => {
            if (!item) {
                await this.discoverAllTests();
            }
        };

        this.controller.createRunProfile(
            'Run',
            vscode.TestRunProfileKind.Run,
            (request, token) => this.runTests(request, token),
            true
        );

        // Watch for test file changes
        this.watcher = vscode.workspace.createFileSystemWatcher('**/tests/**/*.rs');
        this.watcher.onDidCreate(() => this.discoverAllTests());
        this.watcher.onDidChange((uri) => this.parseTestFile(uri));
        this.watcher.onDidDelete((uri) => {
            this.controller.items.delete(uri.toString());
        });
        this.disposables.push(this.watcher);

        // Re-discover when workspace folders change (e.g. folder opened later)
        this.disposables.push(
            vscode.workspace.onDidChangeWorkspaceFolders(() => this.discoverAllTests())
        );

        // Initial discovery — retry once after 2s if workspace isn't ready yet
        this.discoverAllTests();
        setTimeout(() => {
            if (this.controller.items.size === 0) {
                this.discoverAllTests();
            }
        }, 2000);
    }

    dispose(): void {
        this.controller.dispose();
        for (const d of this.disposables) {
            d.dispose();
        }
    }

    private async discoverAllTests(): Promise<void> {
        const roots = new Set<string>();

        // Source 1: workspace folders
        const folders = vscode.workspace.workspaceFolders;
        if (folders) {
            for (const folder of folders) {
                roots.add(folder.uri.fsPath);
            }
        }

        // Source 2: derive from open editors (walk up to find Cargo.toml)
        for (const editor of vscode.window.visibleTextEditors) {
            const filePath = editor.document.uri.fsPath;
            const cargoDir = this.findCargoDir(filePath);
            if (cargoDir) {
                roots.add(cargoDir);
            }
        }

        // Source 3: workspace.rootPath (deprecated but works for single-folder opens)
        if (vscode.workspace.rootPath) {
            roots.add(vscode.workspace.rootPath);
        }

        if (roots.size === 0) {
            this.log.appendLine('[TestController] No workspace folders or open editors found');
            return;
        }

        this.log.appendLine(`[TestController] Scanning ${roots.size} root(s): ${[...roots].join(', ')}`);
        for (const root of roots) {
            this.scanForTestDirs(root, 0);
        }
        this.log.appendLine(`[TestController] Discovery complete: ${this.controller.items.size} test file(s) found`);
    }

    private scanForTestDirs(dir: string, depth: number): void {
        if (depth > 5) { return; }

        // Check if this directory has a Cargo.toml with a tests/ subdirectory
        const cargoPath = path.join(dir, 'Cargo.toml');
        const testsDir = path.join(dir, 'tests');
        if (fs.existsSync(cargoPath) && fs.existsSync(testsDir)) {
            this.log.appendLine(`[TestController] Found Cargo project with tests: ${dir}`);
            this.scanTestDir(testsDir);
        }

        // Recurse into subdirectories (skip target/, node_modules/, build/, .)
        try {
            const entries = fs.readdirSync(dir, { withFileTypes: true });
            for (const entry of entries) {
                if (!entry.isDirectory()) { continue; }
                if (entry.name.startsWith('.') ||
                    entry.name === 'target' ||
                    entry.name === 'node_modules' ||
                    entry.name === 'build' ||
                    entry.name === 'out') {
                    continue;
                }
                this.scanForTestDirs(path.join(dir, entry.name), depth + 1);
            }
        } catch {
            // permission errors etc
        }
    }

    private scanTestDir(testsDir: string): void {
        try {
            const files = fs.readdirSync(testsDir);
            const rsFiles = files.filter(f => f.endsWith('.rs'));
            this.log.appendLine(`[TestController] Scanning ${testsDir}: ${rsFiles.length} .rs file(s)`);
            for (const file of rsFiles) {
                const filePath = path.join(testsDir, file);
                const uri = vscode.Uri.file(filePath);
                this.parseTestFileSync(uri, filePath);
            }
        } catch (e) {
            this.log.appendLine(`[TestController] Error scanning ${testsDir}: ${e}`);
        }
    }

    private parseTestFileSync(uri: vscode.Uri, filePath: string): void {
        let content: string;
        try {
            content = fs.readFileSync(filePath, 'utf-8');
        } catch {
            return;
        }
        this.parseTestContent(uri, content);
    }

    private async parseTestFile(uri: vscode.Uri): Promise<void> {
        let content: string;
        try {
            const raw = await vscode.workspace.fs.readFile(uri);
            content = new TextDecoder().decode(raw);
        } catch {
            return;
        }
        this.parseTestContent(uri, content);
    }

    private parseTestContent(uri: vscode.Uri, content: string): void {
        const lines = content.split('\n');
        const testFunctions: { name: string; line: number }[] = [];

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i].trim();
            // Match #[test] or #[tokio::test] followed by fn declaration
            if (line === '#[test]' || line === '#[tokio::test]') {
                // Look ahead for fn declaration (may have attributes/cfg in between)
                for (let j = i + 1; j < Math.min(i + 10, lines.length); j++) {
                    const fnMatch = lines[j].match(/^\s*(?:pub\s+)?(?:async\s+)?fn\s+(\w+)/);
                    if (fnMatch) {
                        testFunctions.push({ name: fnMatch[1], line: j });
                        break;
                    }
                    // Stop if we hit another test attribute (not cfg/allow/ignore)
                    const trimmed = lines[j].trim();
                    if (trimmed.startsWith('#[') &&
                        !trimmed.includes('cfg') &&
                        !trimmed.includes('allow') &&
                        !trimmed.includes('ignore')) {
                        break;
                    }
                }
            }
        }

        if (testFunctions.length === 0) {
            this.controller.items.delete(uri.toString());
            return;
        }

        this.log.appendLine(`[TestController]   ${path.basename(uri.fsPath)}: ${testFunctions.length} test(s) — ${testFunctions.map(f => f.name).join(', ')}`);

        // Create file-level test item
        const basename = path.basename(uri.fsPath, '.rs');
        const fileItem = this.controller.createTestItem(
            uri.toString(),
            basename,
            uri
        );
        fileItem.canResolveChildren = false;
        fileItem.description = `${testFunctions.length} test${testFunctions.length !== 1 ? 's' : ''}`;

        // Create function-level test items
        for (const fn of testFunctions) {
            const fnItem = this.controller.createTestItem(
                `${uri.toString()}::${fn.name}`,
                fn.name,
                uri
            );
            fnItem.range = new vscode.Range(fn.line, 0, fn.line, 0);
            fileItem.children.add(fnItem);
        }

        this.controller.items.add(fileItem);
    }

    private async runTests(
        request: vscode.TestRunRequest,
        token: vscode.CancellationToken
    ): Promise<void> {
        const run = this.controller.createTestRun(request);

        // Collect tests to run
        const testsToRun: { fileItem: vscode.TestItem; fnItems: vscode.TestItem[] }[] = [];

        if (request.include) {
            for (const item of request.include) {
                if (item.children.size > 0) {
                    // File-level item — run all children
                    const fns: vscode.TestItem[] = [];
                    item.children.forEach(child => fns.push(child));
                    testsToRun.push({ fileItem: item, fnItems: fns });
                } else if (item.parent) {
                    // Function-level item
                    testsToRun.push({ fileItem: item.parent, fnItems: [item] });
                }
            }
        } else {
            // Run all tests
            this.controller.items.forEach(fileItem => {
                const fns: vscode.TestItem[] = [];
                fileItem.children.forEach(child => fns.push(child));
                testsToRun.push({ fileItem, fnItems: fns });
            });
        }

        // Exclude any explicitly excluded tests
        const excludeIds = new Set(request.exclude?.map(e => e.id) ?? []);

        for (const { fileItem, fnItems } of testsToRun) {
            if (token.isCancellationRequested) { break; }

            const filteredFns = fnItems.filter(fn => !excludeIds.has(fn.id) && !excludeIds.has(fileItem.id));
            if (filteredFns.length === 0) { continue; }

            for (const fn of filteredFns) {
                run.started(fn);
            }

            const uri = fileItem.uri!;
            const basename = path.basename(uri.fsPath, '.rs');

            // Find the Cargo.toml directory
            const cargoDir = this.findCargoDir(uri.fsPath);
            if (!cargoDir) {
                for (const fn of filteredFns) {
                    run.errored(fn, new vscode.TestMessage('Could not find Cargo.toml'));
                }
                continue;
            }

            // Build cargo test command
            const args = ['test', '--test', basename];

            if (filteredFns.length === 1) {
                args.push(filteredFns[0].label);
            } else if (filteredFns.length < fnItems.length) {
                const names = filteredFns.map(fn => fn.label).join('|');
                args.push(names);
            }

            args.push('--', '--nocapture');

            const results = await this.execCargoTest(args, cargoDir, token);

            // Map results to test items
            for (const fn of filteredFns) {
                const result = results.get(fn.label);
                if (!result) {
                    if (filteredFns.length === 1 && results.size > 0) {
                        const first = results.values().next().value;
                        if (first) {
                            this.applyResult(run, fn, first);
                        } else {
                            run.skipped(fn);
                        }
                    } else {
                        run.skipped(fn);
                    }
                } else {
                    this.applyResult(run, fn, result);
                }
            }

            // Check for waveform files
            this.checkForWaveforms(cargoDir, run, fileItem);
        }

        run.end();
    }

    private applyResult(
        run: vscode.TestRun,
        item: vscode.TestItem,
        result: { status: 'ok' | 'FAILED' | 'ignored'; output: string; duration?: number }
    ): void {
        switch (result.status) {
            case 'ok':
                run.passed(item, result.duration);
                break;
            case 'FAILED':
                run.failed(item, new vscode.TestMessage(result.output || 'Test failed'), result.duration);
                break;
            case 'ignored':
                run.skipped(item);
                break;
        }
    }

    private execCargoTest(
        args: string[],
        cwd: string,
        token: vscode.CancellationToken
    ): Promise<Map<string, { status: 'ok' | 'FAILED' | 'ignored'; output: string; duration?: number }>> {
        return new Promise((resolve) => {
            const results = new Map<string, { status: 'ok' | 'FAILED' | 'ignored'; output: string; duration?: number }>();
            let fullOutput = '';

            const proc = cp.spawn('cargo', args, { cwd });

            token.onCancellationRequested(() => {
                proc.kill();
            });

            proc.stdout.on('data', (data: Buffer) => {
                fullOutput += data.toString();
            });

            proc.stderr.on('data', (data: Buffer) => {
                fullOutput += data.toString();
            });

            proc.on('close', () => {
                // Parse cargo test output: "test <name> ... ok" or "test <name> ... FAILED"
                const testLineRe = /^test\s+(\S+)\s+\.\.\.\s+(ok|FAILED|ignored)(?:\s+\(([^)]+)\))?/gm;
                let match;
                while ((match = testLineRe.exec(fullOutput)) !== null) {
                    const name = match[1];
                    const status = match[2] as 'ok' | 'FAILED' | 'ignored';
                    const durationStr = match[3];
                    let duration: number | undefined;
                    if (durationStr) {
                        const secs = parseFloat(durationStr);
                        if (!isNaN(secs)) {
                            duration = secs * 1000;
                        }
                    }

                    let output = '';
                    if (status === 'FAILED') {
                        const failureRe = new RegExp(
                            `---- ${name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')} stdout ----\\n([\\s\\S]*?)(?=\\n---- |\\nfailures:)`, 'g'
                        );
                        const failMatch = failureRe.exec(fullOutput);
                        if (failMatch) {
                            output = failMatch[1].trim();
                        } else {
                            output = fullOutput.slice(-2000);
                        }
                    }

                    results.set(name, { status, output, duration });
                }

                if (results.size === 0 && fullOutput.includes('error[E')) {
                    results.set('__compile_error__', {
                        status: 'FAILED',
                        output: fullOutput.slice(-3000),
                    });
                }

                resolve(results);
            });

            proc.on('error', () => {
                resolve(results);
            });
        });
    }

    private findCargoDir(filePath: string): string | undefined {
        let dir = path.dirname(filePath);
        while (dir !== path.dirname(dir)) {
            if (fs.existsSync(path.join(dir, 'Cargo.toml'))) {
                return dir;
            }
            dir = path.dirname(dir);
        }
        return undefined;
    }

    private checkForWaveforms(cargoDir: string, run: vscode.TestRun, fileItem: vscode.TestItem): void {
        const buildDir = path.join(cargoDir, 'build');
        if (!fs.existsSync(buildDir)) { return; }

        try {
            const skwFiles = fs.readdirSync(buildDir)
                .filter(f => f.endsWith('.skw.gz') || f.endsWith('.skw'));
            if (skwFiles.length > 0) {
                const latest = skwFiles
                    .map(f => ({ name: f, mtime: fs.statSync(path.join(buildDir, f)).mtimeMs }))
                    .sort((a, b) => b.mtime - a.mtime)[0];

                const skwPath = path.join(buildDir, latest.name);
                const fiveSecondsAgo = Date.now() - 5000;
                if (latest.mtime > fiveSecondsAgo) {
                    run.appendOutput(
                        `\r\nWaveform available: ${latest.name}\r\n`,
                        undefined,
                        fileItem
                    );
                    vscode.window.showInformationMessage(
                        `Waveform available: ${latest.name}`,
                        'Open Waveform'
                    ).then(choice => {
                        if (choice === 'Open Waveform') {
                            const skwUri = vscode.Uri.file(skwPath);
                            vscode.commands.executeCommand('vscode.openWith', skwUri, 'skalp.waveformViewer');
                        }
                    });
                }
            }
        } catch {
            // ignore fs errors
        }
    }
}

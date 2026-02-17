import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

export interface CliResult {
    exitCode: number;
    stdout: string;
    stderr: string;
    outputPath?: string;
}

export class CliRunner {
    private outputChannel: vscode.OutputChannel;
    private runningProcess: cp.ChildProcess | null = null;
    private extensionPath: string;
    private resolvedCliPath: string | undefined;

    constructor(outputChannel: vscode.OutputChannel, extensionPath: string) {
        this.outputChannel = outputChannel;
        this.extensionPath = extensionPath;
    }

    private getCliPath(): string {
        if (this.resolvedCliPath) {
            return this.resolvedCliPath;
        }
        const config = vscode.workspace.getConfiguration('skalp');
        const configured = config.get<string>('cliPath') || 'skalp';

        // If user set a custom path, use it directly
        if (configured !== 'skalp') {
            this.resolvedCliPath = configured;
            return configured;
        }

        // Auto-detect: look for built binary relative to extension (repo root)
        const repoRoot = path.resolve(this.extensionPath, '..');
        const releaseBin = path.join(repoRoot, 'target', 'release', 'skalp');
        const debugBin = path.join(repoRoot, 'target', 'debug', 'skalp');
        if (fs.existsSync(releaseBin)) {
            this.resolvedCliPath = releaseBin;
            return releaseBin;
        }
        if (fs.existsSync(debugBin)) {
            this.resolvedCliPath = debugBin;
            return debugBin;
        }

        // Fall back to PATH lookup
        this.resolvedCliPath = configured;
        return configured;
    }

    private getWorkspaceDir(): string {
        const folders = vscode.workspace.workspaceFolders;
        if (folders && folders.length > 0) {
            return folders[0].uri.fsPath;
        }
        return process.cwd();
    }

    async runBuild(filePath: string): Promise<CliResult> {
        const dir = path.dirname(filePath);
        const outputDir = path.join(dir, 'build');
        this.outputChannel.appendLine(`Building ${path.basename(filePath)}...`);
        return this.run(['build', '-s', filePath, '--output', outputDir], outputDir);
    }

    async runSimulate(filePath: string, cycles?: number): Promise<CliResult> {
        const config = vscode.workspace.getConfiguration('skalp');
        const defaultCycles = config.get<number>('simulation.defaultCycles') || 1000;
        const numCycles = cycles || defaultCycles;
        const dir = path.dirname(filePath);
        const skwPath = path.join(dir, 'build', path.basename(filePath, path.extname(filePath)) + '.skw.gz');

        this.outputChannel.appendLine(`Simulating ${path.basename(filePath)} for ${numCycles} cycles...`);
        return this.run(
            ['sim', filePath, '--duration', `${numCycles}`, '--output', skwPath],
            skwPath
        );
    }

    async runSynth(filePath: string, device?: string): Promise<CliResult> {
        const args = ['synth', filePath];
        if (device) {
            args.push('-d', device);
        }
        this.outputChannel.appendLine(`Synthesizing ${path.basename(filePath)}...`);
        const dir = path.dirname(filePath);
        return this.run(args, path.join(dir, 'build'));
    }

    async runFormat(filePath: string): Promise<CliResult> {
        this.outputChannel.appendLine(`Formatting ${path.basename(filePath)}...`);
        return this.run(['fmt', filePath]);
    }

    async runAnalyze(filePath: string): Promise<CliResult> {
        this.outputChannel.appendLine(`Analyzing ${path.basename(filePath)}...`);
        return this.run(['analyze', filePath]);
    }

    async runEc(filePath: string): Promise<CliResult> {
        const config = vscode.workspace.getConfiguration('skalp');
        let outputDir = config.get<string>('ec.outputDir') || '';
        if (!outputDir) {
            outputDir = path.join(path.dirname(filePath), 'ec_output');
        }
        this.outputChannel.appendLine(`Running equivalence check on ${path.basename(filePath)}...`);
        return this.run(
            ['ec', filePath, '--output', outputDir, '--format', 'json'],
            path.join(outputDir, 'ec_report.json')
        );
    }

    cancel(): void {
        if (this.runningProcess) {
            this.runningProcess.kill();
            this.runningProcess = null;
            this.outputChannel.appendLine('Process cancelled.');
        }
    }

    private run(args: string[], outputPath?: string): Promise<CliResult> {
        return new Promise((resolve) => {
            const cliPath = this.getCliPath();
            const cwd = this.getWorkspaceDir();

            this.outputChannel.appendLine(`> ${cliPath} ${args.join(' ')}`);
            this.outputChannel.show(true);

            let stdout = '';
            let stderr = '';

            const proc = cp.spawn(cliPath, args, { cwd });
            this.runningProcess = proc;

            proc.stdout.on('data', (data: Buffer) => {
                const text = data.toString();
                stdout += text;
                this.outputChannel.append(text);
            });

            proc.stderr.on('data', (data: Buffer) => {
                const text = data.toString();
                stderr += text;
                this.outputChannel.append(text);
            });

            proc.on('close', (code: number | null) => {
                this.runningProcess = null;
                const exitCode = code ?? 1;
                if (exitCode === 0) {
                    this.outputChannel.appendLine('Done.');
                } else {
                    this.outputChannel.appendLine(`Exited with code ${exitCode}`);
                }
                resolve({ exitCode, stdout, stderr, outputPath });
            });

            proc.on('error', (err: Error) => {
                this.runningProcess = null;
                this.outputChannel.appendLine(`Error: ${err.message}`);
                resolve({ exitCode: 1, stdout, stderr: err.message });
            });
        });
    }
}

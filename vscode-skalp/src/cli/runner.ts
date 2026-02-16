import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as path from 'path';

export interface CliResult {
    exitCode: number;
    stdout: string;
    stderr: string;
    outputPath?: string;
}

export class CliRunner {
    private outputChannel: vscode.OutputChannel;
    private runningProcess: cp.ChildProcess | null = null;

    constructor(outputChannel: vscode.OutputChannel) {
        this.outputChannel = outputChannel;
    }

    private getCliPath(): string {
        const config = vscode.workspace.getConfiguration('skalp');
        return config.get<string>('cliPath') || 'skalp';
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
        return this.run(['build', filePath, '--output', outputDir], outputDir);
    }

    async runSimulate(filePath: string, cycles?: number): Promise<CliResult> {
        const config = vscode.workspace.getConfiguration('skalp');
        const defaultCycles = config.get<number>('simulation.defaultCycles') || 1000;
        const numCycles = cycles || defaultCycles;
        const dir = path.dirname(filePath);
        const skwPath = path.join(dir, 'build', path.basename(filePath, path.extname(filePath)) + '.skw');

        this.outputChannel.appendLine(`Simulating ${path.basename(filePath)} for ${numCycles} cycles...`);
        return this.run(
            ['sim', filePath, '--duration', `${numCycles}`, '--format', 'skw', '--vcd', skwPath],
            skwPath
        );
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

import * as vscode from 'vscode';
import * as path from 'path';

interface SkalpTaskDefinition extends vscode.TaskDefinition {
    command: string;
    file?: string;
    target?: string;
    cycles?: number;
    device?: string;
    optimize?: string;
}

export class SkalpTaskProvider implements vscode.TaskProvider {
    static readonly type = 'skalp';

    async provideTasks(): Promise<vscode.Task[]> {
        const tasks: vscode.Task[] = [];
        const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
        if (!workspaceFolder) {
            return tasks;
        }

        // Scan for skalp.toml or .sk files to confirm this is a SKALP project
        const skalpFiles = await vscode.workspace.findFiles('**/*.sk', '**/node_modules/**', 1);
        const tomlFiles = await vscode.workspace.findFiles('skalp.toml', undefined, 1);
        if (skalpFiles.length === 0 && tomlFiles.length === 0) {
            return tasks;
        }

        tasks.push(this.createTask(
            { type: SkalpTaskProvider.type, command: 'build' },
            workspaceFolder,
            'build',
            vscode.TaskGroup.Build
        ));

        tasks.push(this.createTask(
            { type: SkalpTaskProvider.type, command: 'build', target: 'gates' },
            workspaceFolder,
            'build (gates)',
            vscode.TaskGroup.Build
        ));

        tasks.push(this.createTask(
            { type: SkalpTaskProvider.type, command: 'simulate' },
            workspaceFolder,
            'simulate',
            undefined
        ));

        tasks.push(this.createTask(
            { type: SkalpTaskProvider.type, command: 'synthesize' },
            workspaceFolder,
            'synthesize',
            undefined
        ));

        tasks.push(this.createTask(
            { type: SkalpTaskProvider.type, command: 'ec' },
            workspaceFolder,
            'ec',
            undefined
        ));

        tasks.push(this.createTask(
            { type: SkalpTaskProvider.type, command: 'format' },
            workspaceFolder,
            'format',
            undefined
        ));

        tasks.push(this.createTask(
            { type: SkalpTaskProvider.type, command: 'analyze' },
            workspaceFolder,
            'analyze',
            undefined
        ));

        return tasks;
    }

    resolveTask(task: vscode.Task): vscode.Task | undefined {
        const definition = task.definition as SkalpTaskDefinition;
        if (!definition.command) {
            return undefined;
        }
        const workspaceFolder = task.scope as vscode.WorkspaceFolder
            ?? vscode.workspace.workspaceFolders?.[0];
        if (!workspaceFolder) {
            return undefined;
        }
        return this.createTask(definition, workspaceFolder, definition.command, task.group);
    }

    private createTask(
        definition: SkalpTaskDefinition,
        workspaceFolder: vscode.WorkspaceFolder,
        label: string,
        group: vscode.TaskGroup | undefined
    ): vscode.Task {
        const args = this.buildArgs(definition);
        const cliPath = vscode.workspace.getConfiguration('skalp').get<string>('cliPath') || 'skalp';

        const execution = new vscode.ShellExecution(cliPath, args, {
            cwd: workspaceFolder.uri.fsPath
        });

        const task = new vscode.Task(
            definition,
            workspaceFolder,
            `skalp: ${label}`,
            'skalp',
            execution,
            '$skalp'
        );

        if (group) {
            task.group = group;
        }
        task.presentationOptions = { reveal: vscode.TaskRevealKind.Always, panel: vscode.TaskPanelKind.Shared };

        return task;
    }

    private buildArgs(definition: SkalpTaskDefinition): string[] {
        const args: string[] = [];
        const file = definition.file || '${file}';

        switch (definition.command) {
            case 'build': {
                args.push('build', '-s', file);
                if (definition.target === 'gates') {
                    args.push('--target', 'gates');
                } else if (definition.target === 'mir') {
                    args.push('--target', 'mir');
                }
                if (definition.optimize) {
                    args.push('--optimize', definition.optimize);
                }
                break;
            }
            case 'simulate': {
                const cycles = definition.cycles
                    || vscode.workspace.getConfiguration('skalp').get<number>('simulation.defaultCycles')
                    || 1000;
                args.push('sim', file, '--duration', `${cycles}`, '--output', '${fileDirname}/build/${fileBasenameNoExtension}.skw.gz');
                break;
            }
            case 'synthesize': {
                args.push('synth', file);
                if (definition.device) {
                    args.push('-d', definition.device);
                }
                break;
            }
            case 'ec': {
                args.push('ec', file);
                break;
            }
            case 'format': {
                args.push('fmt', file);
                break;
            }
            case 'analyze': {
                args.push('analyze', file);
                break;
            }
        }

        return args;
    }
}

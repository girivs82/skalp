/**
 * Inline signal value decorations for SKALP debug sessions.
 *
 * After each step/breakpoint, shows current signal values as gray inline text
 * annotations next to signal references in the source code. Changed signals
 * are highlighted in orange.
 */

import * as vscode from 'vscode';

interface InlineValue {
    line: number;
    signal: string;
    value: string;
    changed: boolean;
}

export class InlineValueDecorator {
    private unchangedDecorationType: vscode.TextEditorDecorationType;
    private changedDecorationType: vscode.TextEditorDecorationType;
    private active: boolean = false;

    constructor() {
        this.unchangedDecorationType = vscode.window.createTextEditorDecorationType({
            after: {
                color: new vscode.ThemeColor('editorCodeLens.foreground'),
                fontStyle: 'italic',
                margin: '0 0 0 1.5em',
            },
            isWholeLine: false,
        });

        this.changedDecorationType = vscode.window.createTextEditorDecorationType({
            after: {
                color: '#ff9800',
                fontWeight: 'bold',
                margin: '0 0 0 1.5em',
            },
            isWholeLine: false,
        });
    }

    /**
     * Apply inline value decorations from the debug server's response.
     */
    public applyValues(sourceFile: string, values: InlineValue[]): void {
        this.active = true;
        const editor = vscode.window.visibleTextEditors.find(
            e => e.document.uri.fsPath === sourceFile
        );
        if (!editor) { return; }

        const unchangedDecorations: vscode.DecorationOptions[] = [];
        const changedDecorations: vscode.DecorationOptions[] = [];

        for (const v of values) {
            const line = v.line - 1; // DAP lines are 1-indexed, VSCode is 0-indexed
            if (line < 0 || line >= editor.document.lineCount) { continue; }

            const lineText = editor.document.lineAt(line);
            const range = new vscode.Range(
                line, lineText.text.length,
                line, lineText.text.length
            );

            const decoration: vscode.DecorationOptions = {
                range,
                renderOptions: {
                    after: {
                        contentText: `  ${v.signal} = ${v.value}`,
                    },
                },
            };

            if (v.changed) {
                changedDecorations.push(decoration);
            } else {
                unchangedDecorations.push(decoration);
            }
        }

        editor.setDecorations(this.unchangedDecorationType, unchangedDecorations);
        editor.setDecorations(this.changedDecorationType, changedDecorations);
    }

    /**
     * Clear all inline value decorations (e.g., on continue or disconnect).
     */
    public clear(): void {
        if (!this.active) { return; }
        this.active = false;

        for (const editor of vscode.window.visibleTextEditors) {
            editor.setDecorations(this.unchangedDecorationType, []);
            editor.setDecorations(this.changedDecorationType, []);
        }
    }

    public dispose(): void {
        this.clear();
        this.unchangedDecorationType.dispose();
        this.changedDecorationType.dispose();
    }
}

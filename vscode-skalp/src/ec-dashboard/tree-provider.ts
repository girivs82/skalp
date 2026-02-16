import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

interface GateResult {
    name: string;
    status: 'pass' | 'fail' | 'unknown';
    time_ms?: number;
    counterexample?: Record<string, boolean>;
}

interface EcReport {
    design: string;
    status: 'pass' | 'fail' | 'unknown';
    outputs: GateResult[];
    latches: GateResult[];
    timing: {
        phase1_sim_ms: number;
        phase2_sat_ms: number;
        phase2c_sim_ms: number;
        phase3_selftest_ms: number;
        total_ms: number;
    };
    self_test: {
        bugs_injected: number;
        bugs_detected: number;
        detection_rate: number;
    };
}

type EcTreeItem = DesignItem | CategoryItem | GateItem;

class DesignItem extends vscode.TreeItem {
    constructor(public readonly report: EcReport) {
        super(report.design, vscode.TreeItemCollapsibleState.Expanded);
        this.iconPath = statusIcon(report.status);
        this.description = `${report.status.toUpperCase()} (${(report.timing.total_ms / 1000).toFixed(1)}s)`;
    }
}

class CategoryItem extends vscode.TreeItem {
    constructor(
        public readonly label: string,
        public readonly gates: GateResult[]
    ) {
        super(label, vscode.TreeItemCollapsibleState.Collapsed);
        const pass = gates.filter(g => g.status === 'pass').length;
        const fail = gates.filter(g => g.status === 'fail').length;
        const unknown = gates.filter(g => g.status === 'unknown').length;
        this.description = `${pass} pass, ${fail} fail, ${unknown} unknown`;
        if (fail > 0) {
            this.iconPath = new vscode.ThemeIcon('error', new vscode.ThemeColor('testing.iconFailed'));
        } else if (unknown > 0) {
            this.iconPath = new vscode.ThemeIcon('question', new vscode.ThemeColor('testing.iconQueued'));
        } else {
            this.iconPath = new vscode.ThemeIcon('pass', new vscode.ThemeColor('testing.iconPassed'));
        }
    }
}

class GateItem extends vscode.TreeItem {
    constructor(public readonly gate: GateResult) {
        super(gate.name, vscode.TreeItemCollapsibleState.None);
        this.iconPath = statusIcon(gate.status);
        if (gate.time_ms !== undefined) {
            this.description = `${gate.time_ms}ms`;
        }
        if (gate.counterexample) {
            this.tooltip = 'Counterexample: ' + JSON.stringify(gate.counterexample, null, 2);
        }
    }
}

function statusIcon(status: string): vscode.ThemeIcon {
    switch (status) {
        case 'pass':
            return new vscode.ThemeIcon('pass', new vscode.ThemeColor('testing.iconPassed'));
        case 'fail':
            return new vscode.ThemeIcon('error', new vscode.ThemeColor('testing.iconFailed'));
        default:
            return new vscode.ThemeIcon('question', new vscode.ThemeColor('testing.iconQueued'));
    }
}

export class EcResultTreeProvider implements vscode.TreeDataProvider<EcTreeItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<EcTreeItem | undefined>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private report: EcReport | null = null;
    private watcher: vscode.FileSystemWatcher | null = null;

    loadReport(reportPath: string): void {
        try {
            const content = fs.readFileSync(reportPath, 'utf-8');
            this.report = JSON.parse(content) as EcReport;
            this._onDidChangeTreeData.fire(undefined);

            // Watch for updates
            if (this.watcher) {
                this.watcher.dispose();
            }
            const dir = path.dirname(reportPath);
            this.watcher = vscode.workspace.createFileSystemWatcher(
                new vscode.RelativePattern(dir, 'ec_report.json')
            );
            this.watcher.onDidChange(() => this.loadReport(reportPath));
        } catch {
            vscode.window.showErrorMessage(`Failed to load EC report: ${reportPath}`);
        }
    }

    getTreeItem(element: EcTreeItem): vscode.TreeItem {
        return element;
    }

    getChildren(element?: EcTreeItem): EcTreeItem[] {
        if (!this.report) {
            return [];
        }

        if (!element) {
            return [new DesignItem(this.report)];
        }

        if (element instanceof DesignItem) {
            const items: EcTreeItem[] = [];
            if (this.report.outputs.length > 0) {
                items.push(new CategoryItem(`Outputs (${this.report.outputs.length})`, this.report.outputs));
            }
            if (this.report.latches.length > 0) {
                items.push(new CategoryItem(`Latches (${this.report.latches.length})`, this.report.latches));
            }
            if (this.report.self_test) {
                const st = this.report.self_test;
                const selfTestItem = new vscode.TreeItem(
                    `Self-Test: ${st.bugs_detected}/${st.bugs_injected} detected (${(st.detection_rate * 100).toFixed(0)}%)`,
                    vscode.TreeItemCollapsibleState.None
                );
                selfTestItem.iconPath = st.detection_rate >= 0.8
                    ? new vscode.ThemeIcon('pass', new vscode.ThemeColor('testing.iconPassed'))
                    : new vscode.ThemeIcon('warning', new vscode.ThemeColor('testing.iconQueued'));
                items.push(selfTestItem as EcTreeItem);
            }
            return items;
        }

        if (element instanceof CategoryItem) {
            return element.gates.map(g => new GateItem(g));
        }

        return [];
    }
}

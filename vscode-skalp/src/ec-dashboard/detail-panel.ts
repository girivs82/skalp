import * as vscode from 'vscode';

interface EcReport {
    design: string;
    status: string;
    outputs: GateResult[];
    latches: GateResult[];
    timing: {
        phase1_sim_ms: number;
        phase2_sat_ms: number;
        phase2c_sim_ms: number;
        phase3_selftest_ms: number;
        total_ms: number;
    };
    self_test?: {
        bugs_injected: number;
        bugs_detected: number;
        detection_rate: number;
    };
}

interface GateResult {
    name: string;
    status: string;
    time_ms?: number;
    counterexample?: Record<string, boolean>;
}

export class EcDetailPanel {
    private panel: vscode.WebviewPanel | null = null;

    show(report: EcReport): void {
        if (this.panel) {
            this.panel.reveal();
        } else {
            this.panel = vscode.window.createWebviewPanel(
                'skalpEcDetail',
                `EC: ${report.design}`,
                vscode.ViewColumn.One,
                { enableScripts: false }
            );
            this.panel.onDidDispose(() => { this.panel = null; });
        }

        this.panel.webview.html = this.getHtml(report);
    }

    private getHtml(report: EcReport): string {
        const statusBanner = this.getStatusBanner(report.status);
        const outputRows = report.outputs.map(g => this.gateRow(g)).join('');
        const latchRows = report.latches.map(g => this.gateRow(g)).join('');

        const totalSec = (report.timing.total_ms / 1000).toFixed(1);
        const phase1 = (report.timing.phase1_sim_ms / 1000).toFixed(1);
        const phase2 = (report.timing.phase2_sat_ms / 1000).toFixed(1);
        const phase2c = (report.timing.phase2c_sim_ms / 1000).toFixed(1);
        const phase3 = (report.timing.phase3_selftest_ms / 1000).toFixed(1);

        let selfTestHtml = '';
        if (report.self_test) {
            const st = report.self_test;
            const pct = (st.detection_rate * 100).toFixed(0);
            selfTestHtml = `
            <h3>Self-Test (Bug Injection)</h3>
            <table>
                <tr><td>Bugs Injected</td><td>${st.bugs_injected}</td></tr>
                <tr><td>Bugs Detected</td><td>${st.bugs_detected}</td></tr>
                <tr><td>Detection Rate</td><td>${pct}%</td></tr>
            </table>`;
        }

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>EC Report: ${report.design}</title>
    <style>
        body {
            font-family: var(--vscode-font-family);
            color: var(--vscode-editor-foreground);
            background: var(--vscode-editor-background);
            padding: 16px;
            font-size: 13px;
        }
        .banner { padding: 8px 16px; border-radius: 4px; margin-bottom: 16px; font-weight: bold; font-size: 16px; }
        .banner-pass { background: #1b5e20; color: #a5d6a7; }
        .banner-fail { background: #b71c1c; color: #ef9a9a; }
        .banner-unknown { background: #e65100; color: #ffcc80; }
        table { border-collapse: collapse; width: 100%; margin-bottom: 16px; }
        th, td { padding: 4px 12px; text-align: left; border-bottom: 1px solid var(--vscode-panel-border); }
        th { background: var(--vscode-sideBarSectionHeader-background); font-weight: bold; }
        .pass { color: #4caf50; }
        .fail { color: #f44336; }
        .unknown { color: #ff9800; }
        h2 { margin-top: 24px; border-bottom: 1px solid var(--vscode-panel-border); padding-bottom: 4px; }
        h3 { margin-top: 16px; }
        .timing { display: flex; gap: 24px; flex-wrap: wrap; }
        .timing-item { background: var(--vscode-sideBarSectionHeader-background); padding: 8px 16px; border-radius: 4px; }
        .timing-label { font-size: 11px; color: var(--vscode-descriptionForeground); }
        .timing-value { font-size: 18px; font-weight: bold; }
    </style>
</head>
<body>
    <div class="banner ${statusBanner}">${report.status.toUpperCase()}: ${report.design}</div>

    <h2>Timing (${totalSec}s total)</h2>
    <div class="timing">
        <div class="timing-item">
            <div class="timing-label">Phase 1: Simulation</div>
            <div class="timing-value">${phase1}s</div>
        </div>
        <div class="timing-item">
            <div class="timing-label">Phase 2: SAT</div>
            <div class="timing-value">${phase2}s</div>
        </div>
        <div class="timing-item">
            <div class="timing-label">Phase 2c: Miter Sim</div>
            <div class="timing-value">${phase2c}s</div>
        </div>
        <div class="timing-item">
            <div class="timing-label">Phase 3: Self-Test</div>
            <div class="timing-value">${phase3}s</div>
        </div>
    </div>

    <h2>Outputs (${report.outputs.length})</h2>
    <table>
        <tr><th>Gate</th><th>Status</th><th>Time</th></tr>
        ${outputRows}
    </table>

    <h2>Latches (${report.latches.length})</h2>
    <table>
        <tr><th>Gate</th><th>Status</th><th>Time</th></tr>
        ${latchRows}
    </table>

    ${selfTestHtml}
</body>
</html>`;
    }

    private getStatusBanner(status: string): string {
        if (status === 'pass') { return 'banner-pass'; }
        if (status === 'fail') { return 'banner-fail'; }
        return 'banner-unknown';
    }

    private gateRow(gate: GateResult): string {
        const cls = gate.status;
        const time = gate.time_ms !== undefined ? `${gate.time_ms}ms` : '-';
        return `<tr><td>${gate.name}</td><td class="${cls}">${gate.status.toUpperCase()}</td><td>${time}</td></tr>`;
    }
}

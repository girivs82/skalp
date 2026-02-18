/**
 * Debug adapter factory — creates inline DAP sessions for SKALP debugging.
 */

import * as vscode from 'vscode';
import { SkalpDebugSession } from './session';

export class SkalpDebugAdapterFactory implements vscode.DebugAdapterDescriptorFactory {
    private extensionPath: string;
    private waveformPostMessage: ((message: any) => void) | null = null;

    constructor(extensionPath: string) {
        this.extensionPath = extensionPath;
    }

    /**
     * Set a callback to post messages to the waveform viewer webview.
     * Called by the extension when the waveform provider is available.
     */
    setWaveformCallback(callback: ((message: any) => void) | null): void {
        this.waveformPostMessage = callback;
    }

    createDebugAdapterDescriptor(
        session: vscode.DebugSession
    ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
        console.log('[SKALP Debug] createDebugAdapterDescriptor called for', session.name);
        const debugSession = new SkalpDebugSession();
        debugSession.setExtensionPath(this.extensionPath);
        console.log('[SKALP Debug] extensionPath:', this.extensionPath);
        return new vscode.DebugAdapterInlineImplementation(debugSession);
    }
}

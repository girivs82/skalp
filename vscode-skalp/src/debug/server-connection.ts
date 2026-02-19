/**
 * Manages the skalp-debug subprocess and JSON-line communication.
 */

import * as cp from 'child_process';
import { EventEmitter } from 'events';
import { resolveBinaryPath } from '../extension';

export interface DebugServerEvent {
    event: string;
    [key: string]: any;
}

export class SkalpDebugServer extends EventEmitter {
    private process: cp.ChildProcess | null = null;
    private buffer: string = '';
    private extensionPath: string;

    constructor(extensionPath: string) {
        super();
        this.extensionPath = extensionPath;
    }

    /**
     * Start the skalp-debug subprocess.
     */
    start(workingDir: string): void {
        const cliPath = this.getDebugBinaryPath();
        console.log('[SKALP Debug] Spawning:', cliPath, 'cwd:', workingDir);

        this.process = cp.spawn(cliPath, [], {
            cwd: workingDir,
            stdio: ['pipe', 'pipe', 'pipe'],
            env: { ...process.env, SKALP_CACHE: '0' },
        });

        this.process.stdout!.on('data', (data: Buffer) => {
            this.onStdoutData(data);
        });

        this.process.stderr!.on('data', (data: Buffer) => {
            // Diagnostic output — forward to debug console
            this.emit('log', data.toString());
        });

        this.process.on('close', (code: number | null) => {
            this.emit('exit', code);
            this.process = null;
        });

        this.process.on('error', (err: Error) => {
            this.emit('error', err);
            this.process = null;
        });
    }

    /**
     * Send a JSON command to the debug server's stdin.
     */
    sendCommand(cmd: object): void {
        if (!this.process || !this.process.stdin) {
            throw new Error('Debug server not running');
        }
        const line = JSON.stringify(cmd) + '\n';
        this.process.stdin.write(line);
    }

    /**
     * Stop the debug server process.
     */
    stop(): void {
        if (this.process) {
            try {
                this.sendCommand({ cmd: 'disconnect' });
            } catch {
                // ignore — process may already be dead
            }
            setTimeout(() => {
                if (this.process) {
                    this.process.kill();
                    this.process = null;
                }
            }, 2000);
        }
    }

    /**
     * Wait for a specific event from the debug server.
     */
    waitForEvent(eventName: string, timeout: number = 30000): Promise<DebugServerEvent> {
        return new Promise((resolve, reject) => {
            const timer = setTimeout(() => {
                reject(new Error(`Timeout waiting for event: ${eventName}`));
            }, timeout);

            const handler = (event: DebugServerEvent) => {
                if (event.event === eventName) {
                    clearTimeout(timer);
                    this.removeListener('event', handler);
                    resolve(event);
                }
            };
            this.on('event', handler);
        });
    }

    private onStdoutData(data: Buffer): void {
        this.buffer += data.toString();

        // Process complete JSON lines
        let newlineIdx: number;
        while ((newlineIdx = this.buffer.indexOf('\n')) !== -1) {
            const line = this.buffer.substring(0, newlineIdx).trim();
            this.buffer = this.buffer.substring(newlineIdx + 1);

            if (line.length === 0) { continue; }

            try {
                const event: DebugServerEvent = JSON.parse(line);
                this.emit('event', event);
            } catch (e) {
                this.emit('log', `[skalp-debug] Invalid JSON: ${line}`);
            }
        }
    }

    private getDebugBinaryPath(): string {
        return resolveBinaryPath('skalp-debug', 'skalp-debug', this.extensionPath);
    }
}

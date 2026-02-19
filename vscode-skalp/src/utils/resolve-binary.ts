import * as path from 'path';
import * as fs from 'fs';

/**
 * Resolve a SKALP binary path with the following priority:
 * 1. User-configured explicit path (if not the default name)
 * 2. Sibling of another configured binary (same directory)
 * 3. Bundled binary inside extension (bin/)
 * 4. PATH lookup (covers cargo install, homebrew, etc.)
 * 5. Dev mode: repo-relative (../target/release/ or ../target/debug/)
 */
export function resolveBinaryPath(
    binaryName: string,
    configured: string,
    extensionPath: string,
    siblingHints?: string[]
): string {
    // 1. User set an explicit path — use it directly
    if (configured !== binaryName) {
        return configured;
    }

    // 2. Sibling of a configured binary (e.g., skalp-debug next to skalp)
    if (siblingHints) {
        for (const hint of siblingHints) {
            if (hint && hint !== path.basename(hint)) {
                const candidate = path.join(path.dirname(hint), binaryName);
                if (fs.existsSync(candidate)) {
                    return candidate;
                }
            }
        }
    }

    // 3. Bundled binary inside extension
    const bundled = path.join(extensionPath, 'bin', binaryName);
    if (fs.existsSync(bundled)) {
        return bundled;
    }

    // 4. PATH lookup — check if the binary is available on PATH
    try {
        const { execFileSync } = require('child_process');
        const which = process.platform === 'win32' ? 'where' : 'which';
        const result = execFileSync(which, [binaryName], { encoding: 'utf8', timeout: 3000 }).trim();
        if (result) {
            return result.split('\n')[0];
        }
    } catch {
        // not on PATH
    }

    // 5. Dev mode: repo-relative
    const repoRoot = path.resolve(extensionPath, '..');
    const releaseBin = path.join(repoRoot, 'target', 'release', binaryName);
    if (fs.existsSync(releaseBin)) {
        return releaseBin;
    }
    const debugBin = path.join(repoRoot, 'target', 'debug', binaryName);
    if (fs.existsSync(debugBin)) {
        return debugBin;
    }

    // Fallback: hope it's on PATH (will fail with a clear error if not)
    return binaryName;
}

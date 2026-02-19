/**
 * Generates a Rust test file using the skalp_testing::testbench API
 * from a parsed SKALP entity declaration.
 */

import { SkalpEntity, SkalpPort } from './entity-parser';

/**
 * Convert PascalCase entity name to snake_case.
 */
function toSnakeCase(name: string): string {
    return name
        .replace(/([A-Z]+)([A-Z][a-z])/g, '$1_$2')
        .replace(/([a-z\d])([A-Z])/g, '$1_$2')
        .toLowerCase();
}

/**
 * Generate a Rust testbench file for the given entity.
 *
 * @param entity - Parsed entity with ports
 * @param sourceRelPath - Relative path from Cargo project root to the .sk source file (e.g., "src/main.sk")
 */
export function generateTestbench(entity: SkalpEntity, sourceRelPath: string): string {
    const snakeName = toSnakeCase(entity.name);
    const inputPorts = entity.ports.filter(p => p.direction === 'in' && p.type !== 'clock');
    const outputPorts = entity.ports.filter(p => p.direction === 'out');
    const hasReset = inputPorts.some(p => p.type === 'reset' || p.type.startsWith('reset('));
    const nonResetInputs = inputPorts.filter(p => p.type !== 'reset' && !p.type.startsWith('reset('));
    const resetPorts = inputPorts.filter(p => p.type === 'reset' || p.type.startsWith('reset('));

    const lines: string[] = [];

    // Imports
    lines.push('use skalp_testing::testbench::*;');
    lines.push('');

    // Test function
    lines.push('#[tokio::test]');
    lines.push(`async fn test_${snakeName}() {`);

    // Testbench constructor
    lines.push('    let mut tb = Testbench::behavioral_with_top(');
    lines.push(`        concat!(env!("CARGO_MANIFEST_DIR"), "/${sourceRelPath}"),`);
    lines.push(`        "${entity.name}",`);
    lines.push('    ).await.expect("Failed to create testbench");');
    lines.push('');

    // Reset sequence
    if (hasReset) {
        lines.push('    // Reset sequence');
        for (const rst of resetPorts) {
            lines.push(`    tb.set("${rst.name}", 1u8);`);
        }
        lines.push('    tb.clock(5).await;');
        for (const rst of resetPorts) {
            lines.push(`    tb.set("${rst.name}", 0u8);`);
        }
        lines.push('    tb.clock(1).await;');
        lines.push('');
    }

    // Input stubs
    if (nonResetInputs.length > 0) {
        lines.push('    // --- Inputs ---');
        for (const port of nonResetInputs) {
            const suffix = rustLiteralSuffix(port.rustType);
            const comment = port.isCustomType
                ? `    // TODO: set correct type for ${port.type}`
                : `    // ${port.type}`;
            lines.push(`    tb.set("${port.name}", 0${suffix});${comment}`);
        }
        lines.push('');
    }

    // Simulation
    lines.push('    // Run simulation');
    lines.push('    tb.clock(10).await;');
    lines.push('');

    // Output stubs
    if (outputPorts.length > 0) {
        lines.push('    // --- Outputs ---');
        for (const port of outputPorts) {
            const comment = port.isCustomType
                ? `    // TODO: set correct type for ${port.type}`
                : `    // ${port.type}`;
            lines.push(`    let _${port.name}: ${port.rustType} = tb.get_as("${port.name}").await;${comment}`);
        }
        lines.push('');
    }

    // Waveform export
    lines.push('    // Export waveform');
    lines.push(`    tb.export_waveform(concat!(env!("CARGO_MANIFEST_DIR"), "/build/test_${snakeName}.skw.gz")).ok();`);

    lines.push('}');
    lines.push('');

    return lines.join('\n');
}

/**
 * Get the Rust literal suffix for a type (e.g., "u8" -> "u8", "i32" -> "i32").
 */
function rustLiteralSuffix(rustType: string): string {
    return rustType; // "0u8", "0u16", "0i32", etc.
}

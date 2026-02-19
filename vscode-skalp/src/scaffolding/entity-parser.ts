/**
 * Lightweight regex-based parser for SKALP entity declarations.
 * Extracts entity name, generics, and port list from .sk source text.
 */

export interface SkalpPort {
    direction: 'in' | 'out' | 'inout';
    name: string;
    type: string;         // raw type string: "clock", "nat[8]", "BmsData"
    width: number | null;  // extracted width if numeric type, null for custom
    rustType: string;      // mapped Rust type: "u8", "u32", "u64", "i32"
    isCustomType: boolean; // true if type couldn't be mapped automatically
}

export interface SkalpEntity {
    name: string;
    generics: string | null;  // raw generic params string
    ports: SkalpPort[];
}

/**
 * Map a SKALP type string to a Rust type and width.
 */
function mapType(typeStr: string): { rustType: string; width: number | null; isCustom: boolean } {
    const t = typeStr.trim();

    // clock — handled separately in testbench (tb.clock()), but include for completeness
    if (t === 'clock') {
        return { rustType: 'u8', width: 1, isCustom: false };
    }

    // reset variants
    if (t === 'reset' || t.startsWith('reset(')) {
        return { rustType: 'u8', width: 1, isCustom: false };
    }

    // bare bit
    if (t === 'bit' || t === 'bool') {
        return { rustType: 'u8', width: 1, isCustom: false };
    }

    // bit[N] or bit<N>
    const bitMatch = t.match(/^bit\s*[\[<]\s*(\d+)\s*[\]>]$/);
    if (bitMatch) {
        const n = parseInt(bitMatch[1], 10);
        return { rustType: unsignedRustType(n), width: n, isCustom: false };
    }

    // nat[N] or nat<N>
    const natMatch = t.match(/^nat\s*[\[<]\s*(\d+)\s*[\]>]$/);
    if (natMatch) {
        const n = parseInt(natMatch[1], 10);
        return { rustType: unsignedRustType(n), width: n, isCustom: false };
    }

    // int[N] or int<N>
    const intMatch = t.match(/^int\s*[\[<]\s*(\d+)\s*[\]>]$/);
    if (intMatch) {
        const n = parseInt(intMatch[1], 10);
        return { rustType: signedRustType(n), width: n, isCustom: false };
    }

    // fp32, fp16, fp64
    const fpMatch = t.match(/^fp(\d+)$/);
    if (fpMatch) {
        const n = parseInt(fpMatch[1], 10);
        return { rustType: unsignedRustType(n), width: n, isCustom: false };
    }

    // Custom / struct type — default to u32
    return { rustType: 'u32', width: null, isCustom: true };
}

function unsignedRustType(bits: number): string {
    if (bits <= 8) { return 'u8'; }
    if (bits <= 16) { return 'u16'; }
    if (bits <= 32) { return 'u32'; }
    return 'u64';
}

function signedRustType(bits: number): string {
    if (bits <= 8) { return 'i8'; }
    if (bits <= 16) { return 'i16'; }
    if (bits <= 32) { return 'i32'; }
    return 'i64';
}

/**
 * Parse a single port line like `in enable: bit` or `out count: nat[8]`.
 */
function parsePortLine(line: string): SkalpPort | null {
    const trimmed = line.trim();
    // Remove trailing comma if present
    const cleaned = trimmed.replace(/,\s*$/, '');

    const match = cleaned.match(/^(in|out|inout)\s+(\w+)\s*:\s*(.+)$/);
    if (!match) {
        return null;
    }

    const direction = match[1] as 'in' | 'out' | 'inout';
    const name = match[2];
    const rawType = match[3].trim();
    const { rustType, width, isCustom } = mapType(rawType);

    return { direction, name, type: rawType, width, rustType, isCustomType: isCustom };
}

/**
 * Parse all entities from a SKALP source file.
 * Returns an array of parsed entities.
 */
export function parseEntities(source: string): SkalpEntity[] {
    const entities: SkalpEntity[] = [];
    const lines = source.split('\n');

    let i = 0;
    while (i < lines.length) {
        // Match entity declaration: `entity Name` or `entity Name<T, N: nat>`
        const entityMatch = lines[i].match(/^\s*entity\s+(\w+)\s*(<[^>]+>)?\s*\{?\s*$/);
        if (!entityMatch) {
            i++;
            continue;
        }

        const entityName = entityMatch[1];
        const generics = entityMatch[2] ? entityMatch[2].slice(1, -1) : null;
        const ports: SkalpPort[] = [];

        // If opening brace is on this line, start scanning ports from next line
        // If not, look for it on the next line
        let braceFound = lines[i].includes('{');
        i++;

        if (!braceFound) {
            // Skip to opening brace
            while (i < lines.length) {
                if (lines[i].includes('{')) {
                    braceFound = true;
                    i++;
                    break;
                }
                i++;
            }
        }

        if (!braceFound) {
            continue;
        }

        // Scan port lines until closing brace
        let braceDepth = 1;
        while (i < lines.length && braceDepth > 0) {
            const line = lines[i];

            // Track brace depth for nested types
            for (const ch of line) {
                if (ch === '{') { braceDepth++; }
                if (ch === '}') { braceDepth--; }
            }

            if (braceDepth <= 0) {
                break;
            }

            // Only parse ports at top level (depth 1)
            if (braceDepth === 1) {
                const port = parsePortLine(line);
                if (port) {
                    ports.push(port);
                }
            }

            i++;
        }

        entities.push({ name: entityName, generics, ports });
        i++;
    }

    return entities;
}

/**
 * Parse the first entity from source. Convenience wrapper.
 */
export function parseEntity(source: string): SkalpEntity | null {
    const entities = parseEntities(source);
    return entities.length > 0 ? entities[0] : null;
}

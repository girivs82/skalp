#!/usr/bin/env node
// Offline test for schematic parsing — exercises parseEntityAtCursor + extractSchematic logic
// Run: node vscode-skalp/test/test_schematic_parsing.js

const fs = require('fs');
const path = require('path');

// ── Extracted parsing logic (mirrors provider.ts, no vscode dependency) ──

function inferWidth(typeStr) {
    typeStr = typeStr.trim();
    if (typeStr === 'bit' || typeStr === 'bool' || typeStr === 'clock' || typeStr.startsWith('reset')) { return 1; }
    const bitMatch = typeStr.match(/bit<(\d+)>/);
    if (bitMatch) { return parseInt(bitMatch[1]); }
    const natMatch = typeStr.match(/nat<(\d+)>|nat\[(\d+)\]/);
    if (natMatch) { return parseInt(natMatch[1] || natMatch[2]); }
    const intMatch = typeStr.match(/int<(\d+)>|int\[(\d+)\]/);
    if (intMatch) { return parseInt(intMatch[1] || intMatch[2]); }
    if (typeStr === 'fp32') { return 32; }
    if (typeStr === 'fp16') { return 16; }
    if (typeStr === 'fp64') { return 64; }
    if (/^[A-Z]\w+$/.test(typeStr)) { return 8; }
    return 1;
}

function parseEntityAtCursor(source, cursorLine) {
    const lines = source.split('\n');

    // Find all entity declarations with their line ranges
    const entities = [];
    let eStart = -1;
    let eName = '';
    let braceDepth = 0;

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const entityMatch = line.match(/^\s*(?:pub\s+)?entity\s+(\w+)/);
        if (entityMatch && eStart < 0) {
            eStart = i;
            eName = entityMatch[1];
            braceDepth = 0;
        }
        if (eStart >= 0) {
            for (const ch of line) {
                if (ch === '{') { braceDepth++; }
                if (ch === '}') {
                    braceDepth--;
                    if (braceDepth === 0) {
                        entities.push({ name: eName, start: eStart, end: i });
                        eStart = -1;
                        break;
                    }
                }
            }
        }
    }

    // Also find all impl blocks and their line ranges
    const implBlocks = [];
    let iStart = -1;
    let iName = '';
    braceDepth = 0;

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const implMatch = line.match(/^\s*impl\s+(\w+)/);
        if (implMatch && iStart < 0) {
            iStart = i;
            iName = implMatch[1];
            braceDepth = 0;
        }
        if (iStart >= 0) {
            for (const ch of line) {
                if (ch === '{') { braceDepth++; }
                if (ch === '}') {
                    braceDepth--;
                    if (braceDepth === 0) {
                        implBlocks.push({ name: iName, start: iStart, end: i });
                        iStart = -1;
                        break;
                    }
                }
            }
        }
    }

    // Determine which entity to show
    let targetEntity = null;
    let targetEntityStart = -1;
    let targetEntityEnd = -1;

    for (const ent of entities) {
        if (cursorLine >= ent.start && cursorLine <= ent.end) {
            targetEntity = ent.name;
            targetEntityStart = ent.start;
            targetEntityEnd = ent.end;
            break;
        }
    }

    if (!targetEntity) {
        for (const impl of implBlocks) {
            if (cursorLine >= impl.start && cursorLine <= impl.end) {
                targetEntity = impl.name;
                const entDecl = entities.find(e => e.name === impl.name);
                if (entDecl) {
                    targetEntityStart = entDecl.start;
                    targetEntityEnd = entDecl.end;
                }
                break;
            }
        }
    }

    if (!targetEntity && entities.length > 0) {
        targetEntity = entities[0].name;
        targetEntityStart = entities[0].start;
        targetEntityEnd = entities[0].end;
    }

    if (!targetEntity || targetEntityStart < 0) { return null; }

    const implBlock = implBlocks.find(ib => ib.name === targetEntity);

    return {
        entityName: targetEntity,
        entityStart: targetEntityStart,
        entityEnd: targetEntityEnd,
        implBlock: implBlock || null,
        allEntities: entities,
        allImpls: implBlocks,
        schematic: extractSchematic(targetEntity, lines, targetEntityStart, targetEntityEnd, implBlock || null)
    };
}

function extractSchematic(entityName, lines, entityStart, entityEnd, implBlock) {
    const ports = [];
    const signals = [];
    const instances = [];
    const assignments = [];

    // Parse ports from entity declaration
    for (let i = entityStart; i <= entityEnd; i++) {
        const line = lines[i].trim();
        if (line.startsWith('//')) { continue; }
        const cleanLine = line.replace(/\/\/.*$/, '').trim();
        const portMatch = cleanLine.match(/^\s*(in|out|inout)\s+(\w+)\s*:\s*(.+?)[\s,;]*$/);
        if (portMatch) {
            const width = inferWidth(portMatch[3]);
            ports.push({
                name: portMatch[2],
                direction: portMatch[1],
                type: portMatch[3].trim(),
                width,
                line: i
            });
        }
    }

    // Parse impl block
    let onBlockInputPorts = new Set();
    let onBlockOutputPorts = new Set();
    let onBlockSignals = new Set();
    if (implBlock) {
        const result = parseImplBlock(lines, implBlock.start, implBlock.end, ports, signals, instances, assignments);
        onBlockInputPorts = result.onBlockInputPorts;
        onBlockOutputPorts = result.onBlockOutputPorts;
        onBlockSignals = result.onBlockSignals;
    }

    // Scan combinational assignments for field-access input port references
    const inputPortNames = new Set(ports.filter(p => p.direction === 'in').map(p => p.name));
    for (const a of assignments) {
        for (const portName of inputPortNames) {
            if (a.rhs.includes(portName + '.')) {
                onBlockInputPorts.add(portName);
            }
        }
    }

    const portNames = new Set(ports.map(p => p.name));
    const outputPortNames = new Set(ports.filter(p => p.direction === 'out').map(p => p.name));
    const logicOutputPorts = new Set(onBlockOutputPorts);
    for (const a of assignments) {
        if (!a.isOutputPort) { continue; }
        const outName = a.lhs.split('.')[0];
        if (logicOutputPorts.has(outName)) { continue; }
        const rhs = a.rhs.trim();
        if (!/^\w+$/.test(rhs)) {
            logicOutputPorts.add(outName);
        }
    }

    return {
        entity_name: entityName,
        ports,
        signals,
        instances,
        assignments,
        onBlockInputPorts: [...onBlockInputPorts],
        onBlockOutputPorts: [...onBlockOutputPorts],
        onBlockSignals: [...onBlockSignals],
        logicOutputPorts: [...logicOutputPorts],
    };
}

function parseImplBlock(lines, implStart, implEnd, ports, signals, instances, assignments) {
    const portNames = new Set(ports.map(p => p.name));
    const inputPortNames = new Set(ports.filter(p => p.direction === 'in').map(p => p.name));
    const outputPortNames = new Set(ports.filter(p => p.direction === 'out').map(p => p.name));

    const instanceRanges = [];
    const onBlockRanges = [];
    const onBlockInputPorts = new Set();
    const onBlockOutputPorts = new Set();
    const onBlockSignals = new Set();

    // First pass: find on(...) blocks
    let i = implStart + 1;
    while (i <= implEnd) {
        const line = lines[i].trim();
        if (line.match(/^\s*on\s*\(/)) {
            const blockStart = i;
            let depth = 0;
            for (let j = i; j <= implEnd; j++) {
                for (const ch of lines[j]) {
                    if (ch === '{') { depth++; }
                    if (ch === '}') {
                        depth--;
                        if (depth === 0) {
                            onBlockRanges.push({ start: blockStart, end: j });
                            i = j;
                            break;
                        }
                    }
                }
                if (depth === 0) { break; }
            }
        }
        i++;
    }

    // Scan on() block contents for port references
    for (const range of onBlockRanges) {
        for (let j = range.start; j <= range.end; j++) {
            const line = lines[j].replace(/\/\/.*$/, '').trim();
            if (!line) { continue; }
            const assignMatch = line.match(/^(\w+)\s*=/);
            if (assignMatch) {
                const lhsName = assignMatch[1];
                if (outputPortNames.has(lhsName)) {
                    onBlockOutputPorts.add(lhsName);
                } else if (!inputPortNames.has(lhsName) && lhsName !== 'if' &&
                           lhsName !== 'match' && lhsName !== 'else') {
                    onBlockSignals.add(lhsName);
                }
            }
            const tokens = line.match(/\b\w+\b/g) || [];
            for (const tok of tokens) {
                if (inputPortNames.has(tok)) {
                    onBlockInputPorts.add(tok);
                }
            }
        }
    }

    // Second pass: find instances
    i = implStart + 1;
    while (i <= implEnd) {
        const line = lines[i];
        const trimmed = line.trim();
        if (trimmed.startsWith('//')) { i++; continue; }

        const instMatch = trimmed.match(/^\s*let\s+(\w+)\s*=\s*(\w+)(?:::<[^>]*>)?\s*\{/);
        if (instMatch) {
            const instName = instMatch[1];
            const instType = instMatch[2];
            const connections = [];
            const instStartLine = i;

            let depth = 0;
            let foundClose = false;
            for (let j = i; j <= implEnd && !foundClose; j++) {
                const jLine = lines[j];
                for (let k = 0; k < jLine.length; k++) {
                    if (jLine[k] === '{') { depth++; }
                    if (jLine[k] === '}') {
                        depth--;
                        if (depth === 0) {
                            instanceRanges.push({ start: i, end: j });
                            i = j;
                            foundClose = true;
                            break;
                        }
                    }
                }
            }

            for (let j = instStartLine; j <= (instanceRanges.length > 0 ? instanceRanges[instanceRanges.length - 1].end : i); j++) {
                let connLine = lines[j].replace(/\/\/.*$/, '').trim();
                // Skip the `let name = Type {` line itself
                if (connLine.match(/^\s*let\s+\w+\s*=/)) { continue; }

                // Match all `port: expr` pairs using global regex
                const pairRegex = /(\w+)\s*:\s*([^,}]+)/g;
                let pairMatch;
                while ((pairMatch = pairRegex.exec(connLine)) !== null) {
                    const connPort = pairMatch[1];
                    const connExpr = pairMatch[2].trim();
                    if (connPort === instType) { continue; }
                    connections.push({ port: connPort, signal: connExpr });
                }
            }

            instances.push({
                name: instName,
                entity_type: instType,
                connections,
                line: instStartLine
            });
        }

        i++;
    }

    // Third pass: signals and assignments
    for (let li = implStart + 1; li < implEnd; li++) {
        if (instanceRanges.some(r => li >= r.start && li <= r.end)) { continue; }
        if (onBlockRanges.some(r => li >= r.start && li <= r.end)) { continue; }

        const line = lines[li].replace(/\/\/.*$/, '').trim();
        if (!line || line === '{' || line === '}') { continue; }

        const sigMatch = line.match(/^\s*signal\s+(\w+)\s*:\s*(.+?)[\s;]*$/);
        if (sigMatch) {
            const width = inferWidth(sigMatch[2]);
            signals.push({ name: sigMatch[1], type: sigMatch[2].trim(), width, line: li });
            continue;
        }

        if (line.startsWith('let ') || line.startsWith('signal ') || line.startsWith('on') ||
            line.startsWith('if ') || line.startsWith('match ') || line.startsWith('generate ') ||
            line.startsWith('impl ') || line.startsWith('entity ') || line.startsWith('}') ||
            line.startsWith('{') || line.startsWith('else')) { continue; }

        const assignMatch = line.match(/^(\w+(?:\.\w+)*)\s*=\s*(.+)$/);
        if (assignMatch) {
            const lhs = assignMatch[1].trim();
            let rhs = assignMatch[2].trim();
            rhs = rhs.replace(/[;,]\s*$/, '').trim();

            if (rhs.endsWith('{')) {
                let depth = 1;
                for (let sli = li + 1; sli < implEnd && depth > 0; sli++) {
                    const sLine = lines[sli].replace(/\/\/.*$/, '').trim();
                    for (const ch of sLine) {
                        if (ch === '{') { depth++; }
                        if (ch === '}') { depth--; }
                    }
                    if (depth > 0) {
                        rhs += ' ' + sLine.replace(/[,}]/g, '');
                    }
                }
            }

            assignments.push({
                lhs,
                rhs,
                line: li,
                isOutputPort: portNames.has(lhs.split('.')[0])
            });
        }
    }

    return { onBlockInputPorts, onBlockOutputPorts, onBlockSignals };
}

// ── Test runner ──

let passed = 0;
let failed = 0;

function assert(cond, msg) {
    if (!cond) {
        console.error('  FAIL:', msg);
        failed++;
    } else {
        passed++;
    }
}

// ── Test: protection.sk ──

const protectionPath = path.resolve('/Users/girivs/src/design/sangam/src/lib/protection.sk');
if (!fs.existsSync(protectionPath)) {
    console.error('Cannot find protection.sk at:', protectionPath);
    process.exit(1);
}

const protectionSource = fs.readFileSync(protectionPath, 'utf8');

console.log('\n[TEST] Parse protection.sk — entity detection');
{
    const result = parseEntityAtCursor(protectionSource, 0);
    assert(result !== null, 'parseEntityAtCursor should not return null');
    if (result) {
        console.log(`  Found ${result.allEntities.length} entities:`);
        for (const e of result.allEntities) {
            console.log(`    ${e.name} (lines ${e.start}-${e.end})`);
        }
        console.log(`  Found ${result.allImpls.length} impl blocks:`);
        for (const ib of result.allImpls) {
            console.log(`    ${ib.name} (lines ${ib.start}-${ib.end})`);
        }
        assert(result.allEntities.length >= 6, `expected >= 6 entities (ThresholdComparator, FaultLatch, VoltageProtection, CurrentProtection, TemperatureProtection, ProtectionSystem), got ${result.allEntities.length}`);
        assert(result.allImpls.length >= 6, `expected >= 6 impl blocks, got ${result.allImpls.length}`);

        // Default (cursor at 0) should pick first entity
        assert(result.entityName === 'ThresholdComparator', `default entity should be ThresholdComparator, got ${result.entityName}`);
    }
}

console.log('\n[TEST] Parse protection.sk — ProtectionSystem at cursor');
{
    // Find ProtectionSystem entity line
    const lines = protectionSource.split('\n');
    let psLine = -1;
    for (let i = 0; i < lines.length; i++) {
        if (lines[i].includes('pub entity ProtectionSystem')) {
            psLine = i;
            break;
        }
    }
    assert(psLine >= 0, 'found ProtectionSystem entity line');

    if (psLine >= 0) {
        console.log(`  ProtectionSystem entity at line ${psLine}`);
        const result = parseEntityAtCursor(protectionSource, psLine);
        assert(result !== null, 'parseEntityAtCursor(ProtectionSystem) should not return null');
        if (result) {
            assert(result.entityName === 'ProtectionSystem', `entity should be ProtectionSystem, got ${result.entityName}`);
            const s = result.schematic;

            console.log(`\n  Ports (${s.ports.length}):`);
            for (const p of s.ports) {
                console.log(`    ${p.direction} ${p.name}: ${p.type} [${p.width}]`);
            }

            console.log(`\n  Signals (${s.signals.length}):`);
            for (const sig of s.signals) {
                console.log(`    ${sig.name}: ${sig.type} [${sig.width}]`);
            }

            console.log(`\n  Instances (${s.instances.length}):`);
            for (const inst of s.instances) {
                console.log(`    ${inst.name}: ${inst.entity_type} (${inst.connections.length} connections)`);
                for (const c of inst.connections) {
                    console.log(`      ${c.port}: ${c.signal}${c.direction ? ' [' + c.direction + ']' : ''}`);
                }
            }

            console.log(`\n  Assignments (${s.assignments.length}):`);
            for (const a of s.assignments) {
                const tag = a.isOutputPort ? ' [OUTPUT]' : '';
                console.log(`    ${a.lhs} = ${a.rhs.substring(0, 60)}${a.rhs.length > 60 ? '...' : ''}${tag}`);
            }

            console.log(`\n  On-block input ports: [${s.onBlockInputPorts.join(', ')}]`);
            console.log(`  On-block output ports: [${s.onBlockOutputPorts.join(', ')}]`);
            console.log(`  On-block signals: [${s.onBlockSignals.join(', ')}]`);
            console.log(`  Logic output ports: [${s.logicOutputPorts.join(', ')}]`);

            // Assertions
            assert(s.ports.length >= 10, `ProtectionSystem should have >= 10 ports, got ${s.ports.length}`);
            assert(s.instances.length >= 8, `ProtectionSystem should have >= 8 instances (3 prot + 5 latches), got ${s.instances.length}`);

            // Check key instances
            const instNames = s.instances.map(i => i.name);
            for (const name of ['voltage_prot', 'current_prot', 'temp_prot',
                                'hw_ov_latch', 'hw_uv_latch', 'hw_oc_latch', 'hw_ot_latch', 'desat_latch']) {
                assert(instNames.includes(name), `instance ${name} found`);
            }

            // Check that assignments were parsed
            assert(s.assignments.length >= 3, `should have >= 3 combinational assignments, got ${s.assignments.length}`);
        }
    }
}

console.log('\n[TEST] Parse protection.sk — cursor in impl block');
{
    // Find ProtectionSystem impl line
    const lines = protectionSource.split('\n');
    let implLine = -1;
    for (let i = 0; i < lines.length; i++) {
        if (lines[i].match(/^\s*impl\s+ProtectionSystem/)) {
            implLine = i;
            break;
        }
    }
    assert(implLine >= 0, 'found ProtectionSystem impl line');

    if (implLine >= 0) {
        // Place cursor in the middle of the impl block
        const result = parseEntityAtCursor(protectionSource, implLine + 10);
        assert(result !== null, 'cursor in ProtectionSystem impl → not null');
        if (result) {
            assert(result.entityName === 'ProtectionSystem', `cursor in impl → entity is ProtectionSystem, got ${result.entityName}`);
        }
    }
}

// ── Test: main.sk (regression — single connection per line) ──

const mainPath = path.resolve('/Users/girivs/src/design/sangam/src/battery_dcdc/main.sk');
if (fs.existsSync(mainPath)) {
    const mainSource = fs.readFileSync(mainPath, 'utf8');

    console.log('\n[TEST] Parse main.sk — BatteryDcdcController');
    {
        const result = parseEntityAtCursor(mainSource, 0);
        assert(result !== null, 'parseEntityAtCursor(main.sk) should not return null');
        if (result) {
            console.log(`  Entity: ${result.entityName}`);
            const s = result.schematic;
            console.log(`  Ports: ${s.ports.length}, Instances: ${s.instances.length}, Assignments: ${s.assignments.length}`);

            // Verify no connection has comma-separated values (regression check)
            let badConns = 0;
            for (const inst of s.instances) {
                for (const c of inst.connections) {
                    if (c.signal.includes(', ') && c.signal.includes(':')) {
                        console.error(`    BAD: ${inst.name}.${c.port} = "${c.signal}"`);
                        badConns++;
                    }
                }
            }
            assert(badConns === 0, `no multi-connection signals (got ${badConns} bad)`);

            // Check that instances have reasonable connection counts
            for (const inst of s.instances) {
                assert(inst.connections.length > 0, `${inst.name} has connections`);
                console.log(`    ${inst.name}: ${inst.entity_type} (${inst.connections.length} conns)`);
            }
        }
    }
} else {
    console.log('\n[SKIP] main.sk not found — skipping regression test');
}

// ── Summary ──

console.log(`\n${'─'.repeat(40)}`);
console.log(`Results: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);

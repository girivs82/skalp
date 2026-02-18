#!/usr/bin/env node
// Offline test for schematic ELK layout — mirrors computeLayout() from schematic.js
// Run: node vscode-skalp/test/test_schematic_layout.js

const ELK = require('elkjs');

// Layout constants (must match schematic.js)
const PORT_SPACING = 24;
const ENTITY_PADDING = 16;
const INSTANCE_PADDING = 12;
const COLUMN_GAP = 180;
const ROW_GAP = 30;
const HEADER_HEIGHT = 22;
const FONT_SIZE = 11;
const ENTITY_BOX_MIN_WIDTH = 180;
const INSTANCE_BOX_MIN_WIDTH = 140;

function measureTextWidth(text, fontSize) {
    return text.length * (fontSize * 0.62);
}

function computeBoxSize(name, entityType, inPorts, outPorts) {
    const portCount = Math.max(inPorts.length, outPorts.length, 1);
    const h = HEADER_HEIGHT + INSTANCE_PADDING * 2 + portCount * PORT_SPACING;
    let maxIn = 0, maxOut = 0;
    for (const p of inPorts) { maxIn = Math.max(maxIn, measureTextWidth(p, FONT_SIZE)); }
    for (const p of outPorts) { maxOut = Math.max(maxOut, measureTextWidth(p, FONT_SIZE)); }
    const displayLabel = name === 'logic' ? 'State Machine' : (entityType || name);
    const nameW = measureTextWidth(displayLabel, FONT_SIZE + 1);
    const w = Math.max(INSTANCE_BOX_MIN_WIDTH, maxIn + maxOut + 40, nameW + 30);
    return { w, h };
}

// Core layout logic extracted from schematic.js computeLayout()
async function computeLayout(schematicData) {
    const layoutElements = [];
    const layoutWires = [];

    const data = schematicData;
    const inputPorts = data.ports.filter(p => p.direction === 'in');
    const outputPorts = data.ports.filter(p => p.direction === 'out');

    const elkNodes = [];
    const elkEdges = [];
    const netWidthMap = new Map();
    const netNameMap = new Map();

    // Entity input port bar
    const entityInPortCount = Math.max(inputPorts.length, 1);
    const entityInH = HEADER_HEIGHT + ENTITY_PADDING * 2 + entityInPortCount * PORT_SPACING;
    let maxInPortNameW = 0;
    for (const p of inputPorts) { maxInPortNameW = Math.max(maxInPortNameW, measureTextWidth(p.name, FONT_SIZE)); }
    const entityInW = Math.max(ENTITY_BOX_MIN_WIDTH, maxInPortNameW + 50);

    elkNodes.push({
        id: '__entity_in__',
        width: entityInW,
        height: entityInH,
        layoutOptions: {
            'elk.layered.layerConstraint': 'FIRST',
            'elk.portConstraints': 'FIXED_ORDER'
        },
        ports: inputPorts.map((p, i) => ({
            id: '__entity_in___' + p.name + '_out',
            width: 1, height: 1,
            layoutOptions: { 'elk.port.side': 'EAST', 'elk.port.index': String(i) }
        }))
    });

    // Instance boxes
    for (const inst of data.instances) {
        const inConns = inst.connections.filter(c => c.direction === 'in' || !c.direction);
        const outConns = inst.connections.filter(c => c.direction === 'out');
        const size = computeBoxSize(inst.name, inst.entity_type, inConns.map(c => c.port), outConns.map(c => c.port));

        const ports = [];
        for (let j = 0; j < inConns.length; j++) {
            ports.push({
                id: inst.name + '_' + inConns[j].port + '_in',
                width: 1, height: 1,
                layoutOptions: { 'elk.port.side': 'WEST', 'elk.port.index': String(j) }
            });
        }
        for (let j = 0; j < outConns.length; j++) {
            ports.push({
                id: inst.name + '_' + outConns[j].port + '_out',
                width: 1, height: 1,
                layoutOptions: { 'elk.port.side': 'EAST', 'elk.port.index': String(j) }
            });
        }

        elkNodes.push({
            id: inst.name,
            width: size.w, height: size.h,
            ports,
            layoutOptions: { 'elk.portConstraints': 'FIXED_ORDER' }
        });
    }

    // Entity output port bar
    const entityOutPortCount = Math.max(outputPorts.length, 1);
    const entityOutH = HEADER_HEIGHT + ENTITY_PADDING * 2 + entityOutPortCount * PORT_SPACING;
    let maxOutPortNameW = 0;
    for (const p of outputPorts) { maxOutPortNameW = Math.max(maxOutPortNameW, measureTextWidth(p.name, FONT_SIZE)); }
    const entityOutW = Math.max(ENTITY_BOX_MIN_WIDTH, maxOutPortNameW + 50);

    elkNodes.push({
        id: '__entity_out__',
        width: entityOutW,
        height: entityOutH,
        layoutOptions: {
            'elk.layered.layerConstraint': 'LAST',
            'elk.portConstraints': 'FIXED_ORDER'
        },
        ports: outputPorts.map((p, i) => ({
            id: '__entity_out___' + p.name + '_in',
            width: 1, height: 1,
            layoutOptions: { 'elk.port.side': 'WEST', 'elk.port.index': String(i) }
        }))
    });

    // Valid port IDs
    const validPortIds = new Set();
    for (const node of elkNodes) {
        for (const port of (node.ports || [])) {
            validPortIds.add(port.id);
        }
    }

    // Nets → ELK edges
    let edgeIdx = 0;
    let skippedEdges = 0;
    for (const net of (data.nets || [])) {
        const driver = net.driver;
        let sourcePortId;
        if (driver.type === 'entity_port') {
            sourcePortId = '__entity_in___' + driver.port + '_out';
        } else {
            sourcePortId = driver.name + '_' + driver.port + '_out';
        }

        if (!validPortIds.has(sourcePortId)) { skippedEdges++; continue; }

        for (const sink of net.sinks) {
            let targetPortId;
            if (sink.type === 'entity_port') {
                targetPortId = '__entity_out___' + sink.port + '_in';
            } else {
                targetPortId = sink.name + '_' + sink.port + '_in';
            }

            if (!validPortIds.has(targetPortId)) { skippedEdges++; continue; }

            const edgeId = 'e' + edgeIdx++;
            netWidthMap.set(edgeId, net.width);
            netNameMap.set(edgeId, net.name);
            elkEdges.push({ id: edgeId, sources: [sourcePortId], targets: [targetPortId] });
        }
    }

    const graph = {
        id: 'root',
        layoutOptions: {
            'elk.algorithm': 'layered',
            'elk.direction': 'RIGHT',
            'elk.layered.spacing.nodeNodeBetweenLayers': String(COLUMN_GAP),
            'elk.spacing.nodeNode': String(ROW_GAP),
            'elk.edgeRouting': 'ORTHOGONAL',
            'elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
            'elk.layered.nodePlacement.strategy': 'BRANDES_KOEPF',
            'elk.spacing.portPort': String(PORT_SPACING),
            'elk.layered.spacing.edgeEdgeBetweenLayers': '15',
            'elk.layered.spacing.edgeNodeBetweenLayers': '15'
        },
        children: elkNodes,
        edges: elkEdges
    };

    const elk = new ELK();
    const result = await elk.layout(graph);

    // Extract positions
    for (const child of (result.children || [])) {
        const portMap = new Map();
        for (const port of (child.ports || [])) {
            portMap.set(port.id, { x: child.x + port.x, y: child.y + port.y });
        }

        if (child.id === '__entity_in__') {
            const outPorts = [];
            for (const p of inputPorts) {
                const pos = portMap.get('__entity_in___' + p.name + '_out');
                if (pos) { outPorts.push({ name: p.name, x: pos.x, y: pos.y }); }
            }
            layoutElements.push({
                x: child.x, y: child.y, w: child.width, h: child.height,
                name: data.entity_name, type: 'entity_in',
                inputPorts: [], outputPorts: outPorts, line: 0
            });
        } else if (child.id === '__entity_out__') {
            const inPorts = [];
            for (const p of outputPorts) {
                const pos = portMap.get('__entity_out___' + p.name + '_in');
                if (pos) { inPorts.push({ name: p.name, x: pos.x, y: pos.y }); }
            }
            layoutElements.push({
                x: child.x, y: child.y, w: child.width, h: child.height,
                name: data.entity_name, type: 'entity_out',
                inputPorts: inPorts, outputPorts: [], line: 0
            });
        } else {
            const inst = data.instances.find(i => i.name === child.id);
            if (!inst) { continue; }
            const inConns = inst.connections.filter(c => c.direction === 'in' || !c.direction);
            const outConns = inst.connections.filter(c => c.direction === 'out');

            const instInPorts = [];
            for (const c of inConns) {
                const pos = portMap.get(inst.name + '_' + c.port + '_in');
                if (pos) { instInPorts.push({ name: c.port, x: pos.x, y: pos.y }); }
            }
            const instOutPorts = [];
            for (const c of outConns) {
                const pos = portMap.get(inst.name + '_' + c.port + '_out');
                if (pos) { instOutPorts.push({ name: c.port, x: pos.x, y: pos.y }); }
            }

            layoutElements.push({
                x: child.x, y: child.y, w: child.width, h: child.height,
                name: inst.name, type: 'instance', entityType: inst.entity_type,
                inputPorts: instInPorts, outputPorts: instOutPorts, line: inst.line
            });
        }
    }

    // Extract wire routing
    for (const edge of (result.edges || [])) {
        const segments = [];
        let fromPt = null;
        let toPt = null;

        for (const section of (edge.sections || [])) {
            const pts = [];
            pts.push(section.startPoint);
            if (section.bendPoints) {
                for (const bp of section.bendPoints) { pts.push(bp); }
            }
            pts.push(section.endPoint);

            if (!fromPt) { fromPt = { x: pts[0].x, y: pts[0].y }; }
            toPt = { x: pts[pts.length - 1].x, y: pts[pts.length - 1].y };

            for (let i = 0; i < pts.length - 1; i++) {
                segments.push({ x1: pts[i].x, y1: pts[i].y, x2: pts[i + 1].x, y2: pts[i + 1].y });
            }
        }

        if (fromPt && toPt) {
            layoutWires.push({
                from: fromPt, to: toPt, segments,
                width: netWidthMap.get(edge.id) || 1,
                netName: netNameMap.get(edge.id) || ''
            });
        }
    }

    return { layoutElements, layoutWires, skippedEdges, elkEdgeCount: elkEdges.length };
}

// ────────────────────── TEST CASES ──────────────────────

// Test 1: Simple chain — A → B → output
const simpleChain = {
    entity_name: 'SimpleChain',
    ports: [
        { name: 'clk', direction: 'in', type: 'clock', width: 1 },
        { name: 'data_in', direction: 'in', type: 'bit<8>', width: 8 },
        { name: 'data_out', direction: 'out', type: 'bit<8>', width: 8 },
    ],
    instances: [
        { name: 'stage_a', entity_type: 'PipeStage', connections: [
            { port: 'clk', signal: 'clk', direction: 'in' },
            { port: 'din', signal: 'data_in', direction: 'in' },
            { port: 'dout', signal: 'mid', direction: 'out' },
        ], line: 10 },
        { name: 'stage_b', entity_type: 'PipeStage', connections: [
            { port: 'clk', signal: 'clk', direction: 'in' },
            { port: 'din', signal: 'mid', direction: 'in' },
            { port: 'dout', signal: 'data_out', direction: 'out' },
        ], line: 20 },
    ],
    nets: [
        { name: 'clk', width: 1,
          driver: { type: 'entity_port', name: 'clk', port: 'clk' },
          sinks: [
            { type: 'instance', name: 'stage_a', port: 'clk' },
            { type: 'instance', name: 'stage_b', port: 'clk' },
          ]},
        { name: 'data_in', width: 8,
          driver: { type: 'entity_port', name: 'data_in', port: 'data_in' },
          sinks: [{ type: 'instance', name: 'stage_a', port: 'din' }]},
        { name: 'mid', width: 8,
          driver: { type: 'instance', name: 'stage_a', port: 'dout' },
          sinks: [{ type: 'instance', name: 'stage_b', port: 'din' }]},
        { name: 'data_out', width: 8,
          driver: { type: 'instance', name: 'stage_b', port: 'dout' },
          sinks: [{ type: 'entity_port', name: 'data_out', port: 'data_out' }]},
    ],
    signals: [], assignments: [],
    filePath: 'test.sk', entityLine: 0, implLine: 5,
};

// Test 2: DC-DC-like topology — multiple instances, fan-out, Logic block
const dcdcLike = {
    entity_name: 'BatteryController',
    ports: [
        { name: 'clk', direction: 'in', type: 'clock', width: 1 },
        { name: 'reset', direction: 'in', type: 'reset', width: 1 },
        { name: 'voltage', direction: 'in', type: 'nat<12>', width: 12 },
        { name: 'current', direction: 'in', type: 'int<12>', width: 12 },
        { name: 'pwm_out', direction: 'out', type: 'bit', width: 1 },
        { name: 'fault', direction: 'out', type: 'bit', width: 1 },
        { name: 'phase', direction: 'out', type: 'bit<3>', width: 3 },
    ],
    instances: [
        { name: 'voltage_loop', entity_type: 'PiController', connections: [
            { port: 'clk', signal: 'clk', direction: 'in' },
            { port: 'reset', signal: 'reset', direction: 'in' },
            { port: 'feedback', signal: 'voltage', direction: 'in' },
            { port: 'output', signal: 'v_ctrl', direction: 'out' },
        ], line: 10 },
        { name: 'current_loop', entity_type: 'PiController', connections: [
            { port: 'clk', signal: 'clk', direction: 'in' },
            { port: 'reset', signal: 'reset', direction: 'in' },
            { port: 'feedback', signal: 'current', direction: 'in' },
            { port: 'output', signal: 'i_ctrl', direction: 'out' },
        ], line: 20 },
        { name: 'cc_cv', entity_type: 'CcCvController', connections: [
            { port: 'v_ctrl', signal: 'v_ctrl', direction: 'in' },
            { port: 'i_ctrl', signal: 'i_ctrl', direction: 'in' },
            { port: 'duty', signal: 'duty_cycle', direction: 'out' },
            { port: 'phase', signal: 'phase', direction: 'out' },
        ], line: 30 },
        { name: 'pwm_gen', entity_type: 'PwmGenerator', connections: [
            { port: 'clk', signal: 'clk', direction: 'in' },
            { port: 'duty', signal: 'duty_cycle', direction: 'in' },
            { port: 'pwm', signal: 'pwm_out', direction: 'out' },
        ], line: 40 },
        { name: 'protection', entity_type: 'Protection', connections: [
            { port: 'voltage', signal: 'voltage', direction: 'in' },
            { port: 'current', signal: 'current', direction: 'in' },
            { port: 'fault', signal: 'fault', direction: 'out' },
        ], line: 50 },
    ],
    nets: [
        { name: 'clk', width: 1,
          driver: { type: 'entity_port', name: 'clk', port: 'clk' },
          sinks: [
            { type: 'instance', name: 'voltage_loop', port: 'clk' },
            { type: 'instance', name: 'current_loop', port: 'clk' },
            { type: 'instance', name: 'pwm_gen', port: 'clk' },
          ]},
        { name: 'reset', width: 1,
          driver: { type: 'entity_port', name: 'reset', port: 'reset' },
          sinks: [
            { type: 'instance', name: 'voltage_loop', port: 'reset' },
            { type: 'instance', name: 'current_loop', port: 'reset' },
          ]},
        { name: 'voltage', width: 12,
          driver: { type: 'entity_port', name: 'voltage', port: 'voltage' },
          sinks: [
            { type: 'instance', name: 'voltage_loop', port: 'feedback' },
            { type: 'instance', name: 'protection', port: 'voltage' },
          ]},
        { name: 'current', width: 12,
          driver: { type: 'entity_port', name: 'current', port: 'current' },
          sinks: [
            { type: 'instance', name: 'current_loop', port: 'feedback' },
            { type: 'instance', name: 'protection', port: 'current' },
          ]},
        { name: 'v_ctrl', width: 12,
          driver: { type: 'instance', name: 'voltage_loop', port: 'output' },
          sinks: [{ type: 'instance', name: 'cc_cv', port: 'v_ctrl' }]},
        { name: 'i_ctrl', width: 12,
          driver: { type: 'instance', name: 'current_loop', port: 'output' },
          sinks: [{ type: 'instance', name: 'cc_cv', port: 'i_ctrl' }]},
        { name: 'duty_cycle', width: 12,
          driver: { type: 'instance', name: 'cc_cv', port: 'duty' },
          sinks: [{ type: 'instance', name: 'pwm_gen', port: 'duty' }]},
        { name: 'pwm_out', width: 1,
          driver: { type: 'instance', name: 'pwm_gen', port: 'pwm' },
          sinks: [{ type: 'entity_port', name: 'pwm_out', port: 'pwm_out' }]},
        { name: 'fault', width: 1,
          driver: { type: 'instance', name: 'protection', port: 'fault' },
          sinks: [{ type: 'entity_port', name: 'fault', port: 'fault' }]},
        { name: 'phase', width: 3,
          driver: { type: 'instance', name: 'cc_cv', port: 'phase' },
          sinks: [{ type: 'entity_port', name: 'phase', port: 'phase' }]},
    ],
    signals: [], assignments: [],
    filePath: 'test.sk', entityLine: 0, implLine: 5,
};

// Test 3: Edge case — entity_port driver that's NOT a real input port (signal-mediated)
const signalMediatedDriver = {
    entity_name: 'EdgeCase',
    ports: [
        { name: 'clk', direction: 'in', type: 'clock', width: 1 },
        { name: 'result', direction: 'out', type: 'bit<8>', width: 8 },
    ],
    instances: [
        { name: 'adder', entity_type: 'Adder', connections: [
            { port: 'a', signal: 'computed', direction: 'in' },
            { port: 'sum', signal: 'result', direction: 'out' },
        ], line: 10 },
    ],
    nets: [
        // "computed" is NOT an entity input port — should be skipped gracefully
        { name: 'computed', width: 8,
          driver: { type: 'entity_port', name: 'computed', port: 'computed' },
          sinks: [{ type: 'instance', name: 'adder', port: 'a' }]},
        { name: 'result', width: 8,
          driver: { type: 'instance', name: 'adder', port: 'sum' },
          sinks: [{ type: 'entity_port', name: 'result', port: 'result' }]},
    ],
    signals: [], assignments: [],
    filePath: 'test.sk', entityLine: 0, implLine: 5,
};

// Test 4: No instances, no nets (empty impl)
const emptyEntity = {
    entity_name: 'EmptyEntity',
    ports: [
        { name: 'clk', direction: 'in', type: 'clock', width: 1 },
        { name: 'out', direction: 'out', type: 'bit', width: 1 },
    ],
    instances: [],
    nets: [],
    signals: [], assignments: [],
    filePath: 'test.sk', entityLine: 0, implLine: 5,
};

// ────────────────────── RUNNER ──────────────────────

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

async function runTest(name, data, checks) {
    process.stdout.write(`\n[TEST] ${name}\n`);
    try {
        const result = await computeLayout(data);
        checks(result);
    } catch (err) {
        console.error('  FAIL: threw exception:', err.message || err);
        failed++;
    }
}

async function main() {
    // Test 1: Simple chain
    await runTest('Simple chain (A → B)', simpleChain, (r) => {
        assert(r.layoutElements.length === 4, `expected 4 elements, got ${r.layoutElements.length}`);

        const entityIn = r.layoutElements.find(e => e.type === 'entity_in');
        const entityOut = r.layoutElements.find(e => e.type === 'entity_out');
        const stageA = r.layoutElements.find(e => e.name === 'stage_a');
        const stageB = r.layoutElements.find(e => e.name === 'stage_b');

        assert(entityIn, 'entity_in exists');
        assert(entityOut, 'entity_out exists');
        assert(stageA, 'stage_a exists');
        assert(stageB, 'stage_b exists');

        // Left-to-right ordering: entity_in < stage_a < stage_b < entity_out
        assert(entityIn.x < stageA.x, `entity_in.x (${entityIn.x}) < stage_a.x (${stageA.x})`);
        assert(stageA.x < stageB.x, `stage_a.x (${stageA.x}) < stage_b.x (${stageB.x})`);
        assert(stageB.x < entityOut.x, `stage_b.x (${stageB.x}) < entity_out.x (${entityOut.x})`);

        // Ports populated
        assert(entityIn.outputPorts.length === 2, `entity_in has 2 output ports, got ${entityIn.outputPorts.length}`);
        assert(stageA.inputPorts.length === 2, `stage_a has 2 input ports, got ${stageA.inputPorts.length}`);
        assert(stageA.outputPorts.length === 1, `stage_a has 1 output port, got ${stageA.outputPorts.length}`);

        // Wires: 5 = clk→stage_a + clk→stage_b + data_in + mid + data_out
        assert(r.layoutWires.length === 5, `expected 5 wires, got ${r.layoutWires.length}`);
        assert(r.skippedEdges === 0, `expected 0 skipped edges, got ${r.skippedEdges}`);

        // Every wire has at least 1 segment
        for (const w of r.layoutWires) {
            assert(w.segments.length >= 1, `wire ${w.netName} has ${w.segments.length} segments`);
        }

        // No overlapping boxes
        for (let i = 0; i < r.layoutElements.length; i++) {
            for (let j = i + 1; j < r.layoutElements.length; j++) {
                const a = r.layoutElements[i];
                const b = r.layoutElements[j];
                const overlap = !(a.x + a.w <= b.x || b.x + b.w <= a.x || a.y + a.h <= b.y || b.y + b.h <= a.y);
                assert(!overlap, `${a.name}(${a.type}) and ${b.name}(${b.type}) don't overlap`);
            }
        }

        console.log(`  Elements: ${r.layoutElements.map(e => `${e.name}(${e.type}) @${Math.round(e.x)},${Math.round(e.y)}`).join(', ')}`);
        console.log(`  Wires: ${r.layoutWires.map(w => `${w.netName}[${w.width}] ${w.segments.length}seg`).join(', ')}`);
    });

    // Test 2: DC-DC-like multi-instance topology
    await runTest('DC-DC-like topology (5 instances, fan-out)', dcdcLike, (r) => {
        assert(r.layoutElements.length === 7, `expected 7 elements, got ${r.layoutElements.length}`);

        const entityIn = r.layoutElements.find(e => e.type === 'entity_in');
        const entityOut = r.layoutElements.find(e => e.type === 'entity_out');

        assert(entityIn, 'entity_in exists');
        assert(entityOut, 'entity_out exists');
        assert(entityIn.outputPorts.length === 4, `entity_in has 4 output ports, got ${entityIn.outputPorts.length}`);
        assert(entityOut.inputPorts.length === 3, `entity_out has 3 input ports, got ${entityOut.inputPorts.length}`);

        // All instances present
        for (const name of ['voltage_loop', 'current_loop', 'cc_cv', 'pwm_gen', 'protection']) {
            assert(r.layoutElements.find(e => e.name === name), `instance ${name} exists`);
        }

        // 15 wires: clk×3 + reset×2 + voltage×2 + current×2 + v_ctrl + i_ctrl + duty + pwm + fault + phase
        assert(r.layoutWires.length === 15, `expected 15 wires, got ${r.layoutWires.length}`);
        assert(r.skippedEdges === 0, `expected 0 skipped edges, got ${r.skippedEdges}`);

        // Layering: voltage_loop/current_loop should be left of cc_cv, cc_cv left of pwm_gen
        const vl = r.layoutElements.find(e => e.name === 'voltage_loop');
        const ccv = r.layoutElements.find(e => e.name === 'cc_cv');
        const pwm = r.layoutElements.find(e => e.name === 'pwm_gen');
        assert(vl.x < ccv.x, `voltage_loop.x (${Math.round(vl.x)}) < cc_cv.x (${Math.round(ccv.x)})`);
        assert(ccv.x < pwm.x, `cc_cv.x (${Math.round(ccv.x)}) < pwm_gen.x (${Math.round(pwm.x)})`);

        // entity_in leftmost, entity_out rightmost
        for (const el of r.layoutElements) {
            if (el.type !== 'entity_in' && el.type !== 'entity_out') {
                assert(entityIn.x <= el.x, `entity_in.x <= ${el.name}.x`);
                assert(el.x <= entityOut.x, `${el.name}.x <= entity_out.x`);
            }
        }

        // No overlapping boxes
        for (let i = 0; i < r.layoutElements.length; i++) {
            for (let j = i + 1; j < r.layoutElements.length; j++) {
                const a = r.layoutElements[i];
                const b = r.layoutElements[j];
                const overlap = !(a.x + a.w <= b.x || b.x + b.w <= a.x || a.y + a.h <= b.y || b.y + b.h <= a.y);
                assert(!overlap, `${a.name}(${a.type}) and ${b.name}(${b.type}) don't overlap`);
            }
        }

        // Bus widths preserved
        const busWires = r.layoutWires.filter(w => w.width > 1);
        assert(busWires.length > 0, 'has bus wires');
        const voltageWire = r.layoutWires.find(w => w.netName === 'voltage');
        assert(voltageWire && voltageWire.width === 12, `voltage wire width is 12, got ${voltageWire?.width}`);

        console.log(`  Elements: ${r.layoutElements.map(e => `${e.name}(${e.type}) @${Math.round(e.x)},${Math.round(e.y)}`).join(', ')}`);
        console.log(`  Wires: ${r.layoutWires.length} total, ${busWires.length} buses`);
    });

    // Test 3: Invalid port references get skipped
    await runTest('Signal-mediated driver (invalid port ref)', signalMediatedDriver, (r) => {
        assert(r.layoutElements.length === 3, `expected 3 elements, got ${r.layoutElements.length}`);
        assert(r.skippedEdges === 1, `expected 1 skipped edge, got ${r.skippedEdges}`);
        assert(r.layoutWires.length === 1, `expected 1 wire (result), got ${r.layoutWires.length}`);
        assert(r.layoutWires[0].netName === 'result', `surviving wire is 'result'`);

        console.log(`  Skipped ${r.skippedEdges} invalid edges, ${r.layoutWires.length} valid wires`);
    });

    // Test 4: Empty entity
    await runTest('Empty entity (no instances)', emptyEntity, (r) => {
        assert(r.layoutElements.length === 2, `expected 2 elements (in/out bars), got ${r.layoutElements.length}`);
        assert(r.layoutWires.length === 0, `expected 0 wires, got ${r.layoutWires.length}`);

        const entityIn = r.layoutElements.find(e => e.type === 'entity_in');
        const entityOut = r.layoutElements.find(e => e.type === 'entity_out');
        assert(entityIn.x < entityOut.x, 'entity_in left of entity_out');

        console.log(`  Elements: ${r.layoutElements.map(e => `${e.type} @${Math.round(e.x)},${Math.round(e.y)}`).join(', ')}`);
    });

    // Summary
    console.log(`\n${'─'.repeat(40)}`);
    console.log(`Results: ${passed} passed, ${failed} failed`);
    process.exit(failed > 0 ? 1 : 0);
}

main();

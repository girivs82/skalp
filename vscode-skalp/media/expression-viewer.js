(function () {
    // @ts-ignore
    const vscode = acquireVsCodeApi();

    const canvas = document.getElementById('expr-canvas');
    const ctx = canvas.getContext('2d');
    const targetNameEl = document.getElementById('target-name');
    const statsEl = document.getElementById('stats');

    let exprData = null;
    let panX = 0, panY = 0;
    let zoomLevel = 1.0;
    let hoveredNode = null;

    // ELK layout engine
    const elk = new ELK();

    // Layout results
    let layoutNodes = [];    // { id, type, label, x, y, w, h, inputLabels, inputPositions, outputPosition }
    let layoutInputs = [];   // { id, name, isConstant, x, y, w, h, outputPosition }
    let layoutOutput = null;  // { name, x, y, w, h, inputPosition }
    let layoutWires = [];    // { segments[], fromId, toId, label }

    // --- Constants ---
    const COL_GAP = 90;
    const ROW_GAP = 25;
    const NODE_MIN_W = 56;
    const NODE_MIN_H = 40;
    const NODE_PORT_GAP = 16;
    const INPUT_W = 80;
    const INPUT_H = 22;
    const OUTPUT_W = 80;
    const OUTPUT_H = 22;
    const FONT_SIZE = 11;
    const PORT_DOT_R = 3;
    const STUB_LEN = 12;

    const COLORS = {
        gateBg: '#1e3a5f',
        gateBorder: '#4fc3f7',
        muxBg: '#2d3a2e',
        muxBorder: '#66bb6a',
        inputBg: 'transparent',
        inputText: '#64b5f6',
        clockText: '#4caf50',
        resetText: '#ff9800',
        constantText: '#ce93d8',
        outputText: '#ff9800',
        dffBg: '#3a2350',
        dffBorder: '#b388ff',
        wire: '#5c8dbf',
        wireClock: '#4caf50',
        wireReset: '#ff9800',
        wireHighlight: '#ffeb3b',
        text: '#d4d4d4',
        textDim: '#777',
        emptyText: '#555',
        highlight: '#ffeb3b'
    };

    function isClockName(name) {
        const n = name.toLowerCase();
        return n === 'clk' || n === 'clock' || n.endsWith('_clk') || n.endsWith('.clk');
    }
    function isResetName(name) {
        const n = name.toLowerCase();
        return n === 'rst' || n === 'reset' || n.endsWith('_rst') || n.endsWith('.rst')
            || n.endsWith('.reset') || n.startsWith('reset');
    }

    // --- Gate Shape Definitions ---
    const GATE_SHAPES = {
        'and': 'ieee_and',
        'or': 'ieee_or',
        'xor': 'ieee_xor',
        'not': 'ieee_not',
        'bitnot': 'ieee_not',
        'mux': 'trapezoid',
        'add': 'box',
        'sub': 'box',
        'neg': 'box',
        'mul': 'box',
        'div': 'box',
        'shl': 'box',
        'shr': 'box',
        'cmp': 'box',
        'cast': 'box',
        'bitselect': 'box',
        'concat': 'box',
        'func': 'box',
        'replicate': 'box',
        'dff': 'dff'
    };

    // --- Canvas Resize ---
    function resizeCanvas() {
        const w = canvas.clientWidth;
        const h = canvas.clientHeight;
        if (w <= 0 || h <= 0) { return; }
        const dpr = window.devicePixelRatio || 1;
        canvas.width = w * dpr;
        canvas.height = h * dpr;
        render();
    }

    window.addEventListener('resize', resizeCanvas);

    // --- ELK-Based Layout ---

    function measureText(text, fontSize) {
        return text.length * (fontSize * 0.65);
    }

    async function computeLayout() {
        if (!exprData) { return; }

        layoutNodes = [];
        layoutInputs = [];
        layoutOutput = null;
        layoutWires = [];

        const { nodes, wires, inputs, targetName } = exprData;
        if (nodes.length === 0 && inputs.length === 0) { return; }

        // Track node metadata (survives ELK layout call but keep separate for clarity)
        const nodeTypes = new Map();  // id -> 'input' | 'gate' | 'output'
        const nodeData = new Map();   // id -> original data

        const elkChildren = [];
        const elkEdges = [];

        // Input nodes (leftmost layer)
        for (const inp of inputs) {
            const w = Math.max(INPUT_W, measureText(inp.name, FONT_SIZE) + 20);
            elkChildren.push({
                id: inp.id,
                width: w,
                height: INPUT_H,
                layoutOptions: {
                    'elk.portConstraints': 'FIXED_SIDE',
                    'elk.layered.layering.layerConstraint': 'FIRST'
                },
                ports: [
                    { id: `${inp.id}_out`, layoutOptions: { 'elk.port.side': 'EAST' } }
                ]
            });
            nodeTypes.set(inp.id, 'input');
            nodeData.set(inp.id, inp);
        }

        // Gate nodes (intermediate layers)
        for (const n of nodes) {
            const numInputs = Math.max(
                (n.inputLabels || []).length,
                wires.filter(w => w.toNode === n.id).length,
                1
            );
            const h = Math.max(NODE_MIN_H, numInputs * NODE_PORT_GAP + 16);
            const textW = measureText(n.label, FONT_SIZE + 1);
            const w = Math.max(NODE_MIN_W, textW + 24);

            const ports = [];
            for (let i = 0; i < numInputs; i++) {
                ports.push({
                    id: `${n.id}_in${i}`,
                    layoutOptions: {
                        'elk.port.side': 'WEST',
                        'elk.port.index': `${i}`
                    }
                });
            }
            ports.push({
                id: `${n.id}_out`,
                layoutOptions: { 'elk.port.side': 'EAST' }
            });

            elkChildren.push({
                id: n.id,
                width: w,
                height: h,
                layoutOptions: {
                    'elk.portConstraints': 'FIXED_ORDER'
                },
                ports: ports
            });
            nodeTypes.set(n.id, 'gate');
            nodeData.set(n.id, { ...n, _numInputs: numInputs });
        }

        // Output node (rightmost layer)
        const outputNodeId = '__output__';
        if (targetName) {
            const w = Math.max(OUTPUT_W, measureText(targetName, FONT_SIZE) + 20);
            elkChildren.push({
                id: outputNodeId,
                width: w,
                height: OUTPUT_H,
                layoutOptions: {
                    'elk.portConstraints': 'FIXED_SIDE',
                    'elk.layered.layering.layerConstraint': 'LAST'
                },
                ports: [
                    { id: `${outputNodeId}_in`, layoutOptions: { 'elk.port.side': 'WEST' } }
                ]
            });
            nodeTypes.set(outputNodeId, 'output');
            nodeData.set(outputNodeId, { name: targetName });
        }

        // Wire edges
        for (const wire of wires) {
            elkEdges.push({
                id: wire.id,
                sources: [`${wire.fromNode}_out`],
                targets: [`${wire.toNode}_in${wire.toPort}`]
            });
        }

        // Edges from root nodes to output label
        if (targetName) {
            const usedAsInput = new Set();
            for (const w of wires) { usedAsInput.add(w.fromNode); }
            const rootNodes = nodes.filter(n => !usedAsInput.has(n.id));
            if (rootNodes.length > 0) {
                for (const rn of rootNodes) {
                    elkEdges.push({
                        id: `e_out_${rn.id}`,
                        sources: [`${rn.id}_out`],
                        targets: [`${outputNodeId}_in`]
                    });
                }
            } else if (nodes.length === 0 && inputs.length > 0) {
                // Direct assignment (no gates): connect inputs to output
                for (const inp of inputs) {
                    elkEdges.push({
                        id: `e_direct_${inp.id}`,
                        sources: [`${inp.id}_out`],
                        targets: [`${outputNodeId}_in`]
                    });
                }
            }
        }

        const graph = {
            id: 'root',
            layoutOptions: {
                'elk.algorithm': 'layered',
                'elk.direction': 'RIGHT',
                'elk.layered.spacing.nodeNodeBetweenLayers': `${COL_GAP}`,
                'elk.spacing.nodeNode': `${ROW_GAP}`,
                'elk.edgeRouting': 'ORTHOGONAL',
                'elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
                'elk.layered.nodePlacement.strategy': 'BRANDES_KOEPF',
                'elk.spacing.portPort': '12',
                'elk.layered.spacing.edgeEdgeBetweenLayers': '12',
                'elk.layered.spacing.edgeNodeBetweenLayers': '12'
            },
            children: elkChildren,
            edges: elkEdges
        };

        try {
            const result = await elk.layout(graph);
            buildLayoutFromElk(result, nodeTypes, nodeData);
        } catch (err) {
            console.error('ELK layout failed:', err);
        }
    }

    function buildLayoutFromElk(elkResult, nodeTypes, nodeData) {
        // Build absolute port position map: portId -> { x, y }
        const portPosMap = new Map();
        for (const child of (elkResult.children || [])) {
            for (const port of (child.ports || [])) {
                portPosMap.set(port.id, {
                    x: child.x + port.x,
                    y: child.y + port.y
                });
            }
        }

        // Extract layout data from ELK result
        for (const child of (elkResult.children || [])) {
            const type = nodeTypes.get(child.id);
            const data = nodeData.get(child.id);
            if (!type || !data) { continue; }

            if (type === 'input') {
                const outPort = portPosMap.get(`${child.id}_out`);
                layoutInputs.push({
                    id: child.id,
                    name: data.name,
                    isConstant: data.isConstant,
                    x: child.x,
                    y: child.y,
                    w: child.width,
                    h: child.height,
                    outputPosition: outPort || { x: child.x + child.width, y: child.y + child.height / 2 }
                });
            } else if (type === 'gate') {
                const numInputs = data._numInputs;
                const inputPositions = [];
                for (let i = 0; i < numInputs; i++) {
                    const pos = portPosMap.get(`${child.id}_in${i}`);
                    inputPositions.push(pos || {
                        x: child.x,
                        y: child.y + 8 + (i + 0.5) * NODE_PORT_GAP
                    });
                }
                const outPort = portPosMap.get(`${child.id}_out`);
                layoutNodes.push({
                    id: data.id,
                    type: data.type,
                    label: data.label,
                    inputLabels: data.inputLabels,
                    x: child.x,
                    y: child.y,
                    w: child.width,
                    h: child.height,
                    inputPositions: inputPositions,
                    outputPosition: outPort || { x: child.x + child.width, y: child.y + child.height / 2 }
                });
            } else if (type === 'output') {
                const inPort = portPosMap.get(`${child.id}_in`);
                layoutOutput = {
                    name: data.name,
                    x: child.x,
                    y: child.y,
                    w: child.width,
                    h: child.height,
                    inputPosition: inPort || { x: child.x, y: child.y + child.height / 2 }
                };
            }
        }

        // Build wire segments from ELK edge routing
        for (const edge of (elkResult.edges || [])) {
            const segments = [];
            for (const section of (edge.sections || [])) {
                const points = [section.startPoint];
                if (section.bendPoints) {
                    points.push(...section.bendPoints);
                }
                points.push(section.endPoint);
                for (let i = 0; i < points.length - 1; i++) {
                    segments.push({
                        x1: points[i].x,
                        y1: points[i].y,
                        x2: points[i + 1].x,
                        y2: points[i + 1].y
                    });
                }
            }

            // Extract source/target node IDs from port IDs
            const srcPortId = edge.sources[0] || '';
            const tgtPortId = edge.targets[0] || '';
            const fromId = srcPortId.replace(/_out$/, '');
            const toId = tgtPortId.replace(/_in\d*$/, '').replace(/_in$/, '');

            layoutWires.push({ segments, fromId, toId, label: null });
        }
    }

    // --- Rendering ---
    function render() {
        if (!ctx) { return; }

        const dpr = window.devicePixelRatio || 1;
        const w = canvas.clientWidth;
        const h = canvas.clientHeight;
        if (w <= 0 || h <= 0) { return; }

        canvas.width = w * dpr;
        canvas.height = h * dpr;
        ctx.setTransform(dpr, 0, 0, dpr, 0, 0);

        // Clear
        const bg = getComputedStyle(document.body).getPropertyValue('--vscode-editor-background') || '#1e1e1e';
        ctx.fillStyle = bg;
        ctx.fillRect(0, 0, w, h);

        if (!exprData) {
            ctx.fillStyle = COLORS.emptyText;
            ctx.font = '14px sans-serif';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText('Place cursor on an expression to view its circuit', w / 2, h / 2);
            return;
        }

        if (exprData.nodes.length === 0 && exprData.inputs.length === 0) {
            ctx.fillStyle = COLORS.emptyText;
            ctx.font = '14px sans-serif';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText('No circuit elements found', w / 2, h / 2);
            return;
        }

        ctx.save();
        ctx.translate(panX, panY);
        ctx.scale(zoomLevel, zoomLevel);

        drawWires();
        drawInputs();
        drawNodes();
        drawOutput();

        ctx.restore();
    }

    function drawInputs() {
        for (const inp of layoutInputs) {
            const isHovered = hoveredNode === inp.id;
            const isClock = !inp.isConstant && isClockName(inp.name);
            const isReset = !inp.isConstant && isResetName(inp.name);
            const stubColor = isHovered ? COLORS.highlight :
                isClock ? COLORS.wireClock :
                isReset ? COLORS.wireReset : COLORS.wire;

            // Arrow line
            ctx.strokeStyle = stubColor;
            ctx.lineWidth = 1;
            ctx.beginPath();
            ctx.moveTo(inp.x + inp.w - 8, inp.outputPosition.y);
            ctx.lineTo(inp.outputPosition.x, inp.outputPosition.y);
            ctx.stroke();

            // Arrow head
            const ax = inp.outputPosition.x;
            const ay = inp.outputPosition.y;
            ctx.fillStyle = stubColor;
            ctx.beginPath();
            ctx.moveTo(ax, ay);
            ctx.lineTo(ax - 5, ay - 3);
            ctx.lineTo(ax - 5, ay + 3);
            ctx.closePath();
            ctx.fill();

            // Label
            ctx.fillStyle = inp.isConstant ? COLORS.constantText :
                isClockName(inp.name) ? COLORS.clockText :
                isResetName(inp.name) ? COLORS.resetText :
                COLORS.inputText;
            ctx.font = `${FONT_SIZE}px monospace`;
            ctx.textAlign = 'right';
            ctx.textBaseline = 'middle';
            ctx.fillText(inp.name, inp.x + inp.w - 10, inp.outputPosition.y);
        }
    }

    function drawOutput() {
        if (!layoutOutput) { return; }

        const out = layoutOutput;

        // Arrow line
        ctx.strokeStyle = COLORS.wire;
        ctx.lineWidth = 1;
        ctx.beginPath();
        ctx.moveTo(out.inputPosition.x, out.inputPosition.y);
        ctx.lineTo(out.x + 10, out.inputPosition.y);
        ctx.stroke();

        // Arrow head
        const ax = out.x + 10;
        const ay = out.inputPosition.y;
        ctx.fillStyle = COLORS.outputText;
        ctx.beginPath();
        ctx.moveTo(ax, ay);
        ctx.lineTo(ax - 5, ay - 3);
        ctx.lineTo(ax - 5, ay + 3);
        ctx.closePath();
        ctx.fill();

        // Label
        ctx.fillStyle = COLORS.outputText;
        ctx.font = `bold ${FONT_SIZE}px monospace`;
        ctx.textAlign = 'left';
        ctx.textBaseline = 'middle';
        ctx.fillText(out.name, out.x + 14, out.inputPosition.y);
    }

    function drawNodes() {
        for (const ln of layoutNodes) {
            const isHovered = hoveredNode === ln.id;
            const shape = GATE_SHAPES[ln.type] || 'box';
            const isMux = ln.type === 'mux';
            const isDff = ln.type === 'dff';
            const borderColor = isHovered ? COLORS.highlight : (isDff ? COLORS.dffBorder : isMux ? COLORS.muxBorder : COLORS.gateBorder);
            const bgColor = isDff ? COLORS.dffBg : isMux ? COLORS.muxBg : COLORS.gateBg;

            switch (shape) {
                case 'ieee_and':
                    drawAndGate(ln, bgColor, borderColor);
                    break;
                case 'ieee_or':
                    drawOrGate(ln, bgColor, borderColor);
                    break;
                case 'ieee_xor':
                    drawXorGate(ln, bgColor, borderColor);
                    break;
                case 'ieee_not':
                    drawNotGate(ln, bgColor, borderColor);
                    break;
                case 'trapezoid':
                    drawMux(ln, bgColor, borderColor);
                    break;
                case 'dff':
                    drawDff(ln, bgColor, borderColor);
                    break;
                case 'box':
                default:
                    drawBox(ln, bgColor, borderColor);
                    break;
            }

            // Draw input port dots (clock/reset ports colored)
            if (ln.inputPositions) {
                for (let pi = 0; pi < ln.inputPositions.length; pi++) {
                    const pos = ln.inputPositions[pi];
                    const lbl = (ln.inputLabels && ln.inputLabels[pi]) || '';
                    ctx.fillStyle = isClockName(lbl) ? COLORS.clockText :
                                    isResetName(lbl) ? COLORS.resetText : borderColor;
                    ctx.beginPath();
                    ctx.arc(pos.x, pos.y, PORT_DOT_R, 0, Math.PI * 2);
                    ctx.fill();
                }
            }

            // Draw output port dot
            if (ln.outputPosition) {
                ctx.fillStyle = borderColor;
                ctx.beginPath();
                ctx.arc(ln.outputPosition.x, ln.outputPosition.y, PORT_DOT_R, 0, Math.PI * 2);
                ctx.fill();
            }
        }
    }

    function drawBox(ln, bgColor, borderColor) {
        const r = 4;
        ctx.fillStyle = bgColor;
        ctx.strokeStyle = borderColor;
        ctx.lineWidth = 1.5;
        roundRect(ctx, ln.x, ln.y, ln.w, ln.h, r);
        ctx.fill();
        ctx.stroke();

        // Label
        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE + 1}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(ln.label, ln.x + ln.w / 2, ln.y + ln.h / 2);
    }

    function drawAndGate(ln, bgColor, borderColor) {
        const { x, y, w, h } = ln;
        ctx.fillStyle = bgColor;
        ctx.strokeStyle = borderColor;
        ctx.lineWidth = 1.5;

        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.lineTo(x + w / 2, y);
        ctx.arc(x + w / 2, y + h / 2, h / 2, -Math.PI / 2, Math.PI / 2);
        ctx.lineTo(x, y + h);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();

        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE + 1}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(ln.label, x + w * 0.4, y + h / 2);
    }

    function drawOrGate(ln, bgColor, borderColor) {
        const { x, y, w, h } = ln;
        ctx.fillStyle = bgColor;
        ctx.strokeStyle = borderColor;
        ctx.lineWidth = 1.5;

        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.quadraticCurveTo(x + w * 0.6, y, x + w, y + h / 2);
        ctx.quadraticCurveTo(x + w * 0.6, y + h, x, y + h);
        ctx.quadraticCurveTo(x + w * 0.25, y + h / 2, x, y);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();

        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE + 1}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(ln.label, x + w * 0.4, y + h / 2);
    }

    function drawXorGate(ln, bgColor, borderColor) {
        const { x, y, w, h } = ln;

        ctx.strokeStyle = borderColor;
        ctx.lineWidth = 1.5;
        ctx.beginPath();
        ctx.moveTo(x - 4, y);
        ctx.quadraticCurveTo(x - 4 + w * 0.25, y + h / 2, x - 4, y + h);
        ctx.stroke();

        drawOrGate(ln, bgColor, borderColor);
    }

    function drawNotGate(ln, bgColor, borderColor) {
        const { x, y, w, h } = ln;
        ctx.fillStyle = bgColor;
        ctx.strokeStyle = borderColor;
        ctx.lineWidth = 1.5;

        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.lineTo(x + w - 8, y + h / 2);
        ctx.lineTo(x, y + h);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();

        ctx.beginPath();
        ctx.arc(x + w - 4, y + h / 2, 4, 0, Math.PI * 2);
        ctx.stroke();

        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(ln.label, x + (w - 8) * 0.4, y + h / 2);
    }

    function drawMux(ln, bgColor, borderColor) {
        const { x, y, w, h } = ln;
        const inset = 8;
        ctx.fillStyle = bgColor;
        ctx.strokeStyle = borderColor;
        ctx.lineWidth = 1.5;

        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.lineTo(x + w, y + inset);
        ctx.lineTo(x + w, y + h - inset);
        ctx.lineTo(x, y + h);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();

        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE + 1}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText('MUX', x + w / 2, y + h / 2);

        if (ln.inputLabels && ln.inputPositions) {
            ctx.font = `${FONT_SIZE - 1}px monospace`;
            ctx.textAlign = 'left';
            ctx.fillStyle = COLORS.textDim;
            for (let i = 0; i < Math.min(ln.inputLabels.length, ln.inputPositions.length); i++) {
                const lbl = ln.inputLabels[i];
                const pos = ln.inputPositions[i];
                ctx.fillText(lbl, pos.x + 4, pos.y + 1);
            }
        }
    }

    function drawDff(ln, bgColor, borderColor) {
        const { x, y, w, h } = ln;
        const r = 4;

        // Box body
        ctx.fillStyle = bgColor;
        ctx.strokeStyle = borderColor;
        ctx.lineWidth = 1.5;
        roundRect(ctx, x, y, w, h, r);
        ctx.fill();
        ctx.stroke();

        // Clock triangle at the CLK port (bottom-left input) — green
        if (ln.inputPositions && ln.inputPositions.length >= 2) {
            const clkPos = ln.inputPositions[1]; // CLK is port 1
            const triSize = 6;
            ctx.strokeStyle = COLORS.clockText;
            ctx.lineWidth = 1.5;
            ctx.beginPath();
            ctx.moveTo(x, clkPos.y - triSize);
            ctx.lineTo(x + triSize * 1.2, clkPos.y);
            ctx.lineTo(x, clkPos.y + triSize);
            ctx.closePath();
            ctx.stroke();
        }

        // Label
        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE + 1}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText('DFF', x + w / 2, y + h / 2 - 2);

        // Port labels (CLK label in green)
        if (ln.inputLabels && ln.inputPositions) {
            ctx.font = `${FONT_SIZE - 1}px monospace`;
            ctx.textAlign = 'left';
            for (let i = 0; i < Math.min(ln.inputLabels.length, ln.inputPositions.length); i++) {
                const lbl = ln.inputLabels[i];
                const pos = ln.inputPositions[i];
                ctx.fillStyle = isClockName(lbl) ? COLORS.clockText :
                                isResetName(lbl) ? COLORS.resetText : COLORS.textDim;
                ctx.fillText(lbl, x + 4 + (i === 1 ? 6 : 0), pos.y + 1);
            }
        }
    }

    function drawWires() {
        for (const wire of layoutWires) {
            const isHovered = hoveredNode && (wire.fromId === hoveredNode || wire.toId === hoveredNode);
            // Color clock/reset wires by checking the source input name
            let wireColor = COLORS.wire;
            if (!isHovered) {
                const srcInput = layoutInputs.find(i => i.id === wire.fromId);
                if (srcInput && isClockName(srcInput.name)) { wireColor = COLORS.wireClock; }
                else if (srcInput && isResetName(srcInput.name)) { wireColor = COLORS.wireReset; }
            }
            ctx.strokeStyle = isHovered ? COLORS.wireHighlight : wireColor;
            ctx.lineWidth = 1;
            ctx.lineCap = 'round';
            ctx.lineJoin = 'round';

            for (const seg of wire.segments) {
                ctx.beginPath();
                ctx.moveTo(seg.x1, seg.y1);
                ctx.lineTo(seg.x2, seg.y2);
                ctx.stroke();
            }

            // Junction dots at endpoints
            if (wire.segments.length > 0) {
                const first = wire.segments[0];
                const last = wire.segments[wire.segments.length - 1];
                ctx.fillStyle = isHovered ? COLORS.wireHighlight : wireColor;
                ctx.beginPath();
                ctx.arc(first.x1, first.y1, 2, 0, Math.PI * 2);
                ctx.fill();
                ctx.beginPath();
                ctx.arc(last.x2, last.y2, 2, 0, Math.PI * 2);
                ctx.fill();
            }
        }
    }

    function roundRect(ctx, x, y, w, h, r) {
        ctx.beginPath();
        ctx.moveTo(x + r, y);
        ctx.lineTo(x + w - r, y);
        ctx.arcTo(x + w, y, x + w, y + r, r);
        ctx.lineTo(x + w, y + h - r);
        ctx.arcTo(x + w, y + h, x + w - r, y + h, r);
        ctx.lineTo(x + r, y + h);
        ctx.arcTo(x, y + h, x, y + h - r, r);
        ctx.lineTo(x, y + r);
        ctx.arcTo(x, y, x + r, y, r);
        ctx.closePath();
    }

    // --- Zoom/Pan ---

    function zoomToFit() {
        if (layoutNodes.length === 0 && layoutInputs.length === 0) { return; }

        const padding = 30;
        let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;

        for (const li of layoutInputs) {
            minX = Math.min(minX, li.x - 10);
            minY = Math.min(minY, li.y - 5);
            maxX = Math.max(maxX, li.x + li.w + STUB_LEN);
            maxY = Math.max(maxY, li.y + li.h + 5);
        }

        for (const ln of layoutNodes) {
            minX = Math.min(minX, ln.x - 10);
            minY = Math.min(minY, ln.y - 10);
            maxX = Math.max(maxX, ln.x + ln.w + 10);
            maxY = Math.max(maxY, ln.y + ln.h + 10);
        }

        if (layoutOutput) {
            maxX = Math.max(maxX, layoutOutput.x + layoutOutput.w + 10);
            minY = Math.min(minY, layoutOutput.y - 5);
            maxY = Math.max(maxY, layoutOutput.y + layoutOutput.h + 5);
        }

        for (const wire of layoutWires) {
            for (const seg of wire.segments) {
                minX = Math.min(minX, seg.x1, seg.x2);
                maxX = Math.max(maxX, seg.x1, seg.x2);
                minY = Math.min(minY, seg.y1, seg.y2);
                maxY = Math.max(maxY, seg.y1, seg.y2);
            }
        }

        const contentW = maxX - minX + padding * 2;
        const contentH = maxY - minY + padding * 2;
        const canvasW = canvas.clientWidth || 800;
        const canvasH = canvas.clientHeight || 600;

        zoomLevel = Math.min(canvasW / contentW, canvasH / contentH, 2.0);
        zoomLevel = Math.max(0.1, zoomLevel);
        panX = (canvasW - contentW * zoomLevel) / 2 - (minX - padding) * zoomLevel;
        panY = (canvasH - contentH * zoomLevel) / 2 - (minY - padding) * zoomLevel;

        render();
    }

    function screenToWorld(sx, sy) {
        const rect = canvas.getBoundingClientRect();
        return {
            x: (sx - rect.left - panX) / zoomLevel,
            y: (sy - rect.top - panY) / zoomLevel
        };
    }

    // --- Mouse Interaction ---

    canvas.addEventListener('mousemove', (e) => {
        const pos = screenToWorld(e.clientX, e.clientY);
        let newHovered = null;

        for (const ln of layoutNodes) {
            if (pos.x >= ln.x && pos.x <= ln.x + ln.w && pos.y >= ln.y && pos.y <= ln.y + ln.h) {
                newHovered = ln.id;
                break;
            }
        }

        if (!newHovered) {
            for (const li of layoutInputs) {
                if (pos.x >= li.x && pos.x <= li.x + li.w && pos.y >= li.y && pos.y <= li.y + li.h) {
                    newHovered = li.id;
                    break;
                }
            }
        }

        if (newHovered !== hoveredNode) {
            hoveredNode = newHovered;
            canvas.style.cursor = hoveredNode ? 'pointer' : 'default';
            render();
        }
    });

    // Wheel zoom/pan
    canvas.addEventListener('wheel', (e) => {
        e.preventDefault();

        if (e.ctrlKey || e.metaKey) {
            const rect = canvas.getBoundingClientRect();
            const mouseX = e.clientX - rect.left;
            const mouseY = e.clientY - rect.top;

            const sensitivity = 0.005;
            const factor = Math.exp(-e.deltaY * sensitivity);
            const newZoom = Math.max(0.05, Math.min(10, zoomLevel * factor));

            panX = mouseX - (mouseX - panX) * (newZoom / zoomLevel);
            panY = mouseY - (mouseY - panY) * (newZoom / zoomLevel);
            zoomLevel = newZoom;
        } else {
            panX -= e.deltaX;
            panY -= e.deltaY;
        }

        render();
    }, { passive: false });

    // Toolbar buttons
    document.getElementById('btn-zoom-in').addEventListener('click', () => {
        const rect = canvas.getBoundingClientRect();
        const cx = rect.width / 2;
        const cy = rect.height / 2;
        const newZoom = Math.min(10, zoomLevel * 1.3);
        panX = cx - (cx - panX) * (newZoom / zoomLevel);
        panY = cy - (cy - panY) * (newZoom / zoomLevel);
        zoomLevel = newZoom;
        render();
    });

    document.getElementById('btn-zoom-out').addEventListener('click', () => {
        const rect = canvas.getBoundingClientRect();
        const cx = rect.width / 2;
        const cy = rect.height / 2;
        const newZoom = Math.max(0.05, zoomLevel / 1.3);
        panX = cx - (cx - panX) * (newZoom / zoomLevel);
        panY = cy - (cy - panY) * (newZoom / zoomLevel);
        zoomLevel = newZoom;
        render();
    });

    document.getElementById('btn-zoom-fit').addEventListener('click', () => {
        zoomToFit();
    });

    // --- Message Handler ---
    window.addEventListener('message', (event) => {
        const msg = event.data;
        if (msg.type === 'updateExpression') {
            exprData = msg.data;
            targetNameEl.textContent = exprData.targetName ? exprData.targetName + ' =' : 'No expression';

            const nodeCount = exprData.nodes ? exprData.nodes.length : 0;
            const inputCount = exprData.inputs ? exprData.inputs.length : 0;
            const lineRange = exprData.sourceRange
                ? `L${exprData.sourceRange[0] + 1}-${exprData.sourceRange[1] + 1}`
                : '';
            statsEl.textContent = `${nodeCount} gates, ${inputCount} inputs ${lineRange}`;

            computeLayout().then(() => zoomToFit());
        } else if (msg.type === 'clearExpression') {
            exprData = null;
            targetNameEl.textContent = 'No expression';
            statsEl.textContent = '';
            layoutNodes = [];
            layoutInputs = [];
            layoutOutput = null;
            layoutWires = [];
            render();
        }
    });

    // Initial render
    resizeCanvas();
})();

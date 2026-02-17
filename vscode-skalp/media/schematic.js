// SKALP Schematic Viewer — Canvas-based entity visualization with wire routing
(function () {
    // @ts-ignore
    const vscode = acquireVsCodeApi();

    const canvas = /** @type {HTMLCanvasElement} */ (document.getElementById('schematic-canvas'));
    const ctx = canvas.getContext('2d');
    const entityNameEl = document.getElementById('entity-name');
    const statsEl = document.getElementById('stats');

    let schematicData = null;
    let panX = 0, panY = 0;
    let zoomLevel = 1.0;
    let hoveredItem = null;
    let hoveredNet = null;

    // Layout constants
    const PORT_SPACING = 24;
    const PORT_STUB_LEN = 14;
    const ENTITY_PADDING = 16;
    const INSTANCE_PADDING = 12;
    const COLUMN_GAP = 180;
    const ROW_GAP = 30;
    const HEADER_HEIGHT = 22;
    const FONT_SIZE = 11;
    const PORT_DOT_R = 3;
    const WIRE_CHANNEL_SPACING = 8;
    const ENTITY_BOX_MIN_WIDTH = 180;
    const INSTANCE_BOX_MIN_WIDTH = 140;
    const BORDER_RADIUS = 5;

    // Colors (VS Code dark theme palette)
    const COLORS = {
        entityBg: '#1e3a5f',
        entityBorder: '#4fc3f7',
        entityHeader: '#0d2137',
        instanceBg: '#2a2d2e',
        instanceBorder: '#555',
        instanceHeader: '#383b3d',
        portIn: '#4caf50',
        portOut: '#ff9800',
        portInout: '#9c27b0',
        wire: '#5c8dbf',
        wireBus: '#7baad4',
        wireHighlight: '#ffeb3b',
        text: '#d4d4d4',
        textDim: '#777',
        textMuted: '#555',
        busSlash: '#8cb4d8',
        busLabel: '#8cb4d8',
        highlight: '#ffeb3b',
        junctionDot: '#8cb4d8'
    };

    // Layout result
    /** @type {{ x: number, y: number, w: number, h: number, name: string, type: string, entityType?: string, inputPorts: {name:string, x:number, y:number}[], outputPorts: {name:string, x:number, y:number}[], line?: number }[]} */
    let layoutElements = [];
    /** @type {{ from: {x:number, y:number}, to: {x:number, y:number}, segments: {x1:number, y1:number, x2:number, y2:number}[], width: number, netName: string }[]} */
    let layoutWires = [];

    function resizeCanvas() {
        // The canvas is flex:1 in the body, so clientWidth/clientHeight reflect
        // the actual available space after the toolbar takes its share.
        const w = canvas.clientWidth;
        const h = canvas.clientHeight;
        if (w <= 0 || h <= 0) { return; }
        const dpr = window.devicePixelRatio || 1;
        canvas.width = w * dpr;
        canvas.height = h * dpr;
        render();
    }

    // ────────────────────── LAYOUT ──────────────────────

    function computeLayout() {
        if (!schematicData) { return; }
        layoutElements = [];
        layoutWires = [];

        const data = schematicData;
        const inputPorts = data.ports.filter(p => p.direction === 'in');
        const outputPorts = data.ports.filter(p => p.direction === 'out');

        // ── Step 1: Assign ranks via topological sort ──
        // Build dependency graph: instance A depends on B if A has an input signal that B outputs
        const instByName = new Map();
        for (const inst of data.instances) { instByName.set(inst.name, inst); }

        // Map: signal name → instance that produces it (output connections)
        const signalDriver = new Map();
        for (const inst of data.instances) {
            for (const conn of inst.connections) {
                if (conn.direction === 'out') {
                    signalDriver.set(conn.signal, inst.name);
                }
            }
        }

        // Build adjacency: inst.name → set of instance names it depends on
        const deps = new Map();
        for (const inst of data.instances) {
            deps.set(inst.name, new Set());
        }
        for (const inst of data.instances) {
            for (const conn of inst.connections) {
                if (conn.direction === 'in' || !conn.direction) {
                    const driver = signalDriver.get(conn.signal);
                    if (driver && driver !== inst.name) {
                        deps.get(inst.name).add(driver);
                    }
                }
            }
        }

        // Kahn's topological sort → rank assignment
        const rank = new Map();
        const inDeg = new Map();
        for (const inst of data.instances) {
            inDeg.set(inst.name, deps.get(inst.name).size);
        }
        let queue = [];
        for (const [name, deg] of inDeg) {
            if (deg === 0) { queue.push(name); }
        }
        let currentRank = 0;
        while (queue.length > 0) {
            const nextQueue = [];
            for (const name of queue) {
                rank.set(name, currentRank);
            }
            currentRank++;
            for (const name of queue) {
                for (const [other, otherDeps] of deps) {
                    if (otherDeps.has(name)) {
                        otherDeps.delete(name);
                        inDeg.set(other, inDeg.get(other) - 1);
                        if (inDeg.get(other) === 0) {
                            nextQueue.push(other);
                        }
                    }
                }
            }
            queue = nextQueue;
        }
        // Handle cycles: assign remaining to max rank
        for (const inst of data.instances) {
            if (!rank.has(inst.name)) {
                rank.set(inst.name, currentRank);
            }
        }
        const maxRank = Math.max(0, ...Array.from(rank.values()));

        // ── Step 2: Measure box sizes ──
        function measureTextWidth(text, fontSize) {
            return text.length * (fontSize * 0.62);
        }

        function computeBoxSize(name, entityType, inPorts, outPorts) {
            const portCount = Math.max(inPorts.length, outPorts.length, 1);
            const h = HEADER_HEIGHT + INSTANCE_PADDING * 2 + portCount * PORT_SPACING;
            // Width: max of entity type label, longest port name on each side
            let maxIn = 0, maxOut = 0;
            for (const p of inPorts) { maxIn = Math.max(maxIn, measureTextWidth(p, FONT_SIZE)); }
            for (const p of outPorts) { maxOut = Math.max(maxOut, measureTextWidth(p, FONT_SIZE)); }
            const nameW = measureTextWidth(entityType || name, FONT_SIZE + 1);
            const w = Math.max(INSTANCE_BOX_MIN_WIDTH, maxIn + maxOut + 40, nameW + 30);
            return { w, h };
        }

        // ── Step 3: Build entity input/output port bars ──

        // Entity input ports bar (left column)
        const entityInPortCount = Math.max(inputPorts.length, 1);
        const entityInH = HEADER_HEIGHT + ENTITY_PADDING * 2 + entityInPortCount * PORT_SPACING;
        let maxInPortNameW = 0;
        for (const p of inputPorts) { maxInPortNameW = Math.max(maxInPortNameW, measureTextWidth(p.name, FONT_SIZE)); }
        const entityInW = Math.max(ENTITY_BOX_MIN_WIDTH, maxInPortNameW + 50);
        const entityInX = 40;
        const entityInY = 40;

        const entityInPorts = [];
        for (let i = 0; i < inputPorts.length; i++) {
            entityInPorts.push({
                name: inputPorts[i].name,
                x: entityInX + entityInW,
                y: entityInY + HEADER_HEIGHT + ENTITY_PADDING + i * PORT_SPACING + PORT_SPACING / 2
            });
        }

        layoutElements.push({
            x: entityInX, y: entityInY, w: entityInW, h: entityInH,
            name: data.entity_name, type: 'entity_in',
            inputPorts: [], outputPorts: entityInPorts,
            line: data.entityLine
        });

        // ── Step 4: Place instance boxes by rank ──
        const rankBuckets = [];
        for (let r = 0; r <= maxRank; r++) { rankBuckets.push([]); }
        for (const inst of data.instances) {
            const r = rank.get(inst.name) || 0;
            rankBuckets[r].push(inst);
        }

        // Compute X offsets for each rank column
        const columnX = [];
        let currentX = entityInX + entityInW + COLUMN_GAP;
        for (let r = 0; r <= maxRank; r++) {
            columnX.push(currentX);
            // Find max width in this column
            let maxW = INSTANCE_BOX_MIN_WIDTH;
            for (const inst of rankBuckets[r]) {
                const inPorts = inst.connections.filter(c => c.direction === 'in' || !c.direction).map(c => c.port);
                const outPorts = inst.connections.filter(c => c.direction === 'out').map(c => c.port);
                const size = computeBoxSize(inst.name, inst.entity_type, inPorts, outPorts);
                maxW = Math.max(maxW, size.w);
            }
            currentX += maxW + COLUMN_GAP;
        }

        // Place instances within each column
        for (let r = 0; r <= maxRank; r++) {
            let cy = entityInY;
            for (const inst of rankBuckets[r]) {
                const inConns = inst.connections.filter(c => c.direction === 'in' || !c.direction);
                const outConns = inst.connections.filter(c => c.direction === 'out');
                const inPortNames = inConns.map(c => c.port);
                const outPortNames = outConns.map(c => c.port);
                const size = computeBoxSize(inst.name, inst.entity_type, inPortNames, outPortNames);

                const ix = columnX[r];
                const iy = cy;

                const instInPorts = [];
                for (let j = 0; j < inConns.length; j++) {
                    instInPorts.push({
                        name: inConns[j].port,
                        x: ix,
                        y: iy + HEADER_HEIGHT + INSTANCE_PADDING + j * PORT_SPACING + PORT_SPACING / 2
                    });
                }

                const instOutPorts = [];
                for (let j = 0; j < outConns.length; j++) {
                    instOutPorts.push({
                        name: outConns[j].port,
                        x: ix + size.w,
                        y: iy + HEADER_HEIGHT + INSTANCE_PADDING + j * PORT_SPACING + PORT_SPACING / 2
                    });
                }

                layoutElements.push({
                    x: ix, y: iy, w: size.w, h: size.h,
                    name: inst.name, type: 'instance',
                    entityType: inst.entity_type,
                    inputPorts: instInPorts, outputPorts: instOutPorts,
                    line: inst.line
                });

                cy += size.h + ROW_GAP;
            }
        }

        // Entity output ports bar (right column)
        const entityOutPortCount = Math.max(outputPorts.length, 1);
        const entityOutH = HEADER_HEIGHT + ENTITY_PADDING * 2 + entityOutPortCount * PORT_SPACING;
        let maxOutPortNameW = 0;
        for (const p of outputPorts) { maxOutPortNameW = Math.max(maxOutPortNameW, measureTextWidth(p.name, FONT_SIZE)); }
        const entityOutW = Math.max(ENTITY_BOX_MIN_WIDTH, maxOutPortNameW + 50);
        const entityOutX = currentX;
        const entityOutY = entityInY;

        const entityOutPorts = [];
        for (let i = 0; i < outputPorts.length; i++) {
            entityOutPorts.push({
                name: outputPorts[i].name,
                x: entityOutX,
                y: entityOutY + HEADER_HEIGHT + ENTITY_PADDING + i * PORT_SPACING + PORT_SPACING / 2
            });
        }

        layoutElements.push({
            x: entityOutX, y: entityOutY, w: entityOutW, h: entityOutH,
            name: data.entity_name, type: 'entity_out',
            inputPorts: entityOutPorts, outputPorts: [],
            line: data.entityLine
        });

        // ── Step 5: Route wires ──
        routeWires(data);
    }

    function routeWires(data) {
        layoutWires = [];
        if (!data.nets || data.nets.length === 0) { return; }

        // Build lookup: elementName → layoutElement
        const elemByName = new Map();
        for (const el of layoutElements) {
            if (el.type === 'entity_in') { elemByName.set('__entity_in__', el); }
            else if (el.type === 'entity_out') { elemByName.set('__entity_out__', el); }
            else { elemByName.set(el.name, el); }
        }

        // For channel allocation, track vertical segments per X-column gap
        const channelColumns = new Map(); // columnGapX → nextOffset

        for (const net of data.nets) {
            const driver = net.driver;
            const srcPort = findPortPosition(elemByName, driver, 'out');
            if (!srcPort) { continue; }

            for (const sink of net.sinks) {
                const dstPort = findPortPosition(elemByName, sink, 'in');
                if (!dstPort) { continue; }

                const segments = routeManhattan(srcPort, dstPort, channelColumns);

                layoutWires.push({
                    from: srcPort,
                    to: dstPort,
                    segments,
                    width: net.width,
                    netName: net.name
                });
            }
        }
    }

    function findPortPosition(elemByName, endpoint, side) {
        if (endpoint.type === 'entity_port') {
            // Input ports are on entity_in box (output side), output ports on entity_out box (input side)
            const elIn = elemByName.get('__entity_in__');
            const elOut = elemByName.get('__entity_out__');
            if (side === 'out' && elIn) {
                // Entity input port drives into the design → find on entity_in's output ports
                const p = elIn.outputPorts.find(pp => pp.name === endpoint.port);
                if (p) { return { x: p.x + PORT_STUB_LEN, y: p.y }; }
            }
            if (side === 'in' && elOut) {
                // Entity output port receives from the design → find on entity_out's input ports
                const p = elOut.inputPorts.find(pp => pp.name === endpoint.port);
                if (p) { return { x: p.x - PORT_STUB_LEN, y: p.y }; }
            }
            return null;
        }

        // Instance port
        const el = elemByName.get(endpoint.name);
        if (!el) { return null; }
        if (side === 'out') {
            const p = el.outputPorts.find(pp => pp.name === endpoint.port);
            if (p) { return { x: p.x + PORT_STUB_LEN, y: p.y }; }
        }
        if (side === 'in') {
            const p = el.inputPorts.find(pp => pp.name === endpoint.port);
            if (p) { return { x: p.x - PORT_STUB_LEN, y: p.y }; }
        }
        return null;
    }

    function routeManhattan(src, dst, channelColumns) {
        // 3-segment Manhattan routing: horizontal → vertical → horizontal
        const segments = [];

        if (Math.abs(src.y - dst.y) < 2 && src.x < dst.x) {
            // Straight horizontal line
            segments.push({ x1: src.x, y1: src.y, x2: dst.x, y2: dst.y });
            return segments;
        }

        // Determine midpoint X for the vertical segment
        const midX = (src.x + dst.x) / 2;

        // Quantize to nearest channel column for alignment
        const channelKey = Math.round(midX / 20) * 20;
        const offset = channelColumns.get(channelKey) || 0;
        channelColumns.set(channelKey, offset + 1);
        const actualMidX = channelKey + offset * WIRE_CHANNEL_SPACING;

        // Horizontal from src
        segments.push({ x1: src.x, y1: src.y, x2: actualMidX, y2: src.y });
        // Vertical
        segments.push({ x1: actualMidX, y1: src.y, x2: actualMidX, y2: dst.y });
        // Horizontal to dst
        segments.push({ x1: actualMidX, y1: dst.y, x2: dst.x, y2: dst.y });

        return segments;
    }

    // ────────────────────── RENDERING ──────────────────────

    function render() {
        if (!ctx || !schematicData) { return; }

        const dpr = window.devicePixelRatio || 1;
        const w = canvas.clientWidth;
        const h = canvas.clientHeight;
        if (w <= 0 || h <= 0) { return; }

        canvas.width = w * dpr;
        canvas.height = h * dpr;
        ctx.setTransform(dpr, 0, 0, dpr, 0, 0);

        // Clear
        ctx.fillStyle = getComputedStyle(document.body).getPropertyValue('--vscode-editor-background') || '#1e1e1e';
        ctx.fillRect(0, 0, w, h);

        ctx.save();
        ctx.translate(panX, panY);
        ctx.scale(zoomLevel, zoomLevel);

        // Draw wires (behind boxes)
        drawWires();

        // Draw boxes
        for (const el of layoutElements) {
            if (el.type === 'entity_in' || el.type === 'entity_out') {
                drawEntityBox(el);
            } else {
                drawInstanceBox(el);
            }
        }

        ctx.restore();
    }

    function drawEntityBox(el) {
        const isInput = el.type === 'entity_in';

        drawRoundedRect(ctx, el.x, el.y, el.w, el.h, BORDER_RADIUS, COLORS.entityBg, COLORS.entityBorder, 2);

        // Header bar
        ctx.save();
        ctx.beginPath();
        ctx.moveTo(el.x + BORDER_RADIUS, el.y);
        ctx.lineTo(el.x + el.w - BORDER_RADIUS, el.y);
        ctx.quadraticCurveTo(el.x + el.w, el.y, el.x + el.w, el.y + BORDER_RADIUS);
        ctx.lineTo(el.x + el.w, el.y + HEADER_HEIGHT);
        ctx.lineTo(el.x, el.y + HEADER_HEIGHT);
        ctx.lineTo(el.x, el.y + BORDER_RADIUS);
        ctx.quadraticCurveTo(el.x, el.y, el.x + BORDER_RADIUS, el.y);
        ctx.closePath();
        ctx.fillStyle = COLORS.entityHeader;
        ctx.fill();
        ctx.restore();

        // Header text
        ctx.fillStyle = COLORS.entityBorder;
        ctx.font = `bold ${FONT_SIZE}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        const label = isInput ? el.name + ' (in)' : el.name + ' (out)';
        ctx.fillText(label, el.x + el.w / 2, el.y + HEADER_HEIGHT / 2);

        // Draw ports
        const ports = isInput ? el.outputPorts : el.inputPorts;
        for (const port of ports) {
            const color = isInput ? COLORS.portIn : COLORS.portOut;

            // Port stub line
            ctx.strokeStyle = color;
            ctx.lineWidth = 2;
            ctx.beginPath();
            if (isInput) {
                ctx.moveTo(port.x, port.y);
                ctx.lineTo(port.x + PORT_STUB_LEN, port.y);
            } else {
                ctx.moveTo(port.x - PORT_STUB_LEN, port.y);
                ctx.lineTo(port.x, port.y);
            }
            ctx.stroke();

            // Port dot
            ctx.fillStyle = color;
            ctx.beginPath();
            ctx.arc(isInput ? port.x + PORT_STUB_LEN : port.x - PORT_STUB_LEN, port.y, PORT_DOT_R, 0, Math.PI * 2);
            ctx.fill();

            // Arrow
            const arrowX = isInput ? port.x + PORT_STUB_LEN : port.x - PORT_STUB_LEN;
            const arrowDir = isInput ? 1 : -1;
            ctx.beginPath();
            ctx.moveTo(arrowX + arrowDir * 8, port.y);
            ctx.lineTo(arrowX + arrowDir * 2, port.y - 4);
            ctx.lineTo(arrowX + arrowDir * 2, port.y + 4);
            ctx.closePath();
            ctx.fillStyle = color;
            ctx.fill();

            // Port label
            ctx.fillStyle = COLORS.text;
            ctx.font = `${FONT_SIZE}px monospace`;
            if (isInput) {
                ctx.textAlign = 'right';
                ctx.fillText(port.name, port.x - 4, port.y + 4);
            } else {
                ctx.textAlign = 'left';
                ctx.fillText(port.name, port.x + 4, port.y + 4);
            }
        }
    }

    function drawInstanceBox(el) {
        const isHovered = hoveredItem === el.name;
        const borderColor = isHovered ? COLORS.highlight : COLORS.instanceBorder;
        const lw = isHovered ? 2 : 1;

        drawRoundedRect(ctx, el.x, el.y, el.w, el.h, BORDER_RADIUS, COLORS.instanceBg, borderColor, lw);

        // Header bar
        ctx.save();
        ctx.beginPath();
        ctx.moveTo(el.x + BORDER_RADIUS, el.y);
        ctx.lineTo(el.x + el.w - BORDER_RADIUS, el.y);
        ctx.quadraticCurveTo(el.x + el.w, el.y, el.x + el.w, el.y + BORDER_RADIUS);
        ctx.lineTo(el.x + el.w, el.y + HEADER_HEIGHT);
        ctx.lineTo(el.x, el.y + HEADER_HEIGHT);
        ctx.lineTo(el.x, el.y + BORDER_RADIUS);
        ctx.quadraticCurveTo(el.x, el.y, el.x + BORDER_RADIUS, el.y);
        ctx.closePath();
        ctx.fillStyle = COLORS.instanceHeader;
        ctx.fill();
        ctx.restore();

        // Instance name (bold)
        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(el.name, el.x + el.w / 2, el.y + HEADER_HEIGHT / 2);

        // Entity type (dimmer, below name)
        if (el.entityType) {
            ctx.fillStyle = COLORS.textDim;
            ctx.font = `${FONT_SIZE - 1}px monospace`;
            ctx.fillText(el.entityType, el.x + el.w / 2, el.y + HEADER_HEIGHT + 10);
        }

        // Input ports (left side)
        for (const port of el.inputPorts) {
            // Stub line
            ctx.strokeStyle = COLORS.portIn;
            ctx.lineWidth = 1.5;
            ctx.beginPath();
            ctx.moveTo(port.x - PORT_STUB_LEN, port.y);
            ctx.lineTo(port.x, port.y);
            ctx.stroke();

            // Dot
            ctx.fillStyle = COLORS.portIn;
            ctx.beginPath();
            ctx.arc(port.x - PORT_STUB_LEN, port.y, PORT_DOT_R - 0.5, 0, Math.PI * 2);
            ctx.fill();

            // Label
            ctx.fillStyle = COLORS.text;
            ctx.font = `${FONT_SIZE - 1}px monospace`;
            ctx.textAlign = 'left';
            ctx.textBaseline = 'middle';
            ctx.fillText(port.name, port.x + 4, port.y);
        }

        // Output ports (right side)
        for (const port of el.outputPorts) {
            // Stub line
            ctx.strokeStyle = COLORS.portOut;
            ctx.lineWidth = 1.5;
            ctx.beginPath();
            ctx.moveTo(port.x, port.y);
            ctx.lineTo(port.x + PORT_STUB_LEN, port.y);
            ctx.stroke();

            // Dot
            ctx.fillStyle = COLORS.portOut;
            ctx.beginPath();
            ctx.arc(port.x + PORT_STUB_LEN, port.y, PORT_DOT_R - 0.5, 0, Math.PI * 2);
            ctx.fill();

            // Label
            ctx.fillStyle = COLORS.text;
            ctx.font = `${FONT_SIZE - 1}px monospace`;
            ctx.textAlign = 'right';
            ctx.textBaseline = 'middle';
            ctx.fillText(port.name, port.x - 4, port.y);
        }
    }

    function drawWires() {
        for (const wire of layoutWires) {
            const isBus = wire.width > 1;
            const isHighlighted = hoveredNet === wire.netName;
            const color = isHighlighted ? COLORS.wireHighlight : (isBus ? COLORS.wireBus : COLORS.wire);
            const lineW = isBus ? 2.5 : 1;

            ctx.strokeStyle = color;
            ctx.lineWidth = lineW;
            ctx.lineCap = 'round';
            ctx.lineJoin = 'round';

            // Draw each segment
            for (const seg of wire.segments) {
                ctx.beginPath();
                ctx.moveTo(seg.x1, seg.y1);
                ctx.lineTo(seg.x2, seg.y2);
                ctx.stroke();
            }

            // Bus annotation: slash mark and width label at midpoint of first horizontal segment
            if (isBus && wire.segments.length > 0) {
                const seg = wire.segments[0];
                const mx = (seg.x1 + seg.x2) / 2;
                const my = (seg.y1 + seg.y2) / 2;

                // Slash mark
                ctx.strokeStyle = COLORS.busSlash;
                ctx.lineWidth = 1.5;
                ctx.beginPath();
                ctx.moveTo(mx - 4, my + 5);
                ctx.lineTo(mx + 4, my - 5);
                ctx.stroke();

                // Width label
                ctx.fillStyle = COLORS.busLabel;
                ctx.font = `${FONT_SIZE - 2}px monospace`;
                ctx.textAlign = 'center';
                ctx.textBaseline = 'bottom';
                ctx.fillText(String(wire.width), mx + 8, my - 3);
            }

            // Junction dots at endpoints
            ctx.fillStyle = COLORS.junctionDot;
            ctx.beginPath();
            ctx.arc(wire.from.x, wire.from.y, isBus ? 3 : 2, 0, Math.PI * 2);
            ctx.fill();
            ctx.beginPath();
            ctx.arc(wire.to.x, wire.to.y, isBus ? 3 : 2, 0, Math.PI * 2);
            ctx.fill();

            // Net name label on first segment
            if (wire.segments.length > 0) {
                const seg = wire.segments[0];
                const lx = (seg.x1 + seg.x2) / 2;
                const ly = seg.y1 - 6;
                ctx.fillStyle = COLORS.textMuted;
                ctx.font = `${FONT_SIZE - 2}px monospace`;
                ctx.textAlign = 'center';
                ctx.textBaseline = 'bottom';
                // Only draw label if segment is long enough
                if (Math.abs(seg.x2 - seg.x1) > 60) {
                    ctx.fillText(wire.netName, lx, ly);
                }
            }
        }
    }

    function drawRoundedRect(ctx, x, y, w, h, r, fill, stroke, lineWidth) {
        ctx.beginPath();
        ctx.moveTo(x + r, y);
        ctx.lineTo(x + w - r, y);
        ctx.quadraticCurveTo(x + w, y, x + w, y + r);
        ctx.lineTo(x + w, y + h - r);
        ctx.quadraticCurveTo(x + w, y + h, x + w - r, y + h);
        ctx.lineTo(x + r, y + h);
        ctx.quadraticCurveTo(x, y + h, x, y + h - r);
        ctx.lineTo(x, y + r);
        ctx.quadraticCurveTo(x, y, x + r, y);
        ctx.closePath();
        ctx.fillStyle = fill;
        ctx.fill();
        ctx.strokeStyle = stroke;
        ctx.lineWidth = lineWidth;
        ctx.stroke();
    }

    // ────────────────────── ZOOM TO FIT ──────────────────────

    function zoomToFit() {
        if (layoutElements.length === 0) { return; }

        const padding = 40;
        let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;

        for (const el of layoutElements) {
            minX = Math.min(minX, el.x - PORT_STUB_LEN - 10);
            minY = Math.min(minY, el.y - 10);
            maxX = Math.max(maxX, el.x + el.w + PORT_STUB_LEN + 10);
            maxY = Math.max(maxY, el.y + el.h + 10);
        }

        // Also account for wires
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

    // ────────────────────── EVENT HANDLERS ──────────────────────
    // Matches waveform viewer: trackpad scroll = pan, pinch/ctrl+scroll = zoom,
    // click = navigate. No drag-to-pan.

    function screenToWorld(sx, sy) {
        const rect = canvas.getBoundingClientRect();
        return {
            x: (sx - rect.left - panX) / zoomLevel,
            y: (sy - rect.top - panY) / zoomLevel
        };
    }

    canvas.addEventListener('mousemove', (e) => {
        const { x: mx, y: my } = screenToWorld(e.clientX, e.clientY);
        let foundItem = null;
        let foundNet = null;

        // Hit test boxes
        for (const el of layoutElements) {
            if (mx >= el.x && mx <= el.x + el.w && my >= el.y && my <= el.y + el.h) {
                foundItem = el.name;
                break;
            }
        }

        // Hit test wires (proximity check)
        if (!foundItem) {
            const hitDist = 6 / zoomLevel;
            for (const wire of layoutWires) {
                for (const seg of wire.segments) {
                    if (pointToSegmentDist(mx, my, seg.x1, seg.y1, seg.x2, seg.y2) < hitDist) {
                        foundNet = wire.netName;
                        break;
                    }
                }
                if (foundNet) { break; }
            }
        }

        let needsRedraw = false;
        if (foundItem !== hoveredItem) {
            hoveredItem = foundItem;
            canvas.style.cursor = foundItem ? 'pointer' : 'default';
            needsRedraw = true;
        }
        if (foundNet !== hoveredNet) {
            hoveredNet = foundNet;
            if (!foundItem) { canvas.style.cursor = foundNet ? 'crosshair' : 'default'; }
            needsRedraw = true;
        }
        if (needsRedraw) { render(); }
    });

    canvas.addEventListener('click', (e) => {
        if (hoveredItem) {
            const el = layoutElements.find(el => el.name === hoveredItem);
            if (el && el.type === 'instance' && el.entityType) {
                vscode.postMessage({ type: 'navigateToEntity', entityType: el.entityType });
            } else if (el && el.line !== undefined && schematicData && schematicData.filePath) {
                vscode.postMessage({ type: 'navigateToLine', filePath: schematicData.filePath, line: el.line });
            }
        }
    });

    canvas.addEventListener('wheel', (e) => {
        e.preventDefault();

        if (e.ctrlKey || e.metaKey) {
            // Pinch-to-zoom (or ctrl+scroll) — zoom centered on cursor
            const rect = canvas.getBoundingClientRect();
            const mouseX = e.clientX - rect.left;
            const mouseY = e.clientY - rect.top;

            const sensitivity = 0.005;
            const factor = Math.exp(-e.deltaY * sensitivity);
            const newZoom = Math.max(0.05, Math.min(10, zoomLevel * factor));

            // Keep point under cursor fixed
            panX = mouseX - (mouseX - panX) * (newZoom / zoomLevel);
            panY = mouseY - (mouseY - panY) * (newZoom / zoomLevel);
            zoomLevel = newZoom;
        } else {
            // Two-finger scroll — pan
            panX -= e.deltaX;
            panY -= e.deltaY;
        }

        render();
    }, { passive: false });

    document.getElementById('btn-zoom-in').addEventListener('click', () => {
        zoomLevel = Math.min(10, zoomLevel * 1.3);
        render();
    });

    document.getElementById('btn-zoom-out').addEventListener('click', () => {
        zoomLevel = Math.max(0.05, zoomLevel / 1.3);
        render();
    });

    document.getElementById('btn-zoom-fit').addEventListener('click', () => {
        zoomToFit();
    });

    // ────────────────────── UTILITY ──────────────────────

    function pointToSegmentDist(px, py, x1, y1, x2, y2) {
        const dx = x2 - x1;
        const dy = y2 - y1;
        const lenSq = dx * dx + dy * dy;
        if (lenSq === 0) { return Math.hypot(px - x1, py - y1); }
        let t = ((px - x1) * dx + (py - y1) * dy) / lenSq;
        t = Math.max(0, Math.min(1, t));
        return Math.hypot(px - (x1 + t * dx), py - (y1 + t * dy));
    }

    // ────────────────────── MESSAGE HANDLER ──────────────────────

    window.addEventListener('message', (event) => {
        const msg = event.data;
        if (msg.type === 'updateSchematic') {
            schematicData = msg.data;
            entityNameEl.textContent = schematicData.entity_name || 'No entity';

            // Update stats
            const instCount = schematicData.instances ? schematicData.instances.length : 0;
            const netCount = schematicData.nets ? schematicData.nets.length : 0;
            const portCount = schematicData.ports ? schematicData.ports.length : 0;
            statsEl.textContent = `${portCount} ports, ${instCount} instances, ${netCount} nets`;

            computeLayout();
            zoomToFit();
        }
    });

    window.addEventListener('resize', resizeCanvas);
    resizeCanvas();
})();

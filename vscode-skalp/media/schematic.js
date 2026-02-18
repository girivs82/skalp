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
        logicBg: '#2d2a2e',
        logicBorder: '#8e6aa0',
        logicHeader: '#3d2e42',
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

    // ────────────────────── LAYOUT (ELK) ──────────────────────

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

    async function computeLayout() {
        if (!schematicData) { return; }
        layoutElements = [];
        layoutWires = [];

        const data = schematicData;
        const inputPorts = data.ports.filter(p => p.direction === 'in');
        const outputPorts = data.ports.filter(p => p.direction === 'out');

        // ── Build ELK graph ──
        const elkNodes = [];
        const elkEdges = [];
        const netWidthMap = new Map(); // edgeId → net width
        const netNameMap = new Map(); // edgeId → net name

        // Entity input port bar → ELK node with output ports (drives into design)
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
                width: 1,
                height: 1,
                layoutOptions: {
                    'elk.port.side': 'EAST',
                    'elk.port.index': String(i)
                }
            }))
        });

        // Instance boxes → ELK nodes
        for (const inst of data.instances) {
            const inConns = inst.connections.filter(c => c.direction === 'in' || !c.direction);
            const outConns = inst.connections.filter(c => c.direction === 'out');
            const size = computeBoxSize(inst.name, inst.entity_type, inConns.map(c => c.port), outConns.map(c => c.port));

            const ports = [];
            for (let j = 0; j < inConns.length; j++) {
                ports.push({
                    id: inst.name + '_' + inConns[j].port + '_in',
                    width: 1,
                    height: 1,
                    layoutOptions: {
                        'elk.port.side': 'WEST',
                        'elk.port.index': String(j)
                    }
                });
            }
            for (let j = 0; j < outConns.length; j++) {
                ports.push({
                    id: inst.name + '_' + outConns[j].port + '_out',
                    width: 1,
                    height: 1,
                    layoutOptions: {
                        'elk.port.side': 'EAST',
                        'elk.port.index': String(j)
                    }
                });
            }

            elkNodes.push({
                id: inst.name,
                width: size.w,
                height: size.h,
                ports,
                layoutOptions: {
                    'elk.portConstraints': 'FIXED_ORDER'
                }
            });
        }

        // Entity output port bar → ELK node with input ports (receives from design)
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
                width: 1,
                height: 1,
                layoutOptions: {
                    'elk.port.side': 'WEST',
                    'elk.port.index': String(i)
                }
            }))
        });

        // Build set of all valid port IDs for edge validation
        const validPortIds = new Set();
        for (const node of elkNodes) {
            for (const port of (node.ports || [])) {
                validPortIds.add(port.id);
            }
        }

        // Nets → ELK edges (skip edges referencing non-existent ports)
        let edgeIdx = 0;
        for (const net of (data.nets || [])) {
            const driver = net.driver;
            let sourcePortId;
            if (driver.type === 'entity_port') {
                sourcePortId = '__entity_in___' + driver.port + '_out';
            } else {
                sourcePortId = driver.name + '_' + driver.port + '_out';
            }

            if (!validPortIds.has(sourcePortId)) { continue; }

            for (const sink of net.sinks) {
                let targetPortId;
                if (sink.type === 'entity_port') {
                    targetPortId = '__entity_out___' + sink.port + '_in';
                } else {
                    targetPortId = sink.name + '_' + sink.port + '_in';
                }

                if (!validPortIds.has(targetPortId)) { continue; }

                const edgeId = 'e' + edgeIdx++;
                netWidthMap.set(edgeId, net.width);
                netNameMap.set(edgeId, net.name);
                elkEdges.push({
                    id: edgeId,
                    sources: [sourcePortId],
                    targets: [targetPortId]
                });
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

        // Run ELK layout
        // @ts-ignore — ELK loaded via <script> tag
        const elk = new ELK();
        let result;
        try {
            result = await elk.layout(graph);
        } catch (err) {
            console.error('ELK layout failed:', err);
            return;
        }

        // ── Extract results ──

        // Build port position maps per child node
        for (const child of (result.children || [])) {
            const portMap = new Map(); // portId → {x, y} in absolute coords
            for (const port of (child.ports || [])) {
                portMap.set(port.id, {
                    x: child.x + port.x,
                    y: child.y + port.y
                });
            }

            if (child.id === '__entity_in__') {
                const outPorts = [];
                for (const p of inputPorts) {
                    const pos = portMap.get('__entity_in___' + p.name + '_out');
                    if (pos) {
                        outPorts.push({ name: p.name, x: pos.x, y: pos.y });
                    }
                }
                layoutElements.push({
                    x: child.x, y: child.y, w: child.width, h: child.height,
                    name: data.entity_name, type: 'entity_in',
                    inputPorts: [], outputPorts: outPorts,
                    line: data.entityLine
                });
            } else if (child.id === '__entity_out__') {
                const inPorts = [];
                for (const p of outputPorts) {
                    const pos = portMap.get('__entity_out___' + p.name + '_in');
                    if (pos) {
                        inPorts.push({ name: p.name, x: pos.x, y: pos.y });
                    }
                }
                layoutElements.push({
                    x: child.x, y: child.y, w: child.width, h: child.height,
                    name: data.entity_name, type: 'entity_out',
                    inputPorts: inPorts, outputPorts: [],
                    line: data.entityLine
                });
            } else {
                // Instance node
                const inst = data.instances.find(i => i.name === child.id);
                if (!inst) { continue; }
                const inConns = inst.connections.filter(c => c.direction === 'in' || !c.direction);
                const outConns = inst.connections.filter(c => c.direction === 'out');

                const instInPorts = [];
                for (const c of inConns) {
                    const pos = portMap.get(inst.name + '_' + c.port + '_in');
                    if (pos) {
                        instInPorts.push({ name: c.port, x: pos.x, y: pos.y });
                    }
                }
                const instOutPorts = [];
                for (const c of outConns) {
                    const pos = portMap.get(inst.name + '_' + c.port + '_out');
                    if (pos) {
                        instOutPorts.push({ name: c.port, x: pos.x, y: pos.y });
                    }
                }

                layoutElements.push({
                    x: child.x, y: child.y, w: child.width, h: child.height,
                    name: inst.name, type: 'instance',
                    entityType: inst.entity_type,
                    inputPorts: instInPorts, outputPorts: instOutPorts,
                    line: inst.line
                });
            }
        }

        // Extract wire routing from ELK edges
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
                    segments.push({
                        x1: pts[i].x, y1: pts[i].y,
                        x2: pts[i + 1].x, y2: pts[i + 1].y
                    });
                }
            }

            if (fromPt && toPt) {
                layoutWires.push({
                    from: fromPt,
                    to: toPt,
                    segments,
                    width: netWidthMap.get(edge.id) || 1,
                    netName: netNameMap.get(edge.id) || ''
                });
            }
        }
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
        const isLogic = el.name === 'logic';
        const isHovered = hoveredItem === el.name;
        const borderColor = isHovered ? COLORS.highlight : (isLogic ? COLORS.logicBorder : COLORS.instanceBorder);
        const bgColor = isLogic ? COLORS.logicBg : COLORS.instanceBg;
        const headerColor = isLogic ? COLORS.logicHeader : COLORS.instanceHeader;
        const lw = isHovered ? 2 : 1;

        drawRoundedRect(ctx, el.x, el.y, el.w, el.h, BORDER_RADIUS, bgColor, borderColor, lw);

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
        ctx.fillStyle = headerColor;
        ctx.fill();
        ctx.restore();

        // Instance name (bold)
        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE}px monospace`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        const displayName = isLogic ? 'State Machine' : el.name;
        ctx.fillText(displayName, el.x + el.w / 2, el.y + HEADER_HEIGHT / 2);

        // Entity type (dimmer, below name)
        if (el.entityType && !isLogic) {
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
            if (el && el.type === 'instance' && el.entityType && el.name !== 'logic') {
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

            computeLayout().then(() => zoomToFit());
        }
    });

    window.addEventListener('resize', resizeCanvas);
    resizeCanvas();
})();

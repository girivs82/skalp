// SKALP Schematic Viewer — Canvas-based entity visualization
(function () {
    // @ts-ignore
    const vscode = acquireVsCodeApi();

    const canvas = /** @type {HTMLCanvasElement} */ (document.getElementById('schematic-canvas'));
    const ctx = canvas.getContext('2d');
    const entityNameEl = document.getElementById('entity-name');

    let schematicData = null;
    let panX = 0, panY = 0;
    let zoomLevel = 1.0;
    let dragging = false;
    let dragStartX = 0, dragStartY = 0;
    let hoveredItem = null;

    // Layout constants
    const PORT_SPACING = 30;
    const PORT_WIDTH = 8;
    const ENTITY_PADDING = 20;
    const INSTANCE_MIN_WIDTH = 120;
    const INSTANCE_MIN_HEIGHT = 60;
    const INSTANCE_SPACING = 40;
    const ENTITY_BORDER_RADIUS = 6;
    const HEADER_HEIGHT = 24;
    const FONT_SIZE = 11;

    // Colors
    const COLORS = {
        entityBg: '#1e3a5f',
        entityBorder: '#4fc3f7',
        entityHeader: '#0d2137',
        instanceBg: '#2d2d2d',
        instanceBorder: '#666',
        instanceHeader: '#404040',
        portIn: '#4caf50',
        portOut: '#ff9800',
        portInout: '#9c27b0',
        wire: '#64b5f6',
        signal: '#90a4ae',
        text: '#e0e0e0',
        textDim: '#888',
        highlight: '#ffeb3b'
    };

    /** @type {{ x: number, y: number, w: number, h: number, name: string, type: string }[]} */
    let layoutBoxes = [];

    function resizeCanvas() {
        canvas.width = canvas.parentElement.clientWidth;
        canvas.height = canvas.parentElement.clientHeight - 32;
        render();
    }

    function computeLayout() {
        if (!schematicData) { return; }
        layoutBoxes = [];

        const data = schematicData;
        const inputPorts = data.ports.filter(p => p.direction === 'in');
        const outputPorts = data.ports.filter(p => p.direction === 'out');

        // Main entity box
        const entityPortCount = Math.max(inputPorts.length, outputPorts.length, 1);
        const entityHeight = ENTITY_PADDING * 2 + HEADER_HEIGHT + entityPortCount * PORT_SPACING;
        const entityWidth = 200;
        const entityX = 20;
        const entityY = 20;

        layoutBoxes.push({
            x: entityX,
            y: entityY,
            w: entityWidth,
            h: entityHeight,
            name: data.entity_name,
            type: 'entity'
        });

        // Instance boxes
        const instancesPerRow = 3;
        const instanceStartX = entityX + entityWidth + INSTANCE_SPACING + 80;
        const instanceStartY = entityY;

        for (let i = 0; i < data.instances.length; i++) {
            const inst = data.instances[i];
            const row = Math.floor(i / instancesPerRow);
            const col = i % instancesPerRow;
            const ix = instanceStartX + col * (INSTANCE_MIN_WIDTH + INSTANCE_SPACING);
            const iy = instanceStartY + row * (INSTANCE_MIN_HEIGHT + INSTANCE_SPACING);

            layoutBoxes.push({
                x: ix,
                y: iy,
                w: INSTANCE_MIN_WIDTH,
                h: INSTANCE_MIN_HEIGHT,
                name: inst.name,
                type: 'instance',
                entityType: inst.entity_type
            });
        }
    }

    function render() {
        if (!ctx || !schematicData) { return; }

        const w = canvas.width;
        const h = canvas.height;
        const dpr = window.devicePixelRatio || 1;

        canvas.width = w * dpr;
        canvas.height = h * dpr;
        canvas.style.width = w + 'px';
        canvas.style.height = h + 'px';
        ctx.scale(dpr, dpr);

        // Clear
        ctx.fillStyle = getComputedStyle(document.body).getPropertyValue('--vscode-editor-background') || '#1e1e1e';
        ctx.fillRect(0, 0, w, h);

        ctx.save();
        ctx.translate(panX, panY);
        ctx.scale(zoomLevel, zoomLevel);

        const data = schematicData;
        const inputPorts = data.ports.filter(p => p.direction === 'in');
        const outputPorts = data.ports.filter(p => p.direction === 'out');

        if (layoutBoxes.length === 0) { ctx.restore(); return; }

        const mainBox = layoutBoxes[0];

        // Draw main entity box
        drawRoundedRect(ctx, mainBox.x, mainBox.y, mainBox.w, mainBox.h, ENTITY_BORDER_RADIUS, COLORS.entityBg, COLORS.entityBorder, 2);

        // Header
        ctx.fillStyle = COLORS.entityHeader;
        ctx.fillRect(mainBox.x + 1, mainBox.y + 1, mainBox.w - 2, HEADER_HEIGHT);
        ctx.fillStyle = COLORS.text;
        ctx.font = `bold ${FONT_SIZE}px monospace`;
        ctx.textAlign = 'center';
        ctx.fillText(data.entity_name, mainBox.x + mainBox.w / 2, mainBox.y + HEADER_HEIGHT - 6);

        // Input ports (left side)
        for (let i = 0; i < inputPorts.length; i++) {
            const port = inputPorts[i];
            const py = mainBox.y + HEADER_HEIGHT + ENTITY_PADDING + i * PORT_SPACING;
            const px = mainBox.x;

            // Port stub
            ctx.fillStyle = COLORS.portIn;
            ctx.fillRect(px - PORT_WIDTH, py - 3, PORT_WIDTH, 6);

            // Arrow
            ctx.beginPath();
            ctx.moveTo(px - PORT_WIDTH - 6, py);
            ctx.lineTo(px - PORT_WIDTH, py - 4);
            ctx.lineTo(px - PORT_WIDTH, py + 4);
            ctx.closePath();
            ctx.fillStyle = COLORS.portIn;
            ctx.fill();

            // Label
            ctx.fillStyle = COLORS.text;
            ctx.font = `${FONT_SIZE}px monospace`;
            ctx.textAlign = 'left';
            ctx.fillText(port.name, px + 6, py + 4);

            // Width annotation
            if (port.width > 1) {
                ctx.fillStyle = COLORS.textDim;
                ctx.font = `9px monospace`;
                ctx.fillText(`[${port.width - 1}:0]`, px + 6 + ctx.measureText(port.name).width + 4, py + 4);
            }
        }

        // Output ports (right side)
        for (let i = 0; i < outputPorts.length; i++) {
            const port = outputPorts[i];
            const py = mainBox.y + HEADER_HEIGHT + ENTITY_PADDING + i * PORT_SPACING;
            const px = mainBox.x + mainBox.w;

            // Port stub
            ctx.fillStyle = COLORS.portOut;
            ctx.fillRect(px, py - 3, PORT_WIDTH, 6);

            // Arrow
            ctx.beginPath();
            ctx.moveTo(px + PORT_WIDTH, py - 4);
            ctx.lineTo(px + PORT_WIDTH + 6, py);
            ctx.lineTo(px + PORT_WIDTH, py + 4);
            ctx.closePath();
            ctx.fillStyle = COLORS.portOut;
            ctx.fill();

            // Label
            ctx.fillStyle = COLORS.text;
            ctx.font = `${FONT_SIZE}px monospace`;
            ctx.textAlign = 'right';
            ctx.fillText(port.name, px - 6, py + 4);

            if (port.width > 1) {
                ctx.fillStyle = COLORS.textDim;
                ctx.font = `9px monospace`;
                ctx.textAlign = 'right';
                ctx.fillText(`[${port.width - 1}:0]`, px - 6 - ctx.measureText(port.name).width - 4, py + 4);
            }
        }

        // Draw instance boxes
        for (let i = 1; i < layoutBoxes.length; i++) {
            const box = layoutBoxes[i];
            const isHovered = hoveredItem === box.name;
            const borderColor = isHovered ? COLORS.highlight : COLORS.instanceBorder;

            drawRoundedRect(ctx, box.x, box.y, box.w, box.h, 4, COLORS.instanceBg, borderColor, isHovered ? 2 : 1);

            // Header
            ctx.fillStyle = COLORS.instanceHeader;
            ctx.fillRect(box.x + 1, box.y + 1, box.w - 2, HEADER_HEIGHT - 2);

            // Instance name
            ctx.fillStyle = COLORS.text;
            ctx.font = `bold ${FONT_SIZE}px monospace`;
            ctx.textAlign = 'center';
            ctx.fillText(box.name, box.x + box.w / 2, box.y + HEADER_HEIGHT - 6);

            // Entity type
            ctx.fillStyle = COLORS.textDim;
            ctx.font = `${FONT_SIZE - 1}px monospace`;
            ctx.fillText(box.entityType || '', box.x + box.w / 2, box.y + HEADER_HEIGHT + 14);
        }

        // Draw signals as labels
        if (data.signals && data.signals.length > 0) {
            const sigStartY = mainBox.y + mainBox.h + 20;
            ctx.fillStyle = COLORS.textDim;
            ctx.font = `${FONT_SIZE - 1}px monospace`;
            ctx.textAlign = 'left';
            ctx.fillText('Signals:', mainBox.x, sigStartY);

            for (let i = 0; i < data.signals.length; i++) {
                const sig = data.signals[i];
                ctx.fillStyle = COLORS.signal;
                ctx.fillText(
                    `${sig.name}: ${sig.type}${sig.width > 1 ? '[' + (sig.width - 1) + ':0]' : ''}`,
                    mainBox.x + 10,
                    sigStartY + 16 + i * 14
                );
            }
        }

        ctx.restore();
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

    // --- Event Handlers ---

    canvas.addEventListener('mousedown', (e) => {
        dragging = true;
        dragStartX = e.clientX - panX;
        dragStartY = e.clientY - panY;
    });

    canvas.addEventListener('mousemove', (e) => {
        if (dragging) {
            panX = e.clientX - dragStartX;
            panY = e.clientY - dragStartY;
            render();
        } else {
            // Hit test for hover
            const rect = canvas.getBoundingClientRect();
            const mx = (e.clientX - rect.left - panX) / zoomLevel;
            const my = (e.clientY - rect.top - panY) / zoomLevel;
            let found = null;
            for (const box of layoutBoxes) {
                if (mx >= box.x && mx <= box.x + box.w && my >= box.y && my <= box.y + box.h) {
                    found = box.name;
                    break;
                }
            }
            if (found !== hoveredItem) {
                hoveredItem = found;
                canvas.style.cursor = found ? 'pointer' : 'default';
                render();
            }
        }
    });

    canvas.addEventListener('mouseup', () => {
        dragging = false;
    });

    canvas.addEventListener('wheel', (e) => {
        e.preventDefault();
        const factor = e.deltaY < 0 ? 1.15 : 1 / 1.15;
        zoomLevel = Math.max(0.1, Math.min(10, zoomLevel * factor));
        render();
    }, { passive: false });

    document.getElementById('btn-zoom-in').addEventListener('click', () => {
        zoomLevel *= 1.3;
        render();
    });

    document.getElementById('btn-zoom-out').addEventListener('click', () => {
        zoomLevel = Math.max(0.1, zoomLevel / 1.3);
        render();
    });

    document.getElementById('btn-zoom-fit').addEventListener('click', () => {
        zoomLevel = 1.0;
        panX = 0;
        panY = 0;
        render();
    });

    // --- Message Handler ---

    window.addEventListener('message', (event) => {
        const msg = event.data;
        if (msg.type === 'updateSchematic') {
            schematicData = msg.data;
            entityNameEl.textContent = schematicData.entity_name || 'No entity';
            computeLayout();
            render();
        }
    });

    window.addEventListener('resize', resizeCanvas);
    resizeCanvas();
})();

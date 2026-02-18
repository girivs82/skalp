// SKALP Waveform Viewer — Canvas-based rendering
(function () {
    // @ts-ignore
    const vscode = acquireVsCodeApi();

    /** @type {SkwData | null} */
    let waveformData = null;
    /** @type {string[]} */
    let visibleSignals = [];
    /** @type {string | null} */
    let selectedSignal = null;
    let scrollX = 0;
    let scrollY = 0;
    let zoom = 1.0;
    let cursorTime = -1;
    let radix = 'hex';
    let filterText = '';
    let collapsedGroups = new Set();
    let displayRows = []; // [{type:'group', groupName:'...'}, {type:'signal', name:'...'}]
    let isSyncingScroll = false;
    let hoverX = -1; // mouse x position on canvas (-1 = not hovering)
    let breakpointMarkers = []; // [{cycle, label?, color?}]
    let highlightedSignals = new Set(); // signal names highlighted by debug session

    const cfg = window.skalpConfig || {};
    const FONT_SIZE = cfg.fontSize || 12;
    const ROW_HEIGHT = cfg.rowHeight || Math.round(FONT_SIZE * 2.2);
    const SIGNAL_LIST_WIDTH = cfg.signalListWidth || Math.round(FONT_SIZE * 22);
    const HEADER_HEIGHT = cfg.headerHeight || Math.round(FONT_SIZE * 1.7);
    const VALUE_HEIGHT = HEADER_HEIGHT;
    const CANVAS_FONT = FONT_SIZE + 'px monospace';
    const CANVAS_FONT_SMALL = Math.max(8, FONT_SIZE - 1) + 'px sans-serif';

    const canvas = /** @type {HTMLCanvasElement} */ (document.getElementById('waveform-canvas'));
    const ctx = canvas.getContext('2d');
    const signalListEl = document.getElementById('signal-list');
    const cursorReadout = document.getElementById('cursor-readout');
    const infoEl = document.getElementById('info');
    const searchInput = /** @type {HTMLInputElement} */ (document.getElementById('search'));
    const radixSelect = /** @type {HTMLSelectElement} */ (document.getElementById('radix-select'));

    // Colors by signal type
    const TYPE_COLORS = {
        clock: '#4caf50',
        reset: '#ff9800',
        bit: '#64b5f6',
        nat: '#4fc3f7',
        int: '#7986cb',
        fp32: '#ba68c8',
        fp16: '#ce93d8',
        fp64: '#ab47bc',
        logic: '#90a4ae',
        default: '#64b5f6'
    };

    function getSignalColor(sig) {
        if (sig.display && sig.display.color) { return sig.display.color; }
        return TYPE_COLORS[sig.type] || TYPE_COLORS.default;
    }

    function resizeCanvas() {
        render();
    }

    function formatValue(hexVal, width, rdx, type) {
        if (!hexVal) { return 'x'; }
        const bigVal = BigInt('0x' + hexVal);

        switch (rdx) {
            case 'binary':
                return bigVal.toString(2).padStart(width, '0');
            case 'decimal':
                return bigVal.toString(10);
            case 'signed': {
                if (width > 0 && (bigVal >> BigInt(width - 1)) & 1n) {
                    const mask = (1n << BigInt(width)) - 1n;
                    return (-(((~bigVal) & mask) + 1n)).toString(10);
                }
                return bigVal.toString(10);
            }
            case 'float': {
                if (type === 'fp32' && width === 32) {
                    const buf = new ArrayBuffer(4);
                    new DataView(buf).setUint32(0, Number(bigVal));
                    return new DataView(buf).getFloat32(0).toPrecision(6);
                }
                if (type === 'fp16' && width === 16) {
                    const buf = new ArrayBuffer(2);
                    new DataView(buf).setUint16(0, Number(bigVal));
                    // Simple fp16 decode
                    const bits = Number(bigVal);
                    const sign = (bits >> 15) & 1;
                    const exp = (bits >> 10) & 0x1f;
                    const frac = bits & 0x3ff;
                    if (exp === 0) { return sign ? '-0' : '0'; }
                    if (exp === 31) { return frac ? 'NaN' : (sign ? '-Inf' : 'Inf'); }
                    const val = Math.pow(-1, sign) * Math.pow(2, exp - 15) * (1 + frac / 1024);
                    return val.toPrecision(4);
                }
                return '0x' + hexVal;
            }
            case 'hex':
            default:
                return '0x' + hexVal;
        }
    }

    function getValueAtTime(changes, time) {
        if (!changes || changes.length === 0) { return null; }
        let val = changes[0][1];
        for (const [t, v] of changes) {
            if (t > time) { break; }
            val = v;
        }
        return val;
    }

    function shortName(fullName) {
        // Show leaf name for display (group header provides hierarchy context)
        const parts = fullName.split('.');
        return parts[parts.length - 1];
    }

    function buildSignalList() {
        if (!waveformData) { return; }

        const signals = waveformData.signals;
        const filter = filterText.toLowerCase();

        // Group signals by explicit group or auto-group by hierarchy prefix
        const groups = {};
        const ungrouped = [];

        for (const sig of signals) {
            if (filter && !sig.name.toLowerCase().includes(filter)) { continue; }

            if (sig.group) {
                if (!groups[sig.group]) { groups[sig.group] = []; }
                groups[sig.group].push(sig);
            } else if (sig.name.includes('.')) {
                const lastDot = sig.name.lastIndexOf('.');
                const prefix = sig.name.substring(0, lastDot);
                if (!groups[prefix]) { groups[prefix] = []; }
                groups[prefix].push(sig);
            } else {
                ungrouped.push(sig);
            }
        }

        displayRows = [];
        visibleSignals = [];
        let html = '';

        // Helper to emit a group and its signals
        function emitGroup(groupName, sigs, useShortName) {
            const collapsed = collapsedGroups.has(groupName);
            const count = sigs.length;
            displayRows.push({ type: 'group', groupName });
            html += `<div class="group-header${collapsed ? ' collapsed' : ''}" data-group="${groupName}">${groupName} <span class="group-count">(${count})</span></div>`;
            if (!collapsed) {
                for (const sig of sigs) {
                    displayRows.push({ type: 'signal', name: sig.name });
                    visibleSignals.push(sig.name);
                    const val = cursorTime >= 0 ? getValueAtTime(waveformData.changes[sig.name], cursorTime) : '';
                    const fmtVal = val ? formatValue(val, sig.width, radix, sig.type) : '';
                    const sel = sig.name === selectedSignal ? ' selected' : '';
                    const label = useShortName ? shortName(sig.name) : sig.name;
                    html += `<div class="signal-row${sel}" data-name="${sig.name}">
                        <span class="signal-name" style="color:${getSignalColor(sig)}">${label}</span>
                        <span class="signal-value">${fmtVal}</span>
                    </div>`;
                }
            }
        }

        if (ungrouped.length > 0) {
            emitGroup('top', ungrouped, false);
        }

        const sortedGroups = Object.keys(groups).sort();
        for (const groupName of sortedGroups) {
            emitGroup(groupName, groups[groupName], true);
        }

        const savedScroll = signalListEl.scrollTop;
        signalListEl.innerHTML = html;
        isSyncingScroll = true;
        signalListEl.scrollTop = savedScroll;
        scrollY = signalListEl.scrollTop;
        isSyncingScroll = false;

        // Click handlers for signal rows
        signalListEl.querySelectorAll('.signal-row').forEach(row => {
            row.addEventListener('click', () => {
                selectedSignal = row.getAttribute('data-name');
                buildSignalList();
                render();
            });
        });

        // Click handlers for group headers — toggle collapse, rebuild both panels
        signalListEl.querySelectorAll('.group-header').forEach(hdr => {
            hdr.addEventListener('click', () => {
                const group = hdr.getAttribute('data-group');
                if (collapsedGroups.has(group)) {
                    collapsedGroups.delete(group);
                } else {
                    collapsedGroups.add(group);
                }
                buildSignalList();
                render();
            });
        });
    }

    function render() {
        if (!ctx || !waveformData) { return; }

        const container = canvas.parentElement;
        const w = container.clientWidth - SIGNAL_LIST_WIDTH;
        const h = container.clientHeight;
        const dpr = window.devicePixelRatio || 1;

        canvas.width = w * dpr;
        canvas.height = h * dpr;
        canvas.style.width = w + 'px';
        canvas.style.height = h + 'px';
        ctx.scale(dpr, dpr);

        // Clear
        ctx.fillStyle = getComputedStyle(document.body).getPropertyValue('--vscode-editor-background') || '#1e1e1e';
        ctx.fillRect(0, 0, w, h);

        const endTime = waveformData.endTime || 10000;
        const timePerPixel = endTime / (w * zoom);
        const startTime = scrollX * timePerPixel;

        // Time ruler
        ctx.fillStyle = getComputedStyle(document.body).getPropertyValue('--vscode-editorLineNumber-foreground') || '#888';
        ctx.font = CANVAS_FONT;
        ctx.textAlign = 'center';

        const tickInterval = computeTickInterval(endTime, w, zoom);
        const firstTick = Math.ceil(startTime / tickInterval) * tickInterval;

        for (let t = firstTick; t <= startTime + w * timePerPixel; t += tickInterval) {
            const x = (t - startTime) / timePerPixel;
            ctx.fillText(formatTime(t, waveformData.timescale), x, HEADER_HEIGHT - 6);
            ctx.strokeStyle = 'rgba(128,128,128,0.2)';
            ctx.beginPath();
            ctx.moveTo(x, HEADER_HEIGHT);
            ctx.lineTo(x, h);
            ctx.stroke();
        }

        // Waveforms — iterate displayRows for 1:1 alignment with signal list
        const yOffset = HEADER_HEIGHT - scrollY;
        for (let i = 0; i < displayRows.length; i++) {
            const row = displayRows[i];
            const y = yOffset + i * ROW_HEIGHT;
            if (y + ROW_HEIGHT < HEADER_HEIGHT || y > h) { continue; }

            if (row.type === 'group') {
                // Group header spacer row — subtle background
                ctx.fillStyle = 'rgba(128,128,128,0.08)';
                ctx.fillRect(0, y, w, ROW_HEIGHT);
                ctx.strokeStyle = 'rgba(128,128,128,0.2)';
                ctx.beginPath();
                ctx.moveTo(0, y + ROW_HEIGHT);
                ctx.lineTo(w, y + ROW_HEIGHT);
                ctx.stroke();
                continue;
            }

            const sigName = row.name;
            const sig = waveformData.signals.find(s => s.name === sigName);
            if (!sig) { continue; }

            const changes = waveformData.changes[sigName] || [];
            const color = getSignalColor(sig);

            if (sig.width === 1) {
                drawBitWaveform(ctx, changes, startTime, timePerPixel, y + 4, ROW_HEIGHT - 8, w, color);
            } else {
                drawBusWaveform(ctx, changes, sig, startTime, timePerPixel, y + 2, ROW_HEIGHT - 4, w, color);
            }

            // Highlight selected
            if (sigName === selectedSignal) {
                ctx.fillStyle = 'rgba(255,255,255,0.05)';
                ctx.fillRect(0, y, w, ROW_HEIGHT);
            }

            // Highlight signals triggered by breakpoint (red glow)
            if (highlightedSignals.has(sigName)) {
                ctx.fillStyle = 'rgba(244,67,54,0.12)';
                ctx.fillRect(0, y, w, ROW_HEIGHT);
            }
        }

        // Annotations
        if (waveformData.annotations) {
            for (const ann of waveformData.annotations) {
                const x = (ann.time - startTime) / timePerPixel;
                if (x < 0 || x > w) { continue; }
                ctx.strokeStyle = ann.color || '#ff9800';
                ctx.setLineDash([4, 2]);
                ctx.beginPath();
                ctx.moveTo(x, HEADER_HEIGHT);
                ctx.lineTo(x, h);
                ctx.stroke();
                ctx.setLineDash([]);
                ctx.fillStyle = ann.color || '#ff9800';
                ctx.font = CANVAS_FONT_SMALL;
                ctx.textAlign = 'left';
                ctx.fillText(ann.label, x + 4, HEADER_HEIGHT + FONT_SIZE + 2);
            }
        }

        // Hover guideline
        if (hoverX >= 0) {
            ctx.strokeStyle = 'rgba(255,255,255,0.25)';
            ctx.lineWidth = 1;
            ctx.setLineDash([4, 4]);
            ctx.beginPath();
            ctx.moveTo(hoverX, 0);
            ctx.lineTo(hoverX, h);
            ctx.stroke();
            ctx.setLineDash([]);
        }

        // Breakpoint markers (red dashed vertical lines)
        for (const marker of breakpointMarkers) {
            const mx = (marker.cycle - startTime) / timePerPixel;
            if (mx < -10 || mx > w + 10) { continue; }
            ctx.strokeStyle = marker.color || '#f44336';
            ctx.lineWidth = 1.5;
            ctx.setLineDash([6, 3]);
            ctx.beginPath();
            ctx.moveTo(mx, HEADER_HEIGHT);
            ctx.lineTo(mx, h);
            ctx.stroke();
            ctx.setLineDash([]);
            // Label
            if (marker.label) {
                ctx.fillStyle = marker.color || '#f44336';
                ctx.font = CANVAS_FONT_SMALL;
                ctx.textAlign = 'left';
                ctx.fillText(marker.label, mx + 4, HEADER_HEIGHT + FONT_SIZE + 14);
            }
        }

        // Cursor (placed on click — solid yellow line)
        if (cursorTime >= 0) {
            const cx = (cursorTime - startTime) / timePerPixel;
            ctx.strokeStyle = '#ffeb3b';
            ctx.lineWidth = 1;
            ctx.beginPath();
            ctx.moveTo(cx, 0);
            ctx.lineTo(cx, h);
            ctx.stroke();
        }
    }

    function drawBitWaveform(ctx, changes, startTime, timePerPixel, y, height, canvasWidth, color) {
        ctx.strokeStyle = color;
        ctx.lineWidth = 1.5;
        ctx.beginPath();

        let prevVal = 0;
        let started = false;
        const endTime = waveformData ? (waveformData.endTime || 10000) : 10000;
        const endX = Math.min(canvasWidth, (endTime - startTime) / timePerPixel);

        for (let ci = 0; ci < changes.length; ci++) {
            const [t, hexVal] = changes[ci];
            const val = parseInt(hexVal, 16) & 1;
            const x = (t - startTime) / timePerPixel;

            if (!started) {
                const py = val ? y : y + height;
                ctx.moveTo(Math.max(0, x), py);
                started = true;
                prevVal = val;
                continue;
            }

            // Vertical transition
            const prevY = prevVal ? y : y + height;
            const newY = val ? y : y + height;
            ctx.lineTo(x, prevY);
            ctx.lineTo(x, newY);
            prevVal = val;
        }

        // Extend to simulation end (not canvas edge)
        if (started) {
            const lastY = prevVal ? y : y + height;
            ctx.lineTo(endX, lastY);
        }

        ctx.stroke();
    }

    function drawBusWaveform(ctx, changes, sig, startTime, timePerPixel, y, height, canvasWidth, color) {
        if (changes.length === 0) { return; }

        ctx.strokeStyle = color;
        ctx.lineWidth = 1;

        for (let ci = 0; ci < changes.length; ci++) {
            const [t, hexVal] = changes[ci];
            const nextT = ci + 1 < changes.length ? changes[ci + 1][0] : (waveformData.endTime || t + 100);

            const x1 = Math.max(0, (t - startTime) / timePerPixel);
            const x2 = Math.min(canvasWidth, (nextT - startTime) / timePerPixel);

            if (x2 < 0 || x1 > canvasWidth) { continue; }

            const diamond = 3;

            // Bus diamond shape
            ctx.beginPath();
            ctx.moveTo(x1, y + height / 2);
            ctx.lineTo(x1 + diamond, y);
            ctx.lineTo(x2 - diamond, y);
            ctx.lineTo(x2, y + height / 2);
            ctx.lineTo(x2 - diamond, y + height);
            ctx.lineTo(x1 + diamond, y + height);
            ctx.closePath();
            ctx.fillStyle = color + '20';
            ctx.fill();
            ctx.stroke();

            // Value label
            const labelWidth = x2 - x1;
            if (labelWidth > 30) {
                const fmtVal = formatValue(hexVal, sig.width, radix, sig.type);
                ctx.fillStyle = color;
                ctx.font = CANVAS_FONT;
                ctx.textAlign = 'center';
                ctx.fillText(fmtVal, (x1 + x2) / 2, y + height / 2 + Math.round(FONT_SIZE * 0.35), labelWidth - 10);
            }
        }
    }

    function computeTickInterval(endTime, canvasWidth, zoomLevel) {
        const visibleTime = endTime / zoomLevel;
        const desiredTicks = canvasWidth / 100;
        const raw = visibleTime / desiredTicks;
        const mag = Math.pow(10, Math.floor(Math.log10(raw)));
        if (raw / mag < 2) { return 2 * mag; }
        if (raw / mag < 5) { return 5 * mag; }
        return 10 * mag;
    }

    function formatTime(t, timescale) {
        if (t >= 1e9) { return (t / 1e9).toFixed(1) + 's'; }
        if (t >= 1e6) { return (t / 1e6).toFixed(1) + 'ms'; }
        if (t >= 1e3) { return (t / 1e3).toFixed(1) + 'us'; }
        return t + (timescale || 'ns');
    }

    // --- Event Handlers ---

    canvas.addEventListener('mousemove', (e) => {
        const rect = canvas.getBoundingClientRect();
        hoverX = e.clientX - rect.left;
        render();
    });

    canvas.addEventListener('mouseleave', () => {
        hoverX = -1;
        render();
    });

    canvas.addEventListener('click', (e) => {
        if (!waveformData) { return; }
        const rect = canvas.getBoundingClientRect();
        const x = e.clientX - rect.left;
        const endTime = waveformData.endTime || 10000;
        const timePerPixel = endTime / (canvas.clientWidth * zoom);
        cursorTime = scrollX * timePerPixel + x * timePerPixel;
        buildSignalList();
        render();
        updateCursorReadout();
    });

    canvas.addEventListener('wheel', (e) => {
        e.preventDefault();
        if (e.ctrlKey || e.metaKey) {
            // Zoom — scale factor by deltaY magnitude for smooth trackpad pinch
            const sensitivity = 0.005;
            const factor = Math.exp(-e.deltaY * sensitivity);
            zoom = Math.max(0.01, Math.min(1000, zoom * factor));
        } else if (e.shiftKey || Math.abs(e.deltaX) > Math.abs(e.deltaY)) {
            // Horizontal scroll (shift+wheel or trackpad horizontal swipe)
            const delta = e.shiftKey ? e.deltaY : e.deltaX;
            scrollX = Math.max(0, scrollX + delta);
        } else {
            // Vertical scroll — sync signal list
            scrollY = Math.max(0, scrollY + e.deltaY);
            isSyncingScroll = true;
            signalListEl.scrollTop = scrollY;
            isSyncingScroll = false;
        }
        render();
    }, { passive: false });

    // Signal list scroll → sync canvas
    signalListEl.addEventListener('scroll', () => {
        if (isSyncingScroll) { return; }
        scrollY = signalListEl.scrollTop;
        render();
    });

    document.getElementById('btn-zoom-in').addEventListener('click', () => {
        zoom *= 1.5;
        render();
    });

    document.getElementById('btn-zoom-out').addEventListener('click', () => {
        zoom = Math.max(0.01, zoom / 1.5);
        render();
    });

    document.getElementById('btn-zoom-fit').addEventListener('click', () => {
        zoom = 1.0;
        scrollX = 0;
        render();
    });

    radixSelect.addEventListener('change', () => {
        radix = radixSelect.value;
        buildSignalList();
        render();
    });

    searchInput.addEventListener('input', () => {
        filterText = searchInput.value;
        buildSignalList();
        render();
    });

    // --- Snap to transition (arrow keys) ---

    function findNextTransition(sigName, afterTime) {
        const changes = waveformData && waveformData.changes[sigName];
        if (!changes) { return null; }
        for (const [t, _v] of changes) {
            if (t > afterTime) { return t; }
        }
        return null;
    }

    function findPrevTransition(sigName, beforeTime) {
        const changes = waveformData && waveformData.changes[sigName];
        if (!changes) { return null; }
        let prev = null;
        for (const [t, _v] of changes) {
            if (t >= beforeTime) { break; }
            prev = t;
        }
        return prev;
    }

    function snapCursorToTransition(direction) {
        if (!selectedSignal || !waveformData) { return; }
        const t = direction > 0
            ? findNextTransition(selectedSignal, cursorTime)
            : findPrevTransition(selectedSignal, cursorTime);
        if (t === null) { return; }

        cursorTime = t;

        // Auto-scroll to keep cursor visible
        const container = canvas.parentElement;
        const w = container.clientWidth - SIGNAL_LIST_WIDTH;
        const endTime = waveformData.endTime || 10000;
        const timePerPixel = endTime / (w * zoom);
        const startTime = scrollX * timePerPixel;
        const cx = (cursorTime - startTime) / timePerPixel;
        if (cx < 0 || cx > w) {
            // Center cursor on screen
            scrollX = Math.max(0, (cursorTime / timePerPixel) - w / 2);
        }

        buildSignalList();
        render();
        updateCursorReadout();
    }

    document.addEventListener('keydown', (e) => {
        // Don't capture when typing in search input
        if (e.target === searchInput) { return; }
        if (e.key === 'ArrowRight') {
            e.preventDefault();
            snapCursorToTransition(1);
        } else if (e.key === 'ArrowLeft') {
            e.preventDefault();
            snapCursorToTransition(-1);
        }
    });

    function updateCursorReadout() {
        if (cursorTime < 0 || !waveformData) {
            cursorReadout.style.display = 'none';
            return;
        }
        cursorReadout.style.display = 'block';
        const ts = waveformData.timescale || 'ns';
        cursorReadout.textContent = `t = ${cursorTime.toFixed(0)} ${ts}`;
    }

    // --- Message Handler ---

    window.addEventListener('message', (event) => {
        const msg = event.data;
        if (msg.type === 'loadWaveform') {
            waveformData = msg.data;
            infoEl.textContent = `${waveformData.design || 'Design'} — ${waveformData.signals.length} signals, ${waveformData.endTime || '?'} ${waveformData.timescale || 'ns'}`;
            buildSignalList();
            resizeCanvas();
        } else if (msg.type === 'scrollToCycle') {
            // Scroll waveform to center on a specific cycle (e.g., breakpoint hit)
            if (!waveformData) { return; }
            cursorTime = msg.cycle;
            const container = canvas.parentElement;
            const w = container.clientWidth - SIGNAL_LIST_WIDTH;
            const endTime = waveformData.endTime || 10000;
            const timePerPixel = endTime / (w * zoom);
            // Center the cycle on screen
            scrollX = Math.max(0, (cursorTime / timePerPixel) - w / 2);
            buildSignalList();
            render();
            updateCursorReadout();
        } else if (msg.type === 'addBreakpointMarker') {
            breakpointMarkers.push({
                cycle: msg.cycle,
                label: msg.label || null,
                color: msg.color || '#f44336',
            });
            render();
        } else if (msg.type === 'clearBreakpointMarkers') {
            breakpointMarkers = [];
            render();
        } else if (msg.type === 'highlightSignals') {
            highlightedSignals = new Set(msg.signalNames || []);
            buildSignalList();
            render();
        }
    });

    window.addEventListener('resize', resizeCanvas);

    // Request data on load
    vscode.postMessage({ type: 'requestData' });
})();

(function () {
  const statusEl = document.getElementById("status");
  const cellsEl = document.getElementById("cells");

  function setStatus(text, cls) {
    statusEl.textContent = text;
    statusEl.className = cls || "";
  }

  // Build a transient div, return all its child sections.
  function parseFragment(html) {
    const tmp = document.createElement("div");
    tmp.innerHTML = html;
    return tmp.children;
  }

  // The wire protocol is absolute-state: every push contains every
  // cell. We blow away the existing DOM and rebuild from the message.
  // This means CSS animations on [data-status=fresh] retrigger only
  // for cells the server actually re-rendered (cached cells come in
  // with status="cached" and don't flash). The server still does the
  // dep-aware work; we just don't bother with diffing on the wire.
  function applyInit(msg) {
    cellsEl.replaceChildren();
    for (const c of msg.cells) {
      const nodes = parseFragment(c.html);
      for (const n of nodes) cellsEl.appendChild(n);
    }
  }

  function applyMessage(msg) {
    if (msg.type === "init") {
      applyInit(msg);
    } else if (msg.type === "error") {
      const pre = document.createElement("pre");
      pre.className = "clerk-error";
      pre.textContent = msg.message || "(empty error message)";
      cellsEl.replaceChildren(pre);
    }
  }

  function connect() {
    const proto = location.protocol === "https:" ? "wss:" : "ws:";
    const ws = new WebSocket(proto + "//" + location.host + "/ws");
    setStatus("connecting…");
    ws.onopen = () => setStatus("live", "live");
    ws.onmessage = (ev) => {
      try {
        applyMessage(JSON.parse(ev.data));
      } catch (e) {
        console.error("bad message", ev.data, e);
      }
    };
    ws.onclose = () => {
      setStatus("disconnected — retrying…", "dead");
      setTimeout(connect, 1000);
    };
    ws.onerror = () => ws.close();
  }

  connect();
})();

/// <reference lib="dom" />
// The Casement's page glue: builds the terminal DOM inside a container,
// wires the worker, history, ?seed=, and the possess/release lifecycle.
// Constructed per container element — nothing module-level holds session
// state, so a future page can mount two casements (the diptych).
import { parseSeed, seedFromSearch, type WorkerResponse } from "./protocol.ts";
import { splitResponse } from "./transcript.ts";

function el<K extends keyof HTMLElementTagNameMap>(
  tag: K,
  cls: string,
  parent: HTMLElement,
): HTMLElementTagNameMap[K] {
  const node = document.createElement(tag);
  node.className = cls;
  parent.appendChild(node);
  return node;
}

function mount(container: HTMLElement): void {
  const transcript = el("div", "casement-transcript", container);
  const controls = el("form", "casement-controls", container);
  const seedLabel = el("label", "casement-seedlabel", controls);
  seedLabel.textContent = "seed ";
  const seedInput = el("input", "casement-seed", seedLabel);
  seedInput.type = "text";
  seedInput.inputMode = "numeric";
  seedInput.maxLength = 20;
  const possess = el("button", "casement-possess", controls);
  possess.type = "submit";
  possess.textContent = "possess";
  const prompt = el("form", "casement-promptrow", container);
  const promptMark = el("span", "casement-promptmark", prompt);
  promptMark.textContent = "> ";
  const input = el("input", "casement-input", prompt);
  input.type = "text";
  input.maxLength = 200;
  input.autocomplete = "off";
  input.placeholder = "help lists the verbs";
  const status = el("p", "casement-status", container);

  const worker = new Worker(new URL("./vessel-worker.js", import.meta.url), {
    type: "module",
  });
  const history: string[] = [];
  let historyAt = 0;
  let live = false;
  let busy = false;

  seedInput.value = seedFromSearch(location.search).toString();

  function append(cls: string, text: string): void {
    for (const line of splitResponse(text)) {
      const p = document.createElement("p");
      p.className = cls === "casement-prose" ? line.cls : cls;
      p.textContent = line.text;
      transcript.appendChild(p);
    }
    transcript.scrollTop = transcript.scrollHeight;
  }

  function setIdle(message: string): void {
    busy = false;
    seedInput.disabled = false;
    possess.disabled = false;
    input.disabled = !live;
    status.textContent = message;
    (live ? input : seedInput).focus();
  }

  worker.onmessage = (e: MessageEvent<WorkerResponse>) => {
    const msg = e.data;
    if (msg.type === "started") {
      live = true;
      transcript.replaceChildren();
      append("casement-prose", msg.text);
      setIdle("Possessed. The world stands still; only you move.");
    } else if (msg.type === "error") {
      live = false;
      append("casement-error", msg.text);
      setIdle("The casement is shut. Try another seed.");
    } else {
      append("casement-prose", msg.text);
      if (msg.released) live = false;
      setIdle(live ? "" : "Released. Possess again — any seed is a world.");
    }
  };

  worker.onerror = () => {
    live = false;
    append("casement-error", "The casement is dark: its worker failed to load.");
    setIdle("The casement is shut.");
  };

  controls.onsubmit = (e) => {
    e.preventDefault();
    if (busy) return;
    const seed = parseSeed(seedInput.value);
    if (seed === null) {
      append("casement-error", `'${seedInput.value}' is not a u64 seed.`);
      return;
    }
    busy = true;
    live = false;
    seedInput.disabled = true;
    possess.disabled = true;
    input.disabled = true;
    status.textContent = `The genesis of seed ${seed}… (a few seconds; ` +
      `sky, tectonics, climate, settlements, all from the seed)`;
    worker.postMessage({ type: "start", seed: seed.toString() });
  };

  prompt.onsubmit = (e) => {
    e.preventDefault();
    if (busy || !live) return;
    const line = input.value.trim();
    if (line === "") return;
    history.push(line);
    historyAt = history.length;
    append("casement-echo", `> ${line}`);
    input.value = "";
    busy = true;
    input.disabled = true;
    worker.postMessage({ type: "command", line });
  };

  input.onkeydown = (e) => {
    if (e.key === "ArrowUp" && historyAt > 0) {
      historyAt -= 1;
      input.value = history[historyAt];
      e.preventDefault();
    } else if (e.key === "ArrowDown") {
      historyAt = Math.min(historyAt + 1, history.length);
      input.value = history[historyAt] ?? "";
      e.preventDefault();
    }
  };

  input.disabled = true;
  status.textContent = "Enter a seed and possess.";
  // ?seed=N is a waypost: a link is a world. Auto-possess it.
  if (new URLSearchParams(location.search).get("seed") !== null) {
    controls.requestSubmit();
  }
}

const container = document.getElementById("casement");
if (container) mount(container);

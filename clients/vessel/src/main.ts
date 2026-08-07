/// <reference lib="dom" />
// The Casement's page glue: builds the terminal DOM inside a container,
// wires the worker, history, ?seed=, and the possess/release lifecycle.
// Constructed per container element — nothing module-level holds session
// state, so a future page can mount two casements (the diptych).
import { parseSeed, seedFromSearch, type WorkerResponse } from "./protocol.ts";
import { narrationOf, parseSnapshot, type Sight, sightOf } from "./snapshot.ts";
import { splitResponse } from "./transcript.ts";
import { planCells } from "./pane_plan.ts";
import { chartCells } from "./pane_chart.ts";
import { type PaneGrid, runsOf } from "./pane_cell.ts";

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

/** Draw a pane's grid, plus a caption naming whose eyes coloured it, into
 * `host` — the map `<pre>`. Extracted out of `drawMap`'s closure so it is
 * testable without mounting a page (`main_render_test.ts`).
 *
 * `createElement` + `textContent` per span, **never** `innerHTML`:
 * `pane_plan.ts` draws `mark.noun.charAt(0)` — a sim-authored character, a
 * settlement or creature name's first letter — so a noun beginning with `<`
 * must reach the DOM as text, never get parsed as markup. `map.textContent
 * = wholeString` (the pre-Task-8 shape) was injection-safe by construction;
 * building the DOM ourselves is what makes this a live question.
 *
 * One `<span>` per RUN of like-coloured cells (`runsOf`, from Task 7), not
 * per cell — coalescing keeps node count near the old plain-`textContent`
 * cost instead of multiplying it by the grid's cell count every turn. */
export function renderInto(host: HTMLElement, grid: PaneGrid | null, sight: Sight | null): void {
  host.replaceChildren();
  if (grid) {
    grid.forEach((row, i) => {
      if (i > 0) host.appendChild(document.createTextNode("\n"));
      for (const run of runsOf(row)) {
        const span = document.createElement("span");
        span.textContent = run.text;
        if (run.color) {
          const [r, g, b] = run.color;
          span.style.color = `rgb(${r} ${g} ${b})`;
        }
        host.appendChild(span);
      }
    });
  }
  if (sight) {
    if (grid) host.appendChild(document.createTextNode("\n\n"));
    const caption = document.createElement("span");
    caption.className = "casement-sight";
    const arity = `${sight.channels} channel${sight.channels === 1 ? "" : "s"}, ` +
      `${sight.chromatic} chromatic`;
    caption.textContent = `Seen through ${sight.observer}'s eyes (${arity}), ` +
      `projected ${sight.projection} — preserves ${sight.preserves}.`;
    host.appendChild(caption);
  }
}

function mount(container: HTMLElement): void {
  // The diptych. `main.ts`'s header has anticipated this since The Casement:
  // nothing module-level holds session state, so a page can mount two.
  const panes = el("div", "casement-panes", container);
  const transcript = el("div", "casement-transcript", panes);
  const map = el("pre", "casement-mapview", panes);
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

  /** Redraw the map pane from a parsed snapshot. One band shows at a time:
   * the session refuses `map out` indoors, so the walk-band chart is not
   * derivable inside a building and there is nothing honest to show there.
   * A pane with nothing to draw empties rather than keeping a stale picture
   * on screen — a frozen last-seen chart presented as live is a cheat pane.
   *
   * The whole body is guarded: `planCells`/`chartCells` validate every field
   * they read and refuse rather than throw, but that guarantee lives in
   * those modules, not here — and `renderInto` builds real DOM, which can
   * throw too (a detached node, a hostile host). A pane throw here runs
   * *before* `append` and `setIdle` in every `onmessage` branch below, so an
   * uncaught exception would leave `busy` and `input.disabled` stuck `true`
   * forever — the Casement locks with no error shown, which is worse than a
   * stale or blank pane. The try/catch makes that impossible structurally,
   * at the one call site, rather than by ordering calls correctly at every
   * branch and hoping a future edit does not reorder them back. The catch
   * clears the pane explicitly rather than trusting whatever `renderInto`
   * managed to append before throwing — a half-drawn grid left on screen is
   * a stale picture presented as live, the exact cheat pane this function's
   * header rules out.
   *
   * Task 7 (The Beholding) made both panes return a `PaneGrid` of
   * `{ glyph, color }` cells instead of plain strings; Task 8 draws it —
   * tinted spans via `renderInto`, plus the caption naming whose eyes
   * produced the picture and what the projection dropped. */
  function drawMap(snap: ReturnType<typeof parseSnapshot>): void {
    try {
      const grid = snap ? (planCells(snap) ?? chartCells(snap)) : null;
      const sight = snap ? sightOf(snap) : null;
      renderInto(map, grid, sight);
    } catch (err) {
      console.error("pane render failed; showing no map this turn", err);
      map.replaceChildren();
    }
  }

  worker.onmessage = (e: MessageEvent<WorkerResponse>) => {
    const msg = e.data;
    if (msg.type === "started") {
      live = true;
      transcript.replaceChildren();
      const snap = parseSnapshot(msg.snapshot);
      drawMap(snap);
      append("casement-prose", snap ? narrationOf(snap) : msg.text);
      setIdle("Possessed. The world stands still; only you move.");
    } else if (msg.type === "error") {
      live = false;
      map.replaceChildren();
      append("casement-error", msg.text);
      setIdle("The casement is shut. Try another seed.");
    } else {
      const snap = parseSnapshot(msg.snapshot);
      drawMap(snap);
      append("casement-prose", snap ? narrationOf(snap) : msg.text);
      if (msg.released) live = false;
      setIdle(live ? "" : "Released. Possess again — any seed is a world.");
    }
  };

  worker.onerror = () => {
    live = false;
    map.replaceChildren();
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
    // The map is a live view, not a log — unlike the transcript, which
    // deliberately keeps its history, a stale picture from the previous
    // seed sitting beside "The genesis of seed N…" would contradict
    // `drawMap`'s own rule against keeping a stale picture on screen.
    map.replaceChildren();
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

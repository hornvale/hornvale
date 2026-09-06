/// <reference lib="dom" />
// The Lot exhibit's page glue: builds the four stages inside a container,
// wires the worker, the permalink, and the reveal animations.
//
// **THE CLIENT OWNS THE THEATRE AND NOTHING ELSE** (spec §6.4, decision
// 0022). Every number this file puts on the page is a field of a payload
// the wasm produced; the spin, the crosshair hop, the assembling timeline
// and the log/linear switch are ALL this file does that the sim does not.
// It never draws a life from the odds, because two readers of one permalink
// must see one life, which is only true if the wasm draws it.
//
// Nothing module-level holds session state, so a page could mount two.

import { type Envelope, type WorkerRequest, type WorkerResponse } from "./protocol.ts";
import {
  type Curve,
  type Life,
  parseCurve,
  parseLife,
  parsePlaces,
  type Place,
} from "./payload.ts";
import { type Axis, birthsToPath, xToYear, yearToX } from "./graph.ts";
import { dotRadius, nearestPlace, peakBirths, placeAtSite, placeLabel, project } from "./map.ts";
import { eventsOf, eventX } from "./timeline.ts";
import { byDesignNote, disclaimer, silenceLine, sourceLine, tiles } from "./story.ts";
import { format as formatLink, parse as parseLink } from "./permalink.ts";
import { heroLine, hintFor, spinYears, thousands } from "./stages.ts";

/** The seed this exhibit runs on. Seed 42 is the book's world throughout;
 * the page is an exhibit of one world, not a seed browser. */
const SEED = "42";

/** The map's canvas, at the atlas's own dimensions — 2:1, the aspect an
 * equirectangular projection of a whole sphere has. */
const MAP_W = 1024;
const MAP_H = 512;

/** The graph's viewBox. */
const GRAPH_W = 900;
const GRAPH_H = 220;

/** How long each reveal runs, in milliseconds — and zero for every one of
 * them when the page opened on a permalink, because a reader following a
 * link has already chosen and is waiting to see, not to be shown. */
interface Timings {
  spin: number;
  hop: number;
  timeline: number;
}

const THEATRE: Timings = { spin: 1600, hop: 900, timeline: 1400 };
const INSTANT: Timings = { spin: 0, hop: 0, timeline: 0 };

function el<K extends keyof HTMLElementTagNameMap>(
  tag: K,
  cls: string,
  parent: HTMLElement | SVGElement,
): HTMLElementTagNameMap[K] {
  const node = document.createElement(tag);
  if (cls !== "") node.className = cls;
  parent.appendChild(node);
  return node;
}

function svg<K extends keyof SVGElementTagNameMap>(
  tag: K,
  parent: SVGElement,
  attrs: Record<string, string>,
): SVGElementTagNameMap[K] {
  const node = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [key, value] of Object.entries(attrs)) node.setAttribute(key, value);
  parent.appendChild(node);
  return node;
}

function button(label: string, cls: string, parent: HTMLElement): HTMLButtonElement {
  const b = el("button", `lot-button ${cls}`, parent);
  b.type = "button";
  b.textContent = label;
  return b;
}

/** A year as the page prints it: a whole number, because the payload's
 * fractional birth year is a draw off a continuous curve and the world's
 * own record is annual. */
function year(value: number): string {
  return `${Math.round(value)}`;
}

/** One frame of a linear ramp, resolved when it finishes. `duration` of 0
 * runs the last frame immediately, which is what makes a permalink load
 * skip the theatre without a second code path. */
function ramp(duration: number, onFrame: (t: number) => void): Promise<void> {
  if (duration <= 0) {
    onFrame(1);
    return Promise.resolve();
  }
  return new Promise((resolve) => {
    const started = performance.now();
    const step = (now: number) => {
      const t = Math.min(1, (now - started) / duration);
      onFrame(t);
      if (t < 1) requestAnimationFrame(step);
      else resolve();
    };
    requestAnimationFrame(step);
  });
}

function mount(container: HTMLElement): void {
  // ---- the DOM ----------------------------------------------------
  const status = el("p", "lot-status", container);

  const hero = el("section", "lot-stage lot-hero", container);
  const heroText = el("p", "lot-heroline", hero);
  const heroActions = el("p", "lot-actions", hero);
  const drawButton = button("Draw a random life", "lot-draw", heroActions);

  const when = el("section", "lot-stage lot-when", container);
  when.hidden = true;
  el("h2", "", when).textContent = "When";
  const axisRow = el("p", "lot-axisrow", when);
  const logButton = button("log", "lot-axis lot-axis-on", axisRow);
  const linearButton = button("linear", "lot-axis", axisRow);
  const plot = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  plot.setAttribute("viewBox", `0 0 ${GRAPH_W} ${GRAPH_H + 24}`);
  plot.setAttribute("class", "lot-plot");
  when.appendChild(plot);
  const whenYear = el("p", "lot-bigyear", when);
  const whenHint = el("p", "lot-hint", when);
  const whenActions = el("p", "lot-actions", when);
  const redrawYear = button("Redraw year", "", whenActions);
  const pickYear = button("Select a year", "", whenActions);
  const whenNext = button("Continue", "lot-next", whenActions);

  const where = el("section", "lot-stage lot-where", container);
  where.hidden = true;
  el("h2", "", where).textContent = "Where";
  const canvas = el("canvas", "lot-map", where);
  canvas.width = MAP_W;
  canvas.height = MAP_H;
  const whereCaption = el("p", "lot-caption", where);
  const whereActions = el("p", "lot-actions", where);
  const redrawWhere = button("Redraw location", "", whereActions);
  const pickWhere = button("Select a location", "", whereActions);
  const whereNext = button("Continue", "lot-next", whereActions);

  const lifeStage = el("section", "lot-stage lot-life", container);
  lifeStage.hidden = true;
  el("h2", "", lifeStage).textContent = "Life";
  const odds = el("div", "lot-tiles lot-odds", lifeStage);
  const track = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  track.setAttribute("viewBox", "0 0 900 90");
  track.setAttribute("class", "lot-track");
  lifeStage.appendChild(track);
  const trackNotes = el("ul", "lot-untimed", lifeStage);
  const lifeActions = el("p", "lot-actions", lifeStage);
  const lifeNext = button("Continue", "lot-next", lifeActions);

  const storyStage = el("section", "lot-stage lot-story", container);
  storyStage.hidden = true;
  el("h2", "", storyStage).textContent = "Story";
  const disclaimerLine = el("p", "lot-disclaimer", storyStage);
  const storyTiles = el("div", "lot-tiles", storyStage);
  const byDesign = el("p", "lot-bydesign", storyStage);
  const silences = el("p", "lot-silences", storyStage);
  const sources = el("details", "lot-sources", storyStage);
  const sourcesSummary = el("summary", "", sources);
  sourcesSummary.textContent = "Sources";
  const sourceList = el("ol", "", sources);
  const storyActions = el("p", "lot-actions", storyStage);
  const another = button("Draw another life", "", storyActions);
  const share = button("Share this life", "", storyActions);

  // ---- worker plumbing --------------------------------------------
  const worker = new Worker(new URL("./lot-worker.js", import.meta.url), { type: "module" });
  let nextId = 1;
  const pending = new Map<number, {
    resolve: (json: string) => void;
    reject: (why: Error) => void;
  }>();
  worker.onmessage = (e: MessageEvent<WorkerResponse>) => {
    const reply = e.data;
    const waiter = pending.get(reply.id);
    if (!waiter) return;
    pending.delete(reply.id);
    if (reply.kind === "ok") waiter.resolve(reply.json);
    else waiter.reject(new Error(reply.text));
  };
  worker.onerror = () => {
    for (const waiter of pending.values()) {
      waiter.reject(new Error("the exhibit's worker failed to start"));
    }
    pending.clear();
  };

  function ask(request: WorkerRequest): Promise<string> {
    const id = nextId++;
    return new Promise<string>((resolve, reject) => {
      pending.set(id, { resolve, reject });
      const envelope: Envelope = { id, request };
      worker.postMessage(envelope);
    });
  }

  // ---- session state ----------------------------------------------
  let curve: Curve | null = null;
  let life: Life | null = null;
  let places: Place[] = [];
  let axis: Axis = "log";
  // Starts one below zero so the first draw is lot 0 — the same lot the
  // committed page's first of ten lives is.
  let index = -1n;
  let pick: { year: number | null; site: number | null } = { year: null, site: null };
  let timings: Timings = THEATRE;
  let picking: "year" | "site" | null = null;
  let busy = false;

  function say(message: string): void {
    status.textContent = message;
  }

  function setBusy(state: boolean): void {
    busy = state;
    for (
      const b of [
        drawButton,
        redrawYear,
        pickYear,
        whenNext,
        redrawWhere,
        pickWhere,
        whereNext,
        lifeNext,
        another,
        share,
      ]
    ) {
      b.disabled = state;
    }
  }

  /** A fresh index for a "redraw": the next one along, so redrawing walks
   * the world's lots rather than landing on the same few. The index IS the
   * key (decision 0796) — nothing here is random. */
  function nextIndex(): bigint {
    index += 1n;
    return index;
  }

  // ---- the When stage ---------------------------------------------
  function drawGraph(): void {
    if (!curve) return;
    while (plot.firstChild) plot.removeChild(plot.firstChild);
    const graph = birthsToPath(curve, axis, GRAPH_W, GRAPH_H);
    svg("path", plot, { d: graph.area, class: "lot-plot-area" });
    svg("path", plot, { d: graph.d, class: "lot-plot-line", fill: "none" });
    for (const tick of graph.ticks) {
      svg("line", plot, {
        x1: `${tick.x}`,
        y1: `${GRAPH_H}`,
        x2: `${tick.x}`,
        y2: `${GRAPH_H + 5}`,
        class: "lot-plot-tick",
      });
      const label = svg("text", plot, {
        x: `${tick.x}`,
        y: `${GRAPH_H + 18}`,
        class: "lot-plot-label",
        "text-anchor": "middle",
      });
      label.textContent = tick.label;
    }
    if (life) {
      // The drawn year's mark, placed by the SAME forward map the plot's
      // own points come from, so the mark and the curve cannot disagree
      // about where a year is on whichever axis is showing.
      const at = yearToX(curve, axis, life.birth_year, GRAPH_W);
      svg("line", plot, {
        x1: `${at}`,
        y1: "0",
        x2: `${at}`,
        y2: `${GRAPH_H}`,
        class: "lot-plot-mark",
      });
    }
  }

  async function showWhen(): Promise<void> {
    if (!curve || !life) return;
    when.hidden = false;
    drawGraph();
    whenHint.textContent = hintFor(curve);
    const frames = timings.spin <= 0 ? 1 : 30;
    const spin = spinYears(curve, life.birth_year, frames);
    await ramp(timings.spin, (t) => {
      const at = Math.min(spin.length - 1, Math.floor(t * spin.length));
      whenYear.textContent = year(spin[at]);
    });
    whenYear.textContent = year(life.birth_year);
    drawGraph();
  }

  // ---- the Where stage --------------------------------------------
  function drawMap(crosshair: { x: number; y: number } | null): void {
    const ctx = canvas.getContext("2d");
    if (!ctx) return;
    ctx.clearRect(0, 0, MAP_W, MAP_H);
    ctx.fillStyle = "rgba(120,140,160,0.10)";
    ctx.fillRect(0, 0, MAP_W, MAP_H);
    const peak = peakBirths(places);
    for (const place of places) {
      const at = project(place.latitude, place.longitude, MAP_W, MAP_H);
      ctx.beginPath();
      ctx.arc(at.x, at.y, dotRadius(place, peak), 0, Math.PI * 2);
      ctx.fillStyle = "rgba(184,134,11,0.55)";
      ctx.fill();
    }
    if (crosshair) {
      ctx.strokeStyle = "#b3554d";
      ctx.lineWidth = 1.5;
      ctx.beginPath();
      ctx.moveTo(crosshair.x - 12, crosshair.y);
      ctx.lineTo(crosshair.x + 12, crosshair.y);
      ctx.moveTo(crosshair.x, crosshair.y - 12);
      ctx.lineTo(crosshair.x, crosshair.y + 12);
      ctx.stroke();
      ctx.beginPath();
      ctx.arc(crosshair.x, crosshair.y, 7, 0, Math.PI * 2);
      ctx.stroke();
    }
  }

  async function showWhere(): Promise<void> {
    if (!life) return;
    where.hidden = false;
    say("Reading the map…");
    const doc = parsePlaces(await ask({ kind: "places", year: life.birth_year }));
    places = doc.places;
    say("");
    const home = placeAtSite(places, life.site);
    const target = home === null
      ? { x: MAP_W / 2, y: MAP_H / 2 }
      : project(home.latitude, home.longitude, MAP_W, MAP_H);
    // The crosshair hops in from another populated site rather than fading
    // in on the answer — theatre over payload positions, both of them real.
    const start = places.length > 0
      ? project(places[0].latitude, places[0].longitude, MAP_W, MAP_H)
      : target;
    await ramp(timings.hop, (t) => {
      const ease = 1 - (1 - t) ** 3;
      drawMap({
        x: start.x + (target.x - start.x) * ease,
        y: start.y + (target.y - start.y) * ease,
      });
    });
    drawMap(target);
    whereCaption.textContent = home === null
      ? `Site ${life.site}, which the year's map does not list — the community had gone.`
      : `${placeLabel(home)}, a ${home.people} community of about ${
        thousands(home.population)
      } in year ${year(doc.year)}.`;
  }

  // ---- the Life stage ---------------------------------------------
  function oddsTiles(subject: Life): [string, string][] {
    // EVERY ONE OF THESE IS A FIELD OF `lot/life/v1`. The odds payload's
    // e0 and q_maturity would belong here and are NOT reachable: there is
    // no `hl_lot_odds` export, so the exhibit shows the life course the
    // payload carries rather than the mortality profile behind it.
    const rows: [string, string][] = [
      ["Born", year(subject.birth_year)],
      ["Age at death", `${Math.round(subject.age_at_death)}`],
      ["Reached maturity", subject.matured ? "yes" : "no"],
      ["Ending", subject.ending.cause ?? subject.ending.kind],
      ["Community's curve", subject.shape],
    ];
    if (subject.moved_year !== null) {
      rows.push(["Moved", `in year ${year(subject.moved_year)}`]);
    }
    return rows;
  }

  async function showLife(): Promise<void> {
    if (!life) return;
    lifeStage.hidden = false;
    odds.replaceChildren();
    for (const [label, value] of oddsTiles(life)) {
      const tile = el("div", "lot-tile", odds);
      el("span", "lot-tile-label", tile).textContent = label;
      el("span", "lot-tile-value", tile).textContent = value;
    }

    const events = eventsOf(life);
    while (track.firstChild) track.removeChild(track.firstChild);
    svg("line", track, { x1: "20", y1: "30", x2: "880", y2: "30", class: "lot-track-axis" });
    const marks: SVGElement[] = [];
    for (const event of events) {
      const x = eventX(event, life, 860);
      if (x === null) continue;
      const mark = svg("g", track, { class: `lot-track-mark lot-track-${event.kind}` });
      svg("circle", mark, { cx: `${20 + x}`, cy: "30", r: "5" });
      const label = svg("text", mark, {
        x: `${20 + x}`,
        y: "56",
        "text-anchor": x > 700 ? "end" : x < 60 ? "start" : "middle",
      });
      label.textContent = event.label;
      mark.setAttribute("opacity", "0");
      marks.push(mark);
    }
    await ramp(timings.timeline, (t) => {
      marks.forEach((mark, at) => {
        const arrives = marks.length <= 1 ? 0 : at / marks.length;
        mark.setAttribute("opacity", t >= arrives ? "1" : "0");
      });
    });
    for (const mark of marks) mark.setAttribute("opacity", "1");

    trackNotes.replaceChildren();
    for (const event of events) {
      if (event.age !== null) continue;
      el("li", "", trackNotes).textContent = event.label;
    }
  }

  // ---- the Story stage --------------------------------------------
  function showStory(): void {
    if (!life) return;
    storyStage.hidden = false;
    disclaimerLine.textContent = disclaimer(life);
    storyTiles.replaceChildren();
    for (const tile of tiles(life)) {
      const node = el("div", tile.value === null ? "lot-tile lot-absent" : "lot-tile", storyTiles);
      el("span", "lot-tile-label", node).textContent = tile.label;
      const value = el("span", "lot-tile-value", node);
      value.textContent = tile.value ?? tile.reason ?? "the record does not say";
      if (tile.sources.length > 0) {
        el("span", "lot-tile-refs", node).textContent = tile.sources.map((n) => `[${n}]`).join(" ");
      }
    }
    const note = byDesignNote(life);
    byDesign.textContent = note ?? "";
    byDesign.hidden = note === null;
    silences.textContent = silenceLine(life);
    sourceList.replaceChildren();
    for (const source of life.sources) {
      el("li", "", sourceList).textContent = sourceLine(source);
    }
  }

  // ---- the draw ----------------------------------------------------
  function permalink(): string {
    return formatLink(SEED, index.toString(), pick);
  }

  async function draw(): Promise<void> {
    setBusy(true);
    say("Drawing…");
    try {
      const json = await ask({
        kind: "lot",
        index: index.toString(),
        year: pick.year,
        site: pick.site,
      });
      life = parseLife(json);
      say("");
      globalThis.history.replaceState(null, "", permalink());
      await showWhen();
      await showWhere();
      await showLife();
      showStory();
    } catch (err) {
      // The sim's own sentence — a refused pin names the physical reason.
      // Say plainly that nothing moved: the stages below still show the
      // PREVIOUS life, and a bare error over an unchanged page reads as if
      // the page were showing the life that failed to draw.
      const why = err instanceof Error ? err.message : `${err}`;
      say(life === null ? why : `${why} — the life below is the previous draw, unchanged.`);
    } finally {
      setBusy(false);
    }
  }

  // ---- wiring ------------------------------------------------------
  function setAxis(mode: Axis): void {
    axis = mode;
    logButton.className = `lot-button lot-axis${mode === "log" ? " lot-axis-on" : ""}`;
    linearButton.className = `lot-button lot-axis${mode === "linear" ? " lot-axis-on" : ""}`;
    drawGraph();
  }
  logButton.addEventListener("click", () => setAxis("log"));
  linearButton.addEventListener("click", () => setAxis("linear"));

  drawButton.addEventListener("click", () => {
    pick = { year: null, site: null };
    index = nextIndex();
    void draw();
  });

  redrawYear.addEventListener("click", () => {
    pick = { year: null, site: null };
    index = nextIndex();
    void draw();
  });

  pickYear.addEventListener("click", () => {
    picking = "year";
    say("Click the graph to choose a birth year.");
    plot.classList.add("lot-picking");
  });

  plot.addEventListener("click", (e: MouseEvent) => {
    if (picking !== "year" || !curve || busy) return;
    const box = plot.getBoundingClientRect();
    const x = ((e.clientX - box.left) / box.width) * GRAPH_W;
    picking = null;
    plot.classList.remove("lot-picking");
    pick = { year: Math.round(xToYear(curve, axis, x, GRAPH_W)), site: null };
    index = nextIndex();
    say("");
    void draw();
  });

  redrawWhere.addEventListener("click", () => {
    // A LOCATION redraw holds the year, which is what the label promises.
    // Carrying `pick.year` alone would not: it is null on any life the
    // reader did not pin a year for, and the new index would then move the
    // birth year too — a "redraw location" that silently redraws When.
    if (!life) return;
    pick = { year: pick.year ?? Math.round(life.birth_year), site: null };
    index = nextIndex();
    void draw();
  });

  pickWhere.addEventListener("click", () => {
    picking = "site";
    say("Click the map to choose a place.");
    canvas.classList.add("lot-picking");
  });

  canvas.addEventListener("click", (e: MouseEvent) => {
    if (picking !== "site" || !life || busy) return;
    const box = canvas.getBoundingClientRect();
    const x = ((e.clientX - box.left) / box.width) * MAP_W;
    const y = ((e.clientY - box.top) / box.height) * MAP_H;
    const chosen = nearestPlace(places, x, y, MAP_W, MAP_H);
    picking = null;
    canvas.classList.remove("lot-picking");
    if (chosen === null) {
      say("No community stood anywhere near there in that year.");
      return;
    }
    // The site pin needs a year pin beside it — the ABI takes both — so a
    // location choice fixes the year already on screen.
    pick = { year: pick.year ?? Math.round(life.birth_year), site: chosen.site };
    index = nextIndex();
    say("");
    void draw();
  });

  for (
    const [b, target] of [[whenNext, where], [whereNext, lifeStage], [
      lifeNext,
      storyStage,
    ]] as const
  ) {
    b.addEventListener(
      "click",
      () => target.scrollIntoView({ behavior: "smooth", block: "start" }),
    );
  }

  another.addEventListener("click", () => {
    pick = { year: null, site: null };
    index = nextIndex();
    void draw();
  });

  share.addEventListener("click", () => {
    const url = `${location.origin}${location.pathname}${permalink()}`;
    void navigator.clipboard?.writeText(url).then(
      () => say("Link copied."),
      () => say(url),
    );
  });

  // ---- boot --------------------------------------------------------
  (async () => {
    setBusy(true);
    say("Deriving the world — this takes a few seconds, every time.");
    try {
      await ask({ kind: "new", seed: SEED });
      curve = parseCurve(await ask({ kind: "curve" }));
      heroText.textContent = heroLine(curve, SEED);
      say("");
      drawGraph();
      const link = parseLink(location.hash);
      if (link !== null && link.seed !== SEED) {
        say(`That link names seed ${link.seed}; this exhibit runs on seed ${SEED}.`);
      } else if (link !== null) {
        // A reader following a link has already chosen and is waiting to
        // SEE, not to be shown: the reveals run at zero duration for this
        // one draw, and the theatre comes back for anything they draw next.
        index = BigInt(link.index);
        pick = { year: link.year, site: link.site };
        timings = INSTANT;
        setBusy(false);
        await draw();
        timings = THEATRE;
        return;
      }
    } catch (err) {
      say(`The exhibit is dark: ${err instanceof Error ? err.message : err}`);
    } finally {
      setBusy(false);
    }
  })();
}

const holder = document.getElementById("lot");
if (holder) mount(holder);

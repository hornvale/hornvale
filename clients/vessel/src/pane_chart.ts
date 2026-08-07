// The `scene/surrounds/v2` reader: the walk band's cells in, glyph rows out.
//
// NOT a port of `windows/scene/src/surrounds_ascii.rs`. That file is 543
// lines, nearly all of it lens machinery — lens tables, the colour
// disclosure sentence, legend prose, marks ranking — and the `map` verb
// still owns all of it. What a pane needs is the placement and a glyph.
// Two renderings of one scene for different purposes is exactly what
// decision 0022 licenses; they are not expected to agree glyph-for-glyph.

import type { Snapshot } from "./snapshot.ts";
import { type PaneCell, type PaneGrid, parseColor } from "./pane_cell.ts";

/** The chart schema tag this pane understands. A different tag — absent,
 * unrecognised, or a future epoch that reuses a field name with new
 * meaning — is refused rather than read, the same discipline
 * `snapshot.ts`'s `parseSnapshot` applies to the envelope. This is an
 * allowlist of the one known-good tag, not a denylist of prior ones: a
 * denylist fails open on a schema nobody anticipated, which is exactly the
 * failure mode a *renamed* field degrades safely from but a *reused* field
 * does not — a silently wrong map is what this whole client exists to
 * avoid drawing. */
const SURROUNDS_SCHEMA = "scene/surrounds/v2";

/** One cell of the chart, as much of it as this pane reads. Lattice fields
 * are `null` on a seam cell — `surrounds.rs` sets them `None` because "the
 * lattice bends and no honest local coordinate exists" there. */
interface ChartCell {
  v: number | null;
  w: number | null;
  up: boolean | null;
  seam: boolean;
  state: string;
  water: number;
  /** Raw, unvalidated — `parseColor` narrows it. `unknown` because a
   * malformed or absent `color` must become an uncoloured cell, never a
   * thrown error. */
  color: unknown;
}

/** The mark for the cell the observer stands in — the same `@` the floor
 * plan uses, deliberately. */
const YOU = "@";

/** Drawn where no cell was placed. */
const EMPTY = " ";

/** The `WaterKind` labels (`domains/terrain/src/water.rs::LEGEND`) that
 * count as water for this pane's coarse land/water distinction. The fourth
 * label, `"dry-land"`, is everything else in that legend — so this has to be
 * a *positive* match against the water kinds, not a negative match against
 * one dry-land spelling. `water_legend` never carries a `"none"` sentinel;
 * a `water !== "none"` check would be true for `"dry-land"` too and draw
 * every dry cell as water — plausible-looking, wrong, and exactly the class
 * of bug the `+ w` placement term is the headline example of. */
const WATER_KINDS = new Set(["ocean", "salt-basin", "river"]);

/** The largest lattice coordinate magnitude this pane will place. The sim
 * cannot currently emit a cell past this — the walk chart's radius is small
 * — so this is hardening against a malformed or hostile payload, not a live
 * bug. Two cells at `v: 20000` build a 40,002-character row; at `v: 1e9` the
 * row exceeds V8's max string length and `String.prototype.repeat`-style
 * growth throws a `RangeError`, landing on the same `main.ts` lockup path an
 * uncaught `TypeError` would. Refusing beats hanging the worker. */
const MAX_COORD = 4096;

/** The cell grid for this snapshot's chart, or `null` when there is no
 * chart to draw.
 *
 * Refusing beats drawing something wrong: a malformed or empty chart looks
 * plausible half-rendered, which is the worse failure. */
export function chartCells(snap: Snapshot): PaneGrid | null {
  const spatial = snap.spatial;
  if (!spatial || spatial.band !== "walk") return null;
  const chart = spatial.chart as
    | { schema?: unknown; cells?: unknown; water_legend?: unknown }
    | null
    | undefined;
  if (chart === null || typeof chart !== "object") return null;
  if (chart.schema !== SURROUNDS_SCHEMA) return null;
  if (!Array.isArray(chart.cells) || chart.cells.length === 0) return null;
  // `.map`, never `.filter`: `cell.water` is a positional index into this
  // legend, so dropping a non-string entry would shift every index after it
  // and silently relabel every later cell — a river reading as land with no
  // refusal. A non-string entry maps to `""`, which no `WATER_KINDS` member
  // equals, so it falls through to a land glyph instead.
  const waterLegend = Array.isArray(chart.water_legend)
    ? chart.water_legend.map((w) => typeof w === "string" ? w : "")
    : [];

  // Placement, reasoning carried over from surrounds_ascii.rs:110-135
  // verbatim:
  //   row = -w;  col = 2v + (up ? 0 : 1) + w
  // The `+ w` term cancels the lattice's row offset. Without it, an
  // up-triangle's horizontal-edge neighbour below it would land
  // down-and-to-the-right instead of directly below, and a breadth-first
  // ball would draw as a right-leaning parallelogram rather than the
  // symmetric hexagon it actually is.
  const placed = new Map<string, PaneCell>();
  let rMin = Infinity, rMax = -Infinity, cMin = Infinity, cMax = -Infinity;
  for (const raw of chart.cells) {
    const cell = parseCell(raw);
    // A seam cell (or a malformed one this parse could not trust) has no
    // honest local coordinate, so there is nowhere correct to draw it.
    // Skipping is honest; guessing is not.
    if (!cell || cell.seam || cell.v === null || cell.w === null || cell.up === null) {
      continue;
    }
    const row = -cell.w;
    const col = 2 * cell.v + (cell.up ? 0 : 1) + cell.w;
    const { glyph, ground } = glyphFor(cell, waterLegend);
    // The colour is BEDROCK reflectance (see `glyphFor`'s doc): it is only
    // a truthful claim about the cell when `glyph` is drawing that ground,
    // so a non-ground glyph (`YOU`, a water glyph) gets `null` regardless of
    // what the payload sent — withheld, not merely unparsed.
    const color = ground ? parseColor(cell.color) : null;
    placed.set(`${row},${col}`, { glyph, color });
    rMin = Math.min(rMin, row);
    rMax = Math.max(rMax, row);
    cMin = Math.min(cMin, col);
    cMax = Math.max(cMax, col);
  }
  if (placed.size === 0) return null;

  const grid: PaneGrid = [];
  for (let r = rMin; r <= rMax; r++) {
    const line: PaneCell[] = [];
    for (let c = cMin; c <= cMax; c++) {
      line.push(placed.get(`${r},${c}`) ?? { glyph: EMPTY, color: null });
    }
    grid.push(line);
  }
  return grid;
}

/** Validate and narrow one raw chart-cell payload, or `null` if it is not
 * shaped like a cell at all. `Snapshot.spatial`'s `chart` field is typed
 * `unknown` on purpose (a client bundle can outlive the sim that produced a
 * payload), so guarding here — rather than trusting a cast — is what turns
 * a malformed cell into a skipped one instead of an uncaught `TypeError`. */
function parseCell(raw: unknown): ChartCell | null {
  if (raw === null || typeof raw !== "object") return null;
  const c = raw as Record<string, unknown>;
  if (typeof c.seam !== "boolean") return null;
  if (typeof c.state !== "string") return null;
  const v = c.v === null ? null : typeof c.v === "number" ? c.v : undefined;
  const w = c.w === null ? null : typeof c.w === "number" ? c.w : undefined;
  const up = c.up === null ? null : typeof c.up === "boolean" ? c.up : undefined;
  if (v === undefined || w === undefined || up === undefined) return null;
  if ((v !== null && Math.abs(v) > MAX_COORD) || (w !== null && Math.abs(w) > MAX_COORD)) {
    return null;
  }
  const water = typeof c.water === "number" ? c.water : -1;
  return { v, w, up, seam: c.seam, state: c.state, water, color: c.color };
}

/** The glyph one cell wears, paired with whether that glyph draws the
 * ground itself. Coarse on purpose: the pane distinguishes where you are,
 * what is water, and what is land. The `map` verb's lenses are where fine
 * distinctions live, and duplicating that table here would be a second
 * thing to keep in step with no test able to see the drift.
 *
 * The `ground` half is ported from `terrain_glyph` in
 * `windows/scene/src/surrounds_ascii.rs`: the sim's per-cell `color` is the
 * reflectance of the cell's *bedrock*, so it is a truthful claim about the
 * cell only when the glyph drawn there *is* that ground. Where the glyph
 * has been overridden to name something else — the observer standing here,
 * or the water covering the ground — the bedrock colour describes something
 * the reader cannot see, and the caller withholds it rather than tinting a
 * river the colour of the rock beneath it. */
function glyphFor(cell: ChartCell, waterLegend: string[]): { glyph: string; ground: boolean } {
  if (cell.state === "here") return { glyph: YOU, ground: false };
  const water = waterLegend[cell.water];
  if (water !== undefined && WATER_KINDS.has(water)) return { glyph: "~", ground: false };
  return { glyph: cell.state === "remembered" ? "," : ".", ground: true };
}

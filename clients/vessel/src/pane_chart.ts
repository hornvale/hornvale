// The `scene/surrounds/v1` reader: the walk band's cells in, glyph rows out.
//
// NOT a port of `windows/scene/src/surrounds_ascii.rs`. That file is 543
// lines, nearly all of it lens machinery — lens tables, the colour
// disclosure sentence, legend prose, marks ranking — and the `map` verb
// still owns all of it. What a pane needs is the placement and a glyph.
// Two renderings of one scene for different purposes is exactly what
// decision 0022 licenses; they are not expected to agree glyph-for-glyph.

import type { Snapshot } from "./snapshot.ts";

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

/** The glyph rows for this snapshot's chart, or `null` when there is no
 * chart to draw.
 *
 * Refusing beats drawing something wrong: a malformed or empty chart looks
 * plausible half-rendered, which is the worse failure. */
export function chartRows(snap: Snapshot): string[] | null {
  const spatial = snap.spatial;
  if (!spatial || spatial.band !== "walk") return null;
  const chart = spatial.chart as
    | { cells?: unknown; water_legend?: unknown }
    | null
    | undefined;
  if (chart === null || typeof chart !== "object") return null;
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
  const placed = new Map<string, string>();
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
    placed.set(`${row},${col}`, glyphFor(cell, waterLegend));
    rMin = Math.min(rMin, row);
    rMax = Math.max(rMax, row);
    cMin = Math.min(cMin, col);
    cMax = Math.max(cMax, col);
  }
  if (placed.size === 0) return null;

  const rows: string[] = [];
  for (let r = rMin; r <= rMax; r++) {
    let line = "";
    for (let c = cMin; c <= cMax; c++) {
      line += placed.get(`${r},${c}`) ?? EMPTY;
    }
    rows.push(line);
  }
  return rows;
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
  const water = typeof c.water === "number" ? c.water : -1;
  return { v, w, up, seam: c.seam, state: c.state, water };
}

/** The glyph one cell wears. Coarse on purpose: the pane distinguishes where
 * you are, what is water, and what is land. The `map` verb's lenses are
 * where fine distinctions live, and duplicating that table here would be a
 * second thing to keep in step with no test able to see the drift. */
function glyphFor(cell: ChartCell, waterLegend: string[]): string {
  if (cell.state === "here") return YOU;
  const water = waterLegend[cell.water];
  if (water !== undefined && WATER_KINDS.has(water)) return "~";
  return cell.state === "remembered" ? "," : ".";
}

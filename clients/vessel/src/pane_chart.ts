// The `scene/surrounds/v2` reader: the walk band's cells in, glyph rows out.
//
// NOT a port of `windows/scene/src/surrounds_ascii.rs`. That file is nearly
// all lens machinery — lens tables, the colour disclosure sentence, legend
// prose, marks ranking — and the `map` verb still owns all of it. What a pane
// needs is the placement and a glyph. Two renderings of one scene for
// different purposes is exactly what decision 0022 licenses; they are not
// expected to agree glyph-for-glyph.
//
// What they DO agree on is the geometry and the collision rule. See
// `placeAt` and `boxRank` below — each is the same decision the sim's
// renderer and `clients/game/core/src/chart.rs` make, written from the same
// statement of it rather than ported from either.

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
 * avoid drawing.
 *
 * The tag did NOT move when the chart went north-up: `bearing_deg` and
 * `distance_rad` arrived additively and `orientation` changed value, not
 * shape. A payload predating them still carries this tag, which is why
 * `parseCell` below refuses a cell missing either one rather than trusting
 * the tag alone. */
const SURROUNDS_SCHEMA = "scene/surrounds/v2";

/** One cell of the chart, as much of it as this pane reads.
 *
 * The lattice offsets (`u`/`v`/`w`/`up`) are gone from this interface: they
 * were the old projection's inputs, they are `null` on a seam cell, and
 * that is precisely why a seam cell could not be drawn. A cell's polar
 * coordinate about the observer exists for every cell, seam or not. */
interface ChartCell {
  /** Great-circle azimuth from the observer, degrees clockwise from north. */
  bearingDeg: number;
  /** Great-circle angular distance from the observer, radians. */
  distanceRad: number;
  state: string;
  water: number;
  /** The smallest `salience` among this cell's marks, or `null` when it
   * carries none. Read for the collision rule alone — the pane draws no
   * per-mark glyph, and `salience` is a RANK where lower is more salient. */
  salience: number | null;
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
 * of bug the north-up column factor is now the headline example of. */
const WATER_KINDS = new Set(["ocean", "salt-basin", "river"]);

/** The largest band radius this pane will draw. The projection scales a
 * band to `radius` rows, so the grid it builds is `2*radius+1` rows by
 * `4*radius+1` columns and nothing else bounds it — a hostile `radius:
 * 1e9` would try to build ~8e18 cells and hang the worker, landing on the
 * same `main.ts` lockup path an uncaught `TypeError` would. The sim's walk
 * band ships at radius 4, so 64 is two orders of headroom over anything
 * real. Refusing beats hanging.
 *
 * This replaced a ceiling on the lattice coordinates themselves, which is
 * the quantity the old projection's grid size depended on. */
const MAX_RINGS = 64;

/** Round half away from zero.
 *
 * NOT `Math.round`, which rounds half toward `+∞`: `Math.round(-0.5)` is
 * `-0` where Rust's `f64::round` gives `-1`. The sim's renderer and
 * `clients/game/core` both use the Rust rule, so a bare `Math.round` here
 * would put a cell one row or column out from where the other two put it,
 * on exactly the half-integers a symmetric chart is full of. */
function roundHalfAwayFromZero(x: number): number {
  return x < 0 ? -Math.round(-x) : Math.round(x);
}

/** A cell's `[row, col]` box, from its polar coordinate about the observer.
 *
 * `bearingDeg` is clockwise from north and `distanceRad` is a great-circle
 * angle, so the offset is `(north, east) = distance * (cos θ, sin θ)`.
 * North is up, which is negative row; east is right, positive column.
 *
 * `farthest` is the largest `distanceRad` in the band and `rings` its BFS
 * radius, so the outermost cell lands `rings` row-units out — one row per
 * ring. The scale is read off the band rather than derived from the sphere
 * because a client has no way to compute a room's angular size, which is
 * the whole reason the wire carries bearing and distance at all.
 *
 * The column doubles because a monospace character cell is about twice as
 * tall as it is wide; without it the chart is an ellipse claiming to be a
 * circle. */
function placeAt(
  cell: ChartCell,
  farthest: number,
  rings: number,
): [number, number] {
  if (!(farthest > 0)) return [0, 0];
  const theta = cell.bearingDeg * Math.PI / 180;
  const r = cell.distanceRad / farthest * rings;
  return [
    roundHalfAwayFromZero(-Math.cos(theta) * r),
    roundHalfAwayFromZero(Math.sin(theta) * r * 2),
  ];
}

/** The rank deciding which cell keeps a box when two round into the same
 * one. Smallest wins, compared left to right.
 *
 * **The rule, stated once: salience ranks, weight inks, and neither becomes
 * the other.** The same three clauses are written out in
 * `windows/scene/src/surrounds_ascii.rs::box_rank` and
 * `clients/game/core/src/chart.rs::box_rank`. Decision 0022 licenses the
 * three renderers to differ in *vocabulary*; it does not license them to
 * differ in this rule.
 *
 * 1. The observer never loses their own box — the chart is egocentric, and
 *    a band that drew over `@` would have lost the one cell the reader is
 *    standing in.
 * 2. A marked cell beats an unmarked one, and among marked cells the
 *    numerically smallest `salience` wins.
 * 3. Ties break on document order, which the producer fixes as ascending
 *    `room`.
 *
 * The epistemic state is deliberately absent. This pane has no weight
 * channel at all (a `PaneCell` is a glyph and a colour), so per spec §2.3
 * it loses the epistemic axis rather than recovering it through the
 * ranking — a remembered cell is neither promoted nor demoted by being
 * remembered. */
function boxRank(
  cell: ChartCell,
  index: number,
): [boolean, boolean, number, number] {
  return [
    cell.state !== "here",
    cell.salience === null,
    cell.salience ?? 0,
    index,
  ];
}

/** Whether `a` outranks `b` — lexicographic over `boxRank`'s tuple, with
 * `false` ordering before `true` so the earlier clauses read as "is NOT the
 * observer" and "is NOT marked". */
function outranks(
  a: [boolean, boolean, number, number],
  b: [boolean, boolean, number, number],
): boolean {
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return a[i] < b[i];
  }
  return false;
}

/** The cell grid for this snapshot's chart, or `null` when there is no
 * chart to draw.
 *
 * Refusing beats drawing something wrong: a malformed or empty chart looks
 * plausible half-rendered, which is the worse failure. */
export function chartCells(snap: Snapshot): PaneGrid | null {
  const spatial = snap.spatial;
  if (!spatial || spatial.band !== "walk") return null;
  const chart = spatial.chart as
    | { schema?: unknown; cells?: unknown; water_legend?: unknown; radius?: unknown }
    | null
    | undefined;
  if (chart === null || typeof chart !== "object") return null;
  if (chart.schema !== SURROUNDS_SCHEMA) return null;
  if (!Array.isArray(chart.cells) || chart.cells.length === 0) return null;
  // The radius sets the scale AND bounds the grid, so it is validated like
  // the schema tag rather than defaulted: a chart that does not say how
  // many rings it spans is one this pane cannot place honestly.
  if (
    typeof chart.radius !== "number" || !Number.isInteger(chart.radius) ||
    chart.radius < 0 || chart.radius > MAX_RINGS
  ) {
    return null;
  }
  const rings = Math.max(1, chart.radius);
  // `.map`, never `.filter`: `cell.water` is a positional index into this
  // legend, so dropping a non-string entry would shift every index after it
  // and silently relabel every later cell — a river reading as land with no
  // refusal. A non-string entry maps to `""`, which no `WATER_KINDS` member
  // equals, so it falls through to a land glyph instead.
  const waterLegend = Array.isArray(chart.water_legend)
    ? chart.water_legend.map((w) => typeof w === "string" ? w : "")
    : [];

  const cells: ChartCell[] = [];
  for (const raw of chart.cells) {
    const cell = parseCell(raw);
    // A cell this parse could not trust has nowhere honest to go. Seam
    // cells are NOT in that category any more: they carry a bearing and a
    // distance like every other cell, and skipping them is what used to
    // leave half of a seam-crossing band blank.
    if (cell) cells.push(cell);
  }
  if (cells.length === 0) return null;
  const farthest = cells.reduce((m, c) => Math.max(m, c.distanceRad), 0);

  // One winner per box, chosen by `boxRank` rather than by whichever cell
  // the loop wrote last.
  const held = new Map<string, { rank: [boolean, boolean, number, number]; cell: ChartCell }>();
  const at = new Map<string, [number, number]>();
  cells.forEach((cell, index) => {
    const [row, col] = placeAt(cell, farthest, rings);
    const key = `${row},${col}`;
    const rank = boxRank(cell, index);
    const sitting = held.get(key);
    if (sitting && !outranks(rank, sitting.rank)) return;
    held.set(key, { rank, cell });
    at.set(key, [row, col]);
  });

  const placed = new Map<string, PaneCell>();
  let rMin = Infinity, rMax = -Infinity, cMin = Infinity, cMax = -Infinity;
  for (const [key, { cell }] of held) {
    const [row, col] = at.get(key)!;
    const { glyph, ground } = glyphFor(cell, waterLegend);
    // The ground rule (see `PaneCell`'s doc in `pane_cell.ts`): withheld,
    // not merely unparsed, on a non-ground glyph (`YOU`, a water glyph) —
    // regardless of what the payload sent.
    const color = ground ? parseColor(cell.color) : null;
    placed.set(key, { glyph, color });
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
 * a malformed cell into a skipped one instead of an uncaught `TypeError`.
 *
 * `bearing_deg` and `distance_rad` are REQUIRED and must be finite: a
 * `scene/surrounds/v2` payload predating them carries the same schema tag,
 * and placing such a cell would put the whole band on the observer's own
 * box. Refusing is the honest degradation — an empty chart, not a chart
 * that says everything is right here. */
function parseCell(raw: unknown): ChartCell | null {
  if (raw === null || typeof raw !== "object") return null;
  const c = raw as Record<string, unknown>;
  if (typeof c.state !== "string") return null;
  const bearingDeg = c.bearing_deg;
  const distanceRad = c.distance_rad;
  if (typeof bearingDeg !== "number" || !Number.isFinite(bearingDeg)) return null;
  if (
    typeof distanceRad !== "number" || !Number.isFinite(distanceRad) ||
    distanceRad < 0
  ) {
    return null;
  }
  const water = typeof c.water === "number" ? c.water : -1;
  return {
    bearingDeg,
    distanceRad,
    state: c.state,
    water,
    salience: parseSalience(c.marks),
    color: c.color,
  };
}

/** The smallest `salience` among a cell's marks, or `null` when it carries
 * none this pane can trust. A malformed entry is skipped rather than
 * treated as salience zero, which would let a broken payload take a box
 * from a real settlement. */
function parseSalience(raw: unknown): number | null {
  if (!Array.isArray(raw)) return null;
  let best: number | null = null;
  for (const m of raw) {
    if (m === null || typeof m !== "object") continue;
    const s = (m as Record<string, unknown>).salience;
    if (typeof s !== "number" || !Number.isFinite(s)) continue;
    if (best === null || s < best) best = s;
  }
  return best;
}

/** The glyph one cell wears, paired with whether that glyph draws the
 * ground itself. Coarse on purpose: the pane distinguishes where you are,
 * what is water, and what is land. The `map` verb's lenses are where fine
 * distinctions live, and duplicating that table here would be a second
 * thing to keep in step with no test able to see the drift.
 *
 * `ground` is this pane's instance of `PaneCell`'s ground rule (see its doc
 * in `pane_cell.ts`), ported from `terrain_glyph` in
 * `windows/scene/src/surrounds_ascii.rs`: `false` for the observer's own
 * cell and for any water glyph, `true` for land — the caller uses this to
 * decide whether `cell.color` is a truthful claim to carry. */
function glyphFor(cell: ChartCell, waterLegend: string[]): { glyph: string; ground: boolean } {
  // Not ground: `@` names the observer, not the surface beneath them.
  if (cell.state === "here") return { glyph: YOU, ground: false };
  const water = waterLegend[cell.water];
  // Not ground: water covers the surface the colour would describe.
  if (water !== undefined && WATER_KINDS.has(water)) return { glyph: "~", ground: false };
  return { glyph: cell.state === "remembered" ? "," : ".", ground: true };
}

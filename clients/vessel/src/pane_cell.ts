// The shared cell shape both panes (`pane_chart.ts`, `pane_plan.ts`) return.
//
// A string cannot carry colour, and a per-cell colour is not the only
// attribute coming: `windows/vessel/src/plan.rs`'s own module doc names an
// occupant's `EntityId` and a temperature as later arrivals too. A parallel
// array per attribute would need to stay length-synced with the grid on
// every edit; one cell object per cell does not.

/** One cell of a pane's grid: the glyph to draw, and the colour to draw it
 * in, if any. `color` is `null` whenever the sim withheld it — no `color`
 * key on the wire, a malformed value, or (see each pane's own comment) a
 * glyph that is not honestly describable by the sim's bedrock reflectance. */
export interface PaneCell {
  /** The character this cell draws. */
  glyph: string;
  /** An 8-bit-per-channel RGB triple, or `null` for an uncoloured cell. */
  color: [number, number, number] | null;
}

/** A pane's whole grid, row-major — `grid[row][col]`. */
export type PaneGrid = PaneCell[][];

/** Validate and narrow a raw JSON value into an RGB triple, or `null` if it
 * is not one. Refuse, don't guess: only a 3-length array of integers each in
 * `0..=255` is accepted. A clamp (`Math.min(255, Math.max(0, v))`) would
 * silently repaint an out-of-range or malformed value into a plausible
 * colour instead of admitting the payload was never a colour at all — the
 * same refuse-don't-guess posture `pane_plan.ts`'s and `pane_chart.ts`'s own
 * `parseCell` already take on every other field. */
export function parseColor(raw: unknown): [number, number, number] | null {
  if (!Array.isArray(raw) || raw.length !== 3) return null;
  for (const channel of raw) {
    if (!Number.isInteger(channel) || channel < 0 || channel > 255) return null;
  }
  return [raw[0], raw[1], raw[2]];
}

/** Whether two colours (or absences of one) are the same run. `null` only
 * equals `null` — an uncoloured cell never merges with a coloured one, even
 * a coincidentally-black `[0, 0, 0]`. */
function sameColor(
  a: [number, number, number] | null,
  b: [number, number, number] | null,
): boolean {
  if (a === null || b === null) return a === b;
  return a[0] === b[0] && a[1] === b[1] && a[2] === b[2];
}

/** Coalesce a row of cells into runs of adjacent, identically-coloured
 * glyphs. A terminal-style renderer wraps one `<span>` (or one escape
 * sequence) per run rather than per cell, which is the whole reason a pane
 * returns cells instead of strings: colour needs a boundary somewhere, and
 * the boundary is a colour *change*, not every character. */
export function runsOf(
  row: PaneCell[],
): { text: string; color: [number, number, number] | null }[] {
  const runs: { text: string; color: [number, number, number] | null }[] = [];
  for (const cell of row) {
    const last = runs[runs.length - 1];
    if (last !== undefined && sameColor(last.color, cell.color)) {
      last.text += cell.glyph;
    } else {
      runs.push({ text: cell.glyph, color: cell.color });
    }
  }
  return runs;
}

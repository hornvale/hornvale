// The `vessel/plan/v1` reader: cells in, glyph rows out.
//
// Pure — no DOM, no worker globals — matching snapshot.ts and transcript.ts.
// The sim emits semantic cells and never a picture (decision 0022); choosing
// the glyph is this module's whole job, and it is what leaves a future
// restyle (CLIENT-atmosphere, CLIENT-alive-map) a pure client change.

import type { PlanPayload, Snapshot } from "./snapshot.ts";

/** The glyph for each cell kind. The mark `@` is deliberately the same one
 * the walk-band chart uses: the plan and the chart are one verb's two bands,
 * and a player who has learned to find `@` on one must not have to learn a
 * second mark for the other.
 *
 * `Object.create(null)` on purpose: a plain object literal inherits from
 * `Object.prototype`, so `GLYPH[entry.kind]` for `entry.kind === "constructor"`
 * would resolve up the prototype chain to `Object`'s constructor function
 * rather than falling through to `UNKNOWN` — splicing a function's source
 * text into a map row. A null-prototype lookup table has no chain to climb. */
const GLYPH: Record<string, string> = Object.assign(Object.create(null), {
  wall: "#",
  floor: ".",
  threshold: "+",
});

/** Drawn for a cell kind this client does not know. A newer sim may name a
 * kind that postdates this bundle; showing an honest unknown beats throwing
 * away the whole pane. */
const UNKNOWN = "?";

/** The mark for the cell the possession stands in. */
const YOU = "@";

/** The glyph rows for this snapshot's floor plan, or `null` when there is no
 * plan to draw — out of doors, on a sim that emits no spatial channel, or on
 * a payload that fails validation.
 *
 * Refusing beats drawing something wrong: a short or clamped map looks
 * plausible and is not visibly incorrect, which is the worse failure. */
export function planRows(snap: Snapshot): string[] | null {
  const spatial = snap.spatial;
  if (!spatial || spatial.band !== "chamber") return null;
  const plan: PlanPayload = spatial.plan;
  if (
    !plan?.extent || !Array.isArray(plan.cells) || !Array.isArray(plan.palette) ||
    !plan.you || !Number.isInteger(plan.you.x) || !Number.isInteger(plan.you.y)
  ) {
    return null;
  }
  const { w, h } = plan.extent;
  if (!Number.isInteger(w) || !Number.isInteger(h) || w <= 0 || h <= 0) return null;
  if (plan.cells.length !== w * h) return null;
  if (plan.cells.some((ix) => !Number.isInteger(ix) || ix < 0 || ix >= plan.palette.length)) {
    return null;
  }
  // A palette entry this refuses to trust: `null`, or anything that is not
  // an object. `entry.kind` in the draw loop below would throw on `null` —
  // the same unguarded-dereference class the `plan.you` fix already closed
  // once in this file — so refuse the whole plan the same way an
  // out-of-range index already is, rather than let the draw loop discover it.
  if (plan.palette.some((entry) => entry === null || typeof entry !== "object")) {
    return null;
  }

  const rows: string[] = [];
  for (let y = 0; y < h; y++) {
    let row = "";
    for (let x = 0; x < w; x++) {
      const entry = plan.palette[plan.cells[y * w + x]];
      // `GLYPH` has a null prototype (see its declaration), so an unknown
      // `kind` — including the string `"constructor"` — simply misses and
      // falls through to `UNKNOWN` rather than resolving up a prototype
      // chain that a plain object literal would have had.
      row += GLYPH[entry.kind] ?? UNKNOWN;
    }
    rows.push(row);
  }

  // The mark draws OVER the cell beneath it, and the session never leaves the
  // possession standing in a doorway — a hidden `+` would be a plan that lies
  // about the building. Coordinates are lattice-local, so subtract the
  // extent's origin before indexing.
  const mx = plan.you.x - plan.extent.x;
  const my = plan.you.y - plan.extent.y;
  if (mx >= 0 && mx < w && my >= 0 && my < h) {
    const row = rows[my];
    rows[my] = row.slice(0, mx) + YOU + row.slice(mx + 1);
  }
  return rows;
}

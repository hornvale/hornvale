// Task 7 (The Sighting): marks drawn onto the chamber pane. Split out of
// pane_plan_test.ts because that file predates `marks` entirely — keeping
// the mark-drawing coverage in its own file makes this campaign's diff to
// pane_plan_test.ts a zero, and a future reader looking for "how are marks
// tested" has exactly one file to open.

import { assertEquals } from "@std/assert";
import { parseSnapshot } from "./snapshot.ts";
import { planRows } from "./pane_plan.ts";

// The occupied fixture, not the plain one: the plain chamber fixture's
// script is `enter` alone at turn 1, before any tick populates occupancy,
// so its `marks` is `[]` — asserting mark behaviour against it would
// assert on an empty array and pass vacuously. `wait; enter` gives the
// occupied fixture a real mark at (3,1): the bugbear's noun, kind
// `"agent"`, salience 5.
const OCCUPIED = Deno.readTextFileSync(
  new URL(
    "../../../windows/vessel/tests/fixtures/snapshot-seed-42-chamber-occupied.json",
    import.meta.url,
  ),
);

Deno.test("a mark renders its glyph at its cell", () => {
  const snap = parseSnapshot(OCCUPIED)!;
  const rows = planRows(snap)!;
  // The fixture's one mark: noun "bugbear of Goodogododaga" at (3, 1),
  // lattice-local == pane-local since this plan's extent origin is (0, 0).
  assertEquals(rows[1][3], "b");
});

Deno.test("marks draw over the floor but never over `@`", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 3, h: 1 },
        palette: [{ kind: "wall", chambers: [] }, { kind: "floor", chambers: [0] }],
        cells: [0, 1, 0],
        you: { x: 1, y: 0 },
        // The sim never places a creature where the possession stands
        // (per the task brief), but the client still has to decide who
        // wins if it ever did — and it must be `@`, not the mark.
        marks: [{ x: 1, y: 0, noun: "goblin", kind: "agent", datum: "d", salience: 1 }],
      },
    },
  }))!;
  assertEquals(planRows(snap), ["#@#"]);
});

Deno.test("a mark outside the extent is ignored, not thrown on and not clamped", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 2, h: 1 },
        palette: [{ kind: "wall", chambers: [] }, { kind: "floor", chambers: [0] }],
        cells: [0, 1],
        you: { x: 0, y: 0 },
        marks: [{ x: 100, y: 100, noun: "goblin", kind: "agent", datum: "d", salience: 1 }],
      },
    },
  }))!;
  // Not thrown (the parse above didn't throw), and not clamped onto the
  // nearest in-bounds cell — the row is exactly as it would be with no
  // mark at all.
  assertEquals(planRows(snap), ["@."]);
});

Deno.test("an absent `marks` key renders the plan unchanged", () => {
  // `marks` is optional on the wire (a sim older than The Sighting emits
  // no such key), and the interface says so — this payload has no
  // `marks` field at all, not even an empty array.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 2, h: 1 },
        palette: [{ kind: "wall", chambers: [] }, { kind: "floor", chambers: [0] }],
        cells: [0, 1],
        you: { x: 1, y: 0 },
      },
    },
  }))!;
  assertEquals(planRows(snap), ["#@"]);
});

Deno.test("a malformed mark entry is refused, not thrown on — and its siblings still draw", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 3, h: 1 },
        palette: [{ kind: "wall", chambers: [] }, { kind: "floor", chambers: [0] }],
        cells: [0, 1, 1],
        you: { x: 0, y: 0 },
        marks: [
          null,
          { x: "not-a-number", y: 0, noun: "goblin", kind: "agent", datum: "d", salience: 1 },
          { x: 1, noun: "goblin", kind: "agent", datum: "d", salience: 1 }, // y missing
          { x: 2, y: 0, noun: "", kind: "agent", datum: "d", salience: 1 }, // empty noun
          "not an object",
          42,
          { x: 2, y: 0, noun: "kobold", kind: "agent", datum: "d", salience: 1 }, // the one good entry
        ],
      },
    },
  }))!;
  // A single malformed entry is refused, not thrown on — but unlike a bad
  // palette entry (referenced by index from every cell, so one bad entry
  // can corrupt the whole grid), a malformed mark only affects itself: the
  // rest of the plan, and every other mark, still draws.
  assertEquals(planRows(snap), ["@.k"]);
});

Deno.test("an extent missing x/y is refused, not thrown on, once a mark is present", () => {
  // Found during this task's sibling-dereference audit, not written from
  // the brief: `extent.x`/`extent.y` were never validated anywhere in this
  // file. The pre-existing `@`-placement check is `&&`-chained ("all four
  // bounds must hold"), which happens to degrade safely on `NaN` — every
  // comparison against `NaN` is false, so the whole thing is false and it
  // skips. The marks loop's bounds check is `||`-chained ("skip if ANY
  // bound is violated"), and on `NaN` no comparison is true, so nothing
  // skips and the draw proceeds into `rows[NaN]` (`undefined`), throwing —
  // the exact unguarded-dereference shape The Panes shipped twice.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { w: 2, h: 1 }, // x, y missing entirely
        palette: [{ kind: "wall", chambers: [] }, { kind: "floor", chambers: [0] }],
        cells: [0, 1],
        you: { x: 1, y: 0 },
        marks: [{ x: 5, y: 5, noun: "goblin", kind: "agent", datum: "d", salience: 1 }],
      },
    },
  }))!;
  assertEquals(planRows(snap), null);
});

Deno.test("two marks on the same cell: last in the array wins, deliberately", () => {
  // Marks arrive pre-sorted by (salience, noun) from the Rust side, so
  // array order is deterministic bytes, not discovery order. This client
  // never re-sorts; if two marks land on one cell it draws whichever is
  // later in the array, which is a deliberate, cheap choice rather than an
  // attempt to pick "the more salient" one by re-deriving Rust's own sort.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 1, h: 1 },
        palette: [{ kind: "floor", chambers: [0] }],
        cells: [0],
        you: { x: 9, y: 9 },
        marks: [
          { x: 0, y: 0, noun: "ant", kind: "agent", datum: "d", salience: 1 },
          { x: 0, y: 0, noun: "bee", kind: "agent", datum: "d", salience: 2 },
        ],
      },
    },
  }))!;
  assertEquals(planRows(snap), ["b"]);
});

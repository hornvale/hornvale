import { assertEquals } from "@std/assert";
import { parseSnapshot } from "./snapshot.ts";
import type { PlanPayload } from "./snapshot.ts";
import { planRows } from "./pane_plan.ts";

const CHAMBER = Deno.readTextFileSync(
  new URL(
    "../../../windows/vessel/tests/fixtures/snapshot-seed-42-chamber.json",
    import.meta.url,
  ),
);

Deno.test("a real chamber snapshot renders one row per lattice row", () => {
  const snap = parseSnapshot(CHAMBER)!;
  const rows = planRows(snap)!;
  const plan = (snap.spatial as { band: "chamber"; plan: PlanPayload }).plan;
  assertEquals(rows.length, plan.extent.h);
  for (const row of rows) assertEquals(row.length, plan.extent.w);
});

Deno.test("the standing cell is marked, and exactly once", () => {
  const snap = parseSnapshot(CHAMBER)!;
  const rows = planRows(snap)!;
  const marks = rows.join("").split("").filter((c: string) => c === "@").length;
  assertEquals(marks, 1);
});

Deno.test("a walk-band snapshot draws no plan", () => {
  const snap = parseSnapshot(
    JSON.stringify({
      schema: "vessel/session/v1",
      spatial: { band: "walk", chart: {} },
    }),
  )!;
  assertEquals(planRows(snap), null);
});

Deno.test("a snapshot with no spatial channel draws no plan", () => {
  const snap = parseSnapshot(JSON.stringify({ schema: "vessel/session/v1" }))!;
  assertEquals(planRows(snap), null);
});

Deno.test("a grid whose length disagrees with its extent is refused", () => {
  // Not drawn short, not clamped: refused. A pane that draws a wrong map is
  // worse than a pane that draws none, because only one of the two is
  // visibly wrong.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 3, h: 3 },
        palette: [{ kind: "wall", chambers: [] }],
        cells: [0, 0, 0],
        you: { x: 1, y: 1 },
      },
    },
  }))!;
  assertEquals(planRows(snap), null);
});

Deno.test("an index past the end of the palette is refused", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 2, h: 1 },
        palette: [{ kind: "wall", chambers: [] }],
        cells: [0, 9],
        you: { x: 0, y: 0 },
      },
    },
  }))!;
  assertEquals(planRows(snap), null);
});

Deno.test("an unknown extra field on a palette entry still renders", () => {
  // The property the palette shape exists for: colour, warmth or rubble can
  // ship later without touching a client that predates them.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 2, h: 1 },
        palette: [
          { kind: "wall", chambers: [], color: [1, 2, 3], warmth: 0.5 },
          { kind: "floor", chambers: [0] },
        ],
        cells: [0, 1],
        you: { x: 1, y: 0 },
      },
    },
  }))!;
  assertEquals(planRows(snap), ["#@"]);
});

Deno.test("an unknown cell kind renders as the fallback, not a throw", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 2, h: 1 },
        palette: [{ kind: "portcullis", chambers: [] }, { kind: "floor", chambers: [0] }],
        cells: [0, 1],
        you: { x: 9, y: 9 },
      },
    },
  }))!;
  assertEquals(planRows(snap), ["?."]);
});

Deno.test("a plan with no `you` at all is refused, not thrown", () => {
  // `you` is required by the Rust schema, so a payload lacking it is
  // malformed in exactly the same sense as a cells-length mismatch — refuse
  // it the same way rather than rendering an unmarked map.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 2, h: 1 },
        palette: [{ kind: "wall", chambers: [] }, { kind: "floor", chambers: [0] }],
        cells: [0, 1],
      },
    },
  }))!;
  assertEquals(planRows(snap), null);
});

Deno.test("a null palette entry is refused, not thrown on", () => {
  // A malformed entry that would throw `TypeError: Cannot read properties
  // of null (reading 'kind')` at draw time must be caught by the up-front
  // validation instead — the same "refuse the whole plan" posture the
  // length and index checks already have.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 2, h: 1 },
        palette: [{ kind: "wall", chambers: [] }, null],
        cells: [0, 1],
        you: { x: 0, y: 0 },
      },
    },
  }))!;
  assertEquals(planRows(snap), null);
});

Deno.test("a palette entry naming 'constructor' as its kind does not leak the prototype chain", () => {
  // A plain object literal `{wall: ..., floor: ..., threshold: ...}` used as
  // a lookup table inherits from `Object.prototype`, so `table["constructor"]`
  // resolves to `Object`'s constructor function rather than missing — a
  // client-controlled string reaching into the prototype chain and splicing
  // a function's source text into a rendered map row.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 1, h: 1 },
        palette: [{ kind: "constructor", chambers: [] }],
        cells: [0],
        you: { x: 9, y: 9 },
      },
    },
  }))!;
  assertEquals(planRows(snap), ["?"]);
});

Deno.test("a `you` with a non-integer coordinate is refused", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 2, h: 1 },
        palette: [{ kind: "wall", chambers: [] }, { kind: "floor", chambers: [0] }],
        cells: [0, 1],
        you: { x: 1.5, y: 0 },
      },
    },
  }))!;
  assertEquals(planRows(snap), null);
});

import { assertEquals } from "@std/assert";
import { narrationOf, parseSnapshot, type Sight, sightOf, waysOf } from "./snapshot.ts";

// A minimal fixture in the real schema's shape. Kept small on purpose: the
// full-fidelity byte pin is Rust's (windows/vessel/tests/fixtures).
const FIXTURE = JSON.stringify({
  schema: "vessel/session/v1",
  turn: 0,
  day: 0.5,
  self: { agent: "1", species: "bugbear", settlement: "X", population: 118, room: 7 },
  sensed: {
    room: {
      schema: "locale/room/v2",
      id: 7,
      exits: [
        { kind: "Edge", direction: { Compass: "Se" }, to: 8 },
        { kind: "Vertical", direction: "Exit", to: 9 },
      ],
    },
    sky: "Night.",
    present: [],
  },
  known: { entries: [] },
  social: [],
  narration: { prose: "You stand in a wood.\nWays on: SE.", nouns: [] },
});

Deno.test("parseSnapshot accepts a v1 payload", () => {
  const snap = parseSnapshot(FIXTURE);
  assertEquals(snap?.schema, "vessel/session/v1");
  assertEquals(snap?.turn, 0);
});

Deno.test("parseSnapshot rejects junk and a wrong schema rather than throwing", () => {
  assertEquals(parseSnapshot("not json"), null);
  assertEquals(parseSnapshot(""), null);
  assertEquals(parseSnapshot(JSON.stringify({ schema: "vessel/session/v2" })), null);
});

Deno.test("narrationOf returns the prose verbatim", () => {
  assertEquals(narrationOf(parseSnapshot(FIXTURE)!), "You stand in a wood.\nWays on: SE.");
});

Deno.test("waysOf filters compass edges, so no `ways` field is needed", () => {
  assertEquals(waysOf(parseSnapshot(FIXTURE)!), [{ dir: "Se", room: 8 }]);
});

// Fix round 1 (The Beholding, Task 8): sightOf had zero direct coverage —
// the caption test in main_render_test.ts hand-builds a `Sight` object and
// passes it straight to `renderInto`, so it never exercises the parser at
// all. A reviewer mutated `sightOf` to `return null;` unconditionally and
// every test stayed green. These three close that: a well-formed round
// trip, an absent block, and a partially malformed one — the same
// refuse-don't-guess posture `parseSnapshot`'s own sibling tests already
// hold `pane_plan.ts`/`pane_chart.ts` to.

/** A minimal `vessel/session/v1` payload on the walk band, with the given
 * raw `chart` object substituted in whole — `sightOf` reads only
 * `chart.sight`, so the rest of `SURROUNDS_SCHEMA`'s shape is irrelevant to
 * it and is omitted here on purpose (the real, full-fidelity shape is
 * `windows/vessel/tests/fixtures/snapshot-seed-42-walk.json`, which
 * `sightOf` is cross-checked against separately — see the task report). */
function walkSnapshot(chart: unknown): string {
  return JSON.stringify({
    schema: "vessel/session/v1",
    turn: 0,
    day: 0.5,
    self: { agent: "1", species: "bugbear", settlement: "X", population: 118, room: 7 },
    sensed: { room: { schema: "locale/room/v2", id: 7, exits: [] }, sky: "Night.", present: [] },
    known: { entries: [] },
    social: [],
    narration: { prose: "", nouns: [] },
    spatial: { band: "walk", chart },
  });
}

const WELL_FORMED_SIGHT: Sight = {
  observer: "bugbear",
  channels: 3,
  chromatic: 2,
  projection: "yellow-blue",
  preserves: "the short-to-long opposition; the red-green axis is not carried",
};

Deno.test("sightOf reads every field off a well-formed sight block", () => {
  const snap = parseSnapshot(
    walkSnapshot({ schema: "scene/surrounds/v2", sight: WELL_FORMED_SIGHT }),
  )!;
  assertEquals(sightOf(snap), WELL_FORMED_SIGHT);
});

Deno.test("sightOf is null when the chart carries no sight key at all", () => {
  // `eyes.rs`'s "decline the observer step" path (`windows/vessel/src/
  // eyes.rs`) omits the `sight` key entirely rather than emitting a null —
  // an uncoloured chart, or a sim predating The Beholding.
  const snap = parseSnapshot(walkSnapshot({ schema: "scene/surrounds/v2" }))!;
  assertEquals(sightOf(snap), null);
});

Deno.test("sightOf is null on a partially malformed block — never a partial object", () => {
  // Four of five fields are well-typed; only `channels` is wrong (a string,
  // not an integer). A looser reader might return the other four fields
  // anyway; `sightOf` refuses the whole block instead, the same
  // discipline `parseSnapshot` already holds every other field to.
  const malformed = { ...WELL_FORMED_SIGHT, channels: "three" };
  const snap = parseSnapshot(walkSnapshot({ schema: "scene/surrounds/v2", sight: malformed }))!;
  assertEquals(sightOf(snap), null);
});

Deno.test("sightOf is null on a chamber-band snapshot, even with a walk-shaped sight lying around", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    turn: 0,
    day: 0,
    self: { agent: "1", species: "bugbear", settlement: "X", population: 1, room: 0 },
    sensed: { room: { schema: "locale/room/v2", id: 0, exits: [] }, sky: "", present: [] },
    known: { entries: [] },
    social: [],
    narration: { prose: "", nouns: [] },
    spatial: {
      band: "chamber",
      plan: {
        schema: "vessel/plan/v1",
        extent: { x: 0, y: 0, w: 1, h: 1 },
        palette: [{ kind: "floor", chambers: [0] }],
        cells: [0],
        you: { x: 0, y: 0 },
      },
    },
  }))!;
  assertEquals(sightOf(snap), null);
});

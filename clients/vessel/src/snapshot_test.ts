import { assertEquals } from "@std/assert";
import { narrationOf, parseSnapshot, waysOf } from "./snapshot.ts";

// A minimal fixture in the real schema's shape. Kept small on purpose: the
// full-fidelity byte pin is Rust's (windows/vessel/tests/fixtures).
const FIXTURE = JSON.stringify({
  schema: "vessel/session/v1",
  turn: 0,
  day: 0.5,
  self: { agent: 1, species: "bugbear", settlement: "X", population: 118, room: 7 },
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

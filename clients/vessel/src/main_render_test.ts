// Task 8 (The Beholding): `renderInto` builds the map pane's coloured DOM
// from a `PaneGrid` and states whose eyes produced it. Deno's runtime carries
// no `document` of its own — every other `src/*_test.ts` file in this tree
// is a pure module with nothing to render, so this is the first test file
// here that needs one. `dom_shim.ts` registers a `linkedom` `document` onto
// `globalThis` as a side effect; it must be imported here BEFORE `./main.ts`
// — see that file's own header for why source order alone would not be
// enough. `main.ts`'s module-scope `document.getElementById("casement")`
// runs at import time, before any `Deno.test` body, so without the shim
// already in place it throws before a single test can run.
import "./dom_shim.ts";

import { assertEquals, assertStringIncludes } from "@std/assert";
import { renderInto } from "./main.ts";
import { parseSnapshot, sightOf } from "./snapshot.ts";

Deno.test("a sim-authored noun containing markup becomes text, never an element", () => {
  // pane_plan.ts draws mark.noun.charAt(0) — a sim-authored character. The
  // map pane used to set textContent, which was injection-safe by
  // construction; building DOM is what makes this a live question.
  //
  // Both cells share ONE colour (`null`, the ground rule every mark draws
  // under — see `PaneCell`'s doc in `pane_cell.ts`) so `runsOf` coalesces
  // them into a SINGLE run and thus a single `<span>`. This is load-bearing
  // for the guard, not incidental: the brief's original two-colour version
  // of this fixture ([1,2,3] then null) put "<" and "img" in two SEPARATE
  // spans, and `"<"` alone plus `"img"` alone each parse as inert text even
  // through `innerHTML` — no tag ever spans a run boundary, so that version
  // passed whether or not `renderInto` used `innerHTML`. Verified by
  // mutation (see the task report): a same-colour, one-span "<img>" is the
  // version that actually reddens against an `innerHTML` implementation.
  const host = document.createElement("pre");
  renderInto(host, [[{ glyph: "<img>", color: null }]], null);
  assertEquals(host.querySelectorAll("img").length, 0);
  assertEquals(host.textContent, "<img>");
});

Deno.test("like-coloured neighbours share one span", () => {
  const host = document.createElement("pre");
  const c: [number, number, number] = [1, 2, 3];
  renderInto(host, [[
    { glyph: "a", color: c },
    { glyph: "b", color: c },
    { glyph: "c", color: null },
  ]], null);
  // Two runs on one row, not three cells.
  assertEquals(host.querySelectorAll("span").length, 2);
});

Deno.test("the caption states whose eyes and what the projection drops", () => {
  // Fix round 1: driven through the REAL seam — `parseSnapshot` then
  // `sightOf` — not a hand-built `Sight` object handed straight to
  // `renderInto`. A reviewer found the original version of this test
  // exercised only `renderInto`'s own caption string, so a `sightOf` bug
  // (it was mutated to `return null;` unconditionally) left every test
  // green: the wire-to-caption path had a gap no single unit test covered.
  const json = JSON.stringify({
    schema: "vessel/session/v1",
    turn: 0,
    day: 0.5,
    self: { agent: "1", species: "bugbear", settlement: "X", population: 118, room: 7 },
    sensed: { room: { schema: "locale/room/v2", id: 7, exits: [] }, sky: "Night.", present: [] },
    known: { entries: [] },
    social: [],
    narration: { prose: "", nouns: [] },
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        sight: {
          observer: "bugbear",
          channels: 3,
          chromatic: 2,
          projection: "yellow-blue",
          preserves: "the short-to-long opposition; the red-green axis is not carried",
        },
      },
    },
  });
  const snap = parseSnapshot(json)!;
  const sight = sightOf(snap);

  const host = document.createElement("pre");
  renderInto(host, [[{ glyph: ".", color: null }]], sight);
  const text = host.textContent ?? "";
  // All FIVE rendered fields, not just two — a reviewer mutation dropped
  // `channels`, `chromatic`, and `preserves` from the caption string
  // entirely and only the two-assertion version of this test stayed green.
  // `preserves` is this campaign's whole honesty claim: the caption exists
  // to say what the projection KEEPS, not merely to name it.
  assertStringIncludes(text, "bugbear");
  assertStringIncludes(text, "3 channel");
  assertStringIncludes(text, "2 chromatic");
  assertStringIncludes(text, "yellow-blue");
  assertStringIncludes(text, "the short-to-long opposition; the red-green axis is not carried");
});

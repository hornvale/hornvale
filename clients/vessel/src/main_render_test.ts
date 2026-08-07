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
  const host = document.createElement("pre");
  renderInto(host, [[{ glyph: ".", color: null }]], {
    observer: "bugbear",
    channels: 3,
    chromatic: 2,
    projection: "yellow-blue",
    preserves: "the short-to-long opposition; the red-green axis is not carried",
  });
  assertStringIncludes(host.textContent ?? "", "bugbear");
  assertStringIncludes(host.textContent ?? "", "yellow-blue");
});

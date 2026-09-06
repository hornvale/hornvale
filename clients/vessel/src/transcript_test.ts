import { assertEquals } from "@std/assert";
import { splitResponse } from "./transcript.ts";

Deno.test("room header and ways lines are meta; prose is prose", () => {
  const text = "[room]\nYou stand in a test.\nWays on: N, SW.";
  assertEquals(splitResponse(text), [
    { cls: "casement-meta", text: "[room]" },
    { cls: "casement-prose", text: "You stand in a test." },
    { cls: "casement-meta", text: "Ways on: N, SW." },
  ]);
});

Deno.test("the outdoor nearest-ground sentence is meta, not prose (The Rhumb, F2)", () => {
  // Task 5 replaced the walk band's "Ways on: E, NW, SW." footer with this
  // sentence. It is exit-list content wearing different words, so it must
  // stay muted monospace like the line it replaced — not fall through to
  // the serif prose class, which is what happened before this fix.
  //
  // The Ken (spec §4.3) made the clause conditional: openness is the
  // default and a wall is news, so "No direction here is closed; the
  // nearest ground lies ..." (unconditional, every ordinary turn) is gone,
  // and this test's fixture moved to the surviving sibling that still
  // fires — a bearing actually refused.
  const text =
    "[room]\nYou stand in a test.\nEvery direction here is open but SE; the nearest ground lies E, NW, SW.";
  assertEquals(splitResponse(text), [
    { cls: "casement-meta", text: "[room]" },
    { cls: "casement-prose", text: "You stand in a test." },
    {
      cls: "casement-meta",
      text: "Every direction here is open but SE; the nearest ground lies E, NW, SW.",
    },
  ]);
});

Deno.test("the submerged 'Ways on: surface.' footer is meta", () => {
  // The submerged band's own footer (F1's fix) still starts with "Ways on:"
  // and was already covered by the existing prefix check; pinned here
  // explicitly so a future rewording of either footer is caught at both
  // sites, not just the outdoor one.
  const text = "[room]\nCoral reef.\nWays on: surface.";
  assertEquals(splitResponse(text), [
    { cls: "casement-meta", text: "[room]" },
    { cls: "casement-prose", text: "Coral reef." },
    { cls: "casement-meta", text: "Ways on: surface." },
  ]);
});

Deno.test("plain single-line responses are prose", () => {
  assertEquals(splitResponse("You let go."), [
    { cls: "casement-prose", text: "You let go." },
  ]);
});

Deno.test("empty lines are preserved as prose spacers", () => {
  assertEquals(splitResponse("a\n\nb"), [
    { cls: "casement-prose", text: "a" },
    { cls: "casement-prose", text: "" },
    { cls: "casement-prose", text: "b" },
  ]);
});

Deno.test("a chart's lines take the map class, not the prose one", () => {
  const lines = splitResponse(
    "[lens: terrain · depth 12 · radius 4 · north-up]\n  ..@..\n  ways on: E, Nw, Sw",
  );
  assertEquals(lines.every((l) => l.cls === "casement-map"), true);
});

Deno.test("map mode ends at the chart's own legend line, not only on a blank line", () => {
  // `render_surrounds_ascii` never emits a trailing blank line after a
  // chart — it closes on "  legend: ...". A response that appends prose
  // straight after (no blank separator) must still classify that prose
  // as prose, not map: this pins the EXIT, not just the entry, so a
  // regression that leaves map mode stuck on forever (every line after
  // the first chart misread as a grid) cannot pass silently.
  const lines = splitResponse(
    "[lens: terrain · depth 12 · radius 4 · north-up]\n" +
      "  ..@..\n" +
      "  ways on: E, Nw, Sw\n" +
      "  legend: a settlement, a biome\n" +
      "You notice the world keeps its shape.",
  );
  assertEquals(lines.map((l) => l.cls), [
    "casement-map",
    "casement-map",
    "casement-map",
    "casement-map",
    "casement-prose",
  ]);
});

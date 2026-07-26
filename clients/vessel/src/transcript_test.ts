import { assertEquals } from "@std/assert";
import { splitResponse } from "./transcript.ts";

Deno.test("room header and ways lines are meta; prose is prose", () => {
  const text = "[room 1, day 0]\nYou stand in a test.\nWays on: N, SW.";
  assertEquals(splitResponse(text), [
    { cls: "casement-meta", text: "[room 1, day 0]" },
    { cls: "casement-prose", text: "You stand in a test." },
    { cls: "casement-meta", text: "Ways on: N, SW." },
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
    "[lens: terrain · depth 12 · radius 4 · lattice-aligned, not north-up]\n  ..@..\n  ways on: E, Nw, Sw",
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
    "[lens: terrain · depth 12 · radius 4 · lattice-aligned, not north-up]\n" +
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

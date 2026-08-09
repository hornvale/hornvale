import { assertEquals } from "@std/assert";
import { parseColor, runsOf } from "./pane_cell.ts";

Deno.test("runsOf coalesces adjacent like-coloured cells", () => {
  const row = [
    { glyph: ".", color: [1, 2, 3] as [number, number, number] },
    { glyph: ",", color: [1, 2, 3] as [number, number, number] },
    { glyph: "~", color: null },
    { glyph: "@", color: [9, 9, 9] as [number, number, number] },
  ];
  assertEquals(runsOf(row), [
    { text: ".,", color: [1, 2, 3] },
    { text: "~", color: null },
    { text: "@", color: [9, 9, 9] },
  ]);
});

Deno.test("runsOf does not merge across a colour change", () => {
  const row = [
    { glyph: "a", color: [1, 1, 1] as [number, number, number] },
    { glyph: "b", color: [1, 1, 2] as [number, number, number] },
  ];
  assertEquals(runsOf(row).length, 2);
});

Deno.test("parseColor accepts only a 3-length array of integers in 0..=255", () => {
  assertEquals(parseColor([0, 128, 255]), [0, 128, 255]);
  for (
    const bad of [
      [1, 2],
      [1, 2, 3, 4],
      ["1", 2, 3],
      "red",
      7,
      [1, 2, 300],
      [1, 2, -1],
      [1.5, 2, 3],
      null,
      undefined,
    ]
  ) {
    assertEquals(parseColor(bad), null, `${JSON.stringify(bad)} must not survive`);
  }
});

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
    { text: ".,", color: [1, 2, 3], dim: false },
    { text: "~", color: null, dim: false },
    { text: "@", color: [9, 9, 9], dim: false },
  ]);
});

Deno.test("runsOf does not merge across a colour change", () => {
  const row = [
    { glyph: "a", color: [1, 1, 1] as [number, number, number] },
    { glyph: "b", color: [1, 1, 2] as [number, number, number] },
  ];
  assertEquals(runsOf(row).length, 2);
});

Deno.test("runsOf does not merge across a WEIGHT change either", () => {
  // A run carries one weight as it carries one colour, so a dim cell beside
  // a bright one of the same colour is two runs. Merging them would hand
  // the whole run one weight and silently repaint one of the two cells —
  // the same class of error the colour boundary exists to prevent, and the
  // one that would let a remembered cell render as sensed.
  const c: [number, number, number] = [1, 1, 1];
  assertEquals(
    runsOf([
      { glyph: "a", color: c },
      { glyph: "b", color: c, dim: true },
      { glyph: "c", color: c },
    ]),
    [
      { text: "a", color: c, dim: false },
      { text: "b", color: c, dim: true },
      { text: "c", color: c, dim: false },
    ],
  );
});

Deno.test("an absent `dim` and an explicit `dim: false` are the same run", () => {
  // `dim` is optional because `pane_plan.ts` has no epistemic channel on
  // the wire to carry. Absent must therefore mean exactly "not dim", never
  // a third state that splits a run against an explicit `false`.
  const c: [number, number, number] = [1, 1, 1];
  assertEquals(
    runsOf([{ glyph: "a", color: c }, { glyph: "b", color: c, dim: false }]).length,
    1,
  );
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

import { assert, assertEquals } from "@std/assert";
import { elevationColor, starTint } from "./palette.ts";

Deno.test("ocean is bluer than land at the same delta", () => {
  const sea = elevationColor(0, 400);
  const land = elevationColor(800, 400);
  assert(sea[2] > sea[0], "ocean is blue-dominant");
  assert(land[1] >= land[2], "land is not blue-dominant");
});

Deno.test("star tint is total and hot≠cool", () => {
  const g = starTint("yellow dwarf (G)");
  const m = starTint("red dwarf (M)");
  assertEquals(g.length, 3);
  assert(g[0] + g[1] > m[1] + m[2] || g[2] > m[2], "distinct by class");
});

Deno.test("F-type stars are warm, matching the sim's star_color (F groups with G)", () => {
  assertEquals(starTint("yellow-white dwarf (F)"), starTint("yellow dwarf (G)"));
});

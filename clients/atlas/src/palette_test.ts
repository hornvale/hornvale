import { assert, assertEquals } from "@std/assert";
import { layerPixels, LAYERS } from "./palette.ts";
import { parseScene } from "./scene.ts";

function scene(overrides: Record<string, unknown> = {}) {
  const tiles = 16 * 8;
  return parseScene(JSON.stringify({
    schema: "scene/tiles/v1",
    seed: 1,
    width: 16,
    height: 8,
    sea_level_m: 100.0,
    elevation_m: Array(tiles).fill(150.0),
    ocean: Array(tiles).fill(false),
    biome: Array.from({ length: tiles }, (_, i) => i % 22),
    biome_legend: Array.from({ length: 22 }, (_, i) => `b${i}`),
    plate: Array.from({ length: tiles }, (_, i) => i % 13),
    unrest: Array.from({ length: tiles }, (_, i) => (i % 9) / 8),
    features: [],
    ...overrides,
  }));
}

Deno.test("every layer paints every tile opaquely", () => {
  const s = scene();
  for (const layer of LAYERS) {
    const px = layerPixels(s, layer);
    assertEquals(px.length, 16 * 8 * 4);
    for (let i = 3; i < px.length; i += 4) assertEquals(px[i], 255);
  }
});

Deno.test("biome tile 0 wears the ice color", () => {
  const px = layerPixels(scene(), "biome");
  assertEquals([px[0], px[1], px[2]], [235, 235, 245]);
});

Deno.test("deep ocean is darker than shallow", () => {
  const tiles = 16 * 8;
  const elev = Array(tiles).fill(90.0); // 10 m deep
  elev[1] = -5900.0; // 6000 m deep
  const px = layerPixels(scene({ elevation_m: elev, ocean: Array(tiles).fill(true) }), "elevation");
  assert(px[4] < px[0] && px[6] < px[2], "deeper tile is darker");
});

Deno.test("plate colors follow slot order mod 8", () => {
  const px = layerPixels(scene(), "plate");
  // plate 0 → slot 1 (#2a78d6), plate 8 → slot 1 again
  assertEquals([px[0], px[1], px[2]], [0x2a, 0x78, 0xd6]);
  assertEquals([px[8 * 4], px[8 * 4 + 1], px[8 * 4 + 2]], [0x2a, 0x78, 0xd6]);
});

Deno.test("unrest buckets are monotonic in lightness", () => {
  const tiles = 16 * 8;
  const unrest = Array(tiles).fill(0);
  unrest[0] = 0.0;
  unrest[1] = 1.0;
  const px = layerPixels(scene({ unrest }), "unrest");
  const light = px[0] + px[1] + px[2];
  const dark = px[4] + px[5] + px[6];
  assert(dark < light, "unrest 1.0 must be darker than 0.0");
});

Deno.test("biome index beyond the palette renders neutral gray", () => {
  const tiles = 16 * 8;
  const biome = Array(tiles).fill(0);
  biome[0] = 22; // one past BIOME_RGB's 22 entries; a future legend-grown index
  const s = scene({
    biome,
    biome_legend: Array.from({ length: 23 }, (_, i) => `b${i}`),
  });
  const px = layerPixels(s, "biome");
  assertEquals([px[0], px[1], px[2]], [128, 128, 128]);
});

Deno.test("ocean layer is two-valued", () => {
  const tiles = 16 * 8;
  const ocean = Array(tiles).fill(false);
  ocean[0] = true;
  const px = layerPixels(scene({ ocean }), "ocean");
  assert(px[0] !== px[4] || px[1] !== px[5] || px[2] !== px[6]);
});

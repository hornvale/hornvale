import { assert, assertEquals, assertStringIncludes } from "@std/assert";
import { featureAt, readout } from "./hittest.ts";
import { initialViewport } from "./projection.ts";
import { parseScene } from "./scene.ts";

function scene() {
  const tiles = 16 * 8;
  return parseScene(JSON.stringify({
    schema: "scene/tiles/v1",
    seed: 1,
    width: 16,
    height: 8,
    sea_level_m: 100.0,
    elevation_m: Array(tiles).fill(250.5),
    ocean: Array(tiles).fill(false),
    biome: Array(tiles).fill(1),
    biome_legend: ["ice", "tundra", ...Array.from({ length: 20 }, (_, i) => `b${i}`)],
    plate: Array(tiles).fill(3),
    unrest: Array(tiles).fill(0.25),
    features: [{ name: "Homestead", kind: "flagship", latitude: 0.0, longitude: 0.0 }],
  }));
}

Deno.test("readout carries every field", () => {
  const line = readout(scene(), 2, 3);
  assertStringIncludes(line, "250.5 m");
  assertStringIncludes(line, "tundra");
  assertStringIncludes(line, "plate 3");
  assertStringIncludes(line, "unrest 0.25");
  assertStringIncludes(line, "land");
});

Deno.test("feature hover finds the flagship near its canvas point", () => {
  const s = scene();
  const v = initialViewport();
  // lat 0, lon 0 → canvas center at scale 1.
  const f = featureAt(s, v, 512, 256, 1024, 512);
  assert(f !== null);
  assertEquals(f.name, "Homestead");
  assertEquals(featureAt(s, v, 512, 300, 1024, 512), null);
});

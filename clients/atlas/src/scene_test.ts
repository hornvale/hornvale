import { assertEquals, assertThrows } from "@std/assert";
import { parseScene, SceneFormatError } from "./scene.ts";

function validScene(): Record<string, unknown> {
  const tiles = 16 * 8;
  return {
    schema: "scene/tiles/v1",
    seed: 1,
    width: 16,
    height: 8,
    sea_level_m: 100.0,
    elevation_m: Array(tiles).fill(50.0),
    ocean: Array(tiles).fill(true),
    biome: Array(tiles).fill(21),
    biome_legend: Array.from({ length: 22 }, (_, i) => `biome-${i}`),
    plate: Array(tiles).fill(0),
    unrest: Array(tiles).fill(0.5),
    features: [{ name: "Test", kind: "flagship", latitude: 0.0, longitude: 0.0 }],
  };
}

Deno.test("a valid document parses", () => {
  const scene = parseScene(JSON.stringify(validScene()));
  assertEquals(scene.width, 16);
  assertEquals(scene.elevation_m.length, 128);
  assertEquals(scene.features[0].kind, "flagship");
});

Deno.test("wrong schema string is rejected by name", () => {
  const doc = validScene();
  doc.schema = "scene/tiles/v2";
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "schema");
});

Deno.test("mismatched layer length is rejected by layer name", () => {
  const doc = validScene();
  (doc.elevation_m as number[]).push(1.0);
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "elevation_m");
});

Deno.test("out-of-range biome index is rejected", () => {
  const doc = validScene();
  (doc.biome as number[])[3] = 22;
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "biome");
});

Deno.test("height must be width over two", () => {
  const doc = validScene();
  doc.height = 9;
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "height");
});

Deno.test("malformed feature is rejected", () => {
  const doc = validScene();
  (doc.features as unknown[]).push({ name: 7, kind: "settlement" });
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "feature");
});

Deno.test("non-object documents are rejected as scene errors", () => {
  for (const text of ["null", "42", "[]", '"scene"']) {
    assertThrows(() => parseScene(text), SceneFormatError, "object");
  }
});

Deno.test("fractional dimensions are rejected", () => {
  const doc = validScene();
  doc.width = 16.5;
  doc.height = 8.25;
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError);
});

Deno.test("the committed gallery scene parses", async () => {
  const text = await Deno.readTextFile(
    new URL("../../../book/src/gallery/scene-tiles-seed-42.json", import.meta.url),
  );
  const scene = parseScene(text);
  assertEquals(scene.schema, "scene/tiles/v1");
  assertEquals(scene.width, 256);
  assertEquals(scene.biome_legend.length, 22);
});

Deno.test("the crate's golden fixture parses", async () => {
  const text = await Deno.readTextFile(
    new URL("../../../windows/scene/tests/fixtures/tiles-seed-1-w16.json", import.meta.url),
  );
  assertEquals(parseScene(text).width, 16);
});

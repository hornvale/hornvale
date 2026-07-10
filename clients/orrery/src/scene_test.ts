import { assertEquals, assertThrows } from "@std/assert";
import { parseSystem, parseTiles, SceneFormatError } from "./scene.ts";

const DOC = JSON.stringify({
  schema: "scene/system/v1",
  seed: 42,
  star: { class_name: "yellow dwarf (G)", luminosity_rel: 1, hz_inner_au: 0.9, hz_outer_au: 1.4 },
  world: {
    orbit_au: 1,
    year_days: 372,
    day_length_days: 1,
    obliquity_deg: 23,
    year_phase_offset: 0.1,
  },
  moons: [{ sidereal_days: 16, phase_offset: 0.4, distance_mm: 384, size_rel: 1 }],
});

Deno.test("parseSystem reads a valid document", () => {
  const s = parseSystem(DOC);
  assertEquals(s.seed, 42);
  assertEquals(s.moons.length, 1);
  assertEquals(s.world.dayLengthDays, 1);
});

Deno.test("parseSystem rejects the wrong schema", () => {
  assertThrows(() => parseSystem(JSON.stringify({ schema: "scene/tiles/v1" })), SceneFormatError);
});

Deno.test("parseSystem maps snake_case fields to camelCase", () => {
  const s = parseSystem(DOC);
  assertEquals(s.star.className, "yellow dwarf (G)");
  assertEquals(s.star.luminosityRel, 1);
  assertEquals(s.star.hzInnerAu, 0.9);
  assertEquals(s.star.hzOuterAu, 1.4);
  assertEquals(s.world.orbitAu, 1);
  assertEquals(s.world.yearDays, 372);
  assertEquals(s.world.obliquityDeg, 23);
  assertEquals(s.world.yearPhaseOffset, 0.1);
  assertEquals(s.moons[0].siderealDays, 16);
  assertEquals(s.moons[0].phaseOffset, 0.4);
  assertEquals(s.moons[0].distanceMm, 384);
  assertEquals(s.moons[0].sizeRel, 1);
});

Deno.test("parseSystem maps an absent day_length_days to null", () => {
  const doc = JSON.parse(DOC);
  delete doc.world.day_length_days;
  const s = parseSystem(JSON.stringify(doc));
  assertEquals(s.world.dayLengthDays, null);
});

Deno.test("parseSystem rejects a non-object document", () => {
  for (const text of ["null", "42", "[]", '"scene"']) {
    assertThrows(() => parseSystem(text), SceneFormatError, "object");
  }
});

Deno.test("parseSystem rejects moons that are not an array", () => {
  const doc = JSON.parse(DOC);
  doc.moons = "not an array";
  assertThrows(() => parseSystem(JSON.stringify(doc)), SceneFormatError, "moons");
});

Deno.test("parseSystem rejects a missing required field", () => {
  const doc = JSON.parse(DOC);
  delete doc.world.orbit_au;
  assertThrows(() => parseSystem(JSON.stringify(doc)), SceneFormatError, "orbit_au");
});

Deno.test("the committed gallery scene/system/v1 document parses", async () => {
  const text = await Deno.readTextFile(
    new URL("../../../book/src/gallery/scene-system-seed-42.json", import.meta.url),
  );
  const s = parseSystem(text);
  assertEquals(s.schema, "scene/system/v1");
  assertEquals(s.seed, 42);
  assertEquals(typeof s.world.dayLengthDays, "number");
  assertEquals(s.moons.length, 2);
});

function validTiles(): Record<string, unknown> {
  const tiles = 16 * 8;
  return {
    schema: "scene/tiles/v1",
    width: 16,
    height: 8,
    sea_level_m: 100.0,
    elevation_m: Array(tiles).fill(50.0),
  };
}

Deno.test("parseTiles reads a valid document", () => {
  const t = parseTiles(JSON.stringify(validTiles()));
  assertEquals(t.width, 16);
  assertEquals(t.height, 8);
  assertEquals(t.elevation_m.length, 128);
});

Deno.test("parseTiles rejects the wrong schema", () => {
  const doc = validTiles();
  doc.schema = "scene/system/v1";
  assertThrows(() => parseTiles(JSON.stringify(doc)), SceneFormatError, "schema");
});

Deno.test("parseTiles rejects height not equal to width / 2", () => {
  const doc = validTiles();
  doc.height = 9;
  assertThrows(() => parseTiles(JSON.stringify(doc)), SceneFormatError, "height");
});

Deno.test("parseTiles rejects a mismatched elevation_m length", () => {
  const doc = validTiles();
  (doc.elevation_m as number[]).push(1.0);
  assertThrows(() => parseTiles(JSON.stringify(doc)), SceneFormatError, "elevation_m");
});

Deno.test("the committed gallery scene/tiles/v1 document parses", async () => {
  const text = await Deno.readTextFile(
    new URL("../../../book/src/gallery/scene-tiles-seed-42.json", import.meta.url),
  );
  const t = parseTiles(text);
  assertEquals(t.schema, "scene/tiles/v1");
  assertEquals(t.width, 256);
  assertEquals(t.elevation_m.length, t.width * t.height);
});

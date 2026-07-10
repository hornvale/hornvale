import { assertAlmostEquals, assertEquals } from "@std/assert";
import { parseSystem } from "./scene.ts";
import { moonPhase, rotationPhase, synodicDays, worldPhase } from "./ephemeris.ts";

const sys = parseSystem(
  await Deno.readTextFile("../../book/src/gallery/scene-system-seed-42.json"),
);
const golden = JSON.parse(
  await Deno.readTextFile("./testdata/ephemeris-seed-42.json"),
) as { samples: { t: number; world_phase: number; rotation_phase: number; moons: number[] }[] };

Deno.test("ephemeris reproduces the Rust golden phases", () => {
  for (const row of golden.samples) {
    assertAlmostEquals(worldPhase(sys, row.t), row.world_phase, 1e-9);
    assertAlmostEquals(rotationPhase(sys, row.t), row.rotation_phase, 1e-9);
    row.moons.forEach((p, i) => assertAlmostEquals(moonPhase(sys, i, row.t), p, 1e-9));
  }
});

Deno.test("synodicDays computes P·Y/(Y−P) and is null when the moon never laps the sun", () => {
  const fastMoon = { siderealDays: 10, phaseOffset: 0, distanceMm: 1, sizeRel: 1 };
  const slowMoon = { siderealDays: 400, phaseOffset: 0, distanceMm: 1, sizeRel: 1 };
  const equalMoon = { siderealDays: 100, phaseOffset: 0, distanceMm: 1, sizeRel: 1 };
  const withMoons = (moons: typeof fastMoon[]) => ({
    ...sys,
    world: { ...sys.world, yearDays: 100 },
    moons,
  });

  assertAlmostEquals(synodicDays(withMoons([fastMoon]), 0)!, (10 * 100) / (100 - 10), 1e-9);
  assertEquals(synodicDays(withMoons([slowMoon]), 0), null);
  assertEquals(synodicDays(withMoons([equalMoon]), 0), null);
});

Deno.test("moonPhase is 0 when the moon's synodic period is null", () => {
  const slowMoon = { siderealDays: 400, phaseOffset: 0.3, distanceMm: 1, sizeRel: 1 };
  const s = { ...sys, world: { ...sys.world, yearDays: 100 }, moons: [slowMoon] };
  assertEquals(moonPhase(s, 0, 42), 0);
});

Deno.test("rotationPhase is 0 for a tidally locked world (day_length_days null)", () => {
  const locked = { ...sys, world: { ...sys.world, dayLengthDays: null } };
  assertEquals(rotationPhase(locked, 12345.6789), 0);
});

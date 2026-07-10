import { assert, assertAlmostEquals } from "@std/assert";
import { isLit, sample } from "./globe.ts";

Deno.test("disc center maps to lon = rotation, lat ~ 0 (no tilt)", () => {
  const s = sample(0, 0, 1, 0.25, 0)!;
  assertAlmostEquals(s.lat, 0, 1e-9);
  assertAlmostEquals(((s.lon % 360) + 360) % 360, 90, 1e-6); // 0.25 turn = 90°
});

Deno.test("outside the disc is null", () => {
  assert(sample(0.99, 0.99, 1, 0, 0) === null);
});

Deno.test("terminator: the point toward the sun is lit, opposite dark", () => {
  // sun to the right (angle 0): the right limb pixel is lit, the left dark.
  assert(isLit(0.6, 0, 0) === true);
  assert(isLit(-0.6, 0, 0) === false);
});

Deno.test("top of disc (north limb, no tilt) is near the north pole", () => {
  // y = -1 (screen up) maps to y' = -(-1) = 1 after negation in lat formula,
  // so lat = asin(-y') = asin(-1) = -90? Check against the model directly:
  // with no tilt, y' = y, lat = asin(-y). Top of screen is y = -1 → lat = 90.
  const s = sample(0, -1, 1, 0, 0)!;
  assertAlmostEquals(s.lat, 90, 1e-6);
});

Deno.test("tilt rotates the pole in the screen plane", () => {
  // With a 90° tilt, the point straight up on screen (formerly the pole)
  // rotates into the equatorial plane.
  const s = sample(0, -1, 1, 0, 90)!;
  assertAlmostEquals(s.lat, 0, 1e-6);
});

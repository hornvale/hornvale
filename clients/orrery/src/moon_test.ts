import { assert, assertAlmostEquals } from "@std/assert";
import { illuminatedFraction, litOffset } from "./moon.ts";

Deno.test("fraction: new 0, full 1, quarter 0.5", () => {
  assertAlmostEquals(illuminatedFraction(0), 0, 1e-9);
  assertAlmostEquals(illuminatedFraction(0.5), 1, 1e-9);
  assertAlmostEquals(illuminatedFraction(0.25), 0.5, 1e-9);
});

Deno.test("terminator offset: half at quarter, full disc at new/full", () => {
  // litOffset returns the terminator ellipse x-radius as a fraction of r:
  // |1-2k|. Quarter (k=.5) → 0 (straight terminator); new/full → 1.
  assertAlmostEquals(litOffset(0.25), 0, 1e-9);
  assertAlmostEquals(litOffset(0.5), 1, 1e-9);
  assert(litOffset(0.125) > 0 && litOffset(0.125) < 1);
});

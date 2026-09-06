import { assert, assertAlmostEquals, assertEquals } from "@std/assert";
import { heroLine, hintFor, recentShare, spinYears, STAGES, thousands } from "./stages.ts";
import { flatCurve, growingCurve } from "./fixtures.ts";

Deno.test("the stages are the spec's four, behind the hero", () => {
  assertEquals([...STAGES], ["hero", "when", "where", "life", "story"]);
});

Deno.test("the hero line reads souls_ever off the payload", () => {
  assertEquals(
    heroLine(growingCurve(), "42"),
    "226,972 lives have been lived in seed 42. Choose one.",
  );
});

Deno.test("thousands groups the way the narrator does", () => {
  assertEquals(thousands(0), "0");
  assertEquals(thousands(999), "999");
  assertEquals(thousands(1000), "1,000");
  assertEquals(thousands(226972.15), "226,972");
});

Deno.test("a growing world's hint quotes the share it actually has", () => {
  const curve = growingCurve();
  const share = recentShare(curve);
  assert(share >= 0.33, `expected a growing curve, got ${share}`);
  assertEquals(
    hintFor(curve),
    `Most of these lives were born recently: the last 500 years hold ` +
      `${(share * 100).toFixed(1)}% of them.`,
  );
});

Deno.test("a flat world takes the other sentence", () => {
  const curve = flatCurve();
  assertAlmostEquals(recentShare(curve), 0.25, 0.02);
  assertEquals(
    hintFor(curve),
    "This world's population stopped growing: a birth is about as likely in any century.",
  );
});

Deno.test("a world with no births at all reports no share rather than dividing by zero", () => {
  const curve = growingCurve();
  curve.births_by_epoch = curve.births_by_epoch.map(() => 0);
  assertEquals(recentShare(curve), 0);
  assert(hintFor(curve).startsWith("This world's population stopped growing"));
});

Deno.test("the hint's window is a quarter of THIS world's span, not a fixed 500 years", () => {
  const curve = growingCurve();
  curve.present_year = 4000;
  curve.epoch_years = 50;
  assert(hintFor(curve).includes("the last 1,000 years"));
});

Deno.test("the spin always ends on the drawn year and shows only real ones", () => {
  const curve = growingCurve();
  const spin = spinYears(curve, 1751.6373, 24);
  assertEquals(spin.length, 24);
  assertEquals(spin[spin.length - 1], 1751.6373);
  for (const year of spin.slice(0, -1)) {
    assert(year > curve.start_year && year < curve.present_year, `${year} is off the span`);
  }
  // The spin wanders rather than sweeping one way.
  assert(spin.slice(0, -1).some((y, at) => at > 0 && y < spin[at - 1]));
});

Deno.test("a zero-length spin is just the settled year", () => {
  assertEquals(spinYears(growingCurve(), 1751, 1), [1751]);
  assertEquals(spinYears(growingCurve(), 1751, 0), [1751]);
});

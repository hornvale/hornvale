import { assert, assertAlmostEquals, assertEquals } from "@std/assert";
import { birthsToPath, xToYear, yearToX } from "./graph.ts";
import { growingCurve } from "./fixtures.ts";

const W = 900;
const H = 220;

Deno.test("x is monotone in year on the log axis", () => {
  const curve = growingCurve();
  const graph = birthsToPath(curve, "log", W, H);
  for (let at = 1; at < graph.points.length; at += 1) {
    assert(
      graph.points[at].x > graph.points[at - 1].x,
      `log x fell at bin ${at}: ${graph.points[at - 1].x} -> ${graph.points[at].x}`,
    );
  }
});

Deno.test("x is monotone in year on the linear axis", () => {
  const curve = growingCurve();
  const graph = birthsToPath(curve, "linear", W, H);
  for (let at = 1; at < graph.points.length; at += 1) {
    assert(
      graph.points[at].x > graph.points[at - 1].x,
      `linear x fell at bin ${at}: ${graph.points[at - 1].x} -> ${graph.points[at].x}`,
    );
  }
});

Deno.test("both axes span the full plot width", () => {
  const curve = growingCurve();
  assertAlmostEquals(yearToX(curve, "linear", curve.start_year, W), 0, 1e-9);
  assertAlmostEquals(yearToX(curve, "linear", curve.present_year, W), W, 1e-9);
  assertAlmostEquals(yearToX(curve, "log", curve.start_year, W), 0, 1e-9);
  assertAlmostEquals(yearToX(curve, "log", curve.present_year, W), W, 1e-9);
});

Deno.test("the log axis spreads the recent past that the linear axis crushes", () => {
  // The whole argument for the log axis (spec §2.1): on a linear axis all
  // of a growing world's history is a spike at the right edge, because
  // almost every birth is recent. The last quarter of the span takes a
  // quarter of a linear plot and most of a log one.
  const curve = growingCurve();
  const quarterOpens = curve.present_year - (curve.present_year - curve.start_year) / 4;
  const linearWidth = W - yearToX(curve, "linear", quarterOpens, W);
  const logWidth = W - yearToX(curve, "log", quarterOpens, W);
  assertAlmostEquals(linearWidth, W / 4, 1e-9);
  assert(logWidth > W * 0.75, `the log axis gave the recent quarter only ${logWidth / W}`);
  assert(logWidth > 3 * linearWidth);
});

Deno.test("xToYear inverts yearToX on both axes", () => {
  const curve = growingCurve();
  for (const mode of ["log", "linear"] as const) {
    for (const year of [0, 1, 250, 1000, 1751.6373, 1999, 2000]) {
      const back = xToYear(curve, mode, yearToX(curve, mode, year, W), W);
      assertAlmostEquals(back, year, 1e-6, `${mode} round trip at ${year}`);
    }
  }
});

Deno.test("the path plots exactly the payload's counts, unmodified", () => {
  const curve = growingCurve();
  const graph = birthsToPath(curve, "log", W, H);
  assertEquals(graph.points.length, curve.births_by_epoch.length);
  assertEquals(graph.points.map((p) => p.births), curve.births_by_epoch);
  assertEquals(graph.peak, Math.max(...curve.births_by_epoch));
  // The peak bin sits on the top of the plot; the smallest sits below it.
  const peakPoint = graph.points[graph.points.length - 1];
  assertEquals(peakPoint.y, 0);
  assert(graph.points[0].y > peakPoint.y);
});

Deno.test("the d and area strings are well-formed and share the line", () => {
  const graph = birthsToPath(growingCurve(), "linear", W, H);
  assert(graph.d.startsWith("M"));
  assert(graph.area.startsWith(`M${graph.points[0].x} ${H}`));
  assert(graph.area.endsWith("Z"));
  assert(graph.area.includes(graph.d.slice(1)));
});

Deno.test("a flat y scale does not divide by zero", () => {
  const curve = growingCurve();
  curve.births_by_epoch = curve.births_by_epoch.map(() => 0);
  const graph = birthsToPath(curve, "log", W, H);
  assertEquals(graph.peak, 0);
  assert(graph.points.every((p) => p.y === H));
});

Deno.test("log ticks are powers of ten before the present, plus now", () => {
  const graph = birthsToPath(growingCurve(), "log", W, H);
  assertEquals(graph.ticks[0].label, "now");
  assertEquals(graph.ticks[0].x, W);
  assertEquals(graph.ticks.map((t) => t.label), [
    "now",
    "1 ya",
    "10 ya",
    "100 ya",
    "1000 ya",
  ]);
  for (let at = 1; at < graph.ticks.length; at += 1) {
    assert(graph.ticks[at].x < graph.ticks[at - 1].x);
  }
});

Deno.test("linear ticks divide the span evenly", () => {
  const graph = birthsToPath(growingCurve(), "linear", W, H);
  assertEquals(graph.ticks.map((t) => t.label), ["0", "500", "1000", "1500", "2000"]);
});

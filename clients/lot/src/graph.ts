// The When stage's births graph: the curve payload's `births_by_epoch`
// laid out on one of two axes, as an SVG path and a set of ticks.
//
// **The log axis is the point** (spec §2.1): on a linear axis all of a
// growing world's history is a spike at the right edge. Log runs on
// YEARS BEFORE PRESENT — `present_year - year` — so the present sits at the
// right edge at zero, the RECENT past gets most of the width, and the deep
// past compresses into the left. That is the inverse of the linear axis's
// failure and the whole reason both are offered: on a 2,000-year span the
// last quarter takes 25% of a linear plot and about 82% of a log one.
// `log10(ybp + 1)` rather than `log10(ybp)` because the present itself is
// zero years before present and `log10(0)` is not a coordinate.
//
// Nothing here computes a demographic quantity. Every count plotted is
// `births_by_epoch[i]`, straight from `lot/curve/v1`; this module decides
// only where on a rectangle to put it.

import type { Curve } from "./payload.ts";

/** Which axis the reader chose. */
export type Axis = "log" | "linear";

/** One plotted epoch: the payload's own count, and where it lands. */
export interface GraphPoint {
  /** The epoch's mid-year, the x this point is placed by. */
  year: number;
  /** `births_by_epoch[i]`, unmodified. */
  births: number;
  x: number;
  y: number;
}

/** One axis label and where it sits. */
export interface Tick {
  x: number;
  label: string;
}

/** A laid-out births graph. */
export interface Graph {
  points: GraphPoint[];
  /** The line through the points, as an SVG `d`. */
  d: string;
  /** The same line closed down to the baseline, for a fill. */
  area: string;
  ticks: Tick[];
  /** The largest count in `births_by_epoch` — the y scale's top. */
  peak: number;
}

/** How many decimal places a coordinate keeps in a path string. Enough for
 * a 1024-wide viewport, short enough that the emitted `d` stays readable. */
const PLACES = 2;

function round(value: number): number {
  const scale = 10 ** PLACES;
  return Math.round(value * scale) / scale;
}

/** Years before `present_year`, floored at zero: a year past the present
 * (which a pinned draw cannot produce, but a stale payload could) sits at
 * the right edge rather than off the axis. */
function beforePresent(curve: Curve, year: number): number {
  return Math.max(0, curve.present_year - year);
}

/** The left edge's distance from the present, in log units. */
function logSpan(curve: Curve): number {
  return Math.log10(beforePresent(curve, curve.start_year) + 1);
}

/** Where `year` sits across a `w`-wide plot. Monotone increasing in `year`
 * on both axes: the earliest year is at 0 and the present at `w`. */
export function yearToX(curve: Curve, mode: Axis, year: number, w: number): number {
  if (mode === "linear") {
    const span = curve.present_year - curve.start_year;
    return w * ((year - curve.start_year) / span);
  }
  const span = logSpan(curve);
  if (span <= 0) return w;
  return w * (1 - Math.log10(beforePresent(curve, year) + 1) / span);
}

/** The inverse of [`yearToX`] — what year a click at `x` names. Exact
 * within floating point, which is what makes the "select a year" pick mode
 * land on the year the reader pointed at. */
export function xToYear(curve: Curve, mode: Axis, x: number, w: number): number {
  const t = w === 0 ? 0 : x / w;
  if (mode === "linear") {
    return curve.start_year + t * (curve.present_year - curve.start_year);
  }
  const span = logSpan(curve);
  if (span <= 0) return curve.present_year;
  return curve.present_year - (10 ** ((1 - t) * span) - 1);
}

/** The ticks for an axis: powers of ten before the present on the log axis,
 * round divisions of the span on the linear one. */
function ticksFor(curve: Curve, mode: Axis, w: number): Tick[] {
  const ticks: Tick[] = [];
  if (mode === "log") {
    const oldest = beforePresent(curve, curve.start_year);
    ticks.push({ x: round(yearToX(curve, "log", curve.present_year, w)), label: "now" });
    for (let power = 0;; power += 1) {
      const ybp = 10 ** power;
      if (ybp > oldest) break;
      ticks.push({
        x: round(yearToX(curve, "log", curve.present_year - ybp, w)),
        label: `${ybp} ya`,
      });
    }
    return ticks;
  }
  const divisions = 4;
  for (let at = 0; at <= divisions; at += 1) {
    const year = curve.start_year + (at / divisions) * (curve.present_year - curve.start_year);
    ticks.push({ x: round(yearToX(curve, "linear", year, w)), label: `${Math.round(year)}` });
  }
  return ticks;
}

/** Lay the curve's births out on a `w` x `h` plot.
 *
 * The y scale is linear on both axes and always runs from zero to the
 * curve's own peak, so a bar's height is a share of the busiest epoch and
 * two axis choices show the same counts differently placed, never
 * differently scaled. */
export function birthsToPath(curve: Curve, mode: Axis, w: number, h: number): Graph {
  const peak = curve.births_by_epoch.reduce((most, n) => Math.max(most, n), 0);
  const points: GraphPoint[] = curve.births_by_epoch.map((births, at) => {
    const year = curve.start_year + (at + 0.5) * curve.epoch_years;
    return {
      year,
      births,
      x: round(yearToX(curve, mode, year, w)),
      y: round(peak > 0 ? h - (births / peak) * h : h),
    };
  });
  const d = points
    .map((p, at) => `${at === 0 ? "M" : "L"}${p.x} ${p.y}`)
    .join(" ");
  const first = points[0];
  const last = points[points.length - 1];
  const area = points.length === 0
    ? ""
    : `M${first.x} ${round(h)} L${d.slice(1)} L${last.x} ${round(h)} Z`;
  return { points, d, area, ticks: ticksFor(curve, mode, w), peak };
}

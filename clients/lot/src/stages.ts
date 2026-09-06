// The four stages, the hero line, and the When stage's hint — the pure
// half of the exhibit's script (spec §2.1, §6.4).
//
// **THE HINT IS COMPUTED FROM THIS WORLD'S CURVE, NEVER COPIED FROM THE
// SITE THE CAMPAIGN IS MODELLED ON.** Its threshold and its two sentences
// are the native narrator's (`windows/lot/src/narrate.rs::curve_text`), and
// the share it quotes is summed from the same `births_by_epoch` the graph
// above it draws — so the sentence and the picture cannot disagree. This is
// the one arithmetic this client does over a payload, and it is a SUM of
// payload fields, not a demographic model.

import type { Curve } from "./payload.ts";

/** The stages, in reveal order. */
export const STAGES = ["hero", "when", "where", "life", "story"] as const;

/** One stage's name. */
export type Stage = typeof STAGES[number];

/** The share of births in the span's last quarter at or above which this
 * world's curve is called "recent" — the narrator's `RECENT_SHARE`, held at
 * the same value so the page and the committed prose agree about the same
 * world. */
const RECENT_SHARE = 0.33;

/** The share of the span the closing sentence calls "recently". */
const RECENT_FRACTION = 0.25;

/** `value` with thousands separators, as the narrator prints it. */
export function thousands(value: number): string {
  return Math.round(value).toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

/** The hero line: how many lives this world has held, and the invitation.
 *
 * `souls_ever` is a payload field, rounded for reading — this does not
 * integrate anything. */
export function heroLine(curve: Curve, seed: string): string {
  return `${thousands(curve.souls_ever)} lives have been lived in seed ${seed}. Choose one.`;
}

/** The share of all births falling in the span's last quarter. Summed from
 * `births_by_epoch`, binned by each epoch's own opening year. */
export function recentShare(curve: Curve): number {
  const total = curve.births_by_epoch.reduce((sum, n) => sum + n, 0);
  if (total <= 0) return 0;
  const span = Math.max(0, curve.present_year - curve.start_year);
  const opens = curve.present_year - span * RECENT_FRACTION;
  const recent = curve.births_by_epoch.reduce(
    (sum, n, at) => (curve.start_year + at * curve.epoch_years >= opens ? sum + n : sum),
    0,
  );
  return recent / total;
}

/** The When stage's hint, in this world's own terms. */
export function hintFor(curve: Curve): string {
  const share = recentShare(curve);
  if (share >= RECENT_SHARE) {
    const years = Math.round((curve.present_year - curve.start_year) * RECENT_FRACTION);
    return `Most of these lives were born recently: the last ${thousands(years)} years hold ` +
      `${(share * 100).toFixed(1)}% of them.`;
  }
  return "This world's population stopped growing: a birth is about as likely in any century.";
}

/** The years the spin-and-settle reveal flickers through before landing on
 * the drawn one.
 *
 * They are epoch mid-years off the curve itself, sampled evenly across the
 * span, so the spin shows years this world actually has — theatre over the
 * payload, not over invented numbers. The last entry is always `settleOn`,
 * so a spin of any length ends on the drawn year. */
export function spinYears(curve: Curve, settleOn: number, frames: number): number[] {
  const bins = curve.births_by_epoch.length;
  if (frames <= 1 || bins === 0) return [settleOn];
  const out: number[] = [];
  for (let at = 0; at < frames - 1; at += 1) {
    // A stride coprime with most bin counts, so the spin wanders rather
    // than sweeping monotonically from one end to the other.
    const bin = (at * 7 + 3) % bins;
    out.push(curve.start_year + (bin + 0.5) * curve.epoch_years);
  }
  out.push(settleOn);
  return out;
}

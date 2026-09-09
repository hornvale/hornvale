// Cause rows for the Life stage, formatted from `lot/odds/v1` only.

import type { Odds } from "./payload.ts";

/** Cause labels and percentages in the sim's own deterministic order. */
export function causeRows(odds: Odds): [string, string][] {
  return odds.causes.map((row) => [row.cause, `${(row.share * 100).toFixed(1)}%`]);
}

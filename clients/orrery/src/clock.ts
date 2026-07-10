/** Playback clock: maps real elapsed time to sim-day time, decoupled from `main.ts`'s DOM glue. */

/** Map elapsed real ms and a speed (days per second) to sim-day time. */
export function clockToDay(elapsedMs: number, daysPerSecond: number): number {
  return (elapsedMs / 1000) * daysPerSecond;
}

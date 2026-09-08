// The Life stage's timeline: the drawn life's own events, in order, from
// `lot/life/v1` and nothing else.
//
// **WHY `matured` IS UNDATED, AND WHY THAT IS NOT A GAP TO PAPER OVER.**
// The life payload carries `matured: bool` and no maturity age; the age
// itself lives in `lot/odds/v1`, while this pure module deliberately receives
// only the life payload. So it emits maturity as an event with `age: null`
// and the timeline draws it beside the axis rather than on it. Placing it at
// a guessed age — 15, say, because that is what the calibration band uses —
// would be a number the page invented, which is the one thing spec §2.1
// forbids. An absent tick is the honest rendering.
//
// Every other event's age is arithmetic over two payload fields
// (`moved_year - birth_year`, `age_at_death`), never a model.

import type { Life } from "./payload.ts";

/** One event on the life's timeline. */
export interface LifeEvent {
  /** A short identifier for styling and testing. */
  kind: "birth" | "maturity" | "move" | "ending";
  /** Age in years when the payload dates the event, `null` when it does
   * not. A null-aged event carries no position on the axis. */
  age: number | null;
  /** The world year, where the age is known. */
  year: number | null;
  label: string;
}

/** How the ending reads, from `ending.kind` and its committed cause. */
function endingLabel(life: Life): string {
  const age = Math.round(life.age_at_death);
  if (life.ending.kind === "alive") {
    return `Still alive at ${age}, when the record ends.`;
  }
  if (life.ending.kind === "community-fate" && life.ending.cause !== null) {
    return `Died at ${age}, when the community was ${life.ending.cause}.`;
  }
  if (life.ending.kind === "community-fate") {
    return `Died at ${age}, with the community.`;
  }
  if (life.ending.kind === "outbreak" && life.ending.cause !== null) {
    return `Died at ${age} in an outbreak of ${life.ending.cause}.`;
  }
  return life.matured ? `Died at ${age}.` : `Died at ${age}, before maturity.`;
}

/** The life's events: the dated ones in ascending age, then the undated
 * ones. Sorting is STABLE within equal ages, so a move in the same year as
 * the ending still reads move-then-ending. */
export function eventsOf(life: Life): LifeEvent[] {
  const dated: LifeEvent[] = [
    {
      kind: "birth",
      age: 0,
      year: life.birth_year,
      label: `Born in year ${Math.round(life.birth_year)}.`,
    },
  ];
  if (life.moved_year !== null) {
    dated.push({
      kind: "move",
      age: life.moved_year - life.birth_year,
      year: life.moved_year,
      label: `Moved on at ${Math.round(life.moved_year - life.birth_year)}.`,
    });
  }
  dated.push({
    kind: "ending",
    age: life.age_at_death,
    year: life.death_year,
    label: endingLabel(life),
  });
  dated.sort((a, b) => (a.age as number) - (b.age as number));

  const undated: LifeEvent[] = life.matured
    ? [{
      kind: "maturity",
      age: null,
      year: null,
      label: "They lived to maturity — the record does not say at what age.",
    }]
    : [];
  return [...dated, ...undated];
}

/** Where an event sits along a `w`-wide axis running birth to ending.
 * Undated events have no position and return null. */
export function eventX(event: LifeEvent, life: Life, w: number): number | null {
  if (event.age === null) return null;
  const span = life.age_at_death;
  if (span <= 0) return 0;
  return w * Math.min(1, Math.max(0, event.age / span));
}

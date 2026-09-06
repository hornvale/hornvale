// The Story stage: the life's slots as tiles, the one note the by-design
// silences fold into, and the disclaimer line that opens the stage.
//
// **A tile is absent, never invented** (spec §2.1). A slot the ledger had
// no answer for renders as an absent tile carrying the sim's own reason for
// the silence; it never renders as a blank, a dash, or a plausible value.
// Nothing in this module can invent one — the only strings it produces are
// the fixed labels below and text the payload already holds.

import type { Life, Slot } from "./payload.ts";

/** One rendered tile. */
export interface Tile {
  key: string;
  /** The reader-facing label for the slot. */
  label: string;
  /** The sim's answer, or `null` for an absent tile. */
  value: string | null;
  /** Why the tile is absent — the sim's own sentence — or `null`. */
  reason: string | null;
  /** The `[n]` numbers this answer rests on. */
  sources: number[];
}

/** The label each slot key wears on the page. Presentation only: the label
 * names the QUESTION, and the payload supplies every answer. A key with no
 * entry here falls through to the key itself, so a slot added to
 * `windows/lot` shows up as an unlabelled tile rather than vanishing. */
const LABELS: Record<string, string> = {
  "when": "When",
  "where": "Where",
  "people": "People",
  "name": "Name",
  "community-size": "Community",
  "founded-from": "Founding",
  "founder-kinship": "Founder",
  "community-fate": "The community",
  "tech": "Technology",
  "function": "What it was for",
  "tongue": "Tongue",
  "belief": "Belief",
  "held-true": "Held true",
  "subsistence": "Subsistence",
  "standing": "Standing",
  "tribute": "Tribute",
  "dwelling": "Dwelling",
  "mine": "Workings",
  "climate": "Country",
  "sky": "Sky",
  "ground": "Ground",
  "diet": "Diet",
  "sex": "Sex",
  "family": "Family",
  "work": "Work",
  "literacy": "Letters",
};

/** The label for a slot key. */
export function labelOf(key: string): string {
  return LABELS[key] ?? key;
}

/** Whether a slot is silent because no world here models the thing at all
 * (spec §4.4), rather than because this world's ledger was quiet. */
export function isByDesign(slot: Slot): boolean {
  return slot.silence !== null && slot.silence.kind === "by-design";
}

/** The tiles, in the payload's own slot order, with the by-design silences
 * left out — those become one note instead, because four separate "nothing
 * in the record says" tiles read as four failures of this world's record
 * when they are one statement about what no world here models. */
export function tiles(life: Life): Tile[] {
  return life.slots
    .filter((slot) => !isByDesign(slot))
    .map((slot) => ({
      key: slot.key,
      label: labelOf(slot.key),
      value: slot.value,
      reason: slot.silence === null ? null : slot.silence.reason,
      sources: slot.sources,
    }));
}

/** The one note the by-design silences fold into, or `null` when the
 * payload carries none.
 *
 * Built from the by-design slots the payload actually holds, in their own
 * order, so it stays true if `windows/lot` adds or retires one. */
export function byDesignNote(life: Life): string | null {
  const absent = life.slots.filter(isByDesign).map((slot) => labelOf(slot.key).toLowerCase());
  if (absent.length === 0) return null;
  const named = absent.length === 1
    ? absent[0]
    : `${absent.slice(0, -1).join(", ")} or ${absent[absent.length - 1]}`;
  return `No world here models ${named}. Those silences are the world's, not this telling's.`;
}

/** One slot's value, when it is filled. */
function filled(life: Life, key: string): string | null {
  const slot = life.slots.find((s) => s.key === key);
  return slot?.value ?? null;
}

/** The line that opens the Story stage — the same sentence the native
 * narrator prints (`windows/lot/src/narrate.rs::disclaimer`), composed from
 * the same two slots and the same two numbers, so the exhibit and the
 * committed page say the same thing about the same life. */
export function disclaimer(life: Life): string {
  const who = filled(life, "name") ?? "This";
  const place = filled(life, "where") ?? "a place the record does not name";
  return `${who} is not a real person, but this life is drawn from the statistical reality of ` +
    `${place} in year ${Math.round(life.birth_year)} of seed ${life.seed}.`;
}

/** One entry in the Sources list, resolved from the payload's own row. */
export function sourceLine(source: {
  kind: string;
  entity: string | null;
  predicate: string | null;
  caption: string | null;
  function: string | null;
  inputs: string | null;
}): string {
  if (source.kind === "fact") {
    return `${source.caption ?? "a committed fact"} (entity ${source.entity}, ${source.predicate})`;
  }
  return `derived: ${source.function} (${source.inputs})`;
}

/** The headline the silence tally reads as: how much of this life the
 * record could answer. The numbers are the payload's own (decision 0798);
 * this only says them in a sentence. */
export function silenceLine(life: Life): string {
  const { filled: told, no_fact: quiet, by_design: designed } = life.silences;
  const askable = told + quiet;
  return `${told} of ${askable} answerable slots are filled; ${quiet} went unanswered because ` +
    `this world's ledger is quiet on them, and ${designed} more are not modelled at all.`;
}

import { assert, assertEquals } from "@std/assert";
import { byDesignNote, disclaimer, labelOf, silenceLine, sourceLine, tiles } from "./story.ts";
import { life } from "./fixtures.ts";

Deno.test("a silent slot renders as an absent tile carrying the sim's reason", () => {
  const quiet = tiles(life()).find((tile) => tile.key === "subsistence");
  assert(quiet !== undefined);
  assertEquals(quiet.value, null);
  assertEquals(quiet.reason, "nothing in the record says how they fed themselves");
  assertEquals(quiet.label, "Subsistence");
});

Deno.test("a filled tile carries the payload's own value and its sources", () => {
  const named = tiles(life()).find((tile) => tile.key === "name");
  assert(named !== undefined);
  assertEquals(named.value, "Xaararo");
  assertEquals(named.reason, null);
  assertEquals(named.sources, [4]);
});

Deno.test("the by-design silences leave the tiles and become exactly one note", () => {
  const subject = life();
  const rendered = tiles(subject);
  assertEquals(rendered.map((tile) => tile.key), ["when", "where", "name", "cause", "subsistence"]);
  const note = byDesignNote(subject);
  assertEquals(
    note,
    "No world here models sex, family, work or letters. Those silences are the world's, " +
      "not this telling's.",
  );
  // One note, not four: the four by-design slots produce a single string.
  assertEquals(subject.slots.filter((s) => s.silence?.kind === "by-design").length, 4);
});

Deno.test("a payload with no by-design silence gets no note at all", () => {
  const subject = life();
  subject.slots = subject.slots.filter((slot) => slot.silence?.kind !== "by-design");
  assertEquals(byDesignNote(subject), null);
});

Deno.test("the note names a single by-design silence without a stray 'or'", () => {
  const subject = life();
  subject.slots = subject.slots.filter((slot) =>
    slot.key !== "family" && slot.key !== "work" && slot.key !== "literacy"
  );
  assertEquals(
    byDesignNote(subject),
    "No world here models sex. Those silences are the world's, not this telling's.",
  );
});

Deno.test("the disclaimer is the narrator's sentence, from the payload's own slots", () => {
  assertEquals(
    disclaimer(life()),
    "Xaararo is a non-causal composite case, not a real person; it is drawn from the " +
      "statistical reality of Raaxora, a temperate-forest site, at 16.0°, 122.6° in year " +
      "1752 of seed 42, and cannot write consequences back to that world.",
  );
});

Deno.test("an unnamed life keeps the disclaimer honest rather than inventing a name", () => {
  const anonymous = life();
  anonymous.slots = anonymous.slots.map((slot) =>
    slot.key === "name"
      ? { ...slot, value: null, silence: { kind: "no-fact", reason: "no name survives" } }
      : slot
  );
  assert(disclaimer(anonymous).startsWith("This is a non-causal composite case,"));
});

Deno.test("a life with no where-slot names no place rather than guessing", () => {
  const nowhere = life();
  nowhere.slots = nowhere.slots.filter((slot) => slot.key !== "where");
  assert(disclaimer(nowhere).includes("a place the record does not name"));
});

Deno.test("a source resolves to its caption or its derivation", () => {
  const [fact, derived] = life().sources;
  assertEquals(
    sourceLine(fact),
    "the standard day the occupation began (entity 10760661430244475331, occ-founded)",
  );
  assertEquals(
    sourceLine(derived),
    "derived: lot::draw::draw (the world's births-per-year curve at the lot's index)",
  );
});

Deno.test("the silence line reads the payload's tally, not a recount", () => {
  assertEquals(
    silenceLine(life()),
    "4 of 5 answerable slots are filled; 1 went unanswered because this world's ledger is " +
      "quiet on them, and 4 more are not modelled at all.",
  );
});

Deno.test("an unlabelled slot key shows up rather than vanishing", () => {
  assertEquals(labelOf("a-slot-added-later"), "a-slot-added-later");
});

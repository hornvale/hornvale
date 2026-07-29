# 0084. An epoch is declared only when a derivation moved — `room/furnishing` stays at v1

**Status:** Accepted (2026-07-28) · **Decider:** Nathan · **Refines:**
[0073](0073-epoch-granularity-is-declared.md)

In the context of a campaign whose approved package *led* with "byte-identity
breaks and the health battery becomes the gate — the first time in this
program", facing a measurement that found no committed derivation had moved, we
decided that **`room/furnishing` stays at v1 and no epoch is declared** —
accepting that the discontinuity is thereby **deferred rather than absent**, and
that the campaign's own inventory now holds five patterns whose eventual epoch is
owed to a later campaign.

**Context.** The Blocking added chamber *roles*, so a structure's chambers stop
composing identical prose, and appended five anchor kinds with one pattern each
(`the-strongbox`, `the-high-seat`, `the-loom`, `the-anvil`, `the-altar`). The
pattern inventory's own doc comment stated flatly that adding or reordering a
pattern **is** an epoch, and the spec's causal chain agreed: new patterns → a
different interior → different warmth → a different committed drive history.

The plan was written to *measure* that chain rather than assert it, and the
measurement came back **RE-PIN**. Exactly one committed file moved — the seed-42
possession transcript. Nothing under `book/src/laboratory/` or
`book/src/reference/` moved, so no metric golden and no census golden moved, and
`make gate` came back green at 2413/2413 **as a check** rather than as a gate.

Three verified facts explain why, and each was found by reading the source rather
than by reasoning about it:

1. **`ROOM_FURNISHING` had exactly one occurrence in the workspace — its own
   declaration.** Nothing drew from it. A bump, by itself, re-mints nothing.
2. **The band a creature stands in is the locale, not the chamber.** NPC thermal
   drives read `interior_of`; the chamber composer is read only by the chamber
   renderer, which commits nothing.
3. **`selection` iterates the inventory in order and filters**, so *appending*
   role-gated patterns leaves every existing `(built, cold)` selection
   byte-identical. Order within the inventory is load-bearing; append position is
   not.

**Why declining is the right answer and not merely the cheap one.** A bump with
no moved derivation is an **empty epoch**: it declares a discontinuity that did
not occur, and it charges a permanent manifest row for the declaration. Worlds
written afterwards would claim a break in their own derivation history that never
happened, and the first *genuine* bump would then diff against a baseline that
records a fiction. The label's job is to say when the bytes stopped being
reproducible; saying so when they did not is the same defect as failing to say so
when they did, pointed the other way.

**Why this record exists at all, given that nothing changed.** Because something
did: the campaign's approved risk package was **reversed**. Flagged item 1 was
the largest thing the owner approved, and it turned out not to be true. A decision
log that records only the changes made, never the ones correctly declined, teaches
a future campaign that an epoch is what happens when you add patterns — which is
precisely the over-strict reading this campaign had to repair. The health battery
was a check here. It should be recorded that it was allowed to stay one.

**The vocabulary that made the answer nameable.** The spec had blurred four
distinct things into "the epoch", and separating them is what let the measurement
have three possible answers instead of one assumption:

- **RE-PIN** — transcripts move, no metric golden moves. Not an epoch.
- **EPOCH** — a metric or census golden moves. The health battery becomes the
  *gate*; galleries re-pin in an isolated commit; census regeneration needs the
  owner's authorization.
- **EMPTY** — a bump on a label with no draw site. What `room/furnishing/v2`
  would have been here.
- **LATENT** — an inventory grown behind closed gates, and
- **UNDECLARED** — a derivation that moved with no bump. The one unforgivable
  outcome.

**Consequences — the deferral, stated so it is not lost.** The LATENT condition
holds here *simultaneously and separately*: all five appended patterns carry
`at_locale: false`, so no live read reaches them. The discontinuity is deferred,
not avoided, and its gate condition is now written down rather than folklore:
**the first mark committed inside a chamber** — any fact whose `place` is a
chamber address, where the session's committed facts carry `place: None` today.
On that day a chamber's composition becomes a committed-history input and all
five patterns become an epoch *retroactively*, costing `room/furnishing/v1 → v2`
plus whatever the health battery and the censuses then read off chamber
compositions.

The mandatory response to a LATENT outcome was therefore discharged, not waived:
the inventory's and `ROOM_FURNISHING`'s doc comments no longer state flatly that
appending a pattern is an epoch — an over-strict warning is one that gets
ignored, and an ignored warning is exactly how an **undeclared** epoch ships.

`room/chambers/v1` was not touched either, and that is a separate window rather
than a second declination: nothing commits at chamber granularity today, so
bumping it is *free right now* and will not be once one mark exists inside a
chamber. The rule is written while it costs nothing.

**See also.** [0073](0073-epoch-granularity-is-declared.md) (declared
granularity, and its "pin invariants, not values" obligation, which is why a
green battery is readable at all);
[0083](0083-a-label-per-algorithm-and-never-in-advance.md) (the labels this
campaign *did* declare);
[0069](0069-fine-position-is-never-serialized.md) (why a cell step commits
nothing, so intra-chamber movement could not have forced a bump);
[The Blocking spec](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-07-28-the-blocking-design.md)
§5.1–§5.2 and §12; [The Blocking chronicle](../../book/src/chronicle/the-blocking.md).

Ratified at *The Blocking*'s merge gate.

# 0558. Sleep is never gated — the place grades it

**Status:** Accepted (2026-09-01) · **Decider:** Nathan (ruling) ·
**Relates:** [0347](0347-an-affordance-is-a-relation-not-a-property.md),
[0069](0069-fine-position-is-never-serialized.md),
[0398](0398-a-capability-nothing-can-reach-is-not-a-capability.md) ·
**Ledger:** `docs/superpowers/ledgers/2026-09-01-the-wicket.md` #6, #7, #41,
#43, #46 · **Spec:** The Wicket §6a

In the context of finding that three doc comments described `Sleep` as gated on
`SupportsRest` while `Session::sleep` has never consulted a bed, a home or any
other precondition, Nathan ruled that **sleep is never gated; the place a body
sleeps GRADES the recovery it gets**. A creature must be able to pass out in
the middle of the road, and prefer a bed, a fur or bracken when it can get one.

This binds every future campaign that touches rest, the sleep spell, or the
object-property vocabulary.

## Context

The false comments were not inert. An earlier draft of The Wicket's own spec
reasoned from them: it justified a `kneeler` as the campaign's proof kind on
the strength of `OfferedVerb::Sleep`'s doc ("gates on `SupportsRest`") and
asserted that "a shrine offers nowhere to rest", which is false. Reading the
code instead of the comments settled it — `Session::sleep` refuses a non-empty
argument, charges the clock, commits the bout and sets `wake_at`, and there is
no bed check and no home check anywhere on the path. The creature layer already
agreed and said so in its own doc: a body *"sleeps where it is."*

`SupportsRest` reaches only the **advertisement** layer:
`required_properties(Sleep)` decides which objects list `sleep` in a room's
offer, never whether sleeping is allowed. So the property was misfiled among
gates, and the whole property vocabulary was a vocabulary of gates with one
grade hidden in it.

## What was decided

- **The act is ungated, permanently.** A regression test pins that `sleep`
  succeeds in a room with no rest-affording object in it. It is the tripwire
  for the specific future mistake this record exists to prevent: someone tidying
  the inconsistency by making the verb honour its own old comment.
- **The two routes into sleep must never share a gate.** The voluntary act and
  an imposed effect (a sleep spell) differ in exactly one respect — only one of
  them is chosen — so a precondition on the act would either block the spell or
  be bypassed by it.
- **The place grades the bout.** Fatigue repayment is scaled by what the room
  offered the body that lay down in it, `1.0` on bare ground, so a body that
  never reaches furniture folds exactly the ungraded arithmetic.
- **The grade reads the offer, not the observer.** `room_affords_rest` asks
  `affordance::offered_to`, never `anchor.kind == kinds::BED`, so a future
  `SupportsRest` carrier needs only its `object_registry` row. It deliberately
  does **not** pass through `offered_to_observer`: the knowledge gate governs
  what a body is *told*, not what happens to it, and a body that sleeps on a bed
  it does not recognise as a bed still sleeps on a bed. The body-relative half
  of the offer *is* physical and is kept — `body_can_use` is a mass-ratio
  ceiling, decision 0347's point that a supporter to a sprite is not one to a
  giant (ledger #43).
- **The grade is read at the BOUT, not at the query.** The site is derived from
  the ledger's own `agent-at` timeline. A grade read at the query instant would
  be non-monotonic: a body that slept on a bed and then walked into the road
  would have the bed's repayment retroactively withdrawn.
- **A species with no sleep-debt row gets a documented neutral default, not
  sleeplessness.** Absence means *nobody authored it*, and a coverage ratchet
  makes that visible; an authored `0.0` is what "this species does not sleep"
  looks like. The inverse convention was tried and rejected on three grounds,
  the sharpest being that it fired inside the task that proposed it: a test
  body's `species: "test"` silently became rate 0.0 and broke a walk test, with
  nothing reporting a bad species (ledger #41).

## Consequences

- **The grade is locale-granular by construction, and this is a limit rather
  than an oversight.** In a built, cold locale the grade fires everywhere in
  that locale, so **a player passing out in the street is repaid exactly as one
  who found the bed**. Grading per anchor would require the fold to know which
  anchor a body occupied, and decision 0069 says fine position is never
  serialized — the ledger carries the room, not the spot in it. Making the grade
  anchor-granular is therefore a decision about 0069, not a refinement of this
  one (ledger #46).
- **The feared inversion cannot occur.** `the-fireside-bed` requires `built &&
  cold` at both bands, so there is no world where a chamber has a bed and its
  locale does not.
- **Reachability is measured, not assumed, and the citation is a committed
  census column rather than a throwaway probe.** `cold-built-room-share` has
  measured the population this grade can fire in at n=1000 since The Range
  (1000 present, 0 absent; median near 0.19, p25 near 0.07, mean near 0.26 —
  read the live figures from `book/src/domesday/settlement.md`, which a census
  refresh moves and this record does not). A grade confined to cold, built
  rooms is a grade most rooms do not carry, and that is the intended shape. An
  earlier draft cited a five-seed probe written and deleted inside the task
  that made it; a standing measurement at n=1000 outlives the task and a
  throwaway does not.
- **Two of the three halves of the ruling are deliberately unbuilt, and the
  space for them is free.** *What a people tends to sleep on* is a
  `(species KindId, thing KindId)` edge; *this one just likes a sleeping bag* is
  a `Lineage`-derived per-instance value. After The Wicket a kind is a `KindId`
  with open component tables behind it, so either is a new table and nothing
  else. Neither is a placeholder: both are named consumers waiting on additions
  the object model already owes.
- **`SupportsRest` stays where it is rather than being deleted.** The
  corrections name the grade/gate split as the reason the property is misfiled,
  which is what a later campaign needs in order to move it.

## See also

`windows/vessel/src/liveness.rs` (`room_affords_rest`, `rest_timeline`,
`fatigue_from_rests`, `SiteGrade`); `windows/vessel/src/session.rs`
(`Session::sleep`); `windows/lab/src/metrics.rs` (`cold-built-room-share`).

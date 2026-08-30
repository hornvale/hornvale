# 0397. The knowledge gate denies a passage — 0369's obstacle was the key, and the key is what changed

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot; the third
remedy ruled at the §3 spec stop) · **Answers:**
[0369](0369-a-gate-is-reached-by-addressing-not-by-durability.md) (which
recorded the criterion unmet and named two remedies) · **Relates:**
[0349](0349-the-offer-passes-through-the-observers-knowledge.md) (the gate
itself), [0348](0348-object-properties-are-keyed-by-kind.md),
[0396](0396-a-passage-is-a-thing-and-openness-is-its-fold.md) (the cave-mouth
thing this needs to exist),
[0353](0353-a-regression-test-is-specified-by-the-mutation-it-must-fail.md)
(how the denial is evidenced) ·
[The Chattel](../superpowers/plans/2026-08-28-the-chattel.md)

In the context of The Latch's acceptance criterion 5 standing struck through
and unmet, we decided that **`offered_to_observer` is re-keyed from
`AnchorKind` to `KindId` and the `thing_kind_of` conversion moves out to its
two anchor-side call sites**, accepting that this lifts the *addressing* half
of 0369's obstacle and leaves the *live-reachability* half exactly where it
was.

## Context

0369 is correct and stays in force. Its rule — **a gate's reachability is a
property of what it is keyed on, not of how durable the state behind it is** —
is the finding, and this record does not touch it. What was short was its
enumeration:

> "Reaching the gate needs one of two owner decisions … invent a new
> `AnchorKind` plus a design for what a cave mouth *offers*, or give cave
> chambers their own anchor/interior system."

There is a third, and it is the one this campaign took: **do not add to the
enum, stop asking the question in the enum's currency.** A cave mouth is a
thing (0396), things are keyed by `KindId`, and the property table, `offered`
and `offered_to` had already been re-keyed by this campaign's Task 7. Only
the gate itself was still converting an `AnchorKind` on the way in, which
kept the gate's currency at the enum even though nothing downstream of it
needed that.

## The rule

**1. The conversion belongs at the call site, and moving it is the whole
mechanism.** `offered_to_observer(kind: KindId, body, known)` now takes a
thing-kind; `Session::warm` and `Session::examine_chamber` — the only two
production callers, both holding an `AnchorKind` off a chamber's own anchor
catalogue — apply `thing_kind_of` themselves, the way
`affordance::encloses`'s caller already did since Task 7. While the
conversion lived *inside* the function, the only reachable arguments were the
fourteen `AnchorKind` variants, and `thing_kind_of` is injective over them
with no arm landing on `cave-mouth`. **So the gate could not be asked about a
passage — not "was not asked", could not be.** That is 0369's obstacle stated
mechanically, and it is why the remedy is a signature and not a feature.

**2. The Chattel's acceptance criterion 4 is MET on The Latch's stated
REASON, and NOT met on 0349's vocabulary. This clause names which, because
its first draft asserted both.** It read "MET, in the sense The Latch's
criterion 5 meant it", which is a claim about what a prior campaign intended,
and the ancestry cuts the other way: criterion 5's literal text is "denies
something — a **firing case**, shown by a test that fails if the gate is
removed", and *firing case* is 0349's own term for live reachability ("IV.b's
durable objects give it a firing case with no rewiring"; "No seed, no session
and no transcript in IV.a exercises the denying branch"). Read through that
vocabulary, criterion 5 asks for precisely the half clause 3 concedes is
unmet.

**What this record relies on is The Latch's written reason, not 0349's
word.** The Latch spent its whole justification on *currency* — "The reason is
a type mismatch, not a missing wire": `offered_to_observer` took an
`AnchorKind`, `AnchorKind` has no cave-mouth variant, a cave mouth is a
`Vertex`/`ChamberAddr` — and both remedies it named are keys, not states. That
wall is gone, and this campaign is what removed it.

**The increment is also narrower than "a test that fails if the gate is
removed", which was already true before this campaign.** 0349's own
consequence list says so: "making `offered_to_observer` ignore its `known`
argument reddens `an_unencountered_object_offers_nothing`" — and it still
does, on the same unfiltered run that reds the new test. The thing that is
new is that **a kind with no `AnchorKind` behind it can be named to the gate
at all**. `an_unencountered_passage_offers_nothing`
(`windows/vessel/tests/suite/affordance.rs`) asks the gate about
`KindId("cave-mouth")` with an empty `Knowledge` and gets the empty set; the
gate mutation reds it, alongside two tests that predate it (red pasted
unfiltered in the test's own doc, per decision 0353). Its first assertion is
the precondition that keeps it honest: no `AnchorKind` maps to `cave-mouth`,
so if a cave-mouth anchor variant ever arrives, the test stops being evidence
about addressing and someone has to say so. **That precondition did not fire
against its own named scenario until fix round 1** — the roster it swept was
hand-written, so an added variant was invisible to it. `AnchorKind::ALL` is
now generated from the enum's declaration (`interior/anchor.rs`), and the
probe that was silent reds four tests including this one. Its sibling
`an_encountered_passage_offers_its_verbs` pins `{Enter, Examine}` from the
other direction, so the pair cannot be satisfied by an empty registry row.

**3. What is NOT met, stated here rather than left for a reader to discover
— and stated NARROWER than this clause's first draft, which was reached
without a fact that bears on it.** The draft said the gate "still cannot deny
through a **live `Session`**". That is false about the PATH and true about the
STATE, and the two need separating.

`session.rs`'s `examine_chamber_anchor_is_refused_when_the_observer_has_no_
recorded_knowledge` builds a real world, starts a real `Session`, enters a
real chamber, takes a real anchor's noun, and gets the player-facing refusal
`"You see no {noun} here."` out of `Session::examine_chamber` — which reaches
this gate through `thing_kind_of`. It is one of the three tests the gate
mutation reds on an unfiltered run. So the production call path DOES deny, in
a live session, with a real anchor, and the denial is observable as prose.

What is unreachable is the **state**, not the path, and separately the
**passage**:

- Knowledge absorption is unconditional before the first turn (`Session::new`'s
  `absorb_here`), so `known` is never empty in production. That test reaches
  the denying branch by assigning `session.knowledge = Knowledge::default()`
  — a real and reachable state of the type, written by the test rather than
  produced by the world. In 0349's vocabulary this is still not a *firing
  case*: no seed and no transcript produces it.
- No production caller passes `KindId("cave-mouth")` at all. Chamber entry
  gates on the cave mouth's own `openness` fold (0396), not on this query. So
  the passage denial is not merely un-fired, it is off every production path,
  which is a strictly weaker position than the anchor half above.

**These halves are different claims and 0369 named only the first.** Its
reason section is entirely about currency: the enum, the missing variant, the
`Vertex`/`ChamberAddr`, `passage.rs` holding zero references to `Knowledge`.
That half is discharged. A campaign that wants the second half must change
what `knowledge.rs` already names as its own future direction — gate
absorption on light or perception, or let a lying object write a `Knowledge`
that omits a room truthfully known — and **not** the key, which is now the
wrong lever.

## Consequences

- **`AnchorKind::ALL` is generated from the enum's own declaration, and every
  roster of anchor kinds in the workspace now reads it** (fix round 1). Clause
  2's precondition is the reason this record cares: it is what keeps
  "no anchor kind reaches `cave-mouth`" honest, and it was sweeping a
  hand-written list that no compiler, and no possible test, could hold to the
  enum's cardinality. Three such lists existed —
  `windows/vessel/tests/suite/affordance.rs`,
  `cli/tests/suite/anchor_thing_correspondence.rs`,
  `windows/vessel/src/chamber_prose.rs` — each carrying a comment claiming an
  exhaustive match kept it in step, which is true of an ARM and false of a
  LIST. **All three get the mechanism, not just the one clause 2 depends on**:
  a repair that generalizes in its reasoning and stops at one file leaves the
  next reader with a rule they cannot trust, and the cost of the other two was
  two deletions. A `macro_rules! anchor_kinds` in
  `windows/vessel/src/interior/anchor.rs` declares the enum and the roster
  together; `the_anchor_kind_roster_is_generated_from_the_enums_declaration`
  scans that it stays that way, since unwinding it back into two hand lists
  compiles, behaves identically, and silently removes the property.

- **0369 is answered, not superseded.** Its rule is unchanged and this record
  is evidence for it rather than against it: the fix was a key, exactly as
  0369 said the obstacle was a key. A forecast that was short by one option is
  not a rule that was wrong.
- **0396's forecast is corrected here rather than edited there.** Its
  consequence list says "Task 9's re-key of `offered_to_observer` to `KindId`
  is that caller." The re-key landed and it is *not* a caller: it makes a
  cave-mouth argument expressible, and the test suite is what supplies one.
  Decision records are append-only, so the correction lives in this record and
  in the doc comment on `object_registry`, which carried the same optimistic
  sentence and now carries this one.
- **`AnchorKind` no longer appears in any dispatch signature in
  `affordance.rs`**, and one test's doc said otherwise. The synthetic fixture
  `the_dispatch_scan_catches_an_anchor_kind_keyed_table` is kept anyway — with
  its claim to be "the one dispatch function that still speaks that key"
  removed — because a scanner's coverage of a spelling must not lapse when the
  last live instance of that spelling leaves the tree. The frozen dispatch
  roster (`["offered_by", "offered_to", "offered_to_observer"]`) is unmoved:
  the selector keys on the type shape, never on the spelling, which is the
  property its own fix round was written to buy.
- **The key space widening 0348 flagged now reaches the gate too.**
  `offered_to_observer(KindId("srongbox"), …)` is a well-typed call answering
  `{Examine}` where the enum-keyed version could not compile. That is the same
  loss `offered_by` already took at Task 7, bought back the same way, by
  `cli/tests/suite/anchor_thing_correspondence.rs` pinning every key this
  module mints against `hornvale_thing::THING_KINDS`. It is worth naming twice
  because the gate is the surface where a silently-empty answer reads as a
  *refusal* rather than as an absence.

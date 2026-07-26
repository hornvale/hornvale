# Retrospective — The Hearth

**Campaign:** The Hearth (Rose Window campaign 1 as amended; PSY/CLIENT fine layer)
**Shipped:** 2026-07-25 · T1 `2a76215a`, T2 `28895452`, T3 `357feeb7`,
T4 `ead57a6a`, T4a `d362a889`, T5 `ca0dac31`, T6 `c06587c8`
**Outcome:** the substrate ships inert and provably so — no world changed, no
almanac, lab CSV, elevation map or scene export moved; gate green (2063 passed);
health battery 325.8 s with chronicity `0.0`.

## What worked

- **Seven ideonomy passes overturned the design three times, twice against the
  pass immediately before.** The corrections were not refinements: rung 1 was
  originally "authored room templates," which is precisely the
  catalogue-not-a-language failure the campaign's own criterion forbids, arriving
  on the first rung; a "committed items" tier invented storage where the existing
  fold pattern suffices; and the fine layer turned out to need no lattice and no
  coordinate solve at all, because a room's interior is just a very small
  room-graph. The generalizable part: passes 1–4 overturned, 5–6 added structure,
  7 attacked by negation and moved nothing. **That progression is what
  convergence looks like**, and it is a better stopping rule than "it feels
  settled."

- **Borrowing the vocabulary instead of inventing it was the single highest-value
  decision.** RCC-8 supplies eight relations that are jointly exhaustive and
  pairwise disjoint with a published composition table; Allen's interval algebra
  is its twin for time. Demanding JEPD as a criterion also *structurally* prevents
  catalogue sprawl — a partition cannot be padded. Checking the literature before
  designing a vocabulary cost an afternoon and removed a whole class of
  soundness bug.

- **A parallel campaign reviewed this one mid-flight and materially improved it.**
  The Threshold — branched off this campaign to build its other half — found that
  `compose` was degenerate (a hub: everything one hop from the centre, so field
  decay had nothing to decay over and the demonstration was a single-step route),
  asked for the inventory to reach its intended size *before* furnishing goes
  live, and caught that the future seeded draw must key by name rather than
  position. All three were cheap then and epoch-expensive later. **Two campaigns
  brainstorming the same substrate from opposite ends found things neither would
  have alone**, and the cost of acting was low precisely because the review
  arrived before the task was dispatched.

## What the campaign taught

- **Two functions can answer the same question differently for a long time
  without anyone noticing, and the symptom is a test passing for the wrong
  reason.** `Interior::is_connected` walked adjacency *and* containment;
  `InteriorSpace::successors` walked adjacency only. So a hearth inside an alcove
  was its own routing component, `permits()` approved rooms whose fire no creature
  could reach, and the anti-hub test — written to check that the intended
  four-step chain existed — passed instead via an unrelated three-step arm. **A
  green test that never exercised the thing it names is worse than a red one.**
  The implementer stopped and reported rather than adjusting the assertion, which
  is the behaviour the 3-attempt rule exists to produce. Worth adding to the
  reviewer's checklist: when a test asserts a *property* (depth, presence,
  absence) rather than a *value*, confirm which path satisfied it.

- **The fix belonged in the reader, not the writer, and the reason generalizes.**
  Making `compose` add an adjacency edge for contained anchors would have worked
  and been wrong: the hearth would be both `Ntpp` and `Ec` to the alcove, and
  since `relation()` tests containment first, that edge would have been invisible
  to the vocabulary while visible to the planner. **Prefer the fix that keeps two
  views of one structure in agreement over the fix that adds a second truth.**

- **A stale type-audit tag is invisible until the signature changes.** `Thermal`
  carried `bare-ok(return)` — not a ratified class — and the tool had never parsed
  it because the struct had no bare primitive at its boundary. Adding one
  `Option<f64>` made the tag legible and it failed instantly. This is exactly the
  footgun `tools/type-audit/CLAUDE.md` documents, observed from the other
  direction: not a tag going stale when a signature changes, but a *wrong tag
  lying dormant until a signature makes it checkable*.

- **Writing the plan is a design activity, and it found a scope error the spec
  had shipped.** The spec's headline criterion said a thermally stressed creature
  "targets the hearth anchor," implying the sim did this live. Laying out the
  tasks made it plain that nothing derives an `Interior` from a real room and
  creatures have no anchor position, so every live site passes `None` and the
  outcome is *demonstrated, not observed*. Surfaced to the owner rather than
  quietly narrowed; the criterion was rewritten and the follow-up campaign named
  in §9.1. **The plan is the first place a spec's claims meet arithmetic.**

- **My own plan carried two errors the implementers caught:** an invalid
  `type-audit` class (`bare-ok(tag)`, which is not one of the eleven ratified),
  and an expectation of `ARTIFACTS CLEAN` that ignored the fact that declaring a
  new stream label necessarily changes the generated stream manifest. The second
  is the more interesting: I wrote a byte-identity expectation without asking
  which generated artifacts *enumerate the thing I was adding*. The implementer
  attributed the drift by stashing and regenerating at the previous commit rather
  than assuming — the right instinct, and the one this project keeps having to
  relearn.

## Follow-ups

Owed to **The Threshold** before it dispatches: its plan's Interfaces blocks were
written against this campaign's *spec*, and `compose`, `INVENTORY` and
`route_within` all changed shape afterwards — its own BLOCKING PRECONDITION says
to re-read the real tree, which now has something new to read. Also flagged
there: the bed sits tied with the alcove at 0.5 warmth, so a mildly cold creature
may find the bed sufficient and never cross to the fire; if the namesake
behaviour is wanted, the drive must prefer the strict maximum rather than merely
somewhere warmer.

Reserved in the spec and registry: projective relations (concealment, sightlines,
social protocol), items as entities whose position folds over custody events,
promotion-on-touch (designed, deliberately unbuilt — no consumer in v1), and
ladder rungs 2–4 ending at creatures arranging their own space.

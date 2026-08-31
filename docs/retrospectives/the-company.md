# The Company — retrospective

Process, not product. The chronicle carries what was built.

## The fix for the last campaign's defect was itself asserting on the wrong channel

The Repertory closed by finding a beat that could never fire: it asserted a
pointer name invented from outside the code. This campaign opened by finding
that the beat *beside* it had the same disease in a different organ — it
asserted against `social`, a channel whose own doc says its membership is world
truth and that rendering it unfiltered "ships a cheat pane". The description
claimed co-location; the assertion tested existence-of-any-NPC.

Two beats, written in the same sitting, both wrong, in two different ways,
neither found by re-reading. What found both was contact: the first by
absorbing another campaign's merge, the second by running a measurement
(`0 of 24`) that nobody had asked for.

**The generalisable form:** when a check is written against a data structure
the author has not read, the failure is not random. It lands on whichever
field is *nearest in name* to the thing meant. `social` sounds like who is
around you. `holding` sounds like what someone holds. Both were plausible and
both were wrong, and plausibility is what stopped either from being checked.

## The measurement was already in a doc comment

`0 of 24 witnesses have anyone present` was presented in the spec as a
discovery. It is not: `Session::place_creature_at_me`'s doc comment already
recorded *"seed 42's flagship possession still finds nobody to provoke after
sixty `wait`s"*, with a registry row (`SOC-one-creature-per-settlement`) and a
ruling from the project owner about what it would cost to change.

It was found only because a botched edit displaced that doc comment and the
compiler complained about a missing one. **Nothing in the workflow would
otherwise have surfaced it** — the campaign grepped the code for the *field*
it wanted, not for prose describing the *behaviour* it had just measured.
Worth the habit: after measuring something surprising about the world, grep
the doc comments for it before writing it up as new.

## Four plan-text defects, all in the author's own prose

Consistent with every prior campaign and worth counting rather than
summarising:

1. Tasks 1 and 2 were written as separate commits. They cannot be: the commit
   gate runs `the_client_fixtures_are_current`, so code and regeneration are
   one drift.
2. The plan warned that the type audit would fire on `PresentEntry.carrying`.
   It does not — `Vec<CarriedEntry>` is not a primitive, and `SelfChannel`'s
   own tag omits its identical field. It fired instead on the test seam's
   `kind: &str`, which the plan never mentioned. **Right instinct, wrong
   location**, which is the failure mode a prediction has and a decision rule
   does not.
3. Tagging that primitive drifts the *committed* type-audit report, so
   `rebaseline` has to run after the tag. The plan had it before.
4. The plan asked for a whole-workspace test run to find goldens outside both
   regeneration paths. A guard refuses that locally and is right to: that is
   what the stage gate is for.

## An assertion can predict its own staleness and still go stale

`every_scene_carries_a_witness_and_at_least_one_beat` asserted that every
selector resolves, with the message *"until `UNWITNESSED` exists that is a
corpus error rather than a finding"*. Two tasks later `UNWITNESSED` existed and
the assertion was wrong — it would have made the corpus unable to hold the very
state the campaign added.

The message was written by someone who could see the future clearly enough to
describe the condition that would invalidate the check, and there was still no
mechanism to act on it. A conditional written into a failure string is a note
to a reader, not a tripwire. If the condition is nameable, the check should key
on the thing itself rather than on a sentence about it.

## Absorbing at the stage boundary, again

89 commits absorbed mid-campaign (The Winze, The Gleaning), both touching
`windows/vessel/` including fixtures regenerated an hour earlier. One conflict,
on a generated aggregate, resolved by **regeneration** rather than by hand
because its content is a function of the source tree and not of the two sides
of a diff. The positive control on that resolution: regenerating again against
the merged tree moved nothing, which a wrong hand-merge would not have
survived.

The previous campaign learned the same lesson the expensive way — an
absorption falsified two committed records. Two campaigns, two absorptions,
two findings that no review would have produced.

## What is deliberately not witnessed

`two-in-a-room`'s second beat asserts that the custody *channel* is there, not
that it is populated. No co-located creature in any world found so far is
holding anything, so the non-empty case is proven by vessel's own test and by
no scene. That gap is recorded in the scene's own provenance rather than
papered over, because a beat that quietly asserts less than its title is the
exact defect this campaign opened by finding.

## Followups

- **A carried thing a co-located creature actually holds.** Needs either a
  world where a wild pair carries something, or a verb that puts a thing down
  where another creature can pick it up.
- **The species-constrained search is bounded and shallow** (3 seeds). A real
  answer about drow-and-goblin co-occurrence wants the bound raised, which
  wants the search cheaper than one world build per seed.
- **`PARTIAL`** is still unimplemented, carried from The Repertory.
- **`--creature` refuses one seed-42 roster member** (`13226382737635672064`).
  Treated as a miss, cause unexamined.

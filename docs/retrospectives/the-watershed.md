# The Watershed — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-watershed.md).

## The lesson, four times

**One world is an anecdote. The battery is the instrument.**

This campaign produced four wrong readings from a single seed, and caught three
of them only because something else forced a wider measurement:

| claim | seed 42 said | 8 seeds / 1000 worlds said |
|---|---|---|
| where the arity cliff sits | between 2 and 3 concepts | no cliff; ~20pp per concept |
| the river multiplier | (simulated) ×1.99 | ×1.63 |
| predecessor peoples | *worse*, +0.9pp | better, −0.4pp |
| name transparency | rose, 202→216/329 | **fell**, 0.816→0.793 |

The fourth got into a commit message before the census caught it. The spec
itself had been written from a seed-42 arity table and specified the wrong
floor as a result — and the previous session had already recorded "measure on
the codebase the change will run on" as its hard-won caution, without noticing
that *how many worlds* is the same class of error as *which codebase*.

**What to do about it:** a claim that will steer design gets the battery, not
the reference seed. Seed 42 is for byte-identity and for reading output, not
for deciding whether something works.

## Preregistration protected the campaign from itself

§5's below-15% claim was frozen before the code. Measuring it first — the
tuple-floor computation, an afternoon's work — killed three items' worth of
implementation before any of it was written, and turned the campaign's headline
from a feature into a finding.

That is preregistration working exactly as decision 0016 intends. Worth naming
because the alternative was legible: build items 2–4, measure at the end, and
discover the ceiling with the code already merged.

## A preregistered baseline must be reproducible from the repo alone

The campaign's headline baseline — "59.8% over 8 seeds, 1842 settlements" —
was produced by a harness that lived only in a git-ignored scratch worktree.
It is gone. The number survives as prose in a spec and **nobody can reproduce
it**, which makes it unusable as the baseline a frozen criterion is measured
against.

0016 freezes the hypothesis; nothing freezes the instrument. The fix is cheap:
a number a spec's verification section leans on comes from a committed harness,
never an ad-hoc script. `windows/worldgen/tests/watershed_measure.rs` is now
that harness — and it had to declare a *new* battery, because the original
seeds are unrecoverable.

## "Not here" is not "gone"

The session opened by sweeping the Mac, finding an empty worktree directory and
no branches, and reporting the campaign's ledger and its sonority commit as
lost. They were on **lefford**. One `ssh lefford git branch -a` settles it, and
it should have been the first command rather than a conclusion drawn from a
one-machine search.

The corollary bit later too: `the-shibboleth` existed on exactly one machine and
was never pushed. Work that exists in one place is work that is one disk away
from gone.

## Guards that cannot fail read as coverage

Three instances, all closed here, all the same shape:

1. **The stale second opinion.** `independently_steeped_concepts` duplicates
   the exposure rules deliberately. It silently fell behind an added rule for
   the *second time in eleven days*; `exposure-sound` read false on ~75% of
   worlds and nothing reddened. Repaired — see F8 for the structural proposal,
   which is the one followup worth a decision record.
2. **An ignored test's fixture.** The mutation test guarding `exposure-sound`
   carried a precondition that had gone stale while the test sat `#[ignore]`d.
   A token guard proves a deferral is *findable*, never that it is still *true*
   — which `heavy_tier.rs`'s own doc had already written down.
3. **A pin comparing against NULL.** With the blind-world count at zero,
   `min`/`max` over an empty set return NULL, the comparison returns NULL, and
   `census-check` printed green while two pins verified nothing. Deleted rather
   than re-pinned, on the same argument.

The `open-questions.md` caution — *"an instrument only scores a bet if
something reads it"* — now has a second and sharper instance: an instrument can
be read constantly and still be wrong, and a campaign whose central mechanism
it measures is exactly the campaign least able to notice.

## Absorb early; the branch was 48 and then 20 commits behind

Two absorptions, both at stage boundaries, both cheap: one conflict total
(`docs/timings.md`, an append-only ledger — both sides' rows kept). The
alternative was meeting The Shuttle *and* The Weir at the merge.

The Shuttle also answered an open question for free: the suite fell from
**3739 s to 328 s** across the two absorptions. A 62-minute gate that looked
like this campaign's cost was inherited cost, since fixed by somebody else.
Worth remembering before budgeting from a number in a doc.

## Prose and numbers drift apart in re-pins

A census re-pin touches four files. It turns out there is a fifth surface
nothing checks: the **comments beside the pins**. Two sites were found
asserting one value while their own adjacent note recorded another — a previous
discharge had updated the commentary without the constants. Both are consistent
now, but nothing would have caught it.

## An epoch's artifact surface is wider than `make rebaseline`

`hornvale voice` is not called by `regenerate-artifacts.sh`, and the phonology
page's audio clips are content-addressed on the very words an epoch re-mints.
The only thing that catches it is a test inside the gate reporting a failure
rather than a stale artifact. Filed as F6; the fix is one line in the script,
which claims to be the single source of freshness truth and is not.

## Follow-ups

Promoted here in full, because the scratch register dies with the worktree.
Ranked: F8 and F1 are the two worth acting on beyond this campaign.

- **F8 — the deliberate duplicate breaks on every exposure-rule addition.**
  `independently_steeped_concepts` duplicates `exposure_of`'s rules on purpose
  (a check calling the code under test asserts nothing), and that should stay.
  What should not is that omission is invisible: twice in eleven days it fell
  behind and `exposure-sound` read false on ~75% of worlds with nothing red.
  **Proposal:** share the rule *roster* — one declarative list of exposure
  classes — while both sides keep computing their own answer, so a missing
  class is a compile error and the derivations stay independent. Worth a
  decision record; the repo uses "duplicate deliberately" in several places
  (`phenomenon_concept` has three copies) and none of them says how to keep the
  duplicate honest. **Ratified as
  [decision 0094](../decisions/0094-a-deliberate-duplicate-shares-its-roster-never-its-derivation.md)
  at the close; the conversion of existing duplicates is not required
  immediately, but the shape of the answer is now settled.**
- **F1 — a preregistered baseline must be reproducible from the repo alone.**
  A number a spec's verification section leans on comes from a committed
  harness, never an ad-hoc script in scratch. This campaign lost its own
  headline baseline that way.
- **F2 — `landmass_size_capped` and the spec's `LandmassId` are one
  computation twice.** If the landscape layer is ever built, the window must
  read the domain's definition rather than keep its own capped flood fill, or
  "is this an island" and "which landmass is this" will disagree.
- **F3 — the measurement harness's battery is a declared guess.** Seeds 1..=8
  are not the original set (1837 settlements against the recorded 1842, with a
  demonstrably unmoved pipeline). Its numbers are a new baseline, never a delta
  against 59.8%.
- **F4/F5 — `the-shibboleth` is lefford-local**, and the spec anchored Items
  2–3 on its `sitefact` module, which §8 had simultaneously decided to discard.
  Re-anchor to main's `is_river_cell` / `WATERFALL_MIN_DRAINAGE` /
  `ISLAND_CELL_CAP` / `landmass_size_capped` before planning that work.
- **F6 — `regenerate-artifacts.sh` never calls `hornvale voice`.** The
  phonology page's audio is content-addressed on the very words an epoch
  re-mints; only a gate test catches the drift, and it reports a failure rather
  than a stale artifact. One line in the script fixes a source-of-truth claim
  that is currently false.
- **F7 — resolved during the campaign.** The 62-minute gate was inherited cost,
  not ours: The Shuttle and The Weir took the suite to ~330 s across two
  absorptions.
- **F9 (new, from the close) — a census re-pin has a fifth surface.** It
  touches four files, and the *comments beside the pins* are a fifth that
  nothing checks. Two sites were found asserting one value while their own
  adjacent note recorded another.

## What the next campaign inherits

The landscape layer is specified, measured, and **not built**, with a known
ceiling of ~45%. The empty branches of the tree — names from persons and from
events — have the cardinality that place-particulars lack, because one founder
and one killing per place do not cluster the way a river does. Borrowing is
decided (settlement-count selection) and owes its own stream label under 0083,
which the two labels the spec asked for do not.

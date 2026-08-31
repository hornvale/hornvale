# Retrospective — The Legend

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-legend.md): a central glyph
register with a novelty guard, three collisions resolved by rule, creatures
and terrain finally distinguishable, and the coverage audit's first unmet
item closed. Three lessons here, each recurring enough within one campaign
to be worth naming rather than treating as one-off noise.

## Theme 1: the spec specified four classifiers this codebase already had

Water, elevation, landforms, and point-feature salience each got a
freshly-specified classification scheme in the plan, and this crate already
had all four:

- **Water** (`WaterKind::index()`, `domains/terrain/src/water.rs`) was
  already the stable, self-describing legend `windows/scene/src/region.rs`
  already emitted on the wire. The plan's `WATER_LEGEND` disagreed with the
  canonical one on two of four names ("salt basin" vs "salt-basin"). Caught
  only after Task 3's implementer had built the duplicate and flagged it in
  their own report rather than silently shipping an improvement — the right
  behaviour, and the reason it was caught at all.
- **Elevation** (`relief_band`, `windows/scene/src/surrounds.rs`) was
  already public, already sea-level-datum-correct (the exact bug class The
  Benchmark exists to prevent), and already distinguished abyss from shelf —
  real information the plan's five-band near-copy discarded. Caught by a
  reviewer's Minor, one fix round after the first duplicate had already
  shipped.
- **Landforms** (`FeatureClass`, `hornvale_terrain::landscape`) and
  **point-feature salience** (the same module's declared per-class ordering)
  collapsed the plan's five landforms to two genuinely new ones — waterfalls
  and deltas — with volcanoes, playas and mountain ranges all already
  representable. Caught by grepping the crate before dispatch, not after an
  implementer built anything.

The pattern is the sharpest available test of the campaign's own thesis —
one classification, never a second implementation — landing on the plan
that asserted it. The split matters more than the count: two duplicates were
caught only after a build, costing a fix round each; two were caught for
free by reading the tree first. Grepping before dispatch is strictly
cheaper, every time it was tried.

## Theme 2: three instruments ran green while structurally unable to see their subject

Not three failures of the same shape — three different ways a check can
pass without having tested anything, which is why each needed a different
fix:

1. **A grep that matched on any change at all.** The plan verified
   `elevation_m` survived an additive schema change with
   `git diff -- fixtures/ | grep '^-' | grep -i elevation_m`, expecting no
   output. The fixtures are single-line minified JSON, so the removed line
   contains every key in the document — the check does not fail silently,
   it cries wolf on every legitimate change. Run against the real additive
   commit, it printed a match. The implementer substituted a parsed-field
   diff, got the right answer, and said so in the report rather than
   escalating a false BLOCKED or learning to skip the check.
2. **`git diff --exit-code` against an untracked path.** Verifying a newly
   declared generated path is tracked with the same command that checks an
   already-tracked one's drift is silently vacuous — there is no index
   entry to diff against, so it exits 0 whether or not the file was ever
   `git add`-ed. The real guard already existed
   (`cli/tests/suite/generated_paths.rs`, which names the hazard `vacuous`
   in its own source); the plan had quoted that hazard two paragraphs above
   the command and then specified the vacuous check anyway.
3. **A stability test pinned one layer below where the regression occurs.**
   `tests/lexicon.rs` proved `creature_glyph` is a pure, stateless function
   of the noun alone. The regression this campaign actually cares about — a
   creature's letter changing because of who else is in view — was fixed by
   deleting the *caller-side* rank assignment, not by anything
   `creature_glyph`'s own signature could see. A caller that reintroduced
   per-render de-duplication would pass every assertion in that file
   unchanged. The fix pins the property through the real drawing entry
   points (`chart::draw`, `plan::draw`) instead, and requires the
   regression to be reintroduced, observed red, and pasted before trusting
   the new test.

All three are recorded here as one class because all three PASS is
consistent with the check never having observed its subject: reading the
check does not reveal it (each looks reasonable in isolation), and running
it does not reveal it (it is green either way). What worked: in all three
cases the person closest to the check — an implementer running it against
the real diff, a reviewer reading the cited hazard against the command
above it — is who caught it, not a re-read of the plan.

## Theme 3: a record that outlives its subject produces wrong answers from good-faith readers

CLAUDE.md states this as a general lesson about a stale paragraph misleading
a later reviewer who read it in good faith. This campaign produced two
instances of the identical shape against its own documents, not someone
else's:

**The spec's own §3.0 argument.** Two legs, both weaker than written, found
in the same session — one by a peer campaign's challenge, one by checking
the peer's challenge and finding a second problem underneath it. A cross-
session peer (The Pavement) first corrected 0287's cited text directly
(a grep for "vertex" across the decision record: zero matches — the claim
attributed to it was an implementation corollary, never something 0287
said). Checking that finding turned up the second: 0196 never applied to
the field the argument was built to protect, because that field is discrete
and 0196 only ever governed continuous interpolation. A two-legged argument
with one contingent leg and one leg that did not apply to its subject had
been sitting in the spec since before code existed to check it against —
corrected before Task 11's chronicle could cite the phantom version, but
only because a peer session asked a question this campaign had not asked
itself.

**The specimen sheet's own disjointness assertion.** The sheet asserts that
every candidate glyph is disjoint from the live register — a correct check,
right up until the campaign did the thing the sheet exists to support: Task
6 adopted the winning candidate INTO the register. The assertion's
precondition — nothing is claimed yet — was violated by the campaign's own
success, and `make rebaseline` broke on the very next task for a reason that
had nothing to do with that task's own change. The fix was not to delete the
check but to narrow it: candidates must be disjoint from register claims
belonging to a *different* population, so a winning candidate's own claim
reads as the selection working rather than as a collision.

The throughline: a record's staleness is not always visible to the person
extending it, because the record was true when written and the world moved
out from under it without announcing itself. The mitigation that actually
worked both times was the same one — a second reader (a peer session, an
implementer running the literal check) encountering the record fresh,
without the context that made the original author confident in it.

## The numbers

H1 (spec §7.1, extracting the terrain classifier into a shared function)
was measured, not assumed: three separate invocations of a dedicated
harness, five replicates each, warm 200x200 redraw at the coarsest rung
landed at 0.049-0.068 ms against a 0.20 ms bar — supported, with headroom
to spare, and consistent with the prior campaign's own 0.056 ms baseline
for the identical quantity. Full replicates, load, and the drift check's
result are in `docs/timings.md` and this task's own report.

## Deferred minors (promoted from the scratch ledger at close)

Small, real, and deliberately not chased down mid-campaign:

1. The retargeted agreement pin no longer exercises `draw_terrain_layer`'s
   row/col -> grid wiring.
2. `band_b_still_shows_the_observer_over_its_own_terrain` excludes `@` but
   not settlement/cave marks.
3. Volcano is discovery-gated while waterfalls draw unconditionally —
   intentional per `plate::draw_feature_layer`'s own doc, noted here so a
   future reader does not read the asymmetry as an oversight.
4. `.DS_Store` is not gitignored (`git status` lists it as untracked) — a
   one-line fix, deliberately left for the board lane or a chore commit
   rather than a glyph campaign (`.superpowers/sdd/followups.md` #7).

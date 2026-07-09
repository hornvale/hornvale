# Study 010: The Census of Words

Campaign 12, The Words, gave every registered concept a home in a
per-species lexicon (a real proto-root plus a real Neogrammarian
sound-change history, or a compound over two roots, or a reasoned gap) and
gave every settlement and deity name a **gloss**: the 1-2 site concepts —
its own biome, its species' presiding phenomenon — that name is a truthful
compound of, replacing the free-stem draw Campaign Y2-3 (The Tongues)
shipped. Spec §9 (`docs/superpowers/specs/2026-07-09-campaign-the-words-
design.md`, committed `194eb08`) preregisters five Lab claims this study
exists to measure; this page carries all five, grouped into the four
falsifiable rows the campaign plan's Task 12 assigns to one Lab pass.

**A note on scale and provenance.** This chapter's headline numbers come
from a 10,000-seed run of `studies/census-of-words.study.json`,
executed once by hand at author time, in the same arrangement Studies 001,
002, 003, 006, 007, and 008 use for their own 10,000-seed headline runs. It
is **not** committed and **not** part of CI. The exact-count calibration
rows this page pins are measured instead over the two CI-drift-checked
500-seed populations every prior campaign's calibrations already share:
`census-lands-drift` (`book/src/laboratory/generated/census-lands-drift/`)
for every per-world and per-species metric below, and (unchanged by this
study) `census-of-the-meeting` for the null control's own name-length SMD,
which this task also re-pins alongside the two Tongues-era rows Task 9's
glossed compounds moved out from under. Small differences between a number
quoted here and the 500-seed pinned figure are ordinary 500-vs-10,000-seed
sampling variance, not drift.

**A note on preregistration.** Per ADR 0016, this page's hypotheses are
committed **before** the metrics that measure them exist in code, and
before any census carrying them runs — the two-commit discipline Task 12's
brief spells out: this preregistration commit alone, then a second commit
implementing the metrics, running the census, and pinning the results. No
claim below is adjusted after the fact; a claim that fails is reported as a
failure, not loosened to pass.

## Question

Does every committed settlement name-gloss compose truthfully from that
settlement's own site facts; does each species' lexicon replay its own
sound-change derivations exactly and never mint a root for a concept it has
no exposure to; does the goblin's keener daylight/duller night vision give
it strictly more hue vocabulary than the kobold's; and, now that names are
drawn as compounds over each species' small site-concept vocabulary rather
than free 2-3-syllable stems, has the in-world name-collision rate moved —
and by how much?

## Hypotheses

Quoted and paraphrased from spec §9, "The Lab" (preregistered 2026-07-09,
before any census carrying these metrics ran, per ADR 0016):

**H1 (name-gloss truthfulness, exact).** Spec §9.3: "for each named entity,
the glossed concepts appear among that entity's actual facts, row-by-row; a
broken gloss pipeline is falsifiably caught." Measured here as
`name-gloss-true`: **100%** of settlements carrying a committed
`name-gloss` fact, row-by-row over the census, gloss to a truthful
composition of their own INDEPENDENTLY re-derived site concepts (biome plus
presiding phenomenon) — never a concept borrowed from anywhere else in the
world.

**H2 (lexicon regularity and exposure soundness, exact, both species).**
Spec §9.1: "every proto-lexicon entry containing a rule's conditioning
environment undergoes it; the derivation replay equals the committed modern
form" (`lexicon-regular`). Spec §9.2: "zero roots for UNKNOWN concepts;
every gap and every compound recounts to a ledger-fact or vector reason...
the flag re-derives the exposure class from the ledger independently of the
lexicon pipeline" (`exposure-sound`). Measured here as two independent
per-species flag pairs — `lexicon-regular-goblin` / `lexicon-regular-kobold`
and `exposure-sound-goblin` / `exposure-sound-kobold` — expected **true in
100% of present cases, for both species**.

**H3 (pack-depth calibration, directional, exact ordering).** Spec §9.4:
"each species' color-ladder depth matches its perception vector's
derivation." The shipped roster's night-vision values (goblin 0.5, kobold
0.9) predict, via `pack_depths`' documented derivation, that the goblin's
hue-ladder depth is **strictly greater than** the kobold's:
`hue-depth-goblin > hue-depth-kobold`, in every present world (this is a
structural constant of the roster's authored vectors, not a per-seed draw,
so the census is expected to confirm it holds identically at every seed).

**H4 (name-collision rate, directional-only before measurement).** Spec
§9.5: "glossed compounds shrink the name space versus free stems; the
calibration row re-pins and confirms the rate stays low." Before this
census's exact figures are measured, the only claim committed is
**directional**: `name-collision-rate`'s mean, re-measured over the
500-seed `census-lands-drift` population, stays **below twice the
Tongues-era pinned rate** (2.339% × 2 = 4.678% — Study 008's calibration
row, `windows/lab/tests/calibration.rs::name_collision_rate_is_measured_and
_pinned`, pinned at the Y2-3 re-baseline). The exact re-measured rate — and
the parallel re-measurement of `name-length-goblin`/`name-length-kobold`'s
distributions and the null control's name-length SMD, all three already
carrying `#[ignore]`d Task-12 markers in `calibration.rs` — is pinned only
after this census runs, never tuned to clear the bound after the fact.

## Analysis plan

Measure, over the full 10,000-seed `census-of-words` run: `name-gloss-true`
row-by-row (H1); `lexicon-regular-goblin`/`-kobold` and
`exposure-sound-goblin`/`-kobold` row-by-row (H2); `hue-depth-goblin` vs.
`hue-depth-kobold` per seed (H3); and `name-collision-rate`'s distribution
alongside the per-species `name-length-*` distributions (H4, descriptive at
10k). Pin the exact 500-seed `census-lands-drift` calibration rows for H1
through H4 in `windows/lab/tests/calibration.rs`, re-pinning the three rows
Task 9 left `#[ignore]`d (`name_collision_rate_is_measured_and_pinned`,
`name_length_distributions_are_measured_and_pinned`,
`null_control_name_length_smd_is_pinned`) and un-ignoring them once the new
exact figures are committed.

**Verdict criteria**, fixed before analysis: H1, H2, and H3 are confirmed
only if their respective flags/ordering hold in **100%** of present cases —
any exception is a STOP-and-report condition per ADR 0016, not a rate to
loosen. H4 is confirmed if the re-measured mean collision rate is smaller
than 4.678% (the preregistered directional bound); the exact figure is then
pinned as a calibration row regardless of which direction it moved within
that bound, exactly as Study 008 pinned 2.339% without a floor to clear. A
verdict of **defect** would mean H1, H2, or H3 failing anywhere in the
census, or H4's rate meeting or exceeding the directional bound — no such
case is anticipated, since H1-H3 are structural invariants of the glossing
and exposure pipeline Task 10's keystone suite already exercises at seed
42, and H4 only asks that a combinatorially smaller (but still large)
compound name space stay a small-collision regime, not a collision-free
one.

## Results

**H1: name-gloss truthfulness, at 10k — confirmed exactly.** Of the 9,997
present worlds (3 absent — the same zero-habitable-fraction worlds Studies
002, 003, 006, 007, and 008 already name: seeds 895, 1009, and 4322 hold
neither species' settlements), every committed settlement `name-gloss`
composes truthfully from that SAME settlement's own INDEPENDENTLY
re-derived site concepts — **`name-gloss-true`: 9,997/9,997 true, zero
exceptions.** The 500-seed calibration
(`name_gloss_true_is_100_percent_row_by_row`) reconfirms this at the
CI-guarded population.

**H2: lexicon regularity and exposure soundness, at 10k — confirmed
exactly, both species.** Every one of 10,000 present worlds reads
`lexicon-regular-goblin`, `lexicon-regular-kobold`,
`exposure-sound-goblin`, and `exposure-sound-kobold` **true — 40,000/40,000
true across the four columns, zero exceptions.** No Root entry was ever
minted for a concept an independent, from-the-ledger re-derivation of
exposure classified outside `Steeped`, and every committed Gap carried a
non-empty reason. The 500-seed calibration
(`lexicon_is_regular_and_exposure_sound_for_both_species`) reconfirms this.

**H3: pack-depth ordering, at 10k — confirmed exactly.** `hue-depth-goblin`
reads **4** in all 10,000 present worlds; `hue-depth-kobold` reads **2** in
all 10,000 — the roster's night-vision values (0.5 vs. 0.9) produce a
structural constant, exactly as the shipped-roster derivation predicts,
never varying by seed. The 500-seed calibration
(`goblin_hue_depth_exceeds_kobold_hue_depth`) pins both constants.

**H4: the name-collision rate — measured, and the directional claim
FAILED.** At 10k, across the 9,997 present worlds, the mean in-world
collision rate is **86.77%** (mean 0.867749967402372), with only **13 of
9,997 worlds (0.13%) showing zero collisions** — the inverse of the
Tongues-era shape, where 64.0% of worlds saw zero collisions and the mean
sat at 2.79%. The 500-seed `census-lands-drift` population confirms the
same magnitude: **2 of 500 worlds zero-collision, 498 nonzero, mean
86.28%** (0.862829625681313), now pinned exactly in
`name_collision_rate_is_measured_and_pinned`. This is **dramatically above**
the preregistered directional bound (below 4.678%, twice the Tongues-era
pinned rate) — the claim, as stated, **failed**. Per ADR 0016 and this
task's brief, the finding is reported honestly rather than adjusted: the
bound is not widened, and the measured rate is pinned as-is.

The companion `name-length-goblin`/`name-length-kobold` distributions
explain the mechanism. At 10k, goblin names average **6.51 characters**
(mean 6.5078147653602745, 9,972 present, 28 absent) and kobold names
average **6.82 characters** (mean 6.824753159493136, 9,977 present, 23
absent) — both markedly *shorter* than the Tongues-era (v1) free-stem
figures Study 008 pinned (9.89 / 9.77). The 500-seed calibration pins
**6.689645317538036** (goblin) and **6.907515187810932** (kobold)
(`name_length_distributions_are_measured_and_pinned`), and the null
control's own name-length SMD — the goblin vs. goblin-twin gap over
`census-of-the-meeting` — moves from the Tongues-era **−0.118235** to
**−0.045750720221954** (`null_control_name_length_smd_is_pinned`), still
comfortably inside the ±0.2 sampling bound
`null_control_distributions_are_within_the_sampling_bound` asserts, so the
null control's own verdict (Study 009) is unaffected by this re-pin.

Shorter names from a smaller vocabulary is exactly the mechanism spec §9.5
names ("glossed compounds shrink the name space versus free stems"); what
the preregistered bound underestimated was the *degree* of shrinkage. A
settlement's glossed name is a compound over at most two site concepts —
its own biome and its species' single, world-wide presiding phenomenon
concept (near-constant across a species' own settlements within one
world, since `observed_phenomena_as_in`'s salience order does not vary by
settlement) — so the number of distinct `(biome, phenomenon)` pairs
available to a species in one world is small (bounded by the count of
distinct biomes that species actually settles, typically well under 20),
while a species can place well over 100 settlements in a large-habitable
world (`settlement-count`'s own bucket ceiling). Once settlement count
exceeds the distinct-pair count, the pigeonhole principle guarantees
collision, and empirically most worlds are well past that threshold: a
direct read of seed 42's own settlement names (via `hornvale new --seed 42`
+ a raw ledger dump) shows 59 settlement names collapsing to 26 distinct
strings, with the single most common name (`Ned`) appearing 9 times — a
concrete instance of the same mechanism the census measures in aggregate.

## Verdict: H1-H3 confirmed exactly; H4's directional claim failed, honestly reported

H1, H2, and H3 hold with zero exceptions across every present world in both
the 10,000-seed census and the 500-seed CI-guarded calibration — the
glossing and exposure-soundness invariants Task 10's keystone suite already
proved at seed 42 hold at population scale, and the pack-depth ordering is
the structural constant it was derived to be. H4 is a **reportable
scientific result, not a defect to silently patch within this task**: the
preregistered directional bound (below 4.678%) was wrong by roughly a
factor of 18 (measured ≈86.3-86.8%), because Task 9's glossed-compound
naming shrank the per-species name vocabulary far more severely than spec
§9.5 anticipated — a small, largely per-species-constant site-concept pool
compounded against settlement counts that regularly exceed it. Per ADR
0016 and this task's brief ("if a preregistered claim FAILS, that is a
reportable scientific result — STOP and report... do not adjust the
claim"), the bound is left as originally stated, the measured rate is
pinned exactly, and this page records the failure as the finding it is.
Whether the naming pipeline should draw on a richer site-concept vocabulary
(more than biome + one phenomenon), vary phenomenon choice per settlement,
or otherwise widen the name space is a product decision for a future task,
not this one — Task 12's scope is the Lab instrument: preregister,
instrument, run, pin, and report faithfully what the instrument measures.

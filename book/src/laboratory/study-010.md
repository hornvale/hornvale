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

**H4: the name-collision rate — measured twice; the directional claim
FAILED both times, first catastrophically, then narrowly.**

*First measurement (pure site-concept compounds, recorded as history).* At
10k, across the 9,997 present worlds, the mean in-world collision rate
read **86.77%** (mean 0.867749967402372), with only **13 of 9,997 worlds
(0.13%) showing zero collisions** — the inverse of the Tongues-era shape,
where 64.0% of worlds saw zero collisions and the mean sat at 2.79%. The
500-seed `census-lands-drift` population confirmed the same magnitude:
**2 of 500 worlds zero-collision, 498 nonzero, mean 86.28%**
(0.862829625681313). Dramatically above the preregistered directional
bound (below 4.678%, twice the Tongues-era pinned rate); companion
name-length means at that measurement: 6.51 / 6.82 characters at 10k
(6.6896 / 6.9075 at 500 seeds), both markedly *shorter* than the
Tongues-era free-stem figures (9.89 / 9.77) — a smaller, more-repeated
vocabulary of shorter compound words. The mechanism: a settlement's
glossed name was a compound over at most two site concepts — its own
biome and its species' single, world-wide presiding phenomenon concept
(near-constant across a species' own settlements within one world, since
`observed_phenomena_as_in`'s salience order does not vary by settlement) —
so the distinct `(biome, phenomenon)` pairs available to a species in one
world numbered well under 20, while a species can place well over 100
settlements; once settlement count exceeds the distinct-pair count, the
pigeonhole principle guarantees collision. Concretely, seed 42's 59
settlement names collapsed to 26 distinct strings, the most common
(`Ned`) appearing 9 times.

*The naming-defect fix.* The first measurement was ruled a **defect in the
naming design against spec §9's low-collision success criterion**, and
`Namer::glossed_name` was amended for `NameKind::Settlement` only: the
site-concept compound now also carries a per-salt **drawn stem** (1-2
template syllables from the same `/v2` stream, drawn after the
site-concept picks, filling the modifier slot opposite the site compound's
head — real-world toponymy's descriptor + unique element). The stem is a
proper-name element naming no concept: **glosses are unchanged and stay
truthful** (H1's row-by-row invariant re-confirmed below), deity and
epithet names are untouched, and phonotactic repair still applies to the
whole sequence.

*Post-fix measurement (current, pinned).* The full 10k census re-run reads
a mean in-world collision rate of **11.00%** (mean 0.110004053691488),
with **1,345 of 9,997 present worlds (13.5%) zero-collision**; seed 42's
59 settlements now carry 59 distinct names. The 500-seed
`census-lands-drift` population reads **57 of 500 worlds zero-collision,
443 nonzero, mean 10.71%** (0.107138846580572), pinned exactly in
`name_collision_rate_is_measured_and_pinned`. This is an **~8x
improvement over the defective first measurement, but still above the
preregistered directional bound** (4.678%): the claim, as stated,
**remains failed**, and per ADR 0016 the bound is not widened — the
measured rate is pinned as-is and the failure stands recorded.

The companion name-length distributions move with the fix: at 10k, goblin
names average **10.56 characters** (mean 10.564079354021732, 9,972
present, 28 absent) and kobold names **11.01** (mean 11.011578681459451,
9,977 present, 23 absent) — now *longer* than the Tongues-era free stems
(9.89 / 9.77), since a settlement name is a site word plus a drawn unique
stem. The 500-seed calibration pins **10.768460909737524** (goblin) and
**11.126229787576881** (kobold)
(`name_length_distributions_are_measured_and_pinned`), and the null
control's own name-length SMD — the goblin vs. goblin-twin gap over
`census-of-the-meeting` — reads **−0.050617434805643**
(`null_control_name_length_smd_is_pinned`; it was −0.118235 at the
Tongues-era measurement and −0.045751 at the pre-fix measurement), still
comfortably inside the ±0.2 sampling bound
`null_control_distributions_are_within_the_sampling_bound` asserts, so the
null control's own verdict (Study 009) is unaffected by this re-pin.

## Verdict: H1-H3 confirmed exactly; H4 failed, a defect was fixed, and the claim still fails — honestly reported

H1, H2, and H3 hold with zero exceptions across every present world in
both the 10,000-seed census (re-confirmed unchanged on the post-fix
re-run: 9,997/9,997 glosses true, 40,000/40,000 regularity/soundness
flags, hue-depth 4 vs. 2 at every seed) and the 500-seed CI-guarded
calibration — the glossing and exposure-soundness invariants Task 10's
keystone suite already proved at seed 42 hold at population scale, and
the pack-depth ordering is the structural constant it was derived to be.

H4's story has two honest parts. The first census exposed a genuine
**naming defect** — pure site-concept compounds gave a species only a
handful of distinct settlement names against settlement counts that
regularly exceed a hundred, an 86% collision regime nobody intended — and
the defect was fixed (the settlement stem above), cutting the measured
rate by roughly a factor of eight and restoring seed 42 to
collision-free. But the **preregistered claim itself still fails**: 10.71%
(500-seed) / 11.00% (10k) remains above the 4.678% bound, because even
with a unique element drawn per settlement, compounding it against a
small site-word vocabulary leaves the name space denser than the
Tongues-era free-stem pool the bound was anchored to. Per ADR 0016, the
bound is left as originally stated and the failure stands: whether the
residual rate is acceptable (a low-double-digit share of a world's
settlements sharing a name with a same-biome sibling reads differently
than 86% collapse), or whether the stem should widen further (2-3
syllables, per-settlement phenomenon variation), is a product decision
for the campaign owner, not a bound to quietly re-tune from inside the
Lab task that measured it.

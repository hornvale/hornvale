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

**A note on scale and provenance.** This chapter's headline numbers will
come from a 10,000-seed run of `studies/census-of-words.study.json`,
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

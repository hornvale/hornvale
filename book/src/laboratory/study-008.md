# Study 008: The Census of Tongues

Ten thousand worlds, unpinned — the same seeds 0 through 9,999 every prior
census has walked. Campaign Y2-3 gave every settlement and every deity a
generated name, drawn from a per-species phonology instead of the fixed
`Bolzag`/`Grix`/`Skree` syllable pool the earlier campaigns leaned on, and
gave every deity's epithet a status-basis-keyed voice: goblins mark rank
with a prepended honorific, kobolds mark knowledge with none. Per the
campaign spec's re-baseline clause (§8), committed artifacts and 10k
censuses shift exactly once more, after all code changes land, as the
campaign's final act before the book close — this is that shift, recorded.
Unlike Study 007's re-baseline, this one moves **every proper noun in every
prior artifact**: not a new fact alongside the old ones, but a wholesale
replacement of how names are drawn. That is the whole point of the
campaign, not a regression.

**A note on scale and provenance.** This chapter's headline numbers come
from a 10,000-seed run of `studies/census-of-tongues.study.json`, executed
once by hand at author time, in the same arrangement Studies 001, 002, 003,
006, and 007 use for their own 10,000-seed headline runs. It is **not**
committed and **not** part of CI — `.github/workflows/ci.yml` regenerates
only `census-lands-drift` (500 seeds) in its "Artifacts are current" step;
`census-of-skies`, `census-of-lands`, `census-of-peoples`, `census-of-faiths`,
and `census-of-eyes` are already author-time-only by the same precedent
(only `census-of-skies` commits its raw output, per the overview above), and
`census-of-tongues` joins them as author-time-only too. The drift-checked
sibling, `census-lands-drift`
(`book/src/laboratory/generated/census-lands-drift/`), now carries the same
per-species naming/voice metrics this study measures at 10k; small
differences between a number quoted here and this study's own count are
sampling variance between a 500- and a 10,000-seed draw, not drift — the two
pinned calibration rows below assert the 500-seed figures exactly.

**A note on preregistration.** Per ADR 0016, studies commit their
hypotheses before running the census that tests them. The three claims
below are quoted from §9 of the campaign spec
(`docs/superpowers/specs/2026-07-08-campaign-y2-3-the-tongues-design.md`),
first committed at `81ba9f6` on 2026-07-08 and refined twice the same day,
both refinements landing before this study's 10k census ran: `9e0c718`
sharpened the collision-rate claim from "pre-redraw" to "measured" (since
uniqueness is de-facto — Task 9's owner decision — there is no re-draw to
measure a rate *of*), and `1f1a4e7` added the epithet-honorific flag's
content-derived-cross-check detail (it reads committed epithet text and
detects the prepended affix structurally, not by echoing config) alongside
its own preregistered calibration tests in
`windows/lab/tests/calibration.rs`. The census below confirms all three
claims at scale; it does not discover them.

## Question

Does every generated name obey its own species' phonotactics exactly, does
the drawn name space stay nearly collision-free at scale despite having no
re-draw, and does each species' deity epithets carry the voice its status
basis predicts — visible structurally, from the committed text, not merely
asserted from configuration?

## Hypotheses

Quoted from spec §9, "The Lab" (preregistered 2026-07-08, before any census
ran, per ADR 0016):

**H1 (phonotactic validity, exact).** "Phonotactic validity — 100% of
generated names well-formed for their language, row-by-row over the census
(the instrument reproducing its own grammar exactly)."

**H2 (low collision rate, measured).** "Low collision rate (measured) —
names are pure per-`(seed, species, kind, salt)` draws with no re-draw; the
drawn name space is combinatorially large, so the fraction of names that
collide in-world is small. That fraction is measured and pinned as a
calibration row (contrast the 10-syllable pool's ~1,100 names) — it is a
reported property of the large name space, not a re-draw the engine
performs."

**H3 (voice/morphology calibration, exact and directional).**
"Voice/morphology calibration keyed to status basis — goblin deity epithets
carry an honorific affix; kobold epithets never do — asserted row-by-row
from independent metric columns, like The Eyes' head-domain calibration.
The `epithet-honorific` flag is a content-derived cross-check, not a config
echo: it reads each committed epithet fact and detects the prepended affix
STRUCTURALLY against an independently re-derived honorific-OFF stem
(case-folded, the committed epithet must end with the plain stem and be
strictly longer). A broken honorific pipeline — a goblin epithet committed
without its affix — would match the plain stem and flip the flag to false,
so the calibration is genuinely falsifiable."

## Analysis plan

Measure, over the full 10,000-seed `census-of-tongues` run: per-species
phonotactic validity (`phonotactic-validity-goblin` /
`phonotactic-validity-kobold`), row-by-row, expecting zero exceptions (H1);
the in-world name-collision rate (`name-collision-rate`, settlement and
deity proper nouns only — epithets deliberately excluded as descriptive
words expected to repeat by design), its distribution across the census,
and the fraction of worlds with zero collisions (H2); per-species
epithet-honorific flags (`epithet-honorific-goblin` /
`epithet-honorific-kobold`), row-by-row (H3); and, beyond the three
preregistered claims, the per-species mean generated-name length
(`name-length-goblin` / `name-length-kobold`) as the naming/voice baseline's
descriptive complement — spec §9 names this a new metric this census exists
to establish, not a hypothesis to verdict.

**Verdict criteria**, fixed before analysis: H1 is confirmed if
phonotactic validity is true in 100% of present cases for both species —
an exact-count assertion, already load-bearing in
`windows/lab/tests/calibration.rs`'s
`phonotactic_validity_is_true_for_every_generated_name`, which `cargo test`
asserts on every build (any failure is a STOP-and-report-BLOCKED condition
per the task brief, never an assertion to loosen). H2 is confirmed by
measuring — there is no preregistered floor to clear, only a rate to record
and pin as an exact calibration row over the 500-seed drift study. H3 is
confirmed if the goblin flag reads true and the kobold flag reads false in
100% of present cases, matching the already-preregistered
`epithet_honorific_is_true_for_goblin_and_false_for_kobold` test. A verdict
of **defect** would require H1 or H3 to fail at 10k after passing at 500
seeds, or H2's measured rate to be large enough to suggest the drawn name
space is not, in fact, combinatorially large — no such case is anticipated,
since both exact claims are structural invariants the campaign spec (§8)
elevates to the identity contract's replacement for byte-identity.

## Results

**H1: phonotactic validity, at 10k.** Every one of the 9,972 present goblin
name-sets and every one of the 9,977 present kobold name-sets re-validates
against its own species' independently re-derived, re-parsed phonotactics —
**100.0%, zero exceptions, both species** (`phonotactic-validity-goblin`:
9,972/9,972 true; `phonotactic-validity-kobold`: 9,977/9,977 true). The
28- and 23-world absences are the same zero-settlement and
habitable-land-exhaustion worlds Study 006 and Study 007 already attribute
per species — nothing new about naming causes them. **H1 holds exactly at
10k**: the instrument reproduces its own grammar with no exceptions across
19,949 name-sets.

**H2: the collision rate, measured, at 10k.** Across the 9,997 worlds with
at least one settlement or deity name (the same 3 zero-habitable-fraction
worlds Studies 002, 003, 006, and 007 already name — seeds 895, 1009, and
4322 — hold neither species' settlements nor a pantheon and are excluded),
the mean in-world collision rate is **2.79%** (duplicated names as a share
of all settlement + deity names in that world), with a median of exactly
**0.0%** — most worlds draw no colliding name at all. **6,398 of 9,997
worlds (64.0%) show zero collisions**; the distribution has a long, thin
tail (2.0% of worlds sit at 20–40% collision, 0.4% above 40%, up to a
single worst-case world at 71.3%) rather than a uniform spread, meaning
collisions cluster in a small minority of worlds — almost certainly the
worlds with the largest settlement counts, where the same per-`(seed,
species, kind, salt)` draw space is sampled the most times. This is the
name space behaving exactly as spec §9.2 predicts: combinatorially large
enough that most worlds see no collision, but not infinite, so a bounded
minority do — a sharp contrast with the pre-Tongues 10-syllable pool's
~1,100-name ceiling, where collision (the "Bolzag ×3" era the campaign's
success criteria name explicitly) was closer to the rule than the
exception at this population scale. **H2 is measured, not verdicted** —
spec §9 preregisters only that the rate be small and pinned, not a specific
threshold — and the pinned calibration below fixes the 500-seed figure
(`windows/lab/tests/calibration.rs::name_collision_rate_is_measured_and_pinned`)
as the exact number `cargo test` now guards.

**H3: the voice/morphology calibration, at 10k.** Every one of the 9,972
present goblin epithets carries its prepended honorific affix — **100.0%,
zero exceptions** (`epithet-honorific-goblin`: 9,972/9,972 true) — and
every one of the 9,977 present kobold epithets carries none — **100.0%,
zero exceptions** (`epithet-honorific-kobold`: 0/9,977 true, i.e. false in
every present case). Both figures reproduce the directional preregistration
exactly, and because the flag is a content-derived cross-check rather than
a config echo (it re-derives the honorific-OFF plain stem independently and
tests whether the committed epithet text structurally extends it), a
pipeline bug that silently dropped the honorific affix from a goblin
epithet would flip this reading to false rather than pass by construction —
the calibration is falsifiable and it did not fail. **H3 holds exactly at
10k.**

**The naming/voice baseline (descriptive, not a hypothesis).** Goblin
generated names average **9.89 characters** (median 9.82, σ 2.28, range
4.7–20.0, n = 9,972); kobold generated names average **9.77 characters**
(median 9.67, σ 2.32, range 4.3–19.4, n = 9,977) — nearly identical central
tendency and spread, with goblin names carrying a marginally heavier long
tail (46.9% of goblin names are ≥ 10 characters vs. 44.8% of kobold names).
Neither species' phonology is dramatically more verbose than the other's;
the two languages differ in *mouth-feel* (sibilance and labiality
signature, per the almanac and the `phonology` reference page) rather than
raw length. This baseline is recorded here per spec §9's instruction that
name-length distribution is a new metric this census establishes, not a
directional claim to verdict.

## Verdict: all three preregistered claims confirmed at 10k

H1 and H3 both hold exactly, with zero exceptions across 19,949 and 19,949
name-sets respectively — the instrument reproduces its own grammar and its
own voice-morphology rule with no drift from the 500-seed calibration. H2,
which spec §9 deliberately preregisters as measured rather than
threshold-bound, comes in at a mean 2.79% collision rate with a 64.0%
zero-collision majority — small in the sense the spec anticipates, and now
pinned as an exact figure over the CI-guarded 500-seed population rather
than left as an unpinned observation. No defect: the naming/voice baseline
this study exists to establish reproduces cleanly from the 500-seed drift
study to the 10,000-seed census, and the two exact calibrations
(`windows/lab/tests/calibration.rs`) hold `cargo test` to the same bar on
every future build.

## The anti-reskin phonology echo: deferred

Spec §9 names an optional anti-reskin echo — attributing a name to its
species from phonology alone (the sibilance/labiality signature), the
blind-attribution pattern (Study 007, H2) applied to sound instead of
pantheon structure — as "defined here, may run in The Meeting." This
campaign does not build that attribution rule; `is_phonotactically_valid`
and the phonology reference page already expose everything a future rule
would need (per-species inventory, onset/coda templates), but authoring and
calibrating the rule itself is out of scope here, exactly as spec §9
declines to preregister it as a Y2-3 deliverable. It is scoped to Campaign
4, The Meeting, alongside Study 007's distributional-twin control.

## Pinned calibration rows

Measured over the 500-seed `census-lands-drift` study (the CI-guarded,
exact-count population; `windows/lab/tests/calibration.rs`), at the Y2-3
re-baseline:

- `name_collision_rate_is_measured_and_pinned`: of the 500 worlds, **336
  show zero name collisions and 164 show at least one** (0 absent — the
  500-seed sample happens to include no zero-habitable-fraction world),
  with a mean measured rate of **2.339%** across all 500 — the same shape
  of "small but not zero" figure the 10k run confirms (2.79%), the
  difference being ordinary 500-vs-10,000-seed sampling variance.
- `name_length_distributions_are_measured_and_pinned`: mean generated-name
  length is **9.8693 characters** for goblins and **9.7995 characters**
  for kobolds, each over 498 present worlds (2 absent, the same
  goblin/kobold placement absences the other 500-seed calibrations already
  name) — consistent with the 10k figures (9.89 / 9.77) within ordinary
  sampling variance.

Phonotactic validity and the epithet-honorific split are pinned as
**exact invariants**, not measured rates, in the already-existing
`phonotactic_validity_is_true_for_every_generated_name` and
`epithet_honorific_is_true_for_goblin_and_false_for_kobold` tests — both
preregistered ahead of this study (Task 12) and reconfirmed, not
re-pinned, by this task's 10k run: 100% validity, goblin honorific always
on, kobold honorific always off, with zero exceptions across the full
census. These are the same shape of exact-count pin the Y2-1 slave-rung
and structural-ceiling calibrations use (Study 006, H2/H3) and Y2-2's
head-domain and blind-attribution calibrations use (Study 007): a
deterministic population asserted row-by-row (or aggregate-by-row) in CI,
not a threshold tuned to make a number look good.

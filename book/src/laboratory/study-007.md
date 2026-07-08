# Study 007: The Census of Eyes

Ten thousand worlds, unpinned — the same seeds 0 through 9,999 every prior
census has walked, each now carrying **two** pantheons instead of one.
Campaign Y2-2 gave every species its own perception vector, its own
observation hour, and its own religion run: the goblins keep the sun they
have always kept, and the kobolds — nocturnal, night-sighted, sky-attentive
— get their own eyes for the first time. Per the campaign spec's
re-baseline clause (§7), committed artifacts and 10k censuses shift exactly
once more, after all code changes land, as the campaign's final act before
the book close. This is that shift, recorded.

**A note on scale and provenance.** This chapter's headline numbers come
from a 10,000-seed run of `studies/census-of-eyes.study.json`, executed
once by hand at author time, in the same arrangement Studies 001, 002, 003,
and 006 use for their own 10,000-seed headline runs. It is **not**
committed and **not** part of CI — `.github/workflows/ci.yml` regenerates
only `census-lands-drift` (500 seeds) in its "Artifacts are current" step;
`census-of-faiths`, `census-of-lands`, `census-of-peoples`, and
`census-of-skies` are already author-time-only by the same precedent, and
`census-of-eyes` joins them. The drift-checked sibling,
`census-lands-drift` (`book/src/laboratory/generated/census-lands-drift/`),
now carries the same per-species eyes metrics this study measures at 10k;
small differences between a number quoted here and a bar in that chart are
sampling variance between a 500- and a 10,000-seed draw, not drift.

**A note on preregistration.** Per ADR 0016, studies commit their
hypotheses before running the census that tests them. The two claims below
are quoted verbatim from §9 of the campaign spec
(`docs/superpowers/specs/2026-07-07-campaign-y2-2-the-eyes-design.md`),
committed at `a27106e` on 2026-07-07 and untouched since. The blind-
attribution rule itself was implemented and its first measurement taken on
2026-07-08, against the 500-seed `census-lands-drift` study, before this
10k run — the owner decision recorded that day (pin the honest rate rather
than retune the rule) is folded into both the pinned calibration rows below
and this chapter's account of the moonless-pair finding. The census below
confirms both preregistered claims at scale; it does not discover them.

## Question

Do two peoples on one sky keep different gods, and is the difference
structural rather than lexical — visible even after every name, epithet,
and tenet is stripped away?

## Hypotheses

Quoted verbatim from spec §9, "The Lab" (preregistered 2026-07-07, before
any census ran, per ADR 0016):

**H1 (head-deity domain, directional and partly exact).** "Directional
preregistration: the goblin head is always solar (the sun's raw 1.0 is
untouchable under the identity lens); the kobold head is lunar on **every
world with at least one moon** (a moon's raw ≥ 0.35 × 1.82 always beats the
sun × 0.52). On moonless worlds the kobold head splits between night-stars
and the sun by star brightness — deliberately *not* preregistered as
'never solar' (a moonless world with faint stars legitimately yields a
sun-headed kobold pantheon); the split is pinned as calibration rows after
the model is fixed (Y2-1 practice)."

**H2 (blind attribution, directional).** "Preregistered, directional:
attribution accuracy well above chance on the real vectors; the exact
threshold is pinned as a calibration row after measurement. The
**distributional-twin control** (a second species carrying the goblins'
exact vectors must score at chance) is defined by this metric and *runs*
with The Meeting's comparative studies."

## Analysis plan

Measure, over the full 10,000-seed `census-of-eyes` run: goblin and kobold
head-deity domain (`head-deity-domain-goblin` / `head-deity-domain-kobold`)
cross-tabulated by activity cycle (fixed per species: goblin Diurnal,
kobold Nocturnal), tidal lock (`tidally-locked`), and mooned-ness
(`moons-admitted` ≠ 0) — the calibration matrix spec §9.1 names explicitly
(H1); blind-attribution accuracy (`blind-attribution-correct`) overall and
split by the same mooned/moonless boundary the 500-seed drift study's
Task 10 measurement already flagged as the entire source of the
preregistered floor's miss (H2). Beyond the two preregistered claims, this
study also records the two-pantheon baseline itself — per-species pantheon
size and cult form — since spec §9 names these as the new metrics this
census exists to establish. The distributional-twin control that H2 defines
is **not** run here: it requires a second species carrying the goblins'
exact perception vector, which this campaign does not author (no third
species exists); it is deferred to The Meeting (Campaign 4), exactly as
spec §9 and §13 say.

**Verdict criteria**, fixed before analysis: H1 is confirmed if the goblin
head is solar in 100% of present cases and the kobold head is lunar in
100% of mooned cases, matching the 500-seed drift calibration exactly (both
are exact-count assertions in `windows/lab/tests/calibration.rs`, not
merely directional at this scale). H2 is confirmed if blind-attribution
accuracy at 10k is "well above chance" (a two-outcome guess, so chance is
50%) and consistent within ordinary sampling variance with the 500-seed
figure already pinned. A verdict of **defect** would require either claim
to fail at 10k after passing at 500 seeds — no such case is anticipated,
since H1's exact claims are load-bearing calibrations `cargo test` already
asserts on every build, and H2's honest, sub-floor rate was already
resolved by owner decision before this task began (Task 10, 2026-07-08).

## Results

**H1: the head-deity-domain matrix, at 10k.** Every one of the 9,972
present goblin flagships heads a solar pantheon — **100.0%, zero
exceptions** — confirming the identity-lens claim exactly. The kobold head
splits cleanly on mooned-ness, exactly as preregistered:

| Species | Activity | Locked | Mooned | Solar | Lunar | Total |
|---|---|---|---|---|---|---|
| goblin | Diurnal | false | false | 1,418 | 0 | 1,418 |
| goblin | Diurnal | false | true | 8,097 | 0 | 8,097 |
| goblin | Diurnal | true | false | 67 | 0 | 67 |
| goblin | Diurnal | true | true | 390 | 0 | 390 |
| kobold | Nocturnal | false | false | 1,282 | 138 | 1,420 |
| kobold | Nocturnal | false | true | 0 | 8,100 | 8,100 |
| kobold | Nocturnal | true | false | 62 | 5 | 67 |
| kobold | Nocturnal | true | true | 0 | 390 | 390 |

Collapsing the lock axis: on every one of the 8,490 mooned worlds (8,100
unlocked + 390 locked), the kobold head is lunar — **8,490/8,490, 100.0%**,
the exact claim spec §9.1 preregisters. On the 1,487 moonless worlds
(1,420 unlocked + 67 locked), the split is **1,344 solar (90.4%) / 143
lunar (9.6%)** — the sun wins most moonless skies, but a bright-enough
night-star still outshines it in roughly one moonless world in ten. Lock
state barely moves the moonless split (90.3% solar unlocked vs. 92.5%
solar locked) — consistent with spec §5's account of why: a locked sky
makes nocturnality moot for the characteristic hour, but not for which
phenomenon is brightest once observed. **H1 holds exactly at 10k**, both
the directional half (moonless split, now measured and pinned) and the
exact half (mooned kobold heads always lunar, goblin heads always solar).

**H2: blind attribution, at 10k, and the honest moonless story.** Overall
accuracy over the 9,952 worlds where both species hold a full pantheon
signature: **8,623/9,952 correct — 86.65%**, consistent within ordinary
500-vs-10,000-seed sampling variance with the pinned 500-seed figure of
87.5% (434/496). Both figures clear the spec's directional bar ("well
above chance," i.e., above 50%) by a wide margin, and both sit measurably
below the plan's original preregistered floor of 0.9 — the same honest gap
Task 10 discovered and the owner resolved by pinning the measured rate
rather than retuning the rule (2026-07-08). Splitting by mooned-ness shows
the gap is not spread across the population; it is **entirely** the
moonless pairs, and it is worse at 10k than the 500-seed sample suggested:

| Population | Correct | Total | Accuracy |
|---|---|---|---|
| All attributable pairs | 8,623 | 9,952 | 86.65% |
| Mooned pairs | 8,468 | 8,468 | **100.0%** |
| Moonless pairs | 155 | 1,484 | **10.44%** |

Restricted to mooned worlds, the fixed rule is perfect — Rule 1 (lunar
head) fires on every mooned pair and is never wrong, because H1's mooned
claim is itself exact. The entire miss lives in the 1,484 moonless pairs,
and there the rule does not merely underperform chance — it is
**systematically backwards**, exactly as Task 10's 500-seed diagnostic
found (10.44% correct at 10k vs. 13.9% at 500 seeds, both far below the
50% a coin flip would clear). The mechanism, unchanged from Task 10:
without a moon, the kobold's boosted night-sky weight has fewer phenomena
to work with, not more, so moonless-kobold pantheons tend to be *smaller
and less cyclic* than the goblin's daytime pantheon — the opposite of what
the rule's fallback tiers (cyclic-share, then size) assume. Night-stars are
eternal (`period_days: None`), so on a moonless sky the goblin's cyclic
seasonal deity routinely out-cycles the kobold's, inverting the tier-2
heuristic; this is the same "night-stars are eternal" inversion recorded
in `windows/lab/tests/calibration.rs`. **H2 holds** on its actual
preregistered claim (decisively above chance, restricted-to-mooned exactly
1.0) and the owner's resolution — keep the rule, pin the honest rate — is
confirmed rather than revisited: the 10k population does not move the
verdict, it only sharpens the moonless number from Task 10's 500-seed
estimate.

**The two-pantheon baseline.** Goblin pantheons average **3.05 deities**
(median 3, σ 1.07, range 1–7, n = 9,972); kobold pantheons average **3.00**
(median 3, σ 1.12, range 1–7, n = 9,977) — nearly identical central
tendency, with the kobold distribution carrying a slightly heavier tail
(pantheons of 6–7 deities are more common among kobolds: 167/9,977, 1.7%,
vs. goblins' 101/9,972, 1.0%). Cult form diverges more: goblin pantheons
are organized in **82.5%** of worlds (folk in 17.5%), kobold pantheons
organized in **85.9%** (folk in 14.1%) — kobolds lean measurably more
toward organized cults, consistent with the communal, ranked-by-knowledge
psychology vector Campaign Y2-1 authored for them. Both species hold a
pantheon in the overwhelming majority of worlds (goblin 9,972/10,000,
kobold 9,977/10,000 — the same 28- and 23-world absences Study 006 already
attributes to zero-settlement and habitable-land-exhaustion worlds), and
the intersection where both hold a full comparable signature is 9,952 —
consistent with 9,972 + 9,977 − 9,997, where 9,997 is the union and the
missing 3 are the same zero-habitable-fraction worlds Studies 002, 003,
and 006 already name (seeds 895, 1009, 4322).

## The distributional-twin control: defined here, run in The Meeting

Spec §9.2 defines the distributional-twin control as part of this metric —
a second species carrying the goblins' *exact* perception vector should
score at chance under the same blind-attribution rule, proving the rule
distinguishes structure and not an accidental correlate of "which species
generates second." This campaign authors no such twin species (only goblin
and kobold exist), so the control cannot run yet; `pantheon_sig`'s `ranked`
field is left in place specifically because it, or the twin-control
harness, is its evident next consumer (Task 10's report already flags
this). The control is scoped to Campaign 4, The Meeting, where comparative
studies over the two pantheons are the point.

## Verdict: both preregistered claims confirmed at 10k

H1 and H2 both hold at the campaign's full scale. H1's directional half
(the moonless split) is now measured and pinned as an exact calibration
row rather than left open; its exact half (mooned kobold heads always
lunar, goblin heads always solar) reproduces with zero exceptions across
9,972 and 8,490 present/mooned cases respectively — no defect, no retune.
H2's honest rate — decisively above chance overall, exactly 1.0 restricted
to the population the rule was actually designed for, and a measured,
explained failure mode on the population spec §9.1 explicitly declined to
preregister for the domain calibration — is the campaign's headline
finding: not that blind attribution "works," but that it works *for a
specific, nameable reason* (a moon supplies the extra, more-cyclic deities
the fallback tiers assume exist) and fails for an equally nameable one
(night-stars are eternal, so a moonless sky's cyclic-share signal points
the wrong way). Both are recorded here as the anti-reskin metric's honest
first calibration, not filed as an open question — the preregistered rule
stays untouched by owner decision, and the 10k census sharpens the
500-seed estimate without moving the verdict.

## Pinned calibration rows

Measured over the 500-seed `census-lands-drift` study (the CI-guarded,
exact-count population; `windows/lab/tests/calibration.rs`), at the Y2-2
re-baseline:

- `blind_attribution_beats_chance_decisively`: **434 correct / 496 total**
  attributable pairs (accuracy 0.875, already pinned as a floor; now also
  pinned as an exact count). Restricted to mooned pairs: **424/424**
  correct (100.0%, pinned as an equality invariant rather than a specific
  count, since it holds for whatever the mooned population size is).
- `goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar`: the
  moonless kobold head split, previously recorded but not asserted, is now
  pinned exactly — **62 solar / 10 lunar** (72 moonless kobold-present
  worlds in the 500-seed sample).

These are the same shape of exact-count pin the Y2-1 slave-rung and
structural-ceiling calibrations use (Study 006, H2/H3): a 500-seed
deterministic population asserted row-by-row in CI, not a threshold tuned
to make a number look good.

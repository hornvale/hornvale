# Study 006: The Census of Peoples II: Two Peoples

Ten thousand worlds, unpinned — the same seeds 0 through 9,999 every prior
census has walked, each now peopled by two species instead of one. Campaign
Y2-1 added the goblin/kobold species substrate, joint greedy placement, and
species-modulated culture, and per §8's re-baseline clause committed
artifacts and 10k censuses shift exactly once more, after all code changes
land, as the campaign's final act before the book close. This is that
shift, recorded.

**A note on scale and provenance.** This chapter's headline numbers come
from a 10,000-seed run of `studies/census-of-peoples.study.json`, executed
once by hand at author time, in the same arrangement Study 003 used for the
single-species baseline. It is **not** committed and **not** part of CI.
The drift-checked sibling, `census-lands-drift` (500 seeds,
`book/src/laboratory/generated/census-lands-drift/`), now carries the same
per-species metrics this study measures at 10k; small differences between a
number quoted here and a bar in that chart are sampling variance between a
500- and a 10,000-seed draw, not drift.

**A note on preregistration.** Per ADR 0016, studies commit their
hypotheses before running the census that tests them. The three hypotheses
below are quoted verbatim from §9 of the campaign spec
(`docs/superpowers/specs/2026-07-07-campaign-y2-1-the-peoples-design.md`),
committed at `6fd072e` — the commit that introduced the species substrate
design and has not been touched since (`33f960e`, the only other commit to
touch that file, edited §8, not §9). All three were pinned as exact
row-by-row calibrations in `windows/lab/tests/calibration.rs` well before
this 10k run — the census below confirms the pinned rule holds at scale, it
does not discover it.

## Question

Do the three properties preregistered before the species substrate landed —
kobold flagships being less coastal than goblin's, an exact slave-rung
rule, and a structural ceiling difference between the two peoples — hold at
the campaign's full 10,000-seed scale, and what does two-species placement
reveal that no hypothesis anticipated?

## Hypotheses

Quoted verbatim from spec §9, "The Lab" (preregistered before any census
ran, per ADR 0016):

**H1 (coastal, directional).** "Kobold flagships are less coastal than
goblin flagships (directional; the exact rates are pinned as calibration
rows after measurement — the goblin rate should match Y2-0's 99.3% in
goblin-only worlds)."

**H2 (slave rung, exact).** "Slave rung ⇔ (`status_basis = Rank` ∧ surplus
> 0.6 ∧ population > 300) — the sixth exact calibration, asserted row-by-row
over the 500-seed drift study from independent metric columns."

**H3 (structural ceiling, exact).** "Kobold structures never contain
'slave' and always top out at 'elders'; goblin structures top out at
'chief'."

## Analysis plan

Measure, over the full 10,000-seed `census-of-peoples` run: goblin and
kobold flagship coastal rates from the `goblin-flagship-coastal` /
`kobold-flagship-coastal` columns (H1); a row-by-row re-derivation of the
slave rung from `goblin-flagship-surplus` and `goblin-flagship-population`
against `goblin-flagship-roles`, independent of the calibration test's own
assertion (H2); the full set of distinct role-ladder shapes and their top
rungs for both species (H3). Beyond the three preregistered hypotheses,
because two species now compete for the same sites, this study also
measures what the preregistration did not ask for: settlement-count
distributions per species (the sequel to Study 003's single-species
"first unknown number"), and **exclusion in both directions** — worlds
where one species places zero settlements while land exists for the
other — not only the kobold-excluded direction the 500-seed drift study's
seeds 172 and 257 already flagged as an unauthored emergent phenomenon.
Python's `csv` module is used throughout, not `awk`, because
`goblin-flagship-roles` and `kobold-flagship-roles` are comma-joined and
RFC-4180-quoted.

**Verdict criteria**, fixed before analysis: each hypothesis is confirmed
if the 10k measurement matches the pinned calibration's shape (directional
for H1, exact zero-mismatch for H2 and H3) within ordinary sampling
variance from the 500-seed drift figures already on record. A verdict of
**defect** would require a hypothesis to fail at 10k after passing at 500
seeds — no such case is anticipated, since all three are load-bearing
calibrations `cargo test` already asserts exactly on every build; this
census is confirmatory, not exploratory, for H1–H3, and exploratory only
for the settlement-count and exclusion questions Study 003's method
inherits.

## Results

**H1: coastal rates.** Goblin flagships are coastal in **9,972 of 9,972**
present cases — **100.0%**, with zero inland goblin flagships anywhere in
the 10,000-seed census. Kobold flagships are coastal in **8,512 of 9,977**
present cases — **85.3%** (1,465 inland, 23 worlds where kobolds place no
settlement at all). H1 holds: kobolds are measurably less coastal than
goblins, consistent with the 500-seed drift figures already pinned (goblin
100.0%, kobold 85.7%) within ordinary 500-vs-10,000-seed sampling
variance. The goblin rate itself is a genuine shift from Study 003's
post-Campaign-Y2-0, pre-species figure of 99.3% (9,931 of 9,997 coastal, 66
inland) — see "Two peoples, zero inland goblins" below.

**H2: the slave-rung rule, at 10k.** Re-deriving the rule independently
from `goblin-flagship-surplus` and `goblin-flagship-population` against the
literal presence of `"slave"` in `goblin-flagship-roles`: of 9,972 goblin
flagships checked, **6,851 have a slave rung and 6,851 satisfy
surplus > 0.6 ∧ population > 300 — zero mismatches in either direction**.
H2 holds exactly at 10k, the same exactness `windows/lab/tests/
calibration.rs` already asserts on every CI build over the 500-seed
sample.

**H3: the structural ceiling.** Across every kobold flagship in the census
(9,977 present, five distinct role-ladder shapes — `digger,elders`;
`digger,shaper,keeper,elders`; `digger,warden,elders`;
`digger,warden,shaper,keeper,elders`; and one `digger,keeper,elders`),
`"slave"` appears in **zero** of them, and the top rung is `"elders"` in
**all 9,977**. Across every goblin flagship (9,972 present, seven distinct
shapes), the top rung is `"chief"` in **all 9,972**, with zero exceptions.
H3 holds exactly, both directions, at 10k.

**Kobold settlement-count, the sequel's own unknown number.** Study 003
measured settlement count for a single, undifferentiated species; now that
two peoples compete for sites, the number splits. **Kobolds: mean 26.8,
median 25.0, standard deviation 13.5, range 0–85. Goblins: mean 31.6,
median 31.0, standard deviation 15.6, range 0–93.** Goblins place
systematically more settlements than kobolds on the same worlds — the
joint placement's suitability weighting still favors goblin-preferred
sites more often than not — while the two totals sum close to, but not
identically onto, Study 003's single-species figure: **total settlement
count is now 58.4 (median 60, standard deviation 22.4, range 0–124)**,
against Study 003's pre-species 56.7. Mean population per world moves
similarly, 289.2 → **294.9** (median 292.4, standard deviation 28.7, range
172.0–455.5). Habitable fraction still predicts total settlement count as
strongly as before (**+0.90**, materially unchanged from Study 003's
+0.89).

**Role-ladder distributions.** Goblin flagship role ladders, all 9,972
present:

| Ladder | Worlds | Share |
|---|---|---|
| slave, farmer, artisan, shaman, chief | 6,851 | 68.7% |
| fisher, chief | 1,412 | 14.2% |
| farmer, shaman, chief | 880 | 8.8% |
| farmer, artisan, shaman, chief | 498 | 5.0% |
| farmer, chief | 328 | 3.3% |
| fisher, warrior, chief | 2 | 0.02% |
| farmer, warrior, chief | 1 | 0.01% |

Kobold flagship role ladders, all 9,977 present:

| Ladder | Worlds | Share |
|---|---|---|
| digger, shaper, keeper, elders | 7,593 | 76.1% |
| digger, elders | 1,260 | 12.6% |
| digger, warden, shaper, keeper, elders | 981 | 9.8% |
| digger, warden, elders | 142 | 1.4% |
| digger, keeper, elders | 1 | 0.01% |

Kobold structures cluster harder around a single dominant shape (76.1% in
one ladder, versus goblins' 68.7% in their dominant ladder) and never grow
past five rungs, consistent with the communal, non-ranked status basis the
spec authors for the kobold psychology vector. The three-in-ten-thousand
`fisher, warrior, chief` / `farmer, warrior, chief` goblin ladders are not
a species effect at all: `"warrior"` is a threat-triggered rung
(`env.threat > 0.4`) that has existed unmodified since Campaign 4b's
original structure model — it was always a legal output, but Study 003's
`flagship-structure-size` metric only ever counted rungs, never named
them, so a warrior-bearing four-rung ladder was indistinguishable from any
other four-rung ladder until Task 9 of this campaign added the literal
role-ladder Text metric. The census makes a decade-old rare case visible
for the first time; it does not create it.

## The unasked-for finding: exclusion runs both ways

Neither H1 nor H3 predicts total exclusion — a world where one species
places every viable settlement and the other places none. The 500-seed
`census-lands-drift` sample already flagged two such worlds, seeds 172 and
257, both kobolds-win-all, and Study 003-era analysis treated them as a
curiosity of the goblin coastal-split calibration (the two "absent" cases
in `goblin_flagship_coastal_split_is_pinned`). The 10k census puts a number
on the phenomenon and finds it is **not one-directional**.

Separating the three worlds where *neither* species places a settlement
(seeds 895, 1009, 4322 — the same zero- and near-zero-habitable-fraction
worlds Study 002 and Study 003 already identify, where there is no
land to compete for) from worlds where habitable land exists but only one
species claims it:

- **Kobolds win every settlement, goblins place zero: 25 of 10,000 worlds**
  (0.25%) — seeds 172, 257, 775, 1079, 1206, 1213, 1446, 1549, 2265, 2339,
  3247, 3604, 4207, 4393, 5097, 5214, 5671, 7146, 7421, 8514, 8875, 9138,
  9456, 9554, and 9635.
- **Goblins win every settlement, kobolds place zero: 20 of 10,000 worlds**
  (0.20%) — seeds 187, 375, 531, 1709, 1929, 2123, 3961, 5783, 6332, 6369,
  6410, 6738, 6849, 7483, 7821, 9103, 9298, 9389, 9628, and 9986.

The two directions are close in rate (0.25% vs. 0.20%) and the seed sets
are disjoint, as they must be. More strikingly: **two of the
goblin-wins-all seeds, 187 and 375, are already present in the 500-seed
drift sample** — this is not a phenomenon that needed 10,000 seeds to
exist, only 10,000 seeds (or a differently aimed question) to *notice*.
The existing pinned calibration, `goblin_flagship_coastal_split_is_pinned`,
only asserts on `goblin-flagship-coastal` absence, which detects the
kobold-wins-all direction (goblin flagship missing) but has no symmetric
counterpart checking `kobold-flagship-coastal` absence — so a world where
kobolds are the excluded species has never failed a test, because nothing
has ever looked. This is not a defect in the calibration test, which does
exactly what it was written to assert; it is an honest gap in coverage
that this study's broader question — measure exclusion both ways —
happens to close. Total competitive exclusion remains an unauthored
emergent phenomenon of the joint placement algorithm, exactly as Task 9
described it, but it is now known to run in both directions at
comparable, non-negligible rates, not as a peculiarity of the kobold side
alone.

## Two peoples, zero inland goblins

Study 003's central post-Campaign-Y2-0 finding was that removing the
coastal-freshwater conflation let 66 goblin flagships (0.7%) place
inland, and that this was exactly where herding (4 worlds) and foraging
(12 worlds) finally surfaced as observed subsistence modes. At the
two-species re-baseline, that finding **fully reverses**: every one of the
9,972 goblin flagships in this census is coastal, and the goblin
subsistence split is 8,558 farming (85.8%) and 1,414 fishing (14.2%) —
**zero foraging, zero herding**. The mechanism is not a code change to
placement's suitability formula; it is competition. Kobolds are drawn with
an inland trend (their in-group and status-basis dimensions favor sites
goblins already found marginal before Y2-1), so joint greedy placement now
routes kobolds toward the inland sites goblin flagships occasionally won
in the single-species Study 003 sample, leaving goblins the coastal sites
almost without exception. Herding and foraging did not stop being legal
outputs of `subsistence(BiomeClass, coastal)` — they simply have no goblin
flagship left to attach to, because the flagships that would have needed
them are now kobold's. `flagship-structure-size` shifts to match: 17.4% /
8.9% / 5.0% / 68.7% for sizes 2 through 5 (up from 15.2% / 7.4% / 4.5% /
72.7%), and size 6 — reached by 0.1% of worlds pre-species — is not
observed at all in this census, because the rare herding/foraging-plus-
warrior combinations that could produce it no longer occur on the goblin
side.

## Verdict: all three preregistered hypotheses confirmed at 10k

H1, H2, and H3 all hold at the campaign's full scale, matching the shape
the 500-seed drift calibrations already pin exactly (H2, H3) or
directionally (H1) — no defect, no retune, no surprise in the
preregistration itself. The surprises this census turns up sit entirely
outside what was preregistered: competitive exclusion is symmetric, not a
kobold-side peculiarity, and it was partly visible at 500 seeds all along,
just never checked from both sides; and the two-species re-baseline erases
Study 003's own headline finding about inland goblin flagships, not
through any change to the subsistence function but through the
species-competition dynamics H1 was written to measure in the first
place. Both are recorded here rather than filed as follow-ups, because
both are direct, first-order consequences of exactly the mechanism this
campaign shipped — not open questions for a future one.

# Campaign Y2-0: Firm Ground

**July 2026 · 11 commits · outcome: complete, merged — Year 2 opens by
fixing the one known placement degeneracy and re-baselining Year 1's
numbers exactly once**

## What was attempted

Year 1 closed with all five domains at tier 1 and four censuses' worth of
committed numbers. The Year-2 metaplan (spec §4) refused to build on those
numbers until they stood on firm ground, and named the campaign after the
refusal. Three things were known to be wrong or unexplained: the placement
suitability formula made the flagship coastal in **100% of censused
worlds**, which in turn kept two of culture's four subsistence modes
(herding and foraging) from ever once appearing in any census; and roughly
60% of generated worlds came out with a frozen dominant biome, a skew
nobody had diagnosed. Campaign Y2-0's mandate was surgical: fix the first,
observe the second follow, *diagnose* the third in writing without
retuning anything, and pay the re-baselining cost of the fix exactly once
— so that every study Year 2 runs inherits corrected numbers instead of
re-deriving them mid-flight.

## What landed

**The fix: seawater is not freshwater (one line at the composition
root).** The degeneracy was a conflation, not a formula error: the
suitability formula (freshwater at weight 0.45, a coastal bonus at 0.20,
temperance at 0.35, minus hostility at 0.5) was sound, but the composition
root fed it `freshwater = 1.0` for every coastal cell. A coastal site thus
collected a perfect freshwater score *plus* its own coastal bonus — an
edge no inland river cell could ever match, which is why the global argmax
was coastal in every censused world and why herding and foraging, both
legal outputs of culture's subsistence function, had never been observed.
The fix removes the conflation: freshwater is now
`max(drainage, moisture)`, and sea access is priced by the coast term and
only there. `SiteInput`, the suitability formula, every stream label, and
every draw order are unchanged — no epoch suffixes, no save-format
consequences; only the *value* fed in at `windows/worldgen` moved.

**The measured outcome, pinned.** Over the 500-seed drift study the split
is now **498 coastal, 2 inland**, pinned as an exact calibration row on
every CI build — the coastal-degeneracy sibling of the five calibration
families Year 1 established. Over the re-run 10,000-seed Census of
Peoples: of the 9,997 worlds that place a flagship at all, **9,931 (99.3%)
are still coastal** — the skew survived; the *invariant* did not — and
**all four subsistence modes are finally present: farming 8,768, fishing
1,213, foraging 12, herding 4**. All sixteen herding-or-foraging flagships
are inland, exactly where the old conflation forbade a flagship to stand.
A coastal cell remains a heavily favored site; it is no longer a
guaranteed one, and the difference is sixteen societies the instrument had
never once seen.

**The frozen-worlds diagnosis (Study 005): modeling consequence, not
defect.** The ~60% frozen-dominant skew (ice 37.8%, alpine 20.6%) got the
treatment spec §4 prescribes: a **preregistered** study (ADR 0016) whose
hypotheses and verdict criteria were committed before any crosstab ran,
fed by one new instrument — a **mean-land-temperature-c** metric, the
Lab's thirty-third. The verdict: **H1 holds** (89.2% of ice-dominant
worlds have a mean land temperature below −10 °C — cold worlds are, on the
record, actually cold) and **H2 holds** (99.2% of alpine-dominant worlds
carry mountain coverage of 0.30 or more, with zero cases below 0.15 —
alpine tracks relief, not temperature). The counter-case the defect
verdict needed — warm, low-relief worlds still coming out frozen — is a
0.4% residual, two-thirds of it explained by tidal lock's already-known
substellar/farside asymmetry, leaving 0.14% of the census genuinely open
and honestly recorded. **No distribution was touched**: the star-class
envelope, the obliquity draw, and the classifier thresholds all stay
exactly as they were, with the candidate adjustments written down as
deferred Year-2+ decisions rather than acted on mid-diagnosis.

**Seed 42, reframed: relocation is the point.** The placement fix moved
the Year-1 exit demo's flagships — the spinning world's outright (Torgna,
506 souls, gives way to **Grumoknar**, 359) and the locked world's
re-weighed where it stood (**Bolugrak**, 559 to 522). The gods-of-seed-42
capstone page previously read the pair as one settlement wearing two
skies; it now says what the ledger says: each sky crowns its own village,
and the two villages diverge exactly where the sky reaches (name,
population, settlement count, head deity) while converging everywhere it
does not (farming, the same five-role ladder, an organized priesthood, and
the same two minor deities down to the decimal of their periods). That is
a sharper statement of the enrichment thesis than the same-village framing
ever was — astronomy propagates through *placement*, one step earlier in
the cascade than the page used to show.

## What was learned

- **An exact function can hide a degeneracy in its inputs.** The
  suitability formula was reviewed, calibrated, and correct; the false
  input beside it survived four campaigns because 100%-coastal looked like
  a plausible fact about worlds rather than a symptom. It took a census
  invariant (herding and foraging at exactly zero, ten thousand times) to
  make the conflation legible — the Lab caught what review did not.
- **Preregistration is what kept a diagnosis from becoming a retune.**
  Study 005's hypotheses and verdict criteria were committed before the
  crosstabs ran, so when the frozen skew turned out to be two honest
  mechanisms (insolation and relief) plus a small residual, the campaign
  had no license to "improve" the distributions it was measuring — and the
  0.14% it could not explain is recorded as an open question instead of
  rounded away.
- **Re-baseline once, then build.** Every committed artifact — three
  seed-42 almanacs, the settlement maps, the 500-seed drift study, the
  10k censuses, every calibration pin — moved in a single, byte-stable
  sweep, verified by running the full regeneration twice and diffing. Year
  2's studies now inherit corrected ground truth exactly once, instead of
  each future campaign paying its own partial re-baseline and wondering
  which numbers still describe the old world.
- **The exit demo got truer by breaking.** The same-flagship framing was a
  coincidence the old placement bug made cheap; the corrected world tells
  a stronger story — divergence tracks the sky precisely, in both
  directions — and the capstone page now proves the thesis with two
  villages instead of asserting it with one.

## Deferred, deliberately (spec §4)

The star-class envelope (only K, G, and F stars ever drawn; a lever on the
global temperature distribution) is recorded as a future astronomy-domain
decision, not adjusted here. The obliquity draw range is measured as
*inert* with respect to the frozen skew — noted so no future campaign
re-spends the check. The 14-world spinning residual waits on a per-cell
temperature export the census CSV does not yet carry. The warm-world tail
(why grassland, shrubland, and rainforest never dominate) is a natural
sequel study, not attempted. And the 99.3% coastal share is a fact about
the corrected model, not a target: nothing in this campaign claims it is
the *right* number, only that it is now an honest one.

## Artifacts

[Study 005: The Frozen Worlds](../laboratory/study-005.md) — the
preregistered diagnosis and its verdict. [Study 003, the Census of
Peoples](../laboratory/study-003.md) — re-baselined, with all four
subsistence modes present for the first time. [The Gods of Seed
42](../gallery/the-gods-seed-42.md) — the Year-1 capstone, reframed around
the relocation. The re-baselined seed-42 almanac pair
([spinning](../gallery/almanac-seed-42-sky.md),
[locked](../gallery/almanac-seed-42-locked.md)) and settlement maps; the
`mean-land-temperature-c` metric in the Lab registry; the 498/2
coastal-inland calibration pin in `windows/lab/tests/calibration.rs`.

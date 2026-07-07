# Study 005: The Frozen Worlds

Study 002 already put a number on it without asking why: across the
10,000-seed Census of Lands, ice dominates 37.8% of worlds and alpine
another 20.6% — together 58.4%, the "~60% frozen" figure spec §4 fix 3
names — while desert, savanna, temperate grassland, shrubland, and every
rainforest variant round out the tail at under half a percent each, and
several never win a single world outright. Spec §4 fix 3 does not ask for a
retune; it asks for a **written diagnosis**, because a distribution this
lopsided is exactly the shape a real physical skew and a classifier defect
both produce, and the two demand opposite responses. This study is that
diagnosis, preregistered per ADR 0016: the hypotheses below are committed
before any crosstab is run, against data already on disk from Study 002's
own 10,000-seed `census-of-lands` run, now carrying the `mean-land-
temperature-c` metric Task 3 of Campaign Y2-0 added for exactly this
question.

## Question

Why is the dominant land biome frozen — ice or alpine — in roughly 60% of
generated worlds, and why do desert, grassland, shrubland, and rainforest
never dominate even one?

## Hypotheses

**H1 (insolation).** Ice-dominance is predicted by low `mean-land-
temperature-c`: the census's temperature distribution straddles freezing
because of where the habitable-zone placement and the stellar-class
envelope put the planet, not because cold worlds are being misclassified as
colder than they are. If H1 holds, ice-dominant worlds are worlds whose
land is, on the record, actually cold.

**H2 (relief).** Alpine-dominance is predicted by high `mountain-coverage`,
independent of temperature — a consequence of the uplift model putting
enough land above the tree line to out-count every warmer biome by cell
count, the same mechanism Study 002 already named as the strongest driver
of habitable-fraction's low tail.

**H3 (classification).** If neither H1 nor H2 holds — if warm, low-relief
worlds still come out frozen-dominant — the biome classifier's thresholds
are misassigning classes at the scale the generator actually produces, and
that is a **defect**, not a modeling consequence.

## Analysis plan

Cross-tabulate `dominant-land-biome` against bucketed `mean-land-
temperature-c` (H1) and bucketed `mountain-coverage` (H2), and separately
against `star-class`, `obliquity-degrees`, and `tidally-locked` — the three
upstream levers that could explain *why* the temperature or relief
distribution lands where it does — over the full 10,000-seed Census of
Lands (`lab-out/census-of-lands/rows.csv`). Ice- and alpine-dominant worlds
are pulled out and checked against the specific counter-case H3 needs:
worlds with a warm land mean (comfortably above the classifier's -20 °C ice
threshold) and low mountain coverage that still land on a frozen dominant
biome. No crosstab has been run as of this commit.

**Verdict criteria**, fixed before analysis: if H1 and H2 together account
for the frozen-dominant population — ice-dominant worlds are the ones with
cold land means, alpine-dominant worlds are the ones with high mountain
coverage, and the residual of warm, low-relief worlds still landing on a
frozen biome is negligible — the verdict is **modeling consequence**: no
code change this campaign, and any candidate adjustment to the star-class
envelope or the obliquity draw range is recorded as an explicit deferred
decision for Year 2+, not acted on here. If a material share of warm,
low-relief worlds still comes out frozen-dominant, the verdict is
**defect**: filed, with no silent retune — a fix to the classifier's
thresholds goes through its own reviewed change, not this diagnosis.

## Results

**The full dominant-biome distribution**, over the 9,997 worlds that place
any land at all:

| Dominant biome | Worlds | Share |
|---|---|---|
| ice | 3,776 | 37.8% |
| alpine | 2,057 | 20.6% |
| tropical seasonal forest | 1,942 | 19.4% |
| taiga | 960 | 9.6% |
| temperate forest | 732 | 7.3% |
| tundra | 490 | 4.9% |
| savanna | 42 | 0.4% |
| desert | 1 | 0.01% |

Temperate grassland, shrubland, temperate rainforest, and tropical
rainforest never win a single world. Frozen (ice + alpine) totals 5,833
worlds, 58.3% — the figure spec §4 fix 3 names.

**H1: `mean-land-temperature-c` against ice-dominance.**

| `mean-land-temperature-c` bucket | ice-dominant worlds | share of all ice |
|---|---|---|
| < -20 | 2,317 | 61.4% |
| -20 .. -10 | 1,051 | 27.8% |
| -10 .. 0 | 352 | 9.3% |
| 0 .. 10 | 55 | 1.5% |
| 10 .. 20 | 1 | 0.0% |
| ≥ 20 | 0 | 0.0% |

89.2% of ice-dominant worlds have a whole-land *mean* temperature below
-10 °C, and 61.4% have a mean below -20 °C — the classifier's own
per-cell ice threshold — meaning for the majority of ice-dominant worlds
even the coarsest possible summary (a single global average) already
clears the bar that classifies a cell as ice. H1 holds for the great bulk
of the ice-dominant population: these are worlds whose land, on the
record, actually is cold.

**H2: `mountain-coverage` against alpine-dominance.**

| `mountain-coverage` bucket | alpine-dominant worlds | share of all alpine |
|---|---|---|
| < 0.05 | 0 | 0.0% |
| 0.05 .. 0.15 | 0 | 0.0% |
| 0.15 .. 0.30 | 17 | 0.8% |
| 0.30 .. 0.50 | 223 | 10.8% |
| ≥ 0.50 | 1,817 | 88.3% |

No alpine-dominant world in the census has mountain coverage below 15%,
and 99.2% sit above 30%. Cross-referencing against temperature buckets
from the same run, alpine-dominant worlds spread across the *entire*
temperature range — from below -20 °C (66 worlds) to above 20 °C (9
worlds) — with no concentration at the cold end at all. H2 holds cleanly:
alpine-dominance tracks relief, not temperature, exactly as the
classifier's own precedence (elevation above the tree line, checked
*before* the temperature lookup) predicts.

**Upstream levers: star-class, obliquity, and tidal lock.**

Every one of the 10,000 seeds in this census draws its star from only
three classes — orange dwarf (K, 2,520 worlds), yellow dwarf (G, 3,204),
and yellow-white dwarf (F, 4,276); no cooler (M) or hotter class appears
anywhere in the sample. Within each class the biome mix is essentially the
same shape (ice and alpine together are 56.7% of K worlds, 59.0% of G,
58.8% of F) — the star-class envelope this generator currently draws from
does not by itself separate a warm-world population from a cold one; it
only ever samples the middle of the range.

`obliquity-degrees`, bucketed, shows no relationship at all: the biome mix
is statistically flat across every bucket from under 10° to 30–40° (ice's
share of each bucket is 37.6%, 38.1%, 38.0%, and 37.0% respectively) —
consistent with Study 002's finding that obliquity moves seasonal swing,
not the annual mean a biome classification is keyed to. Obliquity is ruled
out as a driver of the skew.

`tidally-locked` worlds are a small population (457 of 10,000, 4.6%) but a
starkly different one: **452 of 457 locked worlds (98.9%) are
frozen-dominant** (410 ice, 42 alpine) against **5,381 of 9,543 spinning
worlds (56.4%)**. Tidal lock sharply raises the odds of a frozen dominant
biome — the frozen far side outweighing the narrow terminator ring in cell
count, the same mechanism Study 002 already reports — but locked worlds
are too small a share of the census to be *the* explanation for the
aggregate 58.3%: even restricted to spinning worlds alone, more than half
still land on a frozen dominant biome, so H1 and H2 have to carry the
weight for the bulk of the population regardless of lock status.

**The H3 counter-case: warm, low-relief worlds.** Pulling ice- or
alpine-dominant worlds with `mean-land-temperature-c` ≥ 0 °C finds 910
worlds (9.1% of the census) — but 854 of those 910 (93.8%) are
alpine-dominant, exactly what H2 predicts (a warm, high-relief world is
still alpine because elevation, not temperature, drives the classification
there). Only **56 worlds are ice-dominant with a mean land temperature at
or above freezing** — 1.5% of all ice-dominant worlds. Restricting further
to low relief (`mountain-coverage` < 0.15, ruling out an alpine-adjacent
confound) leaves **40 worlds**: 1.06% of the ice-dominant population, 0.40%
of the full census. Of those 40, **26 (65%) are tidally locked** — nearly
fourteen times the locked share of the whole census — and this subset's
`habitable-fraction` averages 7.0% (range 3.0–13.0%), below even the
locked-world population average Study 002 reports (8.7%): a narrow,
marginal terminator ring plus a scorching substellar point is exactly the
already-documented locked-world shape, not a new mechanism. The remaining
**14 worlds are spinning**, share no common star class or obliquity
(K, G, and F stars alike, obliquity 3.2–34.3°, spread evenly), and —
counter to what a "marginal world" story would predict — their
`habitable-fraction` averages 18.0% (range 14.2–24.4%), *above* the
10,000-world population mean of 15.1%. These are not degenerate worlds by
any other measure; they are worlds with a generously habitable pocket that
is nonetheless outweighed, cell for cell, by an even larger expanse of
land the classifier correctly puts below -20 °C elsewhere. That is exactly
the shape a strongly latitude-skewed land distribution produces without
any classifier misbehavior — `mean-land-temperature-c` is a single
whole-world average and cannot distinguish "most land is mildly cold" from
"most land is deep-frozen while a smaller warm-to-hot region pulls the
average up." Confirming that reading definitively would need a per-cell
temperature export this study's CSV does not carry — recorded below as a
follow-up, not chased further here.

## Verdict: modeling consequence

Judged against the preregistered criteria: **H1 and H2 together account
for the frozen-dominant population.** 89.2% of ice-dominant worlds have a
mean land temperature below -10 °C and 61.4% below the classifier's own
-20 °C threshold; 99.2% of alpine-dominant worlds have mountain coverage
above 30%, with zero exceptions below 15%. The counter-case H3 requires —
warm, low-relief worlds still landing on a frozen dominant biome — is not
absent, but it is a small residual (40 of 10,000 worlds, 0.4% of the
census) that is itself mostly explained (65%) by tidal lock's
already-documented substellar/farside asymmetry rather than by anything
new. That residual is not "negligible" in the sense of zero, and the 14
unexplained spinning-world cases inside it are recorded honestly rather
than rounded away, but at 0.14% of the census it does not meet the bar of
a **material** share. The verdict is **modeling consequence**, not
defect: the biome classifier is doing what its thresholds say it does: no
code change this campaign.

**Decisions deferred (Year 2+), recorded per the preregistered criteria
rather than acted on here:**

- **Star-class envelope.** This census's 10,000 seeds draw only K, G, and
  F stars; no M (cooler, redder) or O/B/A (hotter, bluer) class ever
  appears. Widening the drawable range — particularly toward hotter
  classes — is a plausible lever on the aggregate temperature distribution
  and is exactly the kind of change spec §4 fix 3 defers rather than
  authorizes here. Candidate for a future astronomy-domain campaign, not
  this one.
- **Obliquity draw range.** Ruled out as a driver of the current skew (the
  biome mix is flat across every obliquity bucket observed), so widening
  it would not by itself change the frozen-dominant share — noted so a
  future campaign does not re-spend effort re-checking a lever this study
  already measured as inert.
- **The 14-world spinning residual.** A per-cell temperature export (mean
  and extremes, not just the whole-world average this study's
  `mean-land-temperature-c` column carries) would let a future study
  distinguish "deep-frozen majority land offset by a small hot patch" from
  a genuine classifier boundary problem for this specific pocket. Filed as
  an open question, not a defect — the residual is too small and too
  well-explained by tidal lock in the bulk of cases to justify a code
  change on its own.
- **Why grassland, shrubland, and rainforest never dominate.** Outside this
  study's H1–H3 scope, but visible in the same table: every non-frozen,
  non-forest biome is starved by the same two upstream facts already
  identified — the narrow K/G/F star-class envelope keeps most worlds out
  of the hot end of the Whittaker lookup, and mountain coverage's bimodal
  split (Study 002) pushes half the census toward alpine instead of any
  temperate or arid class. A dedicated study of the *warm*-world tail is a
  natural sequel, not attempted here.

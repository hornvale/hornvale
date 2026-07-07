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

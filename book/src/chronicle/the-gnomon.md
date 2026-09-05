# The Gnomon

A gnomon is the fixed part of a sundial — the piece that does not move, and
without which the moving part means nothing. This campaign built three of
them, all pointed inward at the project rather than outward at the world: an
index over when things first happened, a report in which a world volunteers
its own outliers, and a word for an idea that was measured and found false.

The third one got used on the first two within the same campaign.

## The headline: the anomaly report does not concentrate a planted perturbation

The anomaly report is a transpose of the Domesday survey. Where the Domesday
asks *is this metric weak across a thousand worlds*, the report asks *is this
world strange across two hundred and twenty-two metrics*: for one world, rank
every evaluable census column by how deep in the thousand-world distribution
its value sits, and show the ten deepest. It is the autonomous end of the
observation spectrum — a run that flags itself without anyone having asked the
right question first.

The obvious objection is that this is circular. The prior is the census
percentiles; the population scored is that same census; the report flags a
fixed fraction of it by construction. The campaign's answer, frozen in the
spec before any of the code existed, was to score the *ranking* against a
label the census cannot supply. Perturb one generative constant, rebuild a
small seed set, and ask whether the columns the perturbation demonstrably
moved surface in the affected world's top ten. The label comes from which
constant was changed. No percentile decides it.

**H1, preregistered:** recall@10 ≥ 0.60 over the injection battery, where the
unit is an (injection × seed) pair.

**Measured: recall@10 = 0.5667, over 120 pairs. Zero void pairs. H1 is
falsified.**

| injection | constant | perturbation | hits | moved evaluable columns |
|---|---|---|---:|---:|
| geothermal | `CRATONIC_GRADIENT_K_PER_KM` | 15.0 → 22.5 | 20/20 | 1 |
| unconformity | `UNCONFORMITY_COVER_M` | 200 → 400 | 1/20 | 1 |
| aquifer | `CLASTIC_AQUIFER_MIN_POROSITY` | 0.46 → 0.30 | 20/20 | 9 |
| karst | `KARST_MIN_POROSITY` | 0.4 → 0.55 | 5/20 | 9 |
| pantheon | `PANTHEON_FLOOR` | 0.25 → 0.45 | 9/20 | 9 |
| phonology | `LOUDNESS_PENALTY` | 0.22 → 0.60 | 13/20 | 20 |

Six injections, twenty seeds each, two independently generated unperturbed
baselines, every arm authored on the canonical host. All three preregistered
controls held: each injection moved its world (the positive control, run
through the library behind `hornvale lab diff` rather than a second differ
written for the purpose); the second baseline moved nothing; and the two
independent baseline runs ranked identically for all twenty worlds, which is
an identity check rather than a statistic, because the census is
deterministic.

### The figure read both ways, because the honest one is worse

Two arms score 20/20 at a best-hit depth of exactly `0.000000` — the
perturbation pushed the metric clean outside the range any of the thousand
census worlds occupies, where tail depth is zero by construction and the
column cannot fail to rank first. There is a real argument that this is the
report working exactly as designed: a value beyond every world ever measured
*is* maximally unusual. There is an equally real argument that those forty
pairs are the least discriminating in the battery, because nothing about the
ranking was under test.

So the figure is published twice. **With them, 0.5667 (68/120). Without them,
0.35 (28/80).** The battery is not edited — deleting arms after seeing the
result is precisely the post-unblinding retuning the preregistration forbids,
and it is worth noticing that dropping them makes the finding *worse*, so no
reading of this is a rescue.

### What the falsification actually says

A tail rank measures **unusualness against a thousand worlds**. A perturbation
produces unusualness only sometimes. Those are different quantities, and the
gap between them is the whole result.

The clearest case is `unconformity`, at 1/20. The perturbation doubled a cover
depth and moved exactly one column, `unconformity-fraction`, in every world —
and in nineteen of twenty worlds the moved value was still an ordinary value.
It landed at depth 0.0526 against a report whose tenth-place cutoff sat at
0.1001; unremarkable, and correctly so. The world *was* different, and it was
not *strange*. The report never claimed to detect difference, and this is the
first measurement that makes the distinction cost something.

The corollary is a warning about how such a battery reads. Recall is
correlated with how many columns an injection moved — one column for
`unconformity` at 1/20, twenty for `phonology` at 13/20 — because an injection
that disturbs a downstream draw moves many columns and gets many chances to
place one of them in a ten-slot report. That makes recall@10 partly a measure
of blast radius, which is a property of the perturbation, not of the ranking.
A successor instrument should score the *specific* column a perturbation was
aimed at, not the union of everything it touched.

### The report is a pointer, not a detector

That is the sentence the registry now carries, and it is a demotion of the
row's original promise ("the instrument that finds a defect nobody thought to
look for") rather than a restatement of it. The report ships: 116 evaluable
columns out of 222, with 47 excluded as frozen, never-present, or tied at both
rails, rendered to a committed chapter. What does not ship is the claim that
its top ten reliably contains what changed.

## The defect it found anyway, before its own hypothesis was tested

While the exclusion roster was being computed, five columns turned out to be
excluded by the anomaly report and invisible to the Domesday's own weak-column
detectors, because both of those gate on a statistics helper that returns
nothing for a column with no values at all. The most degenerate column
imaginable — one that never produces a value in any world — was structurally
invisible to the instrument whose job is to find degenerate columns.

Two of the five were the campaign's own, and the reason is the finding:

```
first-day-occ-cause-burned      present in    0 / 1000 worlds
first-day-occ-cause-plague      present in    0 / 1000 worlds
first-day-occ-cause-famine      present in  718 / 1000
first-day-occ-cause-fled        present in  999 / 1000
first-day-occ-cause-migrated    present in 1000 / 1000
```

`Burned` and `Plague` are declared variants of the cause-of-end enumeration.
They carry stable, round-trippable text labels. Two separate readers parse
those labels back. And in a thousand worlds the generator has never once
produced either. Registered, labelled, readable, unreachable — the same shape
as the `person-died` predicate that an earlier campaign shipped and a later one
had to resolve, and invisible to the suite for the same reason: no test asserts
that a variant is *reachable*, and a `match` over five arms is exhaustive
whether or not two of them ever run.

Whether they are unreachable by defect or merely astronomically rare is not
established here, and is deliberately not asserted either way. What is
established is the route by which it surfaced. Nobody was looking for it. The
index found it as a side effect of asking *when* something first happened —
which is, exactly, what the anomaly report was supposed to do and, on this
evidence, does less reliably than a first-occurrence index does by accident.

## The index, and how much less it reaches than the row claimed

The first-occurrence index is materialised as nineteen census columns over a
frozen roster of `(predicate, object)` keys: the earliest world-day each key
was committed, per seed. There is no second sweep and no new generated
directory — the census *is* the index once the columns exist, and it inherits
the drift check, the survey chapters and the chart pipeline for free. The
nineteen columns cost nothing measurable at census scale.

The registry row that proposed it said "per predicate". Measured on a real
world's ledger, that framing was wrong twice over. Of 115 distinct predicates,
only 22 ever carry a non-genesis day; keyed on predicate alone, every species
shares one first-day and the motivating query — a species and a condition and a
place, all at once — cannot be expressed at all. The key has to be
`(predicate, object)`, and the index is degenerate for 93 of 115 predicates.

The shipped roster shows the same thing at its own scale. Of the nineteen
columns, two are never present in any world (above), and seven are frozen at
day zero across all thousand — settlements, the four species occupations,
classical technology, and person-founding all begin at genesis by
construction. **Ten of nineteen columns carry information.** That is not a
failure; the history bake and the persons are where the interesting first-
occurrences live, and that is where the ten are. But it is a narrower
instrument than the row advertised, and the row now says so.

One column is worth its own sentence: `first-day-is-person` ranges from
−24,252.934 to −24,048.250 days. Persons first exist *before* day zero,
because the ancestry the founders are drawn from is back-dated. A first-
occurrence index over a world with a prehistory does not have day zero as its
floor.

## The conjunction is nearly universal, and the useful answer is the day

The query the index existed to make cheap — worlds where kobolds occupied a
site, a ruin exists, and tribute was paid — returns **991 of 1000 seeds**.
Tribute is the only limiting predicate at 991; the other two hold in every
world.

That is a weak demonstration of "an intractable search made cheap", and it is
reported as such. Nothing was hard to find. The plan anticipated the opposite
failure (zero rows) and gave a decision rule for it; near-universality
undercuts the framing just as effectively, and pretending otherwise by
substituting a more selective conjunction after the fact would be choosing the
demonstration to fit the instrument.

The honest reframe is that selectivity was never the claim. The query's second
column is:

```sql
greatest("first-day-occ-people-kobold",
         "first-day-is-ruin",
         "first-day-pays-tribute-to") AS replay_from
```

`greatest()` over three first-days *is* the maximum over the intersection, and
it answers a question no census column could express before: **the world-time
a replay must start from to see all three.** Over the 991 matching seeds that
value takes 58 distinct days, from 0 to 703,106.25, with a median of 36,525.
Which worlds is a boring answer here; *when* is not. Pins constrain genesis and
watchpoints target emergent late state; this is the first thing that bridges
them, and it stays useful precisely when the set intersection is dull.

## H2 passes, and a green H2 is not vindication

The control hypothesis asks whether the census percentiles generalise to seeds
the census never saw: fit tail depths on seeds 0–999, score 200 held-out seeds,
and require the share of worlds carrying at least one column at depth ≤ 0.01 to
fall within a factor of two of the in-census share.

In-census 0.7050 over 1000 worlds; held-out 0.7500 over 200; ratio **1.0638**
against a tolerance of 2. It passes comfortably, in 136.8 seconds on the
canonical host.

It should not be read as support for H1, and the campaign said so in advance.
Seven in ten census worlds already hold a column at depth ≤ 0.01, so the
in-census share this is compared against is close to a ceiling; a stationary
distribution passes H2 while flagging nothing useful. H2 says the percentiles
are not overfit to the seeds they were fit on. It says nothing whatever about
whether the ranking is worth reading — which is the question H1 asked and
answered in the negative.

## `refuted`, and its first real user

The registry's status vocabulary was explicitly closed at six, with an
anti-drift sentence forbidding a seventh. A project whose method is
preregistered falsification, and several of whose campaigns ship the null as
the headline, had a word for *we considered it and set it aside* and no word
for *we tested it and reality said no*. Opening the vocabulary was therefore
decision-shaped rather than edit-shaped, and it took a decision record.

**`rejected` is a decision. `refuted` is a measurement.** The citation is
mandatory and mechanically enforced: an uncited refutation is an assertion with
no way to check it.

A hand audit of the 47 rows that mention falsification anywhere in their prose,
against a preregistered admission rule — *the row's own central claim was
tested and found false, and no artifact shipped from it* — reclassified exactly
one. That is the narrow, honest outcome the rule was written to produce. The
thirteen `shipped` rows that also refute a prediction shipped a mechanism; one
token cannot say both, and demoting them would lose the shipped fact.

So the greppable epistemic record starts almost empty, and begins accumulating
with the next falsified campaign — which turned out to be this one. The first
row written under the seventh status is this campaign's own headline finding.
The word was added in one task and spent in the next.

## Two things measured on the way past

**Cross-platform byte-identity holds on a fresh surface.** The injection
battery was authored twice: once on aarch64/Darwin as a pilot, once on
x86_64/Linux for adjudication, with no generative code changed between the two
authorings. Eight arms, sixteen files, 222 metric columns × 20 seeds each — and
the two authorings differ **only** in the manifest's `host` and `sha` fields.
Every `rows.csv` and every `schema.json` is byte-identical. That corroborates
the cross-platform guarantee well outside the forty-world probe it was
originally measured on, on a metric surface that did not exist when the probe
was run.

**A constant's blast radius is not predictable from its domain.** Perturbing a
terrain *lithology* threshold moves *naming* columns — name length, syllable
counts, toponymic roots, collision rate — in all twenty seeds. So does a
religion constant. The coupling runs through the shared draw sequence, not
through anything a reader would infer from the file the constant lives in. Any
future estimate of "what could this constant possibly affect", made by reading
the domain it sits in, will be wrong in this direction.

## What is open

The falsification does not close the anomaly report; it prices it. Three
successor routes are registered rather than argued here: label from pins (a pin
is a known departure from the unselected population, so it labels for free),
retrodiction against defects the project has already found and fixed (highest
external validity, and the most expensive), and a scorer that targets the
column a perturbation was aimed at rather than everything it moved.

The blind spot the report exposed in the Domesday — a never-present column
being invisible to both weak-column detectors — needs either a ninth detector
or a change to the statistics helper's own contract. It is registered and
unfixed.

And the frozen roster will rot. It is frozen at nineteen columns precisely so
the census schema cannot become seed-dependent, which means a predicate added
to the history bake later will silently not be indexed, and nothing checks for
that today.

## Postscript: the world moved, and the verdict did not survive it (2026-08-15)

The finding above — *H1 falsified, recall@10 = 0.5667 against a 0.60 bar* —
was re-measured six weeks later by a campaign that had nothing to do with
anomaly ranking, and it did not hold.

The Glasshouse corrected three defects in the climate model and moved the
census population from a −11.99 °C median land temperature to −3.65 °C. The
injection fixtures were re-authored against the new census on the canonical
box. **Nothing about the report changed** — not `REPORT_SIZE`, not
`TAIL_DEPTH_BAR`, not the scorer, not the evaluable surface. Only the worlds
were different.

Re-measured on those worlds: **recall@10 = 0.6083, 73 hits over the same 120
pairs.** The bar is 0.60.

The tempting reading is that the report works after all and this chapter's
headline was wrong. That reading is not available, for a reason this chapter
should have anticipated and did not. **The new figure clears the bar by one
hit out of a hundred and twenty.** At the bar, the standard error on a
120-pair proportion is `sqrt(0.6 × 0.4 / 120) = 0.0447`. So the original
0.5667 sat 0.75 standard errors *below* the line and the new 0.6083 sits 0.19
*above* it, and the distance between the two measurements is 0.66 — smaller
than the noise in either one.

A 120-pair battery cannot tell "the ranking concentrates a planted
perturbation" from "it does not" at a 0.60 threshold. It never could. The
original refutation looked clean only because the measurement happened to land
on the low side of a distribution wide enough to reach both sides of the bar,
and this campaign happened to land on the high side of the same distribution.

So the honest state of the question is **open**, and its registry row has been
returned to that state rather than flipped to a confirmation. What this
chapter got right is preserved and is worth more than the verdict was: the
*spread* is the finding — per-injection recall ranges from 1/20 to 20/20, and
both perfect arms scored at a tail depth outside the census's entire observed
range, where the report cannot do otherwise. That structure is unchanged by the
re-measurement, and it is still what says the report is a pointer rather than a
detector.

Two things are worth carrying out of this, and neither is about anomalies.

**A preregistered bar needs a power calculation, not just a number.** Freezing
"0.60" before the measurement is the right discipline and it is what this
campaign did. But a threshold is only a decision rule if the instrument can
resolve it, and one line of arithmetic before the freeze would have shown that
120 pairs cannot separate 0.57 from 0.61. The freeze was honest and the bar was
still unusable.

**A published result has a shelf life measured in world-changes, not in time.**
This finding was pinned as an explicit witness precisely so that a change to
the report could not silently rewrite it — and what actually rewrote it was a
change to the *world*, from a campaign that never touched the report. The pin
worked: the re-read was forced rather than skipped. It simply caught a
different mover than the one it was watching for.

## Second postscript: a third world, and the same answer (2026-08-17)

The pin fired again, thirteen days later, for the same reason and with a
different outcome — and the difference is worth more than either number.

The Underworld gave the world a subterranean layer and refreshed the canonical
census against it. Seventy-six of two hundred and twenty-four distributions
moved. As with The Glasshouse, the report was untouched: no change to
`REPORT_SIZE`, `TAIL_DEPTH_BAR`, the scorer, or the evaluable surface. Only the
worlds were different, and this time they were different in a way that has
nothing to do with temperature.

Re-measured: **recall@10 = 0.6000, 72 hits over the same 120 pairs.** The bar
is 0.60. The measurement is the bar.

There is no reading to be had from that, and saying so precisely is the point.
A proportion that lands exactly on its own threshold decides nothing in either
direction; it is the single most uninformative position a reading can occupy.
Laid out against the standard error at the bar — `sqrt(0.6 × 0.4 / 120) =
0.0447` — the three measurements of one unchanged report now read:

| campaign | hits | recall@10 | distance from the bar |
|---|---|---|---|
| The Gnomon (2026-08-13) | 68/120 | 0.5667 | −0.75 SE |
| The Glasshouse (2026-08-15) | 73/120 | 0.6083 | +0.19 SE |
| The Underworld (2026-08-17) | 72/120 | 0.6000 | 0.00 SE |

So the verdict stands exactly where The Glasshouse left it: **the question is
open, the instrument is underpowered, and the fix is more pairs rather than a
moved bar.** The registry row is neither `refuted` nor `shipped`, and this
campaign did not change its status.

What is new is the standing of that verdict rather than its content. The
Glasshouse argued from two readings straddling the bar that a 120-pair battery
cannot resolve a 0.60 threshold — a sound argument, but an inference from two
points, and two points are exactly how this chapter's original mistake was
made. A third census epoch, moved by an unrelated mechanism, has now produced a
third reading inside the same one-standard-error band.

**Careful about what that third reading buys, because it is less than it
sounds.** Three readings clustered inside ±1 SE corroborate that the report's
recall is *stable* across three unrelated world epochs — which is a real and
useful thing to know, and is not the same as demonstrating underpower. The
underpower claim does not need the readings at all: it follows from n = 120
alone, whose standard error at the bar is 0.0447, so a battery this size cannot
distinguish 0.60 from anything inside roughly 0.51–0.69 at two SE. What the
three readings add is that nothing has moved the estimate *out* of that
interval, so no amount of re-measuring at this n will settle the question. The
fix remains more pairs rather than a moved bar.

**A witness that fires twice is worth more than one that never moves.** The
first firing overturned a verdict, which is the dramatic case and the one the
pin was written for. The second firing overturned nothing — it corroborated the
correction the first one forced, which is the quieter case and, over the life
of a result, the more common one. A pin that only ever announces reversals
would be a pin nobody trusted when it stayed silent. This one has now
demonstrated both halves: it caught a wrong verdict, and it confirmed the
replacement.

The obvious temptation, on a reading that sits precisely on the line, is to
adjust something small enough to make the sentence readable again. That is
the retuning the preregistration forbids, and it is worth naming plainly here
rather than only in the test: the bar is not to be widened, and the battery is
not to be re-read until it lands somewhere comfortable. Three epochs have now
established that the direction of the error is smaller than the noise in either
direction. The next thing to change should be `n`.

## Third postscript: a fourth world, and the reading repeats exactly (2026-08-18/19)

The Burr refreshed the canonical census again, this time with a language
epoch (`ROOT_EPOCH v4`, census committed as `635d116d`) that redraws every
generated name and reseeds the phonology cascades feeding it. As with the two
prior re-reads, the report itself did not move — `REPORT_SIZE`,
`TAIL_DEPTH_BAR`, the scorer and the evaluable surface are all unchanged.
Only the worlds are different, and this time in a way that has nothing to do
with either temperature or the subterranean layer.

Re-measured: **recall@10 = 0.6083, 73 hits over the same 120 pairs.** The bar
is 0.60.

That is not a new number. It is The Glasshouse's number, exactly — 73/120,
+0.19 SE from the bar — reproduced by an unrelated mechanism at an unrelated
epoch. Laid out against the same standard error at the bar,
`sqrt(0.6 × 0.4 / 120) = 0.0447`, the four measurements of one unchanged
report now read:

| campaign | hits | recall@10 | distance from the bar |
|---|---|---|---|
| The Gnomon (2026-08-13) | 68/120 | 0.5667 | −0.75 SE |
| The Glasshouse (2026-08-15) | 73/120 | 0.6083 | +0.19 SE |
| The Underworld (2026-08-17) | 72/120 | 0.6000 | 0.00 SE |
| The Burr (2026-08-18/19) | 73/120 | 0.6083 | +0.19 SE |

Four readings, three of them distinct census epochs plus one that ties an
earlier one, all inside a single standard error of the bar — with no two
consecutive readings landing on the same side of it (below, above, on, above).
That pattern is exactly what an instrument with no real signal at this `n`
should produce: noise scattered tightly around the threshold rather than
converging toward either side of it. It is further corroboration of the
underpower diagnosis, not a fourth independent data point toward resolving
it — the conclusion the second postscript already drew from three readings
holds unchanged from a fourth: the question is open, the instrument is
underpowered, and the fix is more pairs rather than a moved bar. The registry
row's status is unchanged by this re-read, exactly as it was unchanged by the
second.

## Postscript 4 — re-read at The Granary's close (2026-08-24)

The Granary's sub-year raid timing moved the worlds again (its fixtures were
re-authored against the new history), and with them the tally. The campaign's
first measurement read **70/120 = 0.5833** — but on LOCAL PILOT fixtures
(`HV_GNOMON_PILOT=1`, Nathan-authorized), which sit in the known ~0.1%
discrete-count host-divergence class against the census goldens. The CANONICAL
re-author on lefford (fixtures at `13695c4ae`, census goldens at `c54fb62c9`)
restored **73/120 = 0.6083, +0.19 SE**, numerically identical to the
Glasshouse/Burr readings; the report was untouched throughout. The canonical
reading list therefore stays five epochs of one unchanged report:
0.5667 / 0.6083 / 0.6000 / 0.6083 / 0.6083 — all within one SE of the bar.
The underpower diagnosis is unchanged, and arguably strengthened: even a
world-moving campaign leaves the canonical reading inside one SE of the bar,
and the pilot detour is its own small lesson — an off-host fixture set can
move the tally by three hits, which is exactly why only canonical readings
enter the list. The registry row's status is unchanged, exactly as it was
unchanged by each of the four before this one.

## Postscript 5 — the first re-read across a moved instrument (The Winze, 2026-08-29)

Every postscript above opens the same way, and until now the sentence was
simply true: *the report did not move, only the worlds did.* That sentence is
what made five readings of one battery a series rather than five unrelated
numbers. `REPORT_SIZE`, `TAIL_DEPTH_BAR` and the scorer were fixed, and so was
the set of census columns the scorer ranks over — the *evaluable surface*, the
117 numeric columns that survive the frozen exclusion tests for being frozen,
too sparse, or tied at both rails.

The Winze broke that. It registered a new census metric,
`breached-delving-count`, and the metric is an integer that varies across
worlds, so it lands on the evaluable side rather than the excluded one. The
surface grew from 117 columns to 118. A world's report is a fixed-size top-ten
cut of that surface, so one more candidate column is one more thing that can
displace the tenth flag; and a *hit* is scored by intersecting a pair's moved
columns with the surface, so the new column enters the numerator's definition
as well. Two mechanisms, both live, by which the **instrument** and not the
world could move the tally.

Re-measured on the refreshed census and the re-authored fixtures:
**recall@10 = 0.6000, 72 hits over the same 120 pairs.** The bar is 0.60.

So the number moved, from 73 to 72, at exactly the epoch where the honest
answer might have been "this reading is not comparable to the five before it."
The pin's own instruction anticipates this case and says to stop: *first ask
whether your change touched the report; if it did, you have a different and
larger question than a re-pin.*

### Measuring the confound instead of declaring it

The question turns out to be answerable, cheaply, and the reason is a property
of how the scorer is written rather than a lucky accident. The evaluable
surface is *derived* from the census's own column list; the per-column
percentile indices are built independently of one another; and the function
that scores an out-of-census world takes the census by reference. Nothing
about the ranking is baked in ahead of time. Remove one column from a census
held in memory and the scorer reproduces the older surface against the newer
worlds — no change to `REPORT_SIZE`, no change to the bar, no change to a line
of committed code, and therefore none of the second incomparability that
editing the scorer to study the scorer would have introduced.

Two checks, run in that spirit:

**The surface moved by exactly one column, and only in one direction.**
Comparing the exclusion pass over the pre-refresh committed census against the
refreshed one: evaluable 117 → 118, excluded 50 → 50, with the difference set
`{breached-delving-count}` on the new side and *empty* on the old. Nothing
else crossed the boundary. That matters more than it first looks: a world
change can silently push a column onto or off the surface by collapsing its
spread, which is precisely what happened at The Burr, where a language epoch
piled a cascade metric onto both rails and dropped it out of the surface
without anyone adding or removing a metric. Here that did not happen, so the
new metric is the whole of the instrument delta rather than the visible part
of it.

**Ablating that column reproduces the tally exactly.** Dropping
`breached-delving-count` from the loaded census and re-scoring the same
committed fixtures reads **72/120** — and, more tellingly than the total,
identically arm for arm: geothermal 20/20, unconformity 4/20, aquifer 20/20,
karst 2/20, pantheon 7/20, phonology 19/20, the same six numbers the full
surface produces. The new column contributed zero hits and zero counted pairs.

The instrument's share of the move is therefore **nothing**, and the reading is
comparable to its five predecessors after all. It repeats The Underworld's
exactly:

| campaign | hits | recall@10 | distance from the bar |
|---|---|---|---|
| The Gnomon (2026-08-13) | 68/120 | 0.5667 | −0.75 SE |
| The Glasshouse (2026-08-15) | 73/120 | 0.6083 | +0.19 SE |
| The Underworld (2026-08-17) | 72/120 | 0.6000 | 0.00 SE |
| The Burr (2026-08-18/19) | 73/120 | 0.6083 | +0.19 SE |
| The Granary (2026-08-24) | 73/120 | 0.6083 | +0.19 SE |
| The Winze (2026-08-29) | 72/120 | 0.6000 | 0.00 SE |

Six readings, six census epochs, one report whose definition has never moved,
every one inside three-quarters of a standard error of the bar. The verdict is
unchanged and the registry row's status is unchanged: the question is open, the
instrument is underpowered at `n = 120`, and the fix is more pairs rather than
a moved bar.

The reading lands *exactly on* the bar, which is worth one sentence and no
more. A battery that cannot separate 0.5667 from 0.6083 certainly cannot make
anything of 0.6000 sitting on the line; it is the same null arriving at its
least legible coordinate, and reading it as "the bar is met" would be the
retuning-by-repetition the preregistration exists to forbid.

One small piece of reassurance, recorded because a null ablation invites the
suspicion that the new column simply does nothing. It does something.
`breached-delving-count` appears in the geothermal arm's own set of moved
columns, and the chain is physically coherent: perturbing the geothermal
gradient moves cave depth, which moves how deep a delve reaches, which moves
how often one breaches. The column is live. It just never displaced anything
out of a ten-column report in a way that changed whether a pair scored.

### What this postscript is actually for

The finding above is a null, and nulls are cheap to under-record. The durable
part is not the number but the move: when a campaign changes both the world and
the instrument at once, "this reading is incomparable" is a *hypothesis*, not a
conclusion, and it is often testable at a cost far below the cost of accepting
it. The test here took one in-memory column deletion and one re-score. Had it
come back non-null, the honest report would have been an incomparable sixth
reading and a smaller series — but it did not, and the difference between those
two outcomes was one command nobody had to be talked into running.

The witness assertion that pins this figure now carries both halves: the
corrected list of the four places the figure lives, and the instruction to
*measure* an instrument change rather than to stop at it.

## Postscript 6 — when the instrument's share is not null (The Weft, 2026-09-04)

The Weft made the same confound larger and, this time, consequential. Its
canonical census added 22 numeric metrics. Twenty-one vary across the thousand
worlds and enter the anomaly report's evaluable surface; only
`weft-legibility-mi-erratic` is frozen and excluded. The surface grew from 118
columns to 139, with the excluded roster moving 50 to 51. The report still
publishes ten flags, so it now cuts ten places from a field with 21 more
candidates.

The injection battery was re-authored by its dedicated script on lefford at
the exact census delivery, `b162273b4`. All eight arms covered the same twenty
seeds with zero refusals. Every injected arm moved at least one distribution,
the independent second baseline moved none, and the fixture schema exactly
matched the 249-metric census.

On the full new surface the witness reads **69/120 = 0.5750**, with zero void
pairs. Arm for arm: geothermal 20/20, unconformity 4/20, aquifer 20/20, karst
2/20, pantheon 6/20, phonology 17/20. The 0.60 bar remains frozen; a new census
does not authorize moving it.

The ablation that licensed comparison at The Winze does not license it here.
Removing the Weft family from the census held in memory — the 21 ranked
columns, plus the already-excluded erratic legibility column whose removal is
inert — and scoring the **same newly authored rows** restores **72/120**. It
also restores every earlier arm total: 20/20, 4/20, 20/20, 2/20, 7/20, 19/20.
The new surface itself therefore displaced three hits, one from pantheon and
two from phonology.

That makes this a pinned seventh canonical reading but **not a seventh point
in the comparable series**. Its numerical distance from the bar is still less
than one standard error, but folding it into the six earlier points would hide
a confound now measured directly. Those six comparable readings remain
0.5667 / 0.6083 / 0.6000 / 0.6083 / 0.6083 / 0.6000, all within one standard
error. Their verdict remains “cannot tell,” and their remedy remains more
pairs, not a moved bar.

The contrast with Postscript 5 is the reusable result. “Run an ablation” is
not a ritual that automatically blesses comparability. The Winze's ablation
read null, so comparison survived; The Weft's read minus three hits, so the
honest record is an incomparable current witness and an unchanged historical
series.

## Postscript 7 — ablate family by family (The Warp, 2026-09-05)

The Warp added thirty-two numeric metrics to the canonical census. Thirty-one
vary across the thousand worlds and enter the anomaly report's evaluable
surface; only the erratic found-fraction column is excluded, absent by
construction. The surface grew from 139 columns to 170, the excluded roster
from 51 to 52, and the census as a whole from 249 metric columns to 281. The
eight injection arms were re-authored by their dedicated script on lefford
against those goldens, twenty seeds each, zero refusals.

On the full new surface the witness reads **66/120 = 0.5500**, with no void
pairs. Arm for arm: geothermal 20/20, unconformity 2/20, aquifer 20/20, karst
2/20, pantheon 6/20, phonology 16/20. The bar stays at 0.60.

The previous campaign's ablation asked one question. This census needed two,
and the difference is the whole postscript. Removing the thirty-two new
columns from the census held in memory reproduces the *previous* surface
exactly — 139 evaluable, 51 excluded, the same two integers the last campaign
recorded — and yet it reads **67/120**, not the 69/120 that campaign pinned.
So the new family displaced exactly one hit, in the phonology arm. The other
two did not come from the surface at all. This campaign re-parameterised two
of the four kinds the previous one measured, and the census non-regression
check found every spring and overhang column moving on 998 to 1,000 of the
thousand rows while every thicket and erratic column moved on none. A column
whose *value* moves moves both the ranking prior it contributes to and the
set of moved columns a hit is scored against — a third mechanism, distinct
from adding a column and from moving the worlds, and one that only shows up
if the ablation is run family by family.

Removing both families together restores the surface of two campaigns ago —
118 evaluable, 50 excluded — and reads **72/120**, arm for arm 20/20, 4/20,
20/20, 2/20, 7/20, 19/20: identical to the reading taken on that surface then,
and identical to the ablated tally the last campaign measured. On the ground
all three share, nothing has moved across two further census epochs. That is
the closest thing to a control this witness has. It is reassuring about the
scorer and says nothing whatever about the bar.

So this is an eighth canonical reading and the second consecutive
*incomparable* one. It extends the previous campaign's case rather than the
six-epoch series, which stands unchanged at 0.5667 / 0.6083 / 0.6000 / 0.6083
/ 0.6083 / 0.6000. The verdict remains “cannot tell,” the instrument remains
underpowered, and the remedy remains more pairs.

The reusable part is smaller than the verdict and more useful. Ablating the
newest family is the obvious move and it is not sufficient: on its own it
would have reported that the instrument displaced one hit and let the other
two be attributed to the world, which is the wrong story told with real
numbers. Ablate every family that changed since the pinned reading — the one
that was added, and the ones whose values moved — and publish the ladder
rather than one rung of it.

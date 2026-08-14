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

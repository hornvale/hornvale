# The Seedbed

*A seedbed is where you find out whether something will grow. This campaign
built a fourth corpus family, asked the world a question its three siblings
cannot ask, and got an answer that was mostly "we did not really ask".*

## The question the other three families cannot put

Hornvale scores itself against frozen external catalogues in three places.
`tropes/` asks whether the world can **represent** a dramatic situation, and
resolves against the concept registry. `systems/` asks whether the program
**implements** a capability, and resolves against repository facts.
`sentences/` asks whether the grammar can **produce or parse** an utterance,
and resolves against `domains/language`.

All three measure static reach. `docs/audits/trope-matrix.md` says so in its
own header — it scores *representability only*. None of them asks whether the
world, left to run, **produces** anything.

That is the question Epstein and Axtell's *Growing Artificial Societies*
(1996) spends a book on. Their formulation is the one this campaign is named
for: perhaps one day "Can you explain it?" will be read as asking **"Can you
grow it?"** — and what is surprising, they insist, "is not the emergent
macroscopic object per se, but the generative sufficiency of the simple local
rules."

`regularities/` puts that question. It is a fourth family under decision
0135's rule — a different **resolution basis** opens a family — and its basis
is measurement over the committed census.

## Why the freeze had to be structural

A corpus that scores the world must state its claims before it sees the
answers (decision 0016). Every previous family has learned, the hard way,
that a *declared* verdict rots: `tropes/` needed a realization witness
(0577/0581/0583), `sentences/` needed one before it (0330), `systems/` needed
machine-checked anchors (0136). Each fix arrived after the instrument had
already been shipping a number nobody could be wrong about.

So the corpus was frozen in a task that ran **before any evaluation code
existed** — no `meets`, no census reader, no audit. The author could not have
peeked, because there was nothing to peek with. That ordering is the whole
guarantee; an assurance of good faith would have been worth nothing.

It nearly leaked anyway, in a place worth recording: a criterion cannot be
authored without knowing whether a column is a rate, a slope or a label, so
the author read `windows/lab/src/metrics.rs` for semantics — and two rustdocs
there disclose **distributions**, not just meanings. One states its median
outright. Where that happened, the item took a criterion the disclosure could
not move.

## Six criteria that could not fail

The corpus froze at 45 items with ten measurable. Reviews then removed six of
the ten, and the reason each was removed is the campaign's real subject.

- One measured **the same statistic twice** on two species and called the
  second "speciation" — two perfectly identical cultures would have scored as
  divergence.
- One was **satisfied by its own negation**: `climate-displacement-events`
  returns `Absent` only when a world has no occupation records at all, so a
  world where climate displaced *nobody* returns `Number(0.0)` — present —
  and cleared a `present-on-fraction` bar.
- One asserted a bound **below a live domain invariant**: `plate-size-gini
  >= 0.2`, where a terrain test already asserts that median exceeds 0.35
  across twelve seeds. It could not have failed without a terrain test
  failing first.
- One measured **sample size wearing seasonality's clothes**. The metric is a
  circular mean resultant length, genuinely zero under a uniform phase
  distribution — so any positive value looks like signal. That is true of the
  *population* value and false of the *estimate*: for n uniformly drawn
  phases, E[R] = 0.886/√n, which is 0.40 at n = 5 and does not fall below the
  authored bound of 0.1 until n ≈ 78. The metric's own floor admits worlds
  with five. On every world between five and seventy-seven endings, a
  completely unseasonal process was *expected* to clear the bar.

Every one of those six had defensible provenance for its number. Provenance
was the thing being checked; provenance was not the thing that mattered. The
test that would have caught all six is cheap and was adopted late: **construct
the world in which the regularity is false, and ask whether the criterion
reddens.** Each surviving item now states that world in its own note.

## What the census said

Four items survived to be measured. They are three independent claims — two
of them read raid rates off the same fold and correlate at r = 0.999 on this
population, which the report merges and says so.

| item | verdict | measured |
| --- | --- | --- |
| `sug-wealth-skew` | **flat** | median −0.577645 against a band of [−1.2, −0.8]; 17 worlds of 1000 inside it |
| `sug-predation-is-bounded` | grown | 968/1000 in band; observed max 0.4533 |
| `sug-retaliation-deters` | grown | median 0.274309; observed max 0.4229 |
| `sug-credit-makes-hierarchy` | grown | median 88.0 against a floor of 1 |

Two of three claims grew. The one that did not is the substantive finding:
**Hornvale's settlement sizes are systematically flatter than the rank-size
law** — not marginally, and not noisily. The whole distribution runs from
−1.05 to −0.35 where Zipf and Auerbach put the exponent near −1, and the
metric's own rustdoc records that the condensation producing it is
deliberately not tuned to a rank-size target. The corpus predicted the risk
before measuring it.

The passes deserve less credit than their count suggests, and the report says
so in its own voice. `sug-retaliation-deters` had **no reachable failing
side**: the maximum initiator rate over a thousand worlds is 0.4229 against a
ceiling of 0.5. `sug-predation-is-bounded`'s upper pole was never at risk
either; only its 0.02 floor discriminated, excluding 32 worlds. The one band
drawn from an outside empirical law is the one that failed, and the ones
authored as safe floors are the ones that passed — but the separating
variable is not provenance. `sug-predation-is-bounded` also took both poles
from the source. It is **where the band sits relative to the data's mass**,
which is exactly what a blind author cannot know. What a blind author *can*
do is argue from the mechanism that a plausible world produces the reddening
value, and that is the discipline the next corpus inherits.

## The guard the siblings do not need

`tropes/`, `systems/` and `sentences/` all ratchet: a built capability stays
built. A **grown** regularity does not. It is emergent, and any retune of the
history bake can destroy it while every other gate stays green.

So the authored verdict and the computed verdict must agree in both
directions. An authored `grown` that measures flat is a lost regularity; an
authored `flat` that measures grown is stale pessimism, and a real gain must
be claimed deliberately rather than absorbed silently. Both are red.

A second guard follows the same logic one level up. Nine of the corpus's
`deferred` items cite a single idea-registry row, so that row shipping would
mis-verdict a fifth of the corpus in one move — and here a stale deferral
does something worse than misreport: it **withholds an item from
measurement**, since the corpus goes on claiming it cannot measure what it
now can. The check caught a real false status one commit after it was built.

## Saying it where a stranger can catch us

Every earlier mechanism for backing a coverage verdict failed the same way:
the program certifying itself to itself. A registry token, then a
hand-maintained list, then an anchor into code, then an anchor into a test —
each defeated in turn.

So this family's terminal anchor is **generated documentation**. A `doc:`
anchor resolves only against a path `docs/generated-paths.txt` gives a
generator; hand-written prose is refused, because prose asserting a
capability is the old failure in new clothes. The Domesday now prints, under
each scored metric, a sentence rather than a number:

> **Frozen claim** — *Holdings are distributed far more unequally than the
> endowments that produce them* (`sugarscape-1996` `sug-wealth-skew`;
> Ch. II, 'Emergence'; Animation II-3). Predicted median in [-1.2, -0.8];
> measured -0.577645. **FLAT.**

Every part of it is derived. Flip the recorded verdict and the sentence
follows; move the census column and the number follows; and the page names
its own exception — one item was measured before the corpus existed, and the
page says so rather than claiming a blindness it does not have.

## What it measured about itself

The instrument is honest about being small. Four measurable items of
forty-five, three independent claims, and a report that prints observed
minima and maxima so a reader can see for themselves that two of the three
passes had no reachable failing side. Twenty-four items are `absent` and the
report splits them: most are a real absence of mechanism — Hornvale has no
economy, no per-individual wealth, no disease model, and roughly half of
Sugarscape's roster is economic — while six name the instrument that would
settle them and are therefore roadmap rather than gap.

The reverse is equally true and is stated on the page: Sugarscape has no
terrain, no astronomy, no language and no deep time, so nothing in this
corpus scores Hornvale's strongest ground. A single column is one reading
through one biased ruler (decision 0095). The matrix wants a second.

## A postscript the campaign did not arrange

Between this branch's submission and its merge, a plague campaign landed and
moved the census. `rank-size-slope`'s median went from −0.577645 to −0.583393
and the count inside the Zipf band rose from 17 worlds to 23.

The two-way guard did not fire, and that is the result worth recording: the
statistic moved, the regularity did not, and the instrument said so without
being asked. A thousand worlds were redistributed by an unrelated campaign and
the answer to *does Hornvale grow the rank-size law* came back unchanged —
still no, still systematically flatter.

One thing did break: a test of this campaign's own, which had transcribed the
old measurement into an assertion whose stated purpose was proving that
measurements are never transcribed. The generated page had already followed
the census; only the test had not. That is the campaign's argument in
miniature, and it cost a queue slot to learn.

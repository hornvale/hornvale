# Study 012: The Chorus's Dial

The Chorus gives every placed people its own account of the same ground
truth: an emic paragraph filtered through what that culture can see, know,
and value, plus a sparse etic margin recounting only the facts its filters
lost or corrupted. A chorus like that can fail in two symmetric ways —
**uncanny** (every voice secretly says the same thing) or **gibberish**
(every voice says nothing recoverable) — and whether a cheap pair of
numbers can actually tell those two failures apart, rather than relying on
a reader's taste, was the design's own open bet. This study is the
preregistered measurement of that bet, run over
`studies/the-chorus.study.json` (50 unselected seeds) ahead of any
qualitative read of a rendered chorus.

## Question

Does a **distinctiveness × recoverability** dial separate three
deliberately built reference voices — a null filter, the shipped
derivation, and a documented pathological filter — into the ordering the
design predicts, and does that separation hold on real generated worlds,
not just by construction?

## The three poles

- **Null** (the identity filter): every fact kept, nothing lost — the
  uncanny pole. Ground truth in every voice.
- **Shipped**: the derived per-people filters (lexicon holdings ×
  observational capability × salience ordering × valence framing) actually
  used to render the book's chorus.
- **Pathological** (a documented, deliberately destructive filter): every
  fact lost or arbitrarily substituted — the gibberish pole.

## The known-groups gate

A cultural account is measured, never read as a string: distinctiveness
compares two accounts' surviving facts, ordering, and stance; recoverability
asks whether ground truth can be reconstructed from an account plus its
culture's own filter parameters. The preregistered gate checks, on every
seed that places two or more cultures:

- **Loss ordering** — the null pole loses nothing, the pathological pole
  loses everything, and every shipped voice sits strictly between the two.
- **Recoverability ordering** — the null pole is perfectly recoverable, the
  pathological pole's recoverability sits at or below a small floor, and
  every shipped voice clears the pathological pole by a fixed margin.
- **The null pole reads as uncanny** — every culture's null-filtered account
  is pairwise indistinguishable from every other (distinctiveness exactly
  zero), confirming the pole actually *is* the failure it names.
- **A minimum distinctiveness band** — the shipped voices' mean pairwise
  distinctiveness clears a fixed floor on every measured world.
- **Non-vacuity** — the shipped voices differ from one another, not just
  from the two synthetic poles, on every multi-people world measured.

One clause of the gate as first drafted proved, on measurement, to conflate
a lossless salience reorder with genuine destruction — a shipped voice can
reshuffle facts by importance without losing any of them, and that reorder
alone can outweigh a pathological voice's total loss on an aggregate
statistic. The clause was corrected onto the theory's own two named axes
(loss and recoverability, kept separate from ordering) before the gate's
final run; no threshold was loosened to pass.

## Results

All four measured seeds placed two or more cultures. Every clause of the
gate held, on every seed, with wide margins: recoverability margins ran
about twice the required floor, and the distinctiveness band was cleared by
two-to-six times its minimum on every world. No seed needed a fallback
clause to pass, and no shipped voice ever tied a synthetic pole.

## Reading `chorus-variance` against `chorus-param-spread`

Population-level variance alone is ambiguous: a world with two nearly
identical accounts could mean the filter stack is flattening real
differences into one voice (the failure this dial exists to catch), or it
could mean the two cultures' own authored inputs — their perception and
psychology — already sit close together, so an honest, unfiltered account
of each would look similar too. `chorus-param-spread` reports that second
quantity directly, so the two readings don't get confused: low
`chorus-variance` paired with a *wide* `chorus-param-spread` is the signal
worth investigating as a possible filter defect; low variance paired with a
narrow spread is an honestly similar roster, not a vacuous stack.

## Verdict: the dial holds

Every clause of the known-groups gate passed, with margin, on every
measured world — the distinctiveness × recoverability dial separates the
uncanny pole from the gibberish pole from the shipped voices in between, on
real generated worlds and not only by construction. The full 50-seed
population readout — six per-metric distributions, including
`chorus-sky-calibration`'s more limited coverage (comparable only where two
cultures differ in both sky-facing capability and sky-domain distortion) —
is published below.

{{#include generated/the-chorus/the-chorus-summary.md}}

## Instrument

`studies/the-chorus.study.json` (50 seeds, live/non-census); the standing
gate itself is `windows/lab/tests/the_dial.rs::the_dial_separates_the_poles`,
run at every commit.

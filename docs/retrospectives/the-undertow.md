# The Undertow — retrospective

**Merged:** 2026-08-18 · **Program:** Myth, campaign 5

Process lessons only. The results are in
[the chronicle](../../book/src/chronicle/the-undertow.md).

## The subject was falsified twice before a spec existed, and both premises were mine

This is the campaign's defining process fact and it is worth stating in its full
shape, because the two eliminations came from the *same* discipline applied
twice, and neither was a bug in anyone's code.

**First, directed contact.** I recommended it to Nathan as "the falsification
test of the predecessor's pooling result", reasoning that pooling followed from
the undirected freeze. A substrate probe, run before any spec, said otherwise:
divergence falls under *every* arm on *every* rule, and under the multiplicative
rule the one-way arm collapses it further than the undirected edge. Pooling
tracks seam volume, not symmetry. **The deeper half of the same probe was the
useful one**: ~70% of cross-people holders cross the seam more than once, so
victim-versus-raider is a story about a single crossing and single crossings are
the minority case. Freezing a hypothesis on that axis would have preregistered
against an axis the substrate does not sit on.

**Second, the tie-break.** The replacement subject was sharper — the shipped walk
orders by damage and never consults who is speaking, so perhaps pooling is the
ordering rule's doing. Measured: of the 8 cells where the baseline pools and an
alternative could disagree, **0** did. And the structural fact behind the table
is stronger than the table: under descent — the ratio's own *denominator* — all
103,405 holders receive exactly one telling, so **half the ratio was never at
stake for any ordering rule.**

Both premises were controller-authored, both were plausible, and both were dead
in a probe that cost a fraction of a campaign. The rule that produced this
outcome is one line long and worth repeating: **measure the substrate before
freezing the hypothesis, not after.** The predecessor did the same and it is what
made two of its findings possible. This is now the second campaign where the
substrate caught a *premise* rather than a defect.

**What it costs, so nobody thinks it is free.** Two subjects died in about a day
of probe time, and both probes are committed and reusable. The alternative — a
readout measuring a real number against a meaningless axis — costs a whole
campaign and is *not detectable from the inside*, because the number would have
been correct.

## The probe that disproved my scale concern did it by overriding the method I gave it

The best single lesson here, and it goes the other way from the usual one.

Mid-campaign I found what looked like a fatal scale error: the crossing penalty's
ceiling is half a finest rung, and I believed the rung gap was 41.7×, so the
mechanism looked inert against real ladders. I commissioned a probe and **asked
for an analytic estimate** — what numerator would be commensurate. The
implementer did not do that. It re-walked the whole panel at multipliers 1× to
256× instead, on the grounds that changing the scale changes which route wins, so
a headroom ratio is only an estimate.

**The analytic answer I asked for would have said "inert", and it would have been
wrong.** Median headroom by calculation is 71× to 32,129×. The 477 holder-rungs
that actually move are the **tail**, not the median, and a ratio of medians
cannot see a tail effect. Had the probe obeyed its brief, I would have rescaled a
formula that needed no rescaling, mid-campaign, on an artifact of my own
arithmetic.

Two of my three inputs were independently wrong as well: the rung gap is not
41.7× but ranges 2.23× to 530.85× with a **median of 12.56×** across 180 ladders
(I had quoted one seed's number as the world's — one seed puts a half-rung
penalty at 40.6% of its gap, another at 0.094%), and width is *cumulative*, so
"one crossing against one gap" was never the right comparison.

**The transferable rule: when a brief specifies a method and the question is
"does this reach at all", the method is the part most likely to be wrong.** An
estimate over a distribution's centre cannot answer a question about its tail. A
subagent that says so and re-measures is doing the job; that is the third task on
this branch to override its brief and be right.

## Every task contained a defect its own author found, and two would have asserted the mechanism away

Four for four, and the pattern is now so consistent it should be a planning
assumption rather than an observation.

1. **Task 1 — my fixture could not discriminate its own mutation.** The planned
   test gave every raid a human victim and a kobold attacker, so the unsorted
   as-given pair key already equalled the canonically-sorted one; deleting the
   ordering left the test green. The implementer diagnosed it rather than
   reporting a pass, added a raid with the roles reversed, and re-verified that
   the mutation now reddens.
2. **Task 2 — a vacuous guard inside the implementer's own test, found only by
   mutation.** `a_step_within_one_people_pays_nothing` stayed green with the
   same-people guard deleted, because the fixture's rung spacing meant no width
   crossed a boundary. It moved the rung and recorded the measurement in the
   fixture's doc.
3. **Task 3 — the caveat arithmetic was wrong on its first draft, and running it
   is what showed that.** The printed floor caveat summed `capped_endings` across
   all three accumulation rules (42) where the reviewer's figure was one rule's
   cell (14 of 138). Caught by actually reading the test's stdout, not by
   checking the arithmetic.
4. **Task 4 — a control that would have asserted the mechanism away.** The first
   non-vacuity control asserted that no kobold holder keeps an ingroup telling on
   a two-people fixture. It failed 6 of 12: that fixture's second ending dies at
   kobold hands, so a kobold is a *witness* and its line holds a telling that
   never crossed anything — which is precisely the mechanism the hypothesis is
   about. In the implementer's own words, the control "would have asserted the
   mechanism away".

Items 1 and 4 are the same defect class at both ends of the campaign: **a control
whose premise is false reads as a passing test in one direction and a false
failure in the other, and only one of those is loud.** Item 1 was silent and
would have shipped.

## A reconciliation that sounded like diligence and did not survive arithmetic

Two synthetic checks of the telescoping mechanism disagreed — 64,819 of 100,000
against 66,276 of 100,000. The first report reconciled them as "consistent given
different RNG". **That does not hold.** At n = 100,000 and p ≈ 0.655, the standard
error of the difference is 0.2126 pp against an observed gap of 1.457 pp:
**z ≈ 6.85σ.** A seed cannot produce that. The two checks differ in *method*, for
a reason neither report identifies, and the honest statement is that they agree
on the structure (additive ~two-thirds ties, the other two rules zero) and
disagree measurably on the rate.

No shipped artifact was ever wrong — the committed module doc hedges the exact
percentage and pins nothing on it. **The lesson is about how it got through**: it
was a plausible reconciliation offered in the register of care, and the register
of care is exactly what suppresses the check. This is the third instance of the
same shape in one task; the other two were self-caught. **When a discrepancy is
explained away rather than measured away, do the arithmetic — it is usually two
lines.**

## The mechanism a reviewer supplied was better than the explanation it replaced

Task 3's implementer explained multiplicative's zero non-argmin rate by the
penalty's small magnitude. That is a non-sequitur: multiplicative is zero under
the *free* arm too, where the penalty is identically zero. The reviewer supplied
the real mechanism — `gen_span` **telescopes**, so within a same-people, locally
monotone founding-day segment an additive route's accumulated width is determined
by its endpoints and is blind to hop count. The defect's signature is precisely
"same width bits, one more hop", so it is structurally additive-only. That
explains all three columns at once.

Two corrections then landed *on the correction*, and both improved it. The first
draft said `g` is fixed for the whole route "since a route never changes which
witness it descends from" — conflating the witness invariant (real, and a
tie-break field) with a people-determines-`g` invariant (false: the amplitude
reads the *current* teller's people at each hop). The fix is piecewise, and the
implementer additionally required the founding-day run to be **locally monotone**,
which I had missed and which is necessary — a sum of absolute differences
collapses to the absolute total only when the signs agree.

**A review that supplies a mechanism is worth more than one that finds defects**,
and this campaign's most load-bearing paragraph came from one.

## The falsification did not license what the decision table said it would

The one finding that changed what the campaign may claim, and it arrived at
review rather than at measurement. The readout's decision table — written in the
preregistration, before any result — said that falsifying the tercile clause
means the contact term is inert and the derivation decorative. It said that
because it was written for a **uniform** rise. What happened was an **inverted**
one, and an inverted ordering is equally consistent with a *constant* penalty,
because 48% of crossings sit at a single people-pair. There is no arm anywhere in
this campaign separating a derived magnitude from a constant one.

The gloss was withdrawn before it shipped. Had it not been, the campaign would
have published that its own derivation — its entire licence under the project's
no-authored-prejudice constraint — was pointless, on a question it never asked.

**The general rule: a preregistered decision table enumerates the outcomes its
author imagined, and a falsification that arrives in an unimagined shape does not
inherit the table's gloss.** The verdict was frozen and stands; the *reading* of
the verdict was not data and had to be re-derived against what actually happened.

## Tooling: a heredoc that ate line continuations past three green gates

Piping Rust doc/string edits through a **non-raw** Python triple-quoted heredoc
silently consumes every intended `\`-newline continuation — Python's own line
continuation gets there first — leaving one long line with the original source
indentation baked in as literal runs of spaces. The result passes `cargo check`,
`clippy -D warnings` **and** `cargo fmt`. The only thing that shows it is running
the test and reading its stdout. It bit twice in one fix round.

Remedies that worked: use the editing tool for affected string literals, and
write long format strings as one physical line with named `{arg}` captures rather
than hand-wrapping them.

Also recurring, and already documented: restoring a file with `mv` gives it an
old mtime, so the cached test binary is reused and a mutation appears not to
fire. `touch` the file. A reviewer hit this and correctly identified it as its own
tooling artifact rather than a defect.

## Two smaller rulings worth carrying

**A derived description is allowed to change when a field is added.** The plan's
global constraint forbade editing pre-existing expected values, and adding a
fourth policy arm forced exactly that on a label-string assertion. Those are
different objects: the constraint protects *measured outcomes*, and a
description that enumerates fields is definitionally supposed to move when a
field arrives. Every behavioural assertion stayed untouched, which is where the
constraint actually bites.

**Cut a tercile on the entity the effect is levied on, not on the events.** 48%
of all crossings on the panel sit at one people-pair, so a per-crossing tercile
would have reported one pair's behaviour as the world's — the wrong-attribution
shape this thread keeps hitting. Cutting on people-pairs costs badly unequal
holder counts (7,621 / 7,842 / 41,741), which is disclosed beside the result
rather than hidden by a rate.

## What did not survive, that should have

- **`timed.sh` records no exit code**, so a timings row is never evidence a run
  was green. Two rows in this campaign also carry a ref one commit behind their
  own timestamp. Both are known limitations; neither is fixed.
- **The scratch ledger dies with the worktree.** Every ruling in this document
  existed only in `.superpowers/sdd/`, which is git-ignored. Nothing mechanical
  would have caught its loss.

# The Plumb

A plumb line is a fixed reference used to check whether anything else is true.
This campaign is about which of Hornvale's fixed references should stop being
fixed.

The previous campaign shipped a number. `FATIGUE_RISE = 0.3` was the rate at
which every creature in the world accumulated sleep debt — one authored `f64`,
shared by the goblin and the treant and the xorn, on every world Hornvale can
generate. The Wicket converted it into a per-species component keyed on
`KindId`, and the note that prompted it was general rather than local:
essentially everything should vary by species, much of it might vary by
individual, and it is worth keeping an eye out for constants that ought to be
components.

The Wicket fixed that one. Nothing stopped the next one. This campaign built
the thing that stops the next one.

## A magic number is a fidelity ceiling nothing measures

The usual objection to a magic number is stylistic: it is opaque, it repeats,
it should be named. None of that is the objection here, because Hornvale's
constants are already named and documented, often at length.

The objection is that a constant is an **assertion about variation**, written in
a syntax that carries no assertion at all. `const SUSTENANCE: f64` says, in
effect, that the rate at which a body needs water does not depend on what kind
of body it is, or where the body lives, or which body it is. That is a
substantive claim about the world, it is usually false, and it is invisible —
it looks exactly like the constants that are genuinely universal, of which
Hornvale has hundreds. Nothing distinguishes the tick lattice from a
species trait when both are `const NAME: f64 = 0.3;`.

So the fix is not to hunt for wrong constants. It is to make every constant
state which claim it is making. [Decision
0586](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0586-every-authored-constant-declares-its-axis-of-variation.md)
sets the vocabulary, and it is a **ladder** rather than a list:

| rung | varies by | the mechanism it needs |
|---|---|---|
| `universal` | nothing | nothing |
| `per-world` | seed and pins | nothing; the world already knows |
| `per-species` | `KindId` | a species component table |
| `per-people` | what a people does | a kind-to-kind edge |
| `per-individual` | the individual | derivation from `Lineage` |

The rungs are ordered by the machinery a value would need to reach them, and
the top two name machinery Hornvale does not have yet. That is deliberate, and
it is what makes the audit more than a lint: tagging a constant `per-people`
today registers it as a named consumer for the campaign that builds
kind-to-kind edges. The audit's output is the backlog for the arc it sits in.

`tools/plumb` enforces the tag default-deny. An untagged constant in
`domains/*/src` or `windows/*/src` fails `plumb check`, which runs in the
commit gate and in the merge queue's gate phase; `docs/audits/plumb-roster.md`
is the committed, drift-checked roster.

## Why the sweep is total rather than clever

The obvious design is a detector: find the constants that are *probably* wrong
and report those. It was rejected on the campaign's own evidence. `FATIGUE_RISE`
was not found by a sweep, a heuristic, or a test. It was found by a person
reading code in a design conversation. If the campaign could predict which
constants are on the wrong rung, it would not have needed the campaign.

So the tool is not clever; it is **total**. It walks 298 files and judges 686
constants, and most of them declare themselves in one word. The effort goes
where the answer is genuinely arguable — 323 of the 686 sit in files that
mention a species or a body, the creature-modelling middle where `FATIGUE_RISE`
lived.

Totality had a second consequence the campaign did not anticipate. The walk
was first restricted to an allowlist of numeric primitives, which is the
obvious way to find numbers — and under that filter the audit could not see
`REST_BOUT`, the one constant the campaign had already decided to convert,
because `REST_BOUT` is a `TickSpan` rather than an `i64`. The blind zone was
not fixed in size: this project pushes quantities toward typed newtypes on
purpose, so **the audit's coverage would decay as the codebase improved**, and
its silence would read as a clean bill. The filter was inverted — judge every
constant except those whose type is declared a non-quantity — and the
population grew from 610 to 686.

## What it found first was its own predecessor

The audit's first substantive finding is in the code the previous campaign had
its hands in.

`fatigue_rise_registry` is a per-species table. It is exactly the mechanism The
Wicket built, keyed on `KindId`, open to any kind, with a neutral fallback and a
ratchet guarding it. Every kind in it carries the identical `0.3`. Only `xorn`
differs, and only because a creature of living stone explicitly does not tire.

The mechanism is per-species. The values are uniform. A reader of the previous
chronicle would take "the rise rate is now per-species" to mean that rates
differ, and they do not — what shipped was the *shape*, and the shape is where
the difficulty was, but the fidelity gain is still entirely prospective. That is
not a defect and nothing about it is wrong; it is the difference between having
a place to put an answer and having the answer. It leads the roster's 27
fidelity findings, and the campaign's most useful property may be that an audit
built to catch the next `FATIGUE_RISE` caught the residue of the last one.

The other 26 findings are published in the roster with file, line, rung and
reason, and they are findings rather than work. The audit reports; it does not
convert. Which rates in this world should differ by species is a question about
what Hornvale is, not a question the tool that found them is entitled to answer.

## The worked example, and the thing under it

One conversion shipped, and it is the campaign's own worked example rather than
a separate errand.

`REST_BOUT` was a quarter of a *standard* day — the fixed 100,000-tick unit —
governing how long a creature rests. Its own documentation asserted a
calibration: one rest must carry a body clear of the hysteresis band the drive
re-engages inside, so the repayment must exceed 0.1. That repayment works out
to `REST_FALL * 0.25 / L` in local days, where `L` is the world's rotation
period. `RotationPin::PeriodHours` admits worlds from four standard hours to a
hundred. **The calibration held only for `L` below about 30 hours**; past that,
a rest repaid 0.03 against a floor of 0.1, the drive that proposed the rest was
still engaged when the body got up, and the body went straight back down. That
is the seven-minute dozing The Wicket removed with a span, restored by a
denominator.

[Decision
0587](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0587-a-physical-span-is-denominated-in-the-local-day.md)
states the rule the fix follows: a constant modelling a **physical duration**
is denominated in the local day; a constant modelling an **algorithm's own
quantity** — an iteration budget, a capacity, an array length — is not. The
line between them is not where the constant is declared or what it is called.
It is whether the thing it measures is a duration in the world.

Then the interesting part. A sleep, in this code, does not have a length of its
own; it runs until the body's own cycle wakes it, found by a scan. That scan is
bounded by two constants denominated in standard days: it gives up after one.
So on a hundred-hour world in permanent night, a body sleeps exactly one
standard day and wakes into more night — which was true before this campaign
touched anything. The converted rest is `L/4`, which at the same pin is 1.0417
standard days.

The rest is now longer than the sleep. Measured, not predicted: 104,166 ticks
against 100,000.

Both quantities were wrong before, consistently, and their consistency is the
only reason the ordering held. Fixing one exposed the other. The campaign
shipped the conversion anyway, and the comparison is one of blast radii: the
fix reaches every rest on every world turning slower than thirty hours; the
exposure needs the pin extreme *and* permanent night, and costs an hour of
ordering. The constant at fault is the sleep scan's give-up fallback, which is
now a published finding rather than a silent one.

The inversion is held by a test that **runs**, asserting the current and wrong
ordering, so the day someone converts the sleep path the tree goes red and the
test must be deleted. An ignored test was written first and rejected. Nothing
runs an ignored test, nothing validates the roster rows that cite it, and a
registration a reader takes for coverage is a worse failure than an absent one.

## What the campaign learned about its own instruments

Two things are worth carrying out of this campaign that have nothing to do with
constants.

**A verdict vocabulary invites answers in a neighbouring vocabulary that sounds
like it.** Four of the first ninety `universal` verdicts gave reasons of the
shape *"a calibration knob, not a physical constant"* — which answers where the
number came from. The rung asks what it varies along. Those are different
questions, and a confident answer to the first is indistinguishable from an
answer to the second. The tell is a reason built on the word *not*, defining
itself against a category rather than naming an axis. Nothing mechanical
detects this; only reading the reason against the question does.

**A check's silence is a claim about the check's reach, not about the tree.**
The conversion changes behaviour on every world whose day is not exactly one
standard day, and seed 42's is 87,988 ticks. Two possession transcripts moved,
which the artifact machinery reported. A committed byte-golden also moved — 89
of about 410 lines, 22 affect labels, sixteen of them into `Lost`, feeding the
distress classification — and neither instrument the campaign consulted could
see it: the regeneration script does not write byte-goldens, and the commit gate
does not run that test. Both were green and both were honest. Two checks
agreeing is worth nothing when they share a blind spot.

## What the fence is worth

A default-deny gate's cost is not paid once at authorship. Every branch landing
after it pays a share, and the first branch to pay was this one: absorbing 325
commits from main brought seventeen constants written by campaigns that had
never heard of the tag, and the ratchet caught all seventeen. That is the
instrument working on its first contact with foreign work.

It also caught something the ratchet is structurally unable to catch. When the
absorption discarded this campaign's side of three files — provably lossless for
the code, which was purely additive — it deleted two `per-individual` verdicts
along with it, and nothing would have complained, because re-tagging them
`pending(wave-1)` satisfies every check while destroying the finding. **A
default-deny gate defends the presence of a verdict, never its content.** The
verdicts were recovered by name from the pre-merge tree rather than re-judged.

The population stands at 686 constants, all declared: 78 `universal`, 5
`per-world`, 19 `per-species`, one `per-people`, two `per-individual`, and 581
`pending(wave-1)`. The backlog is most of it, and that is the honest shape. A
gate that reddened on the whole population on day one would have been ignored
within a week; a `pending` verdict is a worklist entry, while a wrong
`universal` is this campaign committing the exact defect it was chartered to
remove, with a tag on it saying somebody decided.

The plumb line does not tell you the wall is crooked. It tells you what
straight would have been, and leaves the wall to whoever owns it.

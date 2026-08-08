# The Assay (test-suite census)

An assay does not ask whether an ore sample contains gold. It reports how
much gold per tonne. This campaign moved a slice of the test suite's
world-dependent claims from the first question to the second — and, where a
claim genuinely was about one world's behaviour rather than a population's,
moved it off generated worlds entirely.

## The suite was paying for worlds it threw away

Every gate test that needs a world builds its own. Counted directly: 224
gate tests build to `Settlements` or `Full` depth, the two most expensive
rungs on the `BuildDepth` ladder; only 26 read a committed fixture instead.
Meanwhile `windows/lab`'s census already builds 1,000 worlds every time it
regenerates and maps roughly two hundred extractions over each one — and its
existing consumers pay almost nothing for the privilege. Twenty-four
gate-resident checks in `windows/lab/tests/calibration.rs`, each asserting a
property over all 1,000 census worlds, report no measured cost above the
harness's one-second floor. Contrast that with three test binaries that each
built their own small handful of worlds to check comparable properties:
`hornvale-worldgen::diachronic` (7 checks, 3–5 worlds each, 137.9 s),
`hornvale-worldgen::exposure` (18 checks, 1–9 worlds each, 145.2 s), and
`hornvale-vessel::session` (20 checks, 1–few worlds each, 419.7 s), all
measured on the same host. Twenty-four checks against a thousand worlds cost
less than seven checks against five. The instrument that already paid the
thousand-world tax was sitting beside tests still paying it retail, one
world at a time.

Where that retail cost actually goes matters for reading the comparison
honestly. Profiling a full-depth world build shows climate and settlement
placement consuming 81.1% of the total (3.16 s of 3.90 s); terrain sculpting
takes 12.6%; astronomy, alignments, culture, and deep-time together account
for the remaining 6%. A test that needs only astronomy is already nearly
free — the ladder's shallow rungs cost almost nothing, which is exactly why
the campaign's three retired seed-hunts were expensive: two of the three
build at `Settlements` or `Full`, paying the 81%-expensive tier repeatedly to
answer a question about one property. The census itself runs in release
rather than debug, at roughly 0.7 s per world — 5.6 times cheaper per world
than the same build compiled for a test binary — which widens the gap
further in the census's favour.

## The mechanism already existed

This campaign added no new architecture. `BuiltView::build_to` builds a
world once per seed; `MetricValue::Flag(bool)` already expresses an
invariant; `Extractor::rung()` already lets a metric declare the shallowest
depth it needs, and the runner already builds to the deepest rung any
selected metric requires; `load_rows` already reads the committed
`rows.csv` fixture (decision 0032) rather than recomputing anything. What
was missing was habit, not plumbing: three tests each rebuilt worlds from
scratch to answer a one-world question the shared build already had the
data to answer.

Three seed-hunting tests moved onto that fixture. `hydro_witness` swept 8
seeds looking for a real derivation that could produce every `Hydro`
variant — its own module doc recorded the defect it existed to catch:
`Spring` and `Aquifer` had been unreachable from the real lithology pipeline
for the model's entire life, invisible to every hand-built unit test because
a unit test over constructed input certifies the function, never that
anything real calls it with those values. `exposure`'s toponymic sweep built
up to 9 full worlds looking for some seed where every core toponymic concept
won a placed people's naming. `diachronic`'s crisis hunt built up to 200 full
worlds — its own comment already planned for failure, instructing a future
maintainer to *widen the search range* if none of the first 200 showed a
result. All three answered only "does an instance exist somewhere in this
search," never "how common is it" — the question a thousand-world census
answers for free, once the checks ride along with it.

## What the numbers actually said

**The hydro result is the honest anticlimax.** All five `Hydro` variants —
`aquifer`, `aquitard`, `spring`, `runoff`, `karst` — appear on all 1,000
census worlds. Only one combination of the five is ever observed across the
whole population. That is zero variance, not an interesting distribution,
and it is the honest reading of what the retired 8-seed sweep had actually
been testing: every variant it certified was already showing up on the very
first world of every run it ever performed. The sweep was not measuring
rarity, it was measuring nothing, a thousand times over, without the sample
size to notice.

**The toponymic result sits between the two extremes.** The registry's
seven toponymic-domain concepts are a property of the concept registry, not
of any world — `toponymic-core-size` reads 7 on every one of the 1,000
worlds, because it derives from an unconditional registration call rather
than from anything a seed can move. `toponymic-roots-won` — how many of
those seven concepts actually win a placed people's naming on a given
world — ranges from 2 to 7 with a mean of 5.28, and only 131 of 1,000 worlds
(13.1%) reach all seven. The retired 9-world sweep had asked only "does some
world win every concept" and gotten yes; the census can now say that doing
so is the exception, not the rule.

**The crisis result is the headline.** A live prediction crisis — a
culture's tracked eclipse recurrence missing its own prediction at the
hundredth year — fires on 659 of 1,000 census worlds, 65.9%, with zero
worlds reporting the metric as inapplicable. Two worlds in three hold the
mechanism. The retired 200-world hunt could report only that its search had
terminated somewhere in the range; it could never say whether it stopped at
seed 1 or seed 187, and its own comment anticipated widening the search
range indefinitely rather than ever answering the frequency question. The
census answers it directly, and the answer the campaign's thesis rests on is
this one: the retired hunt was never slow, it was uninformative.

None of the three moved checks needed retuning to pass. All three assertions
are gate-resident, load the committed fixture, and cost nothing measurable
beyond the fixture-load floor calibration already pays.

## A retirement measured on one host, done right once

Reporting a before-and-after cost honestly turned out to be harder than
retiring the tests. The diachronic crisis hunt's own retirement carried a
cross-host comparison error before it was caught and corrected: the
defensible number is the retired test's own cost on the host that measured
it, 13.132 s, against that binary's total of 137.864 s on the same host —
9.53%, not the larger figure an earlier draft claimed by comparing across
two different machines. The corrected figure stands in the decision record
and the test's own doc comment.

The one campaign task that produced a cost claim clean on the first attempt
merged the two tests that had shared a seed set inside the diachronic
binary — both sweeping seeds 1 through 5 for related properties — into one
rebuild. Same host, same commit boundary, both figures named: seven tests at
86.318 s became six tests at 64.560 s; the merged pair itself fell from
46.856 s to 26.974 s. Thirty-two assertions survived the merge unchanged; only
their failure messages gained a prefix naming which of the two original
properties broke, so a merged failure still says which law failed.

## A lint that caught something real on its first day

The campaign closed by tagging every seed-looping test in the tree with a
`claim:` token naming its shape — `invariant`, `structural`, `reachability`,
`rate`, `readout`, or a sanctioned sweep — and shipping a default-deny lint
that requires the tag. The final count, re-measured directly against the
shipped scanner rather than copied from an earlier draft: 286 seed-looping
tests declared across the tree (109 invariant, 74 structural, 32
reachability, 26 readout, 24 sanctioned-sweep, 21 rate, 0 behavior), the
floor exact to the test — 287 fails, 286 passes.

The tagging pass itself surfaced thirty live reachability-shaped
hunts beyond the three this campaign retired — roughly seven times the
original audit's count of three-plus-a-fourth — with the most expensive
concentrated in `domains/astronomy/src/facts.rs` (four separate hunts, up to
200 `generate()` calls each — two sweep `0..64`, two sweep `1..=200`),
`domains/terrain/src/carve.rs`, and
`domains/language/src/naming.rs`. None of those are this campaign's to fix;
they are the follow-on's worked target list, found rather than guessed.

The lint's first real test came unscheduled. Absorbing `origin/main` at the
Task 12–13 boundary pulled in two untagged seed loops from a concurrent
campaign's new tests. The lint reported both, correctly, on the very first
merge that could have hidden them — the strongest validation available for a
guard built to catch exactly this, because nobody arranged it.

## What this settles

Decision 0093 split a seed-sweeping test into two boxes: a census question
(measure a frequency) or a synthetic question (hand-build a world already in
the needed state). This campaign found a third shape neither box fit —
a reachability claim, "does every variant of an enum appear somewhere" — and
gave it a home: a per-variant coverage table over the census (decision
0108). It also found, the hard way, that the synthetic box has a
precondition its own users had not been checking: a behaviour that
re-derives from a generated sky rather than reading a committed fact cannot
be hand-built, no matter how closely its shape resembles a synthetic that
can (decision 0112). And it built the one piece of new machinery the whole
scheme depends on — a live tripwire that rebuilds three fixed seeds every
commit and compares them against the fixture, proven by mutation to fire on
a genuine one-cell drift, at 11.437 s against a 15-second budget (decision
0111).

## Where this leaves the Confidence Gradient

Decision 0097 named a triangle of ways a green suite can still be lying: a
check that can never fire, a check that fires on noise near its own
threshold, and a drift check that has no anchor to be wrong against. This
campaign is a worked instance of the middle corner, closed rather than
merely diagnosed: the three retired hunts were exactly the shape 0097 warns
against — an existence claim decided by whichever seed happens to sit
nearest a hunt's break condition — and moving them onto a 1,000-world rate
or coverage table is what 0097 §2 prescribed and what this campaign is the
first tranche to actually build. The Confidence Gradient chapter carries the
re-score.

## What remains

The scope was deliberately one mechanism plus one tranche, not the whole
suite. Out of scope and promoted to the idea registry: the remaining
roughly 224-test migration this tranche's three retirements sampled from;
`hornvale-book`, `hornvale-vessel`, `hornvale-cli`, and `hornvale-scene`'s
own world-building tests, untouched; a second census `pin_set` for
pinned-regime claims, which the current single-`pin_set` fixture cannot
serve; per-test durations for the heavy tier, invisible to the suite's
existing duration alarm; and the thirty further reachability hunts the
tagging pass found. The retrospective carries the process lessons — several
of them about the same mistake, made six times, by five different agents and
the controller once.

## A postscript this campaign's own theme demands recording

This campaign and its spec (`docs/superpowers/specs/2026-08-07-the-assay-design.md`,
branch `campaign/the-assay`) were named "The Assay" without anyone checking
whether the name was already taken. It was: a shipped 2026-07-21 campaign
about creature potency already holds that title, its own chronicle already
filed at this file's rightful path. Nobody caught the collision at spec time,
plan time, or across twelve prior tasks — it surfaces only here, writing the
thirteenth. It is the same error this campaign's retrospective catalogs six
times over under other names: a claim of absence — here, "the name is free"
— asserted from a search nobody actually ran. This entry is filed as
`the-reassay` to avoid overwriting the original; the branch, the spec
filename, and 110-odd commit messages still say "the-assay," and whether to
rename any of that is left to the close.

# The Assize

A tare is the weight subtracted from a scale so it reads the true weight of
what is on it, not the weight of the container. Five of the world's own
measurement instruments were red, and each had been carrying an unweighed
container for some time — a stale assumption, a sample too small, a rule
that had quietly stopped being true. The obvious move, tried first and wrong
four times out of five, was to assume every red wanted the same tare: more
worlds.

## Two kinds of wrong instrument

Sorted by *why* they were wrong rather than by which one screamed loudest,
the five reds split cleanly into two families.

One family had simply never looked at enough of the world. A gate asserting
that migration fires on a single seed, or that at least half of a
twelve-world sample collects tribute, is a gate built for a distribution it
was never shown. The cure for that family is the thousand-world census,
where the population *is* the instrument rather than a sample drawn from it.

The other family had nothing to do with sample size. A calibration bound
written against a mechanism the world no longer uses; a cost ceiling
compared the wrong way; a fixture whose regenerator never got to run — a
thousand worlds would not have helped any of these, because their defect was
never about how much was measured. It was about *what* was measured, or
whether the check itself still asked the right question.

Only one of the five reds belonged to the first family. Discovering that
took charting all five together before touching any code, rather than
reaching for the census — the four-out-of-five hypothesis that "move it to
the census" is the universal cure was falsified before a single test was
edited.

## What moved to the census

`history_battery`'s seed-42 assertion — that migration events fire at
all — failed on a thoroughly alive world with 420 occupations and zero
migrations, and its panic message blamed a cascade that had nothing to do
with it. A broader probe found why no single-seed version of this check
could ever be trustworthy: migration is bimodal across nearly three orders
of magnitude, and it is legitimately zero on roughly one world in eight. A
nine-seed sweep asserting the same thing per seed was passing today only
because its nine seeds happened to be lucky.

The replacement is a census column, `climate-displacement-events`, read at
the settlement rung. Over the full thousand worlds it is zero on 137
(13.7%), median 10, and reaches as high as 1924 — against the 48-world
probe's 12.5% zero, median 6, and a maximum of 578. The zero rate held up;
the tail did not. A sample cannot see three-in-a-thousand events, and this
one had never claimed to.

A second panel, measuring the accumulated tribute a patron has collected
from its subordinates, could not simply migrate the same way: the quantity
it read is a flow the world computes during its bake and then discards —
the ledger keeps only *who* owes tribute to whom, dated from when the
relation began, never *how much* has changed hands. Three candidate
observables were scored against the discarded flow before a fourth,
plainer one won: the standing count of tribute relations agrees with it at
0.934 across the sampled worlds, comfortably ahead of every fancier
alternative that was tried and set aside. The new column,
`tribute-relations-standing`, reads 0 to 227 across the census, median 73,
and 13 of the thousand worlds hold none at all — a state a 36-world probe
of the same quantity never once produced.

Migrating two columns onto the census is not free: every study that reads
"all metrics" gains two new fields, which reddens every committed census
fixture until the census itself is rebuilt. That rebuild is the campaign's
cleanest piece of evidence that nothing else moved underneath it — the
fixture header grew from 194 columns to 196, and across all thousand rows,
every column the two campaigns shared beforehand came back byte-identical.
The change was exactly as additive as it was designed to be.

## A falsified partition, and an ordering that survived it

The third red was a calibration test that had, in its own comments,
predicted its own failure in advance: it split nine settling peoples into
raiders and non-raiders by whether their authored aggression sat above or
below a fixed line, and asserted that raiders re-seat their subordinates
more often than non-raiders do, by a wide margin. When a later campaign
replaced the underlying mechanism — settlements now draw their raiding
decision individually around an authored mean rather than comparing the
mean directly — the two-set partition stopped describing anything real.
Every people now has some settlements on each side of the line; the
question was never going to keep behaving like two populations because the
world no longer produces two populations.

Measured against the current roster, the sharp partition was broken three
different ways at once, not narrowly: the weakest raider still re-seated at
0.433 against a floor of 0.30, which held; but the strongest non-raider had
been expected to stay under 0.25 and instead reached 0.333 — a ceiling
breached by three of the four non-raiding peoples, not the one the test's
own early-exit logic had reported. The gap between the two groups, which
had once been a clean 2.55-to-1 separation, had collapsed to 1.30-to-1.

What survived the collapse is the underlying relationship the partition was
a crude proxy for. Across the full roster, the correlation between a
people's authored aggression and its measured re-seating rate is still a
strong 0.831. The partition — a threshold fitted to data that has since
moved — is dead. The ordering it was trying to protect is not, and the
calibration was rewritten to assert exactly that: a directional claim set
from the mechanism rather than from any measured value, deliberately weaker
than what it replaces, and honest about the trade.

## A rule that was wrong, not merely unwritten

The two cost gates had been read, on the strength of an earlier campaign's
account, as batteries that panicked before they finished measuring. They do
not: both take and print every one of their five timings before asserting
anything. The actual defect was upstream of any code — the written rule for
telling a contended run from a real regression, "a uniform shift across
every metric is the machine; a local shift is the code," gives the wrong
verdict on real data. A run where genesis time-building cost 2.09 times its
recorded basis while four other metrics sat within a few percent of theirs
reads, under that rule, as a local regression. It is not one — a quiet
machine reproduces the same world in well under a third of the ceiling's
time. The rule fails because the five metrics do not share a resource
profile: only genesis sculpts terrain across a large grid, so it is the
only one a saturated runner actually starves.

The fix was to name each budget's measured basis as a constant, compute a
ratio to it on every run, and score contention by resource class instead of
by uniformity — genesis is the contention-sensitive metric, and the four
scene-document timings are its control group. A genesis breach with the
controls flat now reads as the machine; any control moving reads as the
code.

The corrected rule was used for the first time on the very next heavy run,
and it did not report the machine. It reported one control moving —
indoor-snapshot timing, 2.09 times its basis, reproduced three times on a
quiet box. A commit-by-commit bisection across the whole range since the
ceiling was set attributed the entire increase to a single commit, and it
was not the one guessed first: five candidates were named as likely causes
before the bisection ran, and four of them moved the metric by exactly
zero. The fifth widened how a wall's palette entry is keyed, trading one
shared color per material for a per-cell color lookup on every indoor
snapshot — a real, deliberate feature cost rather than a defect, and its
ceiling was raised to match, with the basis moved alongside it so the
alarm keeps meaning something on every future run.

## A fixture the gate itself had stopped writing

The fifth red was the strangest shape: a generated report,
`the-history`'s committed page, had not moved in a stretch of the project's
history where the world underneath it moved twice. The report's own
generator is the last step of the very test that had gone red — and a test
that panics before its last step never reaches its last step. The report
had quietly frozen at the moment the test first failed, and every ordinary
freshness check available was blind to exactly this: nothing looked
different, because nothing had run to make it different. Removing the
test's stale assertion did not cause the staleness; it only allowed the
report to regenerate, and once it did, eighteen of its twenty-one numbers
changed.

## What the scale reads now

None of the five reds turned out to want the same repair, and the
"move it to the census" instinct that opened this campaign would have been
right for exactly one of them and wrong for the other four — a fixed
literal that had rotted, a rule that had never been correct, and a fixture
whose writer had gone silent are not sample-size problems, and no volume of
worlds would have found them. Two new census columns now carry the
questions the retired single-seed and twelve-world panels used to ask, at a
scale those panels could never reach; one calibration keeps a real ordering
after discarding a fitted threshold the world had outgrown; one cost
instrument now says something true about where a slowdown lives instead of
something plausible; and one report tells the world as it is rather than
the world as it stood on the day a test stopped running. Five instruments,
five different tares, and the underlying world did not change at all.

# The Keeping — retrospective

Process lessons. The product is in the chronicle and in decisions 0098–0103.

## What worked

**A preregistered probe stopped a specced campaign for the price of one test.**
Task 0 existed because the spec's own §4 required a cheap pre-check with
interpretation rules fixed in advance. It ran in ten minutes and found that §3
targeted the wrong one of two identical gates: the planned rewire would have
compiled, passed, held byte-identity, and done nothing. Worse, its null result
would have looked exactly like `H4`, a hypothesis the same document had
preregistered — so the campaign would have concluded "the roster is the
constraint" from a change that could not have moved anything. **Preregistering the
null is what made the null unusable as an excuse.** That is the single highest-value
thing this campaign did, and it was a spec section, not a code change.

**Every re-pin carried a direction argument, not just a number.** When step B moved
four pins, each got a sentence saying which way the value should move and why: the
tropical/polar capacity ratio *must* rise because hot-arid ground is tropical and
the poles stay closed; seed 42 gained the least ground so its settlement count
should hold and only names shift; seed 7 gained the most so its stratigraphy pin
should move most. All three predictions held. A re-pin with a direction argument is
a test; a re-pin with only a new number is a capitulation.

**Nathan's three interventions each caught the same failure from a different
angle** — that a claim of mine was reasoning correctly from a parameter whose frame
did not hold. "I thought cells were 1.6km" caught a scale conflation; "that
calculation looks made up" caught a model that was not its cited model; "will that
hold regardless of obliquity, tidal lock" caught a summary statistic whose
coordinate system only suits spinning worlds. Each took one sentence and
invalidated a paragraph.

## What went wrong

**Four claims of mine survived my own review and fell to a measurement.** The
pattern is specific and worth naming: *reasoning correctly from a stated parameter
without asking whether the parameter's frame holds.*

| claim | how it failed |
|---|---|
| "K is a capacity field" | it is a dimensionless saturation; read the name, not the return |
| "the type-audit never reaches a container's payload" | it does; four findings appeared within minutes |
| "the cold has no claimant" | kobold *prefers* −5 °C to +22 °C; I read its optimum, not its curve |
| "derive the calibration floor from Lieth" | valid only for spinning worlds |

Three of the four are the *same* error in different clothing: **read a parameter,
infer behaviour, skip evaluating the function.** In a model built from response
curves, an optimum tells you almost nothing — the width, the floor and the shape
decide the behaviour. The rule this yields: *never characterise a species, a field
or a metric from its constants; evaluate it over its actual domain and print the
table.* Every time I did that, it took under a minute and overturned something.

**I claimed an artifact was fresh from a check that cannot detect staleness.** I ran
`git diff docs/audits/` and got a clean result, and concluded the type-audit report
was current. It only goes stale *relative to a regeneration*, so the diff can never
see it; the pre-commit hook caught what I had declared verified. The freshness check
is `make rebaseline` **then** diff — never diff alone. This is the "common miss"
CLAUDE.md already names, walked into anyway.

**A stray `cd` sent edits and a verification to the wrong checkout.** A `cd` into
the scratchpad reset the shell to the main checkout, after which one `sed` modified
main and several builds — including a byte-identity check — ran against it. So a
"BYTE-IDENTICAL ✓" result was comparing main against itself and proved nothing.
Main was reverted and the verification redone in the worktree. **A verification run
in the wrong directory is worse than no verification, because it is recorded as
evidence.** Prefix every command with an explicit `cd` to the worktree when a
session touches paths outside it.

**I offered a menu where the answer was "both".** Presenting "adopt Lieth" and "move
the tent into per-species niches" as alternatives was itself a conflation — they are
two *stages of one pipeline*, not two competing choices. Nathan saw it immediately.
A menu is the right instrument for genuine forks and the wrong one for a
decomposition; when two options describe different layers, the answer is always
both.

## Process observations

**Ideonomy earned its keep, and the yields were structural rather than creative.**
Nine passes across the campaign produced four overturns of my own recommendations —
committed locale addresses (killed by "never store a balance"), the roster as
co-requisite rather than primary, "normalize" rather than "desaturate", and the
step ordering twice. The organon that paid best was the **dictionary**: forcing a
definition per term surfaced that `suitability` named a capacity while the function
named for capacity returned a suitability. The two words that would have caught the
error each pointed at the other's referent. No amount of re-reading the code would
have found that; writing definitions did.

**The G3 package order buries the document under review.** Autopilot mandates
flagged items → ledger digest → capture manifest → spec path, so the spec arrives
last and a git-ignored scratch file arrives second. At this campaign's G3 Nathan
read the ledger and could not find `H4`, which lives only in the spec — and I had
cited `H4` by label without stating it. Two fixes worth proposing to the skill: put
the spec path **first**, and never cite a spec-internal label in the package
without expanding it inline.

**Main never moved during the campaign,** so the stage-boundary absorption cadence
had nothing to absorb and its absence is not a finding. Recorded because the close
walk asks.

**A decision was corrected the same day it was ratified, before reaching main.**
Decision 0103's §2 claimed the type-audit could not see container payloads;
implementing §1 disproved it in minutes. The correction is a marked block inside
0103 rather than a supersession, on the grounds that a record which has not landed
is better fixed than superseded — but it is worth noticing that a record can be
ratified, committed, and wrong within an hour, and that implementing it is what
found out. **Ratification is not verification.**

## Owed, and deliberately deferred

**The censuses are stale and this campaign did not refresh them.** Step B moved
world identity, so `book/src/laboratory/generated/the-census/` no longer matches
the code. The refresh was deferred because The Tilth — spec'd as this campaign's
successor — will move identity far more, immediately, and two twelve-minute heavy
runs on the canonical host buy nothing over one. **This is a deliberate, owned
deviation from the once-per-campaign refresh rule, and the reason it is recorded
loudly here is The Siding**, which found the census stale for 139 commits while
every gate ran green. Short, named, owned staleness is a different animal from the
invisible kind — but only if somebody writes it down. The Tilth owns the refresh.

**A second Mac appeared in the timings ledger.** `ambrose` (12 cores) and
`MacBookPro` (10 cores) are both in active use, and `CLAUDE.md`'s contention
arithmetic — "`cpu_ratio` 8.25–8.50 on ten cores" — describes only the latter.
This campaign's gates read 4.14–4.87 on ambrose under desktop load. There is also
no `make ci` baseline for ambrose, so the documented first-run-never-fails free
pass is unspent on that box and should be spent deliberately.

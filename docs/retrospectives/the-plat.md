# The Plat — retrospective

Process lessons, not product; the product is
[the chronicle](../../book/src/chronicle/the-plat.md); the decisions are
0646–0649. The Circuit's third and last campaign, seven tasks (0–6), two stage
gates, one absorption of `main`, and a resumption from `origin` with no scratch
(below).

## Two cost errors in one campaign, pointing opposite ways, with the same shape

This is the campaign's sharpest lesson and both halves are mine.

**I predicted the cost of two audit verbs and was wrong by an order of
magnitude.** Before dispatching Task 4 I wrote, in the brief and in the plan,
that moving `hornvale circuit` and `hornvale underworld` to a full build would
cost "tens of minutes" for the two verbs together. The arithmetic was 874
cave-bearing vertices multiplied by 155.8 ms, the per-call cost of
`occupations_at` that Task 2 had measured. Both numbers were correct. They do
not compose. Task 2's figure came from a test calling that function once per
vertex of a fixture's terrain, against a ledger it had just loaded from JSON;
the panel's loop reuses one built world and one sculpted terrain across all 874
calls. Measured afterwards, twice, on two machines: **32.9 s and 11.2 s**, or
**32.88 s and 11.15 s** on the quieter box.

The error was not free. It bought an hour-long timeout budget on the
regeneration step and a paragraph of prominence in the plan warning that a
correct cost would read as a hang — a warning about a cost that never arrived,
which is exactly the kind of prose a later reader treats as a measurement.

**Lesson, and it is one line: time one invocation before extrapolating over a
population.** A single `time hornvale circuit --seed 42` costs 33 seconds and
would have replaced the whole prediction.

**And then the correction over-read in the other direction.** The implementer
who caught the prediction supported the catch with a second argument: the
`make rebaseline` runs before and after Task 4 had user CPU of 433.002 s and
433.381 s, "a 0.09% difference, so no work moved." That does not follow either.
User CPU for the same command on the same host ranges **288.330 s to 441.095 s
across the last twelve recorded runs** — a spread of roughly ±30%, far wider
than the tens of seconds Task 4 could plausibly have added. A metric with that
much run-to-run spread cannot resolve that delta in either direction, so an
agreement between two of its samples is a coincidence and not a null result.
The honest statement is that the rebaseline pair is *uninformative* about the
change, and the per-verb timings are what carry the finding — they do not need
the CPU argument and are weaker with it attached.

Two errors, one shape: **a number treated as more transferable than it was.**
Mine chained a coefficient across harnesses; the correction's read two samples
of a noisy aggregate as a null. Recording both together is the point — the
campaign found the second error only because it had just been burned by the
first, and neither would have been noticed by a reader checking whether the
conclusion was right, because both conclusions were.

## Pre-dispatch verification of the brief found defects in every brief it saw

Three briefs were checked against the code before dispatch — Tasks 4, 5 and 6 —
and they turned up **3, 5 and 4 findings** respectively. Not one brief was
clean. The check has now earned its cost three times out of three, and two
things about *how* it earned it are worth carrying.

**The highest-value findings came from grepping the observable, not from
reading the functions the brief named.** Task 4's brief listed two doc comments
to update when a verb changed its build depth; grepping `BuildDepth::Terrain`
across `cli/` instead found a third site the brief did not name — a sentence in
`hornvale help` that the task made false. The brief's enumeration is the thing a
brief gets wrong, so the check should not be an enumeration of the same shape.
Task 6's brief told the sweep to fix a sentence in a predecessor's chronicle
that **does not exist in that form**; reading the paragraph rather than grepping
the paraphrase found three stale claims in it, one of which — a mechanism
sentence, phrased as how the walk works rather than as what it cannot do — the
brief never named and a reader would most likely carry forward as true.

**One finding could have produced a wrong green, which is the category that
justifies the whole check.** Task 5's brief asked for a fixture helper to be
lifted out of existing code. The existing code reads `descent[0]` behind an
explicit comment saying why; Task 5's acceptance walk stands on a *made* rung,
which is generally not rung 0. A helper that inherited the hardcoding would
have silently answered about the wrong level, and the test would have failed as
though the verb under test were broken. A brief that says "lift this into two
small functions" is asserting that the thing lifts.

**Lesson: verify a brief against the observable the task changes, not against
the functions the brief lists** — and read the passage a doc sweep names rather
than grepping the claim it paraphrases, because a paraphrase that matches no
line reads exactly like "already fixed".

## A ratchet absorbed mid-campaign hit twice and behaved differently each time

`main` brought a new lexicon guard into this branch at the Task 3 → Task 4
absorption: a per-file count of tokens containing "cell", waivable per line
with a `// lexicon:` marker naming the sense. Both encounters taught something
and the second is the one that matters.

**The mechanical trap.** `cargo fmt` relocates a trailing comment off **any line
ending in `{`** — a `match` scrutinee, a function return type, a `let … else {`.
The marker lands on the following line, where it waives nothing, and the
resulting failure names a file whose waiver is visibly present two lines away
and does not look like a formatting problem at all. Markers on lines ending in
`,` or `;`, and on `///` lines, survive. The structural fix is better than
chasing the formatter: name a tuple alias so no signature line spells the word,
and bind to a local before a `let … else`.

**The real finding is what happened when the trap fired the second time: the
guard passed anyway.** In Task 5 the formatter relocated three markers onto
lines where they waive nothing, and the check stayed green — purely because a
fixture refactor in the same task had freed seven tokens elsewhere in the file.
A four-token debt sat under a green check and was found by inspection rather
than by the instrument.

**Lesson: a *count* ratchet has slack, and slack masks an unwaived line.** The
guard is not wrong to be a count — a count is what lets a refactor land before
its waivers, which is exactly the transient headroom that saved Task 5's commit
— but a green count is not evidence that every line is waived, and nothing in
the tool says so. The campaign measured the current slack (14 tokens across the
two files it touched) and deliberately did **not** close it: lowering the
inventory is one edit, and it would redden every parallel branch currently
holding those files at the recorded number, which this campaign does not get to
impose on branches that did nothing wrong. That is a real cost of a shared
ratchet with per-file counts, and it is recorded rather than paid.

## The campaign was resumed with no scratch, and the recovery was luck

This campaign was picked up from `origin` in a fresh checkout. Task 0's
baselines — the two audit panels, the client fixtures and the stream manifest,
captured before any grammar file was touched — live in git-ignored scratch and
had died with the previous checkout, so the comparison Task 4's own branch table
depends on had no left-hand side.

They were reconstructed exactly: both panels are byte-identical between Task 0's
commit and `HEAD`, so `git show <sha>:<path>` reproduces the intended baseline
rather than an approximation of it, and the branch table ran verbatim.

**That worked because the panels happened not to have moved, which is a fact
about this campaign and not a property of the process.** Had Task 2 moved a
panel — which is precisely what Task 2's own branch table exists to detect —
the baseline would have been unrecoverable and the check silently weaker, with
nothing saying so. A baseline captured for a comparison and stored where it
cannot survive an interruption is a baseline that has a survival assumption in
it. The cheap habit: if a baseline is a *committed* artifact at a known commit,
record the commit rather than the copy.

## A stage gate bounced and the queue said it was still queued

The second stage gate was refused because `main` moved twice while the request
sat in the queue. The queue's own state column still read `queued`; the refusal
was visible only by running the mouth directly against the branch. **Nathan
noticed before I did**, while I was reading the queue row and believing it.

The mechanism is understood and is not a defect in the mouth: it fast-fails
outside the claim, so a verdict is about the moment it was taken. What this
campaign adds is that the *queue row* is not a report on the request's fate —
it says what the row says, and a row can be stale in the direction that looks
like progress. The instrument that answers "is this candidate still admissible"
is the mouth, run now.

## The seam fallback was written before anyone knew it would be the only arm

Task 5's plan wrote three acceptance walks and, for the ruin walk, a documented
fallback: if no reachable column of seed 42 is abandoned, enter through the test
seam with an abandoned origin and say so. That was written as a contingency.

It is the only arm that exists. Seed 42 has 26 cave-bearing columns carrying a
cut rung, 24 inhabited and 2 abandoned — but only 5 of the 26 have a mouth both
open and unbarred, and all 5 are inhabited. There is no reachable ruin on this
world at all.

**Writing the fallback was right, and the reason is worth separating from the
luck.** A plan that had asserted "the test scans the 26 and finds an ended one"
— which the spec's acceptance section did assert — would have produced a red
test that read as a defect in the prose it was testing, and a fix round spent
looking in the wrong place. The fallback converted an unknown about the *data*
into a stated branch with a reason, and the branch is what made the measurement
legible when it came back the unexpected way. The finding itself is a product
finding and lives in the chronicle and the registry, not here.

## Where the ledger's follow-ups landed

| follow-up (ledger) | outcome |
|---|---|
| Lower the lexicon inventory to live counts — 14 tokens of slack across `session.rs` and `underground.rs` | **Left, deliberately** (ruling #10, above). One edit, but it reddens every parallel branch holding those files at the recorded number. Needs a quiet queue, not this campaign. |
| No reachable ruin on seed 42 — is an abandoned column ever reachable on any seed? | **Carried as a registry row** (`MAP-abandoned-column-may-be-unreachable`), with the measurement, the open question, and the shape of the answer (a loop over seeds). Also stated in the chronicle and in decision 0649's consequences. |

## The predecessor's inherited list

Named rather than silently skipped, per the standing rule that an inherited
finding is a checklist item and not a background condition.

| inherited from The Brattice | outcome here |
|---|---|
| The `brattice.rs` split — the file is over a thousand lines and the solver is its natural first split | **Not taken, and made marginally worse.** This campaign edited `worked`/`admissible`/`stamp` in that file to read a level's origin. It did not grow it much and it did not split it. |
| `bfs`/`shortest_path` duplication in the solver | **Not taken, and widened.** `plat.rs` adds a *fourth* unweighted traversal of the plan graph beside `brattice.rs`'s two and `circuit.rs`'s several. It is a different question (all-pairs distances for a median, not a keyed reachability search) and it is still a fourth queue over the same adjacency. A shared traversal over `DescentPlan` is now the obvious extraction, and it was out of this campaign's path. |
| No synthetic fixture forces the `NoRoom`/`Unsolvable` rollback paths | **Not taken.** Untouched by this campaign; `Unsolvable` is still exercised only where the panel's data happens to produce it (twice, on seed 42). |
| `Body.keys` tagged `bare-ok(count: keys)` when the shape is a bitset | **Not taken.** No pub boundary this campaign added needed a new tag at all. |
| Two unwitnessed determinism/claim halves | **Not taken.** Out of path. |
| The gallery 45↔46 write-count wobble | **Diagnosed, and it is not a wobble.** Task 3's first regeneration reported `book/src/gallery/ 45 57` against a committed 46 — because `deno` was not on the shell's `PATH`, so the bundle step was skipped and one file went unwritten. With `deno` on `PATH` the manifest is byte-identical. So the "wobble" is a *tool-absence* signal that the write-count manifest reports as a count drop with no content diff, which is a genuinely hard thing to read. Recorded so the next campaign checks `deno` before hunting a generator. |

## Deferred minors, with the cost named

**Closed inside the campaign:**

- Two doc comments asserting that the shipped path constructs no overrides and
  only ever produces found rock — found by Task 3's own opening grep, which was
  written to look for exactly that claim, and both rewritten in the task that
  falsified them.
- A doc paragraph pricing the ledger-derived writer as future work, rewritten to
  point at the writer that now exists and to say precisely which caller uses it
  and for what — because the paragraph's actual subject (a different lattice)
  did **not** move, and a blanket "this is done now" would have been wrong.
- A sentence on the committed circuit panel saying the production walk reaches
  no door, which this campaign made false; corrected in its source in Task 6
  while the freshness sweep was open.

**Still open, each with its cost:**

- **`column_origins` re-derives the whole ledger's occupations per call.** One
  call per delve is the accepted price; the two audit verbs pay it once per
  cave-bearing vertex, which is most of their now-33-second and 11-second wall
  times. Cost: a verb whose cost is linear in the world's history, for a
  question that is per-column.
- **The panel's new section costs ~101 s in the stage gate** — it sculpts a
  globe and sweeps 874 columns because that is what witnesses three
  preregistered readouts. Left unquieted (ruling #9): quieting a preregistration
  witness is what the guard exists to make deliberate, and 10% of a gate nobody
  waits at is the wrong side of that trade. Cost: the slowest single test in the
  stage gate.
- **The walk still reads no run, junction or drawn character.** Narrowed by this
  campaign, not closed: a vertex the lattice says was cut, where no people ever
  settled, is still entered as a wild cave. Cost: two descriptions of the same
  underworld that agree on origin and on nothing else.
- **A count ratchet's slack** (above): 14 tokens, measured, not closed.

## Do differently next time

Time one invocation before extrapolating a per-call cost over a population —
and when correcting someone else's extrapolation, check that the metric doing
the correcting can resolve the delta being argued about. Both halves of that
sentence were learned here, from opposite directions, in the same task.

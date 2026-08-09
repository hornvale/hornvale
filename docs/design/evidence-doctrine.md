# Evidence Doctrine

**Status:** Standing design note (2026-08-09). Not a decision record — it
argues a habit, not a rule, and nothing enforces it. Grew out of campaign The
Range (`docs/superpowers/specs/2026-08-08-the-range-design.md`), whose founding
finding was a mechanism that had been mutation-proven to work and could not
move a world.

This note is about **what a piece of evidence actually establishes**, as
distinct from what it appears to establish. It governs two habits that recur
across every campaign: reading a green check, and attributing a measurement to
a cause. It sits beside `kernel-units-doctrine.md` as a cross-campaign note —
it grew out of one campaign but binds none of that campaign's product.

Two theses. The first is common and cheap to defend against once named. The
second is rarer, more expensive, and the reason this note exists at all.

---

## Thesis 1 — a check drifts one level away from the thing it protects

**General form.** A check is written against a *proxy* for the thing that
matters, because the proxy is the surface that was convenient at the time: a
readout instead of the identity path, a wrapper instead of the writer, a count
instead of a discrimination, wall time instead of the outcome. The proxy and
the thing agree at the moment of writing, so the check is correct. Then the
code moves and they stop agreeing. Nothing goes red, because the check was
never watching the thing — and its *name* still claims it is.

This is not a rare accident. The Range found the shape at least six times in
one session, in unrelated code written by unrelated campaigns and by the
campaign itself:

| Check | What it looks like it guards | What it actually watches |
|---|---|---|
| The Warren's realm gate | where a kind can live | `per_species_suitability`, a readout whose only production caller is a report. One line moved the readout 93.87 points and the committed world **zero bytes** — same hash, same 7 764 facts, same flagship village. The placement path (`per_species_capacity_at`) took no realm parameter at all. |
| The peoples-program probe-validity ladder | that a mechanism reaches the world | its top rung reads "the readout differentiates the axis", so a readout-only mechanism scores a **perfect 4**. The Warren performed the mandated mutation step correctly and passed. The ladder had no rung for *reaches world identity*. |
| The canonical-host guard | that a canonical artifact is authored on the canonical box | `heavy-run.sh`, the *wrapper*. `regenerate_occupancy_readout` is a plain `#[ignore]`d test and authors wherever it is run; invoking it directly on the Mac produced a committed artifact with no guard in the way. (Regenerated on lefford and diffed byte-identical, so no harm — but the agreement is now measured rather than guarded.) |
| `a_stale_claim_is_taken_over_rather_than_waited_on` | that takeover does not wait | `began.elapsed()` from the top of `acquire_at`, which includes **building** the `ClaimInfo` — four subprocess spawns (`hostname`, `date`, `git` twice). The name and the panic message both say "takeover must not wait"; the number is time spent constructing a claim. On a quiet box it passes for the wrong reason. |
| `compared > 0` (this campaign's own anti-vacuity guard) | that a bit-identity assertion is non-vacuous | the *number* of comparisons. `an_absent_affinity_is_bit_identical` compared two identical inputs and the guard was satisfied. A guard that counts is not a guard that discriminates. |
| `docs/timings.md` | the cost of a run | wall time, with **no rc column**. A red gate and a green gate are indistinguishable rows. The ledger now carries at least one row for a failed run and one for a contended one, and nothing marks either. |

Two further instances in the same shape, recorded because they are open:

- `climate-displacement-events` and `tribute-relations-standing` are tagged
  `Extractor::Settlement` but read history facts, which `stage("deep-time")`
  emits past the `depth <= Settlements` early return. They are correct today
  only because every study that selects them uses `"metrics": "all"` and
  therefore builds `Full`. A study selecting either **alone** would report
  `Absent` on every world — silently empty, not loudly wrong. A rung tag is a
  claim about what a metric needs, and nothing checks it.
- `gaps_have_reasons` walks a hand-maintained, unlinked allowlist of markers.
  It is green by luck, not by construction.

`CLAUDE.md` itself carried the shape, in its subtlest form. One paragraph said
censuses run on lefford; another said the sanctioned refresh is "local", naming
no host. The second sentence was not false — it was written from the canonical
box's point of view, where the run *is* local, and it silently changes meaning
depending on where it is read. The guard fails closed on the hostname; the
prose does not know where its reader is sitting. The Range read it from the Mac
and was refused. A doc that is true only in the context it was written in is a
check one level away from its reader.

**The question to ask.** Before accepting any green check as evidence:

> *What exactly would have to change for this to go red — and is that the
> thing I care about?*

Answer it in one concrete sentence naming a source edit. If the sentence
describes a readout, a wrapper, a count, or a duration rather than the property
in the check's name, the check is one level away. If you cannot produce the
sentence at all, the check may be incapable of failing.

**And then perform it.** Naming the mutation is the cheap half; running it is
the half that finds things. Task 3's reviewer applied exactly this method to
three tests and got three right answers — and missed the fourth, because there
is no mutation that *should* turn an absent-affinity test red, so nothing
prompted the question of whether it *could*. **A test that cannot fail has no
natural mutation to try against it, which makes this class invisible to
mutation-based review specifically.** That is a real limit of the method, and
it is why the question above is asked in prose before any mutation is chosen.

The repair pattern, from the fix that worked: an absence-is-a-no-op check
became `None` versus an explicit `Some(default: 1.0, by_biome: [])` **plus** a
requirement that a non-uniform affinity differ from *both*. The first clause
alone was the same trap in a new shape; the second clause is what made the
first capable of failing.

The residual, stated because half-repairs read as whole ones: that test is a
*relative* check between two branches. Break both to the same wrong constant
and it still passes. It does not pin the absolute value.

---

## Thesis 2 — a correct method can reach a wrong answer when the environment is an unexamined variable

This is the harder half. Thesis 1 is defeated by a habit; this one is not
defeated by any habit the project currently has.

Sixteen of the campaign's seventeen defects were ordinary: claims written into
prose that nobody executed. **The seventeenth was investigated properly and the
conclusion was still false.**

The claim: a `census_claim` timing test was green at one commit and red at a
later one, so the campaign that landed between them broke it. The evidence was
not casual. The failure was reproduced in isolation. It was bisected to a
commit range. It was cross-checked against a second host. Four true facts
supported it — green at the earlier commit, red at the later one, the merge
resolution provably could not reach `census_claim`, the intervening campaign
changed the dev profile workspace-wide, and the test is timing-sensitive with a
Linux/Darwin split in its own file.

The cause was **442 leaked ripgrep processes**, every one parented to a single
editor extension host that had been up five days, ages spread from under two
minutes to nearly two hours, spawning continuously and never reaped, ~1 % CPU
each, summing to 589 %. Load average 615.52 on ten cores. The test asserts wall
clock under one second across four subprocess spawns.

The natural experiment, once the leak cleared, is unusually clean:

| | before | after |
|---|---|---|
| load average | 615.52 | 5.26 |
| `rg` processes | 442 | 0 |
| four bare-shell spawns | 1.177 s | 0.031 s (38×) |
| the test | 3.5–4.1 s, FAILING | 0.04 s, PASSING (3 runs, 7/7) |

**No code change. Same commit, same binary, nothing edited.**

Two lessons, and the second is the durable one:

1. **"Reproducible in isolation" is not "independent of the environment."** A
   single-process run still competes with 442 strangers. Isolation controls for
   *your other work*, not for the machine.
2. **A before/after that straddles an invisible load spike is not a
   before/after.** A clean bisect across a confound is still a confound. This
   is the project's already-named top failure mode — the right measurement with
   the wrong attribution — committed while narrating that very failure mode.

**What broke it was a one-line question about a premise nobody had measured:**
"ambrose is actually an MBP too." That killed the platform story, and with the
story gone there was nothing left to explain the split except the box.

That was not an isolated rescue. Two other controller conclusions in the same
session were overturned the same way, both by one-line questions requiring no
knowledge of the code:

- *"Why is this a seed sweep and not a census test?"* — which turned out to be
  prescribed by ratified decision 0097, whose own worked example is **that very
  test**, named in the test's own doc comment four lines above the code that
  had just been read. Three options had been put to the owner and none of them
  was the decided one.
- *"Why would this add any time whatsoever to the census?"* — against an
  estimate of ~105 min added to a 15-min census. The per-seed figure being
  multiplied was almost entirely world-build cost, which the census **already
  pays**: 105 metrics declare rung `full`, the runner builds each world to the
  maximum rung any selected metric declares, so a new `Full`-rung metric costs
  zero additional world-building. The sign was inverted — converting *removes*
  94 s from the gate.

**The rule.** When a diagnosis rests on a premise nobody measured, *the premise
is the thing to test first* — not the next hypothesis. Every one of these three
took seconds to check once asked. The defects were not in reasoning from the
premises; they were in never testing the premises. The premises to suspect
first are the ones that feel like background: what machine is this, what else
is running on it, what does this cost already, and has this been decided
before.

**Operationally, for this repo:** `CLAUDE.md` already documents this blind spot
for `make ci` ("run on a quiet box and distrust a red alarm from a busy one").
It applies to `make gate` identically, and to any assertion with a wall-clock
bound. Nothing enforces it for either. Before attributing a timing to code,
record `uptime` and the core count; if load exceeds cores, the number is about
the machine. Four measurements from that session are contaminated and must not
be cited as evidence of anything: a 1573 s gate at `cpu_ratio` 0.56 now sitting
in `docs/timings.md` as a legitimate row, a 1238 s gate reported as drift
against the ~15 min baseline, a 20.6 min gate flagged as a possible regression,
and a one-hour build timeout blamed on a cold `target/`.

---

## The distribution

Seventeen defects. **All seventeen were in controller-authored text** — briefs,
dispatches, recommendations, ledger entries, plan steps. Zero originated in
implementer code and zero in reviewer findings.

The mechanism is not that controllers are worse. It is that implementer code is
compiled, gated, and mutation-proven, and reviewer findings are re-executed
before they are accepted, while **a brief is prose that nobody runs**. Prose is
the only artifact in the pipeline with no execution step, so it is where
defects survive.

The corollary the campaign kept demonstrating: **executable checks caught what
careful reading did not.** The compiler caught two incomplete file lists. The
gate caught a self-contradictory dispatch. A repo hook caught a banned command
— by firing on the *ledger entry quoting it*, since the guard matches command
text, which is how the defect in an already-in-flight dispatch was found. The
census guard caught a recommendation derived from a stale doc line. A
`--no-fail-fast` pass found twelve failures where every fail-fast gate had been
reporting one.

And in **four cases a subagent executing a step found the defect in the
instruction telling it to do so** — because executing is a different act from
reading, and the brief's author had only read. Those four are cases 1, 2, 4/5
and 8 below. Every dispatch should therefore carry an explicit licence to
challenge the brief on the merits; three of the four came back as challenges,
not as compliance. Two more agents in the same session declined instructions
that were wrong for reasons better than the brief's: one refused to fit a band
from 15 seeds (0/15 gives an exact 95 % binomial upper bound of 18.1 %, "which
is a demonstration of 0097's thesis, not a band") and fixed its rule *before*
unblinding instead; another declined to add a constructor to a struct with
`pub` fields, on the grounds that "advisory enforcement is worse than none in a
campaign about guards that cannot fail."

---

## Casebook

Seventeen defects, in campaign order. Compressed deliberately; each is an
instance, not a story.

| # | Claimed | True | What caught it |
|---|---|---|---|
| 1 | An unmutated before/after must show the fix moving seed 42; identical hashes are campaign-stopping | The bake roster filters `SocialForm::Settled` and both `Subterranean` kinds are solitary fauna, so the fix is byte-neutral on the shipped roster and always would have been. Following the step literally would have manufactured a false red on the campaign's founding prediction | Implementer challenging the brief; substituted a perturbed check |
| 2 | The `git add` list for a one-parameter widening of `from_stores()` | ~11 call sites across 5 files | The compiler |
| 3 | `a_uniform_affinity_is_flat_across_every_biome` documents a gauge property | Every fixture value is 0.5, so a `factor()` that ignores `by_biome` and returns `self.default` passes | Reviewer, on inspection; later mutation-proven after strengthening |
| 4 | Brief test code, ready to run | 8 args in one call and 9 in the next; cannot compile | The compiler |
| 5 | The file list for the same arity ripple, again | ~18 test files under `windows/worldgen/tests/` | The compiler |
| 6 | `an_absent_affinity_is_bit_identical` proves absence is a no-op | The mechanical compile fix made both calls identical, so it asserts only determinism. Would pass if `None` meant "multiply everything by 0.5" | Reading the committed code. **Not** the mutation review, which was otherwise thorough |
| 7 | "Do not regenerate censuses" **and** "a passing gate is your bar" | Contradictory: the census tripwire lives inside `make gate` and fires the instant the world moves | Running the gate |
| 8 | A settlement tally at `BuildDepth::Settlements` | That depth commits no `peopled-by` fact, so the tally reads **empty, not small**. Would have reported "0 arid settlements" as a measurement — a plausible number rather than an error | Implementer executing it |
| 9 | `CLAUDE.md` sanctions a local census | The guard fails closed on the hostname (0063/0079). The doc's "local" was written from the canonical box, where it is true; read from the Mac it means something else. The prose was the defect | The guard |
| 10 | A raw whole-workspace `nextest` invocation given to a subagent | A repo hook blocks it | The hook — firing on the ledger entry that *quoted* the command |
| 11 | A red existence-near-threshold test is a finding about the world, to be re-scoped | Ratified decision 0097 prescribes converting it to a census rate and names **this test** as its worked example; the test's own doc comment says so. 0097 also forecloses relaxing the threshold | The owner's one-line question, then `grep docs/decisions/` — the first habit `docs/CLAUDE.md` names |
| 12 | Conversion adds ~105 min to a 15-min census | Category error: the multiplied figure is world-build cost the census already pays (105 metrics at rung `full`). The sign is inverted — it *removes* 94 s from the gate | The owner's one-line question, then the committed schema's rung column |
| 13 | Gate green | Gate red (`Error 2`). A `GATE_DONE` marker was echoed to stdout while the gate went to a log; grepping the *log* for the marker printed "0", read as "rc: 0" | Reading the wrapper's own recorded rc line |
| 14 | The census-regen commit is complete | Ten golden literals across four test files re-pin by convention after every regen and were omitted from the drifting commit. `make gate` then fail-fasted on the first of them and never reached the test anyone was working on — twice | A `--no-fail-fast` pass: 12 failures, not the 1 or 2 any brief named |
| 15 | "Best seed is 13 at 109/235 = 46.4 %" | Seed 13 has the highest *count*; the predicate is a *ratio*. Best is seed 6 at 50/101 = 49.5 % — the claim misses by 0.5 pp, not 3.6. (The test's panic message reports a count while the assertion tests a ratio) | A subagent re-deriving the sweep |
| 16 | Re-pin the goldens, then run the census | Backwards. All 42 reds are a *header* mismatch, so value drift is unmeasurable until the column exists | Running them |
| 17 | A named campaign broke a timing test; reproduced, bisected, cross-host | False. 442 leaked `rg` processes at load 615 on ten cores. Exonerated | The owner's one-line question correcting an unmeasured premise about the second host |

---

## Practices

Each of these is tied to a case above. None is general advice.

1. **Before writing a check, write the sentence.** One concrete source edit
   that would turn it red. If that sentence names a readout, a wrapper, a
   count, or a duration rather than the property in the check's name, rewrite
   the check. (Cases 3, 6; the ladder; `compared > 0`.)
2. **Then perform the mutation, and prove the mutation applied.** Assert the
   target text was present before substituting; check the binary's mtime is
   after the edit; prefer a positive control column that must move. All three
   were run for The Range's founding null and are why the empty diff is
   evidence.
3. **Make an anti-vacuity guard discriminate, not count.** The working form is:
   assert the two branches agree **and** that a third, deliberately different
   input differs from both. (Case 6.)
4. **Put the guard on the writer, not the wrapper.** A canonical-host check on
   `heavy-run.sh` does not bind `regenerate_occupancy_readout`. The census
   guard, which lives on the writer, refused a wrong host on the first try.
5. **A validity ladder's top rung must be the identity path.** "The readout
   differentiates the axis" is a rung, not the top one. The Range's proposed
   amendment adds: *perturbing the axis changes the committed world.*
6. **Ledger the exit code beside the wall time.** `docs/timings.md` needs an rc
   column; until it has one, a row is not evidence a run succeeded. And never
   grep a log for a marker that was never written to that log — read the
   wrapper's own recorded rc. (Cases 13 and the ledger's contaminated rows.)
7. **Record the environment before attributing a timing.** `uptime`, core
   count, and — for anything with a wall-clock assertion — a bare four-spawn
   probe. If load exceeds cores, the measurement is about the machine. Distrust
   any before/after that straddles them, however clean the bisect. (Case 17.)
8. **Test the unmeasured premise before the next hypothesis.** Suspect the
   background first: which machine, what else is running, what does this
   already cost, has this been decided. (Cases 11, 12, 17.)
9. **`grep docs/decisions/` before proposing any repair to a red test.** Before
   putting options to the owner, not after — a menu with the decided option
   missing gets the best of a bad menu chosen. (Case 11.)
10. **Derive lists from the thing that executes, and positive-control any
    derived table.** The campaign's first extraction of a 33-kind roster
    silently dropped 2 kinds — including the primary occupant — because a regex
    window was too small; it looked complete and 5 of 6 spot-checks matched. It
    was fixed only because it was validated against six independently published
    rows, all six exact. (Cases 2, 5, 15.)
11. **When a plan step predicts an outcome, name the mechanism that would
    produce it and check the mechanism exists.** Both case 1 (a movement that
    could not happen) and case 8 (a tally that would read empty) were
    predictions written without checking the path. Case 8 is the dangerous
    kind: it produces a plausible number rather than an error.
12. **Every dispatch carries an explicit licence to challenge the brief on the
    merits.** Four defects were found by the agent executing the step, and the
    licence is why they came back as challenges rather than as compliance.
13. **Re-pin golden literals in the drifting commit, and run `--no-fail-fast`
    when a regen lands.** A fail-fast gate after a regen reports a failure
    nobody is working on. (Case 14.)
14. **When refreshing a fixture, re-check the directional claim it encodes.**
    A re-pinned witness is legitimate; a re-pinned *claim* is a finding erased.
    (Cases 11, 14.)

---

## Scope limits

Stated plainly, because a note about evidence that overstates its own is
self-refuting.

- **One campaign, one codebase, one session.** Seventeen defects from a single
  execution. Nothing here is a rate.
- **The denominator is unknown.** These are the defects that were *found*. The
  distribution "all seventeen in controller text" describes where defects
  survived long enough to be counted, not where they originate. Implementer
  code that failed to compile never became a defect; it became a compile error.
  The finding is about **which artifacts have no execution step**, not about
  who writes better.
- **Thesis 2 rests on one case.** One clean natural experiment is strong
  evidence that this *can* happen and a poor basis for how often. The four
  contaminated measurements are consistent with the story but were not
  independently controlled.
- **The mutation-review blind spot is one instance.** That a test which cannot
  fail has no natural mutation is an argument; that the reviewer missed exactly
  that test is a single observation, not a measured rate for the method.
- **Nothing here is enforced.** No lint fires for a check one level away from
  its subject. `dead_code` cannot help: `per_species_suitability` is `pub` in a
  library crate and genuinely *is* called. "Is this item live?" and "does this
  item run when the product runs?" are different questions, and no compiler
  lint answers the second. These are habits, and habits decay.
- **This note does not settle anything.** The two open items it names — the
  guard on the writer rather than the wrapper, and the rc column in
  `docs/timings.md` — are follow-ups for their owners, not decisions taken
  here. The `census_claim` latent defect (the label and the measurement being
  different things) is recorded deliberately **unfixed**: it was never broken
  by this campaign, and repairing it here would read as silencing the thing
  that was red.

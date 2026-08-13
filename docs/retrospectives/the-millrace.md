# Retrospective — The Millrace

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-millrace.md): a census restored
from 19,207.751 s to 949.579 s (1.81× *faster* than before the network that
broke it), a determinism-contract change with an empty diff over 2,000 world
evaluations, and three preregistered predictions of which one was falsified
favourably, one held without being able to fail, and one measured a figure an
earlier campaign had correctly refused to guess.

Five implementation tasks, seven review passes, five fix rounds, one
whole-branch review, one absorption of `main`, one canonical census.

This campaign's scratch dies with its checkout. Everything below was promoted
out of it before teardown.

## 1. Five vacuous guards, found and fixed inside one campaign

This is the campaign's most transferable content, and the count is the point.
A vacuous guard is one that cannot fail: it asserts something already true by
construction, so it is green on day one, green forever, and green under the
defect it was written to catch. The project has a memory note for this
("measure, don't narrate the mechanism"; "a guard a comment can satisfy"), and
it did not prevent a single one of these.

**(1) The probe's two claimed corroborations, both against a documented
constant.** Task 2's measurement probe transcribed the two lab metrics'
internals so it could count candidate sets, and claimed the transcription was
corroborated because it reproduced `channel-connectivity = 1.0000`. That column
is a *documented constant*: any transcription, correct or arbitrarily wrong,
prints 1.0. Both halves of the corroboration were vacuous. **Fixed** for the
transect half by pulling two published metric values
(`channel-band-monotonicity-untruncated`, `channel-transect-dry-reach`) through
the public registry surface and asserting bit-equality against the transcribed
sweep — mutation-proved by changing `TRANSECT_STEPS` 48 → 47 and watching it
redden. **Not fixable** for the join half: no non-vacuous published observable
exists for it, and that limitation is recorded in the module doc rather than
papered over.

**(2) A rill tie-break test that would have asserted over an empty
population.** Task 3 was asked to pin two nearest-search tie-breaks with real
assertions. Rather than write the obvious real-world test, the implementer
*measured whether the population existed*: seed 42, level 5, 10,242 queries,
3,352 with two or more candidates, **exact adjacent ties: zero**, closest
non-zero gap 1.48e-8. A real-world rill tie test would have been vacuous on
arrival. Caught **before** it shipped, which is the only one of the five in
that category. The fix was to extract the candidate enumeration so the test can
*supply* the tie.

**(3) The memo-exactness proof, asserting on a vector that is also a
constant.** Task 5's suffix memo needed proof it computed what the unmemoised
walk computes. The controller explicitly warned it not to assert on the metric
*value*, because that value is a documented constant. The implementer correctly
asserted on the per-run intactness *vector* instead — and **the vector is a
constant too**: every walk is intact on every real world, so both sides of the
assertion are `[true; n]`. Mutation-proved by the reviewer: replacing the entire
body of `lab_fold_intact` with `vec![true; verdicts.len()]` left the memo test,
the joins test and the 64-world probe all green with byte-identical output. The
one failure mode the assertion was written for — a suffix's `false` failing to
propagate — is exactly the one it could not see. **The warning was right and
insufficient.**

**(4) A comment claiming a guard that did not exist, inside the fix for (3).**
The same commit carried a comment stating that a future edit "cannot quietly
re-point it at the old rule". Mutation-proved false: swapping the closure back
to the superseded predicate left the test green, because both values are 1.0.
Two of these five therefore live *inside the repair for the campaign's founding
vacuity finding* — the fix for a vacuous measurement shipped two vacuous guards
of its own, on the identical root cause.

**(5) The probe's oracle became the thing it was checking.** Found by the
whole-branch review, in the file whose module doc is a lecture about exactly
this failure. `millrace_probe.rs` took its ground truth from
`net.nearest_line` — which, since Task 4, **is the index**. Probe and index
then rested on the same inequality with the same `L_max`, so a wrong bound
would have been *agreed upon* rather than caught. Structurally invisible to
per-task review: the file was written at Task 2 and invalidated at Task 4, and
no single task's diff contains both halves.

### What generalises

- **A guard's reference must be independent of the thing it guards, and
  independence has a date.** (5) was independent when written. Nothing in the
  per-task review process re-checks a *previously approved* guard against a
  *later* task's change. The only thing that caught it was a whole-branch
  review whose brief said to hunt for this family.
- **"Assert on X, not on Y, because Y is a constant" is half an instruction.**
  In (3) it redirected the assertion one level down onto another constant. The
  complete instruction is *exhibit the failure the assertion is for*: build the
  input that should make it red. The eventual fix — eight hand-built
  `Vec<LabHopVerdict>` chains with expected answers written by hand, no
  `ChannelNetwork` at all, 0.00 s — is both faster and stronger than the
  world-level test it replaced as the proof.
- **The mutation must be applied to the guard's own subject, not to the
  pipeline around it.** Every one of these was settled by a mutation that still
  type-checked, applied at the definition, reverted with a verified fresh
  mtime, and confirmed to recompile (`Compiling` in the output) before being
  called green again.
- **A campaign that hunts a defect family will find that family in its own
  work.** The Rill's retrospective §1 named "a guard whose reference lives
  inside the thing it checks". This campaign inherited that finding, wrote it
  into its own review briefs, and still produced five instances — including one
  in the file documenting it.

## 2. Eight defects traced to controller-authored text

Every defect this campaign found in its own plan text originated in
controller-authored writing — the spec, the plan, the task briefs. The
implementers deviated correctly and disclosed every time.

1. **An invalid `type-audit` tag prescribed by the brief.** The brief specified
   `type-audit: bare-ok(enum: return)` on `BankReading::transverse`. There is no
   `enum` class in `BARE_OK_CLASSES`. It was inert only by luck — `Transverse`
   contains no tracked primitive, so the item never becomes an `AuditItem` and
   the tag is never parsed. The first signature change to touch it would have
   turned a latent trap into a red gate with no obvious cause. Deleted.

2. **A prediction the prescribed arm could not score.** Spec §2.1 predicted the
   duplicate-scan removal would halve *band transects*. The arm the plan
   specified runs all five channel metrics, so its 1.67× is over a mix and the
   transect portion improved by more than that. The prediction is neither
   confirmed nor falsified by the measurement designed to score it — the same
   denominator error the spec's own §1.3 was written about, one layer down and
   in the campaign's favour.

3. **A baseline whose instrument no longer exists.** Spec §2.1's whole cost
   decomposition (build 7.88, lab reads 16.05, total 23.93 CPU-s/world) was
   reconstructed from The Rill's published figures. The campaign's own first arm
   measured 28.74 CPU-s/world for *five metrics plus a terrain-depth build* —
   more than the reconstructed total for everything. Going looking for the
   discrepancy established that The Rill's "attribution harness" **is not in the
   repository**: `grep -rl "attribution harness" --include=*.rs` is empty. It
   died with that campaign's worktree, so the figures cannot be reconciled by
   inspection and cannot be re-measured. Ruling: demoted from baseline to
   historical context, with every subsequent arm taken on one harness this
   campaign owns. *A committed baseline is a claim with a date; a baseline whose
   instrument was never committed is not a claim at all.*

4. **A preregistered hinge that could not swing.** P2 is the campaign's
   headline methodological finding and is written up in the chronicle. In short:
   a floor on `k = L / |candidates|` is a floor on network size in disguise,
   because the denominator is constant and `L` is the quantity under study. The
   discriminating threshold was on the *candidate count*, and better still on
   the delivered count from a *specified* grid — because the grid's cell size
   was a design choice the campaign had not yet made when P2 was frozen.
   **Preregistration protects against metric-chasing; it does not protect
   against preregistering something that cannot be false.**

5. **A wrong file list, corrected by reading the code.** Task 3's brief listed
   integration-test files. `ChannelNetwork`'s `meander` and `trunk_vertex` are
   private, so *no* integration test can build a network — which is why the
   crate's existing `test_network()` helper already lives in-crate. The
   implementer put both tests in-crate and said why.

6. **An unimplementable data structure, prescribed from outside the code.** The
   spec and brief prescribed an epoch-stamped `Vec<u32>` for candidate dedup.
   That needs mutable state behind `&self`, which `ChannelNetwork` cannot hold
   without costing it `Sync`. The implementer deviated to `sort_unstable` +
   `dedup` on a short `Vec`, which additionally yields the ascending order the
   tie-break requires. **And the controller's stated reason for accepting the
   deviation was itself wrong in the other direction**: the reviewer checked all
   four `thread::scope`/`spawn` sites in the workspace and found that no call
   site requires `ChannelNetwork: Sync` or even `Send` today — the lab's scoped
   threads each build their own world inside the closure. The deviation is right
   for two *other* reasons (the sort supplies the ordering an epoch array does
   not; a per-query `vec![0; 3606]` is 14 KB of zeroing against a 4–9 entry
   list). *Prescribing a data structure from outside the code, and then
   justifying the correction from outside the code as well.*

7. **A foot-gun premise that was structurally absent.** Spec §6.2 predicted the
   connectivity repair would make walks chain to the sea, worst case `3,606
   walks × 3,607 hops × 7 probes`, and made the memo mandatory on that basis.
   Measured: deepest walk 10 joins over 64 worlds, mean 0.704. The controller's
   first explanation ("structurally unreachable — `build` claims trunks in
   ascending cell order") was **also wrong**, and the review corrected it:
   ascending claim order gives acyclicity and a *run-count* bound, not a *depth*
   bound. The memo was kept on the corrected reasoning — empirically shallow on
   64 worlds, unbounded in the code — which is a better reason than the one it
   was commissioned under.

8. **A cost prediction about a test-only change, falsified by 21×.** The
   pre-merge fix brief said swapping the probe's oracle to the reference scan
   would cost "approximately what the probe already pays per query". Measured:
   **1.1 s → 23.1 s**, because the reference scan does a per-polyline segment
   projection where the cap sweep does early-exiting dot products. Both the
   `#[ignore]` reason and the module doc now carry the measured number. The
   incidental cost: the reason string is pinned verbatim by
   `cli/tests/heavy_tier.rs`, so changing it reddened the first gate attempt.

The two in this list worth carrying furthest are **(6)** and **(8)**, because
both are the same error: *prescribing, from outside a body of code, a fact that
is only visible from inside it.* One was a data structure; one was a cost.

## 3. Correct refusals, recorded because they are the counterweight

Four times an implementer or reviewer declined to produce a number rather than
estimating one, and each refusal was right.

- Asked to adjudicate P2's discriminating threshold for the *locale* query
  population as well as the lab's, the implementer declined: it had not measured
  that population and would not invent a figure for it.
- Task 4 hypothesised two strengthenings of a thin test (`segment_midpoint_probes`,
  `ranged_offset_probes`), built and measured both, found **both were nulls**,
  and recorded the refutation in the doc comments where it had previously
  claimed otherwise — rather than keeping the claim and the probes.
- Fix round 1 of Task 4 was asked to test the `ρ ≥ π` fallback. Rather than
  contrive it, the implementer proved it unreachable (`π` is a supremum, never
  attained; the tightest case is short by one ULP) and pinned the near-breaking
  geometry instead. It then recorded, unprompted, that the resulting battery's
  `is_some` assertion fires *second* under the mutation and is therefore a
  redundancy guard rather than an independently mutation-proved one.
- Task 1's implementer, asked whether the reconstructed cost model was
  comparable in scope, established what it could (build depth is
  `BuildDepth::Terrain`; the build *is* inside the timed region) and explicitly
  flagged what it could not (whether the reconstructed 7.88 is a Terrain or Full
  cost) rather than assuming either.

## 4. `docs/timings.md` cannot distinguish a failed run from a fast one

**The mechanism, stated correctly, because the campaign got it wrong twice
before settling it.** `scripts/timed.sh:74` captures `rc=$?`. Lines 79–85 then
write the ledger row with exactly **eleven `%s` fields** — `when, label, real,
user, sys, ratio, waited_s, commit, branch, host, cores` — and **`rc` is not
among them**. Line 86 echoes `rc` to *stderr* only; line 87 returns it to the
caller. `docs/timings.md`'s own header names those same eleven columns and has
no `rc` column at all. **The exit code is absent, not forced to zero.** The `0`
that looks like a return code is `waited_s` (`${HV_CENSUS_WAITED_S:-0}`), which
defaults to 0 for any non-queued run.

The consequence is unchanged by the correction: a `make gate` that dies at 2.1
seconds on a stale artifact writes a row indistinguishable from a very fast
green gate, into the one ledger the root `CLAUDE.md` points at as the
authority on cost. This campaign generated six such rows and dropped all six
by hand across two commits.

Two process notes travel with it. First, `timed.sh`'s **own comment at lines
30–39 records that The Cairn already hit this confusion once** — so the
knowledge existed, in the file, and was still re-derived wrongly twice. Second,
the campaign's controller asserted the wrong mechanism ("it records rc as a
literal 0"), a reviewer contradicted it, and it took a third agent reading the
whole script with line numbers to settle it. *Two agents disagreeing about a
file both can read is cheap to settle and expensive to leave.* The fix belongs
to the gate wrapper, not to a campaign about river geometry, and is a
follow-up below.

## 5. Measurement technique worth reusing

- **A preregistered percentile must name which tail is bad.** P2 asked for a
  "95th-percentile-**worst** `k ≥ 2`", and small `k` is the bad direction — so
  the clause is a floor on the **5th percentile of `k`**. Scoring it against the
  literal 95th percentile would have been vacuous, because that quantity cannot
  fall below the median, which already had to clear a higher floor. The probe
  recorded that reading in its own output rather than leaving it to the reader.
- **Isolate matched arms by pathspec, never by a bare stash.** Every before/after
  pair used `git stash push -m <tag> -- <pathspec>` / `apply` / `drop` — the
  repo's hook refuses a bare pop, and a bare stash would have swept unrelated
  files across the arm boundary. The one arm that needed a temporary edit (a
  counterfactual "repair without the memo") was restored from a scratch copy and
  verified by `shasum` equality *plus* `grep -c "NOT FOR COMMIT" == 0`, with a
  `touch` to defeat the stale-mtime trap.
- **Report `uptime` on both sides of every arm.** Two arms in this campaign ran
  at visibly different load (6.09–6.48 versus 7.56–9.46), which is exactly the
  confound that makes a clean-looking ratio meaningless. The control that
  rescued it was that Task 4's *before*-arm reproduced Task 1's *after*-arm to
  within 0.96% — a harness-drift bound measured rather than assumed.
- **Run a mutation against the whole crate, not the new test.** Each of Task 3's
  three mutations reported `198 passed; 1 failed`, so "this contract is held by
  nothing today" was *measured* (the new test is the only objector among 199)
  rather than repeated from the brief.
- **A revert is not verified until the binary recompiles.** Every post-mutation
  green in this campaign was confirmed by a `Compiling` line in the output;
  a `Finished`-only green is a stale binary agreeing with itself.
- **An argument that does not generalise correctly is worth replacing even when
  its conclusion is right.** Task 2 first justified the small candidate sets
  from 14,606 vertices spread *uniformly* over the sphere. Channel vertices are
  on land and dendritic, so that was an order-of-magnitude gesture that happened
  to land. The replacement — the search radius is sub-cell, measured — reaches
  the same conclusion *and* correctly predicts where the result stops holding
  (`windows/locale`, where the radius grows with the distance to the river).
- **`std::time::Instant` is banned in test code**, so no timing lives inside a
  test; every arm was timed with `/usr/bin/time -l` around the whole process.

- **Anti-vacuity inside a tie test.** Task 3's rill tie assertion alone would
  have been satisfied by a constant preference for the query's own cell, so the
  test additionally asserts that a strictly nearer *later* candidate still wins
  and that a candidate answering `None` steps aside rather than winning with an
  absent distance — over both a pentagon (5 neighbours) and a hexagon (6), with
  the degrees confirmed by measurement rather than assumed.
- **A forwarded figure is a hypothesis.** The controller computed the tie
  query's search radius as ~0.96 from the spec's formula and handed it to Task 4
  as a condition to verify. Measured, it is **1.096** — the same conclusion
  (comfortably under the whole-sphere fallback) from a different number, and the
  measured value is what got pinned in the test.

## 6. Findings from the planning pass, promoted because nothing else holds them

These were settled in the campaign's decision ledger before any code was
written, and the ledger is scratch.

- **The campaign brief conflated two functions.** It said the network is
  "already cell-bucketed" and that the nearest-cell index "is already threaded
  into the query path". Both are true of `rill_reading`, which takes an index as
  a parameter and considers exactly a cell plus its neighbours; both are false
  of `nearest_line`, whose signature carries no index at all. Reading the two
  signatures split one scope into a cost deliverable and a contract-only one,
  and it is the reason the index had to be **owned by `ChannelNetwork` and built
  in `build`** rather than passed in. The Rill's own measured null — the
  per-room path went 0.42 → 0.45 CPU-s/world across a 16× densification —
  agreed.
- **A share computed over a retired denominator understated a term by 3×.**
  The brief put `channel-connectivity` at "~7.5% of cost, a slope problem, not a
  size problem". That 7.5% is 5.50 against a *pre*-memoisation total of 72.03,
  and The Rill's own memo had already taken the total to 23.93 without touching
  connectivity — making it **23% of total and 34% of lab reads**, the largest
  identified read term. Same failure family as a stale absolute, one layer in:
  a **live** absolute divided by a **retired** denominator. It would have ranked
  the campaign's tasks wrongly.
- **A census column redefined in place makes its own history a lie.**
  `make census-history` loads a study's git history into a queryable series; a
  column that means two things across that series is indistinguishable from one
  that moved. This is the save-format rule — *deliberate regeneration uses an
  epoch suffix, never a rename* — one layer out, and the metric family already
  carries the precedent (`channel-band-monotonicity` and its `-untruncated`
  companion coexist for exactly this reason). It is why the repair was gated on
  a measured decision rule rather than committed to in advance, and it is the
  argument to reach for the next time a column's meaning is wrong.
- **Two inverse indexes over the same relation, differing in which claim
  wins.** `ChannelNetwork::trunk_vertex` keeps the **first** run to claim a
  cell; the metric's local `lab_run_owner` kept the **last**. Substituting the
  published accessor for the per-world rebuild is right — one implementation
  rather than two — but it is safe **only on the strength of the R-4 invariant**
  that makes the relation functional, and the commit says so. A deduplication
  that silently depends on an invariant is a deduplication that breaks when the
  invariant is relaxed.
- **Mark which task orderings are forced and which are chosen.** Measuring the
  candidate distribution before building the index was *forced* — it was the
  falsification hinge. Landing the duplicate-scan removal before the index was
  *chosen*, for attribution hygiene, so the index would be credited only with
  what it bought. Leaving the two indistinguishable in a plan is how a real
  dependency gets dropped under schedule pressure.

## 7. Small notes

- **A generated artifact merges without conflicting.** The absorption of
  `main` was conflict-free, which means no git hook ran and no drift check
  fired. Regenerating after an absorption is mandatory, not optional.
- **Two independent implementations reproducing a figure is worth arranging.**
  Seed 42's `853 → 2,333` join crossings were produced by Task 2's transcribed
  probe and again by Task 5's in-crate fold — different code, same number. That
  is what made the review willing to accept the transcription's figures despite
  its corroboration being vacuous.
- **Level-5 numbers in test commentary understate the read path by ~4×.** The
  "400 `transverse_at` probes per vertex" comment in `metrics.rs` is a
  `#[cfg(test)]`-only positive control's argument, on a level-5 fixture; the
  census grid is level 6 and the shipped sweep is 98 probes per taken vertex.
  A budget built from that comment would have been wrong twice over.
- **`make ci` was never run during this campaign.** The Mac sat at loadavg
  9–16 for most of it, and the CI alarm cannot see ordinary load — a known blind
  spot. No timing baseline was recorded or re-recorded.
- **The census prediction was made before dispatch and was right.** The
  whole-branch review traced the chain (the confluence repair is unconditional,
  so every newly-traversed hop lands on `from == to` exactly) and wrote: *if the
  refresh is not an empty diff, something is wrong.* It was empty. A predicted
  empty diff is a much stronger result than an unpredicted one.

## Deferred minors, promoted from the campaign ledger

Nothing here blocks the merge; all of it would otherwise die with the
worktree.

- **`windows/locale/src/lib.rs:1033-1039`** — `transverse_of` still inlines its
  own copy of the band expression that `BankReading` now returns. Out of scope
  for the task that found it, no regression, left for whoever next touches that
  file.
- **The join half of `millrace_probe.rs`'s transcription is permanently
  unguarded.** No non-vacuous published observable exists for it, because
  `channel-connectivity` is a documented constant. The durable fix is to move
  the probe in-crate beside `lab_channel_transect_width`, which is larger than
  any task this campaign had.
- **The radius policy is guarded at 1-in-6,175, and the gap is open, not
  closed.** `the_gather_covers_every_line_with_a_vertex_in_the_cap` calls
  `gather` with literal radii and never enters `nearest_line`'s loop, so no
  mutation of the radius policy can ever redden it; the equality test owns the
  policy and sleeps through gather bugs. Both doc comments now say so
  explicitly, in both directions. The remedy — sizing `lat_bands` from `L_max`
  — is registered as its own decision.
- **`clamp(1, 1024)` on `lat_bands`** is an unexercised ceiling; it binds only
  above ~2.1M vertices, four levels beyond the legal maximum.
- **The empty-gather widening (`ρ *= 4.0`)** is untested on a real world,
  because no real network is that sparse. `MIN_SEARCH_RADIUS` exists solely so
  the loop makes progress when `L_max == 0`.
- **`segment_midpoint_probes` and `ranged_offset_probes`** cost ~0.9 s per run
  and caught nothing under any mutation tried. Kept with the refutation
  documented, because the regions are right and a grid change would make them
  fire. A reviewer could reasonably call it dead weight.
- **The confluence collapse is asserted only indirectly.** That every join's
  seven probes are one point is what makes the column read 1.0; a direct
  assertion of it would be a genuine strengthening and was out of Task 5's
  scope.
- **Task 3's four minors**: the rill test pins the extracted seam rather than
  the shipped function (tethered — deleting the call fails `clippy -D warnings`,
  so the gate goes red); `two_line_network` duplicates `test_network` by ~20
  lines; the pentagon/hexagon degree claim is a comment, not an assertion.
- **`millrace_probe.rs`'s module doc heading** says "Four things this file is
  careful about" and now introduces five paragraphs.
- **Hand-running `millrace_probe` now costs ~23 s, not ~1 s.** Nothing in
  `make gate` or `make gate-full` pays it, but expect the number.

## Follow-ups

1. **Make `timed.sh` record the exit code** (§4). Eleven columns, no `rc`; a
   red run is indistinguishable from a fast one in the ledger every cost
   decision is made from. It has now bitten three campaigns.
2. **Take the `lat_bands`-from-`L_max` decision** — it would tighten the gather
   and sharpen the equality test at once, at the cost of a performance
   characteristic. Registered in the idea registry (the grid-sizing row added by
   this campaign).
3. **Re-check a previously-approved guard's independence at the end of a
   branch**, not only when it is written. Vacuous guard (5) was independent when
   approved and stopped being so two tasks later, and no per-task review could
   have seen it.
4. **Move `millrace_probe`'s transcription in-crate** so the join half stops
   being unguardable.

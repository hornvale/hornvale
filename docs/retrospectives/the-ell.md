# The Ell — retrospective

**Completed:** 2026-08-11 (spec
`docs/superpowers/specs/2026-08-10-the-ell-design.md`, plan
`docs/superpowers/plans/2026-08-11-the-ell.md`, five tasks, opened and closed
the same week). Ran under campaign-autopilot. Process lessons only; the product
is in `book/src/chronicle/the-ell.md`, and the two ratified choices are
decisions 0126 and 0127.

## 1. Every review finding originated in the plan's or the spec's text

Four implementation tasks, four rounds of review, and **not one finding traced
to an implementer's judgment**. Each traced to a sentence the plan or the spec
wrote:

| finding | the text that caused it |
|---|---|
| a repl-reachable panic on `sky inf` | the brief's uniform "`.expect("a day value is finite")`" guidance, applied at a stdin-fed site |
| `#[serde(transparent)]` had zero coverage | the plan asserted the wire shape held, and offered "no artifact drifted" as the evidence |
| `wait`'s clock overflow | the same guidance, now at an *accumulation* rather than a parse |
| the unguarded unit crossings | the brief named the emit side and was silent on the read side |
| the founder key is 366× worse | the spec ratified a design from a doc comment's claim rather than from a measurement |

This is the third consecutive campaign with that property. The corollary is not
"write better plans" — it is that **the plan text is the artifact under review**,
and a brief's example code is a design decision wearing the costume of an
illustration. Two of the five above are literally one prescribed idiom
(`.expect(...)`) applied three times in three places where the value's
provenance differed.

**The four correct overrides are the other half of the same fact.** Every task
overrode its brief on evidence and every override held:

1. Task 1: the type-audit tag placement the brief showed was rejected by the
   tool; the split placement (`new` / `day()`) is what it wants.
2. Task 3: reads convert, the domain stays in years — the brief prescribed
   neither, and following it literally would have re-keyed every founder's name
   by accident, one task early.
3. Task 4: the fatal assert the brief asked for cannot be restored while two
   seeds still legitimately collide; restoring it would have reinstated the
   liveness bug a previous campaign was authorized to remove.
4. Task 4: the brief's own test for the new key was **vacuous under the
   signature the brief itself prescribed** — mutating `founded_from` changes
   nothing when the caller resolves the parent and passes coordinates.

## 2. The controller's analysis was wrong twice, and the implementer caught both

The controller published a six-row reader table classifying which consumers of
the moved value must convert. Two rows were wrong, and both were wrong in the
direction that would have shipped a silent defect:

- **`present_day` "does not need converting, both sides move together."** False.
  Its consumers subtract it from a bake year, so leaving it raw mixes units in
  every ruin's age. It converts — and was renamed `present_year`, because the
  name `present_day` on a year-valued function is this campaign's own defect in
  miniature.
- **The almanac converts at the prose site.** Right about *whether*, wrong about
  *where*. The almanac's decoder feeds the same key functions the bake calls
  with years; converting only where the year is printed would have keyed the
  almanac's flesh on days while the bake keyed on years. That is the ell one
  level down, inside the repair.

The generalisable part is not that the controller erred. It is **why the errors
were possible**: the moved value is a `Value::Number` — untyped by
construction — so the compiler finds none of its readers and the classification
had to be done by hand from greps. The controller's own ruling on that table is
the durable one: *classify a reader by the value's **role**, not by whether its
name says "year"*, and convert in **one named helper per crate**, never in N
inline divides. N inline divides is the documented-unit answer decision 0014
gave, which this campaign exists to reject.

## 3. A reviewer's named example was wrong while its class was right

A review reported that the almanac's conquest-victim crossing had no guard, and
demonstrated it: drop the conversion, all 78 almanac tests stay green. The
demonstration was correct and the conclusion was not. The guards live one crate
up, in tests that **are** in the commit gate, and they redden on exactly that
mutation. The review had scoped its mutation to the crate it was reading.

The implementer did the right thing with it. Rather than guard the one line,
it swept the property: **every production call site of every unit crossing,
mutated one at a time, each against the full three-crate suite** — sixteen
sites, sixteen runs, ~170 s each.

```
  16 crossings   5 unguarded   R1 R2 R5 A1 A3
                 1 guarded, but only one crate up (A5, the review's example)
                 3 deliberately unconverted (ordering-only; monotone-invariant)
```

**Five of sixteen, not one.** And the consequential one was not on anybody's
list: `descent::founded_year` feeds the census name renderer, so its blast
radius is a **committed census value**, and no census runs in the commit gate.
Its two plausible existing guards cannot catch it by construction — one counts
zero-gap edges (zero is zero in any unit) and the other keys a map on the year
form (invariant under any injective rescaling).

**Generalisable:** a review finding has a *class* and an *instance*, and they
have different truth values. Answer the class with a sweep; verify the instance
separately; and scope a mutation to the whole gate, not to the crate you happen
to be reading.

## 4. The measurement that mattered most refuted a ratified decision

The campaign's spec ratified the widened founder key from a doc comment's claim
about what an existing function was for, plus a reading of an idea-registry
row's scoring. Task 4 measured it over the census range instead of a handful of
seeds:

```
  key                                colliding worlds   founders dropped
  before  (people,site,founded,ended,peak)     2/1000                  2
  THE RATIFIED DESIGN (founding + 1 hop)     732/1000               1582
  shipped (founding + hop + ended + peak)      0/1000                  0
```

791× worse by founders lost (1582/2) and 366× by colliding worlds (732/2), at
the defect it existed to fix — both axes named, because they differ. It was
escalated rather than silently corrected, because the narrow key's 1,582 "drops"
are only drops if a failed founding and its same-year successor are two
foundings — and that is a world-model question, not a key-selection one.

Three process points, in descending order of transferability:

1. **A five-seed probe could not have seen this.** The plan required the census
   range 0–999 for exactly the reason the previous campaign found its collisions
   at seeds 283 and 705. A measurement that cannot see the thing it measures is
   the vacuity shape this project has now hit repeatedly; requiring the range in
   the plan text is what made this finding possible at all.
2. **The registry row's scoring notation was read backwards** by the spec, the
   plan and the production doc comment alike. "parent-coords 398/2 panics (the
   obvious hop is insufficient)" describes **widening** the five-field key with
   parent coordinates — which is what shipped. It does not describe **replacing**
   the five fields, which nobody had measured. A compressed scoring notation in
   a backlog row is not a measurement; it is a pointer to one.
3. **The falsification was found because the before-arm was re-derived, not
   quoted.** The implementer set its own diff aside and rebuilt the baseline;
   the reviewer went further and transcribed the superseded key from the
   parent commit into its own harness rather than calling the implementer's
   function, and got exact agreement on all three arms. That is what makes the
   791× a finding rather than a claim.

## 5. Overriding a scope recommendation, and how it turned out

The founder-key widening was not part of this campaign's thesis. It arrived
from the board, it is unrelated to the unit defect, and **the controller
recommended against bundling it** on the standard grounds: one campaign, one
thesis, and adding an unrelated root fix makes a reviewable diff unreviewable.
Nathan overruled that on the epoch economics — the campaign was already paying
for an epoch that renames every founder, so the marginal cost of widening the
key inside it was zero, and the window closed when the campaign landed.

**The override was right, and it is worth recording as a calibration datum
rather than as a compliment.** The widening became roughly half the campaign's
value: it produced the second falsification, the identity/discrimination
taxonomy, decision 0127, and the finding that a ratified design was 791x worse
than what it replaced. Deferring it would have cost a second epoch and would
almost certainly have shipped the narrow key, because the narrow key's defect is
invisible without the thousand-world sweep that only a campaign already inside
that code would think to run.

What separates this from ordinary scope creep is a testable property, and that
is the transferable part: **the bundled work rode a cost the campaign was
already paying, and the window was closing.** "One campaign, one thesis" is a
rule about *reviewability*, and reviewability is a cost that can be paid —
separate tasks, separate commits, a measurement instead of an argument — where a
second epoch cannot be un-paid. When those two conditions hold (shared
irreversible cost, closing window), override the scope recommendation and pay
the reviewability price explicitly.

## 6. Two preregistered numbers written without an inventory behind them

**"587 founders" was 45% wrong by the time it was read.** The spec opened on
*587 promoted founders, zero deaths* across five seeds. That figure came from
the prior campaign's tree; on this branch the same five seeds carry **854**.
Nothing was wrong when it was written and nothing had broken since — the roster
had grown, and a count of founders is a function of the roster. The implementer
measured its own before-arm rather than quoting the spec, which is the only
reason the acceptance table is trustworthy: a before-arm quoted from a document
is not a measurement of the tree under test.

**A committed baseline is a claim with a date**, and this project has now been
bitten by that shape from both directions — a previous campaign's size bound was
falsified because its *denominator* had moved while the campaign was parked.
Same lesson: a number frozen in a spec ages against a tree that keeps moving, so
what a freeze must fix is a **definition** ("the count of promoted founders on
seeds 42/7/1000/3/99") rather than a value.

**E4's "only three values move" was written without an inventory, and it
undercounted.** The spec named three moved values and required any fourth to be
reported as a finding rather than re-pinned over. Seven moved. Four are entailed
rather than surprising — the person facts follow from the spec's own §4, and one
bullet reading "the day stamp of every occupation fact" expands to about
fourteen predicates. **The one in nobody's list is `pays-tribute-to`.**
`TributeRelation::since` is a bake year written straight into `Fact.day` at
`windows/worldgen/src/history_emit.rs:315`; it appears in neither the spec's
list nor the controller's reader table, and it was found by grepping the emit
path rather than the predicate list. The conversion is correctly applied and
guarded, so there is no live defect — what would otherwise be lost is the
lesson: **a preregistered containment claim needs an enumeration behind it,
produced by walking the writer, not by listing the values the author happened to
be thinking of.** E4 passing "on the spirit" is the honest score, and it reads
as a finding only because the criterion demanded a report rather than a re-pin.

## 7. Three vacuities this campaign exposed but did not create

Each was green, each had been green for a long time, and each was found by a
different instrument.

- **A guard with no coverage.** `#[serde(transparent)]` is what keeps the fact
  timestamp a bare JSON number rather than a wrapped object. No struct in the
  repo serialized one, so "no committed artifact drifted" was evidence of
  nothing at all — and the attribute was about to enter the task where losing it
  rewrites every saved world. Found by a reviewer asking what the evidence
  *excluded*, fixed with a direct wire-shape assertion, mutation-proven.
- **A test comparing two fixtures against each other.** Four tests in
  `windows/almanac/tests/flesh_id_invariance.rs` asserted that a reconstructed
  occupation matches a committed one — by comparing one fixture against another
  fixture. When the ledger moved to days, the module's stated premise became
  **false** and all four stayed green, because both sides were wrong the same
  way. The sibling case in the same task went red on its own, because there the
  two paths were production and fixture rather than fixture and fixture. **The
  shape to grep for: an invariance test whose two sides are both authored.**
- **A guard entailed by the thing it guards.** `founder_collision.rs` asserts
  that any surviving collision has twin parents. Under the shipped key the
  identity step folds the parent's coordinates, so a handle-equal pair
  *necessarily* has equal parent coordinates: the assertion cannot fail while
  the key is what it is. It is a tripwire for a future **narrowing**, not a
  detector of a new collision shape — and it is now labelled as one, with the
  patch that makes it fire recorded beside it.

## 8. Three stdin-reachable panics, and why the third was structurally invisible

The retype put a validating constructor between ~445 call sites and a value.
Three of those sites were reachable from a person typing at a prompt:

1. `repl.rs`'s `sky <day>` — `inf` parses fine and then panics.
2. `worldgen`'s `observed_phenomena`, behind the repl's `phenomena <day>` —
   found by the implementer sweeping for siblings after the first was reported.
3. `vessel`'s `wait <days>` — **written by the fix for the first two**, and
   invisible to them. `wait` validates its parsed argument correctly; the
   overflow is in the *accumulation* `self.day + days`, which no parse-site
   guard can see. Two `wait 1e308` commands panic the possession loop.

**Generalisable:** a sweep is defined by a *property*, and the property here was
initially "a value parsed from text." The third instance falsifies that framing:
the real property is "a value that can grow without bound across repeated
user-driven operations," which includes values that were validated on the way
in. The second sweep, run on the corrected property, enumerated 211 constructor
call sites, narrowed to 74 non-literal, then to 11 with arithmetic in the
argument, examined ~26, and changed exactly one — with the reasoning for each
site left alone written down. A sweep that changes one site and documents
twenty-five is a stronger result than one that changes five and explains none.

## 9. The decision-number warning fired in flight

The spec said the next free decision number was 0125 and warned — in the same
sentence — that it must be re-derived at the moment the record is written,
because the log had already been renumbered once while this campaign sat at its
spec stop. It happened again during execution: main landed
`0125-github-actions-is-retired.md`, absorbed at a plan-stage boundary, so the
number this campaign needed moved to 0126 while it was running.

A contiguity check asserts the numbers run unbroken from 0001, so a stale number
is a red gate rather than a silent collision — but the cost is a fix round at
the close, and the close is the worst place to spend one. **Re-derive; never
carry.** Verified at write time:

```
$ ls docs/decisions/ | grep -oE '^0[0-9]{3}' | sort -n | awk 'NR!=$1+0 {print "GAP", NR, $1}'
$   # (silent: contiguous 0001..0125)
```

## 10. Two sizing facts that were forced rather than chosen

- **A signature change and its call sites cannot be separate commits** under a
  workspace-wide pre-commit hook: the intermediate state does not compile, so
  the hook rejects it. That made Tasks 1 and 2 large (36 and 28 files) by
  necessity, not by preference, and it is the same seam that blocked an earlier
  campaign's second task for a full round. Plan for one large commit rather than
  two reviewable ones, and say so in the plan so the size does not read as
  sprawl at review.
- **The compiler is the enumeration, but only crate by crate.** Error counts ran
  15 → 8 → 6 → 8 → 6 → 5 → 2 → **79** → 12 → 6 → 0. The 79 is where one crate
  first began compiling and exposed ~35 read sites at once. A "how many errors
  left" progress reading is meaningless mid-sweep; only zero means anything.

## 11. Operational notes worth carrying

- **`git stash pop` is not safe here.** The stash stack is shared across
  worktrees, and another session's work-in-progress was sitting under this one's
  entry. Use `git stash push -m`, `git stash list --format`, `git stash apply
  <sha>`, and drop by verified position.
- **Gate timings landed under host `MacBookPro`, not `ambrose`.** The ledger
  carries **seven** `gate` rows for this branch, of which five are real suite
  runs — 848.5, 986.6, 490.8, 490.8 and 353.4 s — against the `ambrose`
  baseline's 460.8 s. Per the known baseline-forking blind spot, `hostname -s`
  first, and do not rank this suite against another host's file. **The two
  anomalies are worth naming rather than dropping**, because a timings ledger
  read as a list of suite durations will mislead:
  - `3.582 s` at `39badb3a`, `cpu_ratio` 1.29 — nothing ran; not a suite timing.
  - `478.048 s` at `0e7ef367`, `cpu_ratio` **0.89** — 478 s of wall for 427 s of
    CPU, i.e. essentially serial. Whatever that run spent its time on, it was
    **not** the parallel test phase, which reaches `cpu_ratio` 6.3–8.2 on this
    box. The ledger does not record enough to say what it was, and guessing is
    how a timing datum becomes a story. It is left as an open reading, and it
    is the strongest single hint that the suite's wall-clock is dominated by
    something other than test execution.
- **The close broke "run once, inspect many" and paid for it.** The gate's tail
  was captured with `| tail -30`, which is below nextest's summary line, so the
  suite was re-run purely to read `3397 tests run: 3397 passed` — 353 s for one
  line that the first run had already produced and the pipe had discarded. The
  rule exists because an expensive run must emit its own evidence; `tee` to a
  file and grep the file, and never choose a tail depth by guess.
- **`make vessel-check` is RED and was red before this campaign** —
  `pane_plan_marks_test.ts:42`, reproduced identically on `origin/main` in a
  throwaway checkout. It was deliberately not repaired: repairing it means
  re-pinning another campaign's fixture inside an epoch commit.
- **A census refresh is owed.** No census ran, correctly — but the ledger those
  metrics read changed shape, and person deaths exist for the first time.
- **`wait 1e308` takes about five minutes of wall-clock** — 39,684 stirred NPCs
  × `MAX_STEPS = 10_000`. Pre-existing wait-scaling, not introduced here; the
  panic it used to end in is fixed, the cost is not. Relevant to the
  test-duration follow-up, because it is a live example of the shape that
  question is about: cost that lives inside the work, not in the scheduling.
- **The handle change moved exactly two worlds' remembered casts.** Seeds 283
  and 705, both `+1`, symmetric difference 1, and **no world saw a
  substitution** — nobody was swapped for anybody. The ranking key's *value*
  moved for everyone, which is inherent to the epoch, and the *selection* it
  drives barely moved at all. Worth a line because "the key changed" and "the
  cast changed" are different claims and the second one was measured.

## 12. The board's post was right about the defect and stale about the symptom

The founder-key collision entered this campaign from a board post, which is the
medium working as designed — it carried a defect nobody had scheduled, and the
campaign was already paying for the epoch that made fixing it free. But the
post's *symptom* claim ("main panics on seed 2793; the branch panics on 283 and
705") was **stale by the time it was read**: the intervening campaign had
replaced the panic with a fidelity cut, so all three seeds build clean. The
consequence changed and the key did not.

Re-deriving the claim rather than inheriting it is what established that, and it
mattered: had the campaign taken the panic at face value, its acceptance
criterion would have been "these seeds stop panicking," which was already true
and would have passed vacuously. **A board post is a pointer to a defect, not a
description of its current symptom** — symptoms are the half that other
campaigns change.

## 13. The Confidence Gradient

**No bet moved.** This campaign resolves no open question about the world; it
repairs a unit boundary and a key. The chapter's *floor* gained an entry, which
is a different thing: the standing practice section now carries an eleventh
instance, because five of sixteen unit crossings in this campaign had no guard
at all and the most consequential of them reaches a committed census value that
the commit gate never rebuilds.

## 14. Follow-ups, each with a number attached

Recorded as registry rows so they are grep-able; repeated here because a
retrospective is where the reasoning survives.

1. **Person deaths are reachable and unrendered** — 144 facts per world that no
   window reads. `windows/almanac/src/history.rs`'s `founding_sentence` is the
   obvious site.
2. **The identity/discrimination vocabulary applies to four derived keys in
   `domains/history` and only two are labelled.** `material_key` is a
   discrimination key; `layer_key` is an ordering key, a third kind this
   campaign saw one example of and did not try to define.
3. **Two functions produce a founder's handle** — `flesh::founder_handle` and
   `descent::founder_of` — from different fields, with nothing reconciling them.
   They now agree on their founding-side base, which is new, but the census's
   name for an occupation's founder is still not the name the ledger's person
   carries.
4. **The key's discrimination tail cannot be trimmed** on today's evidence:
   `ended` alone 5/1000, `peak` alone 5/1000, nearly disjoint failure sets, and
   seed 447 holding two separate pairs. Every field in the tail is one a future
   campaign can recompute, and each recomputation is a forced epoch.
5. **A profiling campaign for test duration**, pre-authorized for capture. The
   figures this campaign contributes, all on one host: `make gate` at 848.5,
   986.6, 490.8, 490.8 and 353.4 s, against a 460.8 s baseline authored on a
   different, 12-core Mac. **A 2.8× spread on one machine is the first finding**,
   and it says the opening question is how much of the cost is build-cache state
   rather than test work — a distinction the committed baseline cannot make,
   because it records wall time and not what was already compiled. What does
   hold on the fast end is saturation: `cpu_ratio` reaches 8.23 on ten cores, so
   scheduling is not the lever and another `#[ignore]` tier buys nothing.
   `make ci` already writes per-test durations to `target/nextest/ci/run.json`,
   and the committed per-host baseline's history is an unread time series.
6. **`WorldTime` derives `PartialOrd` and has no `total_cmp` companion.**
   `ReferenceElevation` — the kernel newtype this one was built from — pairs
   `PartialOrd` with an explicit `total_cmp` plus `min`/`max` built on it, and
   `grep total_cmp kernel/src/field.rs` returns nothing. The project rule is
   that float sorting uses `total_cmp` with deterministic tie-breaks, and this
   campaign introduced a **new kernel float newtype** without one. **Nothing
   sorts a `WorldTime` today** — no `sort`, `cmp`, `min` or `max` call site
   exists anywhere in the workspace — so this is a shape of risk rather than a
   live defect: the hazard is that the first consumer to want an ordering
   reaches for the derived `PartialOrd`, gets `Option<Ordering>`, and unwraps
   it. Deferred at Task 1 and nearly lost with the scratch.
7. **Three stale figures in the descent path**, all pre-existing: seed 42's
   maximum generations-removed was measured at 52 while `descent.rs:81` and
   `history_units.rs:128` both say 32; the new guard's ceiling of 400 is ~4×
   looser than the roster implies (~92); and `generation_length_of`'s
   `// salt-allow` comment calls its value a day count where the type is
   `Years` and the tests assert years.

# Retrospective — The Panes

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-panes.md): `vessel/session/v1`
gained a `spatial` channel, `vessel/plan/v1` projects a chamber lattice into a
palette plus a dense index grid, the Casement mounts a live map beside its
prose, and the map ships uninhabited on purpose.

## The dominant lesson: every review finding traced to the plan's text

Four findings survived to code review across nine tasks — two Critical, two
Important. **All four originated in my own plan text.** Not one was an
implementer's transcription error. This is now a long enough standing record
that it should be treated as the base rate rather than as a surprise, and the
useful question is no longer *whether* plan text is the defect source but *what
kind* of defect it mints.

| # | Finding | Severity | Origin | Caught by |
|---|---|---|---|---|
| 1 | Unguarded `plan.you` dereference — threw instead of refusing | Critical | plan Step 4 code, verbatim | reviewer probing beyond checklist |
| 2 | Symmetry test blind to the exact mutation it named | Critical | plan test text | reviewer, by mutation |
| 3 | `.casement-map` CSS class collided with `transcript.ts` | Important | plan Step 2 code | reviewer, by mutation |
| 4 | `water !== "none"` against a legend with no `"none"` | Important | plan glyph code | implementer, reading the data |

**Three of the four were invisible to a passing test suite.** That is the part
worth carrying forward. The implementers transcribed the plan faithfully and
their suites went green — which is *precisely why* these survived to review.
A defect that a plan mints and a suite cannot see is not caught by working
harder at either end; it is caught by a reader whose job is to break things.

### What caught them was mutation, not reading

Three of the four were found by a reviewer instructed to **mutate rather than
to read** — delete the constraint, drop the attribute, corrupt the byte, and
watch whether anything reddens. Reading the code confirms it says what it
means to say. Mutating it establishes that something is *watching* what it
says. Those are different claims and only the second one is a test result.

The instruction is cheap and it should be standard in every review dispatch:
*for each guarantee this task claims, name the smallest edit that would break
it, make that edit, and record whether the suite goes red.* A guarantee for
which no such edit exists is unpinned, however green the run.

### Finding 2 is the one to remember, because it was a control that failed

The plan explicitly anticipated the walk chart's headline failure mode. The
placement is `col = 2v + (up ? 0 : 1) + w`, and the `+ w` term cancels the
lattice's row offset; drop it and a symmetric hexagon shears into a
right-leaning parallelogram. The design ledger said so in as many words and
required a **symmetry negative control** rather than trusting a green suite.

The control was written, it was named for exactly this mutation, and **it could
not see it.** It asserted on row *widths* — and row widths are invariant under
horizontal shear. The correct chart indents `[2,1,0,1,2]`; the sheared one
indents `[0,0,0,2,4]`; both have identical row widths. The discriminator is the
**leading-space mirror**, not the width.

So the campaign anticipated the bug class, wrote a test for it by name, and
shipped a test that passes under the bug. Naming a failure mode in a plan is
not the same as discriminating it, and the gap between the two is invisible
from inside the plan. *A mutation proves only what it perturbs* was already a
standing lesson; this is its inverse — **a control proves only what it can
distinguish**, and the check is to state the two states it must tell apart and
verify the assertion differs between them.

### Finding 4 came from reading the data, not the code

`glyphFor` tested `water !== "none"`. The water legend is
`["ocean", "salt-basin", "river", "dry-land"]` and **never contains
`"none"`** — so the predicate is true for `"dry-land"` too, and every dry-land
cell would have drawn as water. The map would have rendered a plausible,
consistent, entirely wrong world.

It was masked twice over. No test caught it because all 31 cells in the
fixture happen to be `"river"`, so the wrong branch and the right branch agree
on every cell the suite ever evaluates. And it was masked in review because
the rendered output looked *correct* — the sim genuinely reads that
neighbourhood as all-river at radius 4, which the controller verified against
the session's own first `map` output rather than assuming.

The implementer found it by opening `domains/terrain/src/water.rs` and reading
the legend the predicate was testing against. **The sentinel a negative test
compares to must be looked up, not assumed**, and a fixture whose cells are all
one value cannot distinguish a predicate from its negation. The fix was a
*positive* match against the water kinds, plus a regression test the
implementer verified as a real negative control.

### Finding 3 is a reminder that the gate does not see styling

`.casement-map` was already owned by `transcript.ts` for inline map-verb lines,
styled at 0.8em and 0.75 opacity, with `transcript_test.ts` asserting on it.
The new pane silently inherited dimmed styling. No test asserts on styling, so
nothing could have reddened. The pane was renamed `casement-mapview`.

The generalisable form: **a name is a namespace claim, and the client tree has
no uniqueness check on class names.** Grep the tree for any string a plan
introduces as an identifier — this cost one review round and a two-second grep
would have cost nothing.

## The measurement discipline worked, and it worked because it was staged first

The plan's first task built a benchmark before any channel existed, and its
fourth re-ran the same fixed sequence after. That ordering produced a **matched
pair** rather than a before-picture reconstructed from memory, and three
consequences followed that would not have otherwise.

**It priced the feature honestly.** `snapshot() + json` went 0.173 → 1.249 ms
(7.22×) and the walk snapshot 4,235 → 11,582 bytes. Neither number was
guessable; the design document had declined to price the time at all and said
why, which is the right posture and is what made building the instrument a
task rather than an afterthought.

**Three independent instruments agreed to one byte.** The design document
computed ~7 KB from a standalone scene emit; the fixture step derived 7,348
bytes of chart per turn from a golden's growth; the benchmark measured +7,347.
Three estimates from instruments sharing no code is the strongest form of
confirmation available cheaply, and it is only available because the
measurement was staged rather than folded into the implementation.

**It killed a specified feature on evidence.** The design document held a
memoization mitigation in reserve, conditioned explicitly on *"if the benchmark
says one is needed."* The benchmark said no: 1.249 ms × the repository's
3.6–3.8× native-to-wasm ratio is ~4.6 ms, which a human pressing a key does not
perceive. Writing the condition into the spec is what made "do not build it" a
finding rather than a scope cut. **A conditional feature needs its condition
written as an arithmetic test, or it gets built by default.**

**It paid down a debt nobody had scheduled.** The turn clock's re-measurement
had been recorded as owed since The Action Clock fired its trigger, wanting "a
session-level benchmark nobody has built." This campaign needed exactly that
benchmark for its own reasons and built it — 1.071 ms native × 3.6–3.8× ≈ 4.0
ms, corroborating rather than contradicting the 4.75 ms wasm floor. Debts of
this shape get paid when a campaign's own needs happen to align with them,
which argues for reading the owed-measurement notes when scoping, not only when
closing.

### A pooled median masked what it was built to show

Task 1's bench pooled `turns` and `snaps` medians across all ten heterogeneous
verbs, so a slow outlier verb was invisible. That was my plan text, and it
mattered specifically because the memo mitigation only helps verbs that move
neither position nor day — the pooled median is the wrong statistic to see that
with. Task 4's dispatch split the reading by verb class, and the split is the
more informative reading: verb handling varies ~80× by class (11.6 / 7.7 / 0.19
ms) while `snapshot() + json` is flat (1.20 / 1.25 / 1.31 ms).

**Pool only over a population you have argued is homogeneous.** The plan named
three verb classes in its own prose and then pooled across them anyway.

## Two controller rulings, recorded because both look like deviations

**The 3× ceiling on `Session::start` is not a deviation.** The reviewer flagged
it against the brief's "≈2×" because the reviewer could not see the dispatch,
which had explicitly instructed a looser ceiling and required the reason in the
constant's doc comment. `Session::start` re-sculpts terrain from scratch on
every call and Task 1's own baseline swung 1451 → 2280 ms between runs minutes
apart — a 1.6× swing on its own. The reviewer's independent re-run measured
5997 ms, against which a 2× ceiling (6884) would have been a coin flip. The
looser ceiling was empirically vindicated by the very review that questioned it.

The process point: **a reviewer sees the brief, not the dispatch.** Any
instruction that relaxes a brief's stated number must land in the artifact — a
doc comment — or it reads as a deviation forever. It did here, which is why the
ruling took one exchange rather than a fix round.

**The memo mitigation is not built, and that is compliance, not omission** —
see above.

## Operational findings

- **A ceiling's basis must name its build profile.** The implementer caught
  that `scripts/gate-full-heavy.sh` runs `cargo nextest --profile heavy` with
  **no `--release`**, so a heavy-tier ceiling set against release-profile
  numbers was nearly tripped by dev-profile reality. The reviewer confirmed it
  against the script rather than against the claim. Ceilings in `cli/tests/*`
  now state dev profile explicitly in their doc comments. Anyone setting a new
  wall-clock ceiling should check which profile actually runs it before
  choosing the basis.
- **`make gate` cannot see `clients/`.** Known, and stated per-task in the plan
  rather than once at the end, which is why it was never missed. Both
  `make gate` and `make vessel-check` were required explicitly at every task
  that touched the client tree.
- **Confirm which fixture moves before rebaselining any of them.** Adding a
  channel moves *snapshot* bytes and must not move *transcript* bytes — a moved
  transcript would mean the change leaked into prose, which is the bug rather
  than the drift. The byte-identity smoke stayed green throughout, which is the
  evidence and not merely the absence of a failure.
- **The golden was re-pinned as a witness, not as a claim.** The reviewer
  verified byte-level that `session-seed-42.json`'s growth (16,345 → 45,733 B)
  was purely additive: no pre-existing value moved, no key was reordered. That
  is the distinction that makes a re-baseline safe, and checking it is a
  five-minute read of a diff rather than a judgement call.
- **The type-audit report drifts mid-campaign, not at the artifact step.**
  Task 2 added `pub` items and regenerated `docs/audits/type-audit-report.md` in
  the same commit; the reviewer re-derived it byte-identically. By the close,
  `make rebaseline` produced **zero** drift across
  `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/` and
  `docs/audits/` — which is what re-pinning in the drifting commit buys.
- **An uncommitted `Cargo.lock` drift in `clients/vessel/wasm/` surfaced only
  at the client gate.** Committed separately as a chore, which keeps the
  feature commit readable.
- **A `deno.json` test task needed `--allow-read` for the new fixture
  reads**, precedented exactly by `clients/atlas/deno.json`. Finding the
  precedent rather than inventing a permission set is the cheap move.

## Confidence Gradient

**A bet moved, and this section first said it had not.** The original entry
read *"No bet moved — N/A"*, on the reasoning that the campaign drew no
world-state, changed no physics, and moved no metric any bet is scored on.
That reasoning is wrong in a way worth keeping on the record, because it is a
tempting error: it scored the gradient on **whether physics moved** rather
than on **whether the chapter holds a bet in this territory**. It does. The
Snapshot's entry is about the client emit seam, and it explicitly names what
it declined to do — prove the seam by adding a second pane, which would have
proved nothing at the time. This campaign added the second pane. That is the
bet advancing along its own stated axis, and it was visible from the chapter's
own text without any judgment call.

Caught at the merge, during the freshness sweep, only because the sweep
grepped the chapter for the campaign's *domains* rather than trusting this
section's conclusion. The re-score now sits in `book/src/open-questions.md`
after The Snapshot's entry: the bet is confirmed, with two refinements — the
emit cost is now measured rather than assumed (1.249 ms, and band-dependent
byte growth of 2.73× / 1.17×), and the seam's weakest point turned out to be
one no test had stated, namely that an emit mirroring sim state inherits that
state's growth. It is recorded as a *narrower* confidence, not a wider one.

**The generalisable form: a campaign that changes no physics can still move a
bet, because not every bet is scored on physics.** The check is a grep of the
chapter for the campaign's territory, and the answer "N/A" needs the same
evidence any other answer does.

## Follow-ups

Promoted here from the campaign's per-worktree scratch (`.superpowers/sdd/`),
which is git-ignored and dies with the worktree.

### Registered as ideas

- **The remembered map is band-asymmetric.** The walk band already accumulates
  (the knowledge ledger holds a room entry per visited locale) and
  `scene/surrounds/v1` has carried a `"remembered"` cell state with no writer
  since it was designed — so a remembered *overworld* minimap is nearly free
  from data that exists today. The chamber band accumulates nothing: verified
  by `printf 'enter\nknows\nrelease\n' | hornvale possess --seed 42`, which
  reports a room, a settlement name and a settlement population after entering
  a chamber, and **no chamber key of any shape**. A remembered *plan* therefore
  needs a new knowledge shape, not a new pane. Filed as a registry row; the
  alive-map row had assumed a remembered map without noticing half of it was
  already paid for.
- **Lift the indoor chart refusal.** The session answers `map out` indoors with
  a refusal, which is what forces this campaign to one band-switching pane
  rather than two simultaneous spatial panes at two scales. It is the single
  identified unlock for that layout. Filed as a registry row. It carries an
  honesty question, not merely an implementation one: what does the chart of the
  country outside mean while you stand in a windowless room?

### Design question for the owner

- **The pane and the `map` verb draw the same cell differently.** Rust gives
  ocean `~`, salt-basin `=`, river `+`, and dry land by relief; the pane
  collapses all water to `~` and land to `.` / `,`. The plan licenses this — two
  renderings of one scene for different purposes — and no test asserts
  agreement. But the campaign deliberately **shared** the `@` mark on the
  reasoning that a player must not learn two marks for one thing, and that
  reasoning arguably extends to terrain glyphs a player sees in one session.
  Not a defect; a call the owner should make before the divergence sets.

### Unresolved measurements and unbounded inputs

- **The memo's benefit is unmeasured**, because construction and serialization
  were never timed separately. The memo would cache *construction* only (the
  bytes ship every turn regardless), and nothing in this campaign says how much
  of the 1.249 ms is which. If a future campaign wants the memo, **that split
  is the measurement it needs first**, and it is a small change to the existing
  bench.
- **The walk chart's coordinates are unbounded.** `pane_plan.ts` is bounded by
  `cells.length === w * h`; `pane_chart.ts` has no equivalent ceiling, so a
  payload carrying `v: 1e9` would build a ~2e9-character row and hang the
  worker. The sim cannot currently produce one, so this is a hardening item
  against a malformed or hostile payload, not a live bug.
- **The snapshot has no CLI surface.** `vessel/session/v1` is reachable only
  through the wasm ABI and Rust unit tests; no `hornvale` subcommand emits it,
  so there is no committed golden snapshot in the gallery and no drift check on
  the schema's bytes. Adding the spatial channel makes this gap materially
  larger — the schema now carries the fine layer.
- **The Casement lockup class is not closed, only narrowed.** Finding 3's
  repair wrapped `drawMap` in a try/catch precisely because an uncaught throw
  in an `onmessage` branch skips `setIdle` and freezes the input box. But
  `narrationOf` runs in the *same unguarded region*, between the guarded
  `drawMap(snap)` and `setIdle(...)` on both the possess and the turn branch
  — and `parseSnapshot` validates only the schema tag (`snap?.schema ===
  SESSION_SCHEMA`), never the shape beneath it. So a payload carrying the
  right tag and a missing `narration` returns a truthy `snap`, `narrationOf`
  throws on `snap.narration.prose`, and the input freezes identically. The
  guard was placed at the symptom rather than at the region. **Pre-existing,
  not introduced here** — the same three lines sit in `main.ts` at this
  campaign's merge base — but the campaign that names a failure mode is the
  one that should have swept for its siblings, and this is the sweep-on-the-
  invariant lesson landing again: the invariant is "every `onmessage` branch
  reaches `setIdle`", not "`drawMap` cannot throw."

### The band fold — a semantic collision the preflight cannot score

Found at the close, merging main in, and worth recording as a *class*. The
Deep Realm added an `underground` band to `Session` while The Panes added a
`spatial` channel that is a tagged union **over bands**. Two parallel
worktrees, one file, no textual conflict — `git` had nothing to complain
about, because the two campaigns edited different lines. Neither campaign's
spec, plan, or chronicle mentions the other's surface; a grep of both new
chronicles for `snapshot`, `pane`, or `spatial` returns nothing.

The outcome is benign and that is the interesting part. `underground` folds
into `walk`, so the pane draws the surface chart from inside a cave — which
is exactly what the `map` verb does in the same state, because `map`'s band
arms guard on the indoor state alone. The Panes' own comment claimed pane and
verb "can never disagree about which band is current," and after the merge
that claim is still *literally true*. It survived by luck rather than by
design: had The Deep Realm made `map` band-aware for `underground` (as it did
for `look`, `go`, and `back`), the pane and the verb would have silently
diverged with every test green.

So the repair is not to the behaviour but to what is *checked*. A test now
asserts the two halves agree in the third band, mutation-verified in both
directions, and the open question — what a surface chart means read from
below ground — is registered rather than answered. **The generalisable form:
when a campaign ships an enumeration over some other campaign's state space,
the absorption check is not "did it merge" but "is the enumeration still
total."** Nothing mechanical asks that.

### Deferred minors from Task 3's review

All four were accepted as minors at the time and none blocks a merge.

1. `the_snapshot_stays_a_pure_read` is an **idempotence canary, not the purity
   proof its name claims** — `&self` plus no interior mutability already forbids
   mutation at compile time. It also covers only the Walk branch.
2. The band-tag test **asserts presence, not exclusivity**: an implementation
   emitting *both* tags would pass it.
3. The **widened failure surface** — a purview error now fails the whole
   snapshot rather than one channel — is pinned by no test, only by a comment.
4. `snapshot()`'s doc still reads *"costs nothing … its measured per-turn cost
   is unchanged."* That is true only for callers that never ask for the payload,
   and the payload grew. **Reworded at the close — and the rewording carried a
   false number, which is the part worth keeping.** It cited "2.8× the bytes"
   and attributed it to `turn_cost.rs`, which reports no such figure:
   `turn_cost.rs` measures the growth **per band**, and the two bands differ by
   more than a factor of two — walk 4235 → 11582 B (2.73×), chamber 4064 → 4759
   B (1.17×).

   The instructive part is where 2.8× came from, because it was **not
   invented**. Task 3 measured the golden fixture `session-seed-42.json`
   growing 16,345 → 45,733 B when the spatial channel landed — a ratio of
   2.797, and both endpoints are exactly right; the file on disk is 45,733 B
   today. So a correct measurement of one quantity (a multi-turn golden
   fixture's size on disk) was transplanted onto a claim about a different
   quantity (one turn's payload), and re-attributed to the bench that measures
   the second. Every individual number in the chain was real. **A number does
   not carry its denominator around with it, and a citation is not a check** —
   the figure looked measured, cited a real file, and was wrong by 2.4× for the
   chamber band. Sanity arithmetic would have caught it: the two committed
   per-band fixtures sum to 16,341 B, nowhere near 45,733.

   The doc now states both bands with their byte counts, and the citation is
   accurate for the first time.

### Deferred minor from Task 7's review

- `parseCell` accepts non-integer coordinates where `pane_plan.ts` uses
  `Number.isInteger`, so a fractional coordinate is not refused the way
  `pane_plan.ts` would refuse one. **Corrected during the final whole-branch
  review**: the earlier note here said this "silently drops a cell," which
  the review found is not what happens in the case that matters. The
  placement formula is `row = -w; col = 2v + (up ? 0 : 1) + w`, and it does
  not preserve fractional inputs as fractional outputs: `v: 1.5, w: 0, up:
  false` gives `col = 2(1.5) + 1 + 0 = 4`, an integer, so the cell is placed
  at a coordinate arithmetically **derived from** a fractional input — a
  cell shown in the wrong place, not a cell refused. Only the narrower case
  where the arithmetic itself lands on a non-integer `row` or `col` fails to
  match the render loop's integer-stepped keys and is dropped. Whoever picks
  this up should fix the misplacement, not just the drop.

## One deliberate deviation from the approved spec

Recorded here as well as in the plan's self-review, because a spec deviation is
stated and never made silently.

Spec §3.2's fourth bullet says `vessel/plan/v1` ships an empty `marks` list.
The plan **omitted the field entirely**. The design principle it encodes —
instances go in marks, types go in the palette — is preserved in `plan.rs`'s
module doc, where The Sighting will read it. What was dropped is the empty
array, on The Hollow's finding that *a field nothing reads cannot be seen to be
wrong*, and because additive-is-free means adding it beside its first writer
costs exactly nothing later. Flagged to the owner before execution and not
overruled.

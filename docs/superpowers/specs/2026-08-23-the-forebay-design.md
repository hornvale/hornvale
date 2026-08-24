# The Forebay — one store for derived values, three validity classes

**Campaign**: The Forebay (the Penstock program, the campaign after The Leat)
**Date**: 2026-08-23
**Branch**: `campaign/the-forebay`
**Base**: `3681a3a8b` (`origin/main`)
**Decision block**: 0206–0215 (reserved on lefford; main ceiling 0197)
**Status**: spec, awaiting G3
**Governing metaplan**: `docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`

A forebay is the pond immediately upstream of a penstock. It holds water
already drawn and ready, so the turbine below never waits on the river above.
That is this campaign: the place derived values live between the moment they
are computed and the moment they are wanted again.

---

## 1. Why this campaign, and why not the other one

The metaplan's §6 stage table is partly superseded by its own later sections,
and a reader arriving at it fresh will pick the wrong campaign. State the
position plainly:

- **Stage 2 is not enterable.** Its gate is *"stage 1 shows locality queries
  are a real share of tick cost."* §6.5 measured ledger queries at **0.04%**.
  Stages 3 and 4 sit behind stage 2 and inherit the block.
- **Stage 7 (log bounding) has the strongest case on merit.** §6.1's falsifier
  fired and §6.3 calls it *"the one item in this program with no
  alternative."* On the temporal-direction axis it is the only candidate whose
  cost **accumulates**; every other candidate is a capped constant factor. A
  33% allocator tax has a 1.5× ceiling and can be shipped with forever. An
  append-only log has no ceiling.
- **Stage 7 is nevertheless deferred, on availability rather than merit.** It
  lives entirely in `windows/vessel/src/liveness.rs`, which carries two live
  hold-offs: The Escapement's Task 9 sweeps 53 `.day()` sites in that file,
  and `campaign/the-hand` holds off the directory. A third campaign queued on
  the repo's most contended file would be reckless. **It is the recommended
  next campaign the moment vessel frees.**

So this campaign builds §6.6 — Nathan's standing direction of 2026-08-23,
*"keep the system general so we're sure to be able to use it for whatever
comes up"* — because it is the only motivated item whose code lives in
`kernel/`, clear of the contention.

### 1.1 What this campaign does NOT build, and why that is deliberate

**No eviction, no budget, no hysteresis.** §5.3 designs them and §6.3
withdraws their justification in the same breath: *"Do not build them on the
strength of §6.2's numbers; they are superseded."* Stage 4's own gate —
"stage 3 shows the rebuild pass actually dominates" — is unmet and
unmeasured. This campaign builds the store, the validity discipline and the
instruments; it leaves a **seam** where the budget goes and does not fill it.

**No condensation boundary, no working set.** That is §2's *visible* half and
stage 5's job. Everything here is §2a — strictly invisible, and its
invisibility is the property the test battery attacks.

## 2. The correction the code forced: three validity classes, not two

§6.6 tabulates two classes of derived value, world-derived and
ledger-derived. Reading the existing memo, there are **three**, and the third
is the one already shipping.

`RoomMeshMemo` (`kernel/src/room.rs`) holds two maps whose invalidation rules
its own doc comments already distinguish:

- `neighbors: BTreeMap<RoomAddr, [RoomAddr; 3]>` — *"Pure geometry, no
  external dependency, so this half never goes stale regardless of which world
  it is reused across."*
- `corner_weights: BTreeMap<RoomAddr, Option<[(CellId, u64); 3]>>` — valid
  only *"for the ONE `(geo, index)` pair this memo is used with."*

Those are different rules, in one struct, distinguished only by prose. The
generalization is a **validity key per entry**:

```
  class        valid for                        key            invalidated by
  ---------    -----------------------------    ------------   -----------------
  Universal    every world, forever             ()             nothing
  World        one world (one geo+index pair)   WorldStamp     nothing, in-world
  Ledger       a ledger prefix                  (pos, deps)    a later fact
                                                               touching deps
```

Storage is identical across all three; **only the validity check differs** —
which is §6.6's thesis, refined by one row. `Universal` is not a degenerate
`World`: a `Universal` entry is *shareable across worlds*, which is a
capability the census (thousands of worlds, one process) can use and a
`World`-stamped entry cannot.

**This is a correctness win, not only a refactor.** `corner_weights`'s
world-scoping is enforced today by a `debug_assert_eq!` on the *first* geo
level ever inserted — its own doc admits it is *"Not a full fix — two
different geospheres at the SAME level would not be caught"*, and it compiles
out of release builds entirely. Promoting that to a real `WorldStamp` checked
on every read makes the guard total and release-active.

### 2.1 `WorldStamp` — what identifies a world

The stamp must be cheap to compare, deterministic, and must distinguish two
geospheres at the same level (which the current guard cannot). Task 3 fixes
the composition; the constraint is that it derives from data the store's
caller already holds — no new plumbing, no wall-clock, no counter — and that
two stamps comparing equal implies the two worlds agree on every value the
`World` class caches. If no such cheap total discriminant exists, the honest
fallback is to keep the partial guard and *say* it is partial in the type's
name, rather than name it a stamp and imply totality.

## 3. Shape

One generic store per value shape, aggregated — **not** a single
heterogeneous store.

```rust
pub struct Derived<K: Ord, V> { /* entries, validity, counters */ }
```

Rationale, and it is a determinism argument before an ergonomics one: a
heterogeneous store needs `Box<dyn Any>` and `TypeId`, and `TypeId`'s ordering
is not stable across builds — so a single store would put an unstable
iteration order under a byte-identity guarantee. A generic-per-shape store
keeps every map a `BTreeMap<K, _>` with `K: Ord`, which is the workspace's
standing rule (no `HashMap`, `clippy.toml` `disallowed-types`). It also
satisfies §3.6's **closed view set before open**: the author declares which
shapes exist, so the property battery can enumerate them.

`RoomMeshMemo` becomes a thin facade over two `Derived` instances, **with its
public API unchanged**. That is load-bearing, not stylistic: its callers are
in `windows/vessel/`, `windows/locale/` and `windows/lab/`, and vessel is
held off. An API-preserving facade means this campaign edits **zero vessel
files**. Strangler-fig, as §6 requires — shippable and reversible.

## 4. The instruments, and the question they settle

`HomeNavCache::searches` is the precedent: a `pub(crate)` counter with a
read-only accessor, *"the scaling property's own deterministic witness …
never a wall-clock proxy."* The store carries the same shape — hits, misses,
invalidations — as deterministic, gate-able counters.

**Task 1 exists because the campaign's own premise is unverified.** §6.5
measured `scan_at` at 13.4% of the bench, 93% of it arriving via
`corner_weights`. But the memo *already* caches `corner_weights`, and
`agent_scaling.rs` already hoists it across all 20 ticks. So the 13.4% may be
**miss** cost — genuinely new `(RoomAddr, geo)` pairs — and a cache lifecycle
does not reduce misses.

Nothing in the tree measures the memo's hit rate. Task 1 does, and it decides
this campaign's headline rather than its scope:

| Task 1 finding | headline | consequence |
|---|---|---|
| hit rate low; reuse is being thrown away | the store retires a real share of 13.4% | the migration is the win |
| hit rate already high; the residual is misses | the store is built for the mature workload, not today's 13.4% | the 13.4% needs a faster `scan_at` or a reachable-set prefill — a follow-up, named as such |

**Either way the store ships**, because §6.6's direction is generality for
what comes next, and §6.5's N×M argument says the ledger-derived column
becomes hot when belief or the social graph lands. What Task 1 forbids is
*claiming* a win this campaign did not measure.

A second, cheaper instrument finding to confirm: `agent_scaling.rs` clones the
whole memo every tick (`let mesh_snapshot = mesh_memo.clone();`) to satisfy a
borrow. With `malloc`+`memcpy` at 33.3% of that bench, a full `BTreeMap` clone
per tick is a candidate contributor, and it is a property of the harness
rather than of the sim. Measure before concluding either way.

## 5. Correctness: the standing gate, rungs 2 and 3

§7's ladder, at the rungs this campaign can reach.

**CACHE ≡ RECOMPUTE (property).** The generalization of the existing
`corner_weights_memo_bit_equals_recomputation`, and of the metaplan's VIEW ≡
SCAN keystone: for random key sequences, a value read through the store is
bit-identical to the value recomputed directly. Per shape, since the shape set
is closed.

**Chaos-eviction (adversarial).** §7 rung 3, and buildable *without* a budget:
a harness that drops entries at every legal opportunity and asserts output is
byte-identical. This is what strict invisibility (§2a) buys, and it is the
reason invisibility is a capability rather than only a restriction. It is also
the test that would have caught the geo-aliasing hole the current
`debug_assert` leaves open.

**Cached absence stays distinct from "not looked up yet."** The existing
`corner_weights_lookup` returns `Option<Option<…>>` precisely to draw this
line (§5.4's tombstone distinction, and there is already a test named
`corner_weights_lookup_distinguishes_miss_from_a_cached_none`). The store must
preserve it, and the battery must pin it — collapsing the two is the obvious
refactoring error here.

**Determinism.** Nothing this campaign builds is serialized (§8), so there is
no save-format surface and no epoch. Byte-identity of committed artifacts is
the master oracle and must be unchanged: `make rebaseline` moves nothing but
`docs/audits/` (the type-audit report drifts on any `pub`-boundary change, and
this campaign adds `pub` items — so that directory moving is *expected*, and
`book/src/gallery/` or a census CSV moving would be a **STOP**).

## 6. Task 0: two guards that are dead on this Mac, today

Both found by this campaign's own machinery rather than by looking — one
during `make worktree-take`, one during its absorption of `origin/main` — and
folded in at Nathan's direction rather than left as followups.

`scripts/test-worktree-freshness.sh` — *"a worktree must never serve a binary
compiled under a different path"* — **does not parse under macOS's bash 3.2**,
on `origin/main`. It fails during `make worktree-take`, so the guard is
silently absent on the Mac. Verified, not inferred:

```
$ bash --version | head -1
GNU bash, version 3.2.57(1)-release (arm64-apple-darwin25)
$ bash -n scripts/test-worktree-freshness.sh
scripts/test-worktree-freshness.sh: line 181: syntax error near unexpected token `('
```

Root cause, reduced to a five-line repro: **bash 3.2 mis-parses an apostrophe
inside a comment inside a `$(...)` command substitution.**

```bash
x="$(
    echo hi
    # the checkout's own path
)"
```

```
line 3: unexpected EOF while looking for matching `''
```

Three such comments sit inside one command substitution in that script
(`:102` "checkout's", `:112` "heavy-wt's", `:138` "git's"). Bash reports the
error at line 181 — an `awk` program far below — which is why this survived:
the reported location is nowhere near the cause.

### 6.1 It is not one defect, and the second one is worse

A scan of every `bash`-shebanged file in `scripts/` found a **second live
defect**, in a different half of the class — and it fired during this
campaign's own absorption of `origin/main`:

```
$ git merge origin/main
...
scripts/hooks/post-merge: line 75: mapfile: command not found
```

`mapfile` is bash 4.0+; macOS ships bash 3.2 as `/bin/bash`. The hook sets
`set -euo pipefail`, so the missing builtin **aborts it at line 75** — and
git ignores a post-merge hook's exit code, so the failure is silent.

**What that hook is for makes this matter more than a broken advisory.** It is
the thing that tells you, after a merge, that a generated artifact or the Rust
code producing one has moved and `make rebaseline` is owed. CLAUDE.md is
explicit that **there is no CI** and that *"a red main is invisible until
someone runs a gate and `make rebaseline` … nothing runs it for you."* This
hook is the only automated nudge toward that check, and on the Mac it has
never run. Observed, not inferred: the absorption above touched both generated
paths and `.rs` files, so the advisory's own conditions were met and it
printed nothing.

There is an irony worth recording because it is also the lesson. The comment
directly above line 75 narrates this very hook being *"embarrassed"* by
missing a case on its first substantive merge, and was extended to close that
blind spot. It now misses **every** case on the Mac.

### 6.2 The project already knows this class; nothing enforces it

The scan's most useful result is that the knowledge exists and is inert. Two
scripts carry comments explaining that they deliberately avoid these very
constructs:

- `scripts/census-canonical-host.sh:90` — *"`tr`, not `${var,,}`: bash 3.2
  ships on macOS and lacks case expansion"*
- `scripts/subfloor-run-chunked.sh:70` — *"`mapfile` is bash 4+; macOS ships
  `/bin/bash` 3.2 … so `mapfile` is [out]"*

So this is the **fourth and fifth** instances of a class the project has
diagnosed correctly at least twice in prose, while a third file used the
banned builtin anyway. The Penstock's own final commit was *"a third bash 3.2
instance, found during the close itself."* Every prior fix was a spot fix, and
the count keeps rising.

### 6.3 So Task 0 is a check with two halves, because one would miss the other

The two defects fail in different ways, and neither detector finds both:

| defect | detector | why the other misses it |
|---|---|---|
| apostrophe in a comment inside `$(...)` | `/bin/bash -n <file>` | it parses fine on bash 4+; a construct grep has nothing to match |
| `mapfile`, `${var,,}`, `declare -A` | grep for bash-4+ constructs | it **parses** cleanly under 3.2 — the failure is at runtime |

Task 0 delivers both, over every `bash`-shebanged file in `scripts/`, plus the
two fixes. Current inventory from the scan: **one parse failure**
(`test-worktree-freshness.sh`) and **one construct violation**
(`hooks/post-merge:75`).

**`shellcheck` reports the parse-failing file completely clean** — verified,
and it is the reason a lint cannot replace either half:

```
$ shellcheck scripts/test-worktree-freshness.sh; echo "exit=$?"
exit=0
```

A clean shellcheck on a file the system bash cannot parse is a false green.



## 7. Metaplan housekeeping

Three corrections to the governing document, each carrying its evidence:

1. **§6.4 asks a question §6.5 answered.** §6.4 declares §11's falsifier-1
   `UNSETTLED` and prescribes *"time the A\* path"* as the settling
   experiment. §6.5, added directly below it **in the same commit**, profiles
   A\* at **5.3%** of the bench. Nothing connects them, so a reader following
   §6.4 re-runs a measurement already taken. Reconcile them, and record what
   5.3% means for falsifier-1: planning does **not** dominate; the tick is
   bound by derived geometry and the allocator.
2. **§6.5's 13.4% survived a harness-artifact challenge, and only the commit
   message says so.** `06a07efcc`: *"Wiring in the cache production always
   passes moved scan_at only 14.9% to 13.4%, so the harness-artifact
   hypothesis was wrong and the cost is real."* That belongs in the spec — the
   `LocaleTerrain::new` / `cache: None` shape is visible in the harness and
   the next reader will raise the same doubt. (This session did.)
3. **§6.6 gains the third validity class** (§2 above), and the stage table
   gains the two gate verdicts from §1: stage 2 not enterable, stage 7
   deferred on availability with its hold-offs named.

## 8. Decisions this campaign will need

From the reserved block 0206–0215.

1. **A derived value carries its own validity key, and there are three
   classes** — universal, world-scoped, ledger-scoped. One store, three
   checks. (§2; refines §6.6, which named two.)
2. **The derived store is generic per value shape, never heterogeneous** —
   because `TypeId` ordering is not build-stable and byte-identity forbids an
   unstable iteration order under a cache. (§3)
3. Possibly: **`WorldStamp` totality** — whether a cheap total world
   discriminant exists, or the guard stays admittedly partial. Task 3
   decides; it is a decision only if the answer is "partial, and named so."
   (§2.1)

## 9. In / out

**In:** the `Derived` store; the three validity classes; the `WorldStamp`;
migrating `RoomMeshMemo` onto it behind an unchanged public API; the hit/miss/
invalidation counters; CACHE ≡ RECOMPUTE and chaos-eviction; Task 0's two
bash 3.2 fixes and the two-half check that would have caught either; the
three §7 metaplan corrections.

**Out:** eviction, budget, hysteresis (§1.1). The condensation boundary and
the working set (stage 5). Log bounding (stage 7 — next campaign). Any edit to
`windows/vessel/` (held off). Any faster `scan_at` or prefill — a follow-up
Task 1 may name but this campaign does not build. Serialization of anything
(§8 of the metaplan).

## 10. What would falsify this campaign

- **Task 1 finds the memo's hit rate already near-total and the residual to be
  pure miss cost.** Then the store buys nothing measurable today, and this
  campaign's honest headline is generality for a workload that has not
  arrived. It still ships (§4), but §11's falsifier-2 — "measured reuse before
  eviction is low" — would be live for the first time with real numbers, and
  stage 3 must open against them.
- **No cheap total `WorldStamp` exists.** Then the geo-aliasing guard cannot
  be promoted to release-active totality, decision 3 records the partiality,
  and the correctness half of §2's win shrinks to the `Universal`/`World`
  split alone.
- **Task 0's construct grep is unbounded in practice.** If the bash-4+ grep
  turns up many violations rather than the one the scan found, the check would
  land red and be disabled — the failure mode the metaplan's own seam-guard
  discussion names ("a gate that failed on the mere EXISTENCE … would go red
  on day one and stay red"). The scan says the current count is one, so the
  check can land enforcing; if a later pass disagrees, it ratchets on novelty
  instead.

- **The facade cannot preserve `RoomMeshMemo`'s API.** Then the campaign
  cannot avoid editing vessel, and it must stop and renegotiate the hold-off
  rather than proceed.

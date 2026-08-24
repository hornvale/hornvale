# The Forebay Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give Hornvale one general store for derived values in `kernel/`, where an entry's key IS its validity, and migrate the existing `RoomMeshMemo` onto it behind an unchanged public API.

**Architecture:** A generic `Derived<K, V>` per value shape (never heterogeneous — `TypeId` ordering is not build-stable and byte-identity forbids an unstable iteration order under a cache). Two validity classes: `Pure`, a pure function of its key, never invalidated; and `Ledger`, a fold over a ledger prefix, invalidated when a later fact touches its dependency set. The store's contribution is making **key-completeness** a typed obligation rather than a doc comment, with chaos-eviction as its mechanised test.

**Tech Stack:** Rust edition 2024, `std` only. Dependencies are `serde`, `serde_json`, `libm` workspace-wide and this campaign adds none. `BTreeMap`/`BTreeSet`/`Vec` only — `HashMap`/`HashSet` are banned by `clippy.toml` `disallowed-types`. `cargo nextest` for tests, `cargo test --doc` for doctests.

**Spec:** `docs/superpowers/specs/2026-08-23-the-forebay-design.md`

## Global Constraints

- **Decision block: 0206-0215.** Reserved on lefford via `make decision-block NAME=the-forebay` (main ceiling was 0197). Mint only inside this range — a number inside another campaign's block is invisible to every mechanical check until both are committed (board post, `main`/lefford, the-stride incident).
- **ZERO edits under `windows/vessel/` or `windows/lab/`.** `campaign/the-hand` holds off both (board `notice`, `polarity=hold-off`, live and unpushed on host MacBookPro). It is restructuring `liveness.rs`/`session.rs`/`agent.rs` — `Npc` and `Agent` merge into one `Body` — and names "the remaining unindexed subject scans from The Penstock" as work to coordinate on. If a task appears to need a vessel edit, **STOP and report**; do not proceed.
- **`kernel/` is free.** `campaign/the-ell`'s hold-off on `kernel/`,`domains/`,`windows/`,`cli/` is **stale**: it promised to retype `Fact.day` to an enforcing newtype, and `kernel/src/ledger.rs:81` already reads `pub day: Option<crate::field::WorldTime>` with decision 0126 in force superseding 0014. The three `scripts/` hold-offs are historical incident reports, not live claims.
- **Nothing this campaign builds is serialized.** No save-format contract, no epoch suffix, no stream label. If a task seems to need serialization, that is a new decision, not an implementation detail — STOP.
- **Every crate sets `#![warn(missing_docs)]`.** Every `pub` item, field and variant gets a one-line doc comment, or the build warns.
- **Every primitive at a `pub` boundary needs a `type-audit:` verdict tag** (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`), else the type-audit check fails default-deny. Follow the neighbours in `kernel/src/room.rs`, e.g. `/// type-audit: bare-ok(count: return)`.
- **`cargo fmt` is the final step before every commit.** A skipped fmt gate is the project's most common review finding.
- **A kernel-layer edit makes `make gate-commit` cost ~470 s.** Budget for it; it is the blast-radius cost, not the suite size. Scope to `-p hornvale-kernel` while iterating and run the full gate only at a task boundary.
- **RUN ONCE, INSPECT MANY.** Never invoke the suite twice to ask it two questions — capture to a file and grep it. A wrapper enforces this and will refuse a command containing two test runs.
- **Kernel integration tests live in `kernel/tests/suite/` and MUST be declared in `kernel/tests/suite.rs`** with an explicit `#[path]` — cargo does not compile subdirectories, so a new file not declared there silently never runs.

---

## Task 0: the two dead bash guards, and a check with two halves

Two guards are broken on macOS's bash 3.2 **on `origin/main` today**, and they fail in different ways so neither detector finds both. The check is written FIRST and must go red on the two known defects before either is fixed — that is this task's red/green cycle.

**Files:**
- Create: `scripts/check-bash32.sh`
- Modify: `scripts/test-worktree-freshness.sh` (two comments, at `:102` and `:112` — verified; `:138` is outside every substitution and must NOT be touched)
- Modify: `scripts/hooks/post-merge:75`
- Modify: `scripts/hooks/pre-commit` (wire the check in)

**Interfaces:**
- Consumes: nothing from earlier tasks (this is first).
- Produces: `scripts/check-bash32.sh`, exit 0 clean / non-zero with a per-file report. Nothing later depends on it.

- [ ] **Step 1: Reproduce both defects and record the output**

Run these and paste the real output into the task notes. Do not skip — the fixes are verified against these exact strings.

```bash
bash --version | head -1
bash -n scripts/test-worktree-freshness.sh; echo "parse exit=$?"
grep -n 'mapfile' scripts/hooks/post-merge
shellcheck scripts/test-worktree-freshness.sh; echo "shellcheck exit=$?"
```

Expected: bash 3.2.57; the parse fails reporting **line 181** (an `awk` program, nowhere near the cause); `mapfile` present at line 75; **shellcheck exits 0 completely clean** — which is why a lint cannot replace this check.

- [ ] **Step 2: Write the check (the failing test)**

Two halves, because `bash -n` cannot see `mapfile` (it parses fine and fails at runtime) and a construct grep cannot see the parse class.

```bash
#!/usr/bin/env bash
# scripts/check-bash32.sh — every shipped shell script must run under the
# bash macOS actually provides.
#
# WHY THIS EXISTS AND WHY IT IS TWO CHECKS. macOS ships bash 3.2.57 as
# /bin/bash (its last GPLv2 release) and that is what a `#!/usr/bin/env bash`
# shebang resolves to on a Mac without a Homebrew bash earlier in PATH. Two
# independent failure modes follow, and a detector for one is blind to the
# other:
#
#   PARSE   bash 3.2 mis-parses an apostrophe inside a comment inside a
#           $(...) command substitution. It reports the error at a line far
#           below the real cause, which is how it survives review.
#   RUNTIME mapfile/readarray (bash 4.0+), declare -A (4.0+) and the
#           case-expansion parameter forms (4.0+) PARSE cleanly under 3.2
#           and fail only when the line is reached.
#
# shellcheck does not substitute for either half: it reported
# scripts/test-worktree-freshness.sh completely clean (exit 0) while
# /bin/bash could not parse the file at all. A clean lint on an unparseable
# file is a false green.
#
# DIRECTION THIS CHECK ENFORCES: every bash-shebanged file under scripts/
# parses under the system bash AND contains no bash-4-only construct. It does
# NOT prove the script is correct, and it does not look outside scripts/.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

SYS_BASH="${SYS_BASH:-/bin/bash}"
status=0

# Only bash-shebanged files; a POSIX sh script is not this check's business.
files=$(git ls-files scripts | while read -r f; do
    [ -f "$f" ] || continue
    case "$(head -1 "$f" 2>/dev/null)" in
        *bash*) printf '%s\n' "$f" ;;
    esac
done)

for f in $files; do
    if ! "$SYS_BASH" -n "$f" 2>/dev/null; then
        printf 'check-bash32: PARSE FAILS under %s: %s\n' "$SYS_BASH" "$f" >&2
        "$SYS_BASH" -n "$f" 2>&1 | sed 's/^/    /' >&2
        status=1
    fi
done

# Construct grep, skipping comment lines so the two files that ALREADY
# document avoiding these constructs stay green.
for f in $files; do
    hits=$(grep -nE '^[^#]*(\bmapfile\b|\breadarray\b|declare[[:space:]]+-[a-zA-Z]*A)' "$f" || true)
    if [ -n "$hits" ]; then
        printf 'check-bash32: BASH 4+ CONSTRUCT in %s:\n' "$f" >&2
        printf '%s\n' "$hits" | sed 's/^/    /' >&2
        status=1
    fi
done

[ "$status" -eq 0 ] && printf 'check-bash32: ok\n'
exit "$status"
```

Add the case-expansion parameter forms to that grep as well. They are omitted from the literal above only because writing them inside this plan's own shell examples is itself a quoting hazard — read `scripts/census-canonical-host.sh:90`, which names the exact construct it avoids, and match that.

- [ ] **Step 3: Run the check and verify it goes RED on exactly the two known defects**

```bash
bash scripts/check-bash32.sh; echo "exit=$?"
```

Expected: non-zero. `PARSE FAILS` for `scripts/test-worktree-freshness.sh`; `BASH 4+ CONSTRUCT` for `scripts/hooks/post-merge` line 75.

**Decision rule on the count, not a prediction:**
- Exactly those two → proceed to Step 4.
- **Zero findings** → the check is broken (probably the shebang filter or `SYS_BASH`); a green here is the no-op-mutation failure mode. Debug the check, do not proceed.
- **More than two** → do NOT fix them all silently. Report the full list. A check that lands red is a check that gets disabled (the metaplan's own seam-guard reasoning: a gate that fails on the mere existence of a problem "would go red on day one and stay red"). Either fix them in this task and say so, or convert the check to a novelty ratchet against a committed roster — and say which you did and why.

- [ ] **Step 4: Fix the parse failure**

**Exactly two lines cause it — 102 and 112 — and NOT 138.** The controller
verified this empirically rather than reasoning about it, because an earlier
draft of this plan asserted three lines and was wrong:

```
$ python3 -c "…strip apostrophes on 102 and 112 only…"
$ /bin/bash -n t2.sh && echo CLEAN
CLEAN
```

Both offending lines sit inside the one `$(...)` that opens at `:96`
(`others="$(`). Line 138's "git's still-stale registry" is outside every
substitution, so bash never parses it as code and it needs no change. The
possessive apostrophe is the only thing bash chokes on.

```
scripts/test-worktree-freshness.sh:102   "the main checkout's own path ..."   -> reword without the apostrophe
scripts/test-worktree-freshness.sh:112   "hornvale-heavy-wt's own"            -> reword without the apostrophe
```

Read each line and reword to preserve its meaning — do not blind-substitute.
Leave `:138` alone; changing it would be noise in the diff, and a reviewer
would reasonably ask why.

Add a line to the file's header noting that an apostrophe inside a `$(...)`
comment breaks bash 3.2, so the next editor does not reintroduce one.

- [ ] **Step 5: Fix the runtime failure**

`scripts/hooks/post-merge:75`. The file sets `set -euo pipefail`, and `generated` is used as an array at line 79, so it must stay an array.

```bash
generated=()
while IFS= read -r line; do
    generated+=("$line")
done < <(grep -v '^#' "$paths_file" | grep -v '^[[:space:]]*$')
```

- [ ] **Step 6: Verify both fixes, and verify the hook now actually speaks**

```bash
bash scripts/check-bash32.sh; echo "check exit=$?"
```

Expected: `check-bash32: ok`, exit 0.

Then prove the hook runs rather than merely parses — the whole point is that it was silently aborting:

```bash
bash scripts/hooks/post-merge; echo "hook exit=$?"
```

Expected: it prints its advisory, or exits 0 silently having found nothing — **not** `mapfile: command not found`. If `ORIG_HEAD` is absent the hook may exit early; in that case re-verify after the next real merge, and record which you did.

- [ ] **Step 7: Wire the check into pre-commit**

Read `scripts/hooks/pre-commit` first and follow its existing shape (it already routes by staged path — it printed `no Rust-relevant paths staged` during this campaign's spec commits). Add: when any `scripts/**` path is staged, run `scripts/check-bash32.sh` and fail the commit non-zero if it fails. Do not make it unconditional — an unconditional shell check on a Rust-only commit is cost with no signal.

- [ ] **Step 8: Gate and commit**

```bash
cargo fmt && shellcheck scripts/check-bash32.sh scripts/hooks/post-merge scripts/test-worktree-freshness.sh && make gate-commit
```

Should be at the cheap end (no Rust changed). Commit with a message stating: the two defects, that they need different detectors, the shellcheck-exit-0 false green, and that these are the fourth and fifth instances of a class the project already documents in two comments while a third file violated it.

---

## Task 1: the memo's hit rate — the number that decides this campaign's headline

The premise of the whole program's read side is unverified. `RoomMeshMemo` already caches `corner_weights` and the bench already hoists it across all 20 ticks (`windows/vessel/examples/agent_scaling.rs:375`, `const TICKS: usize = 20` at `:267`), so the metaplan's 13.4% `scan_at` may be pure **miss** cost — which no cache lifecycle reduces. Nothing in the tree measures the hit rate.

**Files:**
- Modify: `kernel/src/room.rs` (`RoomMeshMemo` struct at `:645`, `impl` at `:671`, `corner_weights_memo` at `:716`, `neighbors_memo` at `:746`)
- Test: `kernel/src/room.rs` `#[cfg(test)] mod tests` — in-module, following the file's convention; `corner_weights_memo_actually_memoizes` at `:1467` is the neighbour to imitate

**Interfaces:**
- Consumes: nothing from Task 0.
- Produces, and Task 3 must preserve all four verbatim:
  - `RoomMeshMemo::corner_weights_hits(&self) -> u64`
  - `RoomMeshMemo::corner_weights_misses(&self) -> u64`
  - `RoomMeshMemo::neighbors_hits(&self) -> u64`
  - `RoomMeshMemo::neighbors_misses(&self) -> u64`

- [ ] **Step 1: Write the failing tests**

Follow `HomeNavCache::searches`' precedent — a private counter with a read-only accessor, "the scaling property's own deterministic witness, never a wall-clock proxy." Counters are `u64` and never reset.

**Read `RoomAddr::new`'s real signature first** (`kernel/src/room.rs:298`/`:361`); if it differs from the shape below, build the address the way the neighbouring tests do. Neither property under test depends on how the address is constructed.

```rust
#[test]
fn memo_counts_hits_and_misses_separately_per_half() {
    let geo = Geosphere::new(3);
    let index = NearestCellIndex::new(&geo);
    let addr = RoomAddr::new(0, &[0, 0, 0]).expect("a level-3 address is valid");
    let mut memo = RoomMeshMemo::new();

    // Cold: one miss on each half, no hits.
    let _ = addr.corner_weights_memo(&geo, &index, &mut memo);
    let _ = addr.neighbors_memo(&mut memo);
    assert_eq!(memo.corner_weights_misses(), 1);
    assert_eq!(memo.corner_weights_hits(), 0);
    assert_eq!(memo.neighbors_misses(), 1);
    assert_eq!(memo.neighbors_hits(), 0);

    // Warm: the same address hits both halves and adds no miss.
    let _ = addr.corner_weights_memo(&geo, &index, &mut memo);
    let _ = addr.neighbors_memo(&mut memo);
    assert_eq!(memo.corner_weights_hits(), 1);
    assert_eq!(memo.corner_weights_misses(), 1);
    assert_eq!(memo.neighbors_hits(), 1);
    assert_eq!(memo.neighbors_misses(), 1);
}

#[test]
fn a_cached_none_counts_as_a_hit_not_a_miss() {
    // An above-the-grid room caches Some(None) -- a cached ABSENCE. Reading it
    // again must count a HIT: conflating a cached None with "not looked up
    // yet" is the exact distinction corner_weights_lookup's Option<Option<_>>
    // exists to draw, and a counter that got it wrong would report a
    // permanently cold cache for every above-the-grid room.
    let geo = Geosphere::new(5);
    let index = NearestCellIndex::new(&geo);
    let shallow = RoomAddr::new(0, &[0]).expect("a level-1 address is valid");
    assert!(shallow.depth() < geo.level(), "this address must be above the grid");

    let mut memo = RoomMeshMemo::new();
    assert!(shallow.corner_weights_memo(&geo, &index, &mut memo).is_none());
    assert_eq!(memo.corner_weights_misses(), 1);
    assert!(shallow.corner_weights_memo(&geo, &index, &mut memo).is_none());
    assert_eq!(memo.corner_weights_hits(), 1, "a cached absence must read as a hit");
    assert_eq!(memo.corner_weights_misses(), 1, "and must not re-miss");
}
```

- [ ] **Step 2: Run and verify it fails**

```bash
cargo test -p hornvale-kernel memo_counts > /tmp/hv-t1-red.log 2>&1; echo "exit=$?"; grep -E "^error|^test result" /tmp/hv-t1-red.log | head
```

Expected: FAIL to compile — no method `corner_weights_hits` on `RoomMeshMemo`. **A compile-error red proves nothing about the assertions**, so Step 4 must confirm the tests discriminate.

- [ ] **Step 3: Add the counters**

Four `u64` fields on `RoomMeshMemo` (private, so `Default` still derives), incremented at the existing hit/miss branches in `corner_weights_memo` (`:716`) and `neighbors_memo` (`:746`), plus four read-only accessors. Each accessor needs a doc comment and a `/// type-audit: bare-ok(count: return)` tag, matching `corner_weights_geo_level` at `:703`. `RoomMeshMemo` derives `Debug, Default, Clone` at `:644` — all three must keep deriving.

- [ ] **Step 4: Verify green, then verify the tests discriminate**

```bash
cargo test -p hornvale-kernel a_cached_none memo_counts > /tmp/hv-t1-green.log 2>&1; echo "exit=$?"; grep -E "^test result" /tmp/hv-t1-green.log
```

Then prove `a_cached_none_counts_as_a_hit_not_a_miss` is not vacuous. Find a mutation that makes it fail — **do not take one from this plan**, because the plan's author cannot see which branch the code actually takes. Hunt for one after reading the implementation, assert the target text is present before substituting it, confirm RED, then revert. A no-op mutation produces evidence and is worse than none.

- [ ] **Step 5: Commit the instrument**

```bash
cargo fmt && make gate-commit
```

Then `git add kernel/src/room.rs` and commit.

- [ ] **Step 6: Take the measurement — location depends on The Hand's answer**

The counters are pure kernel and touch nobody. The **readout** is the coordination point: the natural site is `windows/vessel/examples/agent_scaling.rs`'s existing report, inside `campaign/the-hand`'s hold-off. A question was sent to the live `hornvale-e2` session before this plan was written.

**Branch table — read the answer, then take exactly one branch:**

| The Hand's answer | do this |
|---|---|
| "I'll carry it" | Hand over the four accessor names. Take **no** vessel edit. Record that the number arrives after The Hand lands, and leave the spec's headline question OPEN rather than answered. |
| "land it first, I'll absorb" | Add the four counters to the bench's existing report block as its own commit, ~4 lines, no other change. Run it and record the hit rate. |
| "wait for me" | Same as "I'll carry it". |
| no reply by the time Tasks 2-3 are done | Fall back to a **kernel-side** probe over a synthetic `RoomAddr` walk in `kernel/examples/`, and state plainly that the distribution is synthetic and therefore a weaker instrument than the bench. Do NOT edit vessel on silence. |

Whichever branch runs, record the number and its provenance in the spec's instrument table (naming which of the two headlines it selects), and post a board `reply` so the coordination becomes precedent instead of evaporating.

---

## Task 2: `Derived<K, V>` — the store, and key-completeness as an obligation

**Files:**
- Create: `kernel/src/derived.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod derived;` in alphabetical position — it sorts between `component` and `domain`)
- Create: `kernel/tests/suite/derived.rs`
- Modify: `kernel/tests/suite.rs` (declare the module with an explicit `#[path]`)

**Interfaces:**
- Consumes: nothing from Tasks 0-1.
- Produces, for Task 3:
  - `Derived<K: Ord + Clone, V: Clone>` with `new()`, `get(&K) -> Option<&V>`, `insert(K, V)`, `len()`, `hits()`, `misses()`, `evict_all()`, `evict(&K)`
  - `Validity::Pure` and `Validity::Ledger { position, deps }`
  - `DepKey` — the (subject, predicate, place) triple a `Ledger`-class entry watches
  Settle the exact signatures in Step 3; Task 3 depends on them verbatim, so record them in the task notes.

- [ ] **Step 1: Write the failing property tests**

CACHE ≡ RECOMPUTE, generalising `corner_weights_memo_bit_equals_recomputation` (`kernel/src/room.rs:1436`), plus the chaos-eviction rung.

Create `kernel/tests/suite/derived.rs`:

```rust
//! `Derived` store properties: the cache is invisible, and the key is complete.

use hornvale_kernel::derived::Derived;

/// A stand-in pure derivation with a COMPLETE key: the value is a function of
/// the key and nothing else.
fn square(k: u64) -> u64 {
    k * k
}

fn read_through(store: &mut Derived<u64, u64>, k: u64) -> u64 {
    if let Some(v) = store.get(&k) {
        return *v;
    }
    let computed = square(k);
    store.insert(k, computed);
    computed
}

#[test]
fn cache_equals_recompute_over_a_repeating_key_sequence() {
    let mut store: Derived<u64, u64> = Derived::new();
    // A deterministic pseudo-sequence with repeats, so hits and misses mix.
    // No rand crate exists in this workspace (decision 0004) and none is added.
    let keys: Vec<u64> = (0..400u64).map(|i| i.wrapping_mul(2_654_435_761) % 37).collect();

    for k in &keys {
        assert_eq!(
            read_through(&mut store, *k),
            square(*k),
            "cache diverged from recomputation at {k}"
        );
    }
    assert!(store.hits() > 0, "a repeating sequence must produce hits");
    assert!(store.misses() > 0, "a cold start must produce misses");
}

#[test]
fn eviction_at_every_opportunity_changes_nothing_observable() {
    // CHAOS EVICTION -- the adversarial rung. For a Pure entry this is also
    // the KEY-COMPLETENESS test: dropping the entry forces recomputation
    // through the declared key alone, so a derivation reading anything the key
    // does not carry would diverge here.
    let keys: Vec<u64> = (0..200u64).map(|i| i.wrapping_mul(2_654_435_761) % 23).collect();

    let mut resident: Derived<u64, u64> = Derived::new();
    let mut chaotic: Derived<u64, u64> = Derived::new();
    let mut resident_out = Vec::new();
    let mut chaotic_out = Vec::new();

    for k in &keys {
        resident_out.push(read_through(&mut resident, *k));
        chaotic_out.push(read_through(&mut chaotic, *k));
        chaotic.evict_all(); // evict at EVERY legal opportunity
    }
    assert_eq!(
        resident_out, chaotic_out,
        "an evicted run must be byte-identical to a resident one"
    );
}
```

Then add a third test for `Validity`, written against the API you settle in Step 3, holding the property: a `Pure` entry is never stale at any ledger position, and a `Ledger` entry becomes stale exactly when a later fact touches a key in its `deps`.

- [ ] **Step 2: Declare the module and run to verify failure**

Add to `kernel/tests/suite.rs`:

```rust
#[path = "suite/derived.rs"]
mod derived;
```

```bash
cargo test -p hornvale-kernel --test suite derived > /tmp/hv-t2-red.log 2>&1; echo "exit=$?"; grep -E "^error" /tmp/hv-t2-red.log | head
```

Expected: FAIL to compile — `hornvale_kernel::derived` does not exist.

- [ ] **Step 3: Implement `kernel/src/derived.rs`**

Requirements, each enforced by the gate rather than by taste:

- `BTreeMap` only. `HashMap` is banned workspace-wide by `clippy.toml` `disallowed-types`.
- `#![warn(missing_docs)]` is on for the crate: doc every `pub` item, field and variant.
- Every `pub`-boundary primitive gets a `type-audit:` tag. Counters are `bare-ok(count: return)`; a ledger position is a count too.
- **No wall-clock.** `Instant`/`SystemTime` are banned by the same clippy config; an entry is versioned by ledger position, never by time.
- Generic per shape, not heterogeneous. **The module doc must state the `TypeId`-ordering reason**, because the next reader's instinct is one store for everything.
- **The module doc must also state the direction the store enforces:** it guarantees a value is consistent with its key, and it does NOT and cannot prove the key is complete — the chaos-eviction battery is what pressures that. A check that states its direction cannot be silently mistaken for a stronger guarantee.

- [ ] **Step 4: Run to verify green**

```bash
cargo test -p hornvale-kernel --test suite derived > /tmp/hv-t2-green.log 2>&1; echo "exit=$?"; grep -E "^test result" /tmp/hv-t2-green.log
```

- [ ] **Step 5: Verify chaos-eviction actually discriminates**

Give a test store a deliberately INCOMPLETE key — a derivation reading a value outside its key — and confirm `eviction_at_every_opportunity_changes_nothing_observable` goes RED. Then revert. If it stays green, the chaos test is decorative and must be strengthened before Task 3 relies on it. This is the one step that proves the campaign's central claim is mechanised rather than asserted.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings && make gate-commit
```

Then add `kernel/src/derived.rs kernel/src/lib.rs kernel/tests/suite/derived.rs kernel/tests/suite.rs` and commit.

---

## Task 3: migrate `RoomMeshMemo` onto the store, API unchanged

**Files:**
- Modify: `kernel/src/room.rs` (`RoomMeshMemo` at `:644`-`:706`, `corner_weights_memo` at `:716`, `neighbors_memo` at `:746`)
- Test: `kernel/src/room.rs` test module

**Interfaces:**
- Consumes: Task 2's `Derived<K, V>`; Task 1's four accessors.
- Produces: **no new public surface.** The public API stays exactly `RoomMeshMemo::{new, corner_weights_lookup, corner_weights_geo_level}`, `RoomAddr::{corner_weights, corner_weights_memo, neighbors, neighbors_memo}`, Task 1's four counters, and the `Debug, Default, Clone` derives.

- [ ] **Step 1: Pin the public API before touching it**

This is the hold-off's safety rope. It must pass GREEN against the pre-migration code — it is a regression rope, so write it and run it before changing anything.

```rust
#[test]
fn room_mesh_memo_public_surface_is_unchanged_by_the_forebay() {
    // The migration behind this type MUST NOT move its public API: every
    // caller is in windows/vessel, windows/locale or windows/lab, and
    // campaign/the-hand holds off two of those three. If this stops
    // compiling, the campaign's zero-vessel-edit constraint is broken and the
    // right move is to STOP, not to update the callers.
    let mut memo = RoomMeshMemo::new();
    let cloned = memo.clone();
    let _ = format!("{cloned:?}");
    let _ = RoomMeshMemo::default();

    let geo = Geosphere::new(3);
    let index = NearestCellIndex::new(&geo);
    let addr = RoomAddr::new(0, &[0, 0, 0]).expect("valid");

    let _: Option<[(CellId, u64); 3]> = addr.corner_weights(&geo, &index);
    let _: Option<[(CellId, u64); 3]> = addr.corner_weights_memo(&geo, &index, &mut memo);
    let _: [RoomAddr; 3] = addr.neighbors();
    let _: [RoomAddr; 3] = addr.neighbors_memo(&mut memo);
    let _: Option<Option<[(CellId, u64); 3]>> = memo.corner_weights_lookup(&addr);
    let _: Option<u32> = memo.corner_weights_geo_level();
    let _: (u64, u64) = (memo.corner_weights_hits(), memo.corner_weights_misses());
    let _: (u64, u64) = (memo.neighbors_hits(), memo.neighbors_misses());
}
```

- [ ] **Step 2: Capture the pre-migration baseline**

```bash
cargo test -p hornvale-kernel > /tmp/hv-kernel-before.log 2>&1; echo "exit=$?"; grep -E "^test result" /tmp/hv-kernel-before.log
```

- [ ] **Step 3: Migrate the two halves**

`neighbors` becomes `Derived<RoomAddr, [RoomAddr; 3]>`, key `RoomAddr`. `corner_weights` becomes `Derived<(RoomAddr, u32), Option<[(CellId, u64); 3]>>` — **the level enters the key**, which is the spec's whole finding: `Geosphere::new(level)` is the only constructor and the file has no `Seed`, so the level determines the geosphere and hence the value.

Consequences to honour:
- `corner_weights_lookup(&self, addr)` keeps its single-argument signature. It resolves the level from `corner_weights_geo_level()` and returns `None` (not-looked-up) when no level has been recorded yet.
- **Keep `Option<Option<_>>`.** Cached absence stays distinct from not-looked-up; `corner_weights_lookup_distinguishes_miss_from_a_cached_none` (`:1497`) must pass untouched.
- The `debug_assert_eq!` level guard may stay as a fast-fail, but the key now carries the level, so a mismatched level is a different key rather than a wrong answer.

- [ ] **Step 4: Verify green against the baseline**

```bash
cargo test -p hornvale-kernel > /tmp/hv-kernel-after.log 2>&1; echo "exit=$?"; grep -E "^test result" /tmp/hv-kernel-after.log
```

All six pre-existing `corner_weights*` tests at `:1162`, `:1194`, `:1436`, `:1467`, `:1497`, `:1542` must pass **unmodified**. If any needs editing to pass, that is a behaviour change — STOP and report rather than editing the test.

- [ ] **Step 5: Prove byte-identity at the artifact level**

The master oracle, and the rung the whole determinism argument rests on.

```bash
make rebaseline && git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); echo "drift exit=$?"
```

**Decision rule, not a prediction:**
- **Only `docs/audits/` moved** → expected; the type-audit report drifts on any `pub`-boundary change and this campaign added `pub` items. Commit it in the same commit.
- **`book/src/domesday/` or a census CSV moved** → **STOP.** A pure cache migration must not move a census. This is a determinism defect, the highest severity in the project.
- **`book/src/gallery/` moved** → **STOP**, epoch event.
- **Nothing moved at all** → suspicious, not reassuring. Confirm `make rebaseline` actually ran (it is a DAG and can no-op); a vacuous drift check reads exactly like a clean one.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt && make gate-commit
```

Then add `kernel/src/room.rs docs/audits/` and commit.

---

## Task 4: the doc-comment correction and the decision records

**Files:**
- Modify: `kernel/src/room.rs` (`corner_weights` field doc at `:646`; `corner_weights_geo_level` docs at `:660`-`:668` and `:698`-`:706`)
- Create: `docs/decisions/0206-a-derived-values-key-is-its-validity.md`
- Create: `docs/decisions/0207-the-derived-store-is-generic-per-shape.md`
- Create: `docs/decisions/0208-corner-weights-level-guard-is-total.md`

**Interfaces:** consumes Tasks 2-3. Produces nothing code-facing.

- [ ] **Step 1: Correct the doc comments**

Two comments misdescribe the code, and one is a determinism-adjacent claim:

- The `corner_weights` field doc says the map is valid "for the ONE `(geo, index)` pair this memo is used with" — implying a world. Say instead that the value is a pure function of `(RoomAddr, level)` and that the level is now part of the key.
- `corner_weights_geo_level`'s doc says "Not a full fix — two different geospheres at the SAME level would not be caught." **That case cannot produce a wrong answer**: `Geosphere::new(level)` is the only constructor and `grep -n Seed kernel/src/geosphere.rs` returns nothing, so two geospheres at the same level are byte-identical. Say the guard is total, and cite the constructor as the reason.

- [ ] **Step 2: Write the three decision records**

Read two neighbours in `docs/decisions/` first and match the format exactly (`0126-fact-day-is-a-typed-world-time.md` is a good model). Numbers must be **0206, 0207, 0208** — inside this campaign's reserved block; do not renumber into a gap outside it.

0206 must record that it refines metaplan §6.6, which named two classes but drew the line between world-derived and ledger-derived. The line actually falls at key-completeness, and "world-derived" is `Pure` with the world's identity in the key.

- [ ] **Step 3: Regenerate the digest**

The in-force decision index is a drift-checked artifact and adding a record moves it. **The `>` redirect does the writing** — running these bare regenerates nothing, and the drift check that follows then reads as clean when nothing was rebuilt.

```bash
cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md
cargo run --manifest-path tools/digest/Cargo.toml -- render delta > docs/digest/intent-vs-reality.md
git diff --stat -- docs/digest/
```

- [ ] **Step 4: Gate and commit**

```bash
cargo fmt && make gate-commit
```

Then add `kernel/src/room.rs docs/decisions/ docs/digest/` and commit.

---

## Task 5: metaplan housekeeping, and capture of what was deferred

The governing document sends the next reader to the wrong campaign, and two findings live only in a commit message.

**Files:**
- Modify: `docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`
- Modify: `book/src/frontier/idea-registry.md`

- [ ] **Step 1: Reconcile §6.4 with §6.5**

§6.4 declares §11's falsifier-1 `UNSETTLED` and prescribes "time the A\* path" as the settling experiment. §6.5, added **directly below it in the same commit** (`06a07efcc`), profiles A\* at **5.3%** of the bench. That is the answer. Record it: planning does not dominate, and the tick is bound by derived geometry and the allocator. A reader following §6.4 today re-runs a measurement already taken.

- [ ] **Step 2: Promote the harness-artifact finding out of the commit message**

`06a07efcc`'s message says: *"Wiring in the cache production always passes moved scan_at only 14.9% to 13.4%, so the harness-artifact hypothesis was wrong and the cost is real."* §6.5 does not say this. Add it — the `LocaleTerrain::new` / `cache: None` shape is visible in the harness, so the next reader raises the same doubt. This campaign did, and spent real effort re-deriving an answer that was already known.

- [ ] **Step 3: Correct §6.6's table and amend the stage table**

§6.6 lists `corner_weights` under world-derived; it is not (Task 4's decision 0206). Correct it, and note that a genuine seed-keyed tenant does exist — `domains/terrain/`'s `rills_of`/`rill_reading`, which resolve from a `Seed` via `CatchmentCut::Drawn` — at roughly 5% of the profile, unmigrated.

Then amend §6's stage table with the two gate verdicts: stage 2 **not enterable** (its gate wanted locality queries to be a real share of tick cost; they are 0.04%), and stage 7 **deferred on availability, not merit**, naming the two hold-offs.

- [ ] **Step 4: Register what was deferred**

Read the idea-registry's row format first and match it. **Grep for a duplicate row ID before adding** — a duplicate `TOOL-24` once travelled through a spec, a plan, a study JSON and a decision before anyone noticed.

- Log bounding, split in **two** per the spec: the local divergence-test restoration (verified to have **zero** committed-artifact blast radius — no committed artifact carries a ticked ledger) and the epoch-grade fact-lifetime mechanism. Note the `campaign/the-hand` coordination requirement.
- A faster `scan_at`, or a reachable-set prefill — the fix if Task 1 lands on the miss side.
- `TickSystem::step` still has nowhere to hang a cache (metaplan §5.5); vessel's `step_with_occupancy` is a local widening, not the kernel-trait fix.
- `agent_scaling.rs` clones the whole memo per tick, with `malloc`+`memcpy` at 33.3% of that bench.

- [ ] **Step 5: Gate and commit**

```bash
make gate-commit
```

Then add the metaplan and the idea registry and commit.

---

## Task 6: Definition of Done

Not optional — the project book may never lag merged reality.

**Files:**
- Create: `book/src/chronicle/the-forebay.md`
- Create: `docs/retrospectives/the-forebay.md`
- Modify: `book/src/open-questions.md` (only if a Confidence Gradient bet moved)
- Modify: whichever book chapters the freshness sweep finds stale

- [ ] **Step 1: Write the chronicle entry**

Book altitude: technical and mathematical, comprehensible without reading the code it may show. The story is the correction — a doc comment that understated its own guarantee, believed over the type it documented, and a design section rebuilt when the constructor was finally read.

- [ ] **Step 2: Write the retrospective (process, not product)**

Must carry: that three prior bash 3.2 fixes were spot fixes and the fourth and fifth arrived anyway; that `shellcheck` exits 0 on a file bash cannot parse; that the same bash 3.2 defect broke this campaign's own commit message through a heredoc inside `$(...)`, truncating it mid-sentence; the coordination with `campaign/the-hand` over the wire and its board reply; and that a hook correctly refused a plan step that ran the suite twice to ask two questions.

- [ ] **Step 3: Book freshness sweep**

Check every chapter describing derived values, caching, or the ledger's read path. Decision 0030: a campaign that resolves or moves a Confidence Gradient bet re-scores that chapter as part of the sweep.

- [ ] **Step 4: Regenerate artifacts and verify the drift check**

```bash
make rebaseline && git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); echo "exit=$?"; mdbook build book
```

- [ ] **Step 5: Gate, push, and submit to the sluice**

```bash
make gate-commit && git push
```

Then use the `submitting-to-the-sluice` skill. Submit with a **full SHA**, never a branch name:

```bash
make sluice-stage BRANCH=campaign/the-forebay REF=$(git rev-parse HEAD)   # at a stage boundary
make sluice       BRANCH=campaign/the-forebay REF=$(git rev-parse HEAD)   # to merge
```

Do **not** push to `main` by hand — `scripts/hooks/pre-push` refuses any pusher not holding the canonical box's live claim, which only the chamber holds.

- [ ] **Step 6: Close via the skill**

Use `closing-a-campaign`. Note the census question explicitly: this campaign is a pure cache migration and must move **no** census metric, so a moved column is a defect to investigate, never a refresh to accept.

---

## Self-review

**Spec coverage.** §1/§1.1 → Task 5 Step 3 and the Global Constraints. §2.1 → Task 4 Step 1 and Task 3 Step 3. §2.2 → Task 2 Step 3 and Task 4 Step 2. §2.3 → Task 2 Step 5 and the module doc's direction statement. §3 → Task 2 Step 3 and Task 3 Step 1. §4 → Task 1 entire. §5 → Task 2 Steps 1/5, Task 3 Steps 4/5. §6 → Task 0. §7 → Task 5. §8 → Task 4 Step 2. §9's out-of-scope items → Task 5 Step 4, registered rather than dropped. §10's falsifiers → Task 0 Step 3, Task 1 Step 6, Task 3 Step 1.

**Placeholder scan.** No "TBD"/"TODO"/"handle edge cases". Four places defer to the implementer deliberately, each naming the *property* rather than prescribing a mutation from outside the code: Task 0 Step 2's case-expansion grep, Task 1 Step 4, Task 2 Step 1's third test, Task 2 Step 5. Task 1 Step 1 flags that `RoomAddr::new`'s signature must be read rather than trusted from this plan.

**Type consistency.** The four counter names are fixed in Task 1's Interfaces and reused verbatim in Task 3 Step 1's surface test. `Derived<K, V>`'s method set is fixed in Task 2's Interfaces and consumed in Task 3 Step 3. `corner_weights_lookup` keeps `Option<Option<[(CellId, u64); 3]>>` in both Task 3 Step 3 and the surface test. Decision numbers 0206/0207/0208 agree between Task 4's file list and its Step 2.

**Known gap, deliberate.** Task 1 Step 6's measurement site is unresolved pending a reply from `campaign/the-hand`. It is a branch table rather than a guess, and the silence branch avoids the vessel edit rather than defaulting into it.

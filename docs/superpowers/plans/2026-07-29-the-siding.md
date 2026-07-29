# The Siding Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make lefford a heavy-tier-only box whose long jobs serialize on one
first-come-first-served claim, and move campaign worktrees and the commit gate
to the Mac.

**Architecture:** No domain, kernel, or window logic changes. The existing
claim seam (`windows/lab/src/census_claim.rs`, decision 0081) and the existing
canonical-host guard (`scripts/census-canonical-host.sh`, decision 0079) are
both reused as-is; the campaign adds a second job kind (`heavy`) that shares
them, moves the claim acquisition into `scripts/gate-full-heavy.sh` so every
entry point inherits it, and adds `scripts/heavy-run.sh` + `make heavy-remote`
to dispatch the tier from the Mac the way `census-run.sh` dispatches a census.

**Tech Stack:** bash (shellcheck-clean, `set -euo pipefail`), GNU `flock`,
Rust 2024 (std only), `make`, `cargo nextest`.

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` only, workspace-wide. No new
  crates. Shell scripts use `flock`/`timeout`/coreutils, already required by
  `census-run.sh`.
- Every crate sets `#![warn(missing_docs)]`; every public item, field, and
  variant gets a one-line doc comment.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.
  Grammar, exactly: `type-audit: bare-ok(<class>)` for a single item, or
  `type-audit: bare-ok(<class>: <field>), bare-ok(<class>: <field>)` for a
  struct — the class comes first, the field name after the colon.
- Every script under `scripts/**` must pass `make shellcheck`. Prefer explicit
  `if`/`then` over `A && B || C` (SC2015).
- `cargo fmt` is the final step before every commit.
- The commit gate is `make gate`. **Run it on the Mac, not lefford** — that is
  what this campaign establishes.
- Decisions are append-only: supersede, never edit.

---

### Task 1: Settle A2 — do the heavy-tier sweeps diverge across hosts?

The spec's load-bearing unknown. It decides whether Task 6 exists at all, so
it runs first. **This is a measurement, not a code change** — the deliverable
is a recorded result, and either outcome is a valid finding.

**Files:**
- Modify: `docs/superpowers/specs/2026-07-29-the-siding-design.md` (record the
  A2 outcome)

**Interfaces:**
- Consumes: nothing.
- Produces: a yes/no answer that gates Task 6.

- [ ] **Step 1: Wait for lefford to be quiet**

Run: `ssh lefford 'uptime; pgrep -af cargo-nextest | head'`
Expected: load average below ~5 and no `cargo-nextest` processes. If runs are
still in flight, wait — a contended box does not invalidate this measurement,
but a *concurrent writer* to the same artifacts would.

- [ ] **Step 2: Capture the current committed artifacts as the baseline**

```bash
cd /Users/nathan/Projects/hornvale/hornvale
git rev-parse HEAD > /tmp/siding-ref.txt
cat /tmp/siding-ref.txt
```

- [ ] **Step 3: Regenerate the two sweeps on the Mac**

```bash
cd /Users/nathan/Projects/hornvale/hornvale
cargo nextest run --workspace --run-ignored only \
  -E 'test(/run_the_sounding_and_write_the_report$/) | test(/history_gates_full_world_and_cross_seed$/)'
git diff --stat book/src/laboratory/generated/the-history book/src/laboratory/generated/the-sounding
git diff book/src/laboratory/generated/the-history book/src/laboratory/generated/the-sounding \
  > /tmp/siding-mac.diff
git checkout -- book/src/laboratory/generated/the-history book/src/laboratory/generated/the-sounding
```

Both test names are verified as of 2026-07-29:
`history_gates_full_world_and_cross_seed` is at
`cli/tests/history_battery.rs:164` and writes `the-history`;
`run_the_sounding_and_write_the_report` is at
`windows/chronicle/tests/sounding_sweep.rs:15` and writes `the-sounding`. If
either has moved, re-derive it with
`grep -rn 'heavy: live-worldgen' -A1 --include='*.rs' .` — the roster is
discovered from source, never hand-maintained.

- [ ] **Step 4: Regenerate the same two sweeps on lefford at the same ref**

```bash
REF=$(cat /tmp/siding-ref.txt)
ssh lefford "cd ~/Projects/hornvale && git fetch --all --quiet && \
  git -c advice.detachedHead=false checkout --force $REF && \
  cargo nextest run --workspace --run-ignored only \
    -E 'test(/run_the_sounding_and_write_the_report\$/) | test(/history_gates_full_world_and_cross_seed\$/)' && \
  git diff book/src/laboratory/generated/the-history book/src/laboratory/generated/the-sounding" \
  > /tmp/siding-lefford.diff
```

- [ ] **Step 5: Compare the two diffs**

```bash
diff /tmp/siding-mac.diff /tmp/siding-lefford.diff && echo "A2 CLEAN: hosts agree" \
  || echo "A2 DIRTY: hosts disagree — Task 6 is required"
```

Expected: one of two outcomes, both valid.
- **CLEAN** — the sweeps are host-independent. A1 retires; **skip Task 6.**
- **DIRTY** — the sweeps carry the 0063 divergence. **Task 6 is required**,
  and the artifacts must be regenerated on lefford before any commit.

- [ ] **Step 6: Record the outcome in the spec**

Replace the A2 paragraph's "UNMEASURED" wording with the measured result, the
date, the ref, and the command used. Keep it factual: state what was compared
and what came back, not what it implies.

- [ ] **Step 7: Reset lefford's checkout and commit**

```bash
ssh lefford 'cd ~/Projects/hornvale && git checkout --force main && \
  git checkout -- book/src/laboratory/generated/'
cd /Users/nathan/Projects/hornvale/hornvale
git add docs/superpowers/specs/2026-07-29-the-siding-design.md
git commit -m "docs(the-siding): record the A2 cross-host divergence measurement"
```

---

### Task 2: `status_line()` reports the job kind, not always "census"

`status_line()` hardcodes `"census running"` / `"no census running"`, and
`scripts/census-advisory.sh` pattern-matches `"census running"*`. Once the
heavy tier takes the same claim, a heavy holder would be announced as a
census. Fix the seam before anything writes a `heavy` claim.

**Files:**
- Modify: `windows/lab/src/census_claim.rs:446-460` (`status_line`)
- Modify: `scripts/census-advisory.sh` (widen the pattern)
- Test: `windows/lab/src/census_claim.rs` (the existing `mod tests`)

**Interfaces:**
- Consumes: `ClaimInfo { pid, host, user, started, goldens, label, reference,
  cmdline }` and `live_holder_at(&Path) -> Option<ClaimInfo>`, both already in
  this module.
- Produces: `status_line() -> String`, unchanged signature. Output becomes
  `"<kind> running: pid …"` where `<kind>` is `census` when `label` starts
  with `census`, else `heavy`. The no-holder case becomes
  `"no heavy run in progress"`.

- [ ] **Step 1: Write the failing test**

Add to `mod tests` in `windows/lab/src/census_claim.rs`:

```rust
#[test]
fn the_status_line_names_the_job_kind_from_the_label() {
    assert_eq!(job_kind("census-run"), "census");
    assert_eq!(job_kind("census-goldens"), "census");
    assert_eq!(job_kind("heavy"), "heavy");
    assert_eq!(job_kind("gate-full-heavy"), "heavy");
}
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cargo test -p hornvale-lab the_status_line_names_the_job_kind_from_the_label`
Expected: FAIL — `cannot find function 'job_kind' in this scope`.

- [ ] **Step 3: Implement `job_kind` and use it**

In `windows/lab/src/census_claim.rs`, add above `status_line`:

```rust
/// Which kind of heavy job a claim `label` denotes. The claim is shared by
/// censuses and the heavy tier (The Siding); announcing a heavy run as a
/// census is exactly the misreport decision 0081 exists to prevent.
/// type-audit: bare-ok(identifier-text: label), bare-ok(identifier-text: return)
fn job_kind(label: &str) -> &'static str {
    if label.starts_with("census") {
        "census"
    } else {
        "heavy"
    }
}
```

Then replace the body of `status_line`'s `match`:

```rust
    match live_holder_at(&path) {
        None => "no heavy run in progress".to_string(),
        Some(h) => format!(
            "{} running: pid {} ({}@{}) since {}, writing {}, running {} @ {}",
            job_kind(&h.label),
            h.pid,
            h.user,
            h.host,
            h.started,
            h.goldens,
            h.label,
            h.reference
        ),
    }
```

- [ ] **Step 4: Run the test and confirm it passes**

Run: `cargo test -p hornvale-lab the_status_line_names_the_job_kind_from_the_label`
Expected: PASS.

- [ ] **Step 5: Widen the advisory's pattern**

In `scripts/census-advisory.sh`, change the `case` arm from
`"census running"*)` to:

```bash
    "census running"*|"heavy running"*)
```

and change the first `echo` body to read `note: $status` unchanged (it already
prints the full line, which now names the kind).

- [ ] **Step 6: Verify nothing else matched the old string**

Run: `grep -rn "no census running\|census running" --include='*.rs' --include='*.sh' --include='Makefile' . | grep -v '/target/'`
Expected: only `census_claim.rs` (the new format string) and
`census-advisory.sh` (the widened pattern). Any other hit is a consumer that
must be updated in this task.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add windows/lab/src/census_claim.rs scripts/census-advisory.sh
git commit -m "feat(the-siding): the claim status line names the job kind"
```

---

### Task 3: The heavy tier claims the box at its own seam

Put the claim in `gate-full-heavy.sh` rather than only in the dispatch
wrapper, so `make gate-full` inherits it too. This is decision 0081's stated
lesson — guard at the seam, because a wrapper cannot guard a direct
invocation.

**Files:**
- Modify: `scripts/gate-full-heavy.sh`
- Test: `scripts/test-heavy-lock.sh` (create)

**Interfaces:**
- Consumes: `HV_CENSUS_LOCK_HELD` (existing env var, names a holder PID),
  `/tmp/hv-census.lock`, `/tmp/hv-census.claim`.
- Produces: `gate-full-heavy.sh` blocks while another heavy job holds the box;
  honours `HV_CENSUS_LOCK_HELD` to avoid deadlocking against its own ancestor.

- [ ] **Step 1: Write the failing mutual-exclusion test**

Create `scripts/test-heavy-lock.sh`:

```bash
#!/usr/bin/env bash
# scripts/test-heavy-lock.sh — prove the heavy claim EXCLUDES, not merely that
# it exists. Asserting a lock is present is not asserting it serialises; this
# repo has shipped tests that assert nothing.
set -uo pipefail
cd "$(git rev-parse --show-toplevel)"

LOCK="$(mktemp -u /tmp/hv-siding-test-XXXXXX.lock)"
export HV_CENSUS_LOCK="$LOCK"
fail=0

# A holder that keeps the lock for 3s.
( exec 9>"$LOCK"; flock 9; sleep 3 ) &
holder=$!
sleep 0.5

# A second acquirer with a 1s timeout must FAIL to get it.
start=$SECONDS
if ( exec 9>"$LOCK"; flock -w 1 9 ); then
    echo "FAIL: second acquirer got the lock while it was held" >&2
    fail=1
else
    echo "ok: second acquirer was excluded while the lock was held"
fi

wait "$holder"

# After the holder exits, the lock must be immediately available.
if ( exec 9>"$LOCK"; flock -w 5 9 ); then
    echo "ok: lock released when the holder exited"
else
    echo "FAIL: lock still held after the holder exited" >&2
    fail=1
fi

# A KILLED holder must also release it (flock frees the fd on process death).
( exec 9>"$LOCK"; flock 9; sleep 30 ) &
killed=$!
sleep 0.5
kill -9 "$killed" 2>/dev/null
wait "$killed" 2>/dev/null
if ( exec 9>"$LOCK"; flock -w 5 9 ); then
    echo "ok: lock released when the holder was killed"
else
    echo "FAIL: lock wedged after the holder was killed" >&2
    fail=1
fi

rm -f "$LOCK"
[ "$fail" -eq 0 ] && echo "test-heavy-lock: PASS" || echo "test-heavy-lock: FAIL" >&2
exit "$fail"
```

- [ ] **Step 2: Run it and confirm it passes on its own**

```bash
chmod +x scripts/test-heavy-lock.sh
bash scripts/test-heavy-lock.sh
```

Expected: PASS. This validates the *mechanism* before it is wired in — if
`flock` does not behave this way on the box, everything downstream is built on
sand.

- [ ] **Step 3: Add the claim to `gate-full-heavy.sh`**

In `scripts/gate-full-heavy.sh`, immediately after
`cd "$(git rev-parse --show-toplevel)"`, insert:

```bash
# The heavy tier claims the box (The Siding; decision 0081's seam rule).
# It shares ONE claim with the census: both saturate the machine, and the
# binding constraint is the machine, not the directory. Skipped when an
# ancestor already holds it (HV_CENSUS_LOCK_HELD), because flock is per
# open-file-description and a child re-flocking the same path on a fresh fd
# would DEADLOCK against its own parent.
LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
held_by="${HV_CENSUS_LOCK_HELD:-}"
if [ -n "$held_by" ] && kill -0 "$held_by" 2>/dev/null; then
    echo "gate-full-heavy: box already claimed by ancestor pid $held_by — proceeding" >&2
else
    exec 9>"$LOCK"
    timeout_s="${HV_HEAVY_WAIT_TIMEOUT:-2700}"
    echo "gate-full-heavy: waiting for the box claim ($LOCK; up to ${timeout_s}s) …" >&2
    wait_began=$SECONDS
    if ! flock -w "$timeout_s" 9; then
        echo "gate-full-heavy: TIMED OUT after ${timeout_s}s waiting for the box." >&2
        echo "gate-full-heavy: $(cargo run --quiet --release -p hornvale -- lab claim-status 2>/dev/null || echo 'holder unknown')" >&2
        exit 75
    fi
    waited=$((SECONDS - wait_began))
    if [ "$waited" -gt 0 ]; then
        echo "gate-full-heavy: box claimed after ${waited}s queued" >&2
    fi
    export HV_CENSUS_LOCK_HELD=$$
fi
```

Note the default timeout inherits the census's 2700s. It is a **placeholder**
(spec A3); Task 7 revisits it against a measured uncontended run.

- [ ] **Step 4: Verify the ancestor-skip path does not deadlock**

```bash
HV_CENSUS_LOCK_HELD=$$ bash -c 'bash scripts/gate-full-heavy.sh --help 2>&1 | head -3' || true
```

Expected: the "already claimed by ancestor" line appears and the script
proceeds rather than hanging. If it hangs, the `kill -0` guard is wrong — stop
and fix before continuing.

- [ ] **Step 5: shellcheck both scripts**

Run: `make shellcheck`
Expected: clean. `make shellcheck` covers all of `scripts/**`, so the new test
script is included automatically.

- [ ] **Step 6: Commit**

```bash
git add scripts/gate-full-heavy.sh scripts/test-heavy-lock.sh
git commit -m "feat(the-siding): the heavy tier claims the box at its own seam"
```

---

### Task 4: `scripts/heavy-run.sh` and `make heavy-remote`

The dispatch path: run the heavy tier on lefford from the Mac, at a named ref,
in a scratch worktree, with the artifact diff surfaced for review.

**Files:**
- Create: `scripts/heavy-run.sh`
- Modify: `Makefile` (add `heavy-remote`, extend `.PHONY`)

**Interfaces:**
- Consumes: `require_canonical_census_host` from
  `scripts/census-canonical-host.sh`; `scripts/gate-full-heavy.sh` (Task 3);
  `scripts/timed.sh <label> -- <command...>`.
- Produces: `scripts/heavy-run.sh [status]`, honouring `HV_HEAVY_REF` and
  `HV_HEAVY_WORKTREE`; `make heavy-remote REF=<sha>`.

- [ ] **Step 1: Create the script**

```bash
#!/usr/bin/env bash
# scripts/heavy-run.sh — run the heavy tier on THIS box, the single canonical
# platform for the artifacts it authors (The Siding; decisions 0063/0079/0081).
#
# The heavy tier is not merely expensive, it is an AUTHORING path: three of its
# tests write committed artifacts (book/src/laboratory/generated/the-history,
# .../the-sounding, windows/worldgen/tests/fixtures/occupancy.csv), and
# census_fixtures_match_a_probe_of_live_seeds compares a LIVE probe against
# lefford-authored census fixtures. So it carries the same canonical-host guard
# a census does, for the same reason: a wrong-host run does not fail, it
# commits values that drift-check green forever.
#
# Usage — locally on the canonical box, or from the other machine via SSH:
#   scripts/heavy-run.sh                     # run the heavy tier here
#   HV_HEAVY_REF=<sha> scripts/heavy-run.sh  # fetch + run that ref in a
#                                            # dedicated scratch worktree
#   scripts/heavy-run.sh status              # is a heavy job holding the box?
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# `status` before the host guard and before the lock: asking is not authoring,
# so it is legal from any machine and must never block (decision 0081).
if [ "${1:-}" = "status" ]; then
    cargo run --quiet --release -p hornvale -- lab claim-status
    exit $?
fi

# shellcheck source=scripts/census-canonical-host.sh
. "$(dirname "$0")/census-canonical-host.sh"
require_canonical_census_host || exit 1

run_root="$repo_root"
if [ -n "${HV_HEAVY_REF:-}" ]; then
    # Run a specific ref in a dedicated worktree so the caller's checkout (and
    # this canonical one) are left untouched. Outside the repo, to keep
    # `git status` clean here.
    wt="${HV_HEAVY_WORKTREE:-$repo_root/../hornvale-heavy-wt}"
    git -C "$repo_root" fetch --all --quiet
    if [ -d "$wt/.git" ] || git -C "$repo_root" worktree list --porcelain | grep -qF "$wt"; then
        git -C "$wt" fetch --all --quiet
        git -C "$wt" checkout --force "$HV_HEAVY_REF"
        git -C "$wt" reset --hard "$HV_HEAVY_REF" --quiet
    else
        git -C "$repo_root" worktree add --force "$wt" "$HV_HEAVY_REF"
    fi
    run_root="$wt"
    echo "heavy-run: running ref '$HV_HEAVY_REF' in $wt" >&2
    echo "heavy-run: HEAD there is $(git -C "$wt" rev-parse --short HEAD)" >&2
fi

cd "$run_root"

# Publish the claim BEFORE gate-full-heavy.sh takes the flock, and announce
# the hold so that script skips re-acquiring against its own ancestor.
claim_path="${HV_CENSUS_CLAIM_PATH:-/tmp/hv-census.claim}"
LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
exec 9>"$LOCK"
timeout_s="${HV_HEAVY_WAIT_TIMEOUT:-2700}"
echo "heavy-run: waiting for the box claim ($LOCK; up to ${timeout_s}s) …" >&2
wait_began=$SECONDS
if ! flock -w "$timeout_s" 9; then
    echo "heavy-run: TIMED OUT after ${timeout_s}s waiting for the box." >&2
    echo "heavy-run: $(cargo run --quiet --release -p hornvale -- lab claim-status 2>/dev/null || echo 'holder unknown')" >&2
    exit 75
fi
HV_HEAVY_WAITED_S=$((SECONDS - wait_began))
export HV_HEAVY_WAITED_S
echo "heavy-run: box claimed at $(date -Is) after ${HV_HEAVY_WAITED_S}s queued" >&2
export HV_CENSUS_LOCK_HELD=$$

{
    echo "pid=$$"
    echo "host=$(hostname -s 2>/dev/null || echo '-')"
    echo "user=${USER:-unknown}"
    echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "goldens=$run_root/book/src/laboratory/generated"
    echo "label=heavy"
    echo "ref=$(git -C "$run_root" branch --show-current 2>/dev/null || echo '-')@$(git -C "$run_root" rev-parse --short HEAD 2>/dev/null || echo '-')"
    echo "cmdline=heavy-run.sh $*"
} > "$claim_path"
# Release the claim on EVERY exit path, including the error ones, so a failed
# run never wedges the box.
trap 'rm -f "$claim_path"; echo "heavy-run: finished at $(date -Is)" >&2' EXIT

bash scripts/timed.sh heavy -- bash scripts/gate-full-heavy.sh

echo "heavy-run: heavy tier finished. Review and commit the artifacts HERE" >&2
echo "heavy-run: (this box authors them — decisions 0063/0079):" >&2
git -C "$run_root" diff --stat book/src/laboratory/generated \
    windows/worldgen/tests/fixtures/occupancy.csv >&2 || true
```

- [ ] **Step 2: Make it executable and shellcheck it**

```bash
chmod +x scripts/heavy-run.sh
make shellcheck
```
Expected: clean.

- [ ] **Step 3: Verify `status` works from the Mac and does not block**

Run: `bash scripts/heavy-run.sh status`
Expected: prints `no heavy run in progress` (Task 2's wording) and exits 0
**on the Mac**, i.e. it does not hit the host guard. If it prints a refusal,
the `status` early-exit is below the guard — move it above.

- [ ] **Step 4: Verify the host guard refuses the run path on the Mac**

Run: `bash scripts/heavy-run.sh; echo "exit=$?"`
Expected: the `REFUSING to run on …` message and `exit=1`. This is the guard
working, not a failure.

- [ ] **Step 5: Add the Makefile target**

Add to `.PHONY` on line 25: `heavy-remote`. Then add after the `gate-full`
target:

```makefile
heavy-remote: ## Run the heavy tier on the canonical box (The Siding); REF=<sha> required
	@test -n "$(REF)" || { echo "usage: make heavy-remote REF=<full-sha>"; exit 1; }
	ssh lefford 'cd ~/Projects/hornvale && HV_HEAVY_REF=$(REF) scripts/heavy-run.sh'
```

Pass a **SHA**, not a branch name: `HV_HEAVY_REF` feeds `reset --hard`, which
can otherwise land on a stale local branch of that name on the canonical box.

- [ ] **Step 6: Verify the target's argument check**

Run: `make heavy-remote`
Expected: `usage: make heavy-remote REF=<full-sha>` and a non-zero exit,
without opening an SSH connection.

- [ ] **Step 7: Commit**

```bash
git add scripts/heavy-run.sh Makefile
git commit -m "feat(the-siding): add heavy-run.sh and make heavy-remote"
```

---

### Task 5: Document the placement rule and ratify the decision

**Files:**
- Create: `docs/decisions/0083-the-heavy-tier-runs-on-the-canonical-box.md`
  (confirm the next free number first — see Step 1)
- Modify: `CLAUDE.md` (Commands + Process sections)
- Modify: `scripts/CLAUDE.md` (the gate ladder section)

**Interfaces:**
- Consumes: the spec's §3 decisions and §4a placement table.
- Produces: the durable record; no code depends on this task.

- [ ] **Step 1: Confirm the next free decision number**

Run: `ls docs/decisions/ | tail -5`
Use the next unused number. Do not reuse or renumber — the log is append-only.

- [ ] **Step 2: Write the decision record**

Follow the house form used by `0081`: `# NNNN. <title>`, then
`**Status:** Accepted (2026-07-29) · **Decider:** Nathan · **Refines:**
[0081](...)`, then the context, the ruling, and the consequences. It must
state: the placement table; that the claim is shared and FCFS; that the
commit gate is convention-only with no guard; and that the heavy tier is an
authoring path, which is *why* it inherits the canonical-host guard.

- [ ] **Step 3: Add the placement rule to root `CLAUDE.md`**

In the Commands section, immediately above the gate-ladder code block, add:

```markdown
**Where things run (The Siding).** Campaign worktrees and the commit gate run
on the **Mac**; the **heavy tier and censuses run on lefford**, which is the
canonical box for the artifacts they author. `make gate` on lefford is not
forbidden, but it oversubscribes a box whose other jobs are long — dispatch
the heavy tier instead with `make heavy-remote REF=<sha>`.
```

- [ ] **Step 4: Add it to `scripts/CLAUDE.md`**

In the gate-ladder section, add a `heavy-run.sh` bullet beside the existing
`census-run.sh` one, naming the shared claim and the three artifacts the tier
authors.

- [ ] **Step 5: Verify the docs drift-check still passes**

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS. Note this check forbids citing registry IDs outside
`book/src/frontier/` — do not reference any idea-registry row in `CLAUDE.md`.

- [ ] **Step 6: Commit**

```bash
git add docs/decisions/ CLAUDE.md scripts/CLAUDE.md
git commit -m "docs(the-siding): ratify the heavy-tier placement rule"
```

---

### Task 6: Guard the write seam in Rust — ONLY IF Task 1 came back DIRTY

**Skip this task entirely if Task 1 recorded A2 as CLEAN.** If the hosts
agree, there is nothing to protect and this guard would be pure cost.

**Files:**
- Modify: `cli/tests/history_battery.rs` (the artifact-write site, ~line 300)
- Modify: `windows/chronicle/tests/sounding_sweep.rs` (~line 81)
- Modify: `windows/worldgen/tests/occupancy_readout.rs` (~line 196)

**Interfaces:**
- Consumes: `hornvale_lab::census_guard` — read it first
  (`windows/lab/src/census_guard.rs`) and mirror its shape rather than
  inventing a second one.
- Produces: a wrong-host run of these three tests fails loudly instead of
  writing divergent artifacts.

- [ ] **Step 1: Read the existing guard**

Run: `cat windows/lab/src/census_guard.rs`
Understand how it reads `scripts/census-canonical-host.txt` via
`include_str!` — one file, two readers, so the hostname is never hardcoded
twice. Do not add a third copy.

- [ ] **Step 2: Write the failing test**

Add to `cli/tests/history_battery.rs`:

The guard module exposes `current_hostname() -> String` and the
`CANONICAL_CENSUS_HOST: &str` constant (verified 2026-07-29). There is **no**
`is_canonical_host()` — compare the two yourself, case-insensitively, exactly
as `scripts/census-canonical-host.sh` does:

```rust
#[test]
fn the_history_artifact_write_is_host_guarded() {
    // The sweep authors a committed artifact and the hosts were measured to
    // disagree (The Siding, A2), so writing it off the canonical box would
    // commit values that drift-check green forever (decision 0079).
    use hornvale_lab::census_guard::{CANONICAL_CENSUS_HOST, current_hostname};
    let here = current_hostname().to_lowercase();
    let want = CANONICAL_CENSUS_HOST.trim().to_lowercase();
    assert_eq!(
        here, want,
        "the-history must be authored on the canonical box '{want}', not '{here}'"
    );
}
```

- [ ] **Step 3: Run it and confirm it fails on the Mac**

Run: `cargo test -p hornvale --test history_battery the_history_artifact_write_is_host_guarded`
Expected: FAIL on the Mac naming both hostnames; PASS on lefford. If
`cli/` cannot see `hornvale_lab`, check `cli/Cargo.toml` — the CLI already
depends on `hornvale-lab`, so no new dependency is needed or permitted.

- [ ] **Step 4: Apply the same guard at each of the three write sites**

Insert the assertion immediately before each `fs::write` block identified in
the spec's §2 table. Repeat the code at each site rather than referring back
to this step — an implementer may read tasks out of order.

- [ ] **Step 5: Verify on both boxes**

```bash
cargo nextest run --workspace --run-ignored only -E 'test(/the_history_artifact_write_is_host_guarded$/)'
ssh lefford 'cd ~/Projects/hornvale && cargo nextest run --workspace --run-ignored only -E "test(/the_history_artifact_write_is_host_guarded\$/)"'
```
Expected: fails on the Mac, passes on lefford.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add cli/tests/history_battery.rs windows/chronicle/tests/sounding_sweep.rs windows/worldgen/tests/occupancy_readout.rs
git commit -m "feat(the-siding): guard the heavy tier's artifact writes by host"
```

---

### Task 7: Measure A3, migrate the live worktrees, and gate

**Files:**
- Modify: `docs/superpowers/specs/2026-07-29-the-siding-design.md` (record A3)
- Modify: `scripts/gate-full-heavy.sh` and `scripts/heavy-run.sh` if the
  measured runtime justifies changing the timeout

- [ ] **Step 1: Measure the heavy tier uncontended on lefford**

```bash
ssh lefford 'cd ~/Projects/hornvale && uptime && scripts/heavy-run.sh'
```
Expected: completes and appends a `heavy` row to `docs/timings.md` with
`wall_s`, `cpu_ratio`, and `waited_s`. Record the wall time — this is A3's
answer and the first real datapoint (the only prior number, 39:09, was under
3× contention and is not comparable).

- [ ] **Step 2: Verify the run changed nothing (spec §7, zero diff)**

```bash
ssh lefford 'cd ~/Projects/hornvale && git diff --exit-code \
  book/src/laboratory/generated windows/worldgen/tests/fixtures/occupancy.csv \
  && echo "ZERO DIFF: the claim changed nothing computed"'
```

Expected: exit 0 and the confirmation line. Serialising a run must not alter
what it computes — this is the same consequence decision 0081 asserted for the
census. **A non-zero diff here is a stop condition**, not something to
rebaseline away: it means either the artifacts were stale before this campaign
or the run is not reproducible, and both need diagnosis before the campaign
proceeds.

- [ ] **Step 3: Revisit the timeout against the measurement**

If the measured wall time is well under the inherited 2700s default, leave it
and say so in the spec. If a queue two deep would exceed it, raise it
deliberately and record the reasoning. Do not tune it silently.

- [ ] **Step 4: Migrate the campaigns still on lefford**

For each of `the-watershed`, `the-wearing`, `the-winnowing`:

```bash
ssh lefford 'cd ~/.config/superpowers/worktrees/hornvale/<name> && git status --short && git log --oneline -1'
```

If the campaign is **mid-measurement** (a preregistered study whose baseline
is taken but readout is not), **do not move it** — record it as deferred with
that reason and move on. Otherwise: push the branch, promote any
`.superpowers/sdd/` scratch ledger *before* teardown (it dies with the
worktree), then re-create the worktree on the Mac and remove it on lefford.

- [ ] **Step 5: Run the commit gate on the Mac**

Run: `make gate`
Expected: green, and within its ~4-minute budget on a quiet box. Capture the
output once and inspect it — do not re-run the suite to grep a second line.

- [ ] **Step 6: Record A3 and commit**

```bash
git add docs/superpowers/specs/2026-07-29-the-siding-design.md docs/timings.md scripts/
git commit -m "docs(the-siding): record the heavy tier's uncontended runtime"
```

---

## Definition of Done

Beyond the tasks above, the campaign closes with the standard Hornvale DoD —
use the `closing-a-campaign` skill, which covers the chronicle entry
(`book/src/chronicle/`), the stale-chapter freshness sweep, the Confidence
Gradient re-score if any bet moved, and the retrospective in
`docs/retrospectives/`. Carry A1 forward as a follow-up if Task 1 came back
CLEAN and Task 6 was skipped.

# The Sluice Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a strictly serial merge queue on lefford that gates the
*merge product* (`main` + candidate) rather than a campaign branch tip, and
advances `origin/main` only through it.

**Architecture:** A script owns the mechanical path — enqueue, a cheap
mergeability check *outside* the lane claim, then inside one claim: a detached
checkout of `origin/main`, `git merge --no-ff <branch>`, six suites run as one
lane job, artifact regeneration committed, and `git push origin HEAD:main`. A
Claude session owns judgment only: conflict resolution, red triage, and the
hold call. `gate-campaign` retires to a refusing signpost.

**Tech Stack:** bash (dash-compatible where it runs on lefford), git 2.39.5,
`flock`, the existing `scripts/lane-*.sh` machinery, one Rust test in
`cli/tests/`.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-08-15-the-sluice-merge-queue-design.md`.
  Where this plan and the spec disagree, the spec governs.
- **Shell:** every script must pass `make shellcheck`. Prefer explicit
  `if`/`then` over `A && B || C` (SC2015).
- **`git clean -fd`, NEVER `-fdx`.** `target/` is gitignored and `-x` destroys
  lefford's ~15 GB warm build cache, turning every job into a cold build.
- **Refs are full 40-char SHAs, never branch names**, anywhere a ref feeds
  `reset --hard` or `checkout`. `lane-dispatch.sh` already validates this shape;
  copy it.
- **No new workspace dependencies** (decision 0004). The one Rust test uses
  `std` only.
- **The lane claim is shared**: `${HV_CENSUS_LOCK:-/tmp/hv-census.lock}` and
  `${HV_CENSUS_CLAIM_PATH:-/tmp/hv-census.claim}` (decisions 0081/0086/0133).
  The queue takes the same claim as the census and the heavy tier. Never a
  second, private lock.
- **Durable state lives in `~/.local/state/hornvale/sluice/`**, beside the
  lane's own `jobs.tsv`, never `/tmp` — `/tmp` does not survive a reboot.
- **Every new script gets a header comment** in the register of
  `scripts/lane-run.sh`: what it does, what failure it exists to prevent, and
  the evidence. This repo's scripts carry their reasoning; match that.

---

## File Structure

| File | Responsibility |
|---|---|
| `scripts/sluice-queue.sh` | The durable queue: `add`, `next`, `set-state`, `list`. Ancestry coalescing. No git checkouts, no claim. |
| `scripts/sluice-mouth.sh` | The four pre-claim checks, including `git merge-tree`. Read-only; never acquires the claim. |
| `scripts/sluice-run.sh` | The chamber. Runs on lefford under the shared claim: merge, phases, artifact commits, push. |
| `scripts/sluice-request.sh` | The caller's side. Runs anywhere; validates, `ssh`es, returns a request id. |
| `scripts/test-sluice.sh` | Property tests for the three above, in the shape of `scripts/test-lane.sh`. |
| `cli/tests/census_duration.rs` | The 900 s census tripwire. |
| `scripts/lane-sets.tsv` | Gains the `integration` row. |
| `Makefile` | `sluice`, `sluice-status`, `sluice-log`; `gate-campaign` becomes a refusing signpost. |
| `docs/decisions/0139-main-advances-only-through-the-lock.md` | The ratified decision. |

**Queue file format** — `~/.local/state/hornvale/sluice/queue.tsv`, TSV,
append-and-rewrite under its own `flock` (distinct from the lane claim, because
enqueueing must never block on a running gate):

```
enqueued_utc <TAB> request_id <TAB> branch <TAB> sha <TAB> state <TAB> note
```

`state` ∈ `queued` | `running` | `held` | `landed` | `superseded` | `dropped`.

---

## Task 0: The lane's claim is unparseable, so `status` lies

**Added at execution time, before Task 1 was dispatched.** Task 1 Step 1 uses
`census-run.sh status` as its quiet-box gate; that gate is vacuous, so Task 1
cannot be trusted until this is fixed.

`parse_claim` (`windows/lab/src/census_claim.rs:100-129`) requires **all eight**
fields via `?` — `pid host user started goldens label ref cmdline`.
`census-run.sh`, `heavy-run.sh` and `census_claim.rs:86` each write all eight.
`scripts/lane-run.sh` writes **seven**: it omits `goldens` and `cmdline` and
adds a `job` key nothing parses. So `parse_claim` returns `None`,
`live_holder_at` returns `None`, and `status_line()` returns the literal
`"no heavy run in progress"` for the entire duration of *every* lane job.

Observed live at 2026-08-16T00:16Z, with a lane `heavy` job holding 39 of 40
cores: claim file present and correct, holder pid alive, `loadavg 32.37`, and
`lab claim-status` reporting no run. The `flock` is unaffected — serialisation
is fine. This is a false all-clear in the observability path, on the command
root `CLAUDE.md` names as the way to ask.

**Files:**
- Modify: `scripts/lane-run.sh` (the claim block, ~lines 192-200)
- Create: `cli/tests/lane_claim_roundtrip.rs`

**Interfaces:**
- Produces: a lane claim that `hornvale_lab::census_claim::parse_claim` accepts.
  Task 4's chamber copies this block, so fix it here first.

- [ ] **Step 1: Write the failing test**

```rust
//! A lane-written claim must round-trip through the claim parser (The Sluice).
//!
//! DIRECTION THIS CHECK ENFORCES: every writer of the shared claim file emits
//! the field set the parser requires. It is blind to the opposite direction —
//! a parser that stopped requiring a field would not fail here.
//!
//! WHY IT EXISTS: `scripts/lane-run.sh` shipped writing seven of the eight
//! fields `parse_claim` requires, so `census-run.sh status`, `make
//! heavy-status` and `lab claim-status` all reported "no heavy run in
//! progress" for the whole duration of every lane job — while the job held the
//! box. The lock was never affected; only the answer to "is the box busy?"
//! was, which is the question CLAUDE.md tells every session to ask first.

use std::path::{Path, PathBuf};

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The claim keys a writer emits, scraped from its `echo "<key>=…"` lines.
fn claim_keys_written_by(script: &str) -> Vec<String> {
    let text = std::fs::read_to_string(repo_root().join(script))
        .unwrap_or_else(|e| panic!("{script} must be readable: {e}"));
    let mut keys = Vec::new();
    for line in text.lines() {
        let t = line.trim();
        let Some(rest) = t.strip_prefix("echo \"") else {
            continue;
        };
        if let Some((key, _)) = rest.split_once('=') {
            if !key.is_empty() && key.chars().all(|c| c.is_ascii_lowercase()) {
                keys.push(key.to_string());
            }
        }
    }
    keys
}

/// Exactly the fields `parse_claim` requires. Kept as a literal on purpose: if
/// the parser gains a required field, this list must be updated deliberately,
/// which is the review moment this test exists to force.
const REQUIRED: [&str; 8] = [
    "pid", "host", "user", "started", "goldens", "label", "ref", "cmdline",
];

#[test]
fn the_scraper_can_see_a_known_good_writer() {
    // Guards the vacuous case: a scraper that matched nothing would make the
    // assertion below pass for every script, including a broken one.
    let keys = claim_keys_written_by("scripts/census-run.sh");
    assert!(
        REQUIRED.iter().all(|r| keys.iter().any(|k| k == r)),
        "the scraper failed on census-run.sh, a writer known to be complete — \
         it has gone vacuous. found: {keys:?}"
    );
}

#[test]
fn every_claim_writer_emits_every_required_field() {
    for script in ["scripts/lane-run.sh", "scripts/census-run.sh", "scripts/heavy-run.sh"] {
        let keys = claim_keys_written_by(script);
        let missing: Vec<&str> = REQUIRED
            .iter()
            .copied()
            .filter(|r| !keys.iter().any(|k| k == r))
            .collect();
        assert!(
            missing.is_empty(),
            "{script} omits {missing:?} from the claim file. parse_claim \
             requires all of {REQUIRED:?} and returns None otherwise, so \
             `census-run.sh status` would report no run while this job holds \
             the box."
        );
    }
}
```

- [ ] **Step 2: Run it and confirm it fails on `lane-run.sh` only**

```bash
cargo test -p hornvale --test lane_claim_roundtrip
```

Expected: `the_scraper_can_see_a_known_good_writer` PASSES,
`every_claim_writer_emits_every_required_field` FAILS naming
`scripts/lane-run.sh omits ["goldens", "cmdline"]`. If the first test fails,
the scraper is wrong — fix it before touching `lane-run.sh`, or you will be
tuning a broken instrument.

- [ ] **Step 3: Fix `scripts/lane-run.sh`'s claim block**

The `goldens` value must be truthful: read the set's `authors` column from
`scripts/lane-sets.tsv` (column 4, `yes`/`no`) rather than hardcoding — a claim
that lies about whether it writes goldens is worse than one that cannot be
parsed, because it will be believed.

```bash
authors_col="$(grep -v '^#' "$repo_root/scripts/lane-sets.tsv" \
    | awk -F'\t' -v s="$set_name" '$1==s{print $4}')"
{
    echo "pid=$$"
    echo "host=$(hostname -s)"
    echo "user=${USER:-unknown}"
    echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "goldens=$authors_col"
    echo "label=lane:$set_name"
    echo "ref=$ref"
    echo "cmdline=$command_line"
    echo "job=$job_id"
} > "$claim_path"
```

`job` stays: nothing parses it, but it is the only link from a claim back to
its log, and `parse_claim` ignores unknown keys.

- [ ] **Step 4: Run the test to verify it passes**

```bash
cargo test -p hornvale --test lane_claim_roundtrip
```

Expected: both PASS.

- [ ] **Step 5: Verify against a REAL running lane job, not only the test**

The test scrapes source text; it does not prove the parser accepts what the
script actually writes at runtime. Dispatch a cheap set and ask while it runs:

```bash
bash scripts/lane-dispatch.sh style "$(git rev-parse origin/main)"
sleep 5
bash scripts/census-run.sh status
```

Expected: a line naming `lane:style`, its pid, and its ref — **not**
`no heavy run in progress`. Paste it. This is the assertion that actually
matters; the source scrape is only its cheap standing guard.

- [ ] **Step 6: Mutate to prove both tests can fail**

Remove `goldens` from `census-run.sh`'s claim block → the second test must name
that file. Restore. Then break the scraper's `strip_prefix` pattern → the first
test must fail. Restore. Paste both red outputs.

- [ ] **Step 7: Commit**

```bash
cargo fmt
make shellcheck
git add scripts/lane-run.sh cli/tests/lane_claim_roundtrip.rs
git commit -m "fix(lane): write a claim the claim parser can actually read

parse_claim requires all eight fields via \`?\`; lane-run.sh wrote seven,
omitting goldens and cmdline. So status_line() returned 'no heavy run in
progress' for the entire duration of every lane job, on the exact command
CLAUDE.md tells you to run before consuming the box. Observed live with a
lane heavy job holding 39 of 40 cores at loadavg 32.37.

The flock was never affected — this was a false all-clear in the
observability path only.

goldens is read from lane-sets.tsv's authors column rather than hardcoded: a
claim that lies about whether it writes goldens would be believed.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 1: Establish a green baseline on `main` (resolves spec P2)

No code. The queue cannot bootstrap against a red `main`, and the evidence
about the heavy tier currently conflicts: a board post from The Repose reports
it red on `main` with one of two failures attributable to contention, while
lane job `bfd21abc` (2026-08-15T22:23:56) returned `rc=0` for `heavy`.

**Files:** none. Produces a written verdict consumed by Task 4.

**Interfaces:**
- Produces: a decision recorded in `.superpowers/sdd/decision-ledger.md` —
  does `heavy` **gate** the integration set, or merely **report**?

- [ ] **Step 1: Confirm the box is quiet before measuring**

```bash
bash scripts/census-run.sh status     # must print "no heavy run in progress"
make lane-status                      # nothing running, nothing queued
uptime                                # loadavg near zero on 40 cores
```

A run started on a busy box produces a `cpu_ratio` that cannot distinguish
contention from regression, which is the exact question this task asks.

**`census-run.sh status` is only trustworthy once Task 0 has landed** — before
that fix it reports "no heavy run in progress" for the whole duration of every
lane job. Until you have confirmed Task 0's Step 5 output, corroborate with
`uptime` and `ps -eo pcpu,args --sort=-pcpu | head`: a lane job shows as a
`cargo-nextest`/`the_*` process at several thousand percent CPU.

- [ ] **Step 2: Dispatch each campaign-rung set against current `origin/main`**

```bash
git fetch origin
REF=$(git rev-parse origin/main)
for s in gate artifacts outboard clients seam-guard heavy; do
    bash scripts/lane-dispatch.sh "$s" "$REF"
done
```

- [ ] **Step 3: Read every result back — do not infer from one**

```bash
make lane-status                      # rc and cpu_ratio per job
make lane-log JOB=<id>                # for each non-zero rc
```

- [ ] **Step 4: Classify each failure before deciding anything**

For each non-zero `rc`, decide which it is and write down the evidence:
*real regression on main* / *contention* (`cpu_ratio` far below the ~28–32 a
quiet lefford run shows) / *flake* (re-run once, at the same ref, and compare).

**Do not generalise from a count taken at one moment.** A peer session reported
`seam-guard` had "never returned a verdict" from five of six historical `rc=2`
rows, while two green runs existed after the fix commit — including one it had
dispatched itself and never checked back on.

- [ ] **Step 5: Decide `heavy`'s role and ledger it**

Write to `.superpowers/sdd/decision-ledger.md` as entry `#6 [G5]`:
`heavy` **gates** (a red blocks the merge) if it is reliably green on a quiet
box; `heavy` **reports** (logged, surfaced, non-blocking) if it proves flaky.
Cite the runs. If it is genuinely red on `main`, that is a finding to fix
before any other task proceeds.

- [ ] **Step 6: Commit the ledger note**

```bash
git add -A .superpowers/sdd/ 2>/dev/null || true   # git-ignored; this is a no-op by design
git commit --allow-empty -m "chore(the-sluice): record the main baseline verdict for the heavy tier

<paste the rc/cpu_ratio table and the verdict here — the ledger is
git-ignored, so the commit message is the durable copy>

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

`.superpowers/sdd/` is git-ignored and dies with the worktree, so the commit
message is where this verdict actually survives.

---

## Task 2: The durable queue

**Files:**
- Create: `scripts/sluice-queue.sh`
- Create: `scripts/test-sluice.sh`

**Interfaces:**
- Produces: `sluice-queue.sh add <branch> <sha>` → prints `request_id`;
  `next` → prints one TSV row or nothing; `set-state <id> <state> [note]`;
  `list`. `HV_SLUICE_DIR` overrides the state directory (tests set it).

- [ ] **Step 1: Write the failing test**

Create `scripts/test-sluice.sh`:

```bash
#!/usr/bin/env bash
# scripts/test-sluice.sh — property tests for the merge queue.
#
# Shaped after scripts/test-lane.sh, which pins flock's ORDERING rather than
# merely asserting a lock file exists. Same discipline here: each test pins a
# property the queue would be worthless without, and each must be shown to
# fail when the property is broken.
#
# HERMETICITY: git exports GIT_DIR and GIT_INDEX_FILE to hooks, and they
# OUTRANK `git -C`. A temp directory is not isolation when the environment
# names the repository — tools/board learned this by re-initialising a
# developer's own checkout as bare. Every git invocation below runs under
# `env -u GIT_DIR -u GIT_INDEX_FILE`.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pass=0; fail=0
ok()   { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad()  { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
g()    { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
export HV_SLUICE_DIR="$tmp/state"

# A scratch repo with a main line and two campaign commits on one branch.
scratch="$tmp/repo"; mkdir -p "$scratch"; cd "$scratch"
g init -q -b main .
g config user.email t@t; g config user.name t
printf 'a\n' > f.txt; g add f.txt; g commit -qm root
g checkout -q -b campaign/x
printf 'b\n' >> f.txt; g commit -qam one; OLD="$(g rev-parse HEAD)"
printf 'c\n' >> f.txt; g commit -qam two; NEW="$(g rev-parse HEAD)"

echo "== queue: FIFO and ancestry coalescing"
cd "$scratch"
id1="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/x "$OLD")"
id2="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/x "$NEW")"

# The older request must be superseded, not silently dropped: the registry row
# TOOL-lane-supersession requires it be ledgered.
state_of() { bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' -v i="$1" '$2==i{print $5}'; }
if [ "$(state_of "$id1")" = "superseded" ]; then
    ok "an ancestor request is superseded when its descendant is enqueued"
else
    bad "expected id1 superseded, got '$(state_of "$id1")'"
fi
if [ "$(bash "$repo_root/scripts/sluice-queue.sh" next | cut -f2)" = "$id2" ]; then
    ok "next() returns the surviving request"
else
    bad "next() did not return id2"
fi

echo "== queue: a NON-ancestor request of the same branch is NOT coalesced"
g checkout -q -b campaign/y main
printf 'z\n' >> f.txt; g commit -qam other; OTHER="$(g rev-parse HEAD)"
id3="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/y "$OTHER")"
if [ "$(state_of "$id2")" = "queued" ]; then
    ok "a different branch does not supersede a queued request"
else
    bad "id2 was wrongly superseded by a different branch"
fi

echo "== queue: a RUNNING request is never superseded"
bash "$repo_root/scripts/sluice-queue.sh" set-state "$id2" running
printf 'd\n' >> f.txt
g checkout -q campaign/x; printf 'd\n' >> f.txt; g commit -qam three
NEWER="$(g rev-parse HEAD)"
bash "$repo_root/scripts/sluice-queue.sh" add campaign/x "$NEWER" >/dev/null
if [ "$(state_of "$id2")" = "running" ]; then
    ok "a running request is not superseded by a newer descendant"
else
    bad "a running request was superseded — an authoring job would be orphaned"
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
```

- [ ] **Step 2: Run it to verify it fails for the right reason**

```bash
bash scripts/test-sluice.sh
```

Expected: fails with `scripts/sluice-queue.sh: No such file or directory` —
not with an assertion. A red from a missing file proves nothing about the
assertions; you are only confirming the harness runs.

- [ ] **Step 3: Implement `scripts/sluice-queue.sh`**

```bash
#!/usr/bin/env bash
# scripts/sluice-queue.sh — the merge queue's durable state.
#
# Append-and-rewrite TSV under its OWN flock, deliberately NOT the shared lane
# claim: enqueueing must never block behind a running gate, or a caller trying
# to queue work would wait tens of minutes to write one line.
#
# COALESCING IS BY ANCESTRY, NOT BRANCH NAME. `git merge-base --is-ancestor`
# survives rebases and detached refs, which branch-keying does not. This is
# TOOL-lane-supersession's prescribed fix. Two constraints it must respect:
# it matches per-BRANCH, and it must NEVER supersede a RUNNING request — an
# authoring job already inside the chamber would be orphaned mid-write.
set -euo pipefail

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
mkdir -p "$HV_SLUICE_DIR"
QUEUE="$HV_SLUICE_DIR/queue.tsv"
LOCK="$HV_SLUICE_DIR/queue.lock"
touch "$QUEUE"

with_lock() { exec 8>"$LOCK"; flock 8; }

cmd="${1:?usage: sluice-queue.sh add|next|set-state|list ...}"
shift || true

case "$cmd" in
add)
    branch="${1:?usage: add <branch> <sha>}"
    sha="${2:?usage: add <branch> <sha>}"
    with_lock
    id="req-$(printf '%.12s' "$sha")-$(date -u +%Y%m%dT%H%M%SZ)"
    # Supersede queued ancestors of THIS sha on THIS branch. `running` is
    # excluded by the state test, not by ordering — see the header.
    tmp="$(mktemp)"
    while IFS=$'\t' read -r when rid rbranch rsha rstate rnote; do
        if [ "$rstate" = "queued" ] && [ "$rbranch" = "$branch" ] \
           && git merge-base --is-ancestor "$rsha" "$sha" 2>/dev/null; then
            rstate="superseded"
            rnote="superseded by $id"
        fi
        printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rnote"
    done < "$QUEUE" > "$tmp"
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$id" "$branch" "$sha" "queued" "" >> "$tmp"
    mv "$tmp" "$QUEUE"
    printf '%s\n' "$id"
    ;;
next)
    with_lock
    awk -F'\t' '$5=="queued"{print; exit}' "$QUEUE"
    ;;
set-state)
    id="${1:?usage: set-state <id> <state> [note]}"
    state="${2:?usage: set-state <id> <state> [note]}"
    note="${3:-}"
    with_lock
    tmp="$(mktemp)"
    while IFS=$'\t' read -r when rid rbranch rsha rstate rnote; do
        if [ "$rid" = "$id" ]; then rstate="$state"; [ -n "$note" ] && rnote="$note"; fi
        printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$when" "$rid" "$rbranch" "$rsha" "$rstate" "$rnote"
    done < "$QUEUE" > "$tmp"
    mv "$tmp" "$QUEUE"
    ;;
list)
    cat "$QUEUE"
    ;;
*)  echo "sluice-queue: unknown command '$cmd'" >&2; exit 2 ;;
esac
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
bash scripts/test-sluice.sh
```

Expected: `3 passed, 0 failed` (four `ok` lines across three sections).

- [ ] **Step 5: Mutate each assertion to prove it can fail**

For **each** of the four properties, break the implementation, re-run, confirm
red, restore. Suggested mutations — but find your own if these do not
discriminate, and **report the mutated run's output**:

- Delete the `[ "$rstate" = "queued" ]` guard → the running-request test must go
  red.
- Replace `--is-ancestor "$rsha" "$sha"` with `true` → the different-branch test
  must go red.

A no-op mutation is worse than no mutation, because it produces evidence.
Assert the target text exists before substituting it.

- [ ] **Step 6: Shellcheck and commit**

```bash
make shellcheck
git add scripts/sluice-queue.sh scripts/test-sluice.sh
git commit -m "feat(sluice): the durable queue, coalescing by ancestry

Supersession is by \`git merge-base --is-ancestor\`, per-branch, and never
against a running request — an authoring job inside the chamber would be
orphaned mid-write. Superseded requests are ledgered, not dropped.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 3: The mouth — checks that never take the claim

**Files:**
- Create: `scripts/sluice-mouth.sh`
- Modify: `scripts/test-sluice.sh` (append a section)

**Interfaces:**
- Consumes: nothing from Task 2 (deliberately independent — the mouth is pure).
- Produces: `sluice-mouth.sh <branch> <sha>` → exit `0` admit, `1` conflict,
  `2` invalid/unreachable, `3` already merged, `4` out-of-band landing. Prints
  a human-readable reason on stderr and, on conflict, the conflicting paths.

- [ ] **Step 1: Write the failing test — append to `scripts/test-sluice.sh`**

```bash
echo "== mouth: a conflicting candidate is refused WITHOUT taking the claim"
cd "$scratch"
g checkout -q main; printf 'MAIN\n' > f.txt; g commit -qam main-edit
g checkout -q -b campaign/conflict main~1
printf 'SIDE\n' > f.txt; g commit -qam side-edit
CONF="$(g rev-parse HEAD)"

# Point the claim at a path in our temp dir so we can assert it is untouched.
export HV_CENSUS_CLAIM_PATH="$tmp/claim"
export HV_CENSUS_LOCK="$tmp/lock"
export HV_SLUICE_ALLOW_UNPUSHED=1     # scratch repo has no remote
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/conflict "$CONF" >/dev/null 2>"$tmp/mouth.err"
rc=$?
set -e
if [ "$rc" -eq 1 ]; then
    ok "a conflicting candidate exits 1"
else
    bad "expected exit 1 for a conflict, got $rc"
fi
# THE PROPERTY THAT MATTERS: assert on the CLAIM, not on the message. A check
# that merely printed the right words while consuming the box would pass a
# message-based assertion.
if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
    ok "the conflicting candidate never acquired the claim"
else
    bad "the mouth took the claim — the whole point is that it must not"
fi
if grep -q 'f.txt' "$tmp/mouth.err"; then
    ok "the conflicting path is reported"
else
    bad "no conflicting path in stderr"
fi

echo "== mouth: an already-merged candidate exits 3"
set +e
bash "$repo_root/scripts/sluice-mouth.sh" main "$(g rev-parse main)" >/dev/null 2>&1
rc=$?
set -e
if [ "$rc" -eq 3 ]; then ok "already-merged exits 3"; else bad "expected 3, got $rc"; fi
```

- [ ] **Step 2: Run to verify it fails**

```bash
bash scripts/test-sluice.sh
```

Expected: FAIL, `sluice-mouth.sh: No such file or directory`.

- [ ] **Step 3: Implement `scripts/sluice-mouth.sh`**

The exit codes for `git merge-tree --write-tree` were **verified on lefford**
(git 2.39.5), not assumed: a clean merge exits `0` with the tree SHA on stdout
line 1; a conflicting merge exits **`1`**, and `--name-only` prints the tree
SHA then the conflicting paths.

```bash
#!/usr/bin/env bash
# scripts/sluice-mouth.sh — the checks that run OUTSIDE the lane claim.
#
# THE CANAL-LOCK RULE: turn a vessel away at the gate, never inside the
# chamber. A candidate that cannot merge must not consume a strictly serial
# resource whose mean queue wait, measured over the lane's first 46 jobs, is
# 903-1823 s per job. Everything here is read-only and takes no lock.
#
# EXIT CODES: 0 admit / 1 conflict / 2 invalid or unpushed / 3 already merged
# / 4 an out-of-band landing on main.
set -euo pipefail
branch="${1:?usage: sluice-mouth.sh <branch> <sha>}"
sha="${2:?usage: sluice-mouth.sh <branch> <sha>}"

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
last_pushed_file="$HV_SLUICE_DIR/last-pushed"

git rev-parse --verify --quiet "$sha^{commit}" >/dev/null \
    || { echo "sluice-mouth: '$sha' is not a commit in this repository" >&2; exit 2; }

# A full 40-char SHA, for the reason lane-dispatch.sh gives: a ref feeds
# checkout/reset on the far end and can land on a stale local branch of that
# name.
case "$sha" in
    [0-9a-f]*) [ "${#sha}" -eq 40 ] || { echo "sluice-mouth: REF must be a full 40-char SHA; got '$sha'" >&2; exit 2; } ;;
    *) echo "sluice-mouth: REF must be a full 40-char SHA; got '$sha'" >&2; exit 2 ;;
esac

if [ -z "${HV_SLUICE_ALLOW_UNPUSHED:-}" ]; then
    if [ -z "$(git branch -r --contains "$sha" 2>/dev/null)" ]; then
        echo "sluice-mouth: $sha is not on any remote branch — push first." >&2
        exit 2
    fi
fi

base="${HV_SLUICE_BASE:-origin/main}"

# An out-of-band landing breaks the queue's inductive guarantee: each merge
# builds on an already-proven main, so anything that lands another way makes
# every later green weaker than it advertises. Detect it loudly; never resume
# quietly.
if [ -f "$last_pushed_file" ]; then
    expected="$(cat "$last_pushed_file")"
    actual="$(git rev-parse "$base")"
    if [ "$expected" != "$actual" ]; then
        echo "sluice-mouth: OUT-OF-BAND LANDING on $base." >&2
        echo "  the queue last pushed: $expected" >&2
        echo "  $base is now:          $actual" >&2
        echo "  Something landed outside the queue. The inductive guarantee is broken" >&2
        echo "  until a human decides what happened." >&2
        exit 4
    fi
fi

if git merge-base --is-ancestor "$sha" "$base" 2>/dev/null; then
    echo "sluice-mouth: $sha is already an ancestor of $base — nothing to merge." >&2
    exit 3
fi

if ! out="$(git merge-tree --write-tree --name-only "$base" "$sha" 2>&1)"; then
    echo "sluice-mouth: MERGE CONFLICT between $base and $sha." >&2
    # Line 1 is the tree SHA; the conflicting paths follow, then a blank line
    # and git's own informational messages.
    printf '%s\n' "$out" | tail -n +2 | sed '/^$/q' | sed 's/^/  conflict: /' >&2
    exit 1
fi

behind="$(git rev-list --count "$(git merge-base "$base" "$sha")".."$base")"
echo "sluice-mouth: ADMIT $branch $sha (merge base is $behind commits behind $base)"
exit 0
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
bash scripts/test-sluice.sh
```

Expected: all sections `ok`, `0 failed`.

- [ ] **Step 5: Mutate to prove the claim assertion can fail**

Add `: > "$HV_CENSUS_CLAIM_PATH"` near the top of `sluice-mouth.sh`, re-run,
and confirm the "never acquired the claim" assertion goes red. Restore. Paste
the mutated output into your report — this is the assertion most likely to be
silently vacuous, because it passes trivially if the variable is unset.

- [ ] **Step 6: Shellcheck and commit**

```bash
make shellcheck
git add scripts/sluice-mouth.sh scripts/test-sluice.sh
git commit -m "feat(sluice): the mouth — refuse a bad candidate before taking the box

git merge-tree --write-tree exit codes verified on lefford (git 2.39.5), not
assumed: clean exits 0, conflict exits 1 with --name-only listing the paths.
The test asserts on the CLAIM FILE rather than on the error message, because
a check that printed the right words while consuming the box would pass a
message-based assertion.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 4: The chamber — merge and phases

**Files:**
- Create: `scripts/sluice-run.sh`

**Interfaces:**
- Consumes: `sluice-mouth.sh` exit codes (Task 3), `sluice-queue.sh set-state`
  (Task 2), and Task 1's verdict on whether `heavy` gates or reports.
- Produces: a job log at `$HV_SLUICE_DIR/<request-id>.log` and a row in
  `$HV_SLUICE_DIR/jobs.tsv`. Task 5 adds the push.

- [ ] **Step 1: Write `scripts/sluice-run.sh` up to the phases**

```bash
#!/usr/bin/env bash
# scripts/sluice-run.sh — the chamber. Runs ON the canonical box.
#
# Takes the SAME shared claim the census and the heavy tier take (decisions
# 0081/0086/0133) — one job, not six, so the queue wait is paid once. Over the
# lane's first 27.4 h, 14.2 h of 21.2 h wall time was queue wait (67%),
# because a campaign gate was six separate dispatches.
#
# DETACHED HEAD IS NOT A WORKAROUND. scripts/hooks/pre-commit refuses a commit
# to 'main' from a linked worktree, and this chamber is a linked worktree that
# commits regenerated artifacts. The guard keys on
# `git_dir != git_common_dir && branch == "main"` where
# `branch="$(git symbolic-ref --short -q HEAD || echo DETACHED)"`, so detached
# is exempt by construction. It is also the honest description: the chamber
# does not own main, it builds a candidate and offers it. main moves only when
# the push in sluice-run's step 6 succeeds.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
branch="${1:?usage: sluice-run.sh <branch> <full-sha>}"
sha="${2:?usage: sluice-run.sh <branch> <full-sha>}"

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
mkdir -p "$HV_SLUICE_DIR"
job_id="sluice-$(printf '%.12s' "$sha")-$(date -u +%Y%m%dT%H%M%SZ)"
run_log="$HV_SLUICE_DIR/$job_id.log"
jobs_tsv="$HV_SLUICE_DIR/jobs.tsv"
began=$SECONDS
waited_s=""
phase_failed=""

record() {
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$job_id" "$branch" "$sha" \
        "$1" "$((SECONDS - began))" "$waited_s" "$phase_failed" >> "$jobs_tsv"
}
trap 'record $?' EXIT

# shellcheck source=scripts/census-canonical-host.sh
. "$repo_root/scripts/census-canonical-host.sh"
require_canonical_host integration || exit 1

exec >>"$run_log" 2>&1
echo "sluice-run: $job_id started $(date -Is) on $(hostname -s) as pid $$"

LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
claim_path="${HV_CENSUS_CLAIM_PATH:-/tmp/hv-census.claim}"
exec 9>"$LOCK"
timeout_s="${HV_SLUICE_WAIT_TIMEOUT:-7200}"
echo "sluice-run: queued for the staff ($LOCK; up to ${timeout_s}s) …"
wait_began=$SECONDS
if ! flock -w "$timeout_s" 9; then
    echo "sluice-run: TIMED OUT after ${timeout_s}s waiting for the staff." >&2
    exit 75
fi
waited_s=$((SECONDS - wait_began))
echo "sluice-run: holds the staff at $(date -Is) after ${waited_s}s queued"

{
    echo "pid=$$"; echo "host=$(hostname -s)"; echo "user=${USER:-unknown}"
    echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"; echo "label=sluice:$branch"
    echo "ref=$sha"; echo "job=$job_id"
} > "$claim_path"
trap 'code=$?; rm -f "$claim_path"; echo "sluice-run: finished $(date -Is) rc=$code"; record "$code"' EXIT

# The chamber's own worktree — NOT the lane's shared scratch tree, which every
# lane dispatch `checkout --force`s and `reset --hard`s. A dedicated tree
# always builds main-plus-a-delta, so its warm target/ stays hot and the
# measured 771 s cold build is paid once, ever.
wt="${HV_SLUICE_WORKTREE:-$repo_root/../hornvale-sluice-wt}"
git -C "$repo_root" fetch --all --quiet
base_sha="$(git -C "$repo_root" rev-parse origin/main)"
if [ -e "$wt/.git" ]; then
    git -C "$wt" fetch --all --quiet
    git -C "$wt" checkout --force --detach "$base_sha"
    git -C "$wt" reset --hard "$base_sha" --quiet
    git -C "$wt" clean -fd --quiet
else
    git -C "$repo_root" worktree add --force --detach "$wt" "$base_sha"
fi
cd "$wt"
echo "sluice-run: detached at $(git rev-parse --short HEAD) (origin/main)"

export HV_CENSUS_LOCK_HELD=$$

# THE MERGE COMMIT MESSAGE IS AN ARTIFACT, not decoration. tools/census/
# history.sh tags every committed census snapshot with an epoch label taken
# from `git log --follow --first-parent main -- <path>` — and under --no-ff the
# campaign's own commit is no longer on that line, so THIS subject becomes the
# label. It must read correctly as a census epoch label standing alone.
campaign="${branch#campaign/}"
headline="${HV_SLUICE_HEADLINE:-$(git log -1 --format=%s "$sha")}"
if ! git merge --no-ff --no-edit -m "merge($campaign): $headline

Gated as the merge product by sluice job $job_id.
main was $base_sha at test time." "$sha"; then
    echo "sluice-run: MERGE CONFLICT — holding. A human resolves this." >&2
    phase_failed="merge"
    git merge --abort || true
    exit 10
fi
merge_sha="$(git rev-parse HEAD)"
echo "sluice-run: merge product is $merge_sha"
```

- [ ] **Step 2: Add the phase loop**

Append to `scripts/sluice-run.sh`:

```bash
# PHASE ORDER IS BY EXPECTED TIME-TO-RED, not by tree hygiene. `git clean -fd`
# between phases makes cleanliness free, which frees the order to optimise for
# detecting the class distinctive to a MERGE PRODUCT — interaction with main,
# which only `gate` and `artifacts` see. `heavy` is last because at a measured
# mean 1678 s it is 47% of the set's ~3602 s.
#
# NEVER `-fdx`: target/ is gitignored and -x destroys the ~15 GB warm cache.
#
# Cleaning between phases also closes by construction the bug that made
# seam-guard return rc=2 "refusing to run on a dirty working tree" in five of
# its first six lane runs: earlier sets in the same dispatch dirtied the tree,
# and rc=2 reads as "found survivors", so the breakage looked like a finding
# for a month.
phases="artifacts outboard gate seam-guard clients heavy"
cmd_for() {
    grep -v '^#' "$repo_root/scripts/lane-sets.tsv" | awk -F'\t' -v s="$1" '$1==s{print $5}'
}
# Sets that write committed artifacts; their drift is committed after they run.
authors() { case "$1" in artifacts|outboard|clients|heavy) return 0 ;; *) return 1 ;; esac; }

for phase in $phases; do
    line="$(cmd_for "$phase")"
    [ -n "$line" ] || { echo "sluice-run: no such set '$phase'" >&2; exit 2; }
    echo "sluice-run: === phase $phase ==="
    if ! bash "$repo_root/scripts/timed.sh" "sluice:$phase" -- sh -c "$line"; then
        # Task 1's verdict decides whether `heavy` gates or reports. If it
        # REPORTS, replace this block for that one phase with a warning that
        # does not exit.
        echo "sluice-run: PHASE $phase FAILED — holding." >&2
        phase_failed="$phase"
        exit 11
    fi
    if authors "$phase" && [ -n "$(git status --porcelain)" ]; then
        git add -A
        git commit -q -m "chore(artifacts): regenerate after $phase

Authored on the canonical host inside sluice job $job_id (decision 0079)."
        echo "sluice-run: committed artifact drift from $phase"
    fi
    git clean -fd --quiet
done
```

- [ ] **Step 3: Shellcheck**

```bash
make shellcheck
```

Expected: clean. Fix any SC2015 (`A && B || C`) with explicit `if`/`then`.

- [ ] **Step 4: Dry-run the chamber against a known-green ref**

Use the SHA Task 1 established as green, and a throwaway branch that merges
cleanly into it, with `HV_SLUICE_WORKTREE` pointed at a scratch path so the
real chamber worktree is not created yet:

```bash
HV_SLUICE_WORKTREE=/tmp/sluice-dry \
  bash scripts/sluice-run.sh campaign/the-sluice "$(git rev-parse origin/campaign/the-sluice)"
tail -40 ~/.local/state/hornvale/sluice/*.log
```

Expected: the merge succeeds, phases run in the documented order, and it exits
before any push (the push arrives in Task 5). Record the wall time per phase —
it is the first real measurement of the integration set.

- [ ] **Step 5: Commit**

```bash
git add scripts/sluice-run.sh
git commit -m "feat(sluice): the chamber — one claim, six phases, detached

Detached HEAD because scripts/hooks/pre-commit refuses a commit to 'main'
from a linked worktree and this chamber commits regenerated artifacts; the
guard's own fallback is \`git symbolic-ref --short -q HEAD || echo DETACHED\`,
so detached is exempt by construction rather than by evasion.

Phase order is by expected time-to-red, with \`git clean -fd\` between phases
making tree hygiene free. That also closes the seam-guard dirty-tree bug by
construction.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 5: The chamber — drift assertion and the push

**Files:**
- Modify: `scripts/sluice-run.sh` (append)
- Modify: `scripts/test-sluice.sh` (append a section)

**Interfaces:**
- Consumes: `merge_sha` from Task 4.
- Produces: `$HV_SLUICE_DIR/last-pushed` containing the SHA just pushed —
  which `sluice-mouth.sh` (Task 3) already reads for out-of-band detection.

- [ ] **Step 1: Write the failing test — append to `scripts/test-sluice.sh`**

```bash
echo "== push: the recorded last-pushed SHA is what the mouth compares against"
cd "$scratch"
mkdir -p "$HV_SLUICE_DIR"
g checkout -q main
echo "$(g rev-parse main)" > "$HV_SLUICE_DIR/last-pushed"
# Move main out from under the recorded value — simulating an out-of-band land.
printf 'oob\n' >> f.txt; g commit -qam out-of-band
export HV_SLUICE_BASE=main
set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/x "$NEW" >/dev/null 2>"$tmp/oob.err"
rc=$?
set -e
if [ "$rc" -eq 4 ]; then
    ok "an out-of-band landing on the base is detected (exit 4)"
else
    bad "expected exit 4 for an out-of-band landing, got $rc"
fi
if grep -q 'OUT-OF-BAND' "$tmp/oob.err"; then
    ok "the out-of-band message names the condition"
else
    bad "no OUT-OF-BAND message"
fi
rm -f "$HV_SLUICE_DIR/last-pushed"; unset HV_SLUICE_BASE
```

- [ ] **Step 2: Run to confirm it fails**

```bash
bash scripts/test-sluice.sh
```

Expected: FAIL — the mouth exits `0`, because nothing writes `last-pushed`
yet, so the file does not exist and the check is skipped.

This is worth pausing on: the assertion is only meaningful because the file is
*present*. Confirm the failure message says `expected exit 4 … got 0`, not a
missing-file error.

- [ ] **Step 3: Implement the tail of `scripts/sluice-run.sh`**

```bash
# The drift check, reading its path list from the one file that declares it.
# `git diff --exit-code` against an UNTRACKED path is silently vacuous, so the
# working tree must also be clean — the two assertions catch different things
# and neither implies the other.
if [ -n "$(git status --porcelain)" ]; then
    echo "sluice-run: working tree is dirty after all phases — refusing to push." >&2
    git status --porcelain >&2
    phase_failed="dirty-tree"
    exit 12
fi
# REMOVED IN IMPLEMENTATION (Task 5 review). A `git diff --exit-code`
# drift check here is PROVABLY UNREACHABLE: `git status --porcelain` empty
# strictly implies `git diff --exit-code -- <any pathspec>` empty, because
# diff sees a subset (unstaged tracked modifications), and this ran only when
# the status check had already passed. The plan asserted the two were
# independent; that was wrong, and backwards.
#
# The consequence is worth stating rather than hiding: **the chamber cannot
# detect a phase that silently failed to regenerate a declared artifact.**
# Catching that needs a re-regeneration compared against the committed tree,
# which is expensive when `artifacts` is already a phase. The surviving
# working-tree check detects exactly one real thing — an untracked nested git
# repo `git clean -fd` cannot remove — and is documented in the chamber as
# such. See the campaign's followup register.

# THE PUSH MUST NOT TRUST A BARE rc=0. Task 4's review found that `code=$?`
# inside an EXIT trap is **0 when the shell dies from a signal** — so a
# chamber killed mid-phase records rc=0 with `phase_failed` empty, which is
# byte-indistinguishable from a full green run. `lane-run.sh:70-72` and
# `heavy-run.sh:83-85` carry `why=SIGTERM`/`INT`/`HUP` traps and a `why`
# column for exactly this reason.
#
# So the push is gated on TWO facts, not one: every phase completed AND the
# run ended by ordinary exit rather than a signal. A queue that pushes `main`
# because a killed job looked green is the worst failure this campaign can
# produce — it would land an untested tree while claiming the opposite, which
# is the precise thing the whole design exists to prevent.
if [ "${why:-exit}" != "exit" ]; then
    echo "sluice-run: run ended via $why, not a normal exit — refusing to push." >&2
    exit 15
fi
# RESTRUCTURED IN IMPLEMENTATION (Task 5 review). As written below, this
# guard was UNREACHABLE: every `phase_failed=` assignment was immediately
# followed by its own `exit`, so nothing set it and reached here. The comment
# above claiming the push is "gated on TWO facts" described a live gate that
# did not exist — this campaign's own signature fault, authored into its own
# plan.
#
# The shipped shape replaces the in-loop `exit`s with `break` to a single
# post-loop gate, so `phase_failed` is the real decision point and the
# existing killed-run and failed-phase tests genuinely cover it. Verified by
# neutralising the gate and watching a failed phase push.
if [ -n "$phase_failed" ]; then
    echo "sluice-run: phase '$phase_failed' failed — refusing to push." >&2
    exit 16
fi

final_sha="$(git rev-parse HEAD)"

# TESTED SHA == PUSHED SHA. Nothing may be created after the last green, so
# this is asserted rather than assumed: if the phases committed artifact drift,
# `final_sha` moved past `merge_sha`, and the LAST phase ran before that
# commit. Re-running the drift check above is what makes the final tree
# equivalent; this assertion catches the case where it is not.
echo "sluice-run: merge product $merge_sha, final tree $final_sha"

# Fast-forward only, ALWAYS. HEAD's first parent is origin/main, so this IS a
# fast-forward; --force-with-lease and --force must never appear here.
if ! git push origin "$final_sha:refs/heads/main"; then
    echo "sluice-run: PUSH REJECTED — main moved under us. Holding." >&2
    phase_failed="push"
    exit 14
fi
printf '%s\n' "$final_sha" > "$HV_SLUICE_DIR/last-pushed"
git push origin "HEAD:refs/heads/$branch" || \
    echo "sluice-run: warning — could not update $branch; main is already landed." >&2
echo "sluice-run: LANDED $final_sha on main"
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
bash scripts/test-sluice.sh
```

Expected: every section `ok`, `0 failed`.

- [ ] **Step 4b: Prove the push refuses a killed run**

This is the assertion that matters most in the whole campaign, and it cannot
be written from the outside — you have to construct a chamber run that dies
from a signal and show the push does not happen.

```bash
# In the scratch repo: start a chamber whose phase sleeps, SIGTERM it,
# then assert BOTH that `main` did not move AND that the recorded row
# carries a non-zero rc and a `why` naming the signal.
```

Then mutate: remove the `why` guard, re-run, and confirm the killed run
pushes. **Paste that red output.** A guard against pushing an untested tree
that has never been observed refusing is not a guard.

- [ ] **Step 5: Prove the push cannot be a force-push**

This property has no runtime test that is safe to run against `origin`, so
pin it statically. Append to `scripts/test-sluice.sh`:

```bash
echo "== push: no force flag exists anywhere in the chamber"
if grep -nE '(--force-with-lease|--force([^-]|$)|push .*\+)' "$repo_root/scripts/sluice-run.sh" \
     | grep -v 'checkout --force' | grep -v 'worktree add --force' | grep -q .; then
    bad "a force flag reaches the push path"
else
    ok "no force flag on any push in sluice-run.sh"
fi
```

Then mutate: add `git push --force origin HEAD:refs/heads/main` to a comment-free
line in `sluice-run.sh`, re-run, confirm red, remove it. **Paste the red
output** — a grep-based guard that never fires is the classic vacuous check,
and this one has two exclusions that could swallow the real case.

- [ ] **Step 6: Shellcheck and commit**

```bash
make shellcheck
git add scripts/sluice-run.sh scripts/test-sluice.sh
git commit -m "feat(sluice): assert a clean tree, then push fast-forward only

Two assertions before the push, catching different things: a dirty working
tree, and a drifted declared generated path. \`git diff --exit-code\` against
an untracked path is silently vacuous, so neither implies the other.

Records last-pushed, which the mouth reads to detect an out-of-band landing —
the fault that would otherwise break the queue's inductive guarantee silently.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 5a: A `pre-push` hook — the control that prose was not

**Added after an incident, 2026-08-16.** A subagent force-pushed campaign WIP
over `origin/main` while probing bash quote-splitting semantics. Its dispatch
named that prohibition as the single most important constraint, in a boxed
warning, with a "print `git remote -v` first" sub-rule. It still happened,
because the agent wrote a *diagnostic* and did not file that under "the push
tests" the rule described. **Prose in a prompt is not a control.**

**Files:**
- Create: `scripts/hooks/pre-push`
- Create: `scripts/test-pre-push.sh`
- Modify: `scripts/CLAUDE.md` (the hooks section)

**Why a hook is the right mechanism, verified:** `core.hooksPath` is already
`scripts/hooks` (repository-level, so it is shared by every linked worktree —
confirmed from both the primary checkout and a campaign worktree). The chamber
disables hooks only for its own `commit` (`-c core.hooksPath=/dev/null`,
`sluice-run.sh:510`), never for a push. So a `pre-push` hook fires for every
push from every session without anyone opting in.

**The rule — gate the destructive class, not all pushes.**

Refuse, unless `HV_PUSH_OK=1` is set in the environment:

1. a **delete** (local sha all zeros), or
2. a **non-fast-forward** (remote sha is non-zero and is not an ancestor of the
   local sha),

and only when the remote is **not** a local path (`file://` or a filesystem
path). Pushes to scratch bare repos stay unrestricted, so tests need no opt-in.

**What is deliberately NOT gated, and why.** An ordinary fast-forward push,
including to `main`. Nathan's normal workflow is to commit and push directly to
`main` with no PR, and gating that would add friction to the common case to
prevent a failure that has never occurred. The incident was a *force* push.
Deletes and non-fast-forwards are the destructive, hard-to-recover class;
fast-forwards are additive.

> **Followup, not this task:** once the merge queue is live, decision 0139
> ("`main` advances only through the lock") could be enforced here by gating
> direct pushes to `refs/heads/main` behind the same opt-in — turning a stated
> invariant into a mechanical one. Premature until the queue actually lands.

- [ ] **Step 1: Write the failing test**

`scripts/test-pre-push.sh`, in the shape of `scripts/test-sluice.sh`. Every git
call under `env -u GIT_DIR -u GIT_INDEX_FILE`. Drive the hook the way git does
— pass `$1`/`$2` and feed the ref lines on **stdin** — rather than by pushing,
so the tests need no network and no real remote at all.

Cases, each of which must be shown to fail when the hook is neutered:

- a force push to a non-local remote is **refused**
- a delete to a non-local remote is **refused**
- the same force push with `HV_PUSH_OK=1` is **allowed**
- a force push to a `file://` remote is **allowed** (tests must stay frictionless)
- an ordinary fast-forward to a non-local remote is **allowed**
- a brand-new branch (remote sha all zeros) is **allowed** — this is the case
  most likely to be misclassified as a force
- **the exact incident**: `21847b08` → `fb71dd2e` on `refs/heads/main`, where
  the old sha is not an ancestor of the new. Assert it is refused. That case is
  the reason this file exists; name it in the test.

- [ ] **Step 2: Run it and confirm it fails for the right reason**

Expected: `scripts/hooks/pre-push: No such file or directory` — a missing-file
red, not an assertion red. You are only confirming the harness runs.

- [ ] **Step 3: Implement `scripts/hooks/pre-push`**

Constraints: `set -euo pipefail`; explicit `if`/`then` over `A && B || C`; no
bash-4+ builtins; `shellcheck` clean; and a header in the register of this
repo's other hooks — what it prevents, and the incident that motivated it.

The refusal message must tell the reader exactly how to proceed deliberately
(set `HV_PUSH_OK=1`), because a guard people cannot satisfy is a guard they
work around.

Note the ancestry test needs the objects present locally; decide what to do
when they are not (a shallow clone, or a remote sha this side has never
fetched) and say which way you erred. **Failing closed is the safer default
here** — but say so rather than letting it happen by accident.

- [ ] **Step 4: Run the tests to verify they pass**

- [ ] **Step 5: Mutate every case**

For each, break the hook so that case should pass wrongly, confirm red, restore.
Paste the reds. In particular prove the `file://` and new-branch allowances are
real allowances and not the hook silently failing open.

- [ ] **Step 6: Verify against a real push, on a scratch remote only**

Create a bare repo, push to it, force-push to it — the hook must allow both
(local remote). Then, **without pushing**, demonstrate the refusal path by
invoking the hook directly with a non-local remote URL. Do not test the refusal
by attempting a real push to GitHub.

- [ ] **Step 7: Commit**

```bash
make shellcheck
git add scripts/hooks/pre-push scripts/test-pre-push.sh scripts/CLAUDE.md
git commit -m "feat(hooks): refuse a force-push or delete to a real remote

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 6: The request path and the Makefile surface

**Files:**
- Create: `scripts/sluice-request.sh`
- Modify: `scripts/lane-sets.tsv` (add the `integration` row)
- Modify: `Makefile`

**Interfaces:**
- Consumes: `sluice-queue.sh add`, `sluice-mouth.sh`, `sluice-run.sh`.
- Produces: `make sluice BRANCH=<branch> REF=<full-sha>`, `make sluice-status`,
  `make sluice-log [JOB=<id>]`.

- [ ] **Step 1: Add the `integration` row to `scripts/lane-sets.tsv`**

Append after the `seam-guard` row (keep the existing tab-separated shape):

```
integration	merge	lane	yes	bash scripts/sluice-run.sh
```

`cli/tests/lane_sets.rs` fails on a second copy of the roster in prose — so do
**not** restate this row in any `CLAUDE.md`. Point at the file instead.

- [ ] **Step 2: Run the roster test to confirm it still passes**

```bash
cargo test -p hornvale --test lane_sets
```

Expected: PASS. If it fails on the new `merge` value in the gate column, the
test's allowed-rung list needs the new rung — read its assertion before
changing it, and extend the vocabulary rather than loosening the check.

- [ ] **Step 3: Write `scripts/sluice-request.sh`**

```bash
#!/usr/bin/env bash
# scripts/sluice-request.sh — the caller's side. Runs on ANY machine.
# Validates, ssh's, prints a request id, RETURNS. It never waits.
#
# Same shape as scripts/lane-dispatch.sh, including its two hard-won guards:
# a REF must be a full 40-char SHA (it feeds checkout on the far end), and the
# remote preflight stays OUTSIDE the backgrounded segment so a dispatch that
# did not start cannot report success.
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
branch="${1:?usage: sluice-request.sh <branch> <full-sha>}"
ref="${2:?usage: sluice-request.sh <branch> <full-sha>}"
host="$(cat "$repo_root/scripts/census-canonical-host.txt")"
remote_dir="${HV_SLUICE_REMOTE_DIR:-~/Projects/hornvale}"

case "$ref" in
    [0-9a-f]*) [ "${#ref}" -eq 40 ] || { echo "sluice-request: REF must be a full 40-char SHA; got '$ref'" >&2; exit 2; } ;;
    *) echo "sluice-request: REF must be a full 40-char SHA (never a branch name); got '$ref'" >&2; exit 2 ;;
esac

if [ -z "$(git -C "$repo_root" branch -r --contains "$ref" 2>/dev/null)" ]; then
    echo "sluice-request: $ref is not on any remote branch — push first." >&2
    exit 2
fi

# shellcheck disable=SC2029  # meant to expand client-side into the remote command
remote_cmd="if cd $remote_dir && [ -x scripts/sluice-queue.sh ]; then \
scripts/sluice-queue.sh add '$branch' '$ref'; else \
echo \"sluice-request-remote: cd '$remote_dir' failed or sluice-queue.sh missing on '$host' -- NOTHING WAS QUEUED\" >&2; \
exit 1; fi"

# shellcheck disable=SC2029
if ! req="$(ssh "$host" "$remote_cmd")"; then
    echo "sluice-request: remote enqueue FAILED — nothing was queued." >&2
    exit 1
fi
echo "sluice-request: $req branch=$branch ref=${ref:0:12} host=$host"
echo "sluice-request: read it back with 'make sluice-status'"
```

- [ ] **Step 4: Add the Makefile targets**

```makefile
sluice: ## Request a merge through the queue (BRANCH=<branch> REF=<full-sha>)
	@bash scripts/sluice-request.sh "$(BRANCH)" "$(REF)"

sluice-status: ## The merge queue: what is queued, running, held, landed
	@bash scripts/sluice-queue.sh list | column -t -s "$$(printf '\t')" || true

sluice-log: ## Read a finished merge-queue job back (JOB=<id>, or omit for the most recent)
	@d="$${HV_SLUICE_DIR:-$$HOME/.local/state/hornvale/sluice}"; \
	if [ -n "$(JOB)" ]; then f="$$d/$(JOB).log"; \
	else f="$$(ls -1t "$$d"/*.log 2>/dev/null | head -1)"; fi; \
	if [ -z "$$f" ] || [ ! -f "$$f" ]; then echo "sluice-log: no such job" >&2; exit 1; fi; \
	echo "== $$f"; cat "$$f"
```

- [ ] **Step 5: Verify the targets are discoverable and the help text renders**

```bash
make help | grep -i sluice
```

Expected: three lines, one per target, each with its `##` description.

- [ ] **Step 6: Shellcheck and commit**

```bash
make shellcheck
git add scripts/sluice-request.sh scripts/lane-sets.tsv Makefile
git commit -m "feat(sluice): the request path and the make surface

lane-sets.tsv gains the 'integration' row on a new 'merge' rung. The roster
stays the single source of truth — cli/tests/lane_sets.rs fails on a second
copy in prose, so no CLAUDE.md restates it.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 7: Retire `gate-campaign` to a refusing signpost

**Files:**
- Modify: `Makefile` (the `gate-campaign` target)
- Modify: `CLAUDE.md` (the gate-ladder block)
- Modify: `scripts/CLAUDE.md` (the gate-ladder section)

**Interfaces:**
- Consumes: `make sluice` from Task 6.

- [ ] **Step 1: Replace the `gate-campaign` body**

Decision 0132 established this exact pattern for `gate`, `ci`, `gate-fast` and
`gate-full`: refuse rather than alias, because silently repointing a target
changes what hundreds of existing calls meant. Match the existing signposts'
wording — read one first.

```makefile
gate-campaign: ## RETIRED (decision 0139) — the merge queue gates the merge product
	@echo "make gate-campaign no longer runs anything."; \
	echo; \
	echo "It gated a BRANCH TIP. What lands is that branch merged into whatever"; \
	echo "main is at merge time, and nothing ever built that object — which is"; \
	echo "how two campaigns both minted decision 0134 through a green gate."; \
	echo; \
	echo "Use the merge queue, which gates the merge product and pushes the"; \
	echo "exact SHA it tested:"; \
	echo "    make sluice BRANCH=<branch> REF=<full-sha>"; \
	echo "    make sluice-status"; \
	echo; \
	echo "make gate-stage REF=<full-sha> is unchanged."; \
	exit 1
```

- [ ] **Step 2: Verify it refuses**

```bash
make gate-campaign; echo "exit=$?"
```

Expected: the message, then `exit=2` (make's own code for a failed recipe).
Confirm it is **non-zero** — a signpost that exits 0 is worse than no signpost,
because a script calling it in a chain proceeds as if the gate ran.

- [ ] **Step 3: Update `CLAUDE.md`'s gate-ladder block**

Change the four-gate list to name the queue, and add `gate-campaign` to the
existing refusing-signpost paragraph beside `gate`/`ci`/`gate-fast`/`gate-full`.
Do **not** restate `lane-sets.tsv`'s roster — `cli/tests/lane_sets.rs` fails on
a second copy.

- [ ] **Step 4: Update `scripts/CLAUDE.md`'s gate-ladder section**

Add `sluice-queue.sh`, `sluice-mouth.sh`, `sluice-run.sh`, `sluice-request.sh`
and `test-sluice.sh` to the script list, each with the one-line "what failure
it prevents" the other entries carry.

- [ ] **Step 5: Run the doc drift check**

```bash
cargo test -p hornvale --test docs_consistency
cargo test -p hornvale --test lane_sets
```

Expected: both PASS.

- [ ] **Step 6: Commit**

```bash
git add Makefile CLAUDE.md scripts/CLAUDE.md
git commit -m "feat(sluice): gate-campaign becomes a refusing signpost

Decision 0132's pattern, reused: refuse rather than alias, because silently
repointing a target changes what every existing call meant. It gated a branch
tip; the queue gates the merge product.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 8: The census duration tripwire

**Files:**
- Create: `cli/tests/census_duration.rs`

**Interfaces:**
- Consumes: `docs/timings.md`'s `| census |` rows.
- Produces: nothing later tasks use.

- [ ] **Step 1: Ground the parser on the real file before writing the test**

```bash
grep '| census |' docs/timings.md | tail -3
```

Observed columns, pipe-separated:
`when | label | wall_s | user_s | sys_s | cpu_ratio | rc | ref | branch | host | cores`
— e.g. `| 2026-08-14T15:36:53Z | census | 882.487 | 27996.712 | 330.979 | 32.10 | 0 | 17e0525a |  | lefford | 40 |`

Splitting on `'|'` yields a leading empty field, so `parts[1]` is `when`,
`parts[2]` is the label, `parts[3]` is `wall_s`, `parts[7]` is `rc`. **Confirm
this against the live file** before trusting it — a plan's code is the one code
nothing compiles.

- [ ] **Step 2: Write the failing test**

The `repo_root()` idiom is copied from `cli/tests/generated_paths.rs:21-27`.

```rust
//! The census duration tripwire (The Sluice).
//!
//! DIRECTION THIS CHECK ENFORCES: the most recent SUCCESSFUL census run in
//! `docs/timings.md` completed within the budget. It is structurally blind to
//! everything else — it says nothing about whether a census is current, ran on
//! the right host, or produced correct goldens.
//!
//! WHY A FIXED CEILING AND NOT A RATCHET. Nathan's rule is "if it takes longer
//! than ~15 minutes we need to freak out and profile it until it is back under
//! 15 minutes" — a budget, not a trend. A ratchet against recent best would arm
//! at the latest 882.487 s and fire on ordinary run-to-run variance, and a
//! check that is always red is ignored exactly as fast as one that is always
//! green.
//!
//! THE MARGIN IS THIN AND THAT IS THE POINT. Of the last three runs at the time
//! this landed — 949.579, 920.212, 882.487 — two would have tripped this.

use std::path::{Path, PathBuf};

/// Seconds a census may take before this test fails. Nathan's ~15 minutes.
const CENSUS_BUDGET_SECS: f64 = 900.0;

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// Every successful census row as (when, wall_seconds), oldest first.
fn successful_census_rows() -> Vec<(String, f64)> {
    let text = std::fs::read_to_string(repo_root().join("docs/timings.md"))
        .expect("docs/timings.md must exist");
    let mut rows = Vec::new();
    for line in text.lines() {
        let parts: Vec<&str> = line.split('|').map(str::trim).collect();
        // A leading empty field precedes `when`, so the label is parts[2].
        if parts.len() < 8 || parts[2] != "census" || parts[7] != "0" {
            continue;
        }
        if let Ok(wall) = parts[3].parse::<f64>() {
            rows.push((parts[1].to_string(), wall));
        }
    }
    rows
}

#[test]
fn the_census_ledger_has_rows_this_test_can_read() {
    // Guards the vacuous case: a parser that silently matches nothing would
    // make the budget assertion below pass forever.
    assert!(
        !successful_census_rows().is_empty(),
        "no successful `| census |` rows parsed from docs/timings.md — the \
         column layout changed and this test has gone vacuous"
    );
}

#[test]
fn the_latest_census_is_within_budget() {
    let rows = successful_census_rows();
    let (when, wall) = rows.last().expect("guarded by the test above").clone();
    let recent: Vec<String> = rows
        .iter()
        .rev()
        .take(5)
        .map(|(w, s)| format!("  {w}  {s:.3} s"))
        .collect();
    assert!(
        wall <= CENSUS_BUDGET_SECS,
        "the latest census took {wall:.3} s (at {when}), over the \
         {CENSUS_BUDGET_SECS:.0} s budget.\n\
         PROFILE IT — do not raise this number.\n\
         last five successful runs, newest first:\n{}",
        recent.join("\n")
    );
}
```

- [ ] **Step 3: Run it and read the output**

```bash
cargo test -p hornvale --test census_duration -- --nocapture
```

Expected: both tests PASS, because the latest run is 882.487 s. If the latest
row is over budget, that is a **finding**, not a test bug — report it and stop.

- [ ] **Step 4: Prove both tests can fail**

```bash
# 1. Budget: temporarily set CENSUS_BUDGET_SECS to 800.0, re-run, confirm the
#    failure message lists five rows. Restore.
# 2. Vacuity: temporarily change parts[2] != "census" to parts[2] != "cenzus",
#    re-run, confirm the FIRST test fails. Restore.
```

Mutation 2 is the important one: without it, a column-layout change would make
the budget assertion pass silently forever. Paste both red outputs.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add cli/tests/census_duration.rs
git commit -m "feat(sluice): a 900 s census tripwire, with a vacuity guard

Nathan's rule: over ~15 minutes, profile it rather than budget for it. A
fixed ceiling, not a ratchet — a ratchet would arm at the latest 882.487 s
and fire on ordinary variance.

Two tests, because one would be vacuous alone: if the column layout changes,
the parser matches nothing and the budget assertion passes forever. The first
test fails in exactly that case.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 9: The merge-subject contract and its witness

**Files:**
- Modify: `scripts/test-sluice.sh` (append)

**Interfaces:**
- Consumes: the merge message written by `sluice-run.sh` (Task 4).

- [ ] **Step 1: Read what actually consumes the subject**

```bash
sed -n '55,62p' tools/census/history.sh
```

It runs `git log --follow --first-parent --name-only --format='C%x09%H%x09%cI%x09%s' main -- "$path"`
and uses `%s` — the subject — as the snapshot's **epoch label**. Under `--no-ff`
the campaign's own commit is no longer on the first-parent line, so the merge
subject becomes that label.

- [ ] **Step 2: Write the failing test — append to `scripts/test-sluice.sh`**

```bash
echo "== merge subject: reads correctly as a census epoch label"
subject="$(g log -1 --format=%s HEAD)"   # any merge the chamber made
# The contract: `merge(<campaign>): <headline>` — one line, names the campaign,
# and is not git's default "Merge branch ..." which says nothing to a reader of
# a census epoch column.
if printf '%s' "$subject" | grep -qE '^merge\([a-z0-9-]+\): .+'; then
    ok "merge subject matches merge(<campaign>): <headline>"
else
    bad "merge subject '$subject' would be a useless census epoch label"
fi
if printf '%s' "$subject" | grep -q '^Merge branch'; then
    bad "git's default merge subject reached the first-parent line"
else
    ok "not git's default merge subject"
fi
```

Place this after a section that actually performs a chamber merge in the
scratch repo; if none exists yet, add one that calls `git merge --no-ff` with
the same `-m` template `sluice-run.sh` uses, so the test pins the **template**
rather than a hand-written string.

- [ ] **Step 3: Run to confirm it fails**

```bash
bash scripts/test-sluice.sh
```

Expected: FAIL on the first assertion, because the scratch repo's HEAD subject
is `two` or `main-edit`, not a merge subject.

- [ ] **Step 4: Make it pass by using the real template**

In the test, construct the merge exactly as `sluice-run.sh` does:

```bash
g checkout -q main
g merge --no-ff --no-edit -m "merge(the-sluice): a headline

Gated as the merge product by sluice job test-job.
main was $(g rev-parse main) at test time." campaign/y
```

- [ ] **Step 5: Run and confirm green, then mutate**

```bash
bash scripts/test-sluice.sh
```

Then change the `-m` template to omit `merge(...)`, re-run, confirm both
assertions go red, restore. Paste the red output.

- [ ] **Step 6: Commit**

```bash
make shellcheck
git add scripts/test-sluice.sh
git commit -m "test(sluice): the merge subject is a census epoch label

tools/census/history.sh tags every committed census snapshot with %s from
\`git log --follow --first-parent main\`. Under --no-ff that is the MERGE
subject, so a bad one silently degrades a committed artifact rather than
merely reading badly.

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 10: Decision 0139 and the documentation sweep

**Files:**
- Create: `docs/decisions/0139-main-advances-only-through-the-lock.md`
- Modify: `book/src/frontier/idea-registry.md` (only once the Mac's uncommitted
  `PROC-merge-queue` row has landed — see `.superpowers/sdd/followups.md`)

- [ ] **Step 1: The number is 0139, and checking `main` alone would have got it wrong**

**Already done, 2026-08-16, and the result is the campaign's own thesis
demonstrated on itself.** `main`'s highest decision is `0136`, so `0137` looks
free — and is not. `campaign/the-glasshouse`, unmerged, already holds **both**:

```
0137  campaign/the-glasshouse  0137-the-craton-clamp-is-a-budget-not-a-limit
0138  campaign/the-glasshouse  0138-a-preregistered-criterion-may-be-restated-when-its-estimator-is-wrong
```

Had this campaign minted `0137`, the merge would have raised **no conflict**
(different slugs), both files would coexist, `docs/digest/` would render one
line per file, and `no_gaps_in_the_decision_log` could not see it because a
duplicate creates no hole. That is exactly how two campaigns both minted
`0134` — the collision this campaign was built to prevent, arriving a second
time, in this campaign, avoided only because someone looked.

**So use `0139`**, and use this to compute it rather than `ls docs/decisions/`:

```bash
{ git ls-tree -r --name-only origin/main docs/decisions/ | grep -oE '/0[0-9]{3}' | tr -d '/'
  for b in $(git branch -r --format='%(refname:short)' | grep -E 'origin/(campaign/|the-)'); do
    git ls-tree -r --name-only "$b" docs/decisions/ 2>/dev/null | grep -oE '/0[0-9]{3}' | tr -d '/'
  done; } | sort -n | uniq | tail -1
```

That prints the highest number claimed **anywhere**, including on unmerged
branches; the next free one is that plus one. Re-run it immediately before
writing the file — another campaign may have claimed one since.

- [ ] **Step 2: Write the decision**

Read `docs/decisions/0133-nontrivial-checks-run-in-one-serial-lane.md` first
and match its structure exactly. Content:

- **Decision:** every commit on `origin/main` is the tip of a tree gated as
  itself by the queue immediately before it was pushed; the merge product, not
  the branch tip, is the gated object; an out-of-band landing is a detected
  fault, not a silent weakening.
- **Context:** the incomparable-guarantees argument; the 0134 collision; the
  measured 33% of lane jobs spent on refs that never landed.
- **Consequences:** `gate-campaign` retires; `gate-stage` and `gate-commit` are
  unchanged; the census stays outside the guarantee; head-of-line blocking is
  accepted deliberately, with `hold and fix` as the policy.
- **Supersedes:** amends 0132's rung table and 0133's placement table.

- [ ] **Step 3: Regenerate the digest and check for drift**

```bash
cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md
cargo run --manifest-path tools/digest/Cargo.toml -- render delta > docs/digest/intent-vs-reality.md
git diff --stat -- docs/digest/
```

The redirect writes the file; the bare command prints to stdout and regenerates
nothing, which makes the following drift check report an empty diff that reads
as "no drift". Branch table for the result:

- `docs/digest/` moved → expected, commit it in this same commit.
- `docs/digest/` did **not** move → the render did not run or the redirect was
  dropped. Investigate before committing; do not read it as "nothing changed".

- [ ] **Step 4: Run the full doc consistency check**

```bash
cargo test -p hornvale --test docs_consistency
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add docs/decisions/0139-*.md docs/digest/
git commit -m "docs(sluice): ratify 0139 — main advances only through the lock

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 11: The first real merge (proves the queue before anything is deleted)

**Files:**
- Create: `book/src/chronicle/the-sluice.md`
- Create: `docs/retrospectives/the-sluice.md`

- [ ] **Step 1: Merge this campaign through its own queue**

The campaign's own thesis, tested on itself. Push the branch, then:

```bash
make sluice BRANCH=campaign/the-sluice REF="$(git rev-parse campaign/the-sluice)"
make sluice-status
make sluice-log
```

If it lands, the queue works on a real campaign. If it holds, that is the first
genuine triage and it belongs in the retrospective either way.

- [ ] **Step 2: Verify the invariant held, on the real artifact**

```bash
git fetch origin
git log --first-parent --oneline origin/main | head -3
git cat-file -p origin/main | head -5      # two parents, first is the old main
cat ~/.local/state/hornvale/sluice/last-pushed
git rev-parse origin/main                  # must equal last-pushed
```

- [ ] **Step 3: STOP here if the merge did not land.** Task 12 deletes the
      lane, and the lane is the only way to run expensive checks until the
      queue demonstrably works. A queue that has not landed a merge does not
      get to remove its predecessor.

---

## Task 12: The absorption — delete the lane, measure the net

**Sequenced deliberately after Task 11.** Nothing here may run until the queue
has landed a real merge on `main`. Deleting the lane first would leave no way
to run anything expensive if the queue turned out to be wrong.

**Files:**
- Delete: `scripts/lane-dispatch.sh` (145), `scripts/lane-run.sh` (283),
  `scripts/test-lane.sh` (377), `scripts/preflight-merge.sh` (145)
- Modify: `Makefile` (remove `lane`, `lane-status`, `lane-log`, `lane-roster`,
  `lane-wait`, `gate-stage`, `preflight`), `scripts/lane-sets.tsv` (the `where`
  column loses its meaning — every phase runs in the chamber),
  `CLAUDE.md`, `scripts/CLAUDE.md`
- Keep: `scripts/lane-outboard.sh` (a phase driver), the `flock` claim (a
  mutex, not a job manager), `scripts/lane-sets.tsv` itself (data)

**Interfaces:**
- Consumes: a working queue (Tasks 2-6) and a landed merge (Task 11).

- [ ] **Step 1: Absorb `gate-stage` into the queue first, before deleting it**

A stage-gate request is a queue entry with no merge: it runs the phases against
`main + branch` and reports, but never pushes. Same mouth, same chamber, same
claim. Add a `kind` column to the queue TSV (`merge` | `stage`) rather than a
second code path — the difference is one branch at the push step.

Verify with a real stage request before the deletion in Step 2:

```bash
make sluice-stage BRANCH=<a branch> REF=<full-sha>
make sluice-status
```

Expected: phases run, nothing is pushed, the entry ends `reported`.

- [ ] **Step 2: Delete, and prove nothing still calls the deleted things**

```bash
git rm scripts/lane-dispatch.sh scripts/lane-run.sh scripts/test-lane.sh scripts/preflight-merge.sh
grep -rn 'lane-dispatch\|lane-run\|test-lane\|preflight-merge\|gate-stage\|make preflight' \
    --include='*.sh' --include='*.rs' --include='Makefile' --include='*.md' . \
    | grep -v '^./docs/superpowers/' | grep -v '^./docs/retrospectives/' | grep -v '^./book/src/chronicle/'
```

Branch table for that grep — a prediction here would be worthless, so decide by
what it prints:

- A hit in `Makefile` or a `*.sh` → a live caller. Fix it.
- A hit in a `CLAUDE.md` → stale prose. Rewrite it in this same commit.
- A hit in `docs/decisions/` → **do not edit.** Decisions are append-only;
  0139 supersedes, it does not rewrite 0132/0133.
- A hit only under the excluded paths → history describing what was true then.
  Leave it.

- [ ] **Step 3: Run the enforcement tests that know about the roster**

```bash
cargo test -p hornvale --test lane_sets
cargo test -p hornvale --test docs_consistency
cargo test -p hornvale --test generated_paths
```

`cli/tests/lane_sets.rs` asserts the roster has no second copy in prose and
may assert on the `where` column. Read its assertions before changing the TSV —
extend the vocabulary deliberately rather than loosening the check.

- [ ] **Step 4: Measure the net, which is the campaign's own commitment**

```bash
git diff --stat $(git merge-base origin/main HEAD)..HEAD -- \
    'scripts/*.sh' Makefile ':!scripts/sluice-*.sh' ':!scripts/test-sluice.sh'
git diff --stat $(git merge-base origin/main HEAD)..HEAD -- \
    'scripts/sluice-*.sh' scripts/test-sluice.sh
```

Spec §3a commits this campaign to ending with **fewer lines of process
machinery than it started**. Record both numbers and the net. If the net is
positive, that is a finding to report, not a number to bury — the guarantee did
not simplify anything, it relocated the payment.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(sluice): absorb the lane — delete 950 lines of dispatch machinery

<paste the measured net from Step 4>

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Task 13: The book

- [ ] **Step 3: Write the chronicle entry**

`book/src/chronicle/the-sluice.md`, at the book's usual altitude — technical,
comprehensible without reading the code. Lead with the incomparability argument
(a branch tip and a post-hoc main are incomparable; their join is the merge
product) and the 0134 collision as the worked example.

- [ ] **Step 4: Write the retrospective**

`docs/retrospectives/the-sluice.md` — process lessons only. Promote everything
from `.superpowers/sdd/followups.md` **before teardown**; that file is
git-ignored and dies with the worktree.

- [ ] **Step 5: Freshness sweep**

Re-read `book/src/open-questions.md` and re-score any Confidence Gradient bet
this campaign moved (decision 0030).

- [ ] **Step 6: Commit**

```bash
git add book/src/chronicle/the-sluice.md docs/retrospectives/the-sluice.md book/src/open-questions.md
git commit -m "docs(sluice): chronicle and retrospective

Claude-Session: https://claude.ai/code/session_01TUBQXYrm5S4cjFrEvaSJcJ"
```

---

## Self-review against the spec

| Spec section | Task |
|---|---|
| §5.1 request, durable queue, coalescing | 2, 6 |
| §5.2 the mouth, four pre-claim checks | 3 |
| §5.3 chamber, one claim, dedicated worktree | 4 |
| §5.3.1 detached HEAD | 4 |
| §5.3.2 merge message as artifact | 4 (writes it), 9 (pins it) |
| §5.3.3 where a fix goes | operator rule; no code — enforced by review |
| §5.4 phase order, clean between phases | 4 |
| §5.5 queue authors artifacts | 4 |
| §6 hold-and-fix, triage, loud hold | 4, 5 (exit codes + `phase_failed`) |
| §6.4 out-of-band detection | 3 (reads), 5 (writes) |
| §7 census outside the path, 900 s tripwire | 8 |
| §8 `gate-campaign` retires | 7 |
| §9 P1 credentials | done pre-plan |
| §9 P2 heavy baseline | 1 |
| §10 testing properties | 2, 3, 5, 8, 9 |
| §11 decision 0139 | 10 |

**Gap accepted deliberately:** §5.3.3 ("a behavioural fix is committed to the
branch, never amended into the merge commit") has **no mechanical enforcement**
in this plan. It is an operator rule enforced by review. Mechanising it would
mean the chamber inspecting its own commits' provenance, which is more
machinery than the risk warrants at 3.5 merges/day — but it is a known hole and
belongs in the retrospective, not left implicit.

**Interface consistency checked:** `sluice-queue.sh add|next|set-state|list`,
`sluice-mouth.sh <branch> <sha>` with exit codes 0/1/2/3/4, `sluice-run.sh
<branch> <sha>`, and `HV_SLUICE_DIR` / `HV_SLUICE_WORKTREE` / `HV_SLUICE_BASE`
/ `HV_SLUICE_ALLOW_UNPUSHED` are used identically wherever they appear.

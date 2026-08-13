# The Sexton — Stages 1–3 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Hornvale's invisible verification costs visible, recover the
accidental ones, and restore automatic scheduling to a project that deleted its
scheduler — without changing any committed artifact byte.

**Architecture:** Three stages, each independently shippable. **See** installs
instruments (a ledger row for every waiting command, a defect ledger, an
index-entry guard, and the timing alarm folded into the gate that already
computes its inputs). **Sweep** recovers accidental cost (a serial artifact
script becomes a DAG; worktrees get recycled instead of destroyed). **Bells**
restores scheduling (systemd timers on lefford, a nightly census that never
commits, and a three-world census sentinel inside the commit gate).

**Tech Stack:** bash (`scripts/`), GNU make, Rust 2024 (`cli/tests`,
`windows/lab`), `cargo nextest`, systemd timers on `lefford` (Debian 12,
systemd 252).

**Spec:** `docs/superpowers/specs/2026-08-13-the-sexton-design.md`

**Stage 4 (Decide) is deliberately not in this plan.** Its four moves — S7 gate
purpose split, S18 suite life cycle, S11 split gate, S12 census sampling — rest
on readings that Stages 1–3 produce. S11's before-arm must come from a recorded
row that Task 2 creates; S12's degradation table is a read over data Stage 3
refreshes. Writing their steps now would mean writing them against today's
partly-wrong information, which is the failure `PROC-floors-erode-unseen`
records. Stage 4 gets its own plan at the Stage 3 → 4 boundary.

## Global Constraints

Copied verbatim from the spec and CLAUDE.md; every task's requirements
implicitly include these.

- **No new workspace dependencies.** `serde`, `serde_json`, `libm` only
  (decision 0004, amended by 0041). Dev tools outside the workspace are a
  separate category (decision 0040) but this plan adds none.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting
  uses `total_cmp`. Enforced by `clippy.toml` `disallowed-types`.
- **No wall-clock time in the sim.** Scheduling infrastructure is outside the
  determinism boundary and may use real time; nothing under `kernel/`,
  `domains/`, or `windows/` may.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant gets a one-line doc comment.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the
  most common review finding.
- **No committed artifact may change bytes in Stages 1–3.** The falsifier is
  `make rebaseline` followed by the drift-check `git diff --exit-code` list
  coming back empty. If any artifact moves, STOP and report — that is an
  unplanned epoch, not a rebaseline.
- **A scheduled job never commits and never touches `main`** (spec §3.7).
- **Decision numbers are chosen after the final absorption**, checked against
  `origin/main` (`PROC-decision-number-collision` has fired three times). This
  plan mints none.
- **Run `cargo test -p hornvale --test docs_consistency` after any docs edit.**
  Registry Idea cells are capped at 600 chars and the waiver fixture is
  append-never; measure the cell with `awk -F' \| ' '{print length($2)}'`,
  never estimate it.
- **Commit messages go through a file** (`git commit -F <file>`), and that file
  is written with a **quoted heredoc** (`<<'EOF'`). `printf` truncates at a bad
  format character and git commits the partial text with exit 0.

---

# Stage 1 — See

## Task 1: A test that every generated directory is indexed

**Why:** CLAUDE.md documents the hazard in its own words — "`git diff
--exit-code <path>` is silently **VACUOUS** against a path with no index entry,
so the FIRST commit that introduces a new generated directory must `git add` it
before the check can ever fail. Nothing in regenerate-artifacts.sh guards
that." The drift check is the load-bearing verification in this repo and this is
the one way it can be silently absent.

**Files:**
- Create: `docs/generated-paths.txt` (the single source of truth for the list)
- Create: `cli/tests/generated_paths.rs`
- (No Makefile edit. An earlier draft listed one; no step performs it, and
  Task 2 rewrites that region of the Makefile anyway — pre-flight Ruling 1.)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `docs/generated-paths.txt` — one repo-relative path per line, `#`
  comments and blank lines ignored. Task 5 reads the same file.

- [ ] **Step 1: Create the path list, populated from CLAUDE.md's drift-check line**

Create `docs/generated-paths.txt`:

```
# The drift-check path list — the single source of truth.
#
# `make rebaseline` diffs these; cli/tests/generated_paths.rs asserts each is
# TRACKED, because `git diff --exit-code <path>` is silently vacuous against a
# path with no index entry (CLAUDE.md, "Generated-artifact freshness").
#
# Adding a generated directory means adding it HERE and `git add`-ing its
# contents in the same commit.
book/src/gallery/
book/src/reference/
book/src/laboratory/
docs/audits/
docs/digest/
book/src/domesday/
clients/game/core/tests/fixtures/
```

- [ ] **Step 2: Write the failing test**

Create `cli/tests/generated_paths.rs`:

```rust
//! Guards the drift-check path list (The Sexton, Task 1).
//!
//! DIRECTION THIS CHECK ENFORCES: every path DECLARED in
//! `docs/generated-paths.txt` has at least one file tracked by git. It is
//! structurally blind to the opposite direction — a generated directory that
//! nobody declared is invisible to it, and always will be.

use std::path::{Path, PathBuf};
use std::process::Command;

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The declared paths, with `#` comments and blank lines stripped.
fn declared_paths() -> Vec<String> {
    let text = std::fs::read_to_string(repo_root().join("docs/generated-paths.txt"))
        .expect("docs/generated-paths.txt must exist");
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(str::to_string)
        .collect()
}

#[test]
fn every_declared_generated_path_is_tracked() {
    let root = repo_root();
    let mut vacuous: Vec<String> = Vec::new();

    for path in declared_paths() {
        let out = Command::new("git")
            .arg("-C")
            .arg(&root)
            .args(["ls-files", "--", &path])
            .output()
            .expect("git ls-files must run");
        if out.stdout.is_empty() {
            vacuous.push(path);
        }
    }

    assert!(
        vacuous.is_empty(),
        "these declared generated paths have NO tracked files, so \
         `git diff --exit-code` over them is vacuous and can never fail — \
         `git add` their contents in the commit that introduces them:\n  {}",
        vacuous.join("\n  ")
    );
}

#[test]
fn the_declared_list_is_not_empty() {
    assert!(
        !declared_paths().is_empty(),
        "docs/generated-paths.txt declared nothing — an empty list makes \
         `every_declared_generated_path_is_tracked` vacuously green, which is \
         the exact defect that test exists to prevent, one level up"
    );
}
```

- [ ] **Step 3: Prove the test discriminates (disarm it)**

The repo rule is *disarm every check to prove it discriminates* — a check only
ever seen to pass is not known to be a check.

Run:
```bash
cd "$(git rev-parse --show-toplevel)"
cp docs/generated-paths.txt /tmp/hv-gp-backup.txt
printf '\nbook/src/no-such-generated-dir/\n' >> docs/generated-paths.txt
cargo test -p hornvale --test generated_paths
```
Expected: **FAIL**, naming `book/src/no-such-generated-dir/`.

Then restore, and prove the restore:
```bash
cp /tmp/hv-gp-backup.txt docs/generated-paths.txt
diff -q /tmp/hv-gp-backup.txt docs/generated-paths.txt && echo "RESTORED"
```

**Not `git checkout --`.** The file is still untracked at this point — it
is not `git add`-ed until Step 5 — so `git checkout -- docs/generated-paths.txt`
errors with "did not match any file(s) known to git" and restores nothing.
Found by Task 1's implementer. The imperative "then restore: `git checkout
--`" was asserting the file was tracked, which is exactly the
hidden-assertion shape `campaign-autopilot` names.

- [ ] **Step 4: Run the tests and verify they pass**

Run: `cargo test -p hornvale --test generated_paths`
Expected: PASS, 2 tests.

- [ ] **Step 5: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
cargo fmt
cat > /tmp/hv-t1.txt <<'EOF'
feat(gate): the drift-check path list is data, and a test proves it is not vacuous

`git diff --exit-code <path>` succeeds silently against a path with no index
entry, so a new generated directory is undefended until someone remembers to
`git add` it. CLAUDE.md documents the hazard and nothing enforced it.

The list moves out of prose into docs/generated-paths.txt, and
cli/tests/generated_paths.rs asserts every declared path has a tracked file.
Disarmed to prove it discriminates: adding a nonexistent path reddens it.

The check states its own direction in its module doc — declared paths are
tracked. It is structurally blind to a generated directory nobody declared.
EOF
git add docs/generated-paths.txt cli/tests/generated_paths.rs
git commit -F /tmp/hv-t1.txt
```

---

## Task 2: Ledger every command that makes a human wait

**Why:** `docs/timings.md` records five labels and nothing else, so `prewarm`
has **zero rows against 73 branches in a month** and roughly eight hours of
waiting is invisible to every cost decision. This is decision 0086's amendment
(the ledger carried zero `gate` rows while the gate crept 234 s → 934 s)
recurring one level out.

**Files:**
- Modify: `Makefile` — targets `prewarm`, `preflight`, `gate-fast`, `quick`,
  `vessel-check`, `world-check`, `game-check`
- Modify: `docs/timings.md` (header prose only — the ledger is not
  drift-checked)

**Interfaces:**
- Consumes: `scripts/timed.sh <label> -- <command...>`, unchanged. It appends
  an 11-field row: `when | label | wall_s | user_s | sys_s | cpu_ratio |
  waited_s | commit | branch | host | cores`, and passes the wrapped command's
  exit status through.
- Produces: rows under the new labels `prewarm`, `preflight`, `gate-fast`,
  `quick`, `vessel-check`, `world-check`, `game-check`. Stage 4's S11
  before-arm reads the `gate` and `gate-fast` labels from this ledger.

- [ ] **Step 1: Read the existing wrapper pattern**

`make gate` and `make ci` already use it — a thin timing target delegating to a
body target, so `timed.sh` measures the wall time a human actually waits:

```make
gate: ## ...
	@bash scripts/timed.sh gate -- make --no-print-directory gate-run

gate-run: fmt-check clippy type-audit type-audit-report test
	@bash scripts/census-advisory.sh || true
```

Every target below follows exactly this split. Do not inline `timed.sh` into a
recipe that already has prerequisites — make runs prerequisites *before* the
recipe, so the timing would exclude them.

- [ ] **Step 2: Convert `prewarm` to the wrapper pattern**

In `Makefile`, replace the `prewarm` target with a wrapper plus body. The body
keeps every existing line verbatim, including the comment block about The
Cairn's binary:

```make
prewarm: ## Warm a fresh worktree's caches (start in the background right after `git worktree add`)
	@bash scripts/timed.sh prewarm -- make --no-print-directory prewarm-run

# THE COLD-BUILD COST WAS INVISIBLE UNTIL THIS LANDED (The Sexton, Task 2).
# docs/timings.md carried five labels and 73 branches went through this target
# in one month with zero rows — roughly eight unrecorded hours, comparable to
# the census line. The wrapper above is the whole fix.
prewarm-run:
	cargo build --workspace --all-targets
	cargo build --release -p hornvale
	cargo build --manifest-path tools/type-audit/Cargo.toml
```

Preserve the remaining body lines of the existing `prewarm` recipe exactly as
they are; only the target name changes and the wrapper is added.

- [ ] **Step 3: Convert the remaining six targets the same way**

Apply the identical wrapper/body split to `preflight`, `gate-fast`, `quick`,
`vessel-check`, `world-check`, `game-check`. For example:

```make
preflight: ## GO/NO-GO before integrating a campaign branch with main (run from the branch)
	@bash scripts/timed.sh preflight -- make --no-print-directory preflight-run

preflight-run:
	@bash scripts/preflight-merge.sh
```

Add every new `*-run` target to the `.PHONY` line at the top of the Makefile.

- [ ] **Step 4: Verify each records a row and passes its exit status through**

Run:
```bash
cd "$(git rev-parse --show-toplevel)"
before=$(grep -c '^|' docs/timings.md)
make quick
after=$(grep -c '^|' docs/timings.md)
echo "rows: $before -> $after"
grep '| quick |' docs/timings.md | tail -1
```
Expected: exactly one new row, labelled `quick`, with a non-empty `host` field.

Then prove the exit status passes through — a wrapper that swallows failure is
worse than no wrapper:
```bash
bash scripts/timed.sh probe -- false; echo "exit=$?"
```
Expected: `exit=1`, and a `probe` row appended. Remove that row before
committing:
```bash
grep -v '| probe |' docs/timings.md > /tmp/t && mv /tmp/t docs/timings.md
```

- [ ] **Step 5: Update the ledger's header prose**

In `docs/timings.md`, extend the header paragraph to say what it now covers.
Add after the existing `waited_s` sentence:

```
Since The Sexton the ledger covers **every command that makes a human wait**,
not only the expensive milestone runs it was built for: `prewarm`, `preflight`,
`gate-fast`, `quick` and the three client checks record rows alongside `gate`,
`ci`, `rebaseline`, `census` and `heavy`. The reason is decision 0086's
amendment generalised — a cost with no label is invisible to every decision
about cost, and `prewarm` had zero rows against 73 branches in a month.
```

- [ ] **Step 6: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
cat > /tmp/hv-t2.txt <<'EOF'
feat(timings): every command that makes a human wait leaves a row

docs/timings.md recorded five labels. prewarm had ZERO rows against 73
distinct branches in one month, at a measured 771 s per full workspace build
— roughly eight hours of waiting invisible to every decision ever made about
cost.

prewarm, preflight, gate-fast, quick, vessel-check, world-check and
game-check now use the same timed.sh wrapper/body split gate and ci already
use. Verified that a wrapped failure still propagates its exit status.

This is decision 0086's amendment generalised: that one wired the ledger to
`gate` after the gate crept 234 s -> 934 s unobserved. The lesson was applied
to the gate and never to anything else.
EOF
git add Makefile docs/timings.md
git commit -F /tmp/hv-t2.txt
```

---

## Task 3: Fold `make ci`'s instrumentation into `make gate`

**Why:** `make ci` has run **9 times against `make gate`'s 368**. The
Timekeeper built a per-test duration alarm to watch a gate that crept 234 s →
934 s, and it runs at 2.4% of the frequency of the thing it watches — while
every gate already computes the durations it needs and discards them.

**Files:**
- Modify: `Makefile` — `gate-run`, `ci`, `ci-run`
- Modify: `.config/nextest.toml` (comment only)

**Interfaces:**
- Consumes: `cargo nextest run --workspace --profile ci --message-format
  libtest-json-plus` writing `target/nextest/ci/run.json`; `cargo test -q -p
  hornvale --test timings_alarm -- --ignored --nocapture`; `cargo run --quiet
  -p hornvale -- ci-record`.
- Produces: `make gate` writes `target/nextest/ci/run.json` and updates
  `docs/timings/test-baseline-$(hostname -s).tsv` on a green run. `make ci`
  becomes an alias for `make gate` and is retained so existing muscle memory
  and documentation keep working.

- [ ] **Step 1: Record the before-arm, on a quiet box**

The ordering constraints in this task are load-bearing and the Makefile
documents them at length. Capture the current behaviour first:

```bash
cd "$(git rev-parse --show-toplevel)"
hostname -s
cp docs/timings/test-baseline-$(hostname -s).tsv /tmp/hv-baseline-before.tsv
wc -l /tmp/hv-baseline-before.tsv
```

- [ ] **Step 2: Move `ci-run`'s body into `gate-run`, preserving the ordering**

Replace `gate-run` in the `Makefile`. **Three constraints from `ci-run`'s
existing comments must survive verbatim in behaviour:** the alarm runs BEFORE
`ci-record` overwrites the baseline it compares against; nextest's status is
captured immediately rather than discarded; and a red run never becomes a
baseline.

```make
# THREE CORRECTIONS TO AN EARLIER DRAFT OF THIS RECIPE, all found by the
# one-task-ahead brief check and all load-bearing:
#
# (a) `nextest-check` STAYS a prerequisite. The earlier draft dropped the
#     `test` target as a prereq and called nextest directly, which silently
#     discarded `test`'s own `nextest-check` prereq — the target whose entire
#     job is to fail with an install hint when cargo-nextest is missing. A
#     machine without it would have got `command not found` instead.
#
# (b) THE DEFAULT PROFILE, NOT `ci`. `.config/nextest.toml` states that
#     `[profile.default]` is "deliberately left at nextest's own defaults:
#     `make gate` must behave exactly as it did before this campaign", and the
#     `ci` profile sets `fail-fast = false`. Running the gate under `ci` would
#     silently turn every red gate into a full-suite run — 368 times a month,
#     on the axis this campaign exists to protect. The durations the alarm
#     needs are complete on a GREEN run regardless of profile, and a green run
#     is the only run whose durations are ever recorded, so the `ci` profile
#     buys nothing here and costs fast red feedback.
#
# (c) THE ALARM RUNS ONLY ON GREEN. Consequence of (b), and correct
#     independently: under fail-fast a red run's `run.json` is TRUNCATED, so
#     alarming against it compares a partial suite to a whole-suite baseline
#     and can report a regression that does not exist. The spec already states
#     this principle for S7 — "a duration measured under a partial run is not
#     comparable to a baseline" — and it binds here first.
#
# THE GATE IS NOW ALSO THE MEASUREMENT (The Sexton, Task 3). `make ci` ran 9
# times against this target's 368: an instrument watching a gate that crept
# 234 s -> 934 s, running at 2.4% of that gate's frequency, while every gate
# already computed the durations it needed and threw them away.
#
# ORDER IS LOAD-BEARING, unchanged from ci-run: the alarm must compare this run
# against the baseline still on disk from the LAST recorded run, so it runs
# BEFORE ci-record overwrites that file. Recording first would make every run
# compare against itself and the alarm could never fire.
#
# A RED RUN NEVER BECOMES THE BASELINE — guarded on BOTH statuses, because the
# unguarded version was a one-way ratchet: the alarm fired at 2x and ci-record
# immediately wrote the inflated durations back as the new reference, erasing
# its own evidence.
gate-run: fmt-check clippy type-audit type-audit-report nextest-check
	@mkdir -p target/nextest/ci docs/timings
	@NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1 cargo nextest run --workspace \
	    --message-format libtest-json-plus \
	    > target/nextest/ci/run.json 2> target/nextest/ci/run.log; \
	nextest_status=$$?; \
	cargo test -q --workspace --doc; \
	doctest_status=$$?; \
	if [ $$nextest_status -eq 0 ] && [ $$doctest_status -eq 0 ]; then \
	    cargo test -q -p hornvale --test timings_alarm -- --ignored --nocapture; \
	    alarm_status=$$?; \
	else \
	    alarm_status=0; \
	    echo "make gate: skipping the duration alarm — the run was red, so its durations are truncated and not comparable to a baseline" >&2; \
	fi; \
	if [ $$nextest_status -eq 0 ] && [ $$doctest_status -eq 0 ] && [ $$alarm_status -eq 0 ]; then \
	    cargo run --quiet -p hornvale -- ci-record; \
	else \
	    echo "make gate: NOT recording a baseline — the run was red, so these durations are not a reference" >&2; \
	fi; \
	echo ""; \
	echo "== detail written to =="; \
	echo "  target/nextest/ci/run.json   structured per-test durations"; \
	echo "  target/nextest/ci/run.log    human output, including failures"; \
	echo "  docs/timings/test-baseline-$$(hostname -s).tsv   recorded baseline"; \
	if [ $$nextest_status -ne 0 ]; then \
	    echo "make gate: FAILED — nextest was red (exit $$nextest_status); see target/nextest/ci/run.log" >&2; \
	    exit $$nextest_status; \
	fi; \
	if [ $$doctest_status -ne 0 ]; then \
	    echo "make gate: FAILED — doctests were red (exit $$doctest_status)" >&2; \
	    exit $$doctest_status; \
	fi; \
	bash scripts/census-advisory.sh || true; \
	exit $$alarm_status
```

Note `test` is no longer a prerequisite of `gate-run` — the nextest invocation
above replaces it, and `nextest-check` takes its place on the prerequisite line
so the install hint survives (Ruling 4). Leave the `test` target itself in
place, but not for the reason an earlier draft gave: **nothing else depends on
it.** Verified — `gate-run` was its only consumer, so after this change `test`
is reachable only by a human typing `make test`, which is still worth keeping.

- [ ] **Step 3: Make `ci` an alias**

```make
ci: gate ## Alias for `make gate`, which now carries the timing alarm (The Sexton)
	@echo "make ci: \`make gate\` now records the baseline and runs the alarm; this is an alias." >&2
```

**The backticks MUST stay escaped.** Unescaped, they are shell command
substitution inside a double-quoted string, so every `make ci` would run the
entire gate a second time — silently, since the printed text looks identical.
Caught by Task 3's implementer. The repo has recorded this exact hazard before:
`PROC-commit-message-via-file` describes backticked prose expanding and running
a real gate.

Delete the `ci-run` target. Remove `ci-run` from `.PHONY`.

- [ ] **Step 4: Verify the alarm fires and a red run does not record**

Run on a quiet box:
```bash
cd "$(git rev-parse --show-toplevel)"
make gate 2>&1 | tee /tmp/hv-gate.txt
tail -20 /tmp/hv-gate.txt
git diff --stat docs/timings/
```
Expected: PASS; `run.json` non-empty; the baseline file for this host shows as
modified.

Then prove the red-run guard, which is the one behaviour whose loss would be
silent. Introduce a deliberate failure, run, and confirm the baseline is
untouched:
```bash
cp docs/timings/test-baseline-$(hostname -s).tsv /tmp/hv-b4red.tsv
# Add a failing test to a cheap crate, run the gate, then remove it.
cat >> kernel/tests/hv_probe_red.rs <<'RS'
#[test]
fn hv_probe_deliberate_red() {
    // NOT `assert!(false, ...)`: clippy::assertions_on_constants denies it under
    // `-D warnings`, so the probe would fail at CLIPPY rather than inside
    // nextest — proving nothing about the alarm and recorder guards it exists
    // to exercise. Found by Task 3's implementer.
    assert_eq!(1, 2, "deliberate red — The Sexton Task 3 guard proof");
}
RS
make gate; echo "gate exit=$?"
diff -q /tmp/hv-b4red.tsv docs/timings/test-baseline-$(hostname -s).tsv \
  && echo "GUARD HELD: baseline untouched by a red run" \
  || echo "GUARD FAILED: a red run rewrote the baseline"
rm kernel/tests/hv_probe_red.rs
```
Expected: non-zero gate exit, and `GUARD HELD`.

- [ ] **Step 5: Update CLAUDE.md's gate-ladder block**

In the root `CLAUDE.md`, replace the `make ci` bullet's opening with:

```
#   make ci          # ALIAS for `make gate` since The Sexton. The Timekeeper's
#                     # duration alarm and baseline recorder now run inside the
#                     # gate itself, because `make ci` had run 9 times against
#                     # `make gate`'s 368 while every gate already computed the
#                     # durations it needed and discarded them.
```

Leave the two known-blind-spot paragraphs below it intact — they still apply,
and blind spot (1) becomes calibratable now that samples arrive 40x more often.

- [ ] **Step 6: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
cargo fmt
cat > /tmp/hv-t3.txt <<'EOF'
feat(gate): the gate is also the measurement; `make ci` becomes an alias

make ci ran 9 times against make gate's 368 — an instrument built to watch a
gate that crept 234 s -> 934 s, running at 2.4% of the frequency of the thing
it watches, while every gate already computed the per-test durations it needed
and threw them away.

The libtest-json stream, the duration alarm and ci-record move into gate-run.
Every ordering constraint ci-run documented is preserved: the alarm compares
against the baseline still on disk BEFORE ci-record overwrites it, statuses
are captured rather than discarded, and a red run never becomes a baseline.

Both guards proved by disarming: a deliberate failing test leaves the baseline
byte-identical and fails the gate.

The Timekeeper's blind spot (1) — the contention guard cannot see ordinary
load — becomes calibratable at 368 samples a month instead of 9.
EOF
git add Makefile CLAUDE.md docs/timings/
git commit -F /tmp/hv-t3.txt
```

---

## Task 4: A committed defect ledger

**Why:** nobody records whether a test has ever caught anything.
`docs/timings.md` has six RED rows and each records only *that* it was red.
Every retirement or scheduling decision in Stage 4 and beyond is an opinion
without this, and actuarial with it.

**Files:**
- Create: `scripts/defect-ledger.sh`
- Create: `docs/timings/defects-README.md`
- Modify: `Makefile` — `gate-run` (one line, after the status capture)

**Interfaces:**
- Consumes: `target/nextest/ci/run.json` (written by Task 3's `gate-run`),
  which carries one JSON object per line including `"type":"test"` events with
  `"name"` and `"event"` fields.
- Produces: `docs/timings/defects-$(hostname -s).tsv`, tab-separated, columns:
  `when_utc`, `commit`, `branch`, `changed_crates`, `failed_test`. One row per
  failing test per red gate.

- [ ] **Step 1: Write the extractor**

Create `scripts/defect-ledger.sh`:

```bash
#!/usr/bin/env bash
# scripts/defect-ledger.sh — what the gate actually catches (The Sexton).
#
# docs/timings.md records six RED rows and each says only THAT it was red.
# Every "retire this test" / "run that one less often" decision therefore
# rests on an opinion. This records WHICH test failed, on WHAT change, so the
# question "has this test ever caught anything?" becomes answerable.
#
# Committed and per-host, matching test-baseline-<host>.tsv, for the reason
# CLAUDE.md gives for that file: `git log -p` on it is the archaeology.
#
# Usage: scripts/defect-ledger.sh <path-to-run.json>
set -uo pipefail

ROOT="$(git rev-parse --show-toplevel)"
HOST="$(hostname -s 2>/dev/null || echo '-')"
LEDGER="$ROOT/docs/timings/defects-$HOST.tsv"
RUN_JSON="${1:?usage: defect-ledger.sh <run.json>}"

[ -f "$RUN_JSON" ] || { echo "defect-ledger: no $RUN_JSON; nothing to record" >&2; exit 0; }

# The crates a human touched, by the same directory->crate mapping gate-fast
# uses. Overapproximation is fine here; this is a correlate, not a gate.
changed_crates="$(
    git -C "$ROOT" diff --name-only HEAD 2>/dev/null \
    | awk -F/ '{ if ($1=="kernel") print "kernel"; else if (NF>1) print $1"/"$2 }' \
    | sort -u | paste -sd, - )"
[ -n "$changed_crates" ] || changed_crates='(none)'

# ORDER-INDEPENDENT, AND THAT IS NOT A STYLE CHOICE. nextest's
# libtest-json-plus emits `event` BEFORE `name`:
#   {"type":"test","event":"ok","name":"crate::bin$test","exec_time":0.021}
# An earlier draft of this script grepped a name-then-event pattern. Measured
# against a real 3,449-test run.json: that pattern matched 0 lines and this one
# matched all 3,449. The failure mode was silent — the script would exit 0,
# print nothing, and leave an empty ledger, which is indistinguishable from
# "no tests failed".
failed_lines="$(grep '"event":"failed"' "$RUN_JSON" 2>/dev/null)"
[ -n "$failed_lines" ] || exit 0

failed="$(printf '%s\n' "$failed_lines" \
          | grep -o '"name":"[^"]*"' | sed 's/^"name":"//; s/"$//' | sort -u)"

# A parse failure must be LOUD. If there are failed events but no name parses
# out of them, the extractor is broken — not the suite — and a silent empty
# ledger would hide exactly the data this script exists to collect.
if [ -z "$failed" ]; then
    echo "defect-ledger: $RUN_JSON has failed events but no parseable test names — the EXTRACTOR is broken, not the suite" >&2
    exit 1
fi

if [ ! -f "$LEDGER" ]; then
    printf '# Defect ledger for %s — which test caught what, and on what change.\n' "$HOST" > "$LEDGER"
    printf '# Written by scripts/defect-ledger.sh on every RED gate. Committed:\n' >> "$LEDGER"
    printf '# `git log -p` on this file is the archaeology of what the suite defends.\n' >> "$LEDGER"
    printf 'when_utc\tcommit\tbranch\tchanged_crates\tfailed_test\n' >> "$LEDGER"
fi

when="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
commit="$(git -C "$ROOT" rev-parse --short HEAD 2>/dev/null || echo '-')"
branch="$(git -C "$ROOT" branch --show-current 2>/dev/null || echo '-')"

printf '%s\n' "$failed" | while IFS= read -r t; do
    printf '%s\t%s\t%s\t%s\t%s\n' "$when" "$commit" "$branch" "$changed_crates" "$t" >> "$LEDGER"
done

echo "defect-ledger: recorded $(printf '%s\n' "$failed" | wc -l | tr -d ' ') failing test(s) to $LEDGER" >&2
```

Make it executable and shellcheck it:
```bash
chmod +x scripts/defect-ledger.sh
shellcheck scripts/defect-ledger.sh
```
Expected: clean.

- [ ] **Step 2: Wire it into `gate-run`**

In the `Makefile`'s `gate-run` recipe from Task 3, add one line immediately
after `alarm_status=$$?;`:

```make
	bash scripts/defect-ledger.sh target/nextest/ci/run.json || true; \
```

`|| true` is deliberate: a bookkeeping failure must never change a gate's
verdict. The script exits 0 when there is nothing to record.

- [ ] **Step 3: Prove it records a real failure and records nothing on green**

```bash
cd "$(git rev-parse --show-toplevel)"
HOST=$(hostname -s)
rm -f docs/timings/defects-$HOST.tsv

cat >> kernel/tests/hv_probe_red.rs <<'RS'
#[test]
fn hv_probe_deliberate_red() {
    // NOT `assert!(false, ...)` — clippy::assertions_on_constants denies it under
    // `-D warnings`, so the probe would redden clippy instead of nextest and
    // never reach the defect ledger this step is testing.
    assert_eq!(1, 2, "deliberate red — The Sexton Task 4 ledger proof");
}
RS
make gate; echo "gate exit=$?"
cat docs/timings/defects-$HOST.tsv
rm kernel/tests/hv_probe_red.rs
```
Expected: a row naming `hv_probe_deliberate_red`, with `changed_crates`
containing `kernel`.

**If that file is empty or missing, STOP and report it** — do not "fix" it by
loosening the grep until something appears. An empty ledger after a known red
run means the extractor did not match, which is the precise silent failure this
script was rewritten to avoid. Confirm the raw material is actually there
before touching the script:

```bash
grep -c '"event":"failed"' target/nextest/ci/run.json
grep '"event":"failed"' target/nextest/ci/run.json | head -1 | cut -c1-200
```

Then the green case:
```bash
rows_before=$(wc -l < docs/timings/defects-$HOST.tsv)
make gate
rows_after=$(wc -l < docs/timings/defects-$HOST.tsv)
[ "$rows_before" = "$rows_after" ] && echo "GREEN RUN ADDED NO ROWS (correct)"
```
Expected: `GREEN RUN ADDED NO ROWS (correct)`.

- [ ] **Step 4: Document what the ledger is for**

Create `docs/timings/defects-README.md`:

```markdown
# The defect ledger

One file per host, `defects-<host>.tsv`, appended by
[`scripts/defect-ledger.sh`](../../scripts/defect-ledger.sh) on every red gate.

**The question it exists to answer: has this test ever caught anything?**
Before The Sexton nothing recorded it. `docs/timings.md` had six RED rows and
each said only *that* it was red, so every "retire this test" or "run that one
less often" proposal rested on an opinion.

**Read it, do not gate on it.** Like `docs/timings.md` it is a record, not a
check. It is committed and per-host so `git log -p` is the archaeology.

**Known limits, stated so nobody over-reads a row.**

- `changed_crates` is a *correlate*, not a cause: it is the working tree's
  changed paths at the moment the gate ran, overapproximated to crate
  granularity. A test that fails for an unrelated reason still records
  whatever was dirty.
- A test that never appears here has not been *proven* useless. It may guard
  something no one has broken yet. Absence is weak evidence; presence is
  strong evidence.
- Only tests that run in `make gate` can appear. The heavy tier and the
  censuses are invisible to it.
```

- [ ] **Step 5: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
cat > /tmp/hv-t4.txt <<'EOF'
feat(timings): a committed defect ledger — which test caught what

Nobody records whether a test has ever caught anything. docs/timings.md has
six RED rows and each says only THAT it was red; make ci writes structured
per-test results and discards them 359 runs out of 368. So every "retire this
test" or "run that one every tenth gate" proposal is an opinion.

scripts/defect-ledger.sh extracts failing test ids from the run.json the gate
now writes and appends them, with the changed-crate set, to a committed
per-host TSV — matching test-baseline-<host>.tsv, for the reason CLAUDE.md
gives for that file: git log -p on it is the archaeology.

Proved in both directions: a deliberate failing test records a row naming it;
a green gate adds none.

Wired with `|| true` — bookkeeping must never change a gate's verdict.

The README states the limits plainly: changed_crates is a correlate not a
cause, absence is weak evidence, and the heavy tier is invisible to it.
EOF
git add scripts/defect-ledger.sh docs/timings/defects-README.md Makefile
git commit -F /tmp/hv-t4.txt
```

---

## Stage 1 boundary

- [ ] **Absorb main and re-gate.** Per CLAUDE.md, campaign branches absorb main
  at every plan-stage boundary.

```bash
cd "$(git rev-parse --show-toplevel)"
git fetch origin
make preflight
# On an ancestry NO-GO: merge origin/main INTO the branch and re-run the gate here.
make gate
make rebaseline
git diff --exit-code $(grep -v '^#' docs/generated-paths.txt | tr '\n' ' ')
```
Expected: gate green, drift check empty. **If any artifact moved, STOP** — no
task in Stage 1 may change a committed artifact byte.

---

# Stage 2 — Sweep

## Task 5: `regenerate-artifacts.sh` runs as a DAG

**Why:** 62 sequential `cargo run` invocations at a measured `cpu_ratio` of
**0.72–2.11 on 10–12 core boxes** — effectively serial. Nobody decided that; it
is an artifact of the script being a shell list. `rebaseline` is 12.2 hours a
month, 15.9% of all measured waiting.

**Files:**
- Modify: `scripts/regenerate-artifacts.sh`

**Interfaces:**
- Consumes: nothing new.
- Produces: byte-identical output to today's, at a higher `cpu_ratio`.

- [ ] **Step 1: Capture the before-arm — the bytes AND the timing**

The success criterion is byte-identity, so capture a reference first:

```bash
cd "$(git rev-parse --show-toplevel)"
hostname -s
make rebaseline
git status --porcelain > /tmp/hv-regen-before-status.txt
tar czf /tmp/hv-regen-before.tgz $(grep -v '^#' docs/generated-paths.txt | tr '\n' ' ')
grep '| rebaseline |' docs/timings.md | tail -3
```

- [ ] **Step 2: Identify the independent groups**

Read `scripts/regenerate-artifacts.sh` in full and classify each invocation:

- **Group A — world builders.** The three `hornvale new` calls producing
  `$w42`, `$wsky`, `$wlocked`. Independent of each other; everything else
  depends on one of them.
- **Group B — readers of `$w42` / `$wsky` / `$wlocked`.** Almanacs, `explain`,
  `dictionary`, `locale`, `possess`, `history`, `connections`, maps. Each reads
  a world file and writes one output file; independent of each other.
- **Group C — world-free dumps.** `concepts`, `concepts --manifest`, `streams`,
  `phonology`, `proto goblinoid|dwarf|elf`, the type-audit report, the digest
  renders. Independent of everything.
- **Group D — the studies.** `lab run` invocations; these already parallelise
  internally across seeds, so they run **serially with respect to each other**
  and must not be co-scheduled with anything else.

Write the classification as a comment block at the top of the script. **Do not
guess a group** — if an invocation's inputs are not obvious from its arguments,
trace them.

- [ ] **Step 3: Add a minimal job runner and parallelise groups A, B and C**

Add near the top of `scripts/regenerate-artifacts.sh`:

```bash
# THE SCRIPT WAS A LIST AND IS NOW A DAG (The Sexton, Task 5).
#
# 62 sequential `cargo run` invocations measured cpu_ratio 0.72-2.11 on 10-12
# core boxes — effectively serial, on 15.9% of all measured human waiting.
# Nobody decided that; it fell out of the file being a shell list.
#
# Outputs are DISTINCT FILES, so ordering cannot affect bytes. The success
# criterion is `make rebaseline` leaving every generated artifact
# byte-unchanged — the same falsifier The Whetstone used for its profile
# change. Group D (studies) stays serial: `lab run` already saturates every
# core internally, so co-scheduling it would oversubscribe, exactly as
# .config/nextest.toml documents for the scattered batteries.
HV_JOBS="${HV_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"
_pids=()
spawn() { "$@" & _pids+=("$!"); }
reap() {
    local rc=0 p
    for p in ${_pids+"${_pids[@]}"}; do wait "$p" || rc=1; done
    _pids=()
    [ "$rc" -eq 0 ] || { echo "regenerate-artifacts: a parallel job failed" >&2; exit 1; }
}
```

Then bracket each independent group with `spawn`/`reap`. Group A first, then a
`reap`, then groups B and C spawned together, then a `reap`, then group D
serially as it is today.

**`reap` must be called after every group.** A missing `reap` means a later
step reads a file still being written — a race that would appear as
intermittent byte drift and would be blamed on determinism.

- [ ] **Step 4: Verify byte-identity — the load-bearing check**

```bash
cd "$(git rev-parse --show-toplevel)"
git checkout -- $(grep -v '^#' docs/generated-paths.txt | tr '\n' ' ')
make rebaseline
git diff --exit-code $(grep -v '^#' docs/generated-paths.txt | tr '\n' ' ') \
  && echo "BYTE-IDENTICAL (criterion met)" \
  || echo "DRIFT — the DAG changed output; STOP and report"
grep '| rebaseline |' docs/timings.md | tail -1
```
Expected: `BYTE-IDENTICAL (criterion met)`, and the new row's `cpu_ratio`
column above 4. **If bytes moved, revert and report** — do not rebaseline over
it.

Run it a second time to catch an ordering race that only fires sometimes:
```bash
make rebaseline
git diff --exit-code $(grep -v '^#' docs/generated-paths.txt | tr '\n' ' ') \
  && echo "STABLE ACROSS TWO RUNS"
```

- [ ] **Step 5: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
shellcheck scripts/regenerate-artifacts.sh
cat > /tmp/hv-t5.txt <<'EOF'
perf(artifacts): regenerate-artifacts.sh is a DAG, not a list

62 sequential cargo run invocations at a measured cpu_ratio of 0.72-2.11 on
10-12 core boxes — effectively serial, on 15.9% of all measured human waiting
(12.2 h in one month). Nobody decided that; it fell out of the file being a
shell list.

World builds, world readers and world-free dumps now run concurrently; the
studies stay serial because lab run already saturates every core internally,
the same argument .config/nextest.toml makes for the scattered batteries.

Byte-identity is the criterion, not a hope: make rebaseline leaves every
generated artifact unchanged, verified twice to catch an ordering race that
only fires sometimes.
EOF
git add scripts/regenerate-artifacts.sh
git commit -F /tmp/hv-t5.txt
```

---

## Task 6: Recycle worktrees instead of destroying them

**Why:** 43 GB of `target/` across three worktrees, 73 distinct branches in one
month, no compilation cache. The cost is caused by *destroying* worktrees, not
by lacking a cache — and recycling captures most of it with zero new
determinism surface. A compiler cache that ever returns a wrong object file is
a silent determinism violation, the worst bug class in this repo.

**Files:**
- Create: `scripts/worktree-take.sh`
- Modify: `Makefile` — add `worktree-take`
- Modify: `CLAUDE.md` — the campaign-worktree paragraph

**Interfaces:**
- Consumes: `make prewarm` (now timed, Task 2).
- Produces: `make worktree-take NAME=<campaign> [BASE=main]`.

- [ ] **Step 1: Write the script**

Create `scripts/worktree-take.sh`:

```bash
#!/usr/bin/env bash
# scripts/worktree-take.sh — claim a RECYCLED worktree (The Sexton, Task 6).
#
# 73 branches went through this repo in one month against 3 live worktrees,
# each carrying 6-29 GB of target/ and each new one paying a full cold build
# (a measured 771 s). The cost is DESTROYING worktrees, not lacking a cache.
#
# A pool member keeps its warm target/ across campaigns; `git switch -c` costs
# an incremental rebuild instead of a full one.
#
# SWEEPS THE SCRATCH. `.superpowers/sdd/` is per-worktree and git-ignored, so a
# recycled worktree would otherwise hand the next campaign the previous one's
# decision ledger — silently, and it would read as its own.
set -euo pipefail

NAME="${NAME:?usage: make worktree-take NAME=<campaign> [BASE=main]}"
BASE="${BASE:-main}"
ROOT="$(git rev-parse --show-toplevel)"
POOL="$ROOT/.claude/worktrees"
DEST="$POOL/$NAME"

if [ -d "$DEST" ]; then
    echo "worktree-take: $DEST already exists; nothing to do" >&2
    exit 0
fi

# Prefer an existing pool member whose branch is already merged into BASE.
recycled=""
while IFS= read -r wt; do
    [ -d "$wt" ] || continue
    br="$(git -C "$wt" branch --show-current 2>/dev/null || echo '')"
    [ -n "$br" ] || continue
    if git -C "$ROOT" merge-base --is-ancestor "$br" "origin/$BASE" 2>/dev/null; then
        recycled="$wt"; break
    fi
done < <(find "$POOL" -mindepth 1 -maxdepth 1 -type d 2>/dev/null)

if [ -n "$recycled" ]; then
    echo "worktree-take: recycling $recycled (its branch is merged into $BASE)" >&2
    git -C "$recycled" fetch origin
    git -C "$recycled" switch -c "campaign/$NAME" "origin/$BASE"
    rm -rf "$recycled/.superpowers/sdd"
    mv "$recycled" "$DEST"
    # `mv` leaves the MAIN REPO's back-pointer stale. Verified, and the naive
    # assumption is wrong in an important way: the moved worktree's own
    # commands keep working (its `.git` file is an absolute path to an
    # unchanged admin dir), so nothing looks broken. What breaks is
    # `.git/worktrees/<name>/gitdir`, which still names the OLD path — so
    # `git worktree list` reports a path that no longer exists and
    # `git worktree prune` may reap a live worktree. Probe output:
    #   $ git worktree repair /tmp/hv-mv-probe-moved
    #   repair: gitdir incorrect: .../.git/worktrees/hv-mv-probe/gitdir
    git -C "$ROOT" worktree repair "$DEST"
    echo "worktree-take: $DEST is warm — no prewarm needed" >&2
else
    echo "worktree-take: no recyclable member; creating a cold worktree" >&2
    git -C "$ROOT" worktree add "$DEST" -b "campaign/$NAME" "origin/$BASE"
    echo "worktree-take: run 'make prewarm' in $DEST, in the background" >&2
fi
```

```bash
chmod +x scripts/worktree-take.sh
shellcheck scripts/worktree-take.sh
```

- [ ] **Step 2: Add the make target**

```make
worktree-take: ## Claim a recycled campaign worktree (NAME=<campaign> [BASE=main])
	@NAME="$(NAME)" BASE="$(BASE)" bash scripts/worktree-take.sh
```

Add `worktree-take` to `.PHONY`.

- [ ] **Step 3: Verify recycling and the scratch sweep**

```bash
cd "$(git rev-parse --show-toplevel)"
git fetch origin
# Cold path, since no pool member is merged yet:
make worktree-take NAME=hv-probe-sexton
ls -d .claude/worktrees/hv-probe-sexton
mkdir -p .claude/worktrees/hv-probe-sexton/.superpowers/sdd
echo "previous campaign's ledger" > .claude/worktrees/hv-probe-sexton/.superpowers/sdd/decision-ledger.md
```

Now prove the sweep, by making that branch an ancestor of main and re-taking:
```bash
git -C .claude/worktrees/hv-probe-sexton switch -c hv-probe-merged origin/main
mv .claude/worktrees/hv-probe-sexton .claude/worktrees/hv-probe-recycle-src
make worktree-take NAME=hv-probe-sexton2
test ! -e .claude/worktrees/hv-probe-sexton2/.superpowers/sdd/decision-ledger.md \
  && echo "SCRATCH SWEPT (correct)" \
  || echo "SCRATCH LEAKED — the next campaign would inherit a foreign ledger"
```
Expected: `SCRATCH SWEPT (correct)`.

Clean up the probes:
```bash
git worktree remove --force .claude/worktrees/hv-probe-sexton2 || true
rm -rf .claude/worktrees/hv-probe-recycle-src
git worktree prune
git branch -D campaign/hv-probe-sexton campaign/hv-probe-sexton2 hv-probe-merged 2>/dev/null || true
```

- [ ] **Step 4: Update CLAUDE.md**

Replace the sentence "Campaigns run in git worktrees under
`.claude/worktrees/<campaign>/` (untracked); `make prewarm` warms a fresh one's
`target/`" with:

```
Campaigns run in git worktrees under `.claude/worktrees/<campaign>/`
(untracked), and since The Sexton those worktrees are a **recycled pool**, not
one-per-campaign: `make worktree-take NAME=<campaign>` reuses a member whose
branch is already merged, keeping its warm `target/` and sweeping its
`.superpowers/sdd/` scratch. 73 branches went through this repo in one month
against 3 live worktrees, each new one paying a full cold build (a measured
771 s) that nothing recorded. `make prewarm` still warms a genuinely cold one —
start it in the background right after taking it. **The scratch sweep is not
optional**: `.superpowers/sdd/` is git-ignored and per-worktree, so a recycled
worktree would otherwise hand the next campaign the previous one's decision
ledger, silently, and it would read as its own.
```

- [ ] **Step 5: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
cat > /tmp/hv-t6.txt <<'EOF'
feat(worktrees): recycle a pool instead of destroying one per campaign

43 GB of target/ across three worktrees, 73 distinct branches in one month, no
compilation cache, and zero ledger rows for any of it. The cost is DESTROYING
worktrees, not lacking a cache — so recycling captures most of it with no new
determinism surface.

Deliberately not sccache. It is admissible (0004 binds the workspace's
dependencies; a RUSTC_WRAPPER is a dev tool, the category 0040 used to admit
nextest) but a cache that ever returns a wrong object file is a silent
determinism violation, which is a poor first instrument for a saving a
scheduling change already gets. TOOL-sccache carries the option with the risk
stated.

worktree-take sweeps .superpowers/sdd/ on recycle, proved by test: it is
git-ignored and per-worktree, so without the sweep the next campaign inherits
the previous one's decision ledger and it reads as its own.
EOF
git add scripts/worktree-take.sh Makefile CLAUDE.md
git commit -F /tmp/hv-t6.txt
```

---

## Stage 2 boundary

- [ ] **Absorb main, re-gate, and record the recovered cost.**

```bash
cd "$(git rev-parse --show-toplevel)"
git fetch origin && make preflight
make gate
make rebaseline
git diff --exit-code $(grep -v '^#' docs/generated-paths.txt | tr '\n' ' ')
bash scripts/timed.sh report rebaseline | tail -5
```
Expected: green, empty drift, and `rebaseline` rows showing `cpu_ratio > 4`.

---

# Stage 3 — Bells

## Task 7: The census sentinel

**Why:** the last two census refreshes moved 3 of 205 columns each, and the
second cost 5 h 20 m — discovered at campaign close, five hours after the
commit that caused it. Measured all-metric cost is 31.34 CPU-s/world, so three
worlds is ~94 CPU-s ≈ 8 s wall on twelve cores: ~1.6% of the 489 s gate floor
on `ambrose`.

**Files:**
- Create: `windows/lab/tests/census_sentinel.rs`
- Create: `windows/lab/tests/fixtures/sentinel-waivers.txt`

**Interfaces:**
- Consumes: `hornvale_lab::load_study`, `hornvale_lab::run`,
  `hornvale_lab::canonical_row` (note: `canonical_row`, **not**
  `canonicalize_row`), `hornvale_lab::load_rows`,
  `hornvale_lab::CENSUS_GOLDENS_DIR` (re-exported at the crate root from
  `census_guard`; value `"book/src/laboratory/generated"`) — verified: `Study`
  derives `Clone` and `Seeds { from: u64, count: u64 }` is public, so
  `sentinel.seeds.count = 3` compiles.
- Produces: nothing later tasks consume.

- [ ] **Step 1: Read the APIs before writing against them**

```bash
cd "$(git rev-parse --show-toplevel)"
grep -n "pub fn canonical_row\|pub fn load_rows\|pub fn run(\|pub fn load_study" -A 4 \
    windows/lab/src/runner.rs windows/lab/src/study.rs windows/lab/src/lib.rs
grep -n "pub struct Row" -A 12 windows/lab/src/*.rs
head -3 book/src/laboratory/generated/the-census/rows.csv | cut -c1-200
```

Confirm the `Row` field names and the CSV's first three seeds before writing
the test. **Do not write the assertion against remembered field names.**

- [ ] **Step 2: Write the failing test**

Create `windows/lab/tests/census_sentinel.rs`:

```rust
//! The census sentinel (The Sexton, Task 7).
//!
//! Runs the full metric roster over the census's first three seeds and compares
//! against the committed `rows.csv` — so census drift reddens at the commit
//! that caused it rather than five hours into a campaign close.
//!
//! THE GOLDEN IS THE CENSUS'S OWN COMMITTED ROWS, not a separate fixture. A
//! second copy of the same values would be a second source of truth and a new
//! drift surface; reading the census's prefix makes this automatically correct
//! the moment a census lands.
//!
//! SECOND JOB, AND IT IS THE LARGER ONE. Decision 0090 audited cross-host
//! reproducibility once, over 40 worlds, and found it clean. This makes that
//! audit CONTINUOUS, over metrics that did not exist when it ran. A metric that
//! is host-divergent reddens here — which is precisely the failure decision
//! 0079 feared and could not detect.
//!
//! DIRECTION THIS CHECK ENFORCES: the first three census rows, recomputed here,
//! equal the committed ones. It says nothing about the other 997.
//!
//! DELIBERATELY CALLS `run` RATHER THAN THE CLI PATH. `cmd_lab_run` takes the
//! census claim (decision 0081) and enforces the canonical host (0079); this
//! test does neither, on purpose. A three-world run must not claim the box, and
//! the host guard governs WRITES — this only reads. Do not "fix" the missing
//! guard: it is what makes a Mac-side sentinel possible at all.

use std::path::Path;

/// Metric names allowed to disagree with the committed census, each with a
/// reason. A reasonless entry is a parse error — the same ratchet
/// `tropes check` and seam-guard use, because a waiver nobody has to justify
/// becomes a place to hide a real divergence.
fn waivers() -> Vec<(String, String)> {
    include_str!("fixtures/sentinel-waivers.txt")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| {
            let (name, reason) = l.split_once(':').unwrap_or_else(|| {
                panic!(
                    "sentinel-waivers.txt: '{l}' has no ':' — every waiver must \
                     carry a reason, because a reasonless waiver is where a real \
                     cross-host divergence would hide"
                )
            });
            assert!(
                !reason.trim().is_empty(),
                "sentinel-waivers.txt: '{name}' has an empty reason"
            );
            (name.trim().to_string(), reason.trim().to_string())
        })
        .collect()
}

#[test]
fn the_first_three_census_worlds_match_the_committed_rows() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("windows/lab always has a grandparent");

    let study = hornvale_lab::load_study(&root.join("studies/the-census.study.json"))
        .expect("the census study must load");

    // Narrow the study to its first three seeds. Everything else — the pin set
    // and the `"all"` metric roster — is inherited unchanged, so this measures
    // exactly what the census measures.
    let mut sentinel = study.clone();
    sentinel.seeds.count = 3;

    let live = hornvale_lab::run(&sentinel).expect("the sentinel study must run");

    let csv = std::fs::read_to_string(
        root.join(hornvale_lab::CENSUS_GOLDENS_DIR)
            .join("the-census/rows.csv"),
    )
    .expect("the committed census rows must exist");
    let committed = hornvale_lab::load_rows(&sentinel, &csv)
        .expect("the committed rows must parse against the census study");

    let waived: Vec<String> = waivers().into_iter().map(|(n, _)| n).collect();
    let mut moved: Vec<String> = Vec::new();

    for (live_row, want_row) in live.rows.iter().zip(committed.rows.iter()) {
        let got = hornvale_lab::canonical_row(live_row);
        for ((name, g), w) in live
            .metric_names
            .iter()
            .zip(got.values.iter())
            .zip(want_row.values.iter())
        {
            if g != w && !waived.iter().any(|x| x == name) {
                moved.push(format!("seed {} · {name}: live {g:?} vs committed {w:?}", got.seed));
            }
        }
    }

    assert!(
        moved.is_empty(),
        "the census sentinel disagrees with the committed rows.csv. Either this \
         change moved the census (refresh it on lefford and commit the goldens), \
         or a metric is host-divergent (add it to \
         windows/lab/tests/fixtures/sentinel-waivers.txt WITH A REASON, and open \
         a follow-up — decision 0079's failure is exactly this, undetected):\n  {}",
        moved.join("\n  ")
    );
}
```

- [ ] **Step 3: Create the waiver fixture, empty**

Create `windows/lab/tests/fixtures/sentinel-waivers.txt`:

```
# Metrics allowed to disagree between this host and the committed,
# lefford-authored census. Format: `metric-name: reason`. A line without a
# ':' is a parse error, deliberately — see the module doc of
# ../census_sentinel.rs.
#
# EMPTY IS THE EXPECTED STATE. Decision 0090's audit found cross-host
# reproducibility clean after the codegen baseline pin at 3a7092c3. An entry
# here is evidence that finding has decayed for some metric, and wants a
# follow-up, not a shrug.
```

- [ ] **Step 4: Run it, and prove it discriminates**

```bash
cd "$(git rev-parse --show-toplevel)"
cargo test -p hornvale-lab --test census_sentinel -- --nocapture
```
Expected: PASS.

Now disarm — a check only ever seen to pass is not known to be a check:
```bash
cp book/src/laboratory/generated/the-census/rows.csv /tmp/hv-rows-backup.csv
python3 - <<'PY'
p='book/src/laboratory/generated/the-census/rows.csv'
lines=open(p).read().split('\n')
f=lines[1].split(',')
f[2] = '999999' if f[2] != '999999' else '888888'
lines[1]=','.join(f)
open(p,'w').write('\n'.join(lines))
PY
cargo test -p hornvale-lab --test census_sentinel
cp /tmp/hv-rows-backup.csv book/src/laboratory/generated/the-census/rows.csv
git diff --exit-code book/src/laboratory/generated/the-census/rows.csv
```
Expected: the test FAILS naming the perturbed metric, then the file restores
byte-identically.

- [ ] **Step 5: Time it against the gate**

```bash
cd "$(git rev-parse --show-toplevel)"
hostname -s
/usr/bin/time -p cargo test -p hornvale-lab --test census_sentinel 2>&1 | tail -3
```
Expected: `user` around 90–100 CPU-s. If the wall-clock addition to `make gate`
on this host exceeds ~10 s (2% of the 489 s floor), reduce `sentinel.seeds.count`
to 1 and record the change and its reason in the campaign ledger.

- [ ] **Step 6: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
cargo fmt
cat > /tmp/hv-t7.txt <<'EOF'
feat(lab): a three-world census sentinel inside the commit gate

The last two census refreshes moved 3 of 205 columns each and the second cost
5 h 20 m — a drift discovered at campaign close, hours after the commit that
caused it. Measured all-metric cost is 31.34 CPU-s/world, so three worlds is
~94 CPU-s, about 1.6% of ambrose's 489 s gate floor.

The golden is the census's OWN committed rows.csv prefix, not a new fixture: a
second copy would be a second source of truth and a new drift surface, and
reading the prefix makes the sentinel automatically correct the moment a
census lands.

Second job, and it is the larger one: decision 0090 audited cross-host
reproducibility once, over 40 worlds. This makes that audit continuous, over
metrics that did not exist when it ran. A host-divergent metric reddens here —
precisely the failure decision 0079 feared and could not detect. The waiver
fixture requires a reason per entry (reasonless is a parse error), on the
tropes-check ratchet pattern, so a divergence cannot be quietly absorbed.

Disarmed to prove it discriminates: perturbing one committed value reddens it.
EOF
git add windows/lab/tests/census_sentinel.rs windows/lab/tests/fixtures/sentinel-waivers.txt
git commit -F /tmp/hv-t7.txt
```

---

## Task 8: A scheduler on lefford

**Why:** decision 0125 deleted `.github/workflows/` because runner minutes are
metered — but compute was never the scarce resource; lefford is idle 97% of a
744-hour window. What was lost was *something remembering to run things*, and
the replacement has been six paragraphs of CLAUDE.md asking humans to remember.

**Files:**
- Create: `scripts/scheduled/nightly-drift.sh`
- Create: `scripts/scheduled/README.md`
- Create: `scripts/scheduled/systemd/hornvale-nightly.service`
- Create: `scripts/scheduled/systemd/hornvale-nightly.timer`

**Interfaces:**
- Consumes: `make rebaseline`, `make board-post`, `make board-sync`,
  `docs/generated-paths.txt` (Task 1).
- Produces: the `scripts/scheduled/` convention Task 9 extends.

- [ ] **Step 1: Write the nightly drift sweep**

Create `scripts/scheduled/nightly-drift.sh`:

```bash
#!/usr/bin/env bash
# scripts/scheduled/nightly-drift.sh — the drift check, unattended.
#
# THE CONSTITUTIONAL CONSTRAINT OF THIS DIRECTORY: a scheduled job NEVER
# commits and NEVER touches main's working tree. It reports. The precedent is
# decision 0129's lane rule — "the lane must never be wired to auto-implement a
# suggestion... that would make the board self-modifying with no human in the
# loop" — and the concrete hazard is a nightly job committing while a session
# is mid-landing, which `make preflight` warns about and cannot prevent.
#
# Wall-clock time is used freely here. The determinism ban governs the SIM;
# scheduling is outside that boundary, exactly as clients/ is.
set -uo pipefail

REPO="${HV_SCHED_REPO:?HV_SCHED_REPO must name a checkout this job owns}"
cd "$REPO" || exit 1

git fetch origin --quiet || { echo "nightly-drift: fetch failed" >&2; exit 1; }
git reset --hard origin/main --quiet || exit 1

make rebaseline >/tmp/hv-nightly-drift.log 2>&1
rc=$?

paths="$(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | tr '\n' ' ')"
# shellcheck disable=SC2086
drift="$(git diff --stat -- $paths)"

# Leave the checkout clean regardless — this job owns no changes.
git checkout -- . --quiet 2>/dev/null || true

if [ "$rc" -ne 0 ]; then
    make board-post KIND=technique BY=scheduler PATHS='scripts/' \
      NOTE="nightly-drift: make rebaseline FAILED on main (rc=$rc). Tail: $(tail -5 /tmp/hv-nightly-drift.log | tr '\n' ' ' | tr -d '"')" || true
elif [ -n "$drift" ]; then
    make board-post KIND=notice BY=scheduler FIELDS='polarity=fyi' PATHS='book/ docs/audits/ docs/digest/' \
      NOTE="nightly-drift: main has UNCOMMITTED generated-artifact drift. Someone merged without running make rebaseline. $(echo "$drift" | tr '\n' ' ')" || true
fi

make board-sync >/dev/null 2>&1 || true
exit 0
```

```bash
chmod +x scripts/scheduled/nightly-drift.sh
shellcheck scripts/scheduled/nightly-drift.sh
```

- [ ] **Step 2: Write the systemd units**

Create `scripts/scheduled/systemd/hornvale-nightly.service`:

```ini
[Unit]
Description=Hornvale nightly unattended checks (The Sexton)
Documentation=file:///home/%i/Projects/hornvale/scripts/scheduled/README.md

[Service]
Type=oneshot
WorkingDirectory=%h/Projects/hornvale-scheduled
Environment=HV_SCHED_REPO=%h/Projects/hornvale-scheduled
ExecStart=%h/Projects/hornvale-scheduled/scripts/scheduled/nightly-drift.sh
# The census ExecStart is added by Task 9, which creates the script it names.
# Shipping both here would make this oneshot unit fail wholesale for anyone
# who installs it between Task 8 and Task 9 (pre-flight Ruling 3).
# A failed run must be READABLE, which is why this is a timer and not cron:
#   journalctl --user -u hornvale-nightly.service -n 200
StandardOutput=journal
StandardError=journal
```

Create `scripts/scheduled/systemd/hornvale-nightly.timer`:

```ini
[Unit]
Description=Run Hornvale's nightly checks

[Timer]
OnCalendar=*-*-* 03:30:00
# Survive a reboot: a missed run fires on next boot rather than being skipped.
Persistent=true
RandomizedDelaySec=600

[Install]
WantedBy=timers.target
```

- [ ] **Step 3: Write the README, including the separate-checkout rule**

Create `scripts/scheduled/README.md`:

```markdown
# Scheduled jobs

Unattended checks on `lefford`, restoring the **scheduling** function decision
0125 deleted along with CI. Compute was never the scarce resource — lefford ran
22.9 h of measured work in a 744-hour window, 3.1% utilisation — but nothing
remembered to run things, and CLAUDE.md accumulated six separate paragraphs
asking humans to remember instead.

## The two rules

1. **A scheduled job never commits and never touches `main`'s working tree.**
   It reports to the board. Precedent: decision 0129's lane rule. The hazard is
   a nightly job committing while a session is mid-landing, which
   `make preflight` warns about and cannot prevent.
2. **Jobs run in a checkout they own** — `~/Projects/hornvale-scheduled`, not
   the shared `~/Projects/hornvale`. A `git reset --hard` in a checkout someone
   else is using destroys their work. This is not hypothetical: CLAUDE.md
   already warns that lefford's regeneration worktree is shared and must have
   its HEAD verified before reuse.

## Install (on lefford, once)

```bash
git clone <origin> ~/Projects/hornvale-scheduled
mkdir -p ~/.config/systemd/user
cp ~/Projects/hornvale-scheduled/scripts/scheduled/systemd/* ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now hornvale-nightly.timer
loginctl enable-linger "$USER"   # so user timers run without an active session
```

## Read it

```bash
systemctl --user list-timers hornvale-nightly.timer
journalctl --user -u hornvale-nightly.service -n 200
systemctl --user start hornvale-nightly.service   # run it now, out of band
```
```

- [ ] **Step 4: Verify the never-commits property by test, not by reading**

The one property that must not be assumed:

```bash
cd "$(git rev-parse --show-toplevel)"
grep -nE '\bgit (commit|push|merge|rebase)\b' scripts/scheduled/*.sh \
  && echo "VIOLATION: a scheduled job mutates history" \
  || echo "NO COMMIT/PUSH IN ANY SCHEDULED JOB (correct)"
```
Expected: `NO COMMIT/PUSH IN ANY SCHEDULED JOB (correct)`.

Then dry-run against a throwaway clone, never the shared checkout:
```bash
tmp=$(mktemp -d)
git clone --quiet "$(git rev-parse --show-toplevel)" "$tmp/hv"
HV_SCHED_REPO="$tmp/hv" bash scripts/scheduled/nightly-drift.sh; echo "exit=$?"
git -C "$tmp/hv" status --porcelain | head
rm -rf "$tmp"
```
Expected: `exit=0` and a clean status — the job leaves no changes behind.

- [ ] **Step 5: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
cat > /tmp/hv-t8.txt <<'EOF'
feat(scheduled): a scheduler on lefford — what decision 0125 actually removed

0125 deleted .github/workflows/ because runner minutes are metered on a
private repo. But compute was never the scarce resource: lefford ran 22.9 h of
measured work in a 744-hour window, 3.1% utilisation, while 50 h of waiting
happened on 10-12 core laptops. What was lost was something REMEMBERING to run
things, and the replacement has been six paragraphs of CLAUDE.md asking humans
to remember.

systemd timers, not cron: journalctl makes a failed run readable and
Persistent=true survives a reboot. A nightly job whose failure is invisible
would reproduce the exact pathology this campaign exists to fix.

Two rules, both enforced rather than described. A scheduled job never commits
and never touches main — grep-verified across the directory, on decision
0129's lane precedent. And jobs run in a checkout they own, because a git
reset --hard in a shared checkout destroys someone's work; CLAUDE.md already
warns about exactly that for lefford's regeneration worktree.

Dry-run against a throwaway clone leaves it clean.
EOF
git add scripts/scheduled/
git commit -F /tmp/hv-t8.txt
```

---

## Task 9: The nightly census

**Why:** the census is the one job in the repo with no reason to block anybody,
and it currently blocks a human at campaign close for up to 5 h 20 m.

**Files:**
- Create: `scripts/scheduled/nightly-census.sh`
- Modify: `CLAUDE.md` — the census block's "refreshed once per campaign" prose

**Interfaces:**
- Consumes: `scripts/census-run.sh` (host-guarded, claim-serialised),
  `make lab-diff STUDY=the-census`, `make board-post`.
- Produces: a nightly board post carrying the census diff against the committed
  rows.

- [ ] **Step 1: Write the job**

Create `scripts/scheduled/nightly-census.sh`:

```bash
#!/usr/bin/env bash
# scripts/scheduled/nightly-census.sh — the census, off the critical path.
#
# The Rill's refresh took 19,207 s (5 h 20 m) to move three of 205 columns, at
# campaign close, with a human waiting. Nothing about that run needed to be
# synchronous. This runs it overnight on the canonical box and POSTS THE DIFF;
# committing a moved column stays a deliberate human act (see this directory's
# README, rule 1).
set -uo pipefail

REPO="${HV_SCHED_REPO:?HV_SCHED_REPO must name a checkout this job owns}"
cd "$REPO" || exit 1

git fetch origin --quiet || exit 1
git reset --hard origin/main --quiet || exit 1
sha="$(git rev-parse HEAD)"

# census-run.sh enforces the canonical host (0079) and serialises on the box's
# claim (0081). If another heavy job holds it, that is not a failure — skip.
if ! bash scripts/census-run.sh status | grep -qi 'no .*run'; then
    echo "nightly-census: the box is held by another heavy run; skipping tonight" >&2
    exit 0
fi

HV_CENSUS_WORKTREE=canonical HV_CENSUS_REF="$sha" bash scripts/census-run.sh \
    >/tmp/hv-nightly-census.log 2>&1
rc=$?

if [ "$rc" -ne 0 ]; then
    make board-post KIND=technique BY=scheduler PATHS='windows/lab/' \
      NOTE="nightly-census: census-run.sh FAILED on main at ${sha:0:8} (rc=$rc). Tail: $(tail -5 /tmp/hv-nightly-census.log | tr '\n' ' ' | tr -d '"')" || true
    exit 0
fi

diff_out="$(make lab-diff STUDY=the-census 2>/dev/null | head -40)"
if [ -n "$diff_out" ]; then
    make board-post KIND=notice BY=scheduler FIELDS='polarity=fyi' PATHS='book/src/laboratory/' \
      NOTE="nightly-census on main at ${sha:0:8}: COLUMNS MOVED. Refresh and commit on lefford before your close. $(echo "$diff_out" | tr '\n' ' ' | tr -d '"')" || true
fi

# Own no changes: the goldens this run wrote are a report, not a commit.
git checkout -- . --quiet 2>/dev/null || true
make board-sync >/dev/null 2>&1 || true
exit 0
```

```bash
chmod +x scripts/scheduled/nightly-census.sh
shellcheck scripts/scheduled/nightly-census.sh
```

- [ ] **Step 2: Verify the skip-when-held path**

The claim check is the one branch a dry run can exercise safely:

```bash
cd "$(git rev-parse --show-toplevel)"
bash scripts/census-run.sh status
grep -n "census-run.sh status" scripts/scheduled/nightly-census.sh
```

Confirm the exact wording `census-run.sh status` prints when the box is
**free**, and adjust the `grep -qi` pattern to match it. **Do not assume the
string** — read the script's own output. If the pattern is wrong in the
permissive direction the job will start a five-hour census on a contended box.

- [ ] **Step 3: Add the census ExecStart to the systemd unit**

Task 8 shipped `scripts/scheduled/systemd/hornvale-nightly.service` with the
drift `ExecStart` only, and a comment naming this step. Replace that comment
with the second line:

```ini
ExecStart=%h/Projects/hornvale-scheduled/scripts/scheduled/nightly-census.sh
```

Verify the unit parses before committing:

```bash
systemd-analyze verify scripts/scheduled/systemd/hornvale-nightly.service 2>&1 | head
```
On macOS `systemd-analyze` does not exist; that is expected — the check runs on
lefford at install time. Confirm by eye that both `ExecStart` lines name files
that exist in `scripts/scheduled/`.

- [ ] **Step 4: Update CLAUDE.md's census block**

Replace "Refreshed once per campaign at the pre-merge close" with:

```
# Refreshed NIGHTLY on lefford since The Sexton (scripts/scheduled/), which
# posts the diff to the board and commits nothing. A campaign close now READS
# last night's result instead of waiting for a run: if the diff is empty you
# are done. Committing a moved column is still a deliberate human act on the
# canonical box. The Rill's refresh took 19,207 s to move three of 205 columns
# with a human waiting on it; nothing about that needed to be synchronous.
```

- [ ] **Step 5: Commit**

```bash
cd "$(git rev-parse --show-toplevel)"
cat > /tmp/hv-t9.txt <<'EOF'
feat(scheduled): the census runs nightly and blocks nobody

The Rill's refresh took 19,207 s — 5 h 20 m — to move three of 205 columns, at
campaign close, with a human waiting. Nothing about that run needed to be
synchronous; it is the one job in the repo with no reason to block anybody.

It now runs overnight on the canonical box and POSTS THE DIFF to the board. A
campaign close reads last night's result: empty diff, you are done. Committing
a moved column stays a deliberate human act on lefford — this directory's
rule 1, on decision 0129's lane precedent.

Skips rather than fails when another heavy run holds the box, so a contended
night is not a red alarm. The claim-status pattern was matched against
census-run.sh's actual output rather than an assumed string: wrong in the
permissive direction, this job would start a five-hour census on a contended
box.
EOF
git add scripts/scheduled/nightly-census.sh CLAUDE.md
git commit -F /tmp/hv-t9.txt
```

---

## Stage 3 boundary — and the campaign's mid-point evidence

- [ ] **Absorb, gate, and check every Stage 1–3 success criterion.**

```bash
cd "$(git rev-parse --show-toplevel)"
git fetch origin && make preflight
make gate
make rebaseline
git diff --exit-code $(grep -v '^#' docs/generated-paths.txt | tr '\n' ' ')
```

Then walk the spec's §4 criteria explicitly:

- [ ] (1) every command in Task 2's list has a `docs/timings.md` row —
      `for l in prewarm preflight gate-fast quick vessel-check world-check game-check; do echo -n "$l: "; grep -c "| $l |" docs/timings.md; done`
- [ ] (2) a red gate writes `docs/timings/defects-$(hostname -s).tsv`; a green
      one adds no row (proved in Task 4)
- [ ] (3) a new undeclared generated directory reddens the gate (proved in Task 1)
- [ ] (4) `make ci` and `make gate` have the same baseline effects (Task 3)
- [ ] (5) `make rebaseline` is byte-identical at `cpu_ratio > 4` (Task 5)
- [ ] (6) the sentinel's addition to `make gate` on this host is `≤ ~10 s`
- [ ] (7) the lefford timers run seven consecutive nights and report a
      deliberately introduced census drift with no human invocation and no
      commit — **this criterion takes a week and is checked at the Stage 4
      boundary, not here**

- [ ] **Post to the board** that Stages 1–3 have landed, naming the changed
      shared surfaces (`Makefile`, `scripts/`, `docs/timings*`, `windows/lab/`,
      `cli/tests/`) and the new `make worktree-take` convention.

- [ ] **Write the Stage 4 plan.** It reads the instruments this stage
      installed: S11's before-arm from `docs/timings.md`'s `gate` rows on a
      quiet box, S7's argument from the same, S12's degradation table from the
      committed n=1000 rows, and S18 from `defects-<host>.tsv`.

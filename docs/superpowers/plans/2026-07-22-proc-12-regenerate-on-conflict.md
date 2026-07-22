# PROC-12: Regenerate-on-Conflict for Generated Artifacts Implementation Plan

**Status: COMPLETE.** Both tasks executed, task-reviewed, and passed a
final whole-branch review (Ready to merge: Yes). Chronicled as
[The Standing Offer](../../../book/src/chronicle/the-standing-offer.md).

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn generated-artifact merge conflicts (the type-audit report, 5 reference-dump pages, and 2 append-only chronicle/registry lists) into no-ops instead of manual resolutions.

**Architecture:** Two git-native mechanisms dispatched per file via `.gitattributes`: a builtin `merge=union` for append-only lists, and a new custom merge driver (`scripts/merge-regenerate.sh`) that reruns the specific generator command for a file and discards both sides' text, registered via `git config` in the existing `make install-hooks` target.

**Tech Stack:** `git` merge-driver protocol (`gitattributes(5)`), bash, `cargo run`.

**Full spec:** `docs/superpowers/specs/2026-07-22-proc-12-regenerate-on-conflict-design.md`

## Global Constraints

- The merge driver script must NEVER write partial/broken output to its
  target on failure — it writes to a temp file first, and only copies
  that temp file over the real target once the whole generator command
  has succeeded.
- The driver's dispatch `case "$path" ... *)` fallback (no entry for this
  path) MUST exit 1 with a clear stderr message — never silently guess a
  command for an unrecognized path.
- A `.gitattributes`-referenced merge driver with no corresponding
  `git config merge.<name>.driver` registration safely falls back to
  git's normal 3-way text merge (git's own documented default). Nothing
  in this plan needs to implement that fallback; just don't break it.
- This campaign touches **no** Rust/domain/kernel code — `make gate`
  must simply stay exactly as green as it already is on `main` (a
  sanity confirmation, not a targeted new-code test).
- **Never** add a dispatch-table entry, `.gitattributes` line, or test
  scenario for: `book/src/laboratory/generated/*/rows.csv` or
  `schema.json` (census data — authorization-gated, excluded per spec
  §3.2), or `cli/tests/fixtures/world-seed-42.json`,
  `cli/tests/fixtures/pre-branches-seed-42-world.json`,
  `windows/scene/tests/fixtures/tiles-seed-1-w16.json` (golden/keystone
  fixtures — deliberately frozen, excluded per spec §3.2, the single
  most important exclusion in this campaign).
- Run `shellcheck` on every new/modified shell script before committing
  (per this project's own CLAUDE.md tooling convention).

---

## Task 1: `.gitattributes`, `scripts/merge-regenerate.sh`, `Makefile`

**Files:**
- Create: `.gitattributes` (does not exist yet in this repo)
- Create: `scripts/merge-regenerate.sh`
- Modify: `Makefile:126-128` (the `install-hooks` target)

**Interfaces:**
- Produces: `scripts/merge-regenerate.sh` invoked by git as
  `scripts/merge-regenerate.sh %O %A %B %P` (positional args: `$1`=
  ancestor temp file, unused; `$2`=`ours` temp file, the merge-driver's
  write target; `$3`=`theirs` temp file, unused; `$4`=the real
  repo-relative path being merged, used for dispatch). Task 2's
  integration test invokes this indirectly, through real `git merge`
  operations, once `make install-hooks` has registered it.

- [ ] **Step 1: Create `.gitattributes`**

```gitattributes
# Generated-artifact merge conflicts (PROC-12): a conflict on one of
# these files means two branches each hold a stale or independently
# regenerated copy, never a real semantic disagreement worth a human's
# time. See docs/superpowers/specs/2026-07-22-proc-12-regenerate-on-conflict-design.md.

# Tier A — append-only lists (git's builtin union driver; no setup step,
# no risk of dropping either side's addition, at most a cosmetic
# ordering difference between the two sides' new lines).
book/src/SUMMARY.md merge=union
book/src/frontier/idea-registry.md merge=union

# Tier B — fully re-derived documents (scripts/merge-regenerate.sh,
# registered via `make install-hooks`; safely falls back to a normal
# 3-way merge on any clone that hasn't run that target).
docs/audits/type-audit-report.md merge=hv-regenerate
book/src/reference/concept-registry-generated.md merge=hv-regenerate
book/src/reference/concept-manifest-generated.md merge=hv-regenerate
book/src/reference/stream-manifest-generated.md merge=hv-regenerate
book/src/reference/phonology.md merge=hv-regenerate
book/src/reference/proto-goblinoid-generated.md merge=hv-regenerate
```

- [ ] **Step 2: Create `scripts/merge-regenerate.sh`**

```bash
#!/usr/bin/env bash
# scripts/merge-regenerate.sh %O %A %B %P — a git merge driver (PROC-12).
#
# Regenerates a fully-re-derived documentation artifact instead of
# textually merging it: neither side's committed text is authoritative,
# only a fresh run of the one command that produces this file is.
#
# Registered via `make install-hooks` (git config merge.hv-regenerate.
# driver); referenced per-path in .gitattributes. A clone that hasn't
# run that target sees no change in behavior — git falls back to its
# normal 3-way merge for any .gitattributes-named driver with no
# corresponding `git config` entry (gitattributes(5)).
#
# Exits nonzero on ANY failure (unrecognized path, or the generator
# command itself failing — most likely because a *different*, still-
# unresolved conflict elsewhere in the same merge means the crate
# doesn't currently build) and writes nothing to $A in that case. Git's
# actual behavior for a custom `merge=<driver>`-attributed path on a
# nonzero exit (verified directly, not assumed from prose): the path is
# left marked UNMERGED in the index (three conflict stages — base/ours/
# theirs), with the working-tree file at its pre-merge "ours" content —
# NOT textual <<<<<<< diff3 markers, which only appear for git's
# default, undriven conflict handling. `git status`/`git ls-files -u`
# show the real conflict either way; this file is never silently left
# with partial/broken driver output, and the merge as a whole still
# fails, requiring a human to resolve it normally. Never trust $A's
# on-disk content after a nonzero exit.
set -euo pipefail

ours="$2"
path="$4"

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

tmp_out="$(mktemp)"
trap 'rm -f "$tmp_out"' EXIT

case "$path" in
    docs/audits/type-audit-report.md)
        cargo run --manifest-path tools/type-audit/Cargo.toml -- report > "$tmp_out"
        ;;
    book/src/reference/concept-registry-generated.md)
        cargo run -q -p hornvale -- concepts > "$tmp_out"
        ;;
    book/src/reference/concept-manifest-generated.md)
        cargo run -q -p hornvale -- concepts --manifest > "$tmp_out"
        ;;
    book/src/reference/stream-manifest-generated.md)
        cargo run -q -p hornvale -- streams > "$tmp_out"
        ;;
    book/src/reference/phonology.md)
        cargo run -q -p hornvale -- phonology > "$tmp_out"
        ;;
    book/src/reference/proto-goblinoid-generated.md)
        cargo run -q -p hornvale -- proto > "$tmp_out"
        ;;
    *)
        echo "merge-regenerate: no dispatch entry for '$path'" >&2
        exit 1
        ;;
esac

cp "$tmp_out" "$ours"
```

Make it executable:

```bash
chmod +x scripts/merge-regenerate.sh
```

- [ ] **Step 3: Run shellcheck**

Run: `shellcheck scripts/merge-regenerate.sh`
Expected: no warnings. If shellcheck flags anything (e.g. `$path`/`$ours`
quoting), fix it and re-run until clean.

- [ ] **Step 4: Modify the `install-hooks` target in `Makefile`**

Current (lines 126-128):

```makefile
install-hooks: ## Point git at scripts/hooks (runs `make quick` + the golden-pins.sql tripwire guard pre-commit)
	git config core.hooksPath scripts/hooks
	@echo "git hooks path set to scripts/hooks; 'make quick' now runs pre-commit."
```

Replace with:

```makefile
install-hooks: ## Point git at scripts/hooks + register the regenerate-on-conflict merge driver (PROC-12)
	git config core.hooksPath scripts/hooks
	git config merge.hv-regenerate.driver 'scripts/merge-regenerate.sh %O %A %B %P'
	@echo "git hooks path set to scripts/hooks; 'make quick' now runs pre-commit."
	@echo "merge.hv-regenerate driver registered for generated-artifact conflicts."
```

- [ ] **Step 5: Manually verify registration**

```bash
make install-hooks
git config --get merge.hv-regenerate.driver
```

Expected output of the second command: `scripts/merge-regenerate.sh %O %A %B %P`

- [ ] **Step 6: Manually verify one Tier B file resolves a real conflict**

```bash
cd /tmp && rm -rf proc12-manual-check && mkdir proc12-manual-check && cd proc12-manual-check
git clone /Users/nathan/.config/superpowers/worktrees/hornvale/proc-12 . -q
make install-hooks

git checkout -b branch-a -q
echo "stray line from branch-a" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray line on branch-a"

git checkout main -q
git checkout -b branch-b -q
echo "different stray line from branch-b" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray line on branch-b"

git merge branch-a -q
```

Expected: the merge succeeds with no conflict (git prints a normal merge
commit message, not `CONFLICT`). Then verify the resolved file is a
genuine fresh regeneration, not either side's stray line:

```bash
grep -c "stray line" book/src/reference/phonology.md
```

Expected: `0` — neither side's stray line survived; the file was
regenerated fresh by the driver, proving the mechanism resolved the
conflict by running the generator, not by picking a side.

Clean up:

```bash
cd / && rm -rf /tmp/proc12-manual-check
```

- [ ] **Step 7: Commit**

```bash
cd /Users/nathan/.config/superpowers/worktrees/hornvale/proc-12
git add .gitattributes scripts/merge-regenerate.sh Makefile
git commit -m "feat(git): regenerate-on-conflict merge driver for generated artifacts (PROC-12)

Tier A (SUMMARY.md, idea-registry.md) gets git's builtin merge=union.
Tier B (type-audit-report.md + 5 reference-dump pages) gets a new
custom driver, scripts/merge-regenerate.sh, that reruns the specific
generator command for a path instead of textually merging it.
Registered via make install-hooks. Census data and golden/keystone test
fixtures are deliberately excluded (see spec section 3.2) — the former
is authorization-gated, the latter must stay real conflicts by design."
```

---

## Task 2: Integration test — `scripts/test-merge-regenerate.sh`

**Files:**
- Create: `scripts/test-merge-regenerate.sh`

**Interfaces:**
- Consumes: `scripts/merge-regenerate.sh` and the `Makefile`'s
  `install-hooks` target (Task 1).
- Produces: nothing consumed by later tasks — this is the campaign's
  final deliverable, the automated proof the mechanism works.

This test creates and destroys its own throwaway clone under a temp
directory (never touches the real worktree's branches), so it can run
repeatedly without leaving stray state, and cleans up on both success
and failure via a `trap`.

- [ ] **Step 1: Write `scripts/test-merge-regenerate.sh`**

```bash
#!/usr/bin/env bash
# scripts/test-merge-regenerate.sh — integration test for the
# regenerate-on-conflict merge driver (PROC-12). Not part of `make gate`
# (this campaign adds no Rust code); run manually or wire into `make
# quick` at a later campaign's discretion. Exercises three scenarios in
# a throwaway clone: Tier B success (regenerates past a real conflict),
# Tier B failure (falls back to real conflict markers when the crate
# doesn't build), and Tier A (union keeps both sides' additions).
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
work="$(mktemp -d "${TMPDIR:-/tmp}/hv-merge-regen-test.XXXXXX")"
trap 'rm -rf "$work"' EXIT

pass() { echo "PASS: $1"; }
fail() { echo "FAIL: $1" >&2; exit 1; }

clone="$work/clone"
git clone -q "$repo_root" "$clone"
cd "$clone"
git config merge.hv-regenerate.driver 'scripts/merge-regenerate.sh %O %A %B %P'

base_branch="$(git branch --show-current)"

# --- Scenario 1: Tier B success path ---
echo "--- Scenario 1: Tier B regenerates past a real conflict ---"

git checkout -b scenario1-a -q
echo "stray-a" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray-a"

git checkout "$base_branch" -q
git checkout -b scenario1-b -q
echo "stray-b" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray-b"

if ! git merge scenario1-a -q; then
    fail "scenario 1: expected the merge to succeed via the driver, but it conflicted"
fi

if grep -q "^<<<<<<<\|^=======\|^>>>>>>>" book/src/reference/phonology.md; then
    fail "scenario 1: conflict markers left in phonology.md"
fi
if grep -q "stray-a\|stray-b" book/src/reference/phonology.md; then
    fail "scenario 1: a stray line survived — the driver picked a side instead of regenerating"
fi

fresh="$work/fresh-phonology.md"
cargo run -q -p hornvale -- phonology > "$fresh"
if ! diff -q "$fresh" book/src/reference/phonology.md > /dev/null; then
    fail "scenario 1: resolved phonology.md does not byte-match a fresh regeneration"
fi
pass "scenario 1: Tier B conflict resolved by regeneration, byte-matches fresh output"

# --- Scenario 2: Tier B failure path falls back to real conflict markers ---
echo "--- Scenario 2: Tier B falls back to conflict markers when the crate can't build ---"

git checkout "$base_branch" -q
git checkout -b scenario2-a -q
echo "fn this_does_not_compile_a( {" >> kernel/src/lib.rs
git add kernel/src/lib.rs
git commit -q -m "test: break the build on scenario2-a"
echo "stray-a2" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray-a2 alongside the broken build"

git checkout "$base_branch" -q
git checkout -b scenario2-b -q
# Both branches must touch kernel/src/lib.rs, not just scenario2-a: a
# one-sided change auto-merges cleanly (git takes the only side that
# changed it), so the file would sit on disk in its OLD, still-buildable
# form at the moment the driver runs for phonology.md's conflict --
# the build check would then spuriously succeed. Touching it on BOTH
# sides forces a genuine two-sided conflict, so git deposits real
# <<<<<<< markers into kernel/src/lib.rs immediately as part of its own
# standard (undriven) conflict handling -- invalid Rust syntax on disk
# before any driver for any OTHER file ever runs, guaranteeing the build
# is actually broken when merge-regenerate.sh's dispatch tries it.
echo "fn this_does_not_compile_b( {" >> kernel/src/lib.rs
git add kernel/src/lib.rs
git commit -q -m "test: also modify kernel/src/lib.rs on scenario2-b (forces a genuine two-sided conflict)"
echo "stray-b2" >> book/src/reference/phonology.md
git add book/src/reference/phonology.md
git commit -q -m "test: stray-b2"

if git merge scenario2-a -q 2>/dev/null; then
    fail "scenario 2: expected the merge to report a conflict (build broken), but it succeeded"
fi

# A custom `merge=<driver>`-attributed path does NOT get textual
# <<<<<<< diff3 markers on driver failure (that's only git's default,
# undriven conflict handling) -- verified directly against real git
# behavior, not assumed from documentation prose: git leaves the path
# marked unmerged in the index (three conflict stages: base/ours/
# theirs) with the working-tree file at its pre-merge "ours" content.
# `git ls-files -u` is the correct signal to check.
if ! git ls-files -u -- book/src/reference/phonology.md | grep -q .; then
    fail "scenario 2: expected phonology.md to be left unmerged (conflict stages present) when the driver can't build the crate, found none"
fi
if [ ! -s book/src/reference/phonology.md ]; then
    fail "scenario 2: phonology.md is empty — the driver must never leave partial/empty output on failure"
fi
git merge --abort
pass "scenario 2: build failure correctly falls back to real conflict markers, no partial output"

# --- Scenario 3: Tier A union keeps both sides' additions ---
echo "--- Scenario 3: Tier A union merge keeps both sides' SUMMARY.md bullets ---"

git checkout "$base_branch" -q
git checkout -b scenario3-a -q
printf -- '- [Scenario Three A](./chronicle/scenario-three-a.md)\n' >> book/src/SUMMARY.md
git add book/src/SUMMARY.md
git commit -q -m "test: scenario3-a SUMMARY bullet"

git checkout "$base_branch" -q
git checkout -b scenario3-b -q
printf -- '- [Scenario Three B](./chronicle/scenario-three-b.md)\n' >> book/src/SUMMARY.md
git add book/src/SUMMARY.md
git commit -q -m "test: scenario3-b SUMMARY bullet"

if ! git merge scenario3-a -q; then
    fail "scenario 3: expected the union merge to succeed, but it conflicted"
fi
if ! grep -q "Scenario Three A" book/src/SUMMARY.md; then
    fail "scenario 3: branch-a's bullet is missing after the union merge"
fi
if ! grep -q "Scenario Three B" book/src/SUMMARY.md; then
    fail "scenario 3: branch-b's bullet is missing after the union merge"
fi
pass "scenario 3: union merge kept both sides' SUMMARY.md bullets"

echo "All scenarios passed."
```

Make it executable:

```bash
chmod +x scripts/test-merge-regenerate.sh
```

- [ ] **Step 2: Run shellcheck**

Run: `shellcheck scripts/test-merge-regenerate.sh`
Expected: no warnings. Fix and re-run until clean.

- [ ] **Step 3: Run the test**

Run: `bash scripts/test-merge-regenerate.sh`
Expected: all three `PASS:` lines printed, then `All scenarios passed.`,
exit code 0. This clones the real repo into a `mktemp -d` directory and
tears it down via `trap ... EXIT`, so it never touches the real
worktree's own branches and leaves nothing behind whether it passes or
fails.

- [ ] **Step 4: Confirm the real worktree is untouched**

```bash
cd /Users/nathan/.config/superpowers/worktrees/hornvale/proc-12
git status --short
git branch
```

Expected: clean working tree, and no `scenario1-*`/`scenario2-*`/
`scenario3-*` branches present (the test operated entirely inside its
own throwaway clone under `mktemp -d`, never on this checkout).

- [ ] **Step 5: Full gate sanity check**

This campaign added no Rust code, so this is a pure sanity confirmation
that nothing else broke:

```bash
make gate
```

Expected: exit 0, same pass count as `main` already has (no new
tests were added by this campaign; this just confirms the `.gitattributes`/
`Makefile`/shell-script changes didn't disturb anything).

- [ ] **Step 6: Commit**

```bash
git add scripts/test-merge-regenerate.sh
git commit -m "test(git): integration test for the regenerate-on-conflict driver (PROC-12)

Exercises all three scenarios end to end in a throwaway clone: Tier B
success (regenerates past a real conflict, byte-matches fresh output),
Tier B failure (falls back to real conflict markers when the crate
can't build, never leaves partial/empty output), and Tier A (union
keeps both sides' SUMMARY.md additions)."
```

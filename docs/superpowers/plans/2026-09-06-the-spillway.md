# The Spillway Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A queued census refresh that grows the metric registry delivers its goldens on a branch without a human breaking a deadlock by hand.

**Architecture:** Three shell seams change and nothing in Rust behaves differently. `scripts/sluice-census.sh` re-authors the Gnomon injection arms at the census's ref, under the box lock, before its delivery commit; `scripts/gnomon-injection.sh` narrows its clean-tree guard to the paths a build can see and gains a `check` pre-flight; `scripts/subfloor-roster.sh` can omit one roster term so `pre-commit` can stand the column-count witness down for the delivery commit alone. Every change is pinned by a shell test the `outboard` set already runs.

**Tech Stack:** bash (3.2-compatible: no `mapfile`, no GNU-only `sed`), git, `flock(1)` (lefford only), the existing shell test harnesses, `shellcheck`.

**Spec:** `docs/superpowers/specs/2026-09-06-the-spillway-design.md` — read §1 (the three legs), §3.1 (the rule), §3.2–3.4 (each seam) before the task that touches it.

**Ledger:** `docs/superpowers/ledgers/2026-09-06-the-spillway.md` — every task ends with a `## Task N — complete` section there (rulings, deferred minors, what was rejected), committed with the task. Task state and fix rounds go to `.superpowers/sdd/the-spillway/progress.md` (scratch), never to the ledger.

## Global Constraints

- **Every commit passes the commit hook.** A commit touching `.rs` runs `make gate-commit` (minutes); a docs-only commit runs `make docs-tests`. Never `--no-verify`.
- **Commit trailer**, verbatim, on every commit: `Claude-Session: https://claude.ai/code/session_01DCmbLQnYU317DpApjFm1k5`.
- **Shell:** `make shellcheck` clean; bash 3.2 on macOS (`/bin/bash`); `set -uo pipefail` without `-e` in `sluice-census.sh` (its convention), `set -euo pipefail` in `gnomon-injection.sh` (its convention).
- **Shell tests run through `scripts/lane-outboard.sh`** — a test file nobody registers there is a test that does not exist. Run each test file directly while working; run `make shellcheck` before every commit that touches `scripts/`.
- **`docs/audits/campaign-reconciliation.tsv` is shared and append-only.** Every file under `docs/superpowers/specs`, `docs/superpowers/plans`, `book/src/chronicle`, `docs/retrospectives` must appear in exactly one row (`docs_consistency::campaign_reconciliation_covers_every_campaign_record`). The campaign's row is `the-spillway`; edit that row, never add a second.
- **Decision numbers:** the campaign's block is 0836–0845. A Rust comment may not cite a decision number until the record exists (`decision_cites_in_sources_resolve`); Task 5 mints 0836 before any comment cites it.
- **No new crates, no Rust behaviour change.** Rust edits in this plan are comments and one assertion message.
- **The canonical box is lefford.** Nothing in Tasks 1–5 needs it. Task 6 submits queue work from the Mac with `make sluice-stage` / `make sluice-census` / `make sluice`, always with a full 40-char SHA.

---

## File map

| file | task | responsibility |
| --- | --- | --- |
| `scripts/gnomon-injection.sh` | 1 | authoring script: `check` subcommand; guard narrowed to `. :!fixtures :!book :!docs` |
| `scripts/test-gnomon-injection.sh` (new) | 1 | drives `check` in a scratch repo; registered in `lane-outboard.sh` |
| `scripts/lane-outboard.sh` | 1 | one `run` line for the new test |
| `scripts/sluice-census.sh` | 2, 3 | library: `census_schema_columns`, `injection_arms_stale`; delivery: the arms step |
| `scripts/test-sluice-census.sh` | 2 | the two library functions over synthetic schemas |
| `scripts/test-sluice.sh` (census block) | 3 | stub `gnomon-injection.sh` in the stub worktree; five arms |
| `scripts/subfloor-roster.sh` | 4 | `HV_SUBFLOOR_EXCLUDE` omits matching roster lines |
| `scripts/hooks/pre-commit` | 4 | sets the exclusion inside its `HV_CENSUS_DELIVERY` gate-commit branch |
| `scripts/test-census-guard.sh` | 4 | two-way agreement test and the stand-down count pin |
| `docs/decisions/0836-*.md`, `docs/digest/decisions-in-force.md` | 5 | the rule (spec §3.1) as a record |
| `windows/lab/src/domesday/anomaly.rs`, `windows/lab/tests/suite/anomaly_injection.rs` | 5 | comments and one message |
| `windows/lab/tests/fixtures/injection/README.md`, `windows/lab/CLAUDE.md`, `scripts/CLAUDE.md`, `CLAUDE.md` | 5 | prose brought current |
| registry row, chronicle, retrospective, `SUMMARY.md`, reconciliation row | 6 | close |

---

### Task 1: `gnomon-injection.sh` — the `check` subcommand and the narrowed guard

**Files:**
- Modify: `scripts/gnomon-injection.sh` (the USAGE block in the header; the guards section that begins `# Guards`; the line `requested=("$@")`)
- Create: `scripts/test-gnomon-injection.sh`
- Modify: `scripts/lane-outboard.sh` (the `run` list, after `run "census guard"`)

**Interfaces:**
- Produces: `scripts/gnomon-injection.sh check` — exit 0 when the host guard and the tree guard both pass, exit 1 with the guard's own refusal text otherwise, exit 2 on extra arguments. Never builds, never mutates. Task 3 calls it before taking the lock.
- Produces: the tree guard's new predicate — `git status --porcelain -- . ":!$FIXTURES" ":!book" ":!docs"` must be empty.

- [ ] **Step 1: Write the failing test file**

Create `scripts/test-gnomon-injection.sh`:

```bash
#!/usr/bin/env bash
# scripts/test-gnomon-injection.sh — the authoring script's guards, driven
# without a build.
#
# WHY THIS TEST EXISTS. gnomon-injection.sh refused to run on ANY dirty tree
# outside its own fixture directory, and a census delivery's staged goldens are
# exactly that dirt — so a census that grew the registry could not re-author
# the arms its own gate compares against (The Warp, ledger #12; The Spillway
# spec §1, leg 2). The guard's stated job is narrower than its old predicate:
# it protects SOURCE it mutates and restores, and the manifest's `sha` claim
# about what was BUILT. book/ and docs/ are neither.
#
# DIRECTION THIS TEST ENFORCES: dirt under book/, docs/ and the fixture dir is
# allowed; dirt anywhere else still refuses. Both halves are asserted, because
# "book/ is allowed" alone would pass for a guard that had been deleted.
set -uo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
g() { env -u GIT_DIR -u GIT_INDEX_FILE -u GIT_WORK_TREE -u GIT_COMMON_DIR git -C "$tmp" "$@"; }

# A scratch repo carrying the script and the two files it sources. The script
# `cd`s to its own ../, so it runs against THIS repo, never the real one.
mkdir -p "$tmp/scripts" "$tmp/domains/terrain/src" "$tmp/book/src" "$tmp/docs" \
         "$tmp/windows/lab/tests/fixtures/injection/baseline-a"
cp "$root/scripts/gnomon-injection.sh" "$tmp/scripts/"
cp "$root/scripts/census-canonical-host.sh" "$tmp/scripts/"
cp "$root/scripts/census-canonical-host.txt" "$tmp/scripts/"
printf 'const X: f64 = 1.0;\n' > "$tmp/domains/terrain/src/strata.rs"
printf 'chapter\n' > "$tmp/book/src/x.md"
printf 'baseline\n' > "$tmp/docs/timings.md"
printf 'readme\n' > "$tmp/windows/lab/tests/fixtures/injection/README.md"
printf '{}\n' > "$tmp/windows/lab/tests/fixtures/injection/baseline-a/schema.json"
g init -q 2>/dev/null
g config user.name t; g config user.email t@t
g add -A; g commit -qm base

canonical="$(cat "$root/scripts/census-canonical-host.txt" | tr '[:upper:]' '[:lower:]')"
here="$(hostname -s 2>/dev/null || hostname)"; here="$(printf '%s' "$here" | tr '[:upper:]' '[:lower:]')"

# Runs `check` with the host guard lifted (PILOT) unless we ARE the canonical
# box, so every tree-guard arm below asserts the TREE verdict and nothing else.
check() {
    if [ "$here" = "$canonical" ]; then
        (cd "$tmp" && bash scripts/gnomon-injection.sh check 2>&1)
    else
        (cd "$tmp" && HV_GNOMON_PILOT=1 bash scripts/gnomon-injection.sh check 2>&1)
    fi
}
reset_tree() { g reset -q --hard; g clean -qfd; }

# --- the host guard, exercised for real ------------------------------------
out="$(cd "$tmp" && bash scripts/gnomon-injection.sh check 2>&1)"; rc=$?
if [ "$here" = "$canonical" ]; then
    if [ "$rc" -eq 0 ]; then ok "on the canonical box, check passes a clean tree without PILOT"
    else bad "on the canonical box, check refused a clean tree: $out"; fi
else
    if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -q 'REFUSING to author the battery'; then
        ok "off the canonical box, check refuses and names the host"
    else bad "off-host check gave rc=$rc without the host refusal: $out"; fi
fi

# --- the tree guard --------------------------------------------------------
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "clean tree: check passes"; else bad "clean tree refused: $out"; fi

printf 'edited\n' >> "$tmp/book/src/x.md"
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "a MODIFIED book/ file is allowed (the census's own output lives here)"
else bad "modified book/ refused — the delivery's goldens would deadlock again: $out"; fi

printf 'svg\n' > "$tmp/book/src/new.svg"
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "an UNTRACKED book/ file is allowed (a census adds new golden files)"
else bad "untracked book/ refused: $out"; fi

printf 'row\n' >> "$tmp/docs/timings.md"
g add -A -- book docs
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "STAGED book/ and docs/ dirt is allowed (a delivery stages before it re-authors)"
else bad "staged book/docs dirt refused: $out"; fi
reset_tree

printf 'dirt\n' > "$tmp/windows/lab/tests/fixtures/injection/baseline-a/rows.csv"
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "dirt inside the fixture directory is allowed (it is the script's own output)"
else bad "fixture-dir dirt refused: $out"; fi
reset_tree

# THE CONTROL: the guard still guards. Without these two, every arm above
# passes for a guard that has been deleted.
printf 'const X: f64 = 2.0;\n' > "$tmp/domains/terrain/src/strata.rs"
out="$(check)"; rc=$?
if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -q 'domains/terrain/src/strata.rs'; then
    ok "CONTROL: a modified source file refuses and is named"
else bad "modified source did NOT refuse (rc=$rc): $out"; fi
reset_tree

printf 'x\n' > "$tmp/scripts/new.sh"; g add scripts/new.sh
out="$(check)"; rc=$?
if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -q 'scripts/new.sh'; then
    ok "CONTROL: a staged new file outside book/docs refuses and is named"
else bad "staged new script did NOT refuse (rc=$rc): $out"; fi
reset_tree

out="$(cd "$tmp" && HV_GNOMON_PILOT=1 bash scripts/gnomon-injection.sh check extra 2>&1)"; rc=$?
if [ "$rc" -eq 2 ]; then ok "check with extra arguments is a usage error (rc=2)"
else bad "check with extra arguments gave rc=$rc: $out"; fi

# Anti-vacuity: `check` must never have built or authored anything.
if [ ! -f "$tmp/windows/lab/tests/fixtures/injection/manifest.json" ] && [ ! -d "$tmp/target" ]; then
    ok "check authored nothing and built nothing"
else bad "check left a manifest or a target/ behind — it is not a pre-flight"; fi

printf '\ntest-gnomon-injection: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
```

- [ ] **Step 2: Run it to verify it fails**

Run: `bash scripts/test-gnomon-injection.sh`
Expected: the tree-guard arms FAIL — today `check` is parsed as an ARM NAME ("unknown arm 'check'", exit 1) and `book/` dirt refuses. The "modified source refuses" control may pass already; that is fine. Exit non-zero.

- [ ] **Step 3: Add the `check` subcommand and narrow the guard**

In `scripts/gnomon-injection.sh`, in the USAGE block of the header, add a line after `#   scripts/gnomon-injection.sh baseline-a karst # only these arms (a pilot)`:

```bash
#   scripts/gnomon-injection.sh check            # run the guards, build nothing, exit 0/1
```

and add this paragraph to the header, after the `WHERE IT RUNS.` paragraph:

```bash
# WHAT THE TREE GUARD PROTECTS, EXACTLY (The Spillway). This script mutates
# tracked SOURCE and restores it with `git checkout --`, and stamps the
# manifest with `sha=$(git rev-parse HEAD)` as a claim about what was BUILT.
# Both are claims about source. So the guard refuses dirt anywhere a build or
# the mutation can see, and allows it under `book/` (the census's own output
# and the project book) and `docs/` (prose and ledgers), which the `hornvale`
# binary neither compiles nor reads on a `lab run`. It used to refuse the
# whole tree, and that is what deadlocked a census delivery: the staged
# goldens ARE dirt under book/, and the arms this script authors are what the
# delivery's own gate compares against them (The Warp, ledger #12; spec §1).
# `check` runs both guards and stops, so the delivery can ask before it waits
# for the box lock, and so this guard has a test that needs no build.
```

Replace the guards section. The current text from `# Guards` down to and including the `exit 1` / `fi` of the dirty-tree check becomes:

```bash
# ---------------------------------------------------------------------------
# Guards
# ---------------------------------------------------------------------------
mode="run"
if [ "${1:-}" = "check" ]; then
    mode="check"
    shift
    if [ "$#" -ne 0 ]; then
        echo "gnomon-injection: usage: gnomon-injection.sh check   (takes no arms)" >&2
        exit 2
    fi
fi

# shellcheck source=scripts/census-canonical-host.sh
. "$(dirname "$0")/census-canonical-host.sh"
here="$(hostname -s 2>/dev/null || hostname)"
if [ "$(printf '%s' "$here" | tr '[:upper:]' '[:lower:]')" \
     != "$(printf '%s' "$CANONICAL_CENSUS_HOST" | tr '[:upper:]' '[:lower:]')" ]; then
    if [ "${HV_GNOMON_PILOT:-}" != "1" ]; then
        cat >&2 <<EOF
gnomon-injection: REFUSING to author the battery on '$here'.

These fixtures are scored against census goldens authored on
'$CANONICAL_CENSUS_HOST' (decisions 0063/0079), and the boxes are not
byte-identical: ~0.1% of discrete-count metrics differ by one unit, decided
upstream of quantize-at-emit. Author them there:

  ssh $CANONICAL_CENSUS_HOST 'cd ~/Projects/hornvale && git fetch --all && \\
    git checkout <full-sha> && scripts/gnomon-injection.sh'

To validate the MACHINERY off-host, set HV_GNOMON_PILOT=1. The authoring host
is recorded per arm in the manifest, and anomaly_injection.rs reads it: a
battery with any off-host arm is a pilot and does not adjudicate H1.
EOF
        exit 1
    fi
    echo "gnomon-injection: PILOT run on '$here' (not '$CANONICAL_CENSUS_HOST') — these fixtures do not adjudicate H1." >&2
fi

# The tree must be clean everywhere a build or the mutation can see: this
# script edits TRACKED source in place, and a restore into a tree that
# already carried uncommitted edits to those files would silently discard
# them; and the manifest's `sha` is a claim about the source that was built.
#
# THREE trees are excluded, and each exclusion is what makes this a guard
# rather than a one-shot or a deadlock:
#   - the fixture directory: this script's own output, wiped and rebuilt on
#     every invocation, so an unconditional check refuses forever after the
#     first run;
#   - book/: the census's own output and the project book. A census delivery
#     runs this script with its goldens STAGED there (The Spillway); the
#     `hornvale` binary neither compiles nor reads book/ on a `lab run`;
#   - docs/: prose, timings, audits. Same argument.
# Anything else dirty — kernel/, domains/, windows/, cli/, studies/, scripts/,
# Cargo.* — refuses, and is named.
dirty="$(git status --porcelain -- . ":!$FIXTURES" ":!book" ":!docs")"
if [ -n "$dirty" ]; then
    echo "gnomon-injection: REFUSING to run with a dirty tree — this script rewrites" >&2
    echo "tracked source in place and restores it with 'git checkout --'; uncommitted" >&2
    echo "work in those files would be destroyed. Commit or stash first:" >&2
    printf '%s\n' "$dirty" >&2
    exit 1
fi

if [ "$mode" = "check" ]; then
    echo "gnomon-injection: check OK — host '$here', tree clean outside $FIXTURES, book/ and docs/" >&2
    exit 0
fi
```

Leave everything from `# Restore whatever is currently substituted` onward unchanged.

- [ ] **Step 4: Run the test to verify it passes, then shellcheck**

Run: `bash scripts/test-gnomon-injection.sh && make shellcheck`
Expected: every arm `ok`, `test-gnomon-injection: N passed, 0 failed`, shellcheck silent.

- [ ] **Step 5: Register the test and record the ledger section**

In `scripts/lane-outboard.sh`, after the line `run "census guard"    bash scripts/test-census-guard.sh`, add:

```bash
# The authoring script's guards, driven without a build. The guard used to
# refuse the whole tree, which deadlocked a census delivery against the arms
# its own gate reads (The Spillway); this pins the narrowed predicate in
# BOTH directions — book/docs dirt allowed, source dirt still refused.
run "gnomon injection" bash scripts/test-gnomon-injection.sh
```

Append to `docs/superpowers/ledgers/2026-09-06-the-spillway.md` a `## Task 1 — complete` section: what shipped, the test's arms, and any ruling made while implementing (e.g. if the host-guard arm needed a different shape than planned, say what and why). State explicitly whether an ideonomy pass was run (none is expected for a task with no design fork).

- [ ] **Step 6: Commit**

```bash
git add scripts/gnomon-injection.sh scripts/test-gnomon-injection.sh scripts/lane-outboard.sh docs/superpowers/ledgers/2026-09-06-the-spillway.md
git commit -m "feat(gnomon-injection): a check subcommand, and a tree guard narrowed to what it protects

The guard refused any dirt outside the fixture directory; a census
delivery's staged goldens are that dirt, so a census that grew the
registry could not re-author the arms its own gate compares against
(The Warp, ledger #12). The guard now allows book/ and docs/, which the
authoring binary neither compiles nor reads, and still refuses source.
check runs both guards and stops, so the delivery can ask before it
takes the box lock.

Claude-Session: https://claude.ai/code/session_01DCmbLQnYU317DpApjFm1k5"
```

---

### Task 2: `sluice-census.sh` library — column sets and stale arms

**Files:**
- Modify: `scripts/sluice-census.sh` (the library section: insert after `census_golden_count()` and before the `if [ -n "${HV_CENSUS_LIB:-}" ]` early return)
- Modify: `scripts/test-sluice-census.sh` (append arms before the final `printf`)

**Interfaces:**
- Produces: `census_schema_columns <schema.json>` — prints the column names of a lab `schema.json`, sorted, one per line: every `"name"` value at indent ≥ 4 (the study's own `"name"` sits at indent 2 and is excluded). Prints nothing for a missing file.
- Produces: `injection_arms_stale <worktree>` — prints one line per arm under `<worktree>/windows/lab/tests/fixtures/injection/*/schema.json` whose column set differs from `<worktree>/book/src/laboratory/generated/the-census/schema.json`; prints nothing when every arm matches, when there are no arms, or when the census schema is absent. Always exits 0; callers read stdout. Task 3 consumes both.

- [ ] **Step 1: Write the failing tests**

Append to `scripts/test-sluice-census.sh`, before the line `printf '\ntest-sluice-census: %d passed, %d failed\n' "$pass" "$fail"`:

```bash
# --- THE ARMS' COLUMN SET (The Spillway) ------------------------------------
# A census delivery re-authors the Gnomon arms when the world moved OR when an
# arm's columns differ from the census's. The second trigger is this pair of
# functions. Synthetic schemas in the serde pretty-print shape both real files
# have: the study's own "name" at indent 2, column names at indent 6.
if ! declare -f injection_arms_stale >/dev/null 2>&1; then
    bad "HV_CENSUS_LIB=1 did not expose injection_arms_stale"
else
    ok "injection_arms_stale is exposed by the library"
fi
arms="$tmp/windows/lab/tests/fixtures/injection"
mkdir -p "$arms/baseline-a" "$arms/karst"
write_schema() {  # $1 = path, $2 = study name, $3.. = column names
    local p="$1" study="$2"; shift 2
    {
        printf '{\n  "columns": [\n'
        local sep=""
        for c in "$@"; do
            printf '%s    {\n      "kind": "numeric",\n      "name": "%s"\n    }' "$sep" "$c"
            sep=$',\n'
        done
        printf '\n  ],\n  "name": "%s"\n}\n' "$study"
    } > "$p"
}
census="$tmp/book/src/laboratory/generated/the-census/schema.json"
write_schema "$census" the-census seed pin_set karst-fraction
write_schema "$arms/baseline-a/schema.json" gnomon-injection seed pin_set karst-fraction
write_schema "$arms/karst/schema.json"      gnomon-injection seed pin_set karst-fraction

cols="$(census_schema_columns "$census" | tr '\n' ' ')"
if [ "$cols" = "karst-fraction pin_set seed " ]; then
    ok "census_schema_columns lists the columns sorted and IGNORES the study's own name"
else
    bad "census_schema_columns gave '$cols'"
fi
if [ -z "$(injection_arms_stale "$tmp")" ]; then
    ok "matching arms whose STUDY NAME differs from the census's read as not stale"
else
    bad "arms with identical columns were called stale: $(injection_arms_stale "$tmp")"
fi

# The census gains a column (a campaign registered a metric).
write_schema "$census" the-census seed pin_set karst-fraction warp-lift
stale="$(injection_arms_stale "$tmp")"
if printf '%s\n' "$stale" | grep -q '^baseline-a: 1 column' && printf '%s\n' "$stale" | grep -q '^karst: 1 column'; then
    ok "a column the census has and the arms lack marks EVERY arm stale, by name"
else
    bad "census-gained-a-column: got '$stale'"
fi

# An arm carries a column the census lacks (authored against another registry).
write_schema "$census" the-census seed pin_set karst-fraction
write_schema "$arms/karst/schema.json" gnomon-injection seed pin_set karst-fraction ghost
stale="$(injection_arms_stale "$tmp")"
if [ "$(printf '%s\n' "$stale" | grep -c .)" -eq 1 ] && printf '%s\n' "$stale" | grep -q '^karst: 0 column(s) the census has and the arm lacks, 1 the arm has'; then
    ok "a column only an arm has marks THAT arm stale and no other"
else
    bad "arm-has-extra-column: got '$stale'"
fi

# No census schema at all: nothing to compare, nothing printed, exit 0.
rm -f "$census"
if [ -z "$(injection_arms_stale "$tmp")" ]; then
    ok "an absent census schema compares nothing (prints nothing, exits 0)"
else
    bad "absent census schema produced output: $(injection_arms_stale "$tmp")"
fi
rm -rf "$arms"
```

- [ ] **Step 2: Run to verify it fails**

Run: `bash scripts/test-sluice-census.sh`
Expected: `FAIL: HV_CENSUS_LIB=1 did not expose injection_arms_stale` and the arms after it fail or error (`command not found`); exit non-zero. The pre-existing arms still pass.

- [ ] **Step 3: Implement the two functions**

In `scripts/sluice-census.sh`, insert after the closing `}` of `census_golden_count` and before the `# shellcheck disable=SC2317  # the exit is the fallback…` line:

```bash
# THE ARMS' COLUMN SET (The Spillway, decision 0836). A census delivery
# re-authors the Gnomon injection arms when the world moved (goldens > 0) OR
# when an arm's column set differs from the census's — the second trigger
# catches arms left stale by an earlier epoch. The comparison is the same one
# `anomaly_injection::the_fixture_columns_match_the_census` makes, over the
# same two files, without a build.
#
# A lab schema.json is serde's pretty print: the study's own "name" sits at
# indent 2 and every column's "name" at indent 6 (verified against the
# committed census and baseline-a: 285 "name" keys each, 284 columns each,
# the odd one out being the study's). Names at indent >= 4 are columns.
# Both studies declare "metrics": "all", so at one ref the two sets are
# identical by construction; a difference means the arms were authored at
# another ref. A mis-parse here can only cause a NEEDLESS re-authoring,
# never a missed one: a real column difference is always a "name" line
# difference.
census_schema_columns() {
    local f="${1:?census_schema_columns <schema.json>}"
    [ -f "$f" ] || return 0
    grep -E '^ {4,}"name": "' "$f" | sed -E 's/^ *"name": "([^"]*)".*/\1/' | sort
}
injection_arms_stale() {
    local wt="${1:?injection_arms_stale <worktree>}"
    local census="$wt/book/src/laboratory/generated/the-census/schema.json"
    [ -f "$census" ] || return 0
    local want arm got missing extra name
    want="$(census_schema_columns "$census")"
    for arm in "$wt"/windows/lab/tests/fixtures/injection/*/schema.json; do
        [ -f "$arm" ] || continue
        got="$(census_schema_columns "$arm")"
        missing="$(comm -23 <(printf '%s\n' "$want") <(printf '%s\n' "$got") | grep -c . || true)"
        extra="$(comm -13 <(printf '%s\n' "$want") <(printf '%s\n' "$got") | grep -c . || true)"
        if [ "${missing:-0}" -ne 0 ] || [ "${extra:-0}" -ne 0 ]; then
            name="$(basename "$(dirname "$arm")")"
            printf '%s: %s column(s) the census has and the arm lacks, %s the arm has and the census lacks\n' \
                "$name" "$missing" "$extra"
        fi
    done
    return 0
}
```

- [ ] **Step 4: Run to verify it passes, then shellcheck**

Run: `bash scripts/test-sluice-census.sh && make shellcheck`
Expected: all arms `ok`, 0 failed; shellcheck silent. If shellcheck objects to `grep -c . || true` inside `$(...)`, keep the `|| true` (the count is 0 on empty input and `grep` exits 1 then) and add the directive it names.

- [ ] **Step 5: Ledger section and commit**

Append `## Task 2 — complete` to the ledger (what shipped; the indent-rule as the one judgement call and why it is safe in the needless-re-author direction; ideonomy: none, no fork).

```bash
git add scripts/sluice-census.sh scripts/test-sluice-census.sh docs/superpowers/ledgers/2026-09-06-the-spillway.md
git commit -m "feat(sluice-census): read the Gnomon arms' column set against the census's

Two library functions, under the same HV_CENSUS_LIB seam as
census_golden_count: the column names of a lab schema.json, and the arms
whose set differs from the census's. The delivery uses the second as its
stale-arms trigger (The Spillway, spec §3.2).

Claude-Session: https://claude.ai/code/session_01DCmbLQnYU317DpApjFm1k5"
```

---

### Task 3: `sluice-census.sh` — the delivery re-authors the arms

**Files:**
- Modify: `scripts/sluice-census.sh` (header prose; the delivery flow between `git -C "$wt" diff --cached --stat | sed …` and the `# HV_CENSUS_DELIVERY=1 tells pre-commit's golden-pins guard…` comment; the commit message; the final DELIVERED report)
- Modify: `scripts/test-sluice.sh` (the census block: after `cen_wt="$tmp/cen-wt"; cp -r "$cen" "$cen_wt"`, the `run_census` helper, new arms after the timings-only arm and before `write_stub fails`)

**Interfaces:**
- Consumes: `injection_arms_stale "$wt"` (Task 2); `bash "$wt/scripts/gnomon-injection.sh" check` (Task 1); `bash "$repo_root/scripts/timed.sh" <label> -- <cmd>` (existing; appends a row to the ledger of the repo at the CURRENT DIRECTORY, so it is run with cwd `$wt`); `HV_CENSUS_LOCK` (default `/tmp/hv-census.lock`, shared with `census-run.sh` and `sluice-run.sh`); `HV_CENSUS_WAIT_TIMEOUT` (default 2700).
- Produces: log lines the tests grep for, verbatim: `re-authoring the Gnomon injection arms`, `Gnomon arms re-authored at`, `Gnomon arms unchanged`, `ARMS NOT RE-AUTHORED`, `refused its pre-flight`; exit 4 on every arms failure (the same code as COMMIT REFUSED).

- [ ] **Step 1: Write the failing tests**

In `scripts/test-sluice.sh`, right after the line `cen_wt="$tmp/cen-wt"; cp -r "$cen" "$cen_wt"`, add the stub and the schemas the delivery will compare. Note the census scratch tree is created earlier in the same block (`mkdir -p book/src/laboratory/generated/the-census` … `g add -A; g commit -qm root`); the schema files must be committed there BEFORE the `cp -r`, so add them inside that `( cd "$cen" … )` subshell, immediately before `g add -A; g commit -qm root`:

```bash
    # The Spillway: the delivery compares the census's column set with the
    # Gnomon arms' and re-authors the arms when the world moved or the sets
    # differ. Both files in serde's pretty-print shape (study name at indent
    # 2, column names at indent 6), matching, so the null arms below stay null.
    mkdir -p windows/lab/tests/fixtures/injection/baseline-a
    printf '{\n  "columns": [\n    {\n      "name": "seed"\n    },\n    {\n      "name": "value"\n    }\n  ],\n  "name": "the-census"\n}\n' \
        > book/src/laboratory/generated/the-census/schema.json
    printf '{\n  "columns": [\n    {\n      "name": "seed"\n    },\n    {\n      "name": "value"\n    }\n  ],\n  "name": "gnomon-injection"\n}\n' \
        > windows/lab/tests/fixtures/injection/baseline-a/schema.json
    printf 'seed,value\n0,1\n' > windows/lab/tests/fixtures/injection/baseline-a/rows.csv
```

Then, after `cen_wt="$tmp/cen-wt"; cp -r "$cen" "$cen_wt"`, add:

```bash
# THE STUB AUTHORING SCRIPT lives in the WORKTREE copy, because the delivery
# runs the ref's own gnomon-injection.sh (its ARMS literals must match the
# source at that ref), never the queue's. `check` answers per the mode; a run
# records that it happened, records whether the box lock was HELD around it
# (flock -n on a fresh descriptor fails while the delivery holds the lock —
# the positive control for spec §3.2 step 3), and rewrites one arm file.
# Markers go under $tmp, outside the worktree, so `add -u` cannot sweep them.
cp "$repo_root/scripts/timed.sh" "$cen/scripts/timed.sh"
write_gnomon() {  # $1 = ok|refuse|fail
    cat > "$cen_wt/scripts/gnomon-injection.sh" <<STUB
#!/usr/bin/env bash
if [ "\${1:-}" = "check" ]; then
    [ "$1" = "refuse" ] && { echo "gnomon-injection: REFUSING to run with a dirty tree (stub)" >&2; exit 1; }
    exit 0
fi
echo authored >> "$tmp/gnomon-ran"
if flock -n "\${HV_CENSUS_LOCK:?}" -c true 2>/dev/null; then echo free > "$tmp/gnomon-lock"; else echo held > "$tmp/gnomon-lock"; fi
[ "$1" = "fail" ] && { echo "stub authoring exploded" >&2; exit 1; }
printf 'seed,value\n0,9\n' > "$cen_wt/windows/lab/tests/fixtures/injection/baseline-a/rows.csv"
exit 0
STUB
    chmod +x "$cen_wt/scripts/gnomon-injection.sh"
}
write_gnomon ok
```

Change the `run_census` helper to export a private lock path:

```bash
run_census() {  # $1 = ref ; echoes rc
    HV_SLUICE_REPO_ROOT="$cen" HV_SLUICE_DIR="$tmp/cen-state" HV_CENSUS_LOCK="$tmp/census.lock" \
        bash "$repo_root/scripts/sluice-census.sh" "$1" >/dev/null 2>&1
    echo $?
}
```

Immediately after the existing `moves` arm's last assertion (the `ok "the delivered branch carries the regenerated golden, not the old one"` block), add:

```bash
# --- THE ARMS RODE ALONG (The Spillway, spec §3.2) --------------------------
# The world moved, so the delivery re-authored the arms, under the lock, and
# committed them with the goldens.
if [ -f "$tmp/gnomon-ran" ]; then ok "a moving census re-authored the Gnomon arms"
else bad "a moving census did NOT run gnomon-injection.sh"; fi
if [ "$(cat "$tmp/gnomon-lock" 2>/dev/null)" = "held" ]; then
    ok "the box lock was HELD while the arms were authored (flock -n failed inside the stub)"
else
    bad "the arms were authored with the lock FREE (marker: $(cat "$tmp/gnomon-lock" 2>/dev/null || echo none))"
fi
if [ -n "$delivered" ] && g -C "$cen_origin" show "$delivered:windows/lab/tests/fixtures/injection/baseline-a/rows.csv" 2>/dev/null | grep -q '0,9'; then
    ok "the delivered branch carries the RE-AUTHORED arm beside the goldens"
else bad "the delivered branch does not carry the re-authored arm"; fi
if [ -n "$delivered" ] && g -C "$cen_origin" log -1 --format=%B "$delivered" | grep -q 'Gnomon arms re-authored at'; then
    ok "the commit message names the arms"
else bad "the commit message does not name the arms"; fi
if [ -n "$delivered" ] && g -C "$cen_origin" show "$delivered:docs/timings.md" 2>/dev/null | grep -q '| gnomon-injection |'; then
    ok "the re-authoring's cost landed as a gnomon-injection row in docs/timings.md"
else bad "no gnomon-injection row in the delivered timings ledger — the cost went unrecorded"; fi
rm -f "$tmp/gnomon-ran" "$tmp/gnomon-lock"
```

After the timings-only arm's last assertion (`ok "the delivered branch carries the timings row ALONE -- no golden rode along"` block) and before `write_stub fails`, add:

```bash
# --- ARMS: the null does not re-author; a stale column set does -------------
if [ ! -f "$tmp/gnomon-ran" ] && grep -q 'Gnomon arms unchanged' "$t_log"; then
    ok "a census that moved nothing, with matching arms, left the arms alone and said so"
else bad "null census: ran=$([ -f "$tmp/gnomon-ran" ] && echo yes || echo no), log lacks 'Gnomon arms unchanged'"; fi

# The column trigger alone: the census moves nothing, but an arm was authored
# against a registry the census has since outgrown. Committed in the worktree
# so the tree is clean when the delivery starts, exactly as a real ref is.
printf '{\n  "columns": [\n    {\n      "name": "seed"\n    }\n  ],\n  "name": "gnomon-injection"\n}\n' \
    > "$cen_wt/windows/lab/tests/fixtures/injection/baseline-a/schema.json"
g -C "$cen_wt" add -A; g -C "$cen_wt" -c user.name=c -c user.email=c@c commit -qm "stale arm"
rm -f "$tmp/gnomon-ran"
rc_c=$(run_census "$cen_ref")
c_log="$(ls -t "$tmp/cen-state"/census-*.log | head -1)"
if [ "$rc_c" = "0" ] && [ -f "$tmp/gnomon-ran" ] && grep -q 'arms are stale' "$c_log" && grep -q '^sluice-census:   baseline-a: 1 column' "$c_log"; then
    ok "a null census with a STALE arm re-authors, and the log names the arm and the count"
else bad "stale-arm census: rc=$rc_c ran=$([ -f "$tmp/gnomon-ran" ] && echo yes || echo no) log=$c_log"; fi
g -C "$cen_wt" reset -q --hard HEAD~1
rm -f "$tmp/gnomon-ran" "$tmp/gnomon-lock"

# --- ARMS: a failed authoring is a refusal, not a delivery --------------------
write_stub moves; write_gnomon fail
before_af="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
rc_af=$(run_census "$cen_ref")
after_af="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
af_log="$(ls -t "$tmp/cen-state"/census-*.log | head -1)"
if [ "$rc_af" = "4" ] && [ "$before_af" = "$after_af" ] && grep -q 'ARMS NOT RE-AUTHORED' "$af_log" \
   && [ -n "$(g -C "$cen_wt" diff --cached --name-only)" ]; then
    ok "a failed re-authoring exits 4, pushes nothing, names itself, and leaves the goldens staged for recovery"
else bad "failed authoring: rc=$rc_af branches $before_af -> $after_af staged=$(g -C "$cen_wt" diff --cached --name-only | wc -l) log=$af_log"; fi
g -C "$cen_wt" reset -q --hard; rm -f "$tmp/gnomon-ran" "$tmp/gnomon-lock"

# --- ARMS: a refused pre-flight never authors and never takes the lock -------
write_gnomon refuse
before_ar="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
rc_ar=$(run_census "$cen_ref")
after_ar="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
ar_log="$(ls -t "$tmp/cen-state"/census-*.log | head -1)"
if [ "$rc_ar" = "4" ] && [ "$before_ar" = "$after_ar" ] && grep -q 'refused its pre-flight' "$ar_log" && [ ! -f "$tmp/gnomon-ran" ]; then
    ok "a refused check exits 4, pushes nothing, and authoring was never entered"
else bad "refused check: rc=$rc_ar branches $before_ar -> $after_ar ran=$([ -f "$tmp/gnomon-ran" ] && echo yes || echo no) log=$ar_log"; fi
g -C "$cen_wt" reset -q --hard; rm -f "$tmp/gnomon-ran" "$tmp/gnomon-lock"
write_gnomon ok
```

- [ ] **Step 2: Run to verify it fails**

Run (on lefford — the file SKIPs without `flock`; from the Mac: `ssh lefford 'cd ~/Projects/hornvale && git fetch origin && git checkout <this branch tip> && bash scripts/test-sluice.sh 2>&1 | tail -40'` — or push the branch and check it out in lefford's main checkout, restoring `main` afterwards with `git checkout main`):
Expected: the new arms FAIL (`a moving census did NOT run gnomon-injection.sh`, `no gnomon-injection row`, …); the pre-existing census arms still pass.

- [ ] **Step 3: Implement the arms step**

In `scripts/sluice-census.sh`, add to the header, after the `WHAT IT DELIBERATELY DOES NOT DO: push main` paragraph:

```bash
# WHAT IT NOW CARRIES BESIDE THE GOLDENS (The Spillway, decision 0836). The
# Gnomon injection arms (windows/lab/tests/fixtures/injection/) are evidence
# scored against the census, and the commit gate compares their column set to
# the census's. A refresh that registers a metric was therefore refused by
# its own delivery commit, and the authoring script refused the delivery's
# staged goldens as dirt — a cycle The Warp broke by hand (ledger #12). The
# delivery now re-authors the arms itself, at the censused ref, with the
# ref's OWN gnomon-injection.sh (its literals must match that source), under
# the same flock every expensive job takes (decision 0133), timed into
# docs/timings.md under `gnomon-injection`, and commits them with the
# goldens. Trigger: the world moved (goldens > 0) OR an arm's columns differ.
#
# THE RULE THAT PLACES EACH CENSUS-SHAPED CHECK: a delivery SATISFIES every
# check whose remedy is a regeneration, and DEFERS only a check whose remedy
# is a human re-statement. The arms are the former. The column-COUNT witness
# in anomaly.rs is the latter — pre-commit stands it down for this one commit
# under HV_CENSUS_DELIVERY, and the merge of the delivery branch demands the
# re-pin, exactly as it demands the calibration pins.
#
# NO CLAIM FILE IS WRITTEN for the arms. The eight-field claim format belongs
# to census_claim.rs and its two writers; a third would be the drift
# lane_sets.rs fails on. So for those minutes `census-run.sh status` reports
# nothing while every flock-taker waits. Accepted (spec §3.2).
```

Then, after the line `git -C "$wt" diff --cached --stat | sed 's/^/sluice-census:   /'` and before the `# HV_CENSUS_DELIVERY=1 tells pre-commit's golden-pins guard…` comment, insert:

```bash
# --- THE GNOMON ARMS (The Spillway, decision 0836; header) -------------------
# Reached only when something is staged (the empty-index branch above exited),
# which is every production run, since the timings row is always staged.
stale="$(injection_arms_stale "$wt")"
arms_note=""
if [ "$n_goldens" -gt 0 ] || [ -n "$stale" ]; then
    if [ "$n_goldens" -gt 0 ]; then
        echo "sluice-census: the world moved ($n_goldens golden path(s)) — re-authoring the Gnomon injection arms at ${ref:0:12}"
    else
        echo "sluice-census: the census moved nothing but the arms are stale — re-authoring the Gnomon injection arms at ${ref:0:12}:"
    fi
    [ -z "$stale" ] || printf '%s\n' "$stale" | sed 's/^/sluice-census:   /'
    gnomon="$wt/scripts/gnomon-injection.sh"
    if [ ! -f "$gnomon" ]; then
        echo "sluice-census: ARMS NOT RE-AUTHORED — $ref carries no scripts/gnomon-injection.sh." >&2
        echo "sluice-census: the goldens stay staged in $wt; nothing was pushed. Recover by hand (The Warp, ledger #12)." >&2
        exit 4
    fi
    # The ref's OWN copy, before the lock: a ref that predates The Spillway
    # refuses the delivery's staged goldens here, in milliseconds, and the
    # box is never taken for it.
    if ! (cd "$wt" && bash "$gnomon" check); then
        echo "sluice-census: ARMS NOT RE-AUTHORED — ${ref:0:12}'s gnomon-injection.sh refused its pre-flight (above)." >&2
        echo "sluice-census: a ref that predates The Spillway refuses the delivery's own staged goldens as dirt." >&2
        echo "sluice-census: the goldens stay staged in $wt; nothing was pushed. Recover by hand (The Warp, ledger #12)." >&2
        exit 4
    fi
    if ! command -v flock >/dev/null 2>&1; then
        echo "sluice-census: ARMS NOT RE-AUTHORED — no flock(1) on $(hostname -s), and the census could not have run here without one." >&2
        exit 4
    fi
    arms_lock="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
    exec 9>"$arms_lock"
    arms_wait_began=$SECONDS
    if ! flock -w "${HV_CENSUS_WAIT_TIMEOUT:-2700}" 9; then
        echo "sluice-census: ARMS NOT RE-AUTHORED — timed out waiting for the box lock ($arms_lock); the goldens stay staged in $wt; nothing was pushed." >&2
        exit 4
    fi
    arms_waited=$((SECONDS - arms_wait_began))
    echo "sluice-census: holds the box lock for the arms after ${arms_waited}s queued (no claim file — see the header)"
    if ! (cd "$wt" && HV_CENSUS_WAITED_S="$arms_waited" bash "$repo_root/scripts/timed.sh" gnomon-injection -- bash "$gnomon"); then
        exec 9>&-
        echo "sluice-census: ARMS NOT RE-AUTHORED — gnomon-injection.sh failed (read its output above)." >&2
        echo "sluice-census: this is a refusal, not a census failure: the goldens stay staged in $wt; nothing was pushed." >&2
        echo "sluice-census: a VOID arm is a real finding; do not deliver the goldens without the arms." >&2
        exit 4
    fi
    exec 9>&-
    git -C "$wt" add -A -- windows/lab/tests/fixtures/injection 2>/dev/null || true
    git -C "$wt" add -u 2>/dev/null || true
    n_arms="$(find "$wt/windows/lab/tests/fixtures/injection" -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l | tr -d ' ')"
    arms_note="Gnomon arms re-authored at ${ref:0:12} ($n_arms arms)."
else
    arms_note="Gnomon arms unchanged (census moved nothing; columns match)."
fi
echo "sluice-census: $arms_note"
```

In the `git … commit -q -m "chore(census): …"` message, add a line after `Census wall time: ${elapsed}s.`:

```
${arms_note}
```

And in the final report, after the `DELIVERED on` lines and before the `submit it with` line, add:

```bash
echo "sluice-census: $arms_note"
```

- [ ] **Step 4: Run to verify it passes, then shellcheck**

Run on lefford as in Step 2: `bash scripts/test-sluice.sh 2>&1 | tail -60`; locally: `make shellcheck && bash scripts/test-sluice-census.sh`.
Expected: every census-block arm `ok`, including the five new ones; the lock marker reads `held`; shellcheck silent. If shellcheck flags the `exec 9>` pattern (SC2094 or similar), copy the directive `census-run.sh` uses at its own `exec 9>"$LOCK"`.

Decision rule if the `held` marker reads `free`: the stub's `flock -n` used the SAME descriptor as the parent (it must not) or the lock was released before `timed.sh` ran. Check that `exec 9>&-` sits AFTER the `timed.sh` call on the success path, and that the stub opens the lock by PATH (`flock -n "$HV_CENSUS_LOCK" -c true`), not by fd 9.

- [ ] **Step 5: Ledger section and commit**

Append `## Task 3 — complete` to the ledger: what shipped; the measured wall of the stub-driven run; any change to the plan's shape the implementer made and why (e.g. the exact grep strings if they had to move); ideonomy: none.

```bash
git add scripts/sluice-census.sh scripts/test-sluice.sh docs/superpowers/ledgers/2026-09-06-the-spillway.md
git commit -m "feat(sluice-census): the delivery re-authors the Gnomon arms before it commits

A census that grew the registry could not deliver: its gate compares the
injection arms' columns to the census's, and the arms could not be
re-authored until the goldens were committed (The Warp, ledger #12). The
delivery now runs the ref's own gnomon-injection.sh under the box lock,
ledgers the cost under gnomon-injection, and commits the arms with the
goldens. Trigger: the world moved, or an arm's column set differs.

Claude-Session: https://claude.ai/code/session_01DCmbLQnYU317DpApjFm1k5"
```

- [ ] **Step 6: Stage gate**

Push the branch and submit `make sluice-stage BRANCH=campaign/the-spillway REF=$(git rev-parse HEAD)` (the `submitting-to-the-sluice` skill governs). Absorb `main` first if `docs/audits/campaign-reconciliation.tsv` has moved on `origin/main` (The Warp's retro, "Do differently" 5). Record the result (rc, wall, the row id) in the ledger under Task 3.

---

### Task 4: the count witness stands down for a delivery commit

**Files:**
- Modify: `scripts/subfloor-roster.sh` (the `awk` line and the header)
- Modify: `scripts/hooks/pre-commit` (the final `if ! make gate-commit; then` block)
- Modify: `scripts/test-census-guard.sh` (append before the final `printf`)

**Interfaces:**
- Produces: `HV_SUBFLOOR_EXCLUDE` — an ERE; roster lines whose test path (the part after `$`) matches are omitted from the flat filterset. Empty or unset: byte-identical output.
- Produces: in `pre-commit`, `subfloor_exclude='evaluable_columns_measured_surface_on_the_[0-9]+_column_census'`, set only inside the `HV_CENSUS_DELIVERY` branch; the test reads the pattern OUT of the hook.

- [ ] **Step 1: Write the failing tests**

Append to `scripts/test-census-guard.sh`, before `printf '\ntest-census-guard: %d passed, %d failed\n' "$pass" "$fail"`:

```bash
# --- the column-count witness stands down for a delivery (The Spillway) ----
# `domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_<N>_column_census`
# carries the census's column count in its NAME and is in the sub-floor roster,
# so a census that grows the registry reds it on the delivery commit — it is
# what refused The Warp's delivery FIRST (spec §1, leg 3). It can only be
# re-stated by a human, so the delivery DEFERS it and the merge demands it.
# TWO-WAY AGREEMENT, not execution: the gate-commit path builds and runs the
# roster, so this reads the pattern OUT of the hook and drives the roster
# script with it, asserting the omitted set is exactly one term.
xpat="$(grep -m1 "^ *subfloor_exclude='" scripts/hooks/pre-commit | sed "s/^[^']*'//; s/'$//")"
if [ -n "$xpat" ]; then ok "read subfloor_exclude from the hook: $xpat"
else bad "could not read subfloor_exclude from scripts/hooks/pre-commit"; fi
without="$(bash scripts/subfloor-roster.sh)"
with="$(HV_SUBFLOOR_EXCLUDE="$xpat" bash scripts/subfloor-roster.sh)"
terms() { printf '%s' "$1" | tr '|' '\n' | grep -c 'test(='; }
n_without="$(terms "$without")"; n_with="$(terms "$with")"
if [ "$((n_without - n_with))" -eq 1 ]; then
    ok "the exclusion omits EXACTLY one roster term ($n_without -> $n_with)"
else bad "the exclusion omitted $((n_without - n_with)) term(s), want 1 — the pattern has rotted or matches too widely"; fi
if printf '%s' "$without" | grep -qE "test\(=[^)]*${xpat}\)"; then
    ok "CONTROL: without the escape the witness IS selected"
else bad "control failed: the witness is not in the roster at all, so the stand-down proves nothing"; fi
if printf '%s' "$with" | grep -qE "${xpat}"; then
    bad "with the escape the witness is STILL selected"
else ok "with the escape the witness is not selected"; fi
same="$(HV_SUBFLOOR_EXCLUDE='no_such_test_zzz_[0-9]+' bash scripts/subfloor-roster.sh)"
if [ "$same" = "$without" ]; then ok "an exclusion matching nothing leaves the filterset byte-identical (rot fails SAFE: the witness runs)"
else bad "a non-matching exclusion changed the filterset"; fi
if printf '%s' "$with" | grep -q '(' && ! printf '%s' "$with" | grep -q 'and not'; then
    ok "the excluded filterset is still FLAT (no wrapping) — subfloor-run-chunked.sh splits on ' | '"
else bad "the excluded filterset is wrapped, which the chunker cannot split"; fi
# THE STAND-DOWN COUNT. Three checks stand down for a delivery: the golden-pins
# guard, the yellow-census alarm, and now the column-count witness. A fourth
# is placed by the rule in decision 0836 and edits this expectation with its
# reason, never silently.
n_sd="$(grep -cF 'if [ -n "${HV_CENSUS_DELIVERY:-}" ]' scripts/hooks/pre-commit)"
if [ "$n_sd" = "3" ]; then ok "HV_CENSUS_DELIVERY stands down exactly three checks"
else bad "HV_CENSUS_DELIVERY stand-down branches: $n_sd, want 3"; fi
```

- [ ] **Step 2: Run to verify it fails**

Run: `bash scripts/test-census-guard.sh`
Expected: `could not read subfloor_exclude`, the omit-count arm fails (0 omitted), the stand-down count reads 2. Pre-existing arms pass.

- [ ] **Step 3: Implement the exclusion and set it in the hook**

In `scripts/subfloor-roster.sh`, replace the final `awk` line with:

```bash
# HV_SUBFLOOR_EXCLUDE narrows this roster for ONE caller: a census delivery
# commit (scripts/hooks/pre-commit under HV_CENSUS_DELIVERY), which cannot
# satisfy the column-count witness by construction — its name carries a count
# that does not exist until the census does, and only a human re-states it
# (The Spillway, decision 0836). Lines whose test path matches are OMITTED,
# never wrapped in `and not (...)`: subfloor-run-chunked.sh splits the
# filterset on a literal ' | ' and is lossless only while it stays flat. A
# pattern that matches nothing is a no-op, and that is the safe direction —
# the witness then runs and reds the delivery loudly. Empty for every other
# caller, so the default roster is unchanged.
awk -F'$' -v X="${HV_SUBFLOOR_EXCLUDE:-}" \
    '!/^#/ && NF>1 && (X=="" || $2 !~ X) {printf "%stest(=%s)", sep, $2; sep=" | "}' "$roster"
```

In `scripts/hooks/pre-commit`, replace the final block

```bash
if ! make gate-commit; then
    echo "pre-commit: 'make gate-commit' failed — fix fmt/clippy/type-audit/tests before committing." >&2
    exit 1
fi
```

with:

```bash
# THE COLUMN-COUNT WITNESS CANNOT BE SATISFIED BY A DELIVERY COMMIT either, so
# it is stood down for that one commit — the third scoped opt-out under the
# same variable, set by the same script (The Spillway, decision 0836).
#
# `domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_<N>_column_census`
# carries the census's metric-column count in its NAME and in two assertions,
# and its doc-comment is a per-epoch narrative ("re-measure and update this").
# It is in the sub-floor roster by exact name, and it is what refused The
# Warp's census delivery FIRST. No machine may re-pin it: that is the rule
# separating this from the injection arms, which the delivery regenerates
# itself — satisfy what a regeneration remedies, defer what needs a human.
# DEFERRED, not discharged: the merge of the delivery branch runs the full
# suite and demands the re-pin of the campaign that submits it, exactly as it
# demands the calibration pins. scripts/test-census-guard.sh reads this
# pattern out of the hook and asserts it omits exactly one roster term.
subfloor_exclude=""
if [ -n "${HV_CENSUS_DELIVERY:-}" ]; then
    subfloor_exclude='evaluable_columns_measured_surface_on_the_[0-9]+_column_census'
    echo "pre-commit: HV_CENSUS_DELIVERY=1 — standing down the census column-count witness for this delivery." >&2
    echo "pre-commit: its name carries a count that does not exist until the census does; only a human re-states it." >&2
    echo "pre-commit: the obligation is DEFERRED, not discharged — the merge of the delivery branch will demand the re-pin." >&2
fi
if ! HV_SUBFLOOR_EXCLUDE="$subfloor_exclude" make gate-commit; then
    echo "pre-commit: 'make gate-commit' failed — fix fmt/clippy/type-audit/tests before committing." >&2
    exit 1
fi
```

- [ ] **Step 4: Run to verify it passes, then shellcheck, then the gate**

Run: `bash scripts/test-census-guard.sh && make shellcheck`, then the byte-identity control:

```bash
sp=/private/tmp/claude-501/-Users-nathan-Projects-hornvale-hornvale/1b412e14-acfc-48ba-bbe6-90b64ae2e8d5/scratchpad
git stash -q && bash scripts/subfloor-roster.sh > "$sp/roster-before" && git stash pop -q
bash scripts/subfloor-roster.sh | cmp - "$sp/roster-before" && echo IDENTICAL-BY-DEFAULT
```
Expected: all arms `ok`; shellcheck silent; `IDENTICAL-BY-DEFAULT` (the default filterset is byte-identical before and after this change — the positive proof that nothing but the escape changed).

- [ ] **Step 5: Ledger section and commit**

Append `## Task 4 — complete` to the ledger. This commit touches no `.rs`, so the hook runs `docs-tests`; the roster script change is exercised by `test-census-guard.sh` and by the next `.rs` commit's own gate (Task 5).

```bash
git add scripts/subfloor-roster.sh scripts/hooks/pre-commit scripts/test-census-guard.sh docs/superpowers/ledgers/2026-09-06-the-spillway.md
git commit -m "feat(pre-commit): a census delivery stands down the column-count witness

The witness carries the census's column count in its own name and is in
the sub-floor roster; it refused The Warp's delivery first and only a
human may re-pin it. subfloor-roster.sh can now omit matching terms
(kept flat for the chunker); pre-commit sets the exclusion inside its
HV_CENSUS_DELIVERY branch. Deferred, not discharged: the merge demands
the re-pin.

Claude-Session: https://claude.ai/code/session_01DCmbLQnYU317DpApjFm1k5"
```

---

### Task 5: decision 0836, and the prose brought current

**Files:**
- Create: `docs/decisions/0836-a-census-delivery-regenerates-the-evidence-its-gate-reads.md`
- Modify: `docs/digest/decisions-in-force.md` (regenerated, never hand-edited)
- Modify: `windows/lab/src/domesday/anomaly.rs` (the comment line `// masked it (this test is not in the sub-floor tier a local gate runs).`)
- Modify: `windows/lab/tests/suite/anomaly_injection.rs` (the branch table above `fn the_fixture_columns_match_the_census` and its assertion message)
- Modify: `windows/lab/tests/fixtures/injection/README.md` (the "Regenerate with the script" section)
- Modify: `windows/lab/CLAUDE.md` (the paragraph beginning `**That last sentence no longer describes what happens`)
- Modify: `scripts/CLAUDE.md` (a bullet beside the existing `sluice-census.sh` claim-bookkeeping bullet)
- Modify: `CLAUDE.md` (the census block, after the `A RUN THAT MOVES NOTHING STILL PUSHES A BRANCH` paragraph)

**Interfaces:**
- Produces: decision 0836, citable from Rust comments from this task on.

- [ ] **Step 1: Verify the decision record (minted in Task 2 by controller ruling)**

**Superseded in execution.** `decision_cites_in_sources_resolve` scans `scripts/` as well as Rust, so the first script comment citing 0836 (Task 2) could not land before the record existed; the record below was therefore minted INSIDE Task 2 (commit `7f7333b72`) with this exact text, and the digest regenerated there. In this task, only VERIFY: `ls docs/decisions/0836-*` shows the file, `git diff --exit-code docs/digest/` after re-running the two `render` commands below is empty. The record's text, for reference — `docs/decisions/0836-a-census-delivery-regenerates-the-evidence-its-gate-reads.md`:

```markdown
# 0836. A census delivery regenerates the evidence its gate reads, and defers only what needs a human re-statement

**Status:** Accepted (2026-09-06) · **Campaign:** The Spillway · **Decider:**
Nathan · **Relates:**
[0079](0079-census-goldens-are-authored-on-one-enforced-host.md),
[0133](0133-nontrivial-checks-run-in-one-serial-lane.md),
[0139](0139-main-advances-only-through-the-lock.md),
[0514](0514-a-census-refresh-needs-no-authorization.md)

In the context of a queued census refresh that registers a metric being
refused by its own delivery commit — the commit gate compares the Gnomon
injection arms' column set to the census's, the arms' authoring script
refused the delivery's staged goldens as dirt, and a second gate test carries
the column count in its own name — we decided that **the delivery satisfies
every check whose remedy is a regeneration and defers only a check whose
remedy is a human re-statement**, accepting roughly eight more minutes of the
canonical box on a moving census, a lock held without a claim file for those
minutes, and a delivery branch that carries a deferred witness until the
campaign that merges it re-states it.

**Context.** The Warp's census at `4a419e996ef7` moved 136 goldens and could
not be committed (ledger #12): the goldens were landed through an ungated
intermediate object and a by-hand re-authoring on lefford. `HV_CENSUS_DELIVERY`
already stood down two checks for a delivery commit, each justified
separately; this record states the rule they were instances of and places
two more checks by it.

**Decision.**
1. `scripts/sluice-census.sh` re-authors the Gnomon injection arms at the
   censused ref, with that ref's own `gnomon-injection.sh`, under the shared
   flock (0133), timed into `docs/timings.md` as `gnomon-injection`, and
   commits them with the goldens — whenever the world moved (a census golden
   changed) or an arm's column set differs from the census's. The manifest's
   `sha` is therefore the census's ref and its host the canonical one (0079).
2. `gnomon-injection.sh`'s clean-tree guard covers everything a build or its
   mutation can see and excludes `book/`, `docs/` and its own fixtures.
3. The column-count witness
   (`domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_<N>_column_census`)
   is stood down for the delivery commit only, by omitting its roster term.
   The merge of the delivery branch runs it and demands the re-pin — safe
   because a delivery never pushes `main` (0139).
4. The next census-shaped check is placed by the rule, not by precedent: if a
   machine on the canonical box can regenerate what it reads, the delivery
   does so; if a human must re-state it, the delivery defers it.

**Consequence.** A refresh that grows the registry delivers on its own; the
by-hand path (`ssh lefford … scripts/gnomon-injection.sh`) survives as the
exception. A ref predating The Spillway carries the old guard and is refused
at the delivery's pre-flight with the goldens left staged. No claim file is
written for the arms, so status readers see nothing for those minutes while
every flock-taker waits.

**See also.** `docs/superpowers/specs/2026-09-06-the-spillway-design.md` §3;
`scripts/sluice-census.sh` header; `scripts/hooks/pre-commit` (the third
`HV_CENSUS_DELIVERY` branch); `scripts/test-census-guard.sh` (pins the
stand-down count at three).
```

The four `Relates:` filenames were read from `docs/decisions/` at planning time (`ls docs/decisions/ | grep -E '^(0079|0133|0139|0514)-'`); the doc-consistency tests resolve links, so re-check with the same command before committing.

Regenerate the digest: `cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md` and confirm `git diff --stat docs/digest/` shows the index moved by one entry. Also `cargo run --manifest-path tools/digest/Cargo.toml -- render delta > docs/digest/intent-vs-reality.md` — expected unchanged (the registry row flips in Task 6); if it moved, commit it too.

- [ ] **Step 2: The Rust comments and one message**

In `windows/lab/src/domesday/anomaly.rs`, replace the comment line

```rust
        // masked it (this test is not in the sub-floor tier a local gate runs).
```

with

```rust
        // masked it. (An earlier draft of this line said this test "is not in
        // the sub-floor tier a local gate runs". It IS: docs/timings/
        // subfloor-roster.tsv selects it by exact name, and it is what refused
        // The Warp's census delivery first. Since The Spillway a census
        // delivery commit stands it down under HV_CENSUS_DELIVERY, and the
        // merge of the delivery branch demands the re-pin of the campaign
        // that submits it — decision 0836.)
```

In `windows/lab/tests/suite/anomaly_injection.rs`, replace the branch-table bullet

```rust
/// - the census gained columns and the fixtures did not → re-author the
///   fixtures in the same commit as the refresh (Task 7 carries this);
```

with

```rust
/// - the census gained columns and the fixtures did not → a queued census
///   delivery (`scripts/sluice-census.sh`) re-authors them at the census's
///   ref, under the box lock, in the same commit as the goldens (decision
///   0836); a delivery that could not — a ref predating The Spillway, whose
///   authoring script refuses the staged goldens as dirt — is refused at its
///   pre-flight, and the by-hand path is `scripts/gnomon-injection.sh` on the
///   canonical box;
```

and in that test's `assert!` message, replace `Re-author with scripts/gnomon-injection.sh on the canonical box in the same commit as the census refresh — never filter the mismatch away.` with `Re-author with scripts/gnomon-injection.sh on the canonical box in the same commit as the census refresh — a queued delivery (make sluice-census) does this itself since The Spillway, decision 0836 — never filter the mismatch away.` (keep the `\` line continuations the file uses).

- [ ] **Step 3: The prose**

`windows/lab/tests/fixtures/injection/README.md` — replace the section from `## Regenerate with the script, and only with the script` through the paragraph ending `adjudicates nothing.` with:

```markdown
## Regenerated by the census delivery, or by the script by hand

Since The Spillway (decision 0836) the ordinary author is the queued census
delivery: `make sluice-census BRANCH=<branch> REF=<full-sha>` re-runs
`scripts/gnomon-injection.sh` at that ref, under the box lock, whenever the
census moved a golden or an arm's column set differs from the census's, and
commits the arms beside the goldens. The manifest's `sha` is the census's
ref and its `host` the canonical box.

The by-hand path survives as the exception — a ref that predates The
Spillway, or a battery authored without a census:

    ssh lefford 'cd ~/Projects/hornvale && git fetch --all && \
      git checkout <full-sha> && scripts/gnomon-injection.sh'

The script refuses dirt anywhere a build or its mutation can see (it allows
`book/`, `docs/` and this directory — `scripts/gnomon-injection.sh check`
says which), asserts each target literal is present **exactly once** before
substituting, and restores the file under a `trap` so an interrupt cannot
leave the tree mutated. `HV_GNOMON_PILOT=1` lifts its canonical-host refusal
for machinery validation and stamps the authoring host into the manifest;
`anomaly_injection.rs` reads that stamp, and a battery with any off-host arm
is a **pilot** that prints its recall figure and adjudicates nothing.
```

`windows/lab/CLAUDE.md` — replace the paragraph beginning `**That last sentence no longer describes what happens, and the replacement is QUIETER, not louder.**` (through `...therefore MORE load-bearing than it was, not less.`) with:

```markdown
**That last sentence no longer describes what happens, twice over.**
`anomaly_injection` reads the arms as authored, so a newly registered metric
leaves its movement control GREEN and prints a PREDATES line; only
`the_fixture_columns_match_the_census` reds, and it does so in the commit
gate. And since The Spillway (decision 0836) the arms are re-authored by the
**census delivery itself** — `make sluice-census` runs
`scripts/gnomon-injection.sh` at the censused ref, under the box lock, when
the world moved or the columns differ, and commits the arms with the
goldens. So the second refresh a registration used to owe by hand is paid
by the same queued job as the first. What is still true: the arms are
covered by no drift check between censuses, and a ref that predates The
Spillway cannot self-deliver (its authoring script refuses the delivery's
staged goldens as dirt) — that ref's census is refused at pre-flight and
the by-hand path applies.
```

`scripts/CLAUDE.md` — after the bullet that contains `sluice-census.sh each claim their own row when nothing claimed it for them`, add a bullet:

```markdown
- **`sluice-census.sh` re-authors the Gnomon injection arms before it commits
  (The Spillway, decision 0836)** — with the ref's OWN `gnomon-injection.sh`
  (its literals must match that source), under the shared flock, timed into
  `docs/timings.md` as `gnomon-injection`, whenever a golden moved or an
  arm's columns differ. It writes no claim file for those minutes, so
  `census-run.sh status` reads idle while flock-takers wait. The rule that
  places each census-shaped check: a delivery SATISFIES what a regeneration
  remedies and DEFERS what needs a human re-statement — so `pre-commit`
  stands down three checks under `HV_CENSUS_DELIVERY` (golden pins, the
  yellow alarm, the column-count witness) and `test-census-guard.sh` pins
  that count. A ref predating The Spillway is refused at the delivery's
  pre-flight, goldens left staged, and The Warp's by-hand path applies.
```

Root `CLAUDE.md` — after the paragraph ending `...check whether its test is exercising the case that actually occurs before concluding nobody wrote one.`, add:

```markdown
# A DELIVERY NOW CARRIES THE GNOMON ARMS BESIDE THE GOLDENS (The Spillway,
# decision 0836). The commit gate compares the injection arms' column set to
# the census's, so a refresh that REGISTERED a metric was refused by its own
# delivery commit, and the arms' authoring script refused the delivery's
# staged goldens as dirt — a cycle The Warp broke by hand (ledger #12). The
# delivery re-authors the arms itself, at the censused ref, under the box
# lock, timed as `gnomon-injection` in docs/timings.md, whenever a golden
# moved or an arm's columns differ; a ref that predates The Spillway is
# refused at pre-flight with the goldens left staged. THE RULE, so the next
# census-shaped check is placed by it and not by precedent: a delivery
# SATISFIES every check a regeneration remedies and DEFERS only what needs a
# human re-statement. Three checks are deferred under HV_CENSUS_DELIVERY —
# the golden pins, the yellow alarm, and the column-count witness whose name
# carries the count — and the MERGE of the delivery branch demands all three
# of the campaign that submits it.
```

- [ ] **Step 4: Gate**

Run: `cargo fmt --check && make gate-commit` (Rust comments changed; the hook will run it anyway). Also `cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)'` — link resolution for the new decision and the cites.
Expected: green. Decision rule: `decision_cites_in_sources_resolve` red → the record's filename or number does not match what the comments cite; fix the cite, never the record's number.

- [ ] **Step 5: Ledger section and commit**

Append `## Task 5 — complete` to the ledger, listing every prose site touched (the freshness sweep's evidence).

```bash
git add docs/decisions/0836-*.md docs/digest/ windows/lab/src/domesday/anomaly.rs windows/lab/tests/suite/anomaly_injection.rs windows/lab/tests/fixtures/injection/README.md windows/lab/CLAUDE.md scripts/CLAUDE.md CLAUDE.md docs/superpowers/ledgers/2026-09-06-the-spillway.md
git commit -m "docs(the-spillway): decision 0836, and every description of the delivery brought current

Claude-Session: https://claude.ai/code/session_01DCmbLQnYU317DpApjFm1k5"
```

---

### Task 6: close — registry, chronicle, retrospective, the census of the tip, the merge

**Files:**
- Modify: `book/src/frontier/idea-registry.md` (row `TOOL-census-delivery-and-injection-fixtures-deadlock`)
- Create: `book/src/chronicle/the-spillway.md`; Modify: `book/src/SUMMARY.md` (after the last chronicle entry, chronological)
- Create: `docs/retrospectives/the-spillway.md`
- Modify: `docs/audits/campaign-reconciliation.tsv` (the `the-spillway` row only)
- Modify: `docs/superpowers/ledgers/2026-09-06-the-spillway.md` (close entries)

- [ ] **Step 1: Flip the registry row**

Replace the row's status `raw` with `shipped`, its **Where** with the shape shipped `TOOL-*` rows use (see `TOOL-sluice-queue-in-rust`: campaign name and date, what shipped, a `[chronicle](../chronicle/the-spillway.md)` link) — `The Spillway (2026-09-06): the census delivery re-authors the arms at the census's ref under the box lock and commits them with the goldens; decision 0836. [chronicle](../chronicle/the-spillway.md)`, and compact the description to under 600 characters, ending with `Shipped by The Spillway: the delivery re-authors the arms at the census's ref under the box lock and commits them with the goldens; the guard allows book/ and docs/; the column-count witness is deferred to the merge`. Run `cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)'` — the registry has a row-length test and a status-vocabulary test.

- [ ] **Step 2: Chronicle and retrospective**

`book/src/chronicle/the-spillway.md` — book altitude (technical, comprehensible without the code), roughly 60–100 lines: the deadlock as three legs with the log excerpt from spec §1; the rule; what the delivery does now and what it refuses; the accepted gaps (the lock without a claim file; refs predating the merge); what was NOT changed (no witness re-pinned by machine). Add `- [The Spillway](./chronicle/the-spillway.md)` to `book/src/SUMMARY.md` after the last chronicle line.

`docs/retrospectives/the-spillway.md` — header in The Warp's shape (`**Close:** 2026-09-06, awaiting G6 · **Ledger:** … · **Chronicle:** … · **Decisions:** 0836`); the headline; a "Do differently" list with at least: *read the harness's own guards before deciding how a script degrades* (ledger #3's correction); *a reconciliation row is owed the moment a record file exists, not at close* (the first design commit was refused for it); *the registry row named two legs and the log had three — read the log*. The deferred-minors table lists every accepted minor from the task ledger sections.

- [ ] **Step 3: Reconciliation row and ledger close**

Edit the `the-spillway` row in `docs/audits/campaign-reconciliation.tsv`: disposition `shipped`, evidence `plan: complete through Task 6; chronicle and retrospective landed with it`, and fill the plans/chronicles/retrospectives columns with `docs/superpowers/plans/2026-09-06-the-spillway.md`, `book/src/chronicle/the-spillway.md`, `docs/retrospectives/the-spillway.md`. Run `cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)'`.

Append `## Close` to the ledger: the digest of every entry after G3 (the G6 package draws from it), deferred minors, the census result (Step 5).

- [ ] **Step 4: Commit the close docs**

```bash
git add book/src/frontier/idea-registry.md book/src/chronicle/the-spillway.md book/src/SUMMARY.md docs/retrospectives/the-spillway.md docs/audits/campaign-reconciliation.tsv docs/superpowers/ledgers/2026-09-06-the-spillway.md
git commit -m "docs(the-spillway): chronicle, retrospective, registry row shipped

Claude-Session: https://claude.ai/code/session_01DCmbLQnYU317DpApjFm1k5"
```

- [ ] **Step 5: The census of the tip, through the queue**

Push, then `make sluice-census BRANCH=campaign/the-spillway REF=$(git rev-parse HEAD)`. This is the production run of the null path (this campaign registers no metric): expected log lines `NO GOLDENS MOVED` or a small golden move, and `Gnomon arms unchanged (census moved nothing; columns match)` — OR, if `main`'s arms are stale against the census at this tip (possible: The Warp's manifest was authored at an intermediate object), `re-authoring the Gnomon injection arms` followed by a `gnomon-injection` timings row. Either outcome is a result; record which in the ledger with the wall from `docs/timings.md`. If the delivery is REFUSED, that is a Critical finding about this campaign: read the log (`make sluice-log JOB=<id>`), fix, and re-run before proceeding.

Merge the `census/<ref>-<stamp>` branch it delivers into the campaign branch (`git merge --no-edit`), re-run `make gate-commit` if anything Rust-facing moved, and commit.

- [ ] **Step 6: Stage gate, then the G6 stop**

`make sluice-stage BRANCH=campaign/the-spillway REF=$(git rev-parse HEAD)`; on green, present the G6 package (the post-G3 ledger digest, spec §3.1's rule leading it) and wait for Nathan; then the `closing-a-campaign` skill and `make sluice BRANCH=campaign/the-spillway REF=<full-sha>`.

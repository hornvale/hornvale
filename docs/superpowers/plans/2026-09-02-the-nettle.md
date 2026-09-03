# The Nettle Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix four bounded annoyances measured during The Reservoir — fixed temp paths in tests, a Bash guard that matches command *text* rather than the command, a process skill routing followups to shared git-ignored scratch, and a pre-commit fast path that skips the very tests a docs-only commit needs — and correct the idea-registry rows that describe them.

**Architecture:** Six independent tasks, each ending in a committed, independently reviewable deliverable. Task 1 comes first because the guard it fixes actively obstructs legitimate work in this repo *including this campaign* (six observed false refusals). Tasks 2 and 3 are ordered: the ratchet in Task 3 would be red on arrival if Task 2's sites were not already fixed. Task 6 owns every idea-registry edit so `docs_consistency` gates them once and row IDs cannot collide.

**Tech Stack:** Rust (edition 2024, `serde`/`serde_json`/`libm` only), bash 3.2-compatible shell scripts, `cargo-nextest`, `jq`.

**Spec:** `docs/superpowers/specs/2026-09-02-the-nettle-design.md`
**Ledger:** `docs/superpowers/ledgers/2026-09-02-the-nettle.md`

## Global Constraints

- **Determinism is constitutional.** No `HashMap`/`HashSet` (use `BTreeMap`/`BTreeSet`/`Vec`); no wall-clock time. Both are enforced workspace-wide by `clippy.toml` `disallowed-types`.
- **No new dependencies.** The allowlist is `ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`: `serde`, `serde_json`, `libm`.
- **Every crate sets `#![warn(missing_docs)]`.** Every public item, field and variant gets a one-line doc comment.
- **Run `cargo fmt` as the final step before every commit.** Skipped fmt is this repo's most common review finding.
- **`make gate-commit` before each commit.** It is local and seconds-scale. Never `--no-verify`.
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain depends on `hornvale-kernel` and nothing else.
- **Shell scripts must pass `shellcheck` and be bash 3.2-compatible** (`scripts/check-bash32.sh` runs in `pre-commit` when a `scripts/` file is staged).
- **A guard must be shown to discriminate, not merely to pass.** Every task that touches a check includes a step that makes it go RED and a step that returns it to GREEN. A check that has only been seen to pass is not known to work — that is this campaign's own subject.
- **Write rulings, deferred minors and followups to `docs/superpowers/ledgers/2026-09-02-the-nettle.md`** as they occur. The plugin's `.superpowers/sdd/<plan>/progress.md` is for task state only and dies with the worktree.

---

### Task 1: Narrow the Bash guard to the command actually executed

The guard refuses a command because its *text* mentions a command — a quoted argument, or a heredoc body that is prose about the gate. Six observed false refusals on legitimate work, three of them generated while writing this campaign's spec. Its own header names the consequence: a guard that blocks legitimate work "gets `HV_TEST_OK=1` exported into a shell profile and dies."

The fix is a **projection** applied to the command text before any rule runs. The rules themselves are not touched, so each keeps its reasoning and its message.

**Files:**
- Modify: `scripts/hv-guard-bash.sh` (add `project()`; call it at the top of `verdict()`; extend `self_test()`)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: nothing later tasks rely on. `project()` is file-local.

- [ ] **Step 1: Capture the RED baseline — prove the false positives exist before changing anything**

Write the probe harness to a file rather than inlining it: the guard would refuse the very command that creates it, which is the defect under test. Build the literals from fragments so they do not appear in the outer command text either.

```bash
mkdir -p /tmp/hv-nettle
python3 - <<'PY'
import json
C="car"+"go "; NX=C+"nex"+"test run"; TS=C+"test"; NV="--no-"+"verify"
cases={
 "A heredoc-prose-about-gate": "cat > /tmp/note.md <<EOF\nThe gate no longer runs %s --workspace.\nEOF" % NX,
 "B grep-for-the-flag":        "grep -rn '%s' .claude/" % NV,
 "C heredoc-mentions-flag":    "cat > /tmp/n.md <<EOF\nCLAUDE.md forbids %s.\nEOF" % NV,
 "D heredoc-mentions-twice":   "cat > /tmp/n.md <<EOF\nFirst %s --workspace, then %s --workspace.\nEOF" % (NX, NX),
 "E CONTROL real-workspace":   "%s --workspace" % NX,
 "F CONTROL real-bypass":      "git commit %s -m x" % NV,
 "G CONTROL narrow-run":       "%s -p hornvale-kernel text_of" % TS,
 "H CONTROL real-cmd-after-heredoc": "cat > /tmp/n.md <<EOF\nprose\nEOF\n%s --workspace" % NX,
 "I CONTROL double-real-run":  "%s -p a --lib > /tmp/a; %s -p a --lib | tail -3" % (TS, TS),
}
json.dump(cases, open("/tmp/hv-nettle/cases.json","w"))
print("wrote", len(cases))
PY
```

Then run them:

```bash
python3 -c "
import json,subprocess
cases=json.load(open('/tmp/hv-nettle/cases.json'))
for name,cmd in sorted(cases.items()):
    p=subprocess.run(['bash','scripts/hv-guard-bash.sh'],input=json.dumps({'tool_input':{'command':cmd}}),capture_output=True,text=True)
    try: v=json.loads(p.stdout).get('hookSpecificOutput',{}).get('permissionDecision','allow')
    except Exception: v='allow'
    print('%-38s %s' % (name, v))
"
```

- [ ] **Step 2: Confirm the baseline is the expected RED, and that the harness discriminates**

Expected, before the fix:

```
A heredoc-prose-about-gate             deny    <- false positive, to be fixed
B grep-for-the-flag                    deny    <- false positive, to be fixed
C heredoc-mentions-flag                deny    <- false positive, to be fixed
D heredoc-mentions-twice               deny    <- false positive, to be fixed
E CONTROL real-workspace               deny    <- correct, must stay deny
F CONTROL real-bypass                  deny    <- correct, must stay deny
G CONTROL narrow-run                   allow   <- correct, must stay allow
H CONTROL real-cmd-after-heredoc       deny    <- correct, must stay deny
I CONTROL double-real-run              deny    <- correct, must stay deny
```

**STOP if G returns `deny`, or if E/F/H/I return `allow`.** That means the harness is not discriminating, not that the guard is wrong — an earlier version of this exact probe returned `allow` for every case because of nested-quoting mangling, which looked like a clean result. Fix the harness before touching the guard.

`H` is the load-bearing control for this task: it proves heredoc stripping does not swallow a real command that follows the heredoc.

- [ ] **Step 3: Add the projection to `scripts/hv-guard-bash.sh`**

Insert directly above `verdict()`, after the `deny()` helper:

```bash
# ---------------------------------------------------------------- projection
#
# The rules below must see the command being EXECUTED, not every string that
# appears in the command's text. Six observed false refusals came from that
# confusion: a heredoc body that is prose *about* the gate, and a `grep`
# searching *for* a flag, were both refused as though they were the thing they
# mentioned. The guard's own header says why that is the fatal direction — a
# guard that blocks legitimate work gets overridden into uselessness — whereas
# a missed detection is the failure mode it already accepts by design ("any
# internal error allows the command").
#
# Two strips, in this order:
#
#   1. HEREDOC BODIES. A heredoc body is data on some program's stdin; this
#      shell never executes it. The opener LINE is kept, because `cat > f
#      <<EOF` really does run `cat`.
#   2. QUOTED LITERALS. A quoted span is an argument, not a command.
#
# ACCEPTED HOLE, recorded so it is not rediscovered as a defect: `bash -c
# "cargo nextest run --workspace"` and `bash <<EOF … EOF` become invisible.
# Both are real and both are rare, and this is the direction the guard is
# built to fail in.
#
# Herestrings (`<<<`) are NOT heredocs and must not trigger the skip: after
# `<<` the next character is `<`, which matches neither the optional quote nor
# the delimiter's leading `[A-Za-z_]`, so the opener pattern cannot match one.
project() {
    local text="$1" out="" line trimmed delim="" in_body=0

    while IFS= read -r line; do
        if [[ "$in_body" -eq 1 ]]; then
            trimmed="${line#"${line%%[![:space:]]*}"}"
            trimmed="${trimmed%"${trimmed##*[![:space:]]}"}"
            if [[ "$trimmed" == "$delim" ]]; then
                in_body=0
            fi
            continue
        fi
        if [[ "$line" =~ \<\<-?[[:space:]]*[\'\"]?([A-Za-z_][A-Za-z0-9_]*)[\'\"]? ]]; then
            delim="${BASH_REMATCH[1]}"
            in_body=1
        fi
        out+="$line"$'\n'
    done <<<"$text"

    # Strip quoted spans in ONE alternating pass, so the quote that opens
    # FIRST wins. Two sequential passes are wrong, and the difference is not
    # subtle: stripping `'...'` before `"..."` treats any two apostrophes on a
    # line as a pair, with no idea they may sit inside unrelated double-quoted
    # strings. Measured against the two-pass version:
    #
    #   RAW:  echo "I don't think" && cargo nextest run --workspace \
    #                             && cargo nextest run --workspace \
    #                             && echo "you can't stop it"
    #   OUT:  echo
    #
    # Both real runs vanished between the apostrophes in "don't" and "can't",
    # and the guard returned `allow` -- a silent false NEGATIVE on exactly what
    # Rule 1 exists to catch, triggered by ordinary English contractions.
    # `sed -E` with an alternation takes the LEFTMOST match, so a `"` before a
    # `'` consumes its own span and scanning resumes after it.
    #
    # Line-oriented, which is sufficient: heredocs -- the only multi-line
    # quoting this guard has ever seen in practice -- are gone by now.
    # RESIDUAL LIMIT, accepted: a backslash-escaped quote inside a same-type
    # quoted span ends the span early. Strictly better than the two-pass form,
    # and the guard fails open.
    printf '%s' "$out" | sed -E "s/'[^']*'|\"[^\"]*\"/ /g"
}
```

- [ ] **Step 4: Call the projection from `verdict()`**

Replace the first line of `verdict()`'s body:

```bash
verdict() {
    local cmd="$1"
```

with:

```bash
verdict() {
    # Every rule below reasons about the command being executed, so it reads
    # the projection, never the raw text. See `project()` above.
    local cmd
    cmd="$(project "$1")"
```

- [ ] **Step 5: Re-run the probe harness and confirm the fix, controls first**

```bash
python3 -c "
import json,subprocess
cases=json.load(open('/tmp/hv-nettle/cases.json'))
for name,cmd in sorted(cases.items()):
    p=subprocess.run(['bash','scripts/hv-guard-bash.sh'],input=json.dumps({'tool_input':{'command':cmd}}),capture_output=True,text=True)
    try: v=json.loads(p.stdout).get('hookSpecificOutput',{}).get('permissionDecision','allow')
    except Exception: v='allow'
    print('%-38s %s' % (name, v))
"
```

Branch table — do not read this as a prediction, read it as the decision rule:

- A/B/C/D now `allow` **and** E/F/H/I still `deny` **and** G still `allow` → **the projection is correct; continue.**
- Any of E/F/H/I now `allow` → **STOP.** The projection is too aggressive and has removed a rule rather than narrowing it. Do not "fix" this by weakening a rule.
- Any of A/B/C/D still `deny` → the projection is not reaching that case. Diagnose `project()` directly: `bash -c 'source scripts/hv-guard-bash.sh 2>/dev/null; project "$(cat /tmp/case.txt)"'`.
- G now `deny` → the projection has broken the narrow-run allowance, which is design constraint 2 of the guard. **STOP.**

- [ ] **Step 6: Fold the new cases into the script's own `self_test()`**

The self-test is the durable half; the probe harness is scaffolding. Add to `self_test()`, immediately after the Rule 5 block and before the reason-TEXT check:

```bash
    # The PROJECTION — the rules must see the command executed, not every
    # string in the command text. Six false refusals on legitimate work came
    # from that confusion (The Nettle). Both directions, because a narrowing
    # that only ever allows has deleted the rule rather than narrowed it.
    check allow 'cat > /tmp/n.md <<EOF
The gate no longer runs cargo nextest run --workspace.
EOF'
    check allow 'grep -rn "no-verify" .claude/'
    check allow 'cat > /tmp/n.md <<EOF
CLAUDE.md forbids --no-verify.
EOF'
    check allow 'cat > /tmp/n.md <<EOF
First cargo nextest run --workspace, then cargo nextest run --workspace.
EOF'
    # The load-bearing control: a REAL command after a heredoc must still be
    # seen. If heredoc stripping ever swallows the rest of the input, this is
    # the case that catches it.
    check deny 'cat > /tmp/n.md <<EOF
prose
EOF
cargo nextest run --workspace'
    # A herestring is not a heredoc and must not start a body skip.
    check deny 'grep -q cargo <<<"$x"; cargo nextest run --workspace'
    # TWO quote-pairs on ONE line -- the case none of the above exercises. An
    # apostrophe inside a double-quoted string must not pair with a later one:
    # under the two-pass strip this returned `allow`, the whole command eaten
    # between the two apostrophes. This is the regression test for that.
    check deny 'echo "I don'"'"'t think" && cargo nextest run --workspace'
```

- [ ] **Step 7: Run the script's self-test**

Run: `bash scripts/hv-guard-bash.sh --self-test`
Expected: `hv-guard-bash: N/N cases pass`, with `N` larger than before by exactly the number of cases added.

If any pre-existing case fails, the projection broke a rule — return to Step 5's branch table.

- [ ] **Step 8: shellcheck and bash 3.2 compatibility**

```bash
shellcheck scripts/hv-guard-bash.sh
bash scripts/check-bash32.sh
```

Expected: both clean. **`check-bash32.sh` takes no file arguments** — it discovers its own file set from `git ls-files scripts`, so pass it nothing. (Consequence worth knowing: a script not yet `git add`-ed is invisible to it.)

**The constructs this task introduces are bash-3.2-safe, verified rather than assumed** — `BASH_REMATCH` and `[[ =~ ]]` appear nowhere else in `scripts/`, so there was no precedent to lean on. Tested directly against `/bin/bash` 3.2.57, which is what `check-bash32.sh` shells out to:

| construct | verdict |
|---|---|
| `${l#"${l%%[![:space:]]*}"}` whitespace trim | OK |
| `[[ =~ ]]` + `${BASH_REMATCH[1]}` on the heredoc opener | OK — extracted `EOF` |
| `out+="$line"$'\n'` string append | OK |
| the same regex against a **herestring** (`<<<"$v"`) | correctly does NOT match |

The last row is the one that matters for correctness, not just portability: it confirms `<<<` cannot start a body skip.

- [ ] **Step 9: Commit**

```bash
cargo fmt
make gate-commit
git add scripts/hv-guard-bash.sh
git commit -m "fix(guard): match the command executed, not the command text

The rules ran against the raw command string, so a quoted argument or a
heredoc body that merely MENTIONED a command was refused as though it were
that command. Six observed false refusals on legitimate work, three of them
generated while writing this campaign's own spec.

A projection now strips heredoc bodies and quoted literals before any rule
runs; the rules and their messages are untouched. Six self-test cases added,
in both directions -- including a real command after a heredoc, which is the
control proving the strip does not swallow what follows it, and a herestring,
which must not start a body skip.

Accepted hole, documented in the code: bash -c \"...\" and bash <<EOF become
invisible. That is the direction this guard is designed to fail in."
```

---

### Task 2: Uniquify the 14 fixed temp paths

A test writing to `std::env::temp_dir().join("<fixed name>")` shares that path with every concurrent run of the same test on the box. Observed 2026-09-02: `scene_surrounds_colour_cli.rs` asserted `new` succeeded, then panicked at `World::load` with `NotFound`, taking down a fail-fast run at test 274 of 4869. Two of the sites additionally `remove_dir_all` a fixed directory, which is destructive rather than merely colliding.

**The convention to follow is each file's own.** Repo-wide (worktree copies excluded): 66 `temp_dir()` sites, 50 already uniquified, and the dominant form is a `std::process::id()` in the name. `CARGO_TARGET_TMPDIR` (4 sites) is for files that must *persist* past the run — a different job. Do not introduce a new pattern.

**Files:**
- Modify: `cli/tests/suite/locale_cli.rs:19,34`
- Modify: `cli/tests/suite/scene_moons_cli.rs:19`
- Modify: `cli/tests/suite/scene_surrounds_colour_cli.rs:42`
- Modify: `windows/lab/src/domesday/comparators.rs:289,310,328`
- Modify: `tools/type-audit/src/walk.rs:181`
- Modify: `tools/placement-audit/src/walk.rs:138`
- Modify: `tools/digest/src/mcp.rs:68,90,105,116,138`

**Interfaces:**
- Consumes: nothing.
- Produces: the state Task 3's ratchet asserts — every `temp_dir()` site that *writes* carries a uniquifier. Task 3 is red on arrival without this.

**Do NOT change these two**, and understand why before starting:

- `windows/lab/tests/suite/anomaly_holdout.rs:160` — `hv-gnomon-holdout-path-check` is **never created**. The test asserts the path does not start with the committed goldens directory; it is pure path arithmetic. Changing it adds noise. Task 3 puts it in the ratchet's roster with that reason.
- `cli/tests/suite/repertory_corpus.rs:242` — a **false alarm**. `scratch_name()` at line 232 is `format!("hv-repertory-{}-{}-{}", std::process::id(), seed, n)`; the inner `script.txt` sits inside that already-unique directory.

- [ ] **Step 1: Capture the baseline count**

```bash
python3 - <<'PY'
import re, subprocess
files = subprocess.run(
    "grep -rln 'temp_dir()' --include=*.rs . | grep -v '^./target' | grep -v '^./.claude/worktrees'",
    shell=True, capture_output=True, text=True).stdout.split()
fixed = []
for f in sorted(files):
    lines = open(f).read().split('\n')
    for i, l in enumerate(lines):
        if 'temp_dir()' not in l or l.strip().startswith('//'):
            continue
        expr = '\n'.join(lines[i:i+6]); s = expr.find(';')
        if s > 0: expr = expr[:s]
        if not (('process::id' in expr) or re.search(r'\{n\}|\{tag\}|\{i\}|COUNTER|nanos|SystemTime', expr)):
            fixed.append("%s:%d" % (f, i+1))
print("fixed sites: %d" % len(fixed))
for s in fixed: print("  " + s)
PY
```

Expected: **16** sites — the 14 to fix, plus `anomaly_holdout.rs:160` and `repertory_corpus.rs:242`.

Branch table:

- exactly 16, and the two exceptions are among them → proceed.
- more than 16 → main has moved and a new site landed. Fix the new one too and say so in the commit message; add it to this list.
- fewer than 16 → a site was fixed by another campaign. Confirm which, and drop it from the list rather than assuming the classifier is broken.

- [ ] **Step 2: Fix the three `cli/` test files**

**Collapse each pair into one statement**, deleting the now-unused `let dir = …` binding. `dir` has no other use in any of these four sites (verify with `grep -n '\bdir\b' <file>` before deleting). This is not cosmetic: Task 3's scanner cuts the expression at the first `;`, so a uniquifier on the *next* statement is invisible to it and the site would still be reported as fixed. Keeping `temp_dir()` and its uniquifier in one statement is what makes the site checkable, and it matches the repo's dominant one-liner form.

`cli/tests/suite/locale_cli.rs` — two sites. Lines 19-20 become:

```rust
    let world =
        std::env::temp_dir().join(format!("hv-locale-test-{}.json", std::process::id()));
```

Lines 34-35 become:

```rust
    let world =
        std::env::temp_dir().join(format!("hv-locale-test2-{}.json", std::process::id()));
```

`cli/tests/suite/scene_moons_cli.rs` lines 19-20 become:

```rust
    let world =
        std::env::temp_dir().join(format!("hv-scene-moons-test-{}.json", std::process::id()));
```

`cli/tests/suite/scene_surrounds_colour_cli.rs` lines 42-43 become:

```rust
    let world_path = std::env::temp_dir().join(format!(
        "hv-scene-surrounds-colour-test-{}.json",
        std::process::id()
    ));
```

- [ ] **Step 3: Fix `windows/lab/src/domesday/comparators.rs` — three sites**

Match the file's own neighbours at lines 239 and 264, which already use `format!("hv-domesday-…-{}", std::process::id())`. Line 289, 310, 328 respectively:

```rust
        let dir = std::env::temp_dir().join(format!("armature-dir-test-{}", std::process::id()));
```

```rust
        let dir = std::env::temp_dir().join(format!("armature-none-test-{}", std::process::id()));
```

```rust
        let dir = std::env::temp_dir().join(format!("armature-ok-test-{}", std::process::id()));
```

- [ ] **Step 4: Fix the two `remove_dir_all` sites in `tools/`**

These are the destructive pair: each `remove_dir_all`s a fixed directory before creating it, so two concurrent runs have one wiping the other's tree mid-test.

`tools/type-audit/src/walk.rs` line 181:

```rust
        let dir = std::env::temp_dir().join(format!(
            "type_audit_walk_exclusion_test_{}",
            std::process::id()
        ));
```

`tools/placement-audit/src/walk.rs` line 138:

```rust
        let dir = std::env::temp_dir().join(format!(
            "placement_audit_walk_exclusion_test_{}",
            std::process::id()
        ));
```

- [ ] **Step 5: Fix the five sites in `tools/digest/src/mcp.rs`**

Lines 68, 90, 105, 116, 138 — each is `std::env::temp_dir().join("<name>")` followed by `create_dir_all` and a write to `facts.jsonl`:

```rust
        let dir = std::env::temp_dir().join(format!("digest-mcp-test-{}", std::process::id()));
```

```rust
        let dir = std::env::temp_dir().join(format!("digest-mcp-test2-{}", std::process::id()));
```

```rust
        let dir = std::env::temp_dir().join(format!("digest-mcp-test-notfound-{}", std::process::id()));
```

```rust
        let dir = std::env::temp_dir().join(format!("digest-mcp-test-zero-subject-{}", std::process::id()));
```

```rust
        let dir = std::env::temp_dir().join(format!("digest-mcp-test-unreadable-{}", std::process::id()));
```

- [ ] **Step 6: Re-run the classifier and confirm only the two exceptions remain**

Re-run Step 1's script.

Expected: **2** sites — `windows/lab/tests/suite/anomaly_holdout.rs:160` and `cli/tests/suite/repertory_corpus.rs:242`, and nothing else.

If a site you edited is *still* listed, the uniquifier landed on a later statement than the `temp_dir()` call and the classifier cut the expression at the `;` between them. Collapse it into one statement, as Step 2 describes — do not widen the classifier.

- [ ] **Step 7: Run the affected tests**

The three tool crates are outside the cargo workspace and have their own manifests:

```bash
cargo test --manifest-path tools/type-audit/Cargo.toml 2>&1 | tail -5
cargo test --manifest-path tools/placement-audit/Cargo.toml 2>&1 | tail -5
cargo test --manifest-path tools/digest/Cargo.toml 2>&1 | tail -5
```

Then the workspace ones, scoped:

```bash
cargo nextest run -p hornvale --test suite -E 'test(locale_cli) or test(scene_moons_cli) or test(scene_surrounds_colour_cli)' 2>&1 | tail -5
cargo nextest run -p hornvale-lab -E 'test(comparators)' 2>&1 | tail -5
```

Expected: all pass. If a `tools/` suite was already red before this change, say so in the report rather than attributing it here — confirm by stashing nothing and testing at `HEAD~` for that file only.

- [ ] **Step 8: Demonstrate the fix actually addresses concurrency**

A green suite does not prove the collision is gone — the collision needs two processes. Run one of the fixed tests twice concurrently and confirm both pass:

```bash
cargo nextest run -p hornvale --test suite -E 'test(scene_surrounds_colour_cli)' > /tmp/hv-a.log 2>&1 &
cargo nextest run -p hornvale --test suite -E 'test(scene_surrounds_colour_cli)' > /tmp/hv-b.log 2>&1 &
wait
tail -3 /tmp/hv-a.log; tail -3 /tmp/hv-b.log
```

Expected: both report all tests passed. Report the observed result either way — if it passes for a reason other than the fix (e.g. nextest serialising them), say so rather than claiming the fix was demonstrated.

- [ ] **Step 9: Commit**

```bash
cargo fmt
make gate-commit
git add cli/tests/suite/locale_cli.rs cli/tests/suite/scene_moons_cli.rs \
        cli/tests/suite/scene_surrounds_colour_cli.rs \
        windows/lab/src/domesday/comparators.rs \
        tools/type-audit/src/walk.rs tools/placement-audit/src/walk.rs \
        tools/digest/src/mcp.rs
git commit -m "fix(tests): uniquify 14 fixed temp paths

Each wrote to a fixed name under std::env::temp_dir(), shared by every
concurrent run of the same test on the box. Observed 2026-09-02:
scene_surrounds_colour_cli asserted 'new' succeeded and then panicked at
World::load with NotFound, taking a fail-fast run down at test 274 of 4869.

Two of the sites -- type-audit and placement-audit's walk tests --
remove_dir_all a fixed directory before creating it, so concurrent runs
destroyed each other's fixtures rather than merely sharing a path.

Each site takes the shape its own file already uses (a std::process::id() in
the name; 50 of the repo's 66 sites already do). Two sites deliberately
untouched: anomaly_holdout.rs:160 never creates its path, and
repertory_corpus.rs:242 is already uniquified via scratch_name()."
```

---

### Task 3: A ratchet so a fixed temp path cannot re-accrete

A fixed temp path is greppable, which makes it the class where a standing guard is cheap and permanent — it converts a one-off observation into a check that cannot go stale. Task 2 fixed 14 sites; nothing stops a fifteenth.

Follow `cli/tests/suite/test_binary_ratchet.rs` exactly. Read it first — the three properties that make it work are deliberate:

- **Three-valued, not a wall.** A frozen roster of accepted sites, so the guard is not red on day one and trained away by day two.
- **Both directions.** A new ununiquified site reddens; a roster entry whose site no longer exists *also* reddens, so the list cannot rot.
- **Explicit directory enumeration, never a walk from the root.** That is what keeps `.claude/worktrees/` out — those copies would otherwise multiply every finding by the number of live worktrees.

**Files:**
- Create: `cli/tests/suite/temp_path_ratchet.rs`
- Create: `cli/tests/fixtures/fixed-temp-paths.txt`
- Modify: `cli/tests/suite.rs` (declare the module)

**Interfaces:**
- Consumes: Task 2's state — every writing site uniquified.
- Produces: nothing later tasks rely on.

- [ ] **Step 1: Write the roster fixture**

`cli/tests/fixtures/fixed-temp-paths.txt` — one entry per line as `<path>::<joined expression>` TAB `<reason>`; `#` comments and blank lines ignored:

```
# Sites that construct a FIXED name under std::env::temp_dir() and are
# accepted anyway. Append with a reason in the same commit; the guard checks
# both directions, so an entry whose site no longer matches is an error too
# and this list cannot rot.
#
# The default is NOT to be here: a fixed temp path is shared by every
# concurrent run of the same test on the box, and 14 of them were fixed by
# The Nettle after one took down a fail-fast run at test 274 of 4869.
#
# KEYED BY THE JOINED EXPRESSION, NOT BY LINE NUMBER. A roster keyed by line
# drifts the moment anyone edits above the site, turning every unrelated edit
# into a stale-entry failure -- which is exactly how a three-valued ratchet
# gets trained away.
windows/lab/tests/suite/anomaly_holdout.rs::"hv-gnomon-holdout-path-check"	never creates the path -- asserts it does NOT start with the committed goldens dir; pure path arithmetic
cli/tests/suite/repertory_corpus.rs::scratch_name(witness.seed)	already uniquified -- scratch_name() embeds std::process::id(); the joined value is a fn call, which this syntactic check cannot see through
```

- [ ] **Step 2: Write the failing test**

`cli/tests/suite/temp_path_ratchet.rs`:

```rust
//! The fixed-temp-path ratchet: a test writing to a FIXED name under
//! `std::env::temp_dir()` shares that path with every concurrent run of the
//! same test on the box.
//!
//! **Observed, 2026-09-02** (The Reservoir, parked finding P1):
//! `scene_surrounds_colour_cli.rs` asserted `new` succeeded — so the CLI had
//! written the file — then panicked at `World::load` with
//! `Os { code: 2, kind: NotFound }`. Under the default fail-fast profile that
//! took the whole run down at test 274 of 4869. Two further sites
//! (`type-audit`, `placement-audit`) `remove_dir_all`ed a fixed directory
//! before creating it, so concurrent runs destroyed each other's fixtures.
//!
//! The Nettle fixed 14 sites. **Nothing stopped a fifteenth**, which is what
//! this ratchet is for — the same three-valued shape `tropes check`, the
//! timings baseline and type-audit's `waiver(...)` use. A guard that failed on
//! the mere existence of an accepted site would be red on day one and
//! trained-away by day two.
//!
//! **Direction this check enforces:** every `temp_dir()` site whose joined
//! name is a literal without a uniquifier must be declared. It is a
//! *syntactic* check over the joined expression, so it is blind to a site
//! that hides the name behind a function call — `repertory_corpus.rs:242` is
//! exactly that, and is declared for that reason rather than because it is
//! unsafe.
//!
//! **To accept a site**, append `<path>::<joined expression>` TAB `<reason>` to
//! `cli/tests/fixtures/fixed-temp-paths.txt` in the same commit. **To remove
//! one**, uniquify the site and delete its line — the check runs both
//! directions, so a stale entry fails too.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

/// The frozen roster. Append-with-reason; never edit in bulk.
const FROZEN: &str = include_str!("../fixtures/fixed-temp-paths.txt");

/// Directories scanned for `temp_dir()` sites, relative to the repo root.
///
/// Enumerated explicitly rather than walked from the root, which is what
/// keeps `.claude/worktrees/` out: a blind walk would report every finding
/// once per live worktree. `tools/` and `clients/` are included even though
/// they sit outside the cargo workspace — two of the fixed sites lived in
/// `tools/`, and `clients/` has four files using `temp_dir()` (all currently
/// uniquified, so including it costs nothing today and covers a future one).
fn scanned_dirs(root: &Path) -> Vec<PathBuf> {
    let mut out = vec![root.join("kernel"), root.join("cli")];
    for parent in ["domains", "windows", "tools", "clients"] {
        let Ok(entries) = std::fs::read_dir(root.join(parent)) else {
            continue;
        };
        let mut kids: Vec<PathBuf> = entries
            .filter_map(Result::ok)
            .map(|e| e.path())
            .filter(|p| p.is_dir())
            .collect();
        kids.sort();
        out.extend(kids);
    }
    out
}

/// Does this joined expression carry a uniquifier?
///
/// Mirrors the conventions actually in the tree: a `std::process::id()`, a
/// per-test tag or counter interpolation, or a nanosecond clock.
fn is_uniquified(expr: &str) -> bool {
    expr.contains("process::id")
        || expr.contains("{n}")
        || expr.contains("{tag}")
        || expr.contains("{i}")
        || expr.contains("COUNTER")
        || expr.contains("nanos")
        || expr.contains("SystemTime")
}

/// Every `.rs` file under `dir`, recursively, skipping `target/`.
fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut kids: Vec<PathBuf> = entries.filter_map(Result::ok).map(|e| e.path()).collect();
    kids.sort();
    for path in kids {
        if path.is_dir() {
            if path.file_name().is_some_and(|n| n == "target") {
                continue;
            }
            rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// A stable identity for a site: the file, then the expression joined onto
/// `temp_dir()`, whitespace-collapsed.
///
/// **Deliberately not the line number.** A roster keyed by line drifts the
/// moment anyone edits above the site, so every unrelated edit would raise a
/// stale-entry failure — which is precisely how a three-valued ratchet gets
/// trained away. The joined expression is what the acceptance is actually
/// *about*, and it moves only when the site itself does.
fn site_key(rel: &str, window: &str) -> String {
    let snippet = match window.find(".join(") {
        Some(n) => {
            let rest = &window[n + ".join(".len()..];
            let mut depth = 1usize;
            let mut end = rest.len();
            for (i, c) in rest.char_indices() {
                match c {
                    '(' => depth += 1,
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            end = i;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            rest[..end].to_string()
        }
        // No `.join(` in the window — the call's result is bound and used
        // later. Fall back to the line itself, which still identifies it.
        None => window.lines().next().unwrap_or("").trim().to_string(),
    };
    let collapsed = snippet.split_whitespace().collect::<Vec<_>>().join(" ");
    format!("{rel}::{}", collapsed.chars().take(70).collect::<String>())
}

/// `<path>::<joined expression>` for every site whose joined name has no
/// uniquifier.
fn fixed_sites(root: &Path) -> BTreeSet<String> {
    let mut found = BTreeSet::new();
    for dir in scanned_dirs(root) {
        let mut files = Vec::new();
        rs_files(&dir, &mut files);
        for file in files {
            let Ok(src) = std::fs::read_to_string(&file) else {
                continue;
            };
            let lines: Vec<&str> = src.lines().collect();
            for (i, line) in lines.iter().enumerate() {
                if !line.contains("temp_dir()") || line.trim_start().starts_with("//") {
                    continue;
                }
                // The joined expression can wrap across lines; take this line
                // plus the next few, cut at the statement's `;`.
                let window = lines[i..(i + 6).min(lines.len())].join("\n");
                let expr = match window.find(';') {
                    Some(n) => &window[..n],
                    None => &window[..],
                };
                if is_uniquified(expr) {
                    continue;
                }
                let rel = file
                    .strip_prefix(root)
                    .expect("scanned path is under the repo root");
                let rel = rel.to_string_lossy().replace('\\', "/");
                found.insert(site_key(&rel, expr));
            }
        }
    }
    found
}

/// The roster's declared sites, as `<path>::<expr>` with the reason dropped.
fn declared() -> BTreeSet<String> {
    FROZEN
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| l.split('\t').next().unwrap_or(l).trim().to_string())
        .collect()
}

#[test]
fn a_reasonless_roster_entry_is_a_parse_error() {
    let offenders: Vec<&str> = FROZEN
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .filter(|l| l.split('\t').nth(1).is_none_or(|r| r.trim().is_empty()))
        .collect();
    assert!(
        offenders.is_empty(),
        "every roster entry needs `<path>::<expr>` TAB `<reason>`; a reasonless \
         acceptance can only ever be satisfied, so it rots:\n{offenders:?}"
    );
}

#[test]
fn no_new_fixed_temp_path_appears() {
    let root = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/..")); // cli/ -> repo root
    let found = fixed_sites(root);
    let frozen = declared();

    let added: Vec<&String> = found.difference(&frozen).collect();
    assert!(
        added.is_empty(),
        "new fixed temp path(s) — a fixed name under std::env::temp_dir() is \
         shared by every concurrent run of the same test on the box, and one \
         took a fail-fast run down at test 274 of 4869:\n{}\n\nUniquify it the \
         way its own file already does (a std::process::id() in the name; 50 of \
         the repo's 66 sites do). If the site genuinely cannot collide — it \
         never creates the path, or the name is built by a helper this \
         syntactic check cannot see through — append `<path>::<expr>` TAB \
         `<reason>` to cli/tests/fixtures/fixed-temp-paths.txt in the same \
         commit. The key is the joined expression, not a line number, so it \
         survives edits above the site.",
        added
            .iter()
            .map(|s| format!("  {s}"))
            .collect::<Vec<_>>()
            .join("\n")
    );

    let stale: Vec<&String> = frozen.difference(&found).collect();
    assert!(
        stale.is_empty(),
        "roster entr(y/ies) that no longer match a fixed temp path — the site \
         was uniquified, moved, or its joined expression changed. Delete the \
         line, or correct it to the new expression:\n{}",
        stale
            .iter()
            .map(|s| format!("  {s}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
}
```

- [ ] **Step 3: Declare the module**

In `cli/tests/suite.rs`, in the alphabetically-correct position among the existing declarations (between `system_coverage` and `test_binary_ratchet` — verify against the file, do not assume):

```rust
#[path = "suite/temp_path_ratchet.rs"]
mod temp_path_ratchet;
```

- [ ] **Step 4: Run it and confirm GREEN**

Run: `cargo nextest run -p hornvale --test suite -E 'test(temp_path_ratchet)' 2>&1 | tail -6`
Expected: 2 tests pass.

Branch table:

- both pass → proceed to Step 5.
- `no_new_fixed_temp_path_appears` reports **added** sites → Task 2 missed one, or main moved. Fix the site (preferred) or declare it with a reason; do not widen `is_uniquified` to make the failure go away.
- it reports **stale** entries → a declared expression no longer matches. Correct the roster to the real expression, or delete the line if the site is gone.

- [ ] **Step 5: Prove the ratchet discriminates — the added direction**

A guard that has only been seen to pass is not known to work. Introduce a violation, confirm RED, then remove it:

```bash
cat >> cli/tests/suite/locale_cli.rs <<'EOF'

#[test]
#[ignore = "nettle: temporary ratchet probe, deleted in the next step"]
fn nettle_ratchet_probe() {
    let _ = std::env::temp_dir().join("hv-nettle-probe-fixed");
}
EOF
cargo nextest run -p hornvale --test suite -E 'test(temp_path_ratchet)' 2>&1 | tail -20
```

Expected: `no_new_fixed_temp_path_appears` FAILS, naming
`cli/tests/suite/locale_cli.rs::"hv-nettle-probe-fixed"`.

Then revert:

```bash
git checkout -- cli/tests/suite/locale_cli.rs
cargo nextest run -p hornvale --test suite -E 'test(temp_path_ratchet)' 2>&1 | tail -4
```

Expected: back to 2 passed.

**If the probe did NOT fail, stop and diagnose the scanner** — most likely `scanned_dirs` is not reaching the file, or the `;` window cut the expression short. A ratchet that cannot see a violation in a file it is supposed to scan is worse than none, because it sits in a healthy-looking suite.

- [ ] **Step 6: Prove the ratchet discriminates — the stale direction**

**Mutating an existing roster entry does NOT test this arm, and an earlier
draft of this step wrongly said it did.** Editing a declared key makes both
differences non-empty at once — the real site becomes *added* (it is no longer
declared) and the edited line becomes *stale* — and `added` is asserted first,
so the test reddens under the added heading and the stale assertion never
executes. The stale arm would have gone unobserved while looking tested.

So probe it by **adding a bogus entry and leaving both real ones intact**.
Then `found - frozen` is empty (nothing added) and `frozen - found` holds
exactly the bogus key, which is the only way to reach the second assertion:

```bash
python3 -c "
p='cli/tests/fixtures/fixed-temp-paths.txt'
s=open(p).read()
sentinel='cli/tests/suite/repertory_corpus.rs::scratch_name(witness.seed)'
assert sentinel in s, 'TARGET NOT FOUND -- do not proceed on a no-op mutation'
open(p,'a').write('windows/lab/src/nonexistent.rs::\"hv-nettle-stale-probe\"\tstale-direction probe, deleted in the next step\n')
print('appended a bogus entry; both real entries untouched')
"
cargo nextest run -p hornvale --test suite -E 'test(temp_path_ratchet)' 2>&1 | tail -20
git checkout -- cli/tests/fixtures/fixed-temp-paths.txt
cargo nextest run -p hornvale --test suite -E 'test(temp_path_ratchet)' 2>&1 | tail -4
```

Expected: `no_new_fixed_temp_path_appears` FAILS **under the stale heading**,
naming `windows/lab/src/nonexistent.rs::"hv-nettle-stale-probe"` — then green
again after the revert.

The `assert sentinel in s` is load-bearing in a different way here than in a
substitution: it proves the file you appended to is the roster you think it is,
so an append into the wrong file cannot pass as a probe.

**Branch table:**

- fails under the **stale** heading, naming the bogus key → **both arms are now
  proven; continue.**
- fails under the **added** heading → the append perturbed the found set, which
  should be impossible for a path that does not exist. Read the failure; do not
  proceed.
- does not fail at all → `declared()` is not parsing the appended line (check
  the tab separator) or `stale` is computed against the wrong set. This is the
  arm that would otherwise never be exercised, so a silent pass here is the
  worst outcome available.

- [ ] **Step 7: Commit**

Do **not** hand-edit `docs/timings/subfloor-roster.tsv`. The chamber's `gate` phase rewrites it on a green run and commits it like any other tracked drift.

```bash
cargo fmt
make gate-commit
git add cli/tests/suite/temp_path_ratchet.rs cli/tests/fixtures/fixed-temp-paths.txt cli/tests/suite.rs
git commit -m "test(cli): ratchet against a fixed temp path re-accreting

Task 2 fixed 14 sites; nothing stopped a fifteenth. A fixed temp path is
greppable, which makes it the class where a standing guard converts a one-off
observation into a check that cannot go stale.

Same three-valued shape as test_binary_ratchet: a frozen roster with a
mandatory reason (reasonless is a failure -- a one-directional acceptance can
only ever be satisfied, so it rots), and both directions checked, so an entry
whose site no longer matches fails too.

Directories are enumerated explicitly rather than walked from the root, which
keeps .claude/worktrees/ out; a blind walk reported every finding once per
live worktree. tools/ is scanned even though those crates are outside the
cargo workspace, because two of the fixed sites lived there.

Shown to discriminate in both directions, not merely to pass: an introduced
fixed path reddens the added arm, and a mutated roster line reddens the stale
arm. Roster starts at two entries, each a site that cannot collide."
```

---

### Task 4: Route followups to the committed ledger in `campaign-autopilot`

**Read the spec's §3 before starting.** The defect the brief describes — the skill naming `.superpowers/sdd/decision-ledger.md` — **is already fixed** at `3b9a026aa` (2026-08-30, The Cartulary). Line 190 already reads `docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md`, and no loaded skill anywhere names the superseded path. Do not "fix" it again.

The live defect is three lines up, at 181-183 in *Capture discipline*:

> - Actionable followups → the campaign followup register
>   (`.superpowers/sdd/followups.md` in the worktree; promoted into the
>   campaign retrospective's follow-up section at close).

That path carries **both** defects The Cartulary fixed for rulings: it is flat and shared — decision 0493's exact shape, and two such files exist in this checkout today, one in the shared main checkout — and it is **promoted at close**, the practice that failed five recorded times (The Ell, The Quoin, The Gallery/Lodestar, The Overture, The Attestation). It is the mechanism behind The Reservoir's ledger #20, where four deferred minors lived only in git-ignored scratch and were recovered by a closer's backstop walk rather than by the process.

**Files:**
- Modify: `.claude/skills/campaign-autopilot/SKILL.md:174-186` (the *Capture discipline* section)

**Interfaces:**
- Consumes: nothing.
- Produces: nothing. Task 6 files the registry rows that reference this change.

- [ ] **Step 1: Confirm the premise before changing anything**

```bash
grep -n "superpowers/ledgers\|followups.md" .claude/skills/campaign-autopilot/SKILL.md
grep -rn "decision-ledger.md" .claude/skills/ ~/.claude/plugins/cache/claude-plugins-official/superpowers/6.3.0/skills/ 2>/dev/null | grep -v Binary
ls .superpowers/sdd/followups.md 2>/dev/null && echo "the shared file exists HERE, in this checkout"
```

Branch table:

- line ~190 names `docs/superpowers/ledgers/`, line ~183 names `.superpowers/sdd/followups.md`, and the second grep returns nothing → **the premise holds; proceed.**
- the second grep returns a hit in any skill → a genuinely superseded ledger path survives somewhere. Fix that too and report it; the spec's §3 claim was incomplete.
- line ~183 no longer names `followups.md` → another campaign fixed it. **STOP and report**; there is nothing to do here.

- [ ] **Step 2: Rewrite the Capture discipline bullet**

Replace the `Actionable followups` bullet:

```markdown
- Actionable followups → the campaign's committed decision ledger
  (`docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md`), written **as they
  occur**, in a `## Follow-ups` section.
```

- [ ] **Step 3: Add the sentence naming why the two files differ**

The distinction is what nothing currently makes visible *at the moment of writing* — ledger #20's own generalisable lesson: "Two files with different lifetimes sat open in the same session and I wrote to whichever was nearer the thought." Append immediately after the Capture discipline bullet list:

```markdown
**Which file, and why it is not a matter of taste.** Two files are open in
every campaign session and they have different *lifetimes*:

| goes here | what for | lifetime |
|---|---|---|
| the plugin's `.superpowers/sdd/<campaign>/progress.md` | task state, fix rounds, resume-after-compaction material | **scratch** — git-ignored, per-worktree, swept when the worktree is recycled |
| `docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md` | rulings, `Q` answers, deferred minors, parked findings, followups | **committed**, per-campaign, survives everything |

Writing task state to `progress.md` is correct. Writing anything in the
second row there is a silent loss, and it is not a hypothetical: The
Reservoir wrote four deferred minors into `progress.md` while writing
rulings into the committed ledger *in the same sitting*, because the two
files were both open and neither one announces which it is. A closer's
step-2A walk is the backstop and it worked; the improvement worth wanting is
that it should not have had to.

**Never `.superpowers/sdd/followups.md`.** It is flat and *shared* — every
campaign's copy sat at that identical path, so two campaigns editing it
merged to one side silently (decision 0493) — and it was promoted at close,
the practice that failed five recorded times before The Cartulary replaced
it with writing to the committed ledger as rulings happen.
```

- [ ] **Step 4: Verify no reference to the shared path survives in any skill**

```bash
grep -rn "sdd/followups.md" .claude/skills/ && echo "STILL PRESENT -- fix it" || echo "clean"
grep -rn "followup" .claude/skills/campaign-autopilot/SKILL.md
```

Expected: `clean`, and the remaining `followup` hits all point at the committed ledger.

- [ ] **Step 5: Check the sibling skill agrees**

`closing-a-campaign` step 2B already routes "every deferred minor recorded anywhere in the ledger" to a home, and its Quick Reference table already names the committed ledger. Confirm it does not now contradict the edit:

```bash
grep -n "followups\|deferred minor" .claude/skills/closing-a-campaign/SKILL.md
```

Branch table:

- no mention of `.superpowers/sdd/followups.md` → nothing to change; note it in the report.
- it names the shared path → fix it here in the same commit; a two-skill disagreement is what produced this defect class.

- [ ] **Step 6: Commit**

`.claude/` is not a Rust path, so `pre-commit` will fast-path past `gate-commit` and print `no Rust-relevant paths staged` — which is Task 5's defect. Run the docs tests by hand, as the current mitigation requires:

```bash
cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)' 2>&1 | tail -4
git add .claude/skills/campaign-autopilot/SKILL.md
git commit -m "fix(autopilot): followups go to the committed ledger, not shared scratch

The stated defect -- the skill naming .superpowers/sdd/decision-ledger.md --
was already fixed at 3b9a026aa, before the brief that reported it was written.
The live one was three lines up: Capture discipline routed actionable
followups to .superpowers/sdd/followups.md, which carries BOTH defects The
Cartulary fixed for rulings. It is flat and shared (decision 0493's exact
shape; two such files exist in this checkout), and it was promoted at close --
the practice that failed five recorded times.

It is the mechanism behind The Reservoir's ledger #20: four deferred minors
that lived only in git-ignored scratch, recovered by a closer's backstop walk
rather than by the process.

Also adds the table naming WHY the two files differ, because nothing made
that visible at the moment of writing -- which is ledger #20's own lesson:
'Two files with different lifetimes sat open in the same session and I wrote
to whichever was nearer the thought.'"
```

---

### Task 5: Run the prose-subject tests on a docs-only commit

`scripts/hooks/pre-commit` fast-paths past `make gate-commit` when no Rust-relevant path is staged. The premise is "Rust paths staged → Rust checks matter"; the counterexample is **a Rust test whose subject is prose**. `docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary` is in the sub-floor roster, would have caught a `MAP-25` citation in a chronicle, and never ran across roughly twelve docs-only commits in one campaign. The merge queue finds it instead, on the canonical box, after taking the shared serial claim.

**The cost is bimodal, and the registry row's figure was the wrong mode.** Measured on this Mac with a warm 36 GB `target/`, `cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)'`, 28 tests:

```
condition                                    wall        test execution
-------------------------------------------  ----------  --------------
after a 137-commit absorption (suite cold)    234.6 s          2.094 s
immediately after, binary warm                  2.443 s        1.324 s
```

The `~1 s` in `PROC-docs-only-commits-skip-a-gate-that-checks-docs` is the execution figure; the wall is the suite binary's build. Nathan approved paying it (ledger #7): a campaign's docs commits cluster after one absorption, so realistic cost is the cold build **once** plus ~2.4 s each thereafter (~260 s per cycle for twelve commits) against a merge phase reddening on the shared box at ~1129–1595 s.

**Files:**
- Modify: `scripts/hooks/pre-commit:339-347` (the fast path)
- Modify: `Makefile` (add a `docs-tests` target)

**Interfaces:**
- Consumes: nothing.
- Produces: `make docs-tests` — a target the hook calls and a human can run directly.

- [ ] **Step 1: Derive the test set from what each test READS**

The set is **not** given here, and its absence is deliberate rather than an unfinished step. A plan author does not know which tests read prose; the implementer does, after reading them. Prescribing a set from outside the code is how a plan ships a wrong list that then looks authoritative — so this step names the *property* the set must have and the command that decides it, and the filter it produces is what Steps 2-3 consume. `docs_consistency` is the known member. A grep for `docs/` or `book/src` matches 24 of the 50 files in `cli/tests/suite/`, but mentioning a path in a comment is not reading it.

For each of those 24 files, determine whether it actually opens a file under `docs/` or `book/`:

```bash
cd cli/tests/suite
for f in $(grep -ln 'docs/\|book/src' *.rs); do
  n=$(grep -cE 'read_to_string|include_str!|read_dir|File::open|fs::read' "$f")
  printf '%-34s io-calls=%s\n' "$f" "$n"
done
cd ../../..
```

Then, for each file with `io-calls > 0`, read the actual paths it opens and record them.

Report the derived set as a table of `file → the prose paths it reads`, and classify by:

- reads only `docs/` or `book/` inputs → **in the set**
- reads a mix of prose and Rust sources → **in the set** (a docs-only commit can still move its verdict)
- reads no prose input → **out**, and say which grep hit put it on the candidate list, so the next reader knows it was considered

**STOP and report if** the derived set exceeds ~40 tests, or its warm wall exceeds ~10 s. The cheap shape has stopped being cheap and the scope call returns to Nathan.

- [ ] **Step 2: Measure the derived set's warm wall**

With the suite binary already built, run the derived filter and record the wall. Example shape, with the real filter substituted:

```bash
time cargo nextest run -p hornvale --test suite -E 'test(docs_consistency) or test(<others derived in Step 1>)' 2>&1 | tail -4
```

Record the number in the task report and in the commit message. Do not reuse the 2.443 s figure above unless the derived set is exactly `docs_consistency`.

- [ ] **Step 3: Add the `docs-tests` Makefile target**

Place it next to the other check targets, following the file's `##`-comment convention so `make help` lists it:

```makefile
docs-tests: ## The prose-subject tests -- run by pre-commit when only docs are staged
	@cargo nextest run -p hornvale --test suite -E '<the derived filter from Step 1>'
```

- [ ] **Step 4: Write the failing positive control BEFORE changing the hook**

This is the step that proves the change works. Reproduce the original defect — a registry ID cited from `book/src/chronicle/`, which decision 0031 permits only from `book/src/frontier/`:

```bash
git checkout -b nettle-probe-throwaway
BOOKFILE=$(ls book/src/chronicle/*.md | head -1)
printf '\nA probe sentence citing MAP-25 from a chronicle.\n' >> "$BOOKFILE"
git add "$BOOKFILE"
git commit -m "probe: a docs-only commit carrying a prose defect" 2>&1 | tail -12
```

Expected **before** the hook change: the commit **SUCCEEDS**, and the hook prints `no Rust-relevant paths staged — skipping 'make gate-commit'`. That is the defect, reproduced.

Confirm the test really would have caught it:

```bash
cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)' 2>&1 | tail -12
```

Expected: `the_book_carries_no_registry_ids_or_process_vocabulary` FAILS, naming the file.

**If the test passes here, STOP.** Either the probe sentence does not trip it (adjust the probe — read the test's own matcher to see what it looks for) or the test does not do what §4 claims. Do not proceed to change the hook on an unproven premise.

Then reset:

```bash
git reset --hard HEAD~1
git checkout campaign/the-nettle
git branch -D nettle-probe-throwaway
```

- [ ] **Step 5: Invert the fast path in `scripts/hooks/pre-commit`**

Replace:

```bash
if ! git diff --cached --name-only --diff-filter=ACMR | grep -qE "$rust_relevant"; then
    echo "pre-commit: no Rust-relevant paths staged — skipping 'make gate-commit'." >&2
    exit 0
fi
```

with:

```bash
# THE FAST PATH IS NOW AN INVERSION, NOT A SKIP (The Nettle).
#
# The old premise was "Rust paths staged -> Rust checks matter". The
# counterexample is a Rust test whose SUBJECT IS PROSE:
# `docs_consistency` scans `book/src` and `docs/`, is in the sub-floor
# roster, and so runs in `gate-commit` — and a docs-only commit is exactly
# what it exists to check and exactly what this predicate decided needed no
# checking. One campaign made ~12 docs-only commits through the hole, each
# printing a skip message that reads as an optimisation, and a one-sentence
# prose defect went to the merge queue to be found on the canonical box
# after it had taken the shared serial claim.
#
# So a docs-only commit now runs the PROSE-SUBJECT tests instead of nothing.
# It does NOT run `make gate-commit`: fmt, clippy and the type audit cannot
# be moved by a markdown edit, and that half of the old reasoning was right.
#
# COST, measured 2026-09-02 (The Nettle, spec §4): ~2.4 s with the suite
# binary warm; ~234.6 s when it is cold, because the wall is the BUILD, not
# the 2.1 s of test execution. The cold case is not rare — a chronicle or
# retrospective commit right after absorbing main is exactly when the tree
# is cold. It is paid deliberately: a campaign's docs commits cluster after
# one absorption, so the cost is the cold build once plus ~2.4 s each
# thereafter, against a merge phase reddening at ~1129-1595 s on the box
# everything else is queued behind.
if ! git diff --cached --name-only --diff-filter=ACMR | grep -qE "$rust_relevant"; then
    echo "pre-commit: no Rust-relevant paths staged — running the prose-subject tests instead of 'make gate-commit'." >&2
    if ! make docs-tests; then
        echo "pre-commit: 'make docs-tests' failed — a staged docs change breaks a test whose subject is prose." >&2
        echo "pre-commit: this is the check the merge queue would otherwise find on the canonical box." >&2
        exit 1
    fi
    exit 0
fi
```

- [ ] **Step 6: Re-run the positive control and confirm the hook now REFUSES**

```bash
git checkout -b nettle-probe-throwaway
BOOKFILE=$(ls book/src/chronicle/*.md | head -1)
printf '\nA probe sentence citing MAP-25 from a chronicle.\n' >> "$BOOKFILE"
git add "$BOOKFILE"
git commit -m "probe: a docs-only commit carrying a prose defect" 2>&1 | tail -16
echo "exit=$?"
```

Expected: the commit **FAILS**, the hook prints the `docs-tests` failure, and `git log -1` still shows the previous commit.

Branch table:

- commit refused, naming the failing test → **the fix works.**
- commit succeeded → the inversion is not reaching the failing test. Check the derived filter actually includes `the_book_carries_no_registry_ids_or_process_vocabulary`: `cargo nextest list -p hornvale --test suite -E '<filter>' | grep book_carries`.
- commit refused for a *different* reason (one of the three unconditional guards above the fast path) → not a valid control. Re-probe with a file none of those guards touch.

Then reset:

```bash
git checkout -- . 2>/dev/null; git reset --hard HEAD
git checkout campaign/the-nettle
git branch -D nettle-probe-throwaway
```

- [ ] **Step 7: Confirm the clean case still passes and the three guards still run unconditionally**

The three guards above the fast path must remain unconditional — `.superpowers/` scratch is markdown and `golden-pins.sql` is SQL, so hanging the whole hook on "is Rust staged" is what disabled them once before.

```bash
grep -n "gate-commit\|exit 0" scripts/hooks/pre-commit | sed -n '1,40p'
shellcheck scripts/hooks/pre-commit
bash scripts/check-bash32.sh scripts/hooks/pre-commit
```

Expected: the `.superpowers/` guard, the conflict-marker guard and the stream-manifest guard all still sit **above** the fast path; shellcheck and the bash-3.2 check are clean.

- [ ] **Step 8: Commit**

```bash
make gate-commit
git add scripts/hooks/pre-commit Makefile
git commit -m "fix(hooks): docs-only commits run the prose-subject tests

The fast path skipped gate-commit when no Rust path was staged. The premise
was 'Rust paths staged -> Rust checks matter'; the counterexample is a Rust
test whose SUBJECT IS PROSE. docs_consistency scans book/src and docs/, is in
the sub-floor roster, and never ran on ~12 docs-only commits in one campaign
-- so a MAP-25 citation in a chronicle went to the merge queue to be found on
the canonical box after it had taken the shared serial claim.

Now inverted rather than deleted: a docs-only commit runs the prose-subject
tests, not gate-commit. fmt, clippy and the type audit genuinely cannot be
moved by a markdown edit, and that half of the old reasoning was right. The
three unconditional guards above the fast path are untouched.

MEASURED, and the registry row's figure was the wrong mode: ~2.4 s warm but
~234.6 s cold, because the wall is the BUILD, not the 2.1 s of execution. The
cold case lands at campaign close, when the tree is coldest. Paid
deliberately -- once per absorption cycle -- against a merge phase reddening
at ~1129-1595 s.

Shown to discriminate: the original defect reproduced as a docs-only commit
is now REFUSED, and succeeded before the change."
```

---

### Task 6: The registry sweep — five rows, one commit

Every idea-registry edit in this campaign lands here, in one task, so `docs_consistency` gates them once and row IDs cannot collide with each other.

`book/src/frontier/idea-registry.md` is guarded by roughly a dozen tests in `cli/tests/suite/docs_consistency.rs`: rows must have five columns, IDs must be unique and parseable, statuses must use the closed vocabulary, every row needs a pointer, `refuted` rows must cite what refuted them, and idea cells are length-budgeted. **Read the existing rows' formatting before writing new ones** — an escaped pipe is not a column separator, and there is a test for that.

**Files:**
- Modify: `book/src/frontier/idea-registry.md` (two rows edited, three added)

**Interfaces:**
- Consumes: the outcomes of Tasks 1, 4 and 5 — do not run this task until those are committed, because two of the rows describe what they did.
- Produces: nothing.

- [ ] **Step 1: Read the format and the constraints**

```bash
grep -n "REGISTRY_STATUSES" -A12 cli/tests/suite/docs_consistency.rs
grep -n "registry_idea_cells_are_within_budget" -A12 cli/tests/suite/docs_consistency.rs
grep -n "PROC-autopilot-names-the-superseded-ledger-path\|PROC-docs-only-commits-skip-a-gate-that-checks-docs" book/src/frontier/idea-registry.md
```

Note the admissible statuses, the idea-cell length budget, and the exact five-column shape of a neighbouring `PROC-` row.

- [ ] **Step 2: Refute `PROC-autopilot-names-the-superseded-ledger-path`**

Its claim was false when written — the skill was corrected at `3b9a026aa` on 2026-08-30, before The Reservoir merged. `refuted` is the status for a claim a measurement overturned (distinct from `rejected`, a choice not taken), and `every_refuted_row_cites_its_evidence` requires the citation.

Change its **status** cell to `refuted (The Nettle)` and rewrite its **idea** cell to lead with the correction:

```
**REFUTED — already fixed before this row was written.** `campaign-autopilot`
was corrected at `3b9a026aa` (2026-08-30, The Cartulary): SKILL.md line 190
names `docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md`, and a grep across
`.claude/skills/` and the vendored superpowers tree finds no reference to
`.superpowers/sdd/decision-ledger.md` in any loaded skill. The Reservoir's own
worktree carried the corrected text. The LIVE defect was three lines up —
followups routed to `.superpowers/sdd/followups.md`, flat and shared and
promoted-at-close — carried now by
`PROC-autopilot-routed-followups-to-shared-scratch`
```

Keep the **Where** cell's existing pointers and append the fixing commit.

- [ ] **Step 3: Add `PROC-autopilot-routed-followups-to-shared-scratch`, shipped**

A new five-column row:

```
| PROC-autopilot-routed-followups-to-shared-scratch | **`campaign-autopilot`'s Capture discipline sent actionable followups to `.superpowers/sdd/followups.md`** — flat and *shared* (decision 0493's exact shape: every campaign's copy at one path, so two editing it merge to one side silently) and *promoted at close*, the practice that failed five recorded times before The Cartulary replaced it for rulings. The mechanism behind The Reservoir's ledger #20, where four deferred minors lived only in git-ignored scratch. **Shipped (The Nettle):** followups go to the committed per-campaign ledger as they occur, plus a table naming why the two open files differ — which nothing made visible at the moment of writing | shipped | high (measured, one instance) | [The Nettle spec §3](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-09-02-the-nettle-design.md); `.claude/skills/campaign-autopilot/SKILL.md` |
```

- [ ] **Step 4: Update `PROC-docs-only-commits-skip-a-gate-that-checks-docs` to shipped, with the real figures**

Its `~1 s` is the test-execution time, not the wall — correct it rather than leaving a figure that would mislead the next reader the way this campaign's own inherited figures did. Set status to `shipped` and rewrite the mitigation clause to:

```
**Shipped (The Nettle):** the fast path is inverted, not deleted — a docs-only
commit runs the prose-subject tests instead of nothing, and still skips fmt /
clippy / type-audit, which a markdown edit genuinely cannot move. **The `~1 s`
in this row's first draft was the wrong mode**: measured 2026-09-02, ~2.4 s
with the suite binary warm and **~234.6 s cold**, because the wall is the
build, not the 2.1 s of execution — and the cold case is campaign close, when
the tree is coldest
```

- [ ] **Step 5: Add the corrected item-5 row, `TOOL-derived-terrain-bytes-are-pinned-only-through-projections`**

This replaces The Reservoir's parked finding P2, whose premise was false. **State the corrected premise first**, because the wrong version is what the next reader would otherwise inherit:

```
| TOOL-derived-terrain-bytes-are-pinned-only-through-projections | **Correcting a parked finding that overstated its gap.** The Reservoir's P2 said "nothing pins the derived artifacts' bytes"; that is false — 10 regenerated, drift-checked seed-42 PNGs in `book/src/gallery/` render from worlds built through `GeneratedTerrain`/`GeneratedClimate`, and the census pins ~203 further metrics. (Three of the 13 tracked PNGs are not evidence: `star-chart` is astronomy, and `first-light` and `lithology` have no writer in `regenerate-artifacts.sh`.) The REAL residue is narrow: those PNGs pin *projections* — elevation through a renderer to 8-bit colour — so a change below the colour quantum, or in a field no map renders, slips. Deferred deliberately (The Nettle, G3 option d): deriving `Serialize` argues against the code's own design (`GeneratedClimate`'s doc says "Recomputed on demand, never serialized", and it holds an `Fbm` and two derive-once indices — caches, not state) and is save-format-adjacent, so it needs its own decision. If the residue is closed, a digest of the field values is the cheap option — full precision, no `Serialize`, no multi-MB artifact | raw | med (residue is narrow) | [The Nettle spec §5](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-09-02-the-nettle-design.md); `domains/terrain/src/provider.rs`; `domains/climate/src/provider.rs` |
```

- [ ] **Step 6: Add the decay-asymmetry row, `PROC-a-parked-finding-carries-no-use-by-date`**

The campaign's cross-cutting finding, captured rather than built (spec §7):

```
| PROC-a-parked-finding-carries-no-use-by-date | **A parked finding's content decays while its stated confidence does not, and nothing distinguishes the two kinds at filing time.** An *observation* asserts a property of a file's bytes — re-derivable by one command, stale only if someone edits that file. A *judgment* asserts a property of the repo's whole state ("nothing does X") and decays **silently** the moment any campaign closes the gap, with no edit to the file it named. The Nettle re-derived five findings handed over by The Reservoir: the one observation was durable, and **both judgments were already false** — one fixed three days earlier by an unrelated campaign, one false when written. Both still carried `high` confidence. Measured: **0 of 900 `raw` registry rows carry the SHA they were observed at**, and none carries the command that would re-decide it. Candidate remedy, deliberately not built here: a parked finding declares its kind, the SHA it was observed at, and its one re-deciding command | raw | high (measured, 2 of 5 instances) | [The Nettle spec §0](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-09-02-the-nettle-design.md); [The Nettle ledger #2](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/ledgers/2026-09-02-the-nettle.md) |
```

- [ ] **Step 7: Run the registry's guards**

```bash
cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)' 2>&1 | tail -8
```

Expected: all pass.

Branch table:

- all pass → proceed.
- `registry_rows_have_five_columns` fails → a literal `|` inside a cell is being read as a separator. Escape it, and check `an_escaped_pipe_is_not_a_column_separator` for the accepted form.
- `registry_idea_cells_are_within_budget` fails → an idea cell is over the length budget. Tighten the prose; do not raise the budget.
- `every_refuted_row_cites_its_evidence` fails → the refuted row's status cell needs the campaign name in the documented form.
- `no_new_numbered_registry_ids` fails → a new row used a numbered ID (`TOOL-25`); these must be slug IDs, which is what the three new rows above already use.

- [ ] **Step 8: Commit**

```bash
cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)' 2>&1 | tail -4
git add book/src/frontier/idea-registry.md
git commit -m "docs(registry): five rows for The Nettle, two of them corrections

Refutes PROC-autopilot-names-the-superseded-ledger-path: its claim was false
when written -- the skill was corrected at 3b9a026aa, three days before the
brief that reported it. Leaving a raw/high row asserting a fixed defect is
itself the decay mechanism this campaign is about.

Corrects PROC-docs-only-commits-skip-a-gate-that-checks-docs to shipped, and
replaces its '~1 s' with the measured pair: ~2.4 s warm, ~234.6 s cold. The
original was the test-execution time; the wall is the build.

Replaces The Reservoir's parked finding P2, whose premise was false: 10
drift-checked seed-42 PNGs already pin the derived terrain, so the residue is
'pinned only through projections', not 'not pinned'. Deferred per G3 option d.

Adds PROC-a-parked-finding-carries-no-use-by-date, the campaign's
cross-cutting finding: 0 of 900 raw rows carry the SHA they were observed at,
and the two findings that went stale were both the state-of-the-repo kind."
```

---

## Definition of Done

Beyond the six tasks, `closing-a-campaign` governs the close. This campaign additionally owes:

- [ ] **A chronicle entry** in `book/src/chronicle/`, written at the book's altitude — technical, comprehensible without the code.
- [ ] **A freshness sweep** of chapters this work touches. No Confidence Gradient bet moves here, so no re-scoring is expected; confirm rather than assume.
- [ ] **A retrospective** in `docs/retrospectives/the-nettle.md`. Its headline is the campaign's own finding turned on itself: **two of five inherited premises were stale, both of them the state-of-the-repo kind, and the spec's own G2 self-review then caught three more unverified numbers in the spec — one of which would have overstated the evidence in the single section where Nathan was being asked to decide.**
- [ ] **Every deferred minor in the ledger given a named outcome**, per `closing-a-campaign` step 2B — fixed in a later task, accepted as-is, or carried as a registry row.
- [ ] **The ledger's post-G3 entries** presented as the G6 digest.

## Notes for the executor

- **`make sluice-stage BRANCH=campaign/the-nettle REF=<full-sha>` at the plan-stage boundary.** A SHA, never a branch name. Push first.
- **Never `--no-verify`.** If fmt or clippy fails, that is the finding.
- **Write rulings, deferred minors and followups to `docs/superpowers/ledgers/2026-09-02-the-nettle.md` as they occur** — not to `progress.md`, which is exactly what Task 4 exists to fix. Practising the defect this campaign is repairing would be a poor look and a real loss.
- **Task 1 changes a hook that gates your own shell.** After committing it, the guard behaves differently for the rest of the campaign — that is intended, and Steps 5–7 are what establish it is correct.
- **If a task's Step 1 premise check fails, STOP and report.** Four of this campaign's five items arrived with a premise that did not survive contact; assume yours may be the fifth.

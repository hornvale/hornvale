#!/usr/bin/env bash
# scripts/hv-guard-bash.sh — the PreToolUse(Bash) guard.
#
# WHY THIS EXISTS, and why it is a program rather than another paragraph.
#
# Every rule this file enforces already existed in prose — in CLAUDE.md, in the
# dispatch preamble, in plan Global Constraints — and each one was violated
# anyway. The one rule in this repo with a perfect record is
# `require_canonical_census_host`, which lives INSIDE regenerate-artifacts.sh and
# refuses in a second rather than asking nicely. The difference is not emphasis;
# it is that a rule in an executable is a law and a rule in a document is advice.
#
# The precipitating waste: an agent ran `cargo test --workspace` TWICE in one
# pipeline to extract two different greps from identical output. Cargo caches the
# compilation but re-executes every test, and this workspace's suite time is
# dominated by test runtime, so that paid for the whole suite twice.
#
# DESIGN CONSTRAINTS, both load-bearing:
#
#   1. FAIL OPEN. This runs before every single Bash call. A guard that breaks
#      the session is worse than no guard, so any internal error allows the
#      command. `set -e` is deliberately NOT used, and every failure path prints
#      `{}` and exits 0. This is the same fail-safe-to-permissive posture
#      `Terrain::is_built` takes toward missing world data.
#   2. ALLOW NARROW RUNS. `cargo test -p hornvale-vessel --lib lattice::` is
#      cheap, correct, and what the plans tell implementers to do. A guard that
#      blocks legitimate iteration gets HV_TEST_OK=1 exported into a shell
#      profile and dies. Only WHOLE-WORKSPACE runs are refused.
#
# The interlock is a string in the command, not an inherited environment
# variable: this hook sees the command text, never a nested process's env. That
# is also why `make gate` needs no special case — it does not contain
# `cargo test`, so it is never matched.
#
# Self-test: `bash scripts/hv-guard-bash.sh --self-test`. Every pattern has both
# a case that must be denied and a case that must be allowed, because a guard
# that has only ever been seen to pass is not known to discriminate — the
# practice five implementers used on this campaign's own checks.

set -uo pipefail

# ---------------------------------------------------------------- helpers

# Allow the command: an empty object is "no opinion".
allow() {
    printf '{}\n'
    exit 0
}

# Deny with a reason the agent will read. Built with jq so a reason containing
# quotes or newlines cannot produce malformed JSON (and if jq is missing we fail
# open rather than emitting garbage the harness might mis-parse).
deny() {
    command -v jq >/dev/null 2>&1 || allow
    jq -n --arg reason "$1" \
        '{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "deny", permissionDecisionReason: $reason}}' \
        2>/dev/null || allow
    exit 0
}

# ---------------------------------------------------------------- the rules
#
# Takes the command text, prints a refusal reason and returns 1 if it should be
# denied, or returns 0 for allowed. Factored out of stdin handling so the
# self-test drives exactly the code the hook drives.
verdict() {
    local cmd="$1"

    # Rule 0 — the explicit escape hatch, checked first so it beats everything.
    # A human (or the controller, with authorization) can always override by
    # saying so inline. An override that is hard to find gets replaced by
    # disabling the guard.
    if [[ "$cmd" == *"HV_TEST_OK=1"* ]]; then
        return 0
    fi

    # Rule 1 — the same expensive suite run twice in one command. This is the
    # observed waste. Counted before the whole-workspace rule so the message
    # names the actual mistake rather than the generic one.
    local runs
    runs=$(grep -o -E 'cargo (test|nextest run)' <<<"$cmd" 2>/dev/null | wc -l | tr -d ' ')
    if [[ "${runs:-0}" -ge 2 ]]; then
        # shellcheck disable=SC2016  # the $? is literal advice text
        printf '%b' 'Two cargo test runs in one command. Cargo caches compilation but RE-EXECUTES every test, and this workspace'"'"'s suite time is dominated by test runtime — so this pays for the suite twice to ask it two questions. Capture once, then grep the file:\n\n  cargo test ... > /tmp/hv.log 2>&1; echo "exit=$?"\n  grep -E "^test result" /tmp/hv.log\n  grep -E "FAILED|panicked" /tmp/hv.log\n\nOverride with HV_TEST_OK=1 if you really mean it.'
        return 1
    fi

    # Rule 2 — a whole-workspace test run. `make gate` is the wrapper: it adds
    # nextest's parallel binaries and the doctests, and it is what CI runs, so a
    # raw invocation is both slower and a weaker signal. A run scoped with -p is
    # left alone (see design constraint 2).
    if grep -q -E 'cargo (test|nextest run)' <<<"$cmd" 2>/dev/null; then
        if grep -q -E -- '--workspace|--all([^-]|$)' <<<"$cmd" 2>/dev/null ||
            ! grep -q -E -- '(-p|--package|--manifest-path) ' <<<"$cmd" 2>/dev/null; then
            # shellcheck disable=SC2016  # the $? and backticks are literal advice text
            printf '%b' 'Whole-workspace cargo test. Use the project'"'"'s own targets instead:\n\n  make gate        the commit gate (fmt + clippy + type-audit + nextest + doctests)\n  make quick       the cheap half (fmt + clippy + type-audit), no tests\n  make gate-full   the gate plus the heavy tier\n  make gate-fast   ITERATION ONLY, scoped to changed crates\n\n`make gate` runs nextest'"'"'s parallel binaries AND the doctests and is what CI runs, so a raw cargo test is slower and a weaker signal. To test ONE crate, scope it: cargo test -p hornvale-vessel --lib lattice::\n\nOverride with HV_TEST_OK=1.'
            return 1
        fi
    fi

    # Rule 3 — census regeneration. Owner-authorized only (decision 0063), and
    # serialized against other heavy runs at the write seam (decision 0081).
    # This one is not about waste; it writes committed goldens only the canonical
    # host may author.
    if [[ "$cmd" == *"HV_CENSUS=1"* || "$cmd" == *"HV_CENSUS=true"* ]]; then
        if [[ "$cmd" != *"HV_CENSUS_AUTHORIZED=1"* ]]; then
            printf '%b' 'HV_CENSUS=1 writes committed census goldens, which only the canonical host may author (decision 0063) and which need the owner'"'"'s explicit authorization. A subagent must never set it. Route an authorized refresh through scripts/census-run.sh, which serializes against any other heavy run on the box (decision 0081).\n\nIf the owner has authorized this run, say so: HV_CENSUS_AUTHORIZED=1 HV_CENSUS=1 ...'
            return 1
        fi
    fi

    # Rule 4 — the shared stash stack. The git stash stack is shared across every
    # worktree of this repo AND with other concurrent sessions, so a bare pop can
    # restore someone else's work. `push -u -m <tag>` then `apply <sha>` is the
    # only safe form.
    if grep -q -E '(^|[;&|]|&&)[[:space:]]*git[[:space:]]+stash([[:space:]]|$)' <<<"$cmd" 2>/dev/null; then
        if grep -q -E 'git[[:space:]]+stash[[:space:]]+(list|show|apply|drop|push)' <<<"$cmd" 2>/dev/null; then
            : # an explicit, safe subcommand
        else
            # shellcheck disable=SC2016  # the $? and backticks are literal advice text
            printf '%b' 'Bare `git stash` or `git stash pop`. This repo'"'"'s stash stack is SHARED across every worktree and with other concurrent sessions, so a bare pop can restore work that is not yours.\n\nPrefer a temporary WIP commit to set work aside. If you must stash:\n  git stash push -u -m "<unique-tag>"\n  git stash list --format="%H %gs"        # capture your SHA\n  git stash apply <sha>                   # apply, never pop\n\nOverride with HV_TEST_OK=1.'
            return 1
        fi
    fi

    # Rule 5 — hook bypass. CLAUDE.md forbids --no-verify outright.
    if [[ "$cmd" == *"--no-verify"* ]]; then
        printf '%b' -- '--no-verify bypasses the commit hooks, which CLAUDE.md forbids without exception. Fix what the hook is complaining about instead; if fmt or clippy is failing, that IS the finding.'
        return 1
    fi

    return 0
}

# ---------------------------------------------------------------- self-test

self_test() {
    local fails=0 n=0

    check() { # check <expect: deny|allow> <command>
        n=$((n + 1))
        local want="$1" cmd="$2" got
        if verdict "$cmd" >/dev/null; then got=allow; else got=deny; fi
        if [[ "$got" != "$want" ]]; then
            printf 'FAIL want=%s got=%s : %s\n' "$want" "$got" "$cmd"
            fails=$((fails + 1))
        fi
    }

    # Rule 1 — double run. Both directions.
    check deny 'cargo test --workspace | grep x; cargo test --workspace | grep y'
    check deny 'cargo test -p a --lib > /tmp/a; cargo test -p a --lib | tail -3'
    check allow 'cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -14'

    # Rule 2 — whole-workspace. Narrow runs must survive; this is the rule most
    # likely to be resented into uselessness if it over-reaches.
    check deny 'cargo test --workspace'
    check deny 'cargo test'
    check deny 'cd /repo && cargo test --workspace 2>&1 | tail -5'
    check deny 'cargo nextest run --workspace'
    check allow 'cargo test -p hornvale-vessel'
    check allow 'cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -12'
    check allow 'cargo run --manifest-path tools/type-audit/Cargo.toml -- check'
    check allow 'make gate'
    check allow 'make gate-full 2>&1 | tail -30'
    check allow 'cargo clippy --workspace --all-targets -- -D warnings'
    check allow 'cargo build --release'

    # Rule 0 — the escape hatch beats the rules above it.
    check allow 'HV_TEST_OK=1 cargo test --workspace'

    # Rule 3 — census.
    check deny 'HV_CENSUS=1 bash scripts/regenerate-artifacts.sh'
    check allow 'HV_CENSUS_AUTHORIZED=1 HV_CENSUS=1 bash scripts/census-run.sh'
    check allow 'bash scripts/regenerate-artifacts.sh'

    # Rule 4 — the shared stash stack.
    check deny 'git stash'
    check deny 'git stash pop'
    check deny 'git add -A && git stash'
    check allow 'git stash push -u -m "the-blocking-wip"'
    check allow 'git stash list --format="%H %gs"'
    check allow 'git stash apply 0123abc'

    # Rule 5 — bypass.
    check deny 'git commit --no-verify -m x'
    check allow 'git commit -m "ordinary"'

    # The reason TEXT must survive, not just the verdict. The first version of
    # this file emitted its advice in printf's FORMAT position, so `%H` and `%gs`
    # in the stash guidance were eaten as format specifiers and the self-test
    # passed anyway — the verdict was right and the message was destroyed. Same
    # class as a test helper that filters its own input.
    local msg
    msg="$(verdict 'git stash pop' || true)"
    if [[ "$msg" != *'%H %gs'* ]]; then
        printf 'FAIL the stash advice lost its literal %%H/%%gs\n'
        fails=$((fails + 1))
    fi
    n=$((n + 1))
    if [[ "$msg" == *'invalid format'* ]]; then
        printf 'FAIL printf mangled a reason string\n'
        fails=$((fails + 1))
    fi
    n=$((n + 1))

    # Things that must never be touched.
    check allow 'ls -la'
    check allow 'grep -rn cargo docs/'
    check allow ''

    if [[ "$fails" -eq 0 ]]; then
        printf 'hv-guard-bash: %d/%d cases pass\n' "$n" "$n"
        return 0
    fi
    printf 'hv-guard-bash: %d of %d cases FAILED\n' "$fails" "$n"
    return 1
}

# ---------------------------------------------------------------- entry

if [[ "${1:-}" == "--self-test" ]]; then
    self_test
    exit $?
fi

# Read the hook payload. Anything unexpected allows the command.
command -v jq >/dev/null 2>&1 || allow
payload="$(cat 2>/dev/null)" || allow
cmd="$(jq -r '.tool_input.command // empty' <<<"$payload" 2>/dev/null)" || allow
[[ -n "$cmd" ]] || allow

if reason="$(verdict "$cmd")"; then
    allow
else
    deny "$reason"
fi

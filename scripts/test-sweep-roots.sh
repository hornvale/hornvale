#!/usr/bin/env bash
# scripts/test-sweep-roots.sh — the reclamation pass's SCOPE.
#
# WHY THIS EXISTS. `make sweep` ran `cargo sweep -r .` for the life of decision
# 0848 and reached neither worktree pool, while printing a plausible total and
# exiting 0. Two causes, both invisible from the output: `-r` skips
# dot-directories without `--hidden` (so `.claude/worktrees/` was never
# visited), and the second pool lives outside the repo entirely
# (`~/.config/superpowers/worktrees/`, per the global CLAUDE.md).
#
# THE FAILURE MODE THIS FILE IS SHAPED AROUND is not "sweep errors" — it is
# "sweep succeeds over the wrong set". A test that only asserted `sweep-roots.sh
# prints some paths` would have passed against the broken command too, so case 5
# is the load-bearing one: it drives the REAL cargo-sweep both ways and requires
# the dot-pool worktree to be reachable ONLY through the roots. Without it the
# other four cases are a description of the fix, not evidence of it.
#
# NOTHING HERE TOUCHES THE REAL POOLS. Every case runs in a throwaway repo
# under `mktemp -d`, and case 5 is `--dry-run`.
set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
roots_sh="$repo_root/scripts/sweep-roots.sh"

pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

tmp="$(mktemp -d "${TMPDIR:-/tmp}/hv-sweep-roots.XXXXXX")"
trap 'rm -rf "$tmp"' EXIT
# Canonicalise. On macOS $TMPDIR is `/var/folders/...`, a symlink to
# `/private/var/folders/...`, and git reports the RESOLVED path — so an
# uncanonicalised fixture makes every path comparison here fail for a reason
# that has nothing to do with the code under test.
tmp="$(cd "$tmp" && pwd -P)"

# A repo with both pool shapes: one worktree inside a DOT-directory, one
# entirely outside the repo tree.
d="$tmp/w"; mkdir -p "$d"
git init --quiet "$d/repo"
git -C "$d/repo" config user.email t@e.st
git -C "$d/repo" config user.name Test
printf 'x\n' > "$d/repo/f"
git -C "$d/repo" add -A
git -C "$d/repo" commit --quiet -m init
git -C "$d/repo" branch -M main
mkdir -p "$d/repo/.claude/worktrees" "$d/outside"
git -C "$d/repo" worktree add --quiet "$d/repo/.claude/worktrees/hidden" -b hidden main
git -C "$d/repo" worktree add --quiet "$d/outside/far"                   -b far    main
git -C "$d/repo" worktree add --quiet "$d/outside/gone"                  -b gone   main

roots="$(cd "$d/repo" && bash "$roots_sh")"

# 1 — the dot-directory pool. The `-r` default that skipped it is the whole bug.
if grep -qx "$d/repo/.claude/worktrees/hidden" <<<"$roots"; then
    ok "dot-directory pool member is a root"
else
    bad "dot-directory pool member missing from roots"
fi

# 2 — the out-of-repo pool. No recursion rooted at the repo can reach this.
if grep -qx "$d/outside/far" <<<"$roots"; then
    ok "out-of-repo worktree is a root"
else
    bad "out-of-repo worktree missing from roots"
fi

# 3 — the main checkout itself must still be swept.
if grep -qx "$d/repo" <<<"$roots"; then
    ok "main checkout is a root"
else
    bad "main checkout missing from roots"
fi

# 4 — a worktree directory removed by hand. git still lists it; handing a
# vanished path to cargo-sweep aborts the pass, so it must be dropped silently.
#
# THE PRESENCE ASSERTION IS NOT DECORATION. Written without it, this case
# asserted the absence of a path that was never in the list to begin with, and
# passed against a helper that emitted nothing at all. Establish that the path
# IS a root first, so the absence afterwards means "dropped" rather than
# "never there".
if grep -qx "$d/outside/gone" <<<"$roots"; then
    ok "control: the doomed worktree is a root while it exists"
else
    bad "CONTROL INVALID: doomed worktree was never a root, so case 4 is vacuous"
fi
rm -rf "$d/outside/gone"
roots2="$(cd "$d/repo" && bash "$roots_sh")"; rc=$?
if [ "$rc" -eq 0 ] && ! grep -qx "$d/outside/gone" <<<"$roots2"; then
    ok "vanished worktree is skipped, pass still exits 0"
else
    bad "vanished worktree not skipped (rc=$rc)"
fi

# 5 — THE CONTROL. Drive the real cargo-sweep both ways over the same fixture
# and require the dot-pool target to be reachable ONLY via the roots. A
# fabricated crate is enough: cargo-sweep reports every Rust project folder it
# VISITS, so appearing in the output is exactly "was reached".
mk_crate() {
    mkdir -p "$1/src" "$1/target/debug"
    printf '[package]\nname = "p"\nversion = "0.0.0"\nedition = "2021"\n' > "$1/Cargo.toml"
    printf 'fn main(){}\n' > "$1/src/main.rs"
    printf 'x\n' > "$1/target/debug/artifact"
}
mk_crate "$d/repo"
mk_crate "$d/repo/.claude/worktrees/hidden"

# The tool probe is a function with a SEAM so the absent-tool path is itself
# testable (case 6). Without the seam the only way to exercise that path is to
# uninstall cargo-sweep, which nobody does, so the message a future reader
# actually meets would never have been read by anyone.
have_cargo_sweep() {
    [ -z "${HV_SWEEP_ROOTS_ASSUME_NO_TOOL:-}" ] || return 1
    command -v cargo-sweep >/dev/null 2>&1
}

if have_cargo_sweep; then
    hid="$d/repo/.claude/worktrees/hidden/target"
    before="$(cd "$d/repo" && cargo sweep --dry-run --time 3650 -r . 2>&1)"
    after="$(cd "$d/repo" && bash "$roots_sh" | tr '\n' '\0' \
             | xargs -0 cargo sweep --dry-run --time 3650 -r 2>&1)"
    if grep -qF "$hid" <<<"$before"; then
        bad "CONTROL INVALID: plain \`-r .\` already reaches the dot-pool target"
    else
        ok "control: plain \`-r .\` does NOT reach the dot-pool target"
    fi
    if grep -qF "$hid" <<<"$after"; then
        ok "roots-driven sweep DOES reach the dot-pool target"
    else
        bad "roots-driven sweep still misses the dot-pool target"
    fi
else
    # NAME THE HOST AND THE REMEDY. This is the message a fresh chamber host
    # meets, and on 2026-09-10 it was met by lefford mid-merge: without the
    # host and the install line it reads as a code regression in the candidate
    # rather than as a missing dev tool on the box. It stays a FAILURE and not
    # a skip -- a scope test that opts out of measuring scope is the bug it was
    # written against.
    printf '  FAIL: cargo-sweep is not installed on %s.\n' "$(hostname -s 2>/dev/null || echo 'this host')"
    printf '        A MISSING DEV TOOL ON THIS HOST -- not a code regression.\n'
    printf '        Case 5 is the only case that distinguishes this fix from the\n'
    printf '        bug it guards, and it cannot run without the real binary, so\n'
    printf '        this suite fails rather than reporting a green 5/5.\n'
    printf '        Install it (decision 0848; CLAUDE.md, the sweep block):\n'
    printf '            cargo install cargo-sweep   # or: brew install cargo-sweep\n'
    fail=$((fail+1))
fi

# 6 — THE ABSENT-TOOL PATH ITSELF. Re-runs this script with the seam set and
# requires it to fail, to name the host, and to print the install line. Skipped
# when the seam is already set, which is what stops the recursion.
if [ -z "${HV_SWEEP_ROOTS_ASSUME_NO_TOOL:-}" ]; then
    out="$(HV_SWEEP_ROOTS_ASSUME_NO_TOOL=1 bash "${BASH_SOURCE[0]}" 2>&1)" && rc=0 || rc=$?
    host="$(hostname -s 2>/dev/null || echo 'this host')"
    if [ "$rc" -ne 0 ]; then
        ok "absent tool fails the suite (rc=$rc), never a green skip"
    else
        bad "absent tool did not fail the suite"
    fi
    if grep -qF "$host" <<<"$out"; then
        ok "absent-tool message names the host"
    else
        bad "absent-tool message does not name the host"
    fi
    if grep -qF "install cargo-sweep" <<<"$out"; then
        ok "absent-tool message prints the install line"
    else
        bad "absent-tool message omits the install line"
    fi
fi

printf 'test-sweep-roots: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]

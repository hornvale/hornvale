#!/usr/bin/env bash
# scripts/test-pre-push.sh — property tests for scripts/hooks/pre-push.
#
# Added after the incident, 2026-08-16: a subagent force-pushed campaign WIP
# over origin/main while probing bash quote-splitting semantics. Its dispatch
# named that prohibition as the single most important constraint and it still
# happened, because "print git remote -v first" is prose, and prose is not a
# control (see scripts/hooks/pre-push's own header). This battery pins that
# the CONTROL — not the prose — actually refuses the shape of push that did
# the damage, and that the two deliberate escape hatches (a local remote, and
# HV_PUSH_OK=1) are real allowances rather than the hook silently failing open.
#
# Shaped after scripts/test-sluice.sh: each case is shown to fail when the
# property it pins is broken (see the mutation pass this file's own report
# references), not merely asserted once and trusted.
#
# Drives the hook exactly the way git does — pass $1 (remote name) and $2
# (remote url) as arguments, and feed "<local ref> <local sha1> <remote ref>
# <remote sha1>" lines on the hook's STDIN — rather than by pushing, so most
# cases need no network and no real remote at all. The exception is the "real
# push" section near the end, which uses a `file://` bare repo under
# `mktemp -d`: local remotes are exempt from the hook by design, so that
# section needs no HV_PUSH_OK and touches no network either.
#
# HERMETICITY (scripts/CLAUDE.md): git exports GIT_DIR and GIT_INDEX_FILE to
# hooks, and from a linked worktree they are absolute paths into the real
# repository — GIT_DIR outranks `git -C`, so a temp directory alone is not
# isolation. Every git call below runs under `env -u GIT_DIR -u GIT_INDEX_FILE`.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
hook="$repo_root/scripts/hooks/pre-push"
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
g()   { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }

zero='0000000000000000000000000000000000000000'
nonlocal_url='https://example.invalid/hornvale.git'

# run_hook <remote-name> <remote-url> <local-ref> <local-sha> <remote-ref> <remote-sha>
# Sets run_hook_status and run_hook_stderr as a side effect, mirroring the
# arguments and stdin shape git itself gives a pre-push hook.
run_hook_status=0
run_hook_stderr=''
run_hook() {
    remote_name="$1"; remote_url="$2"
    local_ref="$3"; local_sha="$4"; remote_ref="$5"; remote_sha="$6"
    set +e
    run_hook_stderr="$(printf '%s %s %s %s\n' "$local_ref" "$local_sha" "$remote_ref" "$remote_sha" \
        | env -u GIT_DIR -u GIT_INDEX_FILE bash "$hook" "$remote_name" "$remote_url" 2>&1)"
    run_hook_status=$?
    set -e
}

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# A scratch repo with linear history (R -> A) plus a sibling commit B off the
# same root, so we control both a fast-forward pair (R, A) and a
# force-shaped, non-ancestor pair (A, B) exactly.
scratch="$tmp/repo"; mkdir -p "$scratch"
g init -q -b main "$scratch"
g -C "$scratch" config user.email t@t
g -C "$scratch" config user.name t
printf 'r\n' > "$scratch/f.txt"; g -C "$scratch" add f.txt; g -C "$scratch" commit -qm root
R="$(g -C "$scratch" rev-parse HEAD)"
printf 'a\n' >> "$scratch/f.txt"; g -C "$scratch" commit -qam a
A="$(g -C "$scratch" rev-parse HEAD)"
g -C "$scratch" checkout -q -b side "$R"
printf 'b\n' >> "$scratch/f.txt"; g -C "$scratch" commit -qam b
B="$(g -C "$scratch" rev-parse HEAD)"
g -C "$scratch" checkout -q main

cd "$scratch"

echo "== pre-push: the destructive class is refused on a non-local remote"

run_hook origin "$nonlocal_url" refs/heads/main "$B" refs/heads/main "$A"
if [ "$run_hook_status" -ne 0 ]; then
    ok "a force push (B is not a descendant of A) to a non-local remote is refused"
else
    bad "a force push to a non-local remote was allowed"
fi

run_hook origin "$nonlocal_url" refs/heads/main "$zero" refs/heads/main "$A"
if [ "$run_hook_status" -ne 0 ]; then
    ok "a delete (local sha all zeros) to a non-local remote is refused"
else
    bad "a delete to a non-local remote was allowed"
fi

echo "== pre-push: HV_PUSH_OK=1 is the deliberate, documented override"

export HV_PUSH_OK=1
run_hook origin "$nonlocal_url" refs/heads/main "$B" refs/heads/main "$A"
unset HV_PUSH_OK
if [ "$run_hook_status" -eq 0 ]; then
    ok "the same force push is allowed when HV_PUSH_OK=1 is set"
else
    bad "HV_PUSH_OK=1 did not override the refusal: $run_hook_stderr"
fi

echo "== pre-push: a file:// remote is exempt (tests must stay frictionless)"

run_hook origin "file:///nonexistent/scratch-remote.git" refs/heads/main "$B" refs/heads/main "$A"
if [ "$run_hook_status" -eq 0 ]; then
    ok "a force push to a file:// remote is allowed without HV_PUSH_OK"
else
    bad "a file:// remote's force push was refused: $run_hook_stderr"
fi

echo "== pre-push: an ordinary fast-forward is allowed"

run_hook origin "$nonlocal_url" refs/heads/main "$A" refs/heads/main "$R"
if [ "$run_hook_status" -eq 0 ]; then
    ok "a fast-forward (R is an ancestor of A) to a non-local remote is allowed"
else
    bad "a fast-forward to a non-local remote was refused: $run_hook_stderr"
fi

echo "== pre-push: a brand-new branch (remote sha all zeros) is allowed"
# The case most likely to be misclassified as a force: nothing on the remote
# yet, so there is nothing for the local sha to fail to descend from.

run_hook origin "$nonlocal_url" refs/heads/new-branch "$A" refs/heads/new-branch "$zero"
if [ "$run_hook_status" -eq 0 ]; then
    ok "a brand-new branch (remote sha all zeros) is allowed without HV_PUSH_OK"
else
    bad "a brand-new branch was refused: $run_hook_stderr"
fi

echo "== pre-push: unverifiable ancestry (remote sha not present locally) fails closed"
# A shallow clone, or a remote sha this side has never fetched, cannot be
# ancestry-checked at all. The brief calls failing closed the safer default
# here; this pins that choice rather than letting it fall out of an
# unhandled 'git merge-base' error.

made_up_remote_sha='deadbeefdeadbeefdeadbeefdeadbeefdeadbeef'
run_hook origin "$nonlocal_url" refs/heads/main "$A" refs/heads/main "$made_up_remote_sha"
if [ "$run_hook_status" -ne 0 ]; then
    ok "an unverifiable ancestor (object absent locally) fails closed, refused"
else
    bad "an unverifiable ancestor was wrongly allowed — this fails OPEN, not closed"
fi

echo "== pre-push: THE EXACT INCIDENT — 21847b08 -> fb71dd2e on refs/heads/main"
# That case is the reason this file exists. Both objects are real commits in
# this repository (confirmed by the controller before dispatch), so the
# ancestry check below is exercised for real, not simulated.

cd "$repo_root"
OLD_INCIDENT="21847b086b1caf889ddee23dbee7888dd045406b"
NEW_INCIDENT="fb71dd2e1526efc7a7460c26a71bceaf8eccb9be"
if g cat-file -e "${OLD_INCIDENT}^{commit}" 2>/dev/null && g cat-file -e "${NEW_INCIDENT}^{commit}" 2>/dev/null; then
    ok "test setup: both incident commits are present locally"
else
    bad "test setup: an incident commit is missing locally — cannot exercise the real case"
fi
if g merge-base --is-ancestor "$OLD_INCIDENT" "$NEW_INCIDENT"; then
    bad "test setup: the incident's old sha IS an ancestor of the new one — no longer reproduces the non-fast-forward shape"
else
    ok "test setup: the incident's old sha is confirmed NOT an ancestor of the new one"
fi
run_hook origin "$nonlocal_url" refs/heads/main "$NEW_INCIDENT" refs/heads/main "$OLD_INCIDENT"
if [ "$run_hook_status" -ne 0 ]; then
    ok "THE INCIDENT ITSELF (21847b08 -> fb71dd2e on refs/heads/main) is refused"
else
    bad "THE INCIDENT ITSELF WAS ALLOWED — this is the exact case the hook exists to catch"
fi
cd "$scratch"

echo "== pre-push: a real push to a scratch bare repo, no network, local remote only"
# scripts/hooks/pre-push must never be exercised against a real network
# remote in this test — see this file's own header and the task dispatch's
# incident warning. A file:// bare repo under mktemp -d is the sanctioned
# stand-in.

bare="$tmp/bare.git"
g init -q --bare "$bare"
g -C "$scratch" remote add origin "file://$bare"
g -C "$scratch" config core.hooksPath "$repo_root/scripts/hooks"

if g -C "$scratch" push -q origin main:main 2>"$tmp/push1.err"; then
    ok "a real push to a file:// bare repo succeeds under the real hook"
else
    bad "a real push to a file:// bare repo was refused: $(cat "$tmp/push1.err")"
fi

if g -C "$scratch" push -qf origin side:main 2>"$tmp/push2.err"; then
    ok "a real FORCE push to a file:// bare repo succeeds under the real hook (local remote is exempt)"
else
    bad "a real force push to a file:// bare repo was refused: $(cat "$tmp/push2.err")"
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]

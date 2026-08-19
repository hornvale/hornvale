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
# HV_CENSUS_LOCK_HELD IS SCRUBBED, AND THIS IS LOAD-BEARING, NOT TIDINESS.
# The hook treats a LIVE value in that variable as proof that we ARE the claim
# holder (reason 1) and allows the push outright. This suite builds its own
# claim state in $tmp and asserts on refusals, so an INHERITED live value turns
# every one of those refusals into an allow — silently, and in the direction
# that reports a guard as working when it is not being exercised at all.
#
# It was inherited in exactly one place, and that place is the one that
# matters: `scripts/sluice-run.sh:477` does `export HV_CENSUS_LOCK_HELD=$$`,
# so every phase the chamber runs sees the chamber's own live pid. Measured
# 2026-08-19 by A/B against unmodified main: 0 failures in a bare shell, and
# the same 3 failures under `HV_CENSUS_LOCK_HELD=<live pid>` — a defect that
# predates this line and stayed invisible only because NOTHING ran this file
# until it joined the `outboard` set.
#
# The cases that genuinely exercise reason 1 set the variable themselves, on
# the call, where it is visible in the test rather than ambient.
run_hook() {
    remote_name="$1"; remote_url="$2"
    local_ref="$3"; local_sha="$4"; remote_ref="$5"; remote_sha="$6"
    set +e
    # Scrub the ambient value, then re-add it ONLY from $hook_lock_held — an
    # explicit opt-in a reader can see at the call site, rather than whatever
    # the surrounding process happened to export.
    if [ -n "${hook_lock_held:-}" ]; then
        run_hook_stderr="$(printf '%s %s %s %s\n' "$local_ref" "$local_sha" "$remote_ref" "$remote_sha" \
            | env -u GIT_DIR -u GIT_INDEX_FILE "HV_CENSUS_LOCK_HELD=$hook_lock_held" \
              bash "$hook" "$remote_name" "$remote_url" 2>&1)"
    else
        run_hook_stderr="$(printf '%s %s %s %s\n' "$local_ref" "$local_sha" "$remote_ref" "$remote_sha" \
            | env -u GIT_DIR -u GIT_INDEX_FILE -u HV_CENSUS_LOCK_HELD \
              bash "$hook" "$remote_name" "$remote_url" 2>&1)"
    fi
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
# ON A BRANCH, not main. This case pins the DESTRUCTIVE-class gate: an
# additive push must not be swept up with deletes and force-pushes. It used
# refs/heads/main until 2026-08-19, which was incidental — main is now gated
# for an entirely different reason (decision 0139, the chamber cases below),
# and leaving the ref as main would have made this assertion fail for a cause
# it was never about, reading as a regression in the force-push guard.
run_hook origin "$nonlocal_url" refs/heads/feat/ff "$A" refs/heads/feat/ff "$R"
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

# NO SUMMARY HERE. It used to sit at this line, with 79 lines of chamber
# tests after it — so their pass/fail counts were never printed and never
# gated the exit status. Measured by injecting one `bad` into the branch-push
# case: it printed "FAIL:" and the script still exited 0, under a summary
# reading "12 passed, 0 failed". The summary now lives at the end of the file,
# where it covers every assertion in it.

echo "== chamber: a push to main while the box is CLAIMED by someone else is refused"
# The failure this closes, measured 2026-08-18: a one-line prose commit pushed
# directly while feat/decision-blocks was mid-merge. That candidate had all six
# phases GREEN, main moved underneath it, and its own push was then refused as
# a non-fast-forward. An hour of the one serial box, discarded.
claim="$tmp/hv-census.claim"
sleep 600 & foreign_pid=$!          # a live process that is NOT our ancestor
printf 'pid=%s\nhost=t\nuser=t\nstarted=t\ngoldens=t\nlabel=sluice-merge:campaign/x\nref=deadbeef\ncmdline=t\n' \
    "$foreign_pid" > "$claim"
HV_CENSUS_CLAIM_PATH="$claim" run_hook origin "$nonlocal_url" refs/heads/main "$A" refs/heads/main "$R"
if [ "$run_hook_status" -ne 0 ]; then
    ok "a push to main is refused while a foreign live process holds the claim"
else
    bad "the push was ALLOWED while the chamber held the box — the run it would kill is unprotected"
fi
case "$run_hook_stderr" in
    *"THE CHAMBER IS HOLDING THE BOX"*) ok "the refusal says why, and names the running job" ;;
    *) bad "the refusal does not explain the chamber hold: $run_hook_stderr" ;;
esac
case "$run_hook_stderr" in
    *HV_PUSH_OK*) ok "the refusal names the hotfix escape, so it is satisfiable" ;;
    *) bad "the refusal offers no escape — a guard nobody can satisfy is worked around" ;;
esac

echo "== chamber: a BRANCH push is never gated by the claim"
# Candidates reach the queue by pushing a branch. Gating that would make the
# guard prevent the very thing it is protecting.
HV_CENSUS_CLAIM_PATH="$claim" run_hook origin "$nonlocal_url" refs/heads/feat/x "$A" refs/heads/feat/x "$R"
if [ "$run_hook_status" -eq 0 ]; then
    ok "a branch push is allowed while the box is claimed"
else
    bad "a branch push was refused — candidates could not reach the queue at all"
fi

echo "== chamber: THE CHAMBER'S OWN PUSH IS NOT BLOCKED (the wedge case)"
# sluice-run.sh pushes to main WHILE holding the claim. A naive is-it-claimed
# test refuses the queue itself and wedges every merge — strictly worse than
# the problem being solved. Reason 1: the holder exports HV_CENSUS_LOCK_HELD
# and children inherit it.
HV_CENSUS_CLAIM_PATH="$claim" hook_lock_held="$foreign_pid" \
    run_hook origin "$nonlocal_url" refs/heads/main "$A" refs/heads/main "$R"
if [ "$run_hook_status" -eq 0 ]; then
    ok "the holder's own push to main is allowed via HV_CENSUS_LOCK_HELD"
else
    bad "the CHAMBER'S OWN push was refused — this would wedge every merge: $run_hook_stderr"
fi

echo "== chamber: HERMETICITY — an AMBIENT HV_CENSUS_LOCK_HELD must not decide the outcome"
# THE DEFECT THIS PINS SHIPPED IN main AND HID FOR AS LONG AS THE FILE EXISTED.
# The hook reads a live HV_CENSUS_LOCK_HELD as proof that we are the claim
# holder and allows the push. This suite inherited that variable, so any
# environment exporting a live one turned every refusal assertion into an
# allow — reporting the guard as working while not exercising it at all.
#
# The environment that exports one is the chamber (`sluice-run.sh:477`,
# `export HV_CENSUS_LOCK_HELD=$$`), which is where this file now runs. It was
# invisible until then because NOTHING ran this file. A/B against unmodified
# main, 2026-08-19: 0 failures bare, the same 3 failures with a live value.
sleep 600 & ambient_pid=$!
printf 'pid=%s\nhost=t\nuser=t\nstarted=t\ngoldens=t\nlabel=sluice-merge:campaign/amb\nref=deadbeef\ncmdline=t\n' \
    "$ambient_pid" > "$claim"
export HV_CENSUS_LOCK_HELD="$ambient_pid"
HV_CENSUS_CLAIM_PATH="$claim" run_hook origin "$nonlocal_url" refs/heads/main "$A" refs/heads/main "$R"
unset HV_CENSUS_LOCK_HELD
kill "$ambient_pid" 2>/dev/null || true; wait "$ambient_pid" 2>/dev/null || true
if [ "$run_hook_status" -ne 0 ]; then
    ok "an ambient live HV_CENSUS_LOCK_HELD does not leak into the hook — the refusal still fires"
else
    bad "the ambient value leaked: this suite reports PASS on assertions it is not exercising, and does so inside the chamber"
fi

echo "== chamber: REASON 2 — the ancestry walk allows the chamber with NO env var"
# THE BACKSTOP HAD NO POSITIVE TEST. Reason 1 (HV_CENSUS_LOCK_HELD, inherited)
# is asserted above; reason 2 (the claim pid is a genuine ANCESTOR, walked via
# /proc) was only ever exercised NEGATIVELY, by the mutation that removes
# reason 1 and watches a FOREIGN pid get refused. That says nothing about
# whether the walk allows a real descendant — and reason 2 is the whole point
# of the pair: it catches a chamber descendant whose environment was scrubbed,
# which is exactly what `env -u` and `sh -c` do all over this repo.
#
# Built by having a parent shell write ITS OWN pid into the claim and then run
# the hook as a child, with HV_CENSUS_LOCK_HELD deliberately unset.
if [ -r /proc/$$/status ]; then
    anc_claim="$tmp/hv-ancestor.claim"
    anc_out="$(bash -c '
        printf "pid=%s\nhost=t\nuser=t\nstarted=t\ngoldens=t\nlabel=sluice-merge:campaign/z\nref=deadbeef\ncmdline=t\n" "$$" > "$1"
        printf "refs/heads/main %s refs/heads/main %s\n" "$2" "$3" \
            | HV_CENSUS_CLAIM_PATH="$1" env -u HV_CENSUS_LOCK_HELD -u GIT_DIR -u GIT_INDEX_FILE \
              bash "$4" origin "$5" 2>&1
        echo "rc=$?"
    ' _ "$anc_claim" "$A" "$R" "$hook" "$nonlocal_url")"
    case "$anc_out" in
        *"rc=0"*) ok "a descendant of the claim holder is allowed by the ancestry walk alone" ;;
        *) bad "the ancestry backstop REFUSED a real descendant — if HV_CENSUS_LOCK_HELD is ever scrubbed, every merge wedges: $anc_out" ;;
    esac
else
    echo "  SKIP: no /proc on this host — the ancestry walk cannot be exercised"
fi

echo "== chamber: a DEAD claim is NOT a claim — and a push with no live claim is refused"
# THIS EXPECTATION INVERTED (decision 0139, 2026-08-19). It used to assert
# that a stale claim "does not block anything", which was right about staleness
# and wrong about the push: a dead pid means no chamber is running, and a push
# to main that is not the chamber's is exactly what 0139 forbids. Staleness
# still must not WEDGE anything — that is what HV_PUSH_OK covers, asserted
# below — but it can no longer excuse an out-of-band landing either.
kill "$foreign_pid" 2>/dev/null || true; wait "$foreign_pid" 2>/dev/null || true
HV_CENSUS_CLAIM_PATH="$claim" run_hook origin "$nonlocal_url" refs/heads/main "$A" refs/heads/main "$R"
if [ "$run_hook_status" -ne 0 ]; then
    ok "a stale claim does not count as the chamber — the push is refused"
else
    bad "a push to main with NO live claim was ALLOWED — this is the ca6f34310 hole, still open"
fi

echo "== chamber: MUTATION — without the ancestry/inheritance escape the chamber blocks itself"
mut="$tmp/pre-push-mutant"
# shellcheck disable=SC2016  # matching the hook's SOURCE TEXT, not evaluating it
python3 "$repo_root/scripts/mutate.py" --to "$mut" "$hook" \
    'if [ -n "$mine" ] && kill -0 "$mine" 2>/dev/null; then inside=1; fi' \
    'inside=0' >/dev/null
sleep 600 & foreign2=$!
printf 'pid=%s\nhost=t\nuser=t\nstarted=t\ngoldens=t\nlabel=t\nref=t\ncmdline=t\n' "$foreign2" > "$claim"
set +e
mut_out="$(printf 'refs/heads/main %s refs/heads/main %s\n' "$A" "$R" \
    | HV_CENSUS_CLAIM_PATH="$claim" HV_CENSUS_LOCK_HELD="$foreign2" \
      env -u GIT_DIR -u GIT_INDEX_FILE bash "$mut" origin "$nonlocal_url" 2>&1)"
mut_rc=$?
set -e
kill "$foreign2" 2>/dev/null || true; wait "$foreign2" 2>/dev/null || true
if [ "$mut_rc" -ne 0 ]; then
    ok "MUTATION CONFIRMED: without the escape the holder's own push is refused (the real hook allows it)"
else
    bad "the mutant also allowed the push — the wedge-case test above is not pinning the escape: $mut_out"
fi


echo "== chamber: NO CLAIM AT ALL — the out-of-band hole (decision 0139)"
# ca6f34310 ("book: close The Adit") landed on origin/main with no queue row,
# on an idle box. The old guard gated only "someone ELSE holds the claim", so
# an idle box was a free pass. 0139 calls an out-of-band landing a DETECTED
# fault; nothing detected it.
rm -f "$claim"
HV_CENSUS_CLAIM_PATH="$claim" run_hook origin "$nonlocal_url" refs/heads/main "$A" refs/heads/main "$R"
if [ "$run_hook_status" -ne 0 ]; then
    ok "a push to main with no claim file is refused"
else
    bad "a push to main on an idle box was allowed — main can still move outside the queue"
fi
case "$run_hook_stderr" in
    *"NOT THE MERGE QUEUE"*) ok "the refusal says which rule it enforces" ;;
    *) bad "the refusal does not name the rule: $run_hook_stderr" ;;
esac
case "$run_hook_stderr" in
    *0139*) ok "the refusal cites the decision, so it is checkable rather than merely asserted" ;;
    *) bad "the refusal cites no decision" ;;
esac
case "$run_hook_stderr" in
    *HV_PUSH_OK*) ok "the refusal names the hotfix escape, so it is satisfiable" ;;
    *) bad "the refusal offers no escape — a guard nobody can satisfy is worked around" ;;
esac

echo "== chamber: a BRANCH push with no claim is still allowed"
# The rule is about main. If this ever reddens, candidates cannot reach the
# queue at all and the guard has eaten the workflow it protects.
HV_CENSUS_CLAIM_PATH="$claim" run_hook origin "$nonlocal_url" refs/heads/feat/y "$A" refs/heads/feat/y "$R"
if [ "$run_hook_status" -eq 0 ]; then
    ok "a branch push on an idle box is allowed"
else
    bad "a branch push was refused with no claim held — the queue is unreachable: $run_hook_stderr"
fi

echo "== chamber: HV_PUSH_OK=1 still escapes the no-claim refusal (the hotfix route)"
HV_PUSH_OK=1 HV_CENSUS_CLAIM_PATH="$claim" run_hook origin "$nonlocal_url" refs/heads/main "$A" refs/heads/main "$R"
if [ "$run_hook_status" -eq 0 ]; then
    ok "HV_PUSH_OK=1 allows a hotfix push to main"
else
    bad "HV_PUSH_OK=1 did not escape — there is no hotfix route left: $run_hook_stderr"
fi

echo "== chamber: a LOCAL remote is still exempt (scratch repos need no opt-in)"
# Every shell harness in scripts/ pushes main to a scratch bare repo. The hook
# exits 0 for a local remote before any of this runs; if that ever changes,
# test-sluice.sh and this file both break in a way that looks like a real red.
HV_CENSUS_CLAIM_PATH="$claim" run_hook origin "$tmp/scratch-origin.git" refs/heads/main "$A" refs/heads/main "$R"
if [ "$run_hook_status" -eq 0 ]; then
    ok "a push to main on a local remote is allowed with no claim"
else
    bad "a local-remote push was refused — every scratch-repo harness in scripts/ now fails: $run_hook_stderr"
fi

echo "== chamber: MUTATION — without the no-claim branch, the out-of-band push returns"
# Restore from a COPY, never `git checkout --`.
mut2="$tmp/pre-push-mutant-noclaim"
# shellcheck disable=SC2016  # matching the hook's SOURCE TEXT, not evaluating it
python3 "$repo_root/scripts/mutate.py" --to "$mut2" "$hook" \
    '            refused=1
        fi
    fi
fi' \
    '            refused=0
        fi
    fi
fi' >/dev/null
set +e
mut2_out="$(printf 'refs/heads/main %s refs/heads/main %s\n' "$A" "$R" \
    | HV_CENSUS_CLAIM_PATH="$claim" env -u GIT_DIR -u GIT_INDEX_FILE \
      bash "$mut2" origin "$nonlocal_url" 2>&1)"
mut2_rc=$?
set -e
if [ "$mut2_rc" -eq 0 ]; then
    ok "MUTATION CONFIRMED: neutralising the no-claim branch's own refusal allows the push (the real hook refuses it)"
else
    bad "the mutant also refused — the no-claim test above may be passing for another reason: $mut2_out"
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]

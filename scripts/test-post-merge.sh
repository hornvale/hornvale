#!/usr/bin/env bash
# scripts/test-post-merge.sh — tests for scripts/hooks/post-merge.
#
# WHY THIS EXISTS. Task 3 of The Attestation added a second, tab-separated
# `author` column to docs/generated-paths.txt. Every OTHER bash reader of
# that file goes through an UNQUOTED $(...) command substitution, which
# word-splits on the embedded tab for free (bash's default IFS includes it).
# This hook instead built its pathspec array with
# `while IFS= read -r line; do generated+=("$line"); done`, keeping each
# WHOLE line — tab, author and all — as ONE array element, then passed that
# straight through as a QUOTED expansion:
# `git diff --name-only ORIG_HEAD HEAD -- "${generated[@]}"`. A quoted array
# expansion does not word-split, so every element became a literal
# "path<TAB>author" pathspec matching no tracked file, and the hook went
# silent on every merge, for all ten declared paths, forever. Nothing
# exercised this hook, so it shipped. This file closes that gap.
#
# Shaped after scripts/test-sluice-drain.sh (a simple pass/fail battery) and
# scripts/test-pre-push.sh (driving a hook against a real scratch git repo
# rather than reimplementing its logic in the test).
#
# HERMETICITY (scripts/CLAUDE.md): git exports GIT_DIR and GIT_INDEX_FILE to
# hooks, and from a linked worktree they are absolute paths into the real
# repository — GIT_DIR outranks `git -C`, so a temp directory alone is not
# isolation. Every git call below runs under `env -u GIT_DIR -u GIT_INDEX_FILE`.
set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
hook="$repo_root/scripts/hooks/post-merge"
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
g()   { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# A scratch repo carrying the REAL, current docs/generated-paths.txt (copied
# at test time, not reimplemented here as a hand-maintained duplicate that
# could itself drift from the production file's actual format) plus a
# tracked file at the first declared path.
scratch="$tmp/repo"; mkdir -p "$scratch"
g init -q -b main "$scratch"
g -C "$scratch" config user.email t@t
g -C "$scratch" config user.name t

mkdir -p "$scratch/docs"
cp "$repo_root/docs/generated-paths.txt" "$scratch/docs/generated-paths.txt"

declared_path="$(grep -v '^#' "$repo_root/docs/generated-paths.txt" | grep -v '^[[:space:]]*$' | head -1 | cut -f1)"
if [ -z "$declared_path" ]; then
    echo "test-post-merge: docs/generated-paths.txt yielded no declared path — cannot build a scratch fixture" >&2
    exit 1
fi
case "$declared_path" in
    */) target="$scratch/${declared_path}probe.md" ;;
    *)  target="$scratch/$declared_path" ;;
esac
mkdir -p "$(dirname "$target")"
echo "original" > "$target"
mkdir -p "$scratch/undeclared"
echo "original" > "$scratch/undeclared/probe.md"
g -C "$scratch" add -A
g -C "$scratch" commit -qm "C1: baseline"
C1="$(g -C "$scratch" rev-parse HEAD)"

# Runs the hook exactly as git would after a merge: ORIG_HEAD must already be
# set by the caller to the pre-merge tip, HEAD is the scratch repo's current
# tip. Sets run_status and run_stderr.
run_post_merge() {
    run_stderr="$(cd "$scratch" && env -u GIT_DIR -u GIT_INDEX_FILE bash "$hook" 2>&1)"
    run_status=$?
}

echo "== post-merge: fires when a merge touches a declared path (the case that was silently dead)"
echo "modified" > "$target"
g -C "$scratch" commit -qam "C2: touch the declared path"
g -C "$scratch" update-ref ORIG_HEAD "$C1"
run_post_merge
case "$run_stderr" in
    *"this merge touched generated artifacts"*)
        ok "the hook fires on a merge that touched a declared path" ;;
    *)
        bad "the hook stayed silent on a merge that touched a declared path — the exact defect this file exists to catch: $run_stderr" ;;
esac
case "$run_stderr" in
    *"$(basename "$target")"*)
        ok "the reported diff names the file that actually changed" ;;
    *)
        bad "the hook fired but did not name the changed file: $run_stderr" ;;
esac

echo "== post-merge: stays quiet when the merge touches nothing declared and no .rs file"
# This is the negative control the campaign's own thesis names: a test that
# always fires would pass the case above for the wrong reason.
g -C "$scratch" reset -q --hard "$C1"
echo "modified" > "$scratch/undeclared/probe.md"
g -C "$scratch" commit -qam "C3: touch only an undeclared, non-Rust path"
g -C "$scratch" update-ref ORIG_HEAD "$C1"
run_post_merge
if [ -z "$run_stderr" ] && [ "$run_status" -eq 0 ]; then
    ok "the hook stays quiet when nothing declared (and no .rs file) changed"
else
    bad "the hook spoke when nothing declared or Rust-shaped changed — a false positive here teaches people to stop trusting it on the merges that matter most: $run_stderr"
fi

echo "== post-merge: the advice line it prints is a command that actually works"
# NOTE: this printed line is an unquoted $(...) command substitution, not a
# quoted array — so, unlike the hook's own "${generated[@]}" construction
# above, bash's default IFS already word-splits it on the embedded tab. That
# is why this line was not *broken* by the missing author column the way the
# array was (see Step 3b's probe on the sibling CLAUDE.md one-liners, which
# found the same thing) — the `| cut -f1` added here is defensive
# consistency with every other guide, not a fix for a reproduced failure.
# This test asserts the POSITIVE claim directly: the exact command the hook
# prints, run for real, catches a genuine uncommitted drift.
g -C "$scratch" reset -q --hard "$C1"
echo "modified" > "$target"
g -C "$scratch" commit -qam "C4: touch the declared path again"
g -C "$scratch" update-ref ORIG_HEAD "$C1"
run_post_merge
advice="$(printf '%s\n' "$run_stderr" | grep -oE 'git diff --exit-code.*' | head -1)"
if [ -n "$advice" ]; then
    ok "the hook printed an advice command to extract"
else
    bad "no advice command found in the hook's output: $run_stderr"
fi
# Simulate "make rebaseline just regenerated something": an UNCOMMITTED
# working-tree change under the declared path — exactly the drift the advice
# line exists to surface once a human runs it after this hook speaks.
echo "regenerated, not yet committed" > "$target"
if [ -n "$advice" ] && (cd "$scratch" && env -u GIT_DIR -u GIT_INDEX_FILE bash -c "$advice" >/dev/null 2>&1); then
    bad "the printed advice command reported NO drift against an uncommitted change under the declared path — it would tell a human the merge is clean when it is not"
elif [ -n "$advice" ]; then
    ok "the printed advice command correctly detects the uncommitted drift it exists to catch"
fi
g -C "$scratch" checkout -q -- "$target" 2>/dev/null || true

printf '\ntest-post-merge: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]

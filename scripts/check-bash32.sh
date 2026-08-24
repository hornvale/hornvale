#!/usr/bin/env bash
# scripts/check-bash32.sh — every shipped shell script must run under the
# bash macOS actually provides.
#
# WHY THIS EXISTS AND WHY IT IS TWO CHECKS. macOS ships bash 3.2.57 as
# /bin/bash (its last GPLv2 release) and that is what a `#!/usr/bin/env bash`
# shebang resolves to on a Mac without a Homebrew bash earlier in PATH. Two
# independent failure modes follow, and a detector for one is blind to the
# other:
#
#   PARSE   bash 3.2 mis-parses an apostrophe inside a comment inside a
#           $(...) command substitution. It reports the error at a line far
#           below the real cause, which is how it survives review.
#   RUNTIME mapfile/readarray (bash 4.0+), declare -A (4.0+) and the
#           case-expansion parameter forms (4.0+) PARSE cleanly under 3.2
#           and fail only when the line is reached.
#
# Shellcheck's own lint does not substitute for either half: it reported
# scripts/test-worktree-freshness.sh completely clean (exit 0) while
# /bin/bash could not parse the file at all. A clean lint on an unparseable
# file is a false green.
#
# DIRECTION THIS CHECK ENFORCES: every bash-shebanged file under scripts/
# parses under the system bash AND contains no bash-4-only construct. It does
# NOT prove the script is correct, and it does not look outside scripts/.
#
# A THIRD INSTANCE OF THE VERY CLASS THIS FILE EXISTS TO CATCH, found while
# writing it. A `case ... esac` placed directly inside a `$(...)` command
# substitution does not parse under bash 3.2 -- confirmed by reduction to
# `x=$(case "hello" in *ell*) echo hi ;; esac)`, which fails identically. The
# file-discovery loop below therefore uses `grep -q` instead of `case` to
# classify a shebang line; do not reintroduce a `case` inside this file's own
# `$(...)` blocks.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

SYS_BASH="${SYS_BASH:-/bin/bash}"
status=0

# Only bash-shebanged files; a POSIX sh script is not this check's business.
files=$(git ls-files scripts | while read -r f; do
    [ -f "$f" ] || continue
    if head -1 "$f" 2>/dev/null | grep -q 'bash'; then
        printf '%s\n' "$f"
    fi
done)

for f in $files; do
    if ! "$SYS_BASH" -n "$f" 2>/dev/null; then
        printf 'check-bash32: PARSE FAILS under %s: %s\n' "$SYS_BASH" "$f" >&2
        # `|| true`: under `pipefail`, this pipeline's own exit status is
        # bash -n's non-zero one (that is the point -- we are re-running it
        # to capture the message), and left unguarded that would trip
        # `set -e` and abort the whole script after the FIRST parse failure,
        # never reaching the construct-grep loop below. Found while running
        # this exact check: it silently ate the second known defect.
        "$SYS_BASH" -n "$f" 2>&1 | sed 's/^/    /' >&2 || true
        status=1
    fi
done

# Construct grep, skipping comment lines so the two files that ALREADY
# document avoiding these constructs stay green. Covers mapfile/readarray,
# declare -A, and the bash-4+ case-expansion parameter forms
# (${var,,} / ${var^^} / ${var,} / ${var^}, with or without a trailing
# pattern argument).
for f in $files; do
    hits=$(grep -nE '^[^#]*(\bmapfile\b|\breadarray\b|declare[[:space:]]+-[a-zA-Z]*A|\$\{[A-Za-z_][A-Za-z0-9_]*(\[[^]]*\])?[,^]{1,2}[}A-Za-z])' "$f" || true)
    if [ -n "$hits" ]; then
        printf 'check-bash32: BASH 4+ CONSTRUCT in %s:\n' "$f" >&2
        printf '%s\n' "$hits" | sed 's/^/    /' >&2
        status=1
    fi
done

[ "$status" -eq 0 ] && printf 'check-bash32: ok\n'
exit "$status"

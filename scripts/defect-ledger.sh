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

# The crates a human touched, by the same directory->crate mapping
# scripts/gate-fast.sh used to use before it was deleted (The Ballast,
# Nathan-authorised: it was a working undocumented back door left behind when
# `make gate-fast` was retired into a refusing signpost — see decision 0132).
# That deletion took `cli/tests/gate_fast_closure.rs` with it, which was the
# only *tested* copy of this directory->crate closure; this script's copy was
# always a separate, untested reimplementation, not a call into that one.
# Overapproximation is fine here; this is a correlate, not a gate.
#
# BOTH COMMANDS ARE REQUIRED. `git diff --name-only HEAD` lists tracked
# modifications only — it never sees an untracked file, and a newly added
# file (a new test, a new module, a new fixture) is one of the commonest
# causes of a red gate. Relying on `diff` alone made that exact case record
# `(none)`, indistinguishable from "nothing was changed", on precisely the
# gates most worth explaining. `ls-files --others --exclude-standard` adds
# untracked-but-not-ignored paths so a brand-new file is visible too;
# `--exclude-standard` keeps `.gitignore`d noise (`target/`, etc.) out.
changed_crates="$(
    { git -C "$ROOT" diff --name-only HEAD 2>/dev/null; \
      git -C "$ROOT" ls-files --others --exclude-standard 2>/dev/null; } \
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
    {
        printf '# Defect ledger for %s — which test caught what, and on what change.\n' "$HOST"
        printf '# Written by scripts/defect-ledger.sh on every RED gate. Committed:\n'
        printf '# git log -p on this file is the archaeology of what the suite defends.\n'
        printf 'when_utc\tcommit\tbranch\tchanged_crates\tfailed_test\n'
    } > "$LEDGER"
fi

when="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
commit="$(git -C "$ROOT" rev-parse --short HEAD 2>/dev/null || echo '-')"
branch="$(git -C "$ROOT" branch --show-current 2>/dev/null || echo '-')"

printf '%s\n' "$failed" | while IFS= read -r t; do
    printf '%s\t%s\t%s\t%s\t%s\n' "$when" "$commit" "$branch" "$changed_crates" "$t" >> "$LEDGER"
done

echo "defect-ledger: recorded $(printf '%s\n' "$failed" | wc -l | tr -d ' ') failing test(s) to $LEDGER" >&2

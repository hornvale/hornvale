#!/usr/bin/env bash
# test-reconciliation-union.sh — prove what `merge=union` does to the campaign
# reconciliation ledger, in both directions, against real git merges.
#
# WHY. docs/audits/campaign-reconciliation.tsv is one row per campaign, keyed
# by slug, appended at close — and it became the single most common conflict in
# the merge queue: six campaigns bounced or collided on it in twelve hours on
# 2026-09-05/06, every one on the tail, every one resolved by hand.
#
# Union is not free here. 7 of the last 30 commits to that file contain
# deletions, so a quarter of them rewrite rows rather than appending, and
# union's documented caveat applies. This suite exists so the trade is
# demonstrated rather than argued: the good case really does resolve
# automatically, and the bad case really does land in the duplicate-key guard
# instead of passing silently.
set -uo pipefail
pass=0; fail=0
ok()  { echo "  ok: $1"; pass=$((pass+1)); }
bad() { echo "  FAIL: $1"; fail=$((fail+1)); }

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
r="$tmp/repo"; mkdir -p "$r"
g() { env -u GIT_DIR -u GIT_INDEX_FILE git -C "$r" "$@"; }

g init -q -b main .
g config user.email x@x; g config user.name x
mkdir -p "$r/docs/audits"
printf 'docs/audits/ledger.tsv merge=union\n' > "$r/.gitattributes"
printf 'key\tdisposition\nbase-one\tshipped\nbase-two\tshipped\n' > "$r/docs/audits/ledger.tsv"
g add -A; g commit -qm base

echo "== union: two campaigns appending different rows both survive"
g checkout -q -b campaign/a main
printf 'the-a\tshipped\n' >> "$r/docs/audits/ledger.tsv"; g add -A; g commit -qm a
g checkout -q -b campaign/b main
printf 'the-b\tshipped\n' >> "$r/docs/audits/ledger.tsv"; g add -A; g commit -qm b
g checkout -q campaign/a
if g merge -q --no-edit campaign/b 2>/dev/null; then
    ok "the merge resolved with no conflict (this is the six-bounce case, gone)"
else
    bad "union did not resolve two appends: $(g diff --name-only --diff-filter=U)"
fi
body="$(cat "$r/docs/audits/ledger.tsv")"
if grep -q "^the-a" <<<"$body" && grep -q "^the-b" <<<"$body"; then
    ok "BOTH campaigns' rows are present — neither side was dropped"
else
    bad "a row was lost: $body"
fi
dupes="$(awk -F'\t' 'NR>1{print $1}' "$r/docs/audits/ledger.tsv" | sort | uniq -d)"
if [ -z "$dupes" ]; then ok "no duplicate key in the good case"
else bad "unexpected duplicate key: $dupes"; fi

echo "== union: two campaigns EDITING THE SAME ROW produce a duplicate key, loudly"
# This is the case the caveat is about. It must not resolve silently-wrong;
# it must produce something the existing guard rejects.
g checkout -q -b campaign/c main
sed -i 's/^base-one\tshipped/base-one\trefuted/' "$r/docs/audits/ledger.tsv"; g add -A; g commit -qm c
g checkout -q -b campaign/d main
sed -i 's/^base-one\tshipped/base-one\tpartial/' "$r/docs/audits/ledger.tsv"; g add -A; g commit -qm d
g checkout -q campaign/c
g merge -q --no-edit campaign/d 2>/dev/null
dupes="$(awk -F'\t' 'NR>1{print $1}' "$r/docs/audits/ledger.tsv" | sort | uniq -d)"
if [ -n "$dupes" ]; then
    ok "THE CAVEAT — a same-row edit yields a duplicate key ($dupes), which docs_consistency rejects at gate time"
else
    bad "a same-row edit resolved to something with no duplicate key — the failure would be SILENT: $(cat "$r/docs/audits/ledger.tsv")"
fi

echo "== control: without the attribute, the good case CONFLICTS"
# Proves the attribute is doing the work, not git's default behaviour.
r2="$tmp/repo2"; mkdir -p "$r2"
g2() { env -u GIT_DIR -u GIT_INDEX_FILE git -C "$r2" "$@"; }
g2 init -q -b main .; g2 config user.email x@x; g2 config user.name x
mkdir -p "$r2/docs/audits"
printf 'key\tdisposition\nbase-one\tshipped\n' > "$r2/docs/audits/ledger.tsv"
g2 add -A; g2 commit -qm base
g2 checkout -q -b campaign/a main
printf 'the-a\tshipped\n' >> "$r2/docs/audits/ledger.tsv"; g2 add -A; g2 commit -qm a
g2 checkout -q -b campaign/b main
printf 'the-b\tshipped\n' >> "$r2/docs/audits/ledger.tsv"; g2 add -A; g2 commit -qm b
g2 checkout -q campaign/a
if g2 merge -q --no-edit campaign/b 2>/dev/null; then
    bad "CONTROL FAILED: the same two appends merged cleanly WITHOUT the attribute, so this suite proves nothing about union"
else
    ok "CONTROL: without merge=union the identical case conflicts — the attribute is what resolves it"
fi

echo "== the real file carries the attribute"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
if grep -q "^docs/audits/campaign-reconciliation.tsv merge=union" "$root/.gitattributes"; then
    ok "docs/audits/campaign-reconciliation.tsv is declared merge=union"
else
    bad "the attribute is missing from .gitattributes"
fi

printf '\ntest-reconciliation-union.sh: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]

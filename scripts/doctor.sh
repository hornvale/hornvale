#!/usr/bin/env bash
# scripts/doctor.sh — the repo self-map: one read to orient a fresh session
# (or a cold-started subagent).
#
# Prints the orientation knowledge otherwise scattered across CLAUDE.md, the
# decision log, and the Makefile. Each line is a POINTER to the
# authoritative source, not a restatement, so this script cannot rot far.
# Read-only: never mutates anything.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

section() { printf '\n== %s\n' "$1"; }

echo "hornvale doctor — the repo self-map"

section "Layering (enforced: cli/tests/architecture.rs; picture: book/src/reference/layering.md)"
echo "  kernel -> domains/* -> windows/* -> cli"
echo "  a domain depends on the kernel and NOTHING else; windows/worldgen is the"
echo "  composition root; external deps allowlist: serde, serde_json"

section "The gate (cost-ordered; Makefile / CLAUDE.md Commands)"
make -s help

section "Determinism contracts (CLAUDE.md Determinism section is authoritative)"
echo "  - same seed + pins => byte-identical worlds and artifacts; the seed is a world's identity"
echo "  - floats quantized to 8 significant digits at serialization boundaries ONLY (kernel/src/quantize.rs)"
echo "  - no wall-clock time; no HashMap/HashSet (clippy.toml disallowed-types)"
echo "  - stream labels and consumption order are save-format contracts (docs/decisions/0006-*)"

section "Committed generated artifacts"
echo "  - regenerate all: make rebaseline (scripts/regenerate-artifacts.sh; CI drift-checks the set)"
echo "  - byte-golden test fixtures: make rebaseline-goldens (REBASELINE=1 accept path; kernel/src/golden.rs)"
echo "  - census review surface: make lab-diff STUDY=<name>"
echo "  - historical pre-<campaign> pins are frozen history: scripts/freeze-fixture.sh, never rebaselined"

section "Documentation map"
decision_count=$(find docs/decisions -name '*.md' ! -name 'README.md' | wc -l | tr -d ' ')
echo "  - docs/README.md — what knowledge lives where and how ideas flow"
echo "  - docs/decisions/ — ${decision_count} ratified records (append-only; grep before relitigating)"
echo "  - book/src/frontier/idea-registry.md — the idea registry (check before proposing anything)"
echo "  - WORKFLOW_IMPROVEMENTS_PLAN.md — the workflow/tooling backlog and its stages"

section "Live state"
echo "  branch: $(git branch --show-current)   dirty files: $(git status --porcelain | wc -l | tr -d ' ')"
git worktree list | sed 's/^/  /'

section "Decisions never cited in sources or docs (informational, not a gate)"
# Cites often wrap across lines (the "decision" keyword ends one comment line,
# the backticked slug starts the next), which a per-line grep misses. Build a
# comment-marker-stripped, line-joined corpus once so wrapped cites count. The
# match below also tolerates a short gap after the keyword (e.g. a shared
# "(decisions `a` / `b`)" list crediting both slugs), bounded so it can't leap
# past `.`, `;`, or `)` — which is what keeps it from crediting an unrelated
# "(0003)" that only shares a sentence with an earlier "ADR 0002".
corpus_file="$(mktemp "${TMPDIR:-/tmp}/hv-doctor-corpus.XXXXXX")"
trap 'rm -f "$corpus_file"' EXIT
find kernel domains windows cli tools scripts docs book CLAUDE.md \
    -type f \( -name '*.rs' -o -name '*.sh' -o -name '*.md' \) \
    -not -path '*/target/*' -not -path '*/.git/*' -not -path 'docs/decisions/*' \
    -print0 2>/dev/null \
    | xargs -0 awk '{print}' 2>/dev/null \
    | sed -E 's@^[[:space:]]*(///|//!|//|#)[[:space:]]?@@' \
    | tr '\n' ' ' \
    > "$corpus_file"
printf ' ' >> "$corpus_file"

orphans=0
for f in docs/decisions/*.md; do
    stem="$(basename "$f" .md)"
    [ "$stem" = "README" ] && continue
    case "$stem" in
        [0-9][0-9][0-9][0-9]-*) pat="${stem:0:4}|${stem:5}" ;;
        *) pat="$stem" ;;
    esac
    if ! grep -qE "(decision|decisions|ADR)[^.;)]{0,80}(\`)?($pat)[^a-z0-9-]" "$corpus_file"; then
        echo "  $stem"
        orphans=$((orphans + 1))
    fi
done
[ "$orphans" -eq 0 ] && echo "  (none)"

exit 0

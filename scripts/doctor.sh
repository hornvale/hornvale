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

# The generated block opens with its own `== ` heading and no leading blank
# line, so supply the separator `section()` would have printed. Without this
# the first heading collides with the banner above.
echo
cargo run --quiet --manifest-path tools/digest/Cargo.toml -- render doctor

section "The gate (cost-ordered; Makefile / CLAUDE.md Commands)"
make -s help

section "Live state"
echo "  branch: $(git branch --show-current)   dirty files: $(git status --porcelain | wc -l | tr -d ' ')"
git worktree list | sed 's/^/  /'

# The Cairn. `tools/board/target/` is gitignored and per-worktree, so a fresh
# worktree has no binary until `make prewarm` builds one — and the ambient
# SessionStart render deliberately stays silent when it is missing (it may
# never compile or slow a session). Silence there is correct; silence HERE is
# not. This command is human-invoked orientation, so a missing binary gets a
# line saying so rather than nothing at all: prefer the failure you can see.
board_bin=""
for candidate in tools/board/target/release/board tools/board/target/debug/board; do
  [ -x "${candidate}" ] && board_bin="${candidate}" && break
done
if [ -n "${board_bin}" ]; then
  board_out="$("${board_bin}" read 2>/dev/null || true)"
  if [ -n "${board_out}" ]; then
    section "The board"
    printf '%s\n' "${board_out}" | sed 's/^/  /'
  fi
else
  section "The board"
  echo "  the board binary is not built, so the session-start render, this"
  echo "  block, and the sluice request's hold-off advisory are all inert — run"
  echo "  \`make prewarm\` (or \`make board\`) to build it"
fi

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
find kernel domains windows cli clients tools scripts docs book CLAUDE.md \
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

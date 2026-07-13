#!/usr/bin/env bash
# scripts/gate-fast.sh — the affected-only iteration gate (TOOL-gate-fast).
#
# ITERATION TOOL ONLY. `make gate` (fmt + clippy + workspace tests) stays
# the commit gate — always run it before committing. This script mechanizes
# CLAUDE.md's "scope tests to what changed" prose: it maps the changed file
# set to the crates a change can affect and scopes clippy/test to that
# subset, so an inner iteration loop doesn't pay for `cargo test --workspace`
# every time. Overapproximation is fine; missing a dependent crate is not —
# anything outside the mapped directories (kernel/, domains/*, windows/*,
# cli/) falls back to the full gate.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

section() { printf '\n== %s\n' "$1"; }

windows_list=(hornvale-almanac hornvale-historiography hornvale-lab hornvale-scene hornvale-worldgen)

add_pkg() { # dedupe-append a package name onto $packages
    local p="$1" existing
    for existing in ${packages+"${packages[@]}"}; do
        [[ "$existing" == "$p" ]] && return
    done
    packages+=("$p")
}

merge_base="$(git merge-base main HEAD 2>/dev/null || true)"
if [[ -z "$merge_base" ]]; then
    echo "gate-fast: no merge-base with main found — can't scope; falling back to the full gate"
    exec make gate
fi

# Changed set: everything since the merge-base with main, plus uncommitted
# changes (tracked or not) in the working tree. `git status --porcelain`
# rename lines read "R  old -> new"; keep only the new path.
changed_files="$(
    {
        git diff --name-only "$merge_base"
        git status --porcelain | sed -E 's/^.{2} //; s/.* -> //'
    } | sort -u
)"

if [[ -z "$changed_files" ]]; then
    echo "gate-fast: no changes vs $(git rev-parse --short "$merge_base") (main) — nothing to scope; running the full gate"
    exec make gate
fi

section "Changed files (vs $(git rev-parse --short "$merge_base"), plus working tree)"
printf '%s\n' "$changed_files" | sed 's/^/  /'

full_gate=0
fallback_reasons=()
packages=()
explain=()

while IFS= read -r path; do
    [[ -z "$path" ]] && continue
    case "$path" in
        kernel/*)
            full_gate=1
            fallback_reasons+=("$path (kernel/ — every crate depends on it)")
            ;;
        domains/*/*)
            domain="${path#domains/}"
            domain="${domain%%/*}"
            crate="hornvale-${domain}"
            add_pkg "$crate"
            add_pkg "hornvale-worldgen"
            for w in "${windows_list[@]}"; do add_pkg "$w"; done
            add_pkg "hornvale"
            explain+=("$path -> $crate + hornvale-worldgen + all windows + hornvale (cli)")
            ;;
        windows/worldgen/* | windows/almanac/*)
            # hornvale-worldgen (the composition root) depends on
            # hornvale-almanac (it renders almanac views) — see
            # windows/worldgen/Cargo.toml — so an almanac change must run
            # worldgen's tests and worldgen's own dependents
            # (hornvale-lab, hornvale-scene). Almanac is the only window
            # with an incoming edge today; if another window ever gains
            # one, give it the same treatment (the dependency rules live
            # in cli/tests/architecture.rs).
            for w in "${windows_list[@]}"; do add_pkg "$w"; done
            add_pkg "hornvale"
            explain+=("$path -> all windows + hornvale (cli)")
            ;;
        windows/*/*)
            window="${path#windows/}"
            window="${window%%/*}"
            crate="hornvale-${window}"
            add_pkg "$crate"
            add_pkg "hornvale"
            explain+=("$path -> $crate + hornvale (cli)")
            ;;
        cli/*)
            add_pkg "hornvale"
            explain+=("$path -> hornvale (cli)")
            ;;
        *)
            full_gate=1
            fallback_reasons+=("$path (outside kernel/, domains/*, windows/*, cli/ — can't scope)")
            ;;
    esac
done <<<"$changed_files"

if [[ "$full_gate" -eq 1 ]]; then
    section "Full-gate fallback"
    printf '  %s\n' "${fallback_reasons[@]}"
    echo "  falling back to \`make gate\` (fmt + clippy + workspace tests)"
    exec make gate
fi

packages_sorted="$(printf '%s\n' "${packages[@]}" | sort -u)"
pkg_flags=()
while IFS= read -r p; do
    pkg_flags+=(-p "$p")
done <<<"$packages_sorted"

section "Selected packages"
printf '%s\n' "$packages_sorted" | sed 's/^/  /'

section "Why"
printf '  %s\n' "${explain[@]}"

section "cargo fmt --check"
cargo fmt --check

section "cargo clippy (scoped)"
cargo clippy "${pkg_flags[@]}" --all-targets -- -D warnings

section "cargo nextest + doctests (scoped)"
command -v cargo-nextest >/dev/null 2>&1 || { echo "cargo-nextest not found — cargo install cargo-nextest (decision 0027)"; exit 1; }
cargo nextest run "${pkg_flags[@]}"
cargo test "${pkg_flags[@]}" --doc

section "Verdict"
echo "  gate-fast OK for: ${packages_sorted//$'\n'/, }"
echo "  iteration tool only — \`make gate\` is still the commit gate"

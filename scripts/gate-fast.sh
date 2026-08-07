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

# Dependents are DERIVED from the workspace's own Cargo.toml files, never
# listed here. A hand-maintained roster is what made this script violate its
# own header: `hornvale-vessel` was never added to it, so a change under
# `windows/locale/` scoped to locale alone while vessel, scene and lab all
# depend on locale — and a stale vessel golden rode through green (The Handle,
# 2026-08-06). Deriving means a new crate, or a new edge between two existing
# ones, is picked up the moment its Cargo.toml says so.

# The package name a workspace directory declares.
pkg_name_of_dir() {
    sed -n 's/^name = "\(.*\)"/\1/p' "$1/Cargo.toml" | head -1
}

# Every workspace crate directory.
workspace_dirs() {
    local d
    for d in kernel domains/*/ windows/*/ cli; do
        d="${d%/}"
        [[ -f "$d/Cargo.toml" ]] && printf '%s\n' "$d"
    done
}

# Packages whose Cargo.toml declares a direct dependency on $1. Internal deps
# are always written `hornvale-foo = { path = … }` at line start, so anchoring
# on the name cannot match a path fragment or a comment.
direct_dependents() {
    local pkg="$1" dir name
    while IFS= read -r dir; do
        if grep -q "^${pkg} = " "$dir/Cargo.toml"; then
            name="$(pkg_name_of_dir "$dir")"
            [[ -n "$name" ]] && printf '%s\n' "$name"
        fi
    done < <(workspace_dirs)
}

add_pkg() { # dedupe-append; 0 if newly added, 1 if already present
    local p="$1" existing
    for existing in ${packages+"${packages[@]}"}; do
        [[ "$existing" == "$p" ]] && return 1
    done
    packages+=("$p")
    return 0
}

# Add $1 and, transitively, everything that depends on it. `add_pkg`'s
# already-present answer is the memo that terminates the recursion.
add_closure() {
    local pkg="$1" dep
    if ! add_pkg "$pkg"; then
        return 0
    fi
    while IFS= read -r dep; do
        [[ -n "$dep" ]] && add_closure "$dep"
    done < <(direct_dependents "$pkg")
}

# `--closure <pkg>`: print the packages a change to <pkg> would select, one
# per line, and exit. This is the seam `cli/tests/gate_fast_closure.rs` checks
# against the workspace's real dependency graph — the script answers for
# itself rather than the test re-implementing it, so the two cannot drift the
# way a duplicated rule does.
if [[ "${1:-}" == "--closure" ]]; then
    if [[ -z "${2:-}" ]]; then
        echo "usage: gate-fast.sh --closure <package>" >&2
        exit 2
    fi
    packages=()
    add_closure "$2"
    printf '%s\n' "${packages[@]}" | sort -u
    exit 0
fi

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
        domains/*/* | windows/*/*)
            dir="$(printf '%s' "$path" | cut -d/ -f1-2)"
            if [[ ! -f "$dir/Cargo.toml" ]]; then
                full_gate=1
                fallback_reasons+=("$path (no $dir/Cargo.toml — can't name the crate)")
                continue
            fi
            crate="$(pkg_name_of_dir "$dir")"
            if [[ -z "$crate" ]]; then
                full_gate=1
                fallback_reasons+=("$path ($dir/Cargo.toml declares no name)")
                continue
            fi
            before="${#packages[@]}"
            add_closure "$crate"
            explain+=("$path -> $crate + $(( ${#packages[@]} - before - 1 )) dependent(s)")
            ;;
        cli/*)
            add_closure "hornvale"
            explain+=("$path -> hornvale (cli; nothing depends on it)")
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
command -v cargo-nextest >/dev/null 2>&1 || { echo "cargo-nextest not found — cargo install cargo-nextest (decision 0040)"; exit 1; }
cargo nextest run "${pkg_flags[@]}"
cargo test "${pkg_flags[@]}" --doc

section "Verdict"
echo "  gate-fast OK for: ${packages_sorted//$'\n'/, }"
echo "  iteration tool only — \`make gate\` is still the commit gate"

#!/usr/bin/env bash
# scripts/sluice-phases.sh — which phases a candidate actually needs.
#
# Sourced by `sluice-run.sh`, and by `scripts/test-sluice.sh` so the rule can
# be tested without standing up a chamber. Same shape as
# `sluice-headline.sh`: one implementation, two callers, no second copy to
# drift.
#
# WHY THIS EXISTS. campaign/the-illumination's docs-recovery merge — three
# files, 48 insertions, a retrospective and a registry row — paid the full
# ladder on the one serial box:
#
#   artifacts    79.9 s      outboard    33.1 s      gate       323.6 s
#   seam-guard  966.4 s      clients    250.4 s      heavy     1907.8 s
#
# 3561 s, of which 3124 s (88%) went to three phases that cannot observe a
# prose change. Meanwhile an 82-commit campaign waited behind it.
#
# THE `heavy` DROP IS LIVE AGAIN, AND WAS INERT FOR NINE DAYS. Decision 0148
# had already removed `heavy` from both chamber phase lists when this file was
# written, so dropping it here could never fire; decision 0426 (2026-08-28) put
# it back, and this skip is now the reason a prose-only candidate does not pay
# the ~465.8 s the tier costs (derived once, next to its inputs, in 0426). `seam-guard` remains off the phase lists entirely,
# so its entry in the drop list is still inert — kept deliberately, because a
# drop list that fails toward running LESS is the wrong direction to prune.
#
# WHICH THREE, AND WHY THEY ARE SAFE TO SKIP. `seam-guard` neutralises
# FUNCTIONS and runs scoped test suites — prose has no seams. `clients` builds
# and checks the wasm and Deno trees under clients/, which no allowlisted path
# can reach. `heavy` is the live-worldgen tier and cannot be moved by a file
# no code reads.
#
# WHICH THREE ARE KEPT, AND WHY. `gate` carries docs_consistency — the cite
# checker, registry row form, decision-number uniqueness — which is exactly
# what a prose change CAN break. `artifacts` regenerates and drift-checks
# every declared generated path. `outboard` is 33 s and runs the queue's own
# suite plus the shell lint.
#
# THE ALLOWLIST IS HAND-WRITTEN PROSE, deliberately narrower than "docs and
# book". Every path under book/ that a SKIPPED phase authors is excluded by
# construction, because the whole argument is that no skipped phase could have
# observed the change:
#
#   docs/**                     retrospectives, decisions, specs, timings, audits
#   book/src/chronicle/**       campaign narrative
#   book/src/frontier/**        idea registry and essays
#   book/src/open-questions.md  the confidence gradient
#   book/src/SUMMARY.md         the book's table of contents
#
# Excluded on purpose: book/src/gallery/ (clients builds atlas.js and the wasm
# there), book/src/laboratory/ (heavy authors the-history and the-sounding),
# book/src/domesday/ and book/src/reference/ (generated elsewhere).
#
# THE RULE FAILS TOWARD RUNNING MORE. One .rs file, one Cargo entry, one
# client fixture anywhere in the range and the candidate takes the full
# ladder. An empty or unreadable path list is NOT prose-only either: a
# classifier that cannot see the change must never be the reason a phase is
# skipped.

# Is every path in the newline-separated list hand-written prose?
# Returns 0 (true) only if the list is non-empty AND every entry is allowlisted.
sluice_is_prose_only() {
    local changed="$1" pth seen=0
    [ -n "$changed" ] || return 1
    while IFS= read -r pth; do
        [ -n "$pth" ] || continue
        seen=1
        case "$pth" in
            docs/*|book/src/chronicle/*|book/src/frontier/*|book/src/open-questions.md|book/src/SUMMARY.md) ;;
            *) return 1 ;;
        esac
    done <<EOF
$changed
EOF
    [ "$seen" = "1" ]
}

# Drop the three phases a prose change cannot affect, preserving order.
sluice_drop_expensive_phases() {
    # shellcheck disable=SC2086  # $1 is a space-separated list; splitting is the point.
    printf '%s\n' $1 | grep -vxE 'seam-guard|clients|heavy' | tr '\n' ' ' | sed 's/ $//'
}

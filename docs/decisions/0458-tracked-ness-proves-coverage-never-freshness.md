# 0458. Tracked-ness proves coverage, never freshness

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0456](0456-a-rule-stated-in-two-places-needs-a-bidirectional-agreement-test.md),
[0459](0459-the-freshness-reader-observes-both-directions-and-never-gates.md) ·
[The Attestation](../../book/src/chronicle/the-attestation.md)

In the context of `cli/tests/suite/generated_paths.rs` already asserting that
every declared path is one `regenerate-artifacts.sh` writes, and that
assertion matching a declared path as a literal substring of the script, we
decided that **the tracked-ness check answers "is this file declared and does
some author's source mention it", never "did that author actually run
recently" — and closing the first question does not open the second**,
accepting that a genuinely stale author needs a different instrument, built
separately (0459).

## Context

The existing check's own header already named a blindness, in the direction
it anticipated: a path written through a shell variable would read as
undeclared. This campaign found the same weakness pointed the other way, and it fails
*unsafe* — a false negative — rather than the false positive the header
anticipated: `book/src/laboratory/` satisfies the check because the string
occurs somewhere in `regenerate-artifacts.sh`, while 814 of its files are
never written by it.
Fixing that (Task 3: match per declared author's own source, not one
substring test for the whole file) makes the check strictly more accurate
about *identity* — which source could plausibly write this path — but it does
not, and structurally cannot, tell a reader whether that source has run this
month, this year, or ever since the workflow that used to trigger it was
deleted (`census-of-skies`, whose automated author decision 0125 removed
outright).

## The rule

A drift check (`git diff --exit-code` over a declared path) and a tracked-
ness check (a path's author-source plausibly writes it) both certify a
*state*: the file is covered, the file could be written by what claims to
write it. Neither certifies an *event*: that the author actually ran, and
when. Treating a green tracked-ness result as evidence of recent freshness is
the exact conflation this campaign's three original defects share — a check
that verifies a state read as if it verified an action.

## Consequences

- Freshness gets its own instrument (`cli/src/attest.rs`, decision 0459),
  reading `docs/timings.md`'s own record of what ran, rather than being
  smuggled into the tracked-ness check as a side effect.
- A future author-source rewrite (a path moved behind a new script, a study
  renamed) should extend the tracked-ness check's per-author source map, not
  assume it says anything about cadence.

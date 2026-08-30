# 0457. A generated path's author absence is declared, not deleted

**Status:** Accepted (2026-08-30) · **Decider:** Nathan ·
**Relates:** [0028](0028-the-bare-ok-rubric.md) (the same append-and-review
ratchet idiom — `waiver(<reason>)` — applied to a different registry) ·
[The Attestation](../../book/src/chronicle/the-attestation.md)

In the context of `docs/generated-paths.txt` gaining a second column naming
each declared path's author, and that column exposing 155 files with no real
author and 684 more declared under an author wrong for them, we decided that
**a path nothing writes gets `none(<reason>)`, not silent deletion of its
declaration or a quiet narrowing of what the check covers** — accepting that
the file grows from 11 rows to 72, most of them a directory split into the
authors actually responsible for its parts.

## Context

The task that found this was staged to stop and ask, not to choose: three
options were offered (narrow the declaration to the generated subtree, remove
it entirely, or keep it and accept the check is partial), all of which treat
an unauthored file as a problem to make invisible again. Nathan chose a
fourth, unoffered option — declare the absence, with a reason — because the
project already has this idiom twice over: type-audit's `waiver(<reason>)`
and seam-guard's `expect(survives: <why>)` both convert "this thing does not
have the property you'd expect" into a reviewable, append-checked fact rather
than a hole nothing names. A reasonless `none` is a parse error, exactly as a
reasonless `waiver` or `expect(survives:)` would be — the reason is the row's
entire value, and a one-directional acknowledgement nobody has to justify
rots.

## The rule

- An author name is a roster set name (`artifacts`, `census`, `heavy`) — the
  same vocabulary `scripts/lane-sets.tsv` already publishes — never an
  invented label. Inventing a second vocabulary for a thing the repository
  had already named would have been this campaign's own §1.1 defect,
  committed by the campaign fixing it.
- `none(<reason>)` means no author is expected, checked for staleness (a
  `none` row a source now actually writes is `STALE-DECL`, not silently
  correct forever) but never treated as an author that should have a ledger
  row.
- Two invariants gate any future change to this file: no file drops out of
  tracked-file coverage (proved by a set diff, not eyeballed), and no row's
  author is wrong for any file it covers.

## Consequences

- `docs/generated-paths.txt` went from 11 rows to 72 with the coverage set
  proved byte-identical (939 tracked files, before and after) — the campaign
  found no coverage loss, and the two invariants above make a future loss a
  build failure rather than a silent narrowing.
- `census-of-skies`'s `none(...)` reason names the CI workflow that once
  authored it and decision 0125, which deleted that workflow — materially
  different from the other eight frozen studies' reasons, which were manual
  from the day each was added, because the two absences are not the same
  fact and the reason text is where that distinction has to live.
- The next campaign's freshness reader (0459) must treat `none(...)` as its
  own category, never as an author that failed to run — the ruling this
  decision records is precisely what makes that distinction necessary.

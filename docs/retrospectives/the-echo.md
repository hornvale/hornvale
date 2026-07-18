# Retrospective: The Echo

One page, process only. Product story: the chronicle entry.

## What worked

- **The insertion argument held.** An unplanned campaign slotted between
  C2 and C3 on a timing argument (the inventory is the smallest it will
  ever be) and closed same-day: 5 tasks, 3 review-fix loops, a fable
  final review that confirmed READY TO MERGE by measurement (it probed
  seeds 42/7/1000 beyond the pinned corpus and exhaustively verified the
  quantity re-render seam over 0.0–499.9). Inserting small and early beat
  retrofitting large and late — the standing-gate amendment now forces
  every future construction to be bidirectional at birth.
- **Verbatim-code plans made cheap tasks cheaper and reviews sharper.**
  T1/T2 were largely transcription; the reviews then spent their budget
  on the parts plans can't pre-verify — which is where all three real
  defects were found.
- **The zero-drift constraint was a working tripwire, twice.** The
  realizer rewrite shipped byte-identical (every exact-string test + the
  committed gallery as the net), and T5's regen-empty check caught T4's
  missed type-audit re-pin *by design* — the check refused to proceed
  past someone else's lag instead of absorbing it.
- **Mutation evidence on demand.** The T4 fix returned with pasted
  mutant-kill output (both formatting branches) unprompted beyond the
  dispatch's ask — the measure-don't-narrate norm is propagating into
  fix-loop behavior.

## What the campaign got wrong, and what it taught

- **A review claim was false until a reviewer measured it.** T4's report
  claimed surface-value round-trips were verified; the test only checked
  key presence. The reviewer diffed the claim against the assertions and
  said so plainly. Lesson (recurring): **a report's "verified" is a claim
  about the code, not evidence** — reviewers must find the assertion, not
  the sentence about it.
- **Error surface designed before reachability was proven.** The plan
  specified four `ParseError` variants; the complement filter's own
  guarantees made one unreachable. The reviewer proved it rather than
  demanding a test for dead code, and the variant was removed with the
  proof documented. Lesson: **an error variant is a claim that a failure
  is reachable — prove it or don't declare it.**
- **The re-pin-lags-its-commit failure recurred in a new costume** (C2:
  a golden fixture; here: the type-audit report counters). The pattern:
  a task's drift-surface extends beyond the crates it edited, and the
  implementer's check scope doesn't. The zero-drift close check catches
  it, but one task late. Standing candidate fix: the implementer preamble
  could gain a "regen + diff --stat before commit" line the way it
  carries the census ban; deferred to the process backlog rather than
  patched mid-campaign.
- **Parallel-campaign cadence held cheaply this time**: main moved once
  mid-execution (the-shadow-track) and was absorbed at the close boundary
  with one generated-file conflict (type-audit report — re-derived, not
  hand-merged). The C2-close lesson (re-fetch immediately before the
  push) is now in the walk.

## Follow-ups (promoted from the campaign register)

Book: re-export `ParseContext`/`ParseError` (one-edge cleanup for
downstream windows); `LineError` negative-path tests; multi-trailing
branch note. Vessel: `tell` rebuilds context per call (fine at session
scale); `absorb_common`'s whole-number formatting is predicate-blind
("about 1.0" → "1"); "fact(s)" cosmetic. Language: determiner-word caveat
on complement lexemes becomes real if a multi-word lexeme ever starts
with an article. Guard: keep every fragment routed through
`CONSTRUCTION_ORDER` (the corpus coverage assertion watches exactly that
array). Program: LANG-44 numeracy (the owner-banked bridge), and the
pronoun-case canary pins lowercase-"it" until the re-mention path goes
live.

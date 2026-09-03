# The Nettle — retrospective

Five bounded annoyances inherited from The Reservoir, four of whose premises
did not survive contact. Process lessons only; the product half is the
chronicle.

## Headline: fourteen claims failed checking, and re-reading caught none of them

Every one was caught by somebody **re-deriving** the claim — running the
command, measuring the thing, reading the code. Not one was caught by
re-reading the document that contained it, and re-reading happened on all of
them. Seven were written by the controller.

| # | claim | how it was wrong |
|---|---|---|
| 1 | a skill names a superseded path | fixed 3 days before the note was written |
| 2 | "nothing pins the derived bytes" | 10 committed artifacts pin exactly that |
| 3 | "the cheap shape is ~1 s" | 2.4 s warm, 234.6 s cold — that was test time, not wall |
| 4 | "3 files with fixed temp paths" | 14 sites in 8 files, 2 of them destructive |
| 5 | "`BASH_REMATCH` is already used here" | zero uses in the file or the whole directory |
| 6 | 3 counts in the spec | one overstated the evidence in the only section put to Nathan |
| 7 | 2 defects in the plan | one would have made a correct tool look broken |
| 8 | 5 registry cells "fit" | all 5 over a hard cap; 2 rewrites still failed by 7 and 1 chars |
| 9 | a grep "returned 0" | it returned 3, in output already on screen |
| 10 | a hand-rolled registry parser | 17 rows with identical 1803-char cells; headers counted as rows |
| 11 | "delete the superseded rows" | closed 3 of 5; the commit message claimed the class |
| 12 | "0 of 900 raw rows" | 1026, from a grep matching only exactly-`raw` cells |
| 13 | a cost range of 5.7–17 s | observed max 23.972 s |
| 14 | the fix for 13, at 5.7–20.2 s | written while its own max sat at 23.972 s |

Rows 9, 10 and 12 are one failure repeated: **an ad-hoc parser written beside a
working one.** In row 10 the correct check had passed green four minutes
earlier, in the very run the campaign had just wired up.

## The two-kinds distinction, and what it costs

An **observation** asserts a property of a location; a **judgment** asserts a
property of the whole state. A judgment is falsified by an edit anywhere, by
someone not looking at it. Both file identically with a confidence fixed at
writing. Rows 1 and 2 above were judgments; row 4 was an observation and was
durable.

Cost measured here: unparking five findings needed four re-derivations and
found two of them false. Parking cost one paragraph each.

Left undone deliberately: the missing columns (observed-against state, and the
one re-deciding command). Recorded as a registry row; building it is larger
than five nettles.

## What worked, and is worth keeping

- **Per-task verification one task ahead, not in a batch.** It found the
  `BASH_REMATCH` fabrication, all 14 line numbers still exact after a
  131-commit absorption, and the five over-cap cells before they reddened.
  Verifying Task N+1's brief against the tree Task N+1 will actually find is a
  different and much easier job than verifying five briefs at plan time.
- **Handing implementers the criterion and letting them derive the set.**
  Task 5's implementer traced what every test in 24 candidate files *opens*,
  rejected six whose matches were comment text, hit its stop condition, and
  reported four options without picking one. A prescribed list would have been
  wrong.
- **Stop conditions that name a number.** Two fired. One was right to fire and
  its threshold was wrong — the count was a proxy for cost, and cost was fine —
  which is a good outcome: the stop bought a decision instead of a guess.
- **Asking a reviewer to hunt for siblings of a defect.** The one speculative
  instruction in a dispatch found rows 11 and 13.

## What to do differently

- **A commit message must not claim a class is closed.** Row 11's message said
  "delete the superseded rows" over a document where two remained. State what
  was done, not what is now true.
- **Never hand a subagent verbatim text it may not alter.** A character cap
  forced this once, and it converted the controller's stale number into one the
  implementer was forbidden to correct (row 13). The mitigation that saved it:
  the dispatch told the implementer to *report* a mismatch rather than resolve
  it, and it did.
- **A diagnostic returning zero means "my pattern matched nothing", not
  "nothing exists".** Rows 9 and 11 are both this.
- **Prefer a structural claim to a counted one.** "There is no observed-at
  column" cannot decay; "0 of 900 rows" decayed within the campaign that wrote
  it.
- **Write commit messages with `git commit -F <file>`.** Two implementers hit
  shell quoting failures; one lost a backtick pair to command substitution and
  caught it only on review.

## Deferred minors, with outcomes

1. Timing rows committed outside briefs' file lists — **accepted**, the repo's
   convention (The Governor, Task 7).
2. `census_claim::a_stale_claim_is_taken_over_rather_than_waited_on` flake —
   **accepted**, pre-existing, timing-sensitive, no causal path from this diff.
3. Duplication between `campaign-autopilot`'s capture table and its ledger
   section — **accepted**, a readability note for a future pass.
4. The board-lane guard's early exit sits above the inverted fast path —
   **accepted, and less live than first recorded**: no member of the new test
   set reads anything under `tools/board` at all, so adding prose there would
   not be covered without a new test either.
5. The cost range — **fixed**, three times, ending at 5.7–24.0 s.

## Found at close, not caused here

The shared main checkout holds **four orphaned scratch files** from three
pre-Cartulary campaigns, up to 35 days old, including a 60 KB decision ledger
at the path The Cartulary superseded. Each announces that it "dies with the
worktree" and will be "promoted at close" — the practice that failed five
recorded times. Spot-checked: The Whetstone's named targets do exist, so its
content was promoted and its file is a routed leftover. None was ever tracked,
so no loss is provable. Left in place rather than deleted, since deletion is
the one irreversible option and an audit of three campaigns is not this
campaign's business. Carried as a registry row.

## Process facts worth carrying

- The commit gate ran **997 s** on the one task that staged a `.rs` file
  (1154 sub-floor tests), against ~25 s warm — the recycled-tree touch for
  `CARGO_MANIFEST_DIR` invalidated everything.
- The `PreToolUse` guard resolves its script from the project directory, so a
  repair to it **cannot take effect for the session making the repair** until
  the work merges. The old fault fired on legitimate work four more times
  during its own fix.
- Two absorptions, 131 and 62 commits, both clean; all 16 temp-path sites at
  identical line numbers after the first. The stage-boundary cadence held.

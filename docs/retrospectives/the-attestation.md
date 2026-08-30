# The Attestation — retrospective

**Merged:** 2026-08-30

## A conclusion survived and its stated reason did not, in every direction

This campaign's dominant defect shape was not a wrong fact sitting still — it
was a conclusion that stayed right (or stayed wrong) while the sentence
explaining *why* changed underneath it, in the controller's text, the
implementer's text, and the reviewer's own probes, more or less evenly.
Nothing in the list below was caught by re-reading; every one died to a
command someone actually ran.

- **The controller's silent-breakage mechanism was half right and named the
  wrong half.** Ruled before Task 3 that a two-column `docs/generated-
  paths.txt` would make `nightly-drift.sh`'s `git diff --exit-code -- $paths`
  silently exit 0 forever. A scratch-repo probe refuted it: unquoted word-
  splitting on the embedded tab adds a harmless extra pathspec, it never
  drops the real one. The **fix** (`cut -f1`) was retained on weaker grounds
  — until Task 3's review found the mechanism was exactly right one file
  over: `scripts/hooks/post-merge` reads the whole line with `IFS= read -r`,
  which does not word-split, and its drift notice really was dead for all
  ten declared paths on every merge. The controller's own "conclusion
  survives on weaker grounds" note was itself wrong — the strong ground
  existed, unfound.
- **The campaign's own payoff instrument reported a confirmed finding that
  was five false positives.** Task 6's first run called H3 (the freshness
  reader surfaces an unknown absence) CONFIRMED. The reviewer traced all
  five "owed but absent" jobs to the queue's own logs and found every one a
  documented, legitimate prose-only narrowing. The conclusion (CONFIRMED)
  was simply wrong, discovered by running `scripts/sluice-phases.sh`'s own
  logic against the real jobs rather than trusting what "owed" meant.
- **A stated guarantee outlived the thing that made it true, twice, in the
  same instrument.** Task 4's write-count ledger carries a true self-row and
  a false comment claiming it — a bash redirect stamps the target's mtime
  before the body runs, so the self-check structurally cannot come back
  false, and the file's own prose said it was "covered the same way every
  other row is," which is not true even though the row's fact is. Task 5
  separately found its own precedence rule (more-specific override wins)
  documented in two places and enforced by neither — a factually wrong,
  more-specific declaration passed every test clean until a fix round added
  one.
- **A reviewer's Minor promoted itself into the standard it was checking.**
  Task 2's new agreement test claimed, in its own doc comment, that it "does
  not care which side invents or drops a set." The reviewer found a shared-
  membership case where that clause is imprecise (harmlessly overlapping
  with a pre-existing check). The controller reclassified the Minor as
  Important and spent a fix round on prose alone, because a check whose
  stated blindness is not quite true is this campaign's own subject matter,
  shipped in its own flagship fix.

## The three spec corrections found while planning

Recorded in the plan's own preamble ("What this plan already got wrong three
times"), each found by running a command against a claim rather than trusting
it:

1. §4 proposed *adding* a check that every declared path is written by its
   author. That check already existed (`generated_paths.rs:243`) — the real
   defect was its granularity (a directory-wide substring match), not its
   absence.
2. The positive control for that check costs a full regeneration
   (128.720 s measured) and cannot live in a unit test — it belongs in the
   chamber's `artifacts` phase, which already pays for the run.
3. §3's original approach — *deriving* the chamber's two phase lists from
   the roster rather than checking them against it — would have silently
   reordered every merge: the roster is ordered by rung, the chamber by
   execution sequence, and the two are transposed. Order is load-bearing
   (`artifacts` must run before `gate`); membership is what the roster can
   actually answer.

## Two more corrections found during execution, same shape

- **The author vocabulary was about to be invented rather than read off the
  roster.** The plan's own §4 draft proposed labels like `rebaseline` and
  `rebaseline+census`. A pre-dispatch check of `scripts/lane-sets.tsv`'s
  header — read before Task 2 was dispatched, not after — found the roster
  already publishes an `authors` column naming exactly these things
  (`artifacts`, `census`, `heavy`). Inventing a second vocabulary for
  something the repository had already named would have been this
  campaign's own §1.1 defect (a rule restated with no agreement test),
  committed by the campaign fixing it.
- **A stated technical constraint that did not apply.** The plan asserted
  `scripts/regenerate-artifacts.sh` "must be dash-safe" because
  `sluice-run.sh` sources it. Checked before Task 4's dispatch: the script
  is `#!/usr/bin/env bash`, invoked as `bash scripts/regenerate-
  artifacts.sh`, and `sluice-run.sh` does not source it at all (zero hits).
  Stating a constraint that does not bind would have pushed the implementer
  into needless contortions to satisfy a rule from nowhere.

## What this means for the next campaign

The instruction that actually worked, stated explicitly mid-campaign after
the fourth or fifth instance of this shape: not "check this fact" but
**"re-derive the claim, don't re-read the sentence stating it."** Every
instance above is a case where the sentence read as internally consistent —
the campaign's own review passes, run purely by re-reading, found none of
them. What found them was a scratch-repo probe, a grep of the queue's actual
logs, a coverage-set diff, or a mutation of the claim itself. A controlling
session's own confidence in a mechanism is not evidence for the mechanism,
even — especially — when the mechanism is being cited to justify a fix that
turns out to be correct anyway, for a different reason.

## Appendix: the deferred register, reconstructed after the scratch was lost

**Provenance, stated because it changes how much this list can be trusted.**
This section was NOT written from the SDD ledger. The campaign's worktree was
recycled by `make worktree-take` between the merge landing and the close walk,
which swept `.superpowers/sdd/` with it — the ledger, eight task reports, ten
reviews, the deletion list and the final review are gone and are not
recoverable from git, because that directory is git-ignored by design. What
follows is reconstructed from the controlling session's own transcript, which
recorded each finding as it arrived. Anything that was in the ledger and never
reported upward is not here, and by construction nobody can now say what that
was.

**The close skill names this exact failure and I walked into it anyway.** Its
step 2 says to sweep the scratch *before* writing the retrospective, and warns
that The Ell promoted nine items and still lost six. I read that, wrote the
retrospective from the ledger, and then left the ledger in a recyclable
worktree — reasoning explicitly that I would keep it "until it lands."
**Landing is what makes a worktree eligible for recycling.** The guard was
inverted: the moment I judged it safe to keep was the moment it became unsafe.

### Deferred minors (nine, all triaged as shippable by the final review)

1. `docs/timings.md` rows appended by the pre-commit hook alongside task
   commits — expected side effect, not a hand edit.
2. Two date ranges in `docs/generated-path-authors.md` are off by one day at
   the start (`branches-family`, `census-of-coasts-tuning` — the latter wrong
   at both ends). Cosmetic; the commit counts and the classification are
   unaffected.
3. The H1 summary label "110 frozen one-off studies across nine directories"
   collapses **two** distinguishable cases, not one: `census-of-skies` (whose
   automated author was deleted with `.github/workflows/ci.yml` by decision
   0125) and `earth-mask-l6` (a standalone tool run). Partially corrected in
   the final fix wave; re-check if the label is reused.
4. `written` in `docs/generated-path-writes.tsv` means *mtime advanced*, not
   *content changed* — a byte-identical rewrite counts. Disclosed in the
   script and the TSV header.
5. The capture's `sleep 1` granularity guard assumes ≤1 s filesystem timestamp
   resolution; undocumented for coarser mounts. Fails toward the loud
   direction.
6. The `none(<reason>)` "no roster author" scoping — **closed** during the
   campaign; recorded here only so the trail is complete.
7. An unknown author is caught by two tests, not the one a report's prose
   implied. Prose discrepancy, not a wrong claim.
8. `decision_blocks_do_not_overlap_across_campaigns` skips same-campaign
   spec/plan pairs unconditionally, so a campaign's own spec and plan headers
   *disagreeing* goes undetected. Narrower than it sounds — both are still
   compared against every other campaign, so nothing wrong can be minted — but
   it is a silent absence inside the check built to find silent absences.
9. The stale-waiver check is inlined into one test rather than split per the
   file's existing waiver-list convention. Cosmetic.

### Parked findings (two, with the rulings that parked them)

- **A census-authored override reds with a remedy that cannot work.** Adding a
  `census`-authored overriding row fails `an_overriding_declaration_must_be_measured`
  with "run `make rebaseline`", while the write-capture now excludes census
  rows from emission — so the named remedy cannot satisfy the check. Real, but
  no current row triggers it and the defect is the *message*, not the check.
  *Cost of parking:* a future author loses ten minutes to a wrong remedy.
- **`CONDITIONALLY_DROPPABLE`'s extractor is blind to a more complex pattern.**
  It reads `sluice-phases.sh`'s literal `grep -vxE '…'`; a character class,
  anchor, capture group, second `grep` or multi-line pattern would defeat it.
  It **panics** rather than silently agreeing, which is the correct failure
  mode and was verified by probe — but the blindness is real and stated.

### Deferred by explicit ruling, with their measurements

- **A shared `scripts/lib/generated-paths.sh`.** Three parsers read that file
  and each was fixed independently this campaign. Consolidating them is the
  same single-source discipline the project already applies to the list's
  *contents*, extended to how the list is *read*. Deferred as infrastructure
  with its own review, not because it is wrong.
- **The originally-specced, ledger-reading decision-block check is still
  owed.** What shipped is a strict subset: it compares committed spec headers,
  and it cannot catch a campaign that declares no block at all (the
  `the-stride`/`the-burr` shape). If the block ledger is ever made committable,
  the full check should be built.

# Retrospective — The Burr

Process lessons only; the product story is the chronicle.

## What worked

- **An end-to-end witness on a real regenerated artifact is the only thing
  that catches a vacuous mechanism, and it recurred five times.** A pure-unit
  test can pass while the wiring is inert: `is_liquid_manner` widened to
  include `Nasal` left proto-elf's liquid count at 0 while 23/23 tests stayed
  green (FINDING-8); Task 11's harmony wiring was proven live only by
  regenerating and finding literal `"?"` glyphs, then by mutation-reverting
  `harmonize` in `build_name` and watching four elf names snap back exactly;
  Task 13's dwarf words were checked by reading the actual regenerated
  dictionary (`Napad`, `Jagap`, `Vadad`) rather than trusting `skeleton_shaped`.
  Make an end-to-end outcome check on a real regenerated artifact a DONE
  condition for any mechanism, not an afterthought to a green suite.
- **Mutation-proving a fix is what turned two real bugs into caught bugs, not
  shipped ones.** Task 12's review found `assign_skeleton`'s collision-probe
  loop was dead code — `Stream::pick` never returns `None` on a nonempty
  slice, so `probe` never advanced — and its injectivity held only by seed-42
  luck (FINDING-11, the same birthday-problem shape as the volcano-naming
  defect FINDING-2). Fixing it exposed FINDING-12: a 2-consonant phonology
  yields only 8 skeleton slots, and the used-set retry loop for dozens of
  concepts spun forever — a live infinite loop that hung a census study
  ~3 hours before it was diagnosed (not waited out) and fixed with a
  probe-lengthening growth rule, verified by mutation to fail loudly at the
  probe cap rather than hang.
- **"Investigate before re-pinning" resolved two test failures as defects in
  the test itself, not in the code.** The volcano distinctness test
  (FINDING-2) asserted zero name collisions across 187 draws with no
  collision-avoidance mechanism in the code — a birthday-problem outcome
  documented as an invariant, and decision 0024 already forecloses fixing it
  in-name. Reweighting it to a collision *budget*, mutation-verified to still
  catch a catastrophic collapse, is the shape to reuse: **a distinctness
  assertion over a namespace the design allows to collide is a measurement,
  not an invariant.**

## What to change

- **Every counting error this campaign was a unit error, never an arithmetic
  one — six of them.** Sections read as tongues (one `## ` heading was
  `## Cognates`, reaching the spec, plan, a registry row, a board post and
  three commit messages before Task 2 caught it); a header row matched the
  liquid regex and inflated every tongue's count; a combinatorics comment
  omitted one enum field (135 vs. the real 288); occurrences were quoted as
  call sites (41 tokens on 39 lines, reported as 40); and the "draw_phonology
  has 7 call sites" cost justification (RULING-14) came from a grep piped
  through `| head`, whose truncation was read as the answer — inverting the
  very argument it was meant to support. The habit that would have caught all
  six: **write "one row per WHAT?" before running any counting command.**
- **Tests outside the sub-floor tier drift silently and invisibly during a
  name-moving campaign.** `burr_calibration`'s pin went stale at Task 10 and
  stayed red, undetected, through Task 11 because gate-commit compiles the
  crate but never runs a test with no recorded baseline duration
  (FINDING-10). Three `REBASELINE=1` byte-goldens drifted from Stage 3
  through Stage 4 unnoticed by gate-commit, the generated-paths drift check,
  or plain `make rebaseline` — only `make rebaseline-goldens`, run at a stage
  gate, caught them, and its fail-fast hid a third (FINDING-13). Four
  `hornvale-book` autonym tests went stale the same way (FINDING-15). A
  name/epoch-moving campaign should run `make rebaseline-goldens` and the
  full book suite proactively at the first name change, not discover the gap
  at a stage boundary.
- **An inherited diagnosis is a hypothesis, not a fact.** A subagent called
  four `hornvale-book` failures "pre-existing, out-of-scope"; the claim was
  never checked against main, only against a campaign commit, and it was
  false — all four were campaign-caused re-pins accumulated since Stage 3
  (FINDING-15). Check any "pre-existing" claim about a red test against main
  before accepting it.
- **Report which half was measured, and say plainly when the sufficient half
  is untested.** The trigram classifier measures *distinguishability*
  (0.7202 → 0.7995 over the campaign); the spec's actual bet was
  *aesthetic* — whether the tongues sound lovely, not merely separable. A
  handsome accuracy rise is not the campaign succeeding; FINDING-1 caught
  this early ("a machine can tell them apart at 72%; a reader cannot tell
  them apart at all") and it stayed the frame for every later readout,
  including two negative/null stages (Stage 2's uniform floor pushed accuracy
  DOWN 0.041 with zero lexical effect) that were reported as the honest
  result rather than retuned away.
- **Operational subagent hazards, worth naming for next time:** two API
  deaths and two turn-parks on long background jobs (`dispatching-hornvale-
  subagents`'s un-parking procedure worked both times, but each cost a
  diagnosis pass rather than a blind resume); `git checkout --theirs <file>`
  during a merge conflict resolution takes the WHOLE file and silently
  discards git's own auto-merge — it reverted a call-site fix, caught only by
  `cargo check`, not by the merge itself; and `git add -A` on top of a
  subagent's crashed, unstaged commit swept 77 lines of the campaign's
  central regression guard into an unrelated docs commit (RULING-16) — use
  `git commit -- <paths>` whenever a subagent may have left uncommitted work,
  which under subagent-driven development is every commit the controller
  makes.
- **Stage-boundary absorption cadence was mostly deferred, not kept.** Two
  absorptions landed mid-campaign cleanly, but final integration absorbed
  roughly 185 commits at once across two more merges at the close, each
  requiring bounded-but-real reconciliation (regenerated goldens, re-pinned
  word lists, a `--theirs` mishap). The cadence the campaign process names
  — absorb at every plan-stage boundary — would have kept each absorption
  small instead of stacking three of them into the campaign's final stretch.

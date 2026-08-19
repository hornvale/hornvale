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
- **A census refresh reddens every census-*reading* calibration test, and the
  merge queue's fail-fast reports one, not the set.** The Burr's refresh
  (`ROOT_EPOCH v4`) moved ten pinned measurements — `evaluable_columns` plus
  nine name/homophony/transparency/syllable calibration tests and their
  `golden-pins.sql` mirror — and none were re-pinned in the refresh commit
  itself. The chamber held on the first (`evaluable_columns`) only because
  `census_duration` failed even earlier and nextest cancelled the rest; a local
  `--no-fail-fast` run surfaced the other nine at once. Re-pin census-reading
  tests in the refresh commit, and run the full lab suite `--no-fail-fast`
  after any refresh — the first red is never the whole set.
- **Parallel campaigns collide on decision numbers; reserve a block, don't bump
  a single number.** The Gazetteer minted `0147` while The Burr already held
  it. `make decision-block NAME=<campaign>` reserves a disjoint range from the
  canonical authority (The Burr took `0156-0165`); renumber into that block
  rather than to `main-max + 1`, which every concurrent campaign also computes.
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

## The landing took six attempts, and the sequence is the lesson

The Burr reached `main` on its sixth submission (`a4bb53066`). No two holds
were the same defect, and only one was avoidable at the time:

1. `census_sentinel` + world-identity — the phonology epoch arriving, A/B'd green.
2. Mouth bounce, 25 conflicts — a rebaseline taken against a main that had moved.
3. `census_duration` 918.457 s > 900 — see below; it became policy.
4. `evaluable_columns` 114 vs 115 — a census-refresh re-pin, masked behind #3 by
   nextest's fail-fast. The one that was mine to have caught.
5. A four-file conflict — three merges landed underneath the branch.
6. `draw_phonology` arity — a semantic collision (below).

**The ledger discipline is load-bearing, not bookkeeping — #3 is the proof.**
The census budget breach was invisible while its timing row sat uncommitted in
a worktree the next census would have wiped. Committing the row turned a real
breach into a red, the red into an argument, and the argument into decision
0148's two-tier budget with a per-run profiling ledger. An unledgered expensive
run cannot become policy; a ledgered one did.

**"No gate catches a semantic collision" is repeated more often than it is
qualified, and the qualification is where the risk lives.** Hold #6 — The
Gazetteer's new `draw_phonology` call site against the fourth argument the Burr
epoch had grown — was semantically incompatible *and loudly broken*, so the
chamber's gate phase would have caught it regardless. That is the version with
a floor under it. The genuinely invisible collision is when both sides compile,
agree textually, and mean different things; nothing here exercised that case.
Verifying the fix by running The Gazetteer's own naming tests (8/8), not by "it
compiles", is what made the resolution behaviourally sound rather than
type-correct.

**`census_sentinel` did not run on the merge, and this close says so rather than
letting the absence read as a pass.** Decision 0148 took the heavy tier off
merges, so the four-phase merge that landed The Burr never ran the live census
probe. What *was* established, against the SHA that actually landed
(`a4bb53066`): the census was authored live on lefford at `635d116d`;
`lens_purity` — the world-identity guard, which runs in the gate phase, not
heavy — passed on the merge, so the seed-42 world is byte-identical to a fresh
build; The Gazetteer, whose terrain work is the only intervening world-gen
change, landed six-phase with `census_sentinel` green on the full population;
and `world-seed-42.json` is byte-identical across the whole span of landings.
The census stands on convergent evidence, minus only the one instrument that
would have re-confirmed it directly.

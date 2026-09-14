# Decision ledger — the-attribution (2026-09-14)

Operator tooling, not a campaign: no spec, no plan, no G3/G6. Recorded here
because five defects were found in one evening's queue operation and the
reasoning behind each fix is worth more than the diffs.

All five share a shape. **Every one was a check that answered the wrong
question confidently**, and in four of the five a green test sat on top of it.

## Rulings

### #1 [Q] — may a candidate mint a decision number inside another campaign's reserved block?

**Decision:** yes, when the reserving campaign is CLOSED. The allocator has no
release operation and issues blocks at max+1, so a closed campaign's unused
tail is dead space nobody else will ever be handed. The number is permanently
free, not merely unlikely to be wanted.

**Why it came up:** I held campaign/the-tidemark over decision 0959, inside
campaign/the-planetarium's reserved 0956–0965, writing "the-planetarium has
been landing all week, so this is a live reservation". I did not check.
The-planetarium was merged and closed, chronicle and retrospective both on
main, having minted 0956–0958 and nothing else.

**Precedent consulted:** `cli/tests/suite/docs_consistency.rs`'s
`decision_blocks_do_not_overlap_across_campaigns`, whose own doc comment
records that it compares DECLARED `Decision block:` spec headers and is blind
to a record minted with no declaration. Neither campaign declares one, so no
gate had an opinion either way.

**Alternatives discarded:** requiring a renumber anyway, for tidiness — costs
the campaign a grep across `docs/decisions/`, `docs/digest/`, chronicle and
ledger to buy nothing, since no collision can occur.

**Capture:** the rule is now `campaign_status()` in `scripts/sluice-vet.sh`,
three-valued. Ideonomy: one pass, no overturn; the pass produced the third
value (UNKNOWN), which the two-valued draft did not have.

### #2 [Q] — what makes the third verdict value necessary?

**Decision:** UNKNOWN, for a campaign with no pushed branch and no close
package, licensing nothing.

**Why:** a campaign running in a worktree that has never pushed is
indistinguishable from one that never existed. A two-valued CLOSED/LIVE rule
reports that absence as CLOSED, which is an absence of evidence read as
safety — the same error I made by hand in #1, mechanised and thereby made
permanent. The report says "Do NOT read this as safe" in those words.

### #3 — census freshness is an ORDER question, not a TOUCH question

The vet's SURFACES lines report what a candidate touches. Every world campaign
touches world-producing code, so those lines fire on all of them and
distinguish none. What decides it is whether those sources moved AFTER the
census that ships with them.

Measured over the three candidates in flight: the-tidemark 5 world-producing
files after its census, anchor 0, the-coherence 0. The one that censused first
is the one whose goldens disagreed with anchor's on 101 columns.

### #4 — goldens moved, pins untouched

Added after campaign/anchor-orbital-coherence went red at 1033 s of gate time
with nine calibration drifts. Its diff moved 17 census golden files and touched
`calibration.rs` zero times.

**The trap this rule had to avoid:** `scripts/hooks/pre-commit`'s
`census_guard_files` deliberately includes
`book/src/laboratory/generated/[^/]+/rows.csv` — a golden, not a pin — because
the hook must fire when either side moves. Reusing that pattern here scores
anchor as "2 pin files touched" and reports the exact red it exists to catch as
clean. That mutant is pinned.

### #5 — the overlap advisory attributed main's history to the candidate

`git merge-tree` between two candidates uses THEIR merge base. For a stale row
that base is nowhere near main, so main's own commits count as the candidate's
side. tooling/the-adjudicator — four files under `scripts/` — was reported as
overlapping campaign/the-trencher on 20 paths it does not touch, across a base
123 commits behind main. The same run MISSED the one real overlap, because
merge-tree reports conflicts and the two candidates' shared edit to
`scripts/lane-outboard.sh` merged cleanly.

Scales with time: held rows accumulate (six that night, oldest 2026-09-05) and
the longer one sits the more of main it falsely claims, on every vet.

### #6 — a branch name is not a campaign slug

campaign/the-coherence's campaign is `the-coherent-ground`. The vet reported
`<NO ROW for the-coherence>` while the row sat there, correct. The substring
fallback could not reach it — the names diverge at the eleventh character.
The authority is the chronicle and retrospective a merge candidate ADDS.

### #7 — GATE MACHINERY could not see phase drivers

The roster is read from MAIN; the SCRIPT a roster row names runs from the merge
product. tooling/the-adjudicator added two suites to `lane-outboard.sh` and its
own merge ran them, while the section reported "none — no gate machinery in
this diff". A confident absence, about the one class of change that is both
self-affecting and easy to get wrong.

## The lesson under all of them

**A fixture where the two candidate implementations cannot disagree is not
coverage.** It appeared three times in one evening:

| check | why the fixture could not discriminate |
| --- | --- |
| census freshness | every fixture censused as its FIRST commit, so "since the census" and "on this branch" were the same set; a mutant anchored at the merge base passed the whole suite |
| overlap advisory | the existing fixture branches both sides from main's tip, where the right and wrong computations agree — it passed for months |
| clean-merge case | my own first draft prepended and appended to a ONE-LINE file, which git cannot merge cleanly, so it passed against the very implementation it was written to fail |

The remedy in each case was the same and is cheap: build the fixture from the
real candidate that exposed the bug, rather than from the simplest shape that
exercises the code path.

## Follow-ups

- **Phase drivers are matched by NAME (`lane-*.sh`, `gate-*.sh`), not by
  reachability.** `scripts/visual-proof-container.sh` is a phase driver reached
  through `visual-check-run` and is not matched. It was not missed on
  campaign/the-coherence only because that candidate also changed the Makefile
  recipe, which the existing expansion catches. Deriving the set from
  roster-reachable Makefile recipes would be exact; the name pattern is a
  deliberately narrow stand-in, and widening it to all of `scripts/` is pinned
  as a failure.
- **`visual_proof_build_provenance` is invoked twice per call site** in
  campaign/the-coherence's runner, once per `sed -n`, so each site pays two
  `git status --porcelain` passes and reads revision and cleanliness from two
  separate observations. Reported to them as a follow-up, not a hold.
- **Stale held rows never expire.** Six in the LIVE section, the oldest from
  2026-09-05 and self-declared unmergeable. Now that overlaps are attributed
  correctly they are merely clutter rather than noise, but the question of
  whether a held row should age out is open and is a policy call, not a
  tooling one.

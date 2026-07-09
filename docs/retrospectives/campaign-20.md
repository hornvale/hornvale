# Campaign 20 (Firm Ground II) — retrospective

**Merged:** 2026-07-09

**Recurring findings.** The campaign's central miss was a blast-radius
enumeration that claimed more completeness than it had — and that is now the
*second* campaign in a row to be caught by exactly this. Campaign 19's plan
enumerated a defect's ripple and missed a committed study (`census-of-skies`)
that reported the changed quantity; Campaign 20's plan asserted "no census is
locked" and "only the `census-lands-drift` SVGs drift," and both were false.
A blast-radius list is a claim about completeness, and completeness claims are
the class a fresh reviewer — or, here, the gate itself — catches better than
the author who wrote the list. Two campaigns running is enough to promote it:
an enumerated blast radius should get a dedicated second look *before*
implementation, the way "read the engine before finalizing the plan" already
does. That older rule, meanwhile, earned its keep again — reading
`sky_exit_criterion.rs` at plan-writing time is what revealed that the Y2 exit
criterion (the sun heads every pantheon; moons seat deities) would break under
the spec's literal momentary culling, and turned the design toward
ever-visible culling before a line was written.

**Estimate deltas.** No stage-level estimates were made, so there is nothing
to compare against — but the one cost no plan could have estimated is worth
recording: `main` advanced under the worktree three times during execution,
once with a full parallel campaign (the star chart, which shipped SKY-20's
synodic moon-phase fix and claimed chronicle 19). Reconciling that mid-flight
— a five-file conflict resolution composing synodic phase with the genesis
phase offset, plus the renumber to Campaign 20 — was real work that lived in
no plan. The `git merge-base` check the metaplan mandates before the artifact
re-baseline is what surfaced it in time; without that check the re-baseline
would have regenerated against stale tooling.

**Spec vs. reality.** Three plan assumptions the execution had to correct:

- **"No census is locked."** `census-lands-drift` is fully unpinned, so ~5% of
  its 500 seeds draw tidally-locked worlds. Placing the observer therefore
  hemisphere-culls those worlds' religion, which honestly degraded the Y2
  blind-attribution metric on the divergence census (0.868 → 0.826; the
  preregistered floor moved 0.875 → 0.8). This is the *correct* degradation —
  a locked world's night side is uninhabitable, so every settlement lands
  day-side and sees a sun-only sky, and both species become genuinely
  indistinguishable by sky. The Y2 **null control** (spinning, unaffected)
  stayed byte-intact. The honest lower rate was pinned rather than the design
  quietly reverted, and the owner accepted it explicitly.
- **"Re-baseline once, in the Close."** True for the committed book artifacts
  (checked only by CI's `git diff` step) — false for the golden-pinned
  *tests*. The census calibrations in `windows/lab/tests/calibration.rs` and
  the `tongues_identity` fixture run in the per-commit `cargo test --workspace`
  gate, so a world-changing commit must re-pin (or re-exclude) them *in that
  commit*; deferring broke the gate mid-Plan-1. Only the book artifacts could
  wait for the Close.
- **The spec's momentary day/night culling.** Taken literally, SEQ-5 would cull
  the sun on a spinning world at night and the moons by day — which breaks the
  Y2 pantheon result, because that result depends on religion seeing the whole
  sky at once. A prose spec cannot see that a downstream test rests on the
  provider's unconditional sun; reading the code did.

**Do differently next time.** When a plan enumerates a re-baseline, split the
scope by *which gate enforces it*: the per-commit test gate (golden pins,
identity fixtures — must track reality in the drifting commit) versus the CI
artifact-diff (committed `book/` files — deferrable to the Close). Writing
"defer the re-baseline to the Close" without that split is what broke the
gate. And treat every "there is no X in the census" as the blast-radius
completeness claim it is: an unpinned census draws the entire pin matrix, so
verify against the study's actual pins, not intuition. The one thing that went
right and should be repeated: the two parallel campaigns re-pinned *disjoint*
metrics (the star chart moved period/month counts, this campaign moved
name/attribution counts), so the merge composed cleanly — but that was partly
luck, and a shared metric would have demanded a combined re-measurement no
single branch had. The general rule: two campaigns editing the same
golden-pinned file will each measure a world the other hasn't; only their
disjointness makes the union correct, and that disjointness must be *checked*,
not assumed.

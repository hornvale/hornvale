# Lab Performance — retrospective

**Completed:** 2026-07-14 (name-only designation per decision
`slugs-not-numbers`; plan
`docs/superpowers/plans/2026-07-12-lab-performance.md`)

**A campaign that merges its code but skips its close doesn't read as done —
it reads as still queued.** Stages 1–2 merged to main on 2026-07-13 in a
merge commit literally titled `lab-performance`, and yet a day later the
project's working memory still listed the campaign as waiting to start:
no chronicle, no retrospective, no registry flip, no shipped marker on the
spec or plan, zero of eighty-four checkboxes ticked, the worktree idling.
Establishing what had actually shipped took git archaeology (`git log -S`
on symbol names), because the plan — the document whose whole job is to be
the evidence — said nothing had happened. Two mechanical rules fall out:
tick the plan's checkboxes in the same commits that complete them, and
treat the DoD walk as part of the merge itself, not a later favor. The
closing-a-campaign skill exists because diligence is lowest at the moment
of merge; this campaign demonstrated the failure mode the skill was
written against, a step earlier than expected — the merge happened without
the walk even starting.

**An open campaign accrues descope debt while main moves.** In the day the
campaign sat half-closed, two decisions landed that retired half its
remaining plan: manual-only CI (0042) removed the per-push drift regen
Stage 3 was built to tier, and remote-only censuses (0046) removed the
local census Stage 4's batteries were designed to ride. Neither decision
was wrong to land — they were better answers to the same costs — but the
plan silently became a description of a world that no longer existed, and
nobody was standing in the campaign to notice. The verdicts are now
recorded per-stage in the plan, pointing at the mooting decisions, with
the designs retained for the record. The general rule: when a campaign
pauses, its plan keeps aging; the longer the gap, the more of the close
becomes adjudication rather than execution.

**Pre-declaring tasks as conditional on a recorded readout worked
exactly as designed.** The plan made Stage 2's two riskiest optimizations
(the `strongest()` triple-read collapse, the astronomy ladder-split)
explicitly conditional on Stage 1's profiler numbers, with a named
fail-fast gate task whose output was written into the plan. Both died by
measurement — three percent of a build for restructuring frozen crust
code; optimizing a stage that costs 0.0% — and died cleanly, with the
numbers recorded beside the dropped tasks. This is the cheapest good
decision the campaign made: structure plans so the measurement track can
kill later tasks *in writing*, and dropping them costs one paragraph
instead of one argument.

**Tail-of-plan free-rider tasks are exactly what a premature merge
orphans.** Task 10 — depth-scoping the pin-enumeration battery, a
one-line change explicitly gated on Stage 2's `build_world_to` — was left
unexecuted when the merge went out, and sat for a day while the heavy
tier ran four times slower than it needed to (~64s → ~16s once applied at
close). A task that is deliberately parked at the end of a plan because
it's cheap-once-something-lands is invisible at merge time unless the
close walk happens; it carries no failing test, no red gate, nothing that
protests its absence. The checkbox discipline above is the only thing
that catches it.

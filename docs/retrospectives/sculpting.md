# Sculpting — retrospective

**Completed:** 2026-07-16 (plan `docs/superpowers/plans/2026-07-14-sculpting.md`,
15 tasks: decorations (1-6) → carve wiring and marine composition (7-10) →
observations, lenses, and census metrics (11-12) → batteries and the
Nathan hard stop on performance (13) → the tuning season (14) → this close
(15))

**Falsification-driven tuning struck twice in one campaign, the same pattern
Crust's retrospective named.** Iteration 1's land-only incision slope was the
spec-faithful fix for a real carry-forward (coastal cells saturating at the
incision cap regardless of drainage) and it was logged as a prediction
*before* the probe ran: shoreline-development and shelf-fraction would move
toward their bands. Both moved the wrong way, and a Nathan-ruled gate broke
outright. The diagnosis this forced — the old slope term had been an
accidental wave-erosion proxy, not a bug with no function — is exactly the
kind of thing a passing tuning iteration never surfaces. The season's
fourth iteration repeated the shape at smaller scale: the relief-frequency
fix (iteration 3) was correct on its own aliasing-bug merits and honestly
reported as moving the target metric a bare +0.02 against a +0.5 threshold,
rather than either being suppressed for looking bad or over-claimed as a
win. Two campaigns running the identical discipline is a pattern, not a
coincidence.

**The measure-first review culture caught three Critical bugs by
measurement, not inspection, each with an exact number attached.** Task 8's
review measured a 385.082 m rim overshoot on a specific seed/sink before
proposing the flat-carve-out fix; Task 9's review measured an atoll
stacking +222.322 m *above* sea level at a specific cell before the
elevation-after-wedge fold landed; the sea-trim's review measured 233
emergent wedge-filled cells across a 120-world sweep (one world alone
carrying 40 cells sitting under 49-344 m of retained sediment yet exactly at
sea level) before the marine-membership gate was corrected. Every fix
shipped with a red-then-green assertion reproducing the reviewer's own
counterexample, not a description of the bug in prose. A review culture that
insists on a number before it insists on a fix is measurably cheaper than
one that argues from a diff.

**The estimator-anchor archaeology is the campaign's best example of
distinguishing "the world is wrong" from "the yardstick is wrong."**
Shoreline-development's floor sat unreached after every banked coastal
mechanism (wave-cut, wedge, deltas, atolls, barriers) had been built and
fired for real, measured gains. Rather than treat that as a dead end, the
shoreline diagnostic did the single-craton-hypsometry campaign's own trick
one epoch later: it proved the formula was nowhere near saturated (two
independent probes cleared the band by 2.9x-3.5x), then traced the band's
own anchor — via `git log -p --follow` across the estimator's entire
history — back to a generator Crust had deliberately retired for injecting
fragment-swarm noise the current generator no longer produces. The
diagnosis took one read-only instrument and no new tuning code; it turned
an open miss into a bounded, evidence-carrying handoff (to the rift-and-fit
fake-history coda) instead of either a forced number or an unexplained
shrug.

**A resumed dispatch mid-tuning-season exercised the worktree-ledger
discipline for real, not just as a precaution.** A subagent dispatch during
the tuning season ran into its model turn's output/context limit mid-task
and had to be picked back up in a fresh dispatch. Recovery was cheap
specifically because this campaign's state lives on disk continuously —
`decision-ledger.md`, `progress.md`, and each task's brief/report — rather
than only in the conversation that was cut off; the resumed agent re-read
the ledger's last entry and continued from there rather than needing a
from-scratch re-briefing. This is the same lesson an earlier campaign
banked as "SDD scratch lives in the worktree," and Sculpting is the campaign
that needed it to actually fire.

**This was campaign-autopilot's first full campaign, gate-tagged decisions
end to end.** Every decision-ledger entry carries its gate (`[G3]`/`[G4]`/
`[G5]`/`[Q]`), and the two hard stops the spec pre-declared (Task 13's
performance readout, Task 14's escalation criteria and band-unreachable
trades) both landed as genuine stops with Nathan rulings recorded inline
(ledger #5, #9-#11) rather than as narrated intentions. The autopilot
framing held for a fifteen-task, four-tuning-iteration campaign without
needing an exception.

## Estimate vs reality

The plan's task order (decorations → carve terms → wiring → observations →
lenses/metrics → tuning → close) tracked the spec's own pipeline sequencing
exactly, and Tasks 1-12 landed close to estimate with no scope growth beyond
the two adjudicated Criticals above. The surprise, as with Crust, was
entirely in the endgame: Task 13's hard stop expanded from a performance
readout into four bundled Nathan rulings (perf acceptance, the reroute
watch band, the sea-trim's bounded re-solve, and the shelf-width
tail-dominance criterion), and Task 14 grew from "tune two bands" into a
four-iteration season with two preregistered stop conditions, a dedicated
diagnostic instrument, and a genuinely open verdict requiring a fresh
Nathan ruling rather than a tuning result. Budget the tuning season and its
hard stops as their own multi-checkpoint efforts in the next epoch's plan,
not as a single terminal task — the same lesson Crust's retrospective
recorded, restated because it recurred exactly.

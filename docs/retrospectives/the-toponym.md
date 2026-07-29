# The Toponym — retrospective

Process lessons, not product.

## A build error found the right architecture

The variant vocabulary could not stay in `windows/locale`, because settlement
naming lives in `windows/worldgen` and `locale` already depends on it — the
edge would have been a cycle. The fix (move the vocabulary into
`domains/climate`) was not a workaround; it was where a *facet* belonged, next
to realm, formation and stratum.

**Lesson:** a layering violation surfacing late is often a design signal rather
than an obstacle. The question to ask is not "how do I get the data across this
edge" but "which side should have owned it".

Worth noting the spec did not anticipate this. The Stratum specced O1 without
checking which crate would need to read the vocabulary — a cheap check that
would have moved the discovery from execution to design.

## The move and the change were separated, and it paid

The vocabulary move was landed alone, under the requirement that regenerating
every artifact produce **zero drift**. It did. Only after that did the epoch
land.

This is the second campaign running to use that discipline (The Shoal did it
for the descriptor re-key), and it keeps earning: when the epoch's artifacts
later moved in a dozen places, none of it could be the move.

## Five independent witnesses, two real bugs

A name-gloss's truthfulness is re-derived in five places that never call each
other. Adding a concept to a gloss meant teaching all five, separately, and two
of them failed with genuine correctness errors rather than drift: settlements
were being named for a concept the verifier could not re-derive, so the names
were *unverifiable*, not merely different.

**Lesson:** deliberate triangulation is expensive to change and that is the
feature. A single-witness design would have accepted the epoch silently. When a
project pays that cost, a campaign that touches the triangulated thing should
budget for updating every witness — and should treat "the second witness
disagrees" as information, not as an obstacle.

## Regenerate the census AFTER the metric fix, not before

The census was run, then the metrics it computes were corrected, then it had to
be run again — about twenty-five wasted minutes. A census measures the world
*through* the metric code; changing that code invalidates the run.

**Lesson:** when a campaign changes both the world and the metrics that measure
it, the order is: fix the metrics, then regenerate. Obvious afterwards. The
tell was available earlier — the calibration failures named metric-level
concepts (`exposure-sound`, `name-gloss-true`), not value drift.

## Distinguish the hypothesis from its witnesses

Several calibration failures looked alike and were not. The preregistered
*hypotheses* (bugbear ≥ goblin ≥ hobgoblin; bugbear's homophony highest by 3×)
were re-verified and held. The *witnesses* — pinned exact means and counts —
moved, because every name in every world was redrawn.

Re-pinning the witnesses is correct and routine; the files carry a comment
history of prior epochs doing exactly that. Re-pinning a hypothesis would not
be. The discipline is to measure the claim before touching the number.

## Follow-ups

- **Nine of twelve kingdom×energy combinations** remain unused in the exotic
  candidate table (from The Tare).
- **Swimming** — lateral movement while submerged is refused; traversal needs
  the destination cell's floor depth (from The Column).
- **A sourced stance** — the room description no longer asserts a posture; a
  real one wants the liveness layer to expose an activity (from The Effacement).
- **Names are still hard to pronounce.** `Vngoashshngaoshshngoogootao` is the
  seed-42 flagship. This campaign made names *mean* more; it did nothing for
  how they sound, and the phonotactic work remains its own epoch-bearing
  campaign.

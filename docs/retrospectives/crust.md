# Crust — retrospective

**Completed:** 2026-07-11 (name-only designation per decision 0026; plan
`docs/superpowers/plans/2026-07-10-crust.md`)

**Preregistered bands earned their keep by being wrong on purpose, twice.**
The tuning loop held the generator to six Earth-anchored acceptance bands set
before any generator code changed, and its value was not that it passed but
that it *falsified*. Iteration 2 refuted the pinch-off hypothesis (smoother
rims, the island swarm survived) and the repulsion hypothesis (separated
cratons still merged through their skirts) by direct measurement — and a
read-only provenance probe then found the real cause (sea level sitting in the
abyssal plain) that no amount of knob-tuning would have surfaced. The durable
pattern: log the prediction *before* the census, treat a refuted prediction as
data, and reach for a diagnostic probe before a fourth tuning iteration. The
winning fix (couple the craton budget to the ocean fraction) was structural,
not a tune — three independent knobs had been fighting each other, and the
loop's honest record is what exposed that.

**The mega-agent runaway was the session's most expensive mistake, and its
lesson is granularity.** The merge was first cold-dispatched to a single
subagent bundling seven heavy phases (conflict resolution, refreeze,
calibration re-pins, census, gate) with no intermediate commits. It ran two
hours and twenty minutes, produced an opaque uncommitted tree, and had to be
discarded — determinism made that safe, but the time was gone. Redone
controller-side in fine-grained checkpointed steps (M1–M6, each with a ledger
entry and a visible go/no-go), the same merge went through cleanly and caught
things the cold agent would have fumbled: the registry count was 110 not the
116 a first guess suggested (the shape metrics were already on main from the
Measured Coast), and the calibration conflicts were a genuine *semantic
collision* — our L6 terrain composed with main's founder-floor placement and
the new tides/eclipses to give goldens that were neither side's, re-pinned from
the actual merged census. Judgment-heavy integration belongs in the controller,
which holds the context; a fresh agent re-derives it at the cost of the very
runaway that motivated the finer grain.

**Subagents parked on background jobs three more times despite an explicit
foreground-only preamble** — and the controller did it once too, blocking a
`grep | head` pipe on a twenty-minute build with zero visibility. The
prohibition alone does not hold; the recovery recipe must ship with it (poll
the output file in a bounded loop, never end the turn to wait) and the
controller-side tell — a reply that narrates intent instead of a verdict — must
trigger an immediate un-park. Long jobs go to a background log the controller
can tail, never a blocking foreground pipe.

**Two git hazards worth pre-empting on any future merge.** One of main's
golden-harness tests runs `git reset` during the suite, which can clobber an
in-progress merge's state — so commit the merge *before* running the full gate,
and verify re-pins with targeted `cargo test -p <crate> <name>` while the merge
is open. And in a worktree, `.git` is a file, not a directory, so
`ls .git/MERGE_HEAD` and `echo > .git/MERGE_HEAD` both silently mislead — a
false alarm that nearly sent this session chasing a lost merge state that was
never lost. Trust `git show --format='%p'` (parent count), not `.git/` file
probes.

**Every friction point was a lab-performance argument, which is why that
campaign was resequenced ahead of Sculpting mid-flight.** A twenty-minute
contended build, a 1.5-hour 1000-seed *full-world* census that measures only
language, an un-optimised per-sample craton-seed derivation costing ~600M
allocations per census — each maps directly to a backlog item (content-keyed
regeneration, domain-scoped builds, the byte-identical perf commit that landed
here). The perf commit is the model for the rest: provably byte-identical
(same derivations, hoisted), so `lens_purity` proved it changed no world and it
needed no refreeze. Sculpting is another epoch with another tuning season; it
inherits every saving.

**The pragmatic baseline call.** The permanent Census of Coasts II baseline was
recorded at two thousand seeds rather than the planned ten thousand — a probe
confirmed the medians match, every band verdict's margin dwarfs the sampling
noise, and a multi-hour 10k run on un-optimised infrastructure is poor value
when the lab-performance campaign will make it cheap. Fidelity choices are the
human's; this one was surfaced with its full cost and reasoning and decided
openly, and it is documented in the census page itself, not buried.

## Estimate vs reality

The plan's ten tasks were sound; the surprise was entirely in the endgame.
Tasks 1–8 ran close to plan. Task 9 (band verdicts) expanded from one census
into a four-iteration tuning loop with a human decision at each gate — the plan
anticipated this shape but not its length. The merge (planned as a single
finishing-flow step) became the session's largest single effort. The lesson
for the next epoch's plan: budget the tuning loop and the main-merge as
first-class multi-step efforts with their own checkpoints, not as terminal
one-liners.

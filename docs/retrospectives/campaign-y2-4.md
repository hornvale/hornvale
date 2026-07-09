# Campaign Y2-4 (The Meeting) — retrospective

**Merged:** 2026-07-09

**Spec vs. reality — the null control was degenerate as specified, caught at
plan time.** The approved spec built the null control as two identical-vector
species (`goblin` and a `goblin-twin` clone) placed side by side in one world.
Writing the null-control tasks meant reading the placement engine first, and
that reading killed the design: `place_tagged`
(`domains/settlement/src/placement.rs`) scores every cell purely from the
psychology vector with no stream noise and breaks exact ties by species tag
order, so two identical-vector species score identically at every cell and the
first-tagged one greedily takes the entire spaced scatter — the second places
nothing at all, and every twin metric would read `Absent`. The two-slot roster
could not work. The fix — surfaced to the owner and corrected in the spec in
place, not left to drift — was **solo rosters**: place each goblin-vectored
species alone, over the same seeds, so both land in the identical cells and
diverge only through name-salted noise. This turned out strictly better: the
two solo builds share seed, cell, and phenomena, so their signatures are
*positively* correlated, which makes the independence sampling bound a
*conservative* upper bound (the two-slot design would have been
*anti*-correlated through competition and demanded an inflation factor the spec
couldn't honestly derive). The lesson is the Y2-3 re-draw/pin-isolation catch
recurring: the tricky mechanism in a spec is worth verifying against the actual
engine code *before* the plan is finalized, because a spec review reasoning from
prose cannot see a tie-break rule that erases a whole population.

**Shared-repo concurrency stopped a task and forced two rebases.** The worktree
was created from `origin/main` (`0b9c79c`) despite a `worktree.baseRef head`
config — the native worktree tool took the origin default — and `main` then
advanced twice mid-campaign under other sessions: first from `dac1680` to
`518b65a` (landing an entire unrelated campaign, audible phonology, and
rebasing the parallel Lab runner under new SHAs), then to `02418f6`. Task 5
correctly hit **BLOCKED**: the parallel `run_pin_set`/`build_row` it was
specified to extend simply did not exist on the stale base. The implementer's
BLOCKED-with-diagnosis (it ran `git merge-base` and reported the exact missing
lineage rather than inventing a sequential-runner variant) was the right call,
and recovery was two clean `git rebase main` runs — zero conflicts both times,
because Tasks 1–4's files (`species/lib.rs`, `worldgen/lib.rs`, `lab/metrics.rs`,
`lab/roster.rs`) were disjoint from what the advancing `main` touched
(`runner.rs`, `calibration.rs`, `idea-registry.md`). The lesson for a
multi-hour campaign against a live `main`: verify the worktree's real base with
`git merge-base HEAD main` against what the plan assumes *before* dispatching
tasks that lean on recent infrastructure, and re-check the base before the merge
— the base moved three times in one session.

**A subagent wrote into the main checkout and self-recovered.** Task 8's
implementer's first edits landed in the main working tree (its default cwd)
rather than the worktree, exactly the failure mode Y2-3's retrospective named.
It detected the misplacement, relocated the three files into the worktree
(verifying their bases were byte-identical between branches first), and fully
reverted the main checkout. The standing safeguard — the controller
independently verifies each agent's commit landed on the branch and that the
main checkout is clean, never trusting the self-report — confirmed the recovery
(commit on `worktree-y2-4-the-meeting`, main clean of the book changes). Every
subsequent book-prose dispatch carried an explicit "work in the worktree, all
paths under this prefix, verify the branch before committing" block, and none
repeated the leak. A safeguard earns its keep again, and the boilerplate is
cheaper than the recovery.

**Grounding the model cards in code, not doc comments, detected a book-wide
falsehood.** Task 9's job was to declare every species/language parameter
authored/drawn/derived/approximated. Doing that *against the source* rather than
against existing prose surfaced two stale claims: deliberation latency is not
idle (it feeds `voice_params`'s formality knob since Y2-3), and — the load-
bearing one — `culture::structure()` never reads the species' `communal` flag at
all, so "communal sociality caps the kobold ladder at `elders` / prevents a
`chief`" is simply false: `elders` is the vocabulary word kobold uses for the
same top rung goblin calls `chief`, a relabel that rides with the species, and
`PsychSummary.communal` is set but read nowhere. That falsehood had propagated
from `culture.md` into the freshly-written capstone. Task 10's freshness sweep
corrected both pages, while preserving the *genuine* sociality signal (it drives
the myth-voice repetition knob via `sociality_register`, a different path). The
lesson: the model-cards discipline is not just documentation — a card that must
cite the code that computes each value is a drift detector, and it caught an
inaccuracy three campaigns of prose had carried.

**The null control came out maximally clean, and the review confirmed it still
has teeth.** All 500 solo pairs were structurally indistinguishable
(`decided = 0`), every categorical distance exactly 0.0, only name length
diverging (SMD ≈ −0.118) — the correct, expected result for a perfect vector-
clone, where structure is per-world identical and only names differ. The risk
was a test that cannot fail; the review verified the opposite by tracing the
failure modes: the exact pins (`indistinguishable == 500`, `decided == 0`) break
on any roster-construction, metric-wiring, or placement bug that made the twin
structurally separable, so the test is a real regression guard whose at-chance
sub-clause is simply dormant under a perfect clone. Preregistration held
throughout: directional assertions first, exact numbers pinned from the first
green measurement, nothing tuned — and because identical vectors produce
identical structure, no threshold was ever tempted.

**Estimate deltas.** The re-baseline was genuinely light, as the corrected spec
predicted: the roster was threaded as new functions *alongside* every existing
signature (`build_world` → thin wrapper over `build_world_with_roster`, etc.),
so the shipped `{goblin, kobold}` path is byte-identical by construction, and
regenerating every committed artifact drifted exactly one file —
`census-lands-drift`'s summary gained the six metrics Task 4 added (five
`Absent` on the shipped roster, one populated), every gallery almanac, map, and
reference page byte-identical. The book close ran to the shape the prior three
retrospectives predicted — the freshness sweep touched more than the obvious
pages (the capstone, `culture.md`, `perception.md`, `religion.md`, the
introduction, the laboratory overview) once the sociality correction had to
propagate everywhere the false claim had reached.

**Do differently next time.** When a spec's central mechanism depends on an
engine behavior (a tie-break, a draw order, a placement rule), read that
engine's code during plan-writing and cite it in the plan, before the task that
relies on it is dispatched — the two-slot degeneracy cost only a plan-time
correction because the placement code was read then, but a spec that had shipped
it to implementation would have failed at the census. And against a live shared
`main`, treat the base as volatile: check `git merge-base` before the first
dependent task and again before merge, rather than trusting the worktree was
cut where the config asked.

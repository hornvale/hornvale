# The Self-Describing Sky — retrospective

**Completed:** 2026-07-13 (name-only designation per decision
`slugs-not-numbers`; plan
`docs/superpowers/plans/2026-07-13-the-self-describing-sky.md`)

**Grounding a "raw" registry row against the actual code before scoping the
plan saved real rework, and the row's own provenance note says so
plainly.** The spec that opened this campaign checked its motivating
registry entry — "the ledger drops derived facts" — against `facts.rs`
before writing a single task, and found the row half stale: moon period,
tide, and inclination had already shipped as committed facts in an earlier
campaign, and the row's headline complaint ("insolation is exactly what
climate wants to read") was already satisfied, because `windows/worldgen`
was already computing and threading `L/a²` into the temperature model. Had
the plan trusted the row's prose instead of the code, it would have spent a
task re-deriving facts that already existed. The lesson generalizes past
this campaign, and echoes a near-identical one from the previous campaign's
retrospective (an "open question" flagged at design time deserves a grep
against the actual surface before it is treated as scope): a `raw` or
`elaborated` registry row is a pointer to an argument, not a snapshot of
the code, and the gap between the two only grows the longer a row sits
unshipped.

**The neighbor epoch had a second-order blast radius the spec initially
denied, and it surfaced only during execution, not during design review.**
§5 of the design spec stated plainly that minting neighbor entities drifts
no rendered artifact — every prose consumer reads neighbors from the
in-memory `system.neighbors` struct, never from the ledger fact being
retired. That claim was correct as far as it went, and wrong about the
whole picture: it did not account for a completely different consumer,
`windows/worldgen`'s deity namer, reaching into the *entity-id counter*
itself rather than into any neighbor-specific fact. Minting five new
neighbor entities ahead of religion genesis shifted every belief entity's
id, and the namer had been seeding generated names directly from that id —
a coupling nobody had named as a coupling, because it did not live at the
fact level the blast-radius analysis was checking. The lesson: a
blast-radius audit that enumerates *readers of the fact being changed* is
necessary but not sufficient when the change also perturbs a *side
channel* — here, a monotonic counter — that something elsewhere in the
system happens to depend on. The fix that ideonomy converged on was to
re-source the coupled value's derivation, not to reorder genesis or work
around the counter; a periodic-grid pass (salt source crossed with
invariance) is what made the "no reordering fixes this, only re-sourcing
does" conclusion legible rather than merely intuited.

**Subagent-driven execution stray-edited the main checkout twice before
the mitigation held, and the fix was procedural, not architectural.** Two
implementer subagents, mid-task, wrote to files under the launch
directory's `main` checkout rather than the dispatch worktree — the
harness's file tools resolve paths against the launch directory, not
whatever directory a Bash `cd` moved into, and a subagent that reasons
about "the repo" in the abstract rather than by an explicit absolute path
can silently land in the wrong one. Both incidents were caught by the
dispatching session (a stray diff appearing where the dispatcher expected
none) and reverted before they reached a commit. The mitigation was not a
new check in code; it was making every dispatch prompt state the absolute
worktree path explicitly and require the implementer to verify its first
edit actually appears in that worktree's `git status` before continuing —
now a standing step in this project's subagent-dispatch discipline, not a
one-off fix for this campaign.

**The naming epoch's entity-id-salt bug had a third victim, caught only by
running the full workspace gate at close.** Task 7 fixed the coupling
inside `windows/worldgen`'s committed-name path and its own tests went
green; what its scoped verification could not see was a second copy of the
same mistake living one crate over. `windows/lab`'s `epithet_honorific`
metric independently re-derives a belief's plain (honorific-free) epithet
to detect the affix structurally, and that re-derivation was calling the
namer with the belief's raw entity id — the exact pre-fix salt Task 7 had
just retired from the code path that actually commits names. It compiled
and had nothing wrong with it in isolation; it silently stopped agreeing
with the committed content the moment Task 7 changed what that content was
derived from, and `cargo test -p hornvale-lab` alone would never have run
it against the rebaselined world this task produced. Only `make gate`'s
full-workspace pass, run as this task's own final verification, caught it.
The fix mirrors the one general principle this campaign is already
recording as a decision: don't inline a second copy of a seed derivation
where a shared function can be exported instead — `deity_name_seed_for`
now lives in `windows/worldgen` as the one place both the committed path
and the lab metric derive a deity/epithet's name seed from, so the two
cannot diverge again the way `insolation_rel` already prevents for the sky
physics. The process lesson stacks on top of the previous campaign's
near-identical one (a task that changes a shape or a derivation consumed
outside its own crate needs a full-workspace build in its own
verification): a *rename or shape change* is the visible case that lesson
named; a *changed derivation whose old and new outputs both typecheck* is
the quieter case, and it took this task's own gate run to catch it rather
than the task that introduced it.

## Estimate vs reality

The eight-task plan ran without task reordering; the one genuine surprise
was scope, not sequencing — Task 7 (the deity-name re-sourcing) was not in
the original plan at all, and was added as a scope addition to the design
spec once Task 4's neighbor-entity work was found to have moved deity
names. That addition was sequenced correctly: before the rebaseline, so
every artifact this task regenerated needed only one pass with names
already stable, rather than a rebaseline now and a second one after the
naming fix landed. No Confidence Gradient bet moved — the campaign's work
sits below any bet the chapter currently tracks — and the chronicle records
that explicitly rather than leaving it unaddressed.

## Close note

The rebaseline surfaced exactly the drift the design spec predicted and
nothing else: new facts and predicates, retired `notable-neighbor`,
changed-but-physics-identical deity names in the three almanacs, and no
moved period, tide, or insolation value anywhere in the regenerated
fixture or artifacts — confirmed by a value-level diff of the golden world
fixture, not merely a byte-count glance. `lens_purity` and the
determinism spot-check both passed against the rebaselined fixture on the
first run.

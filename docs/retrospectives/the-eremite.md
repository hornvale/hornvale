# Retrospective — The Eremite (Dragons program, campaign 2, the keystone)

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## The headline: the spec's determinism argument was sound but INCOMPLETE

The spec proved byte-identity via one set-equality — `{Settled} == the four
peoples == the pre-campaign psyche key-set` — and that argument was correct *for
the re-keyed gates*. But it silently assumed those gates were the *only* thing
that read `psyche_registry` as "the peoples." They were not. Adding the dragons
to `psyche_registry` perturbed **every direct consumer** of that registry: the
genesis species-facts pass, the CLI dictionary/phonology/audio/repl surfaces, the
calibration carrying-capacity aggregates, and the cognate renderer. The
byte-identity *held* (each consumer was re-keyed to preserve it), but only after
a sprawling sweep the plan never scoped.

**Lesson: when a campaign widens a registry's membership, the blast radius is
every reader of that registry — not just the gates the spec re-keys.** The
determinism argument must enumerate the *consumers*, not just assert a
set-equality at the producer. A single grep at spec time — "who reads
`psyche_registry`?" — would have sized this correctly and turned five gate
failures into one planned task.

## Two failure patterns recurred, both already in memory

- **"A task-scoped verification cannot see the cross-task interaction."** Task 2
  (the nested invariant) passed its `--lib components` check and committed
  green — but its own change left three sibling tests and two production sites
  latently broken, invisible until Task 4 added the dragons to psyche and Task
  3/4's full-suite runs surfaced them. Task 2's commit was, in isolation, red on
  the full worldgen suite. **Scoped green is not green.** The tiered gate
  (`make gate-fast` per changed crate) is a convenience, never the pre-commit
  proof for a change that alters a shared contract.
- **"A grep-derived plan is only as complete as the grep — and its
  directories."** Task 3's re-key grep was scoped to `windows/worldgen` and
  `windows/vessel` and missed `cli/` entirely, plus the `.iter()`-shaped
  consumers (aggregates) that don't match a `.is_none()`/`.contains()` gate
  pattern. The plan re-keyed four sites; reality had a dozen across four
  directories.

## What went well

- **The genesis `.expect` panic was caught as a determinism bug, not just a
  crash.** Adding the dragon psyche surfaced a pass that would have written *new
  dragon facts into every world*; gating it on `Settled` both stopped the panic
  and preserved byte-identity — the right fix, verified by the clean regen/diff,
  not merely a silenced `.expect`.
- **The guard tests were re-based, not weakened.** When the dragons made
  `{Settled} != {psyche}`, the byte-identity tests were re-pointed at the stable
  named four-peoples reference rather than deleted or loosened — they still fail
  if the settlement roster drifts.
- **The whole-branch review verified the *mechanism*, not the green.** It
  re-derived the byte-identity argument from the mint sequence and independently
  grepped for surviving peoplehood consumers — and it caught that the review
  package (built from `main..HEAD`) was polluted by main's post-fork commits, a
  scoping trap I handed it.
- **The clean textual merge was not trusted as semantically clean.** Absorbing
  main's 50 commits (topology, connection-graph, The Alarm/Teeth, PROC-19) merged
  with no conflicts, but the full gate *and* the regen/diff were re-run on the
  merged tree before landing — the "semantic collisions hide under clean merges"
  discipline, honored.

## Follow-ups (promoted from the campaign register)

- **Split `PsychVector` → `SocioVector` / `SocialPsychVector`** (Nathan-endorsed,
  2026-07-22): the vector mixes individual-temperament dims (read for a wild
  solitary) with society-structure dims (inert for a hermit). A candidate
  campaign once a second minded solitary or a social-mind read makes it pay.
- **The Solitary Tongue (BIO-37)** — the program's next campaign: language drift
  as a function of `SocialForm`.
- Per-chromatic dragon differentiation; minds for xorn/otyugh; deriving
  `SocialForm` from ecology rather than authoring it.

# Retrospective: The Predicates & the Grammar (C2)

One page, process only. Product story: the chronicle entry.

## What worked

- **Subagent-driven execution with per-task review caught real defects
  early.** Six tasks, four review-fix loops, all resolved before the final
  review: a false doc guarantee (`quantity` on negatives), the
  modifier-order spec violation, an ADR misstatement about
  `KERNEL_CORE_PREDICATES` membership, and a T1 golden re-pin miss. The
  final whole-branch review (fable) then found zero criticals — the
  per-task gates had already drained the pool.
- **Autopilot carried the campaign with zero mid-execution owner
  interrupts.** Eight ledgered decisions; the two constitutional stops (G3
  before, G6 after) were the only pauses. The census carve-out — the one
  thing pre-flagged as needing Nathan — never fired: `make gate-full`'s
  census probes came back clean.
- **The absorb-at-boundary rule earned its keep.** Main moved mid-campaign
  (two campaigns landed: ecs-c6, the-wandering-sun). Absorbing before the
  final gate surfaced a genuine cross-campaign semantic collision (below)
  at a 15-commit merge instead of a 100-commit one.

## What the campaign got wrong, and what it taught

- **Plan literals are intent, not ground truth — three of them failed on
  contact.** The plan's failing-test literal used a `constant(1)` world
  that structurally cannot witness the bug (Constant skies commit no
  moon-count); its `quantity` example demands truncation while its test
  name says "rounds"; its target sentence contradicts its own
  "commit-order" mechanism note. Each was caught (implementer, reviewer,
  reviewer respectively) and adjudicated against the G3-approved surface.
  Lesson: **the spec's approved surface governs; plan sketches are
  starting points that execution must re-verify** — and reviewers should
  receive the approved surface verbatim, which is what caught the order
  bug.
- **A golden re-pin lagged its drifting commit.** T1 missed the
  proto-goblinoid root-table golden; the branch sat red for two commits
  until T2's implementer tripped over it. The re-pin discipline
  ("in the drifting commit") failed silently because T1's implementer ran
  the language crate's tests, not worldgen's — the golden lived in a crate
  the task didn't touch. Lesson: **a concept-inventory change is a
  cross-crate drift event; the re-pin sweep after one must run the
  workspace, not the edited crate.**
- **The spec's determinism note was falsified by measurement.** "Additive
  concept-keyed draw ⇒ existing lexemes unchanged" is wrong: the
  merger-aware root assignment re-probes on collision, so adding `person`
  shifted kobold's `sibling` (Raxo→Rotrra). Streams-additive ≠
  lexeme-space-additive. Census-clean held *empirically* this time; the
  program repeats this motion constantly, so the true contract is now
  recorded: **verify census per concept-add, never assume it.**
- **A parallel campaign's fresh mechanism was stale on arrival.** ecs-c6's
  capability schema declared the genesis pipeline as it existed pre-C1 —
  no `planet` stage — and C2's `peoples` stage deepened the gap. Declaring
  the tail truthfully cycles the schema (predicate-granular, subject-blind
  edges vs. kernel-core `name` written late on disjoint subjects), and an
  edge exemption would make the derived schedule lie (the tail's real
  dependency is in-memory, inexpressible as a predicate read). Resolution:
  explicit scope boundary in `schedule.rs`; subject-aware edges filed with
  the ECS program. Lesson: **a schema that models a pipeline goes stale
  the moment a parallel campaign extends the pipeline; the honest interim
  state is a *marked* boundary, not a silent omission** — and preflight
  cannot score this class of collision, only a full-gate on the merged
  tree plus a human reading both campaigns' contracts.

## Follow-ups (promoted to the campaign register at close)

ECS: subject-aware capability edges (reproduction preserved in the ADR-era
merge-fix report). Book: number-aware pronouns ("it are liches" latent);
the None-autonym surface ("The Entity 103 are goblins.") decided together
with its missing test; zero-moons phrasing test. Language:
`quantity_rounds` → `quantity_truncates` rename. Spec hygiene: word future
concept-add specs as streams-additive only.

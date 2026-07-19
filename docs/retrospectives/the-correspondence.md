# Retrospective — The Correspondence

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-correspondence.md); the shipped
mechanism is the `Manifest` choke-point and the drift-checked coverage report.

## What worked

- **Ground the plan in the API, not the remembered picture.** The pre-plan
  Explore agent that read the actual registration surface corrected four wrong
  assumptions the brainstorm had carried in: there was no `Registry` struct
  (it is `ConceptRegistry`), no central phenomenon enum (kinds are
  string-keyed), no existing `compile_fail`/`trybuild` precedent (the house
  pattern is the dead-code destructure tripwire), and no `wind`→AMBIENT code
  link (thematic only). Each would have become a plan defect. The map cost one
  agent and saved a stage of rework.

- **The byte-identity diff is the migration's decisive test, and it is cheap.**
  A mechanical change across nine domains and seventy concepts is exactly where
  a silent `ConceptDef` drift (a doc typo, a wrong kind) hides. Regenerating
  every committed artifact and diffing to zero caught nothing here only because
  nothing drifted — and it would have caught everything if something had. The
  controller ran it independently at every stage rather than trusting the
  subagent's report; that is the right ratio of paranoia for a determinism
  contract.

- **A construction proof need not touch the save format.** Storing the manifest
  edges in a `#[serde(skip)]` field let a compile-time forcing function land
  with zero serialized delta. The invariant tightened; the world's identity did
  not move.

- **The ideonomy pass earned its keep on the "obvious" decision.** The
  choke-point question felt settled ("force all edges at registration"). One
  pass — an atlas of the four edges by information-flow — overturned that into
  the hybrid design (owner-forced for the outward edges, reconciled for the
  inward lexeme), avoiding a re-coupling of a seam the architecture keeps open.

- **The reconciliation had teeth on first contact.** Seven concepts were
  declared `Expected` on optimism with no lexicon behind them. The check found
  all seven immediately, and the honest fix was to correct the declaration, not
  weaken the test. A guard that catches real slack the first time it runs is a
  non-vacuous guard.

## What to carry forward

- **The type-audit trap recurred, and the memory fired correctly.** The Stage-1
  subagent tagged two boundary primitives and missed the `Void` reason strings;
  `make gate` does not run the type-audit, so the miss would have reached
  `main`. The standing memory ("run the standalone type-audit before pushing
  pub-boundary changes") caught it via controller challenge-response. The
  lesson stands: for any pub-boundary change, the type-audit is a separate gate
  the controller verifies, not something the commit gate covers.

- **Stage-boundary absorption was skipped until close.** `main` was quiet
  through Stages 1–4, so no absorption happened until the merge, when nine
  the-foresight commits arrived at once. It merged cleanly — the registration
  files did not overlap, and the one conflict was a generated file
  (`type-audit-report.md`) resolved by regeneration — so the cost was zero this
  time. But that was luck of scheduling, not diligence; a busier `main` would
  have made the 9-commit absorption a semantic-collision hazard. Check `main`
  at each stage boundary even when it has been quiet.

## Handed forward (followups)

- **Wave 1** — reconnect `wind`'s lexeme to the climate wind vectors (the
  percept gap the report now names). Net-new wiring; no existing code link.
- **Wave 2** — name temperature and the compass directions (the
  modeled-but-unnamed gap), coordinated after the diurnal-temperature data
  campaign settles.
- **The cognition column** — every concept's cognition edge is `Uncognized`.
  The Foresight's planner is the machinery that will begin discharging it; the
  first concept to earn a cognitive handle closes the first cell.
- **`TOOL-concept-ownership-vs-reference`** (idea-registry) survives this
  campaign: `register_manifest` still conflates *claim ownership* with *ensure
  present*, so the registration-order dependence the roster works around is
  untouched. The row is updated to point at the renamed primitive.

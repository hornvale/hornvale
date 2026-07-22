# Retrospective — The Tidings

Belief-sharing (`SOC-belief-sharing`). Six tasks, subagent-driven, under
campaign-autopilot. Process lessons, not product.

## What worked

- **Adversarial per-task review earned its cost twice over.** Two of the
  campaign's three pivotal findings came from reviewers, not implementers or the
  controller: the Task-3 implementer caught that an *ignorant* creature reads
  `Searching`, not distress (invalidating the original harness design), and the
  opus Task-4 review caught that the "measured band result" was measuring an
  ignorant, not a `Frustrated`, creature — which unspooled the whole
  home-anchoring obstacle. A cheaper, agree-and-approve review would have merged
  a hollow demonstration.
- **Implementers strengthening weak tests.** The plan's draft tests twice used
  assertions that would pass under both buggy and fixed code (`assert_ne!(pos,
  here)`; a distress label that was structurally unreachable). Implementers
  recognised this and replaced them with genuinely fix-dependent checks
  (the creature must *drink*; `Searching`→`Eager`), verified by reverting. The
  brief asking for "verify the real signal, block rather than fake" was load-
  bearing — it gave them licence to deviate from the plan's literal pseudocode.
- **Ideonomy on the "obvious" G1 answer paid.** The convergence passes reframed
  belief-sharing from an SIR-epidemic (which wants RNG) to a CRDT/anti-entropy
  join, which is *why* determinism held by construction rather than by careful
  sequencing. The framing was the design.
- **The decision ledger absorbed three mid-flight escalations** without losing
  the thread: co-location substrate (does a live band even exist?), home-anchored
  belief (blocks distress-relief), belief transience (blocks durable healing).
  Each was surfaced with options + a recommendation; Nathan decided; the ledger
  carried the resolution into the docs.

## What was hard / what we learned

- **The plan was wrong about the demonstration three times, and only execution
  found it.** The spec/plan confidently described a lost creature relieved of
  distress by a neighbour — a story the belief model cannot tell. Each layer of
  the obstacle (ignorant≠distressed → home-anchoring filters shared water →
  belief re-locks on movement) was invisible until an implementer or reviewer
  ran the actual affect/arbitration code. Lesson: for a campaign whose payoff is
  a *measured behavioural signal*, the spec's demonstration claim deserves a
  cheap end-to-end probe *before* planning, not confident prose.
- **The honest outcome was smaller than the vision, and that was the right call.**
  Rather than expand scope to persistent told-belief (a new predicate, save-format
  territory, overlapping The Echo) or ship a degenerate demo as a splashy headline,
  the campaign shipped the deterministic law + the honest finding that durable
  band-healing needs persistent belief. The sim-first ethos held: the model taught
  us the roadmap, and we recorded what it said instead of overclaiming.

## Process notes

- **No main-absorptions were needed.** `main` was static at `1162f2ed` (The
  Mettle) for the whole campaign, so the stage-boundary absorption cadence never
  triggered — the preflight confirmed main is still an ancestor at close. Not a
  missed cadence, just a quiet main.
- **Golden pins / keystones:** the campaign is byte-identical live (verified: zero
  artifact drift on full regeneration), so no pin drifted and no keystone fixture
  needed refreezing. The only committed-artifact refresh was
  `docs/audits/type-audit-report.md` (counts from the new vessel `bare-ok` tags),
  caught during Task 5.
- **No Confidence-Gradient bet moved:** `open-questions.md` tracks no cognition/
  belief-tier bet, so no chapter re-score was owed (checked by grepping the
  domain, per the close discipline).

## Follow-ups (also in the idea registry / followups)

- **Persistent told-belief** — hearsay that survives the teller leaving. The
  prerequisite for durable band-healing; the natural next campaign; overlaps The
  Echo's `knowledge.rs` "heard ≠ true" entries.
- **The home-anchoring artifact** — a creature ignores water it stands in if that
  water is unreachable from home. Fits The Surmise's reserved "nearest-to-current
  belief" followup.
- **Live bands at all** — belief-sharing between real NPCs still waits on the
  multi-agent co-location substrate The Belonging reserved.

# Retrospective — The Assay (Dragons program, campaign 1)

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## What went well

- **Verifying the blast radius *before* designing made the whole campaign
  cheap.** The first question after "fix the ranking" was not "what values?"
  but "what breaks?" — and the answer (the mighty menagerie is unplaced, so
  `sovereignty_floor` touches no committed artifact) converted a potentially
  census-regenerating change into a five-literal edit with no golden movement.
  The determinism claim ("no artifact bytes change") was made at spec time,
  restated as a plan gate, and confirmed at close — the same fact carried all
  the way through, never assumed.

- **The program decomposition put the smallest, most independent slice first.**
  Nathan chose "warm-up first" over the architectural keystone. That was the
  right call: The Assay establishes the `CR/30` authoring convention (and its
  age-ladder-readiness) that the later roster campaign will reuse, while
  touching nothing structural — so the big reframe starts from a repo where
  "might has a source" is already settled and tested.

- **The fidelity carve-outs went to Nathan, the mechanics did not.** Under
  autopilot the two genuine judgment forks — CR-vs-alternatives and the
  CR→potency mapping (which sets eventual rarity) — were surfaced as explicit
  questions, while storage form, set membership, and the otyugh borderline were
  resolved from precedent and ledgered. The carve-out rule ("fidelity/accuracy
  tradeoffs are Nathan's call") drew the line in the right place.

- **The review earned its keep on a fidelity nit, fittingly.** A campaign about
  matching the source material shipped a doc comment miscategorizing treant as
  *fey* (it is 5E plant-typed). The whole-branch review caught it. Small, but
  exactly the class of error this campaign existed to remove.

## What to carry forward

- **"Trivial exact-content → implement inline, review by subagent" was the
  right shape for a transcription-grade plan.** The plan contained every
  old→new string and the complete test; dispatching a cold implementer per task
  would have re-derived context to type text already written. Inline execution
  with a single independent whole-branch review kept the quality gate without
  the dispatch tax. The standing "always subagent-driven" preference is about
  the *method* (TDD, fresh-eyes review), which was honored — not a mandate to
  dispatch a subagent for a five-line edit the dispatch skill itself says to do
  inline.

- **The ideonomy pass paid off on the framing, not the numbers.** The lift→
  cross-domain pass did not change CR/30; it reframed the whole program
  ("personhood is a region, not a node") and produced a reusable roster
  generator (the sociality×lifespan grid). The enrichment, not an overturn, was
  the return — captured as UNI-31 / BIO-37 for the campaigns that will spend it.

## Non-events (recorded so the next close does not re-check)

- **Confidence Gradient:** no bet moved. The species/coexistence bets in
  `open-questions.md` (The Niche / The Menagerie / niche-differentiation) turn
  on *resource-niche* differentiation and spatial supply, which re-sourcing the
  *potency* scalar does not touch. Grepped the domain before concluding.
- **Golden pins / census:** none drifted; the mighty are unplaced. No regen.
- **Keystone fixtures:** `world-seed-42.json` is byte-identical (no placement
  change), so no refreeze was needed beyond confirming it did not drift.

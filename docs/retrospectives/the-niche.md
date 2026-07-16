# Retrospective — The Niche

Process lessons, not product. The habitat model itself is in the chronicle.

## What went well

- **Observe before you metric.** Before writing the `composition-variance`
  metric, a throwaway inspection dumped seed-42 compositions and byproducts. It
  showed the oatmeal broke only two ways, which reshaped the metric (assert
  variation and strife structure, not four strongholds) *before* it was committed
  — and surfaced the roster-vs-world calibration gap while it was still cheap to
  reason about. A metric written first would have encoded the wrong expectation.
- **Sequencing measured against determinism paid off twice.** C1 (expose the
  substrate fields) was pulled ahead of B2 (author the niches) so the optima were
  authored in the fields' actual measured ranges rather than guessed units. And
  D1 fed the niche-K to the *shadow* accessor only, leaving settlement genesis
  byte-identical — so the campaign shipped a validated observation without
  drifting a single committed artifact, the same strangler shape the coexistence
  stack used.
- **The 3-attempt / carve-out discipline held under a real fidelity finding.**
  Two rounds of niche re-authoring did not fix the cold specialist; the third
  move was not a third attempt but a diagnosis — the shared NPP base plus the
  sovereignty floor structurally cap climate-only differentiation — brought to
  Nathan as a fidelity carve-out rather than tuned around silently.

## What was harder than expected

- **A god-struct hides until you scale the roster.** Four near-identical peoples
  never strained `SpeciesDef`; twelve axis-spanning creatures exposed it
  immediately. The lesson: fat structs are cheapest to catch at the moment the
  first genuinely different member arrives — the menagerie was that member, and
  the right response was to stop and redesign, not to stuff a fungus with a
  proto-language.
- **Preregistered bands are model-specific.** The frozen β-diversity band was set
  on the flat model; niche differentiation legitimately lowered per-cell
  diversity below it. Re-baselining a *preregistered* target (not just a golden
  pin) is a heavier move — it was done with the measured per-seed values recorded
  and a physical justification in the doc, and flagged to lead the merge digest,
  because "widened to pass" and "re-measured for a changed model" must be
  visibly distinguishable.

## What to carry forward

- **The campaign changed shape mid-flight, correctly.** The Niche set out to ship
  a habitat model *and* a menagerie *and* a genesis cutover. It shipped the
  model, proved it, and handed the rest to an entity-component program the
  menagerie's pressure revealed. Closing at the validated critical path — rather
  than dragging a now-mis-scoped F and G to a forced finish — is the right call
  when a campaign discovers it was standing on the seam of a bigger one.
- **Parallel-collision merges are the norm, and keep-both is usually right.** The
  close absorbed two campaigns that had landed on main (BIO-2, Eclipse Seasons),
  both extending the exact files The Niche touched — `SpeciesDef` and the metric
  registry. The conflicts resolved keep-both (both field sets, both metric sets,
  count = 142), verified by build + the affected tests before the merge commit.
  The stage-boundary absorption cadence would have made this smaller; it was one
  large absorption instead, worth noting against next time.

## Follow-ups

Recorded in `.superpowers/sdd/followups.md` (C1 insolation per-cell caching; the
book model-card the niche doc comments point at; the E1 double world-build). The
large one — the entity-component program — is its own metaplan, seeded by
`.superpowers/sdd/ecs-program-decomposition.md` and
`.superpowers/sdd/entity-model-redesign-exploration.md`.

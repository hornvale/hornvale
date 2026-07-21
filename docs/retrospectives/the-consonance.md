# Retrospective — The Consonance (LANG-48)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **Every review cycle this campaign ran closed a real gap in the
  controller's own pre-approved artifacts, not implementer error — and
  every self-reported deviation was independently re-verified against
  source before being accepted, rather than taken on the implementer's or
  the controller's word.** Across six tasks: Task 1's malformed
  type-audit tag was the controller's own plan text (`bare-ok(numerator)`
  instead of `bare-ok(count: numerator)`); Task 4 found the spec's claimed
  3-schema admitted set for `FactShape::CyclicEvent` was actually 4
  (`CycleReturn` omitted), confirmed by the controller grepping
  `schema_table()` directly rather than trusting the discrepancy report;
  Task 6 found the spec's own below-capability modeling was backwards (a
  PRESENT `Disposition::Lost` entry, not an absent one) — a pattern the
  controller had already seen firsthand reading
  `unbindable_cultures_stay_plain_lost`'s test body earlier in the same
  campaign, but hadn't carried through into the spec's own Task 6 text
  consistently. Worth owning plainly: a controller having already read
  the disproving evidence once is not the same as the plan reflecting it
  — the fix has to happen at write time, not rediscovery time.
- **A mandatory tripwire, not an assumption, gated a genuinely risky
  cross-file coupling.** `chorus_ground`'s `CONSTRUCTION_ORDER` has an
  independently-maintained twin in `windows/book`, and Task 3's plan
  required re-running the existing
  `identity_chorus_reproduces_the_gods_eye_lines` test rather than
  reasoning "this should be safe because X." It passed, and the reviewer
  went further than the tripwire required — tracing the real
  `accounts_of → chorus_ground → render_explanations` call chain to
  confirm the safety mechanism was actually exercised by this campaign's
  changes, not merely present in the codebase.
- **A stream-independence detail the plan called out explicitly as
  load-bearing survived intact.** `explain_moon_ratio` shares
  `FactShape::CyclicEvent` with the existing `explain_day`; the plan's
  Task 4 text mandated an extra `.derive(predicate)` leg on the
  schema-selection stream with a comment explaining why it must not be
  "simplified away." It shipped as specified, and the corresponding
  `stream_labels()`/manifest-regen step was baked into the same task
  rather than left for a reviewer to catch — the exact lesson Few and
  Many's and The Residue's own retros both named, applied this time
  without needing a review cycle to enforce it.
- **The keystone-fixture refreeze at close was recognized correctly as
  expected drift, not a bug.** `make gate` failed on
  `seed_42_world_json_matches_the_committed_fixture` after absorbing many
  parallel campaigns; confirmed via the test's own diff output that seed
  42's real moons produce a genuine new `moon-period-ratio` fact (a
  ~2.04:1 ratio, within tolerance) before reaching for
  `make rebaseline-goldens` — the diagnostic step this project's own
  process document names as required, not skipped under gate pressure.

## What was awkward

- **A malformed type-audit tag of the identical shape recurred in Task 5,
  from the same source (the controller's own plan prose), after Task 1
  had already surfaced and fixed the pattern once.** The Task 5
  implementer caught and fixed it independently this time, citing Task
  1's precedent by file:line — a real win for the review discipline
  generally, but it means the controller's plan-writing itself repeated a
  known mistake rather than the earlier fix propagating backward into
  later, not-yet-written task text. A plan with a `bare-ok(class:
  field-name)` requirement stated once at the top should have every
  later task's tag drafted against that same rule, not re-derived per
  task from memory.
- **The spec shipped two claims (the 3-schema admitted set, the
  `render_quantity_at_rung`-based mechanism for `expressible_at_rung`)
  that were wrong at G3-approval time, not just later invalidated by a
  design choice made during implementation.** Both were catchable by the
  same grep-the-real-source discipline that did catch them — just later
  than ideal. The G3 package presented to Nathan carried these claims as
  settled; the corrected version only exists as a post-close erratum.
  Worth naming plainly in case a future close needs the same move: an
  erratum section addressing exactly the wrong claims, left in place
  rather than silently rewriting the historical record, is the right
  shape for this, not a full spec rewrite.

## Follow-ups

- **LANG-50** (moon-vs-year lunisolar relationships, N-ary resonance
  chains, event-coincidence framing via the existing eclipse/conjunction
  machinery, and extending `SchemaId::LinkSympathy` to admit
  `FactShape::CyclicEvent`) — captured to the idea registry during
  brainstorm; real, deliberately deferred.
- **No live rendering exists yet.** The ratio is detected, gated, and
  bound to a schema in a culture's account; saying it aloud in the Book's
  rendered prose, using Few and Many's `comprehend_quantity` seam, is the
  natural next campaign — mirroring the exact "mechanism now, rendering
  later" precedent The Residue and Few and Many both already set.
- **A plan's own stated tag-format rule should be restated at the top of
  every later task's own text, not left to whoever drafts that task to
  remember correctly** — the gap this retro's own "what was awkward"
  section names above, generalizing beyond just this campaign's tags.

# Retrospective — Few and Many (LANG-44)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **A targeted research pass before brainstorming corrected two false
  claims in the campaign's own idea-registry row before any design
  decision leaned on them.** The row asserted this campaign "reuses
  packs.rs ladder machinery" and integrates at "the parse seam LANG-3
  builds" — both checked and found wrong: `packs.rs`'s ladder is a
  per-concept lexical-inclusion gate, structurally unsuited to a
  rendering-style selector, and `ClauseSpec`'s round-trip machinery never
  carries a structured quantity value at all (numerals are baked into
  pre-rendered opaque strings before they reach that layer). Finding both
  before the spec was written, not mid-implementation, meant the spec
  could point at the real integration point (`fact_for`, in
  `windows/book`) and the real precedent (`MorphDepth`'s per-species draw
  shape) from the start. Second campaign in a row where research caught a
  registry row's own claim being false — worth treating as the default
  expectation for a "reuses X" claim, not the exception.
- **A matrix organon changed the actual scope of the campaign, not just
  enriched it.** The registry's own idea named five numeracy rungs; laying
  them out against typological frequency and existing-code-to-reuse
  showed the two rungs WITH code (`cardinal`, `quantity`) sit at opposite
  frequency extremes, and the two rungs the campaign would have had to
  build from nothing are also the least typologically common — a real,
  matrix-derived argument for shipping 3 rungs and banking 2, not a
  vibes-based cut. This is the second campaign in a row where an ideonomy
  pass produced a genuine scope OVERTURN rather than mere enrichment
  (the-residue's MorphDepth-reuse overturn was the first) — worth noting
  the overlay is earning its keep on real decisions, not just checking a
  box.
- **A second ideonomy pass caught a subtler bug in the mechanism's own
  design before any code was written**: the initial plan ("a listener
  always degrades to their own rung") is only correct because the speaker
  happens to be fixed at the highest rung; the pass derived the general
  `min(speaker_rung, listener_rung)` channel-capacity formula and proved
  the simpler version is its exact special case under that fixed-speaker
  constraint. This is the kind of thing that would otherwise have shipped
  as "good enough" and only failed once a future campaign let the speaker
  vary — caught at design time instead, and the general formula is now on
  record (idea registry) for whoever builds that follow-up.
- **The whole-branch reviewer independently confirmed a suspicion the
  controller raised rather than rubber-stamping it.** The controller
  flagged the Task 2 implementer's `#[cfg(test)]` gate as possibly wrong
  (compiling the "demonstrated integration point" out of release builds
  entirely) and asked the reviewer to form an independent judgment, not
  just validate the concern. The reviewer read the actual code, reasoned
  through three converging arguments, and agreed — including correctly
  rejecting a third option (`pub`/`pub(crate)`) the controller hadn't
  fully ruled out. This is the review loop working as designed: a
  controller's hunch got real, independent scrutiny rather than automatic
  agreement.

## What was awkward

- **The exact same class of bug from the immediately-prior campaign
  (The Residue) recurred, in the same shape, but was caught before close
  this time instead of after.** Adding a `stream_labels()` entry drifts
  `book/src/reference/stream-manifest-generated.md`, and neither task's
  plan steps included regenerating it — only the type-audit report got an
  explicit regen step. The controller's dispatch prompt for the final
  whole-branch review explicitly named this exact failure mode (quoting
  the-residue's own retro) and asked the reviewer to check for it
  specifically — and the reviewer found it, before any commit reached
  close-time `make gate`. The lesson from the prior retro ("any task that
  regenerates a stale generated artifact... must trigger a full gate
  re-run") generalizes further than that retro said: **any task that
  ADDS a `stream_labels()` entry, whether or not it also fixes something
  stale, needs a manifest-regen step in the plan itself** — this was
  caught by asking a reviewer to specifically look for it, not by the
  plan anticipating it. A future plan touching `stream_labels()` should
  bake the regen into the task's own steps, the same way Task 2 already
  baked in the type-audit report regen.
- **A plan text detail (private-function visibility) collided with a
  compiler lint (`dead_code` under `-D warnings`) in a way the plan didn't
  anticipate.** The brief specified an unconditional private function
  with no in-crate caller yet — a combination that reliably fails
  `clippy -D warnings` regardless of the function's purpose. The
  implementer's own fix (`#[cfg(test)]`) solved the immediate compile
  failure but silently changed what the campaign actually ships (a
  release-absent test fixture, not a standing library function) — worth
  noting for future plans: "add a private function with a single
  test-only caller for now" is a request that needs its lint-interaction
  spelled out (`#[allow(dead_code)]` + why), not left for the implementer
  to improvise under compile pressure.

## Follow-ups

- **LANG-47** (BodyTally + Fractions rungs, and the general
  `min(speaker_rung, listener_rung)` formula for bidirectional rung
  variation) — captured to the idea registry during brainstorm; real,
  deliberately deferred.
- **No live wiring exists yet.** `comprehend_quantity` is a real, present-
  in-all-builds function with zero callers outside its own tests — wiring
  a possessed character's own species rung into the vessel's `write`/
  `consult` verbs, so comprehension degradation actually happens during
  live play, is the natural next step, deliberately left open (mirroring
  The Residue's own "mechanism now, rendering surface later" precedent).
- **Any future plan adding a `stream_labels()` entry should include an
  explicit manifest-regen step**, not just a type-audit-report regen step
  — the gap this retro's own "what was awkward" section names above.

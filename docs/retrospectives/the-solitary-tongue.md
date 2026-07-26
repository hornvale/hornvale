# Retrospective — The Solitary Tongue (Dragons program, campaign 3)

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## The consumer table paid off — the lesson held

Unlike The Eremite (whose blast radius was discovered one gate-failure at a
time), this campaign put the articulation/lexicon/speaker **consumer table in
the spec up front** (§4) and re-verified it on the merged base as Task 0. The
sweep was then *bounded*: the two latent panics that did surface
(`exposure_of_impl`'s "a speaker perceives", and `cascade_of` re-assembling
canonical registries vs. a `wc`-holding caller) were each caught by a subagent
**running the full suite**, not the touched crate — and both were anticipated as
a *class* by the table, so they read as expected fallout rather than surprises.
Enumerate-consumers-at-spec-time works.

## Three tooling seams worth remembering

- **A task that changes a shared signature cannot commit in isolation under a
  workspace-wide pre-commit hook.** Task 1 added a `build_lexicon` parameter and
  the hook (`make quick`, workspace-wide clippy) refused the commit until every
  downstream caller compiled — correctly. The fix was to land the byte-identical
  scaffolding (`SETTLED` at every call site) with the signature change. And:
  **`cargo build --workspace` misses test-only call sites** — use
  `cargo clippy --workspace --all-targets` (what the hook runs) to find them all
  in one pass, not one E0061 at a time.
- **type-audit reads a struct's field verdicts from the STRUCT's doc comment**,
  not per-field (`push_struct` → `doc_text(&s.attrs)`). Per-field `type-audit:`
  lines are silently ignored; put `bare-ok(count: a), bare-ok(count: b)` on the
  struct doc.
- **Regen-after-fix needs a full re-gate.** Task 6 ran `make gate` *before* the
  artifact re-pin, so the audio-artifacts test never saw the new Draconic
  sample-name references; the missing clips surfaced only at the close's merged
  gate. Re-pinning is a code change — gate *after* it, always.

## The absorb changed a golden — re-pin in place, prove byte-identity elsewhere

Absorbing main pulled in *The Sundering* (isolation-predicts-divergence,
deep-history), which shifted seed-42 biome exposure and drifted the peoples'
lexicon golden by five experiential gap-reasons. This looked alarming for a
campaign claiming peoples-byte-identity — but the drift was **exposure**, not
**word forms**, and the byte-identity was already proven *independently* of any
world snapshot by the `draw_cascade`/`build_lexicon` default-regime property
tests. Lesson: pin byte-identity on a **mechanism property** (default regime ==
old constant) as well as a world golden; when the golden drifts at an absorb, the
property test tells you whether it is your change or the incoming one. Re-pinned
in the merge, not deferred.

## What went well

- **The additive-artifact posture was clean.** "Byte-identical for the peoples,
  additive for the dragons, not an epoch" held exactly: the reference pages grew
  by pure addition, no world moved, and the payoff ("no change" derivations, a
  measured 0.25 < 0.32 divergence) is visible in the committed book.
- **Subagent rigor under a long session.** Task 5's implementer rejected a naive
  proto-vs-modern segment count (nativize noise), asserted the quantity the
  regime actually controls (rule count ≤ 1), length-normalized the divergence
  metric (the raw mean pointed the wrong way), and mutation-verified every
  assertion — measure-don't-narrate, unprompted. Fresh per-task context is a real
  mitigation for controller fatigue on a long campaign.

## Follow-ups

- **The placement-gated `.expect("peopled pass over a fauna kind")` sites** —
  dragon-unreachable now, will panic when a future campaign *places* the
  menagerie; fix them (baseline-fallback, as `exposure_of_impl` did) as part of
  that campaign.
- Dragon perception; per-chromatic Draconic differentiation; the `SocioVector`
  split; deriving `SocialForm` from ecology.

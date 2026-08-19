# 0148. Never ship a typology bundle no family uses

**Status:** Accepted (2026-08-19) · **Decider:** Nathan · **Relates:**
[0147](0147-a-typology-bundle-is-authored-not-derived.md) (a bundle is
authored), [0016](0016-measurement-is-preregistered.md) (measurement is
preregistered)

In the context of building the tone tier, the harmony rule, the templatic
morphology and the per-bundle laws, we decided that **a capability is not
shipped until a real family reaches it** — an authored `Typology` field is
inert scaffolding, not a feature, until `family_typology()` binds it to a kind
that a world actually generates — because a mechanism no family uses passes
every unit test that supplies its own inputs while doing nothing to any world,
and reads to the next campaign as working.

## Context

The tone tier was the precipitating case. `draw_tone_inventory`, the capacity
floor's tone-widening and `RuleKind::Tonogenesis` were built, unit-tested with
synthetic envelopes, and **reached by no shipped species** — every authored
`tonality` was 0.0, so nothing reddened while nothing worked
(`LANG-tone-tier-inert`). The same shape threatened harmony, the sonorant floor
and the phonotactic law: a bundle field can be defined, tested in isolation,
and never bound to a family. The Burr closed each by binding it — draconic to
`isolating-tonal` with a non-zero tonality, the elves to `sonorant-open`, the
dwarves to `templatic` — and proving the binding fires against a regenerated
artifact, not just a green unit test.

## Consequences

- Every bundle field ships with an end-to-end witness: a real regenerated
  dictionary or name that exercises it, the guard against the vacuous-mechanism
  failure this campaign hit repeatedly (a predicate broader than the property
  it protects; a probe loop that never advances).
- A capability that is built but deliberately unbound must say so — an
  idea-registry row with the measurement, not a silent inert field.
- The cost is that "the machinery exists" is never the deliverable; "a family
  audibly uses it" is. The Burr's own tone tier records the boundary: it is
  reached at the proto root and stripped by the atonal daughters, so it is
  bound but not yet audible in speech — recorded, not claimed as done.

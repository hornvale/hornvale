# 0548. Individual deviation is the kind's dispersion, drawn per resident

**Status:** Accepted (2026-09-01, ratified at G3) · **Decider:** Nathan · **Campaign:** The Roll

In the context of a settlement that now derives eighty residents where it
derived one, facing the fact that `body_at` gives every one of them the same
three mind dials, we decided that a resident's deviation is **the kind's
existing `Dispersion.mind` spread, drawn per resident**, accepting that the
mechanism perturbs deliberation and tolerance for threat and not destination.

## Context

`body_at` derives every field from the settlement's `peopled-by` species and
the settlement's own position: same home, same resource, same three dials,
same boldness, same label. Deriving eighty of them unchanged would produce
eighty identical creatures making identical decisions, and the chart legend
would dedupe them to one noun. `PSY-individual-deviation` recorded exactly
this: "every member is currently identical."

The spec's first draft said a new per-kind spread registry would be added.
The plan's survey found one already there: `hornvale_species::Dispersion.mind`
(The Tolerance) and `windows/worldgen/src/disposition.rs`'s `perturb`, which
draws a *settlement's* disposition around its people's mean by exactly the
mechanism a resident needs.

## The rule

- **`species_mean + spread × unit`**, where `unit` is one uniform draw on
  `[-√3, √3]` (matching standard deviation), `spread` is the kind's existing
  `Dispersion.mind` row, and the result is clamped to `[0, 1]`. This is
  byte-for-byte the settlement-level mechanism; the resident draw is its next
  rung, keyed by the resident rather than by the occupation. **Coarse
  constrains fine**, with one spread serving both rungs.
- **Keyed by (site vertex, ordinal)** on the `settlement/resident/v1` stream.
  A settlement carrying no `cell-id` fact fails loudly with the settlement
  named rather than keying onto vertex 0 — a real vertex, which would alias
  one settlement's residents onto another's stream. Every settlement gets a
  `cell-id` at genesis, so absence is corruption.
- **No new registry, and no new coverage test.** The dispersion roster is
  already ratcheted total over every minded kind, so the existing coverage
  carries over unchanged and there is no second number for one concept.
- **The fatigue rate is not perturbed.** The Wicket's per-kind row is
  unmerged and this campaign does not reach into it.

## Consequences

- **A timid goblin and a rash one are now possible**, and a settlement's
  residents no longer move as one blob because their deliberation latency and
  their boldness differ.
- **Whether that is large enough to SEE was preregistered** (spec §8, M5) with
  a null declared a finding in advance. Measured 2026-09-02 across the 64-seed
  probe: **16 seeds show two residents of the home settlement standing in
  different rooms by the end of day 3.** The null did not occur. The count is
  separation, not attribution — it is not a controlled comparison against an
  unperturbed roll, and it should not be quoted as one.
- **The next rung is a different question.** Which axes a species has at all
  (`PSY-axes-from-ecology`) and per-person accumulated components
  (`PSY-expertise-per-individual`) attach to the row this campaign built; they
  are not this decision.

## See also

- `docs/superpowers/specs/2026-09-01-the-roll-design.md` §3.3, §8 M5.
- `windows/worldgen/src/disposition.rs` (`perturb`, `UNIT_SD_HALFWIDTH`),
  `hornvale_species::dispersion_registry` (The Tolerance).
- Decision 0547 (a resident is a living person), 0102 (a stream keys on a
  place in a fixed lattice, never a generation ordinal).
- `book/src/chronicle/the-roll.md`.

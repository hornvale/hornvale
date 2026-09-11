# 0976. Ametabolic life is a category error

**Status:** Accepted (2026-09-11) · **Decider:** Nathan · **Campaign:** The Trencher

## Context

`ThermalStrategy::Absent` is documented as "No metabolism at all
(construct/undead analogue): no life-history", and it has behavioural teeth:
`domains/species/src/allometry.rs` returns a basal metabolic rate of `0.0` for
it and nulls the biological traits before any of the four time laws run, so a
carrier has no lifespan.

**Its only carrier is `xorn`** — which is also `TrophicMode::Chemotrophic`
with an authored `CHEMOSYNTHATE` niche weight of `0.35`, fed deliberately by
The Sources' Task 9 alongside the per-rung energy field that supplies it. The
registry therefore asserts, of one creature, that it has no metabolism at all
and that it eats chemical gradients.

This was authored on purpose rather than by oversight:
`domains/species/tests/suite/coverage.rs` registers `xorn` as the **tested
witness** (`Rung::Witnessed`) for `ThermalStrategy::Absent`. The Gossan's
split of thermal strategy from trophic mode left the thermal half where the
old `MetabolicClass` had put it, and `Absent`'s doc — which still references
the deleted `MetabolicClass::Autotroph` — was never re-glossed for an axis
that had stopped being metabolic.

The Trencher's split of `TrophicMode` into three axes forced the question,
because an ambiguous token copied onto three axes is three times as ambiguous.

## The rule

**A living kind always has a metabolism. `ThermalStrategy::Absent` is for
things that are not alive.**

Nathan's words: ametabolic life does not make sense; if the world wants
ametabolic things, it should have **ghosts** — or constructs, or undead — and
those are not creatures with a trophic mode.

Two consequences follow immediately:

- **`xorn` moves from `Absent` to `Unmodelled`.** It is alive, it burrows
  through stone, and it eats mineral and a chemical gradient. Whether a
  stone-dweller runs warm or cold is a modelling call **nobody has made**, and
  `Unmodelled` is the value that exists to say exactly that ("Has a
  metabolism; its thermal behaviour is not modelled").
- **`ThermalStrategy::Absent` becomes uninhabited**, demoting from
  `Rung::Witnessed` to `Rung::Declared` — a state that enum explicitly
  supports ("The variant or branch exists; no kind carries it"). It is
  **reserved, not retired**: it is the correct value for the first genuinely
  non-living kind the project authors.

## Why `Unmodelled` rather than `Ectothermic`

A creature living in rock at cave temperature is plausibly ectothermic, and
that is precisely why it must not be assigned here. `Unmodelled`'s own doc
records that it exists because "no single existing value preserves both" of
two groupings the shipped code makes, so "the honest answer is a value that
says the modelling call was never made."

Assigning `Ectothermic` would make that call silently, inside a vocabulary
refactor, on the strength of a plausible-sounding inference. That is the shape
of error this project's decision log exists to prevent.

## Consequences

- **`Unmodelled` now carries two distinct debts, not one.** Its doc is written
  about the autotroph physics problem (`BIO-autotroph-physics`); it now also
  holds "is a chemolithotroph thermally coupled to its rock?" Both are
  genuinely unmodelled, and the doc must say so rather than reading as though
  the autotroph case were the only one.
- **World numbers move.** `xorn` gains a basal metabolic rate
  (`B0_ENDOTHERM`, where it had `0.0`) and a lifespan (where it had none).
  Capacity, occupancy and possibly placement follow. This is a `rebaseline`,
  not an epoch — no stream label moves and no consumption order changes.
- **A future campaign must not author an ametabolic creature.** If a kind
  needs `ThermalStrategy::Absent`, that is a signal it is not a creature —
  reach for a ghost, a construct or an undead, which will want their own
  treatment rather than a `BiosphereTraits` row with the life nulled out.
- **The coverage ratchet records a demotion, and that is correct.** A
  `Declared`-but-unwitnessed variant is not a gap to be filled by finding
  something to put in it. Filling it with a living creature is the error this
  record names.
- **`TrophicMode::Absent` was already uninhabited** and is governed by the
  same rule for the same reason. The Trencher's split moves ontological
  absence outside the metabolic triple entirely, so a construct is not
  described as a creature with three absent axes.

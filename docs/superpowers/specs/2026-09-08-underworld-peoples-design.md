# Underworld Peoples — design

## 1. Intent

Add four peoples to the Underworld roster:

- mountain dwarf
- duergar
- svirfneblin
- kuo-toa

The implementation must use the Underworld's existing per-chamber conditions
and delve-depth seating. Surface elevation must not be used as a proxy for
underground depth. The existing dwarf family remains a family, and the new
peoples must be real registry participants rather than display-only names.

## 2. Governing constraints

The Delvers withdrew mountain dwarf and duergar because the old species model
could express only metres above sea level, not depth below the surface. The
Underworld subsequently added a separate delve ladder, chamber depth budgets,
geothermal conditions, water-table state, and chamber seating. This campaign
consumes those signals; it does not revive the old elevation encoding.

The existing Underworld and species code also establishes these boundaries:

- `EnvironmentNiche` is authored data; its values must be explained by fields
  the consumer actually reads.
- `HabitatRealm::Subterranean` is a sparse realm gate, not a substitute for
  chamber seating.
- aquatic suitability is a habitat-medium concern distinct from subterranean
  depth. Kuo-toa may require water-bearing chambers, but must not be made deep
  merely because it is aquatic.
- dwarf longevity and other family-level traits stay shared across dwarf kinds.
- the species roster, language roster, environment corpus, and worldgen
  seating are separate projections that must remain synchronized by tests.

## 3. Recommended model

Author the four kinds as a single accession cohort with two ecological axes:

1. **Delve depth / chamber conditions** for the three terrestrial peoples.
   Mountain dwarf should prefer high, stable, mineral-rich halls; duergar
   should prefer deeper, warmer, more energy-bearing chambers; svirfneblin
   should prefer sheltered, low-light refuge with a broad viable overlap with
   the dwarves.
2. **Water-bearing chamber medium** for kuo-toa. Kuo-toa should be
   subterranean only where a chamber's water state supports it, with depth
   affecting it only through the existing chamber conditions.

The four kinds should be admitted only after the existing preregistered
Underworld measurements are rerun against the live implementation. The
decision table is:

| Measurement outcome | Action |
|---|---|
| Delve seating varies across terrain and kinds can be distinguished | Author the relevant niches and add the four kinds |
| Delve seating varies, but one proposed niche has no viable cells | Keep the kind out and record the null; do not lower the floor |
| Delve seating is effectively constant or aquatic chambers are absent | Ship the measured substrate result only; defer the affected kind(s) |
| A niche separates by an unconsumed or proxy axis | Reject that niche and redesign around a consumed chamber field |

Mountain dwarf and duergar must satisfy the existing family-style separation
criterion: distinct modal seating or chamber preference, with meaningful
overlap rather than total ecological partition. Svirfneblin should act as a
third contrast, not as a second name for duergar. Kuo-toa is the control for
medium: its distinguishing evidence must come from water-bearing chambers,
not authored darkness or a fake low-elevation curve.

## 4. Components and data flow

### Species domain

- Add the four `KindId` rows to the biosphere registry.
- Add condition niches and the necessary family/resource/physiology rows.
- Add subterranean realm rows for the three terrestrial underworld peoples and
  kuo-toa.
- Add dwarf-family membership only for mountain dwarf and duergar.
- Add aquatic locomotion / medium capability for kuo-toa using the existing
  registry shape.
- Extend language, social-form, metabolic, sleep, lifespan, and coverage
  projections according to the existing roster conventions.

### Climate and terrain projections

- Extend the underworld environment corpus only if a new community or chamber
  category is needed to express a real consumed condition.
- Reuse existing cave genera, water state, substrate, heat, and light values
  where they already express the distinction.
- Do not add a second source of truth for chamber habitat or invent a named
  biome solely to make a species fit.

### Worldgen

- Extend delve seating and capacity consumers to see the new niches.
- Keep stream labels and draw order unchanged unless a genuinely new random
  choice is required; pure projections are preferred.
- Ensure surface and underworld occupancy remain separate as specified by the
  existing Underworld design.

### Client and language surfaces

- Update generated/proto language output and any human-readable roster lists.
- Preserve deterministic ordering and update byte/artifact fixtures only after
  reviewing the resulting diff.

## 5. Verification

Tests will cover behavior, not merely registry membership:

- every new kind resolves through all required registries;
- the dwarf family contains the surviving and re-admitted dwarf kinds;
- subterranean and aquatic gates point to the correct kinds;
- mountain dwarf, duergar, and svirfneblin are seated only on reachable
  underworld rungs;
- kuo-toa requires a water-bearing chamber and does not gain suitability from
  darkness alone;
- the dwarf contrasts have viable overlap and no kind is admitted by a proxy
  elevation/depth encoding;
- language/proto output and coverage ratchets include all four names;
- same seed and pins remain byte-identical.

The full workspace gate remains the final verification. Any census or
generated-artifact drift is handled through the repository's canonical
artifact workflow, not silently accepted during implementation.

## 6. Non-goals

- redesigning the settlement/coexistence allocator;
- adding a general Underworld biome taxonomy;
- adding surface↔underworld trade, history, culture, or diplomacy;
- changing the existing three dwarf niches or moving drow/xorn/rust-monster;
- implementing arbitrary multi-community occupancy beyond the Underworld
  behavior already shipped;
- adding client rendering beyond roster/readout support required by the new
  names.

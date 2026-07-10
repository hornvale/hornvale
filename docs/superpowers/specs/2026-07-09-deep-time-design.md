# Deep Time — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-09-year-3-metaplan-design.md` (§5 binds this campaign; Constitution §2 governs)
**Provenance:** Campaign 1 of Year 3 in the metaplan's sense — the first
history. Firm Ground II (chronicle 20) gave the sky a past: obliquity,
eccentricity, and precession are now functions of `WorldTime`
(`domains/astronomy/src/forcing.rs`, `OrbitalForcing`), with a zero-forcing
pin (`ForcingPin::Zero`). This campaign builds the first consumer of that
forcing: a paleoclimate domain that re-runs climate over deep time and
extracts the durable marks a glacial history leaves on the present. The
sequence number 24 follows the highest committed chronicle (23, The Orrery);
the chronicle is the authoritative order (decision 0017), so this renumbers at
chronicle time if a parallel thread claims 24 first. Downstream: Campaign 2
(The Ledger of Ages) reads paleoclimate's strata through a composition-root
summary to derive carrying capacity; the Year-4 epistemic layer reconstructs
this past.

---

## 1. What this campaign delivers

A new domain crate `domains/paleoclimate` that models **Milankovitch glacial
cycling** — ice ages driven by the orbital forcing the sky already holds — and
extracts the **strata** a million years of glaciation leaves behind: fossil
shorelines, refugia, and ice-extent envelopes. A world's "the frost retreated"
becomes a committed, queryable fact rather than a phrase.

**Observable ending:** a paleoclimate map showing a fossil shoreline and a
refugium over the present relief; a Deep Time section in the seed-42 almanac;
and `recount`/`why` on the world speaking "the frost retreated" with the
glacial-maximum era. A zero-forcing world shows none of it.

## 2. The crate and its boundary

`domains/paleoclimate` depends on `hornvale-kernel` and nothing else
(constitutional layering, ADR 0002). This boundary was settled by the metaplan
reading `domains/climate/src/provider.rs`: `GeneratedClimate` already exposes
everything paleoclimate reads as bare kernel types —
`mean_temperature_at(cell) → f64`, `temperature_at(cell, day) → f64`,
`habitability() → &CellMap<bool>`, `geosphere() → &Geosphere` — so the crate
needs no climate internals and no domain-to-domain edge.

Paleoclimate produces a **derived, non-serialized** `PaleoRecord` — strata
fields over the mesh, recomputed on demand from seed + forcing, exactly as
`GeneratedClimate` is recomputed and never serialized (`provider.rs`). Only
**summary facts** land in the ledger (§6), matching terrain's "summary only,
never per-cell" rule (`domains/terrain/src/facts.rs`).

## 3. The physics: the Milankovitch–Köppen model

The scientifically faithful path. This **supersedes** the metaplan §5 binding
decision of "a simple lagged/threshold one-pass model, not a coupled GCM" — a
legitimate refinement under new information (the campaign brainstorming
prioritized scientific accuracy). The supersession is deliberate and recorded
here; the trade it accepts is a stateful forward integration (§4) in exchange
for reproducing the real driver of glacial cycles. It remains fully
deterministic and formula-defined — no GCM, no runtime model (ADR 0009), no
new RNG draws.

The model marches at a **fine ice step** (~2,000 standard years, ~500 steps
over the 1 Myr window). At each step:

1. **Caloric-summer insolation index `g(t)`** — the classic Milankovitch–Köppen
   driver: summer half-year insolation at a high reference latitude, a
   closed-form function of `forcing.obliquity_at(t)`, `eccentricity_at(t)`, and
   `precession_at(t)`. All three orbital elements enter, so all three leave a
   durable mark — required by the year's blind-attribution exit criterion.
2. **Nonlinear global ice volume `V(t)`**, marched with **hysteresis**: ice
   grows slowly when `g` falls below a low threshold and melts fast when `g`
   rises above a high threshold. The asymmetric growth/decay thresholds produce
   the glacial sawtooth and let the weak 100-kyr eccentricity term dominate the
   record — the "100-kyr problem," resolved by ice-sheet nonlinearity rather
   than linear forcing. `V(t)` is the one stateful quantity; it carries the
   memory.
3. **Albedo feedback** — a global cooling offset `ΔT(t) = −k · f(V(t))`. This
   is the amplifier that turns the small raw orbital signal into a real
   glacial–interglacial temperature swing; without it the physics is honest but
   the amplitude is negligible.
4. **Eustatic sea level** — `Δsea(t) = −eustatic(V(t))`: growing ice locks up
   ocean water and the sea falls.

Reference constants (obliquity ~41 kyr, eccentricity ~100 kyr, precession
~21 kyr) already live in `astronomy/src/forcing.rs`. The reference latitude,
the growth/decay thresholds, the albedo gain `k`, the eustatic coefficient, and
the glaciation temperature threshold are paleoclimate constants, declared on
the model card as approximated (calibrated to give ~10 visible cycles and a
plausible glacial–interglacial swing over 1 Myr), not derived.

## 4. The era loop at the composition root

The loop lives in `windows/worldgen`, which already owns `climate_of`
(`windows/worldgen/src/lib.rs`) — not inside either domain. It is **two-rate**:

- **Fine rate (~2 kyr):** compute `g(t)` and march `V(t)` (cheap, scalar).
- **Coarse rate (~25 climate eras across 1 Myr):** re-run `climate::generate`
  with that era's obliquity (`forcing.obliquity_at(t)`), that era's sea level
  (present sea level + `Δsea(t)`), and the `ΔT(t)` offset applied to the
  temperature field; hand the snapshot to paleoclimate for strata extraction.

The present (`t = 0`) is the newest era; the loop looks back 1 Myr. This is the
**single file** where era-tick order is defined, so the determinism-critical
ordering is auditable in one place (metaplan §9), and it is the orchestration
seam Campaign 2's era-ticked population pass reuses — the structural reason the
loop belongs at worldgen rather than inside a climate tier.

**Diagnostic spatial extent.** At each climate era the spatial ice mask is
*diagnosed*, not integrated: a cell is under ice if it is land and its
offset temperature (`temperature + ΔT`) is below the glaciation threshold.
Volume carries the memory; extent follows from the same forcing. This is the
EMIC-style parameterization — the accuracy/complexity sweet spot chosen over a
fully-spatial per-cell ice field, which is deferred to a later refining tier
(§7).

## 5. Strata extraction

The full strata are fields on the non-serialized `PaleoRecord`, extracted from
the time series of climate snapshots:

- **Ice-extent envelope** — the union of the per-era ice masks ("this valley
  was under ice at the glacial maximum").
- **Fossil shoreline** — the tide-mark band swept by eustatic sea-level change:
  the cells lying between the lowest stand (the glacial maximum, when ice locks
  up ocean water and the sea falls, exposing continental shelf) and the highest
  stand, so a cell in the band was sometimes shore, sometimes not. The durable
  record of a sea that moved.
- **Refugia** — cells habitable (per `climate::habitability` semantics, with
  the era offset) through the glacial maximum: places life could persist when
  the ice was greatest.

Present-climate commits none of these; they are paleoclimate's distinctive
output and exist only because the forcing has a history.

## 6. The fact vocabulary

Only *summary* facts land in the ledger, place-tagged via `Fact.place` where a
stratum is somewhere specific. Predicates registered per-domain, value-kind
enforced (ADR 0010):

- `glacial-maximum-era` (Number, functional) — the `WorldTime` day of peak ice.
- `max-ice-fraction` (Number, functional) — the fraction of land under ice at
  that maximum.
- `fossil-shoreline` (Text, non-functional, place-tagged) — a salient
  transgression, e.g. "the sea once reached here."
- `refugium` (Text, non-functional, place-tagged) — a place that stayed
  habitable through the cold extreme.
- `frost-retreat` (Text, non-functional) — the narrative "the frost retreated,"
  so `recount` (`windows/historiography`) speaks the world's glacial past
  without any consumer learning which system produced it.

Summary-only keeps saves small; the full per-cell strata live on the derived
`PaleoRecord` and are re-extracted on demand.

## 7. The provider-tier seam

Paleoclimate exposes its record behind a small provider boundary and commits
the fact vocabulary above. Because downstream consumers bind to **facts** (the
trace protocol), and because ice dynamics are **derived from orbital forcing,
not drawn from RNG** (no seed streams consumed), a future **fully-spatial ice
tier** — per-cell ice with local growth/melt, emergent geometry, and margin lag
as committed history — slots in later as a refining tier with no new
save-format stream contracts and no consumer changes, and is A/B-compared in
the Lab. This is the coexisting-tiers pattern the Constitution already uses
(tier-0 `ConstantSun` beside the generated star system): higher fidelity
refines, never contradicts, lower. The diagnostic-extent model is tier 0/1;
the spatial model is a deliberately-left-open later tier.

## 8. Typed quantities

New newtypes for coherent physical units crossing API boundaries (decision
0008): `IceVolume` and `SeaLevelChange` (metres). The caloric-summer index and
ice fraction are dimensionless ratios and stay bare `f64`. Orbital elements
already have their representations in `domains/astronomy`.

## 9. Determinism and testing

Determinism is the year's sharpest constraint (metaplan §9); this campaign adds
the first forward-run history.

- **Byte-identical per seed+pin:** same world → byte-identical strata, facts,
  map, and almanac section. Asserted in a property battery in
  `domains/paleoclimate/tests/`, mirroring
  `astronomy/tests/genesis_properties.rs` and
  `terrain/tests/tectonic_properties.rs`.
- **The era-tick order and ice-integration order are save-format contracts**
  (ADR 0006), defined solely in the worldgen era loop (§4) so they are
  auditable in one file. Ice consumes **no RNG draws**, so no new seed-stream
  labels are introduced — the pin-isolation obligation is satisfied trivially
  (a deep-time pin must consume the same draws as the unpinned path; here that
  count is zero on both).
- **The zero-forcing pin is the null control** (`ForcingPin::Zero`,
  preregistered per metaplan §2): zero forcing → flat `g(t)` → no ice cycling →
  no ice-extent envelope, no shoreline migration, no refugia distinct from
  present habitability, and `frost-retreat` absent. A zero-forcing world's
  present is distributionally its own genesis snapshot. This is the seam the
  Year-4 epistemic layer turns into a reconstruction score.

## 10. Artifacts and the observable ending

- **A paleoclimate map** — a new CLI command (`hornvale paleo-map [--world
  PATH] [--out PNG]`, markdown to stdout plus optional PNG), following the
  `map`/`biome-map`/`settlement-map` pattern (`cli/src/main.rs`). It shows the
  fossil shoreline and a refugium over the present relief. One committed,
  drift-checked artifact.
- **A Deep Time section** in the seed-42 almanac (`windows/almanac`),
  drift-checked like the other almanac artifacts.
- **`recount`/`why`** on the world entity speaks "the frost retreated" with the
  glacial-maximum era, via the existing historiography tier-0 replay.

## 11. Scope boundaries (deliberate, not omissions)

- **The solid earth is fixed.** Plate tectonics, uplift, mountain building,
  glacial isostatic rebound, and erosion are all out of scope — elevation and
  plates are a genesis-time given from `domains/terrain`. Paleoclimate varies
  only temperature, ice, and eustatic sea level over that fixed relief. A
  fossil shoreline moves here because the sea rose and fell, not because the
  land rose. (Real fossil shorelines also record isostasy and tectonic uplift;
  those are a possible far-future campaign, noted, not built.)
- **Fully-spatial ice geometry** — a later refining tier (§7), not this
  campaign.
- **No consumer wiring beyond map + facts + `recount`.** Settlement's
  carrying-capacity consumption of the strata (via a composition-root
  `PaleoSummary`, the `SocietySummary` pattern) is Campaign 2. This campaign
  commits the facts and renders the map; it does not yet feed population.
- **No scope creep toward the epistemic layer** (metaplan §9 risk 4): this
  campaign builds a *true* past. Making it knowable and mis-knowable is Year 4.

## 12. Constitutional compliance

One new domain crate, kernel-only (ADR 0002); all cross-domain flow via
registered facts and (in Campaign 2) composition-root summaries (0003); no new
dependencies — the Milankovitch and ice models hand-rolled on std (0004); all
registries ordered, `BTreeMap`/`Vec` only (0005); no new RNG draws, so no new
stream labels; the era-tick and ice-integration order join the save-format
contracts (0006); seeds never retried (0007); coherent physical quantities get
newtypes, dimensionless ratios stay bare `f64` (0008); no ML at runtime (0009);
new predicates value-kind enforced (0010); any new Lab measurements are Rust
extractors (0011); config JSON (0012); DoD includes the book — a chronicle
entry, a `domains/paleoclimate` model card, and a freshness sweep (0013); the
null control and any comparative claims preregistered (0016).

## 13. Definition of Done

- `domains/paleoclimate` crate: the physics (§3), strata extraction (§5), fact
  commit (§6), provider seam (§7), typed quantities (§8), property battery (§9).
- Era loop wired at `windows/worldgen` (§4).
- `hornvale paleo-map` CLI command and its committed artifact (§10).
- Deep Time section in the seed-42 almanac (§10).
- `frost-retreat` fact recounted by historiography (§6, §10).
- The full gate passes: `cargo test --workspace`, `cargo fmt --check`,
  `cargo clippy --workspace --all-targets -- -D warnings`.
- Book: chronicle entry (Campaign 24), `domains/paleoclimate` model card
  (each parameter declared derived vs. approximated vs. drawn vs. authored),
  freshness sweep of stale chapters, concept-registry review.
- A one-page retrospective in `docs/retrospectives/` (decision 0020).
- Idea-registry: flip MAP-6 to `shipped` and repoint at this spec; flip the
  relevant SKY rows already drained by Firm Ground II if not yet done.

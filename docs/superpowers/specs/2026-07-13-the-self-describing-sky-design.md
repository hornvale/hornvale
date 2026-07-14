# The Self-Describing Sky — Design

**Date:** 2026-07-13
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-campaign-2-the-sky-design.md` (§5 deficit audit, SKY-15). The Constitution's "a world is a seed plus a ledger" governs the persistence posture; the astronomy model card governs every physics formula this spec commits.
**Provenance:** SKY-15 ("the ledger drops derived facts") is a `raw`, high-confidence registry row. Grounding it against the current code showed the row is **partly stale** — moon period/tide/inclination and the deep-time forcing parameters already ship as facts, and its headline motivation ("insolation is exactly what climate wants to read") is already satisfied: `windows/worldgen` recomputes `L/a²` and threads it into the temperature model. What remains is the row's *durable* value — **interrogability**. The ledger cannot answer "why is this world warm?" because the numbers behind the derivation (star mass, luminosity, orbit, insolation, the moons' masses and distances, the neighbors' classes and brightnesses) are computed and discarded. This campaign commits those numbers and builds the fact-reading `explain` verb (registry TOOL-1, fact-reading tier) that reads them back and narrates the derivation. An ideonomy pass (substitution × organon-construction, charting the fact set by naturalness × ledger-shape) surfaced the anchor-mass and habitable-zone gaps, the `explain = DAG ⋈ values` framing, and the entity-hood rule recorded in §7.

---

## 1. Goal

Make the sky's ledger self-describing, and prove it with a verb.

1. **Commit the dropped derived facts** — star mass, luminosity, habitable-zone bounds; anchor mass, orbit, insolation; each moon's mass, distance, angular size; each neighbor's class, distance, brightness, and position — so a world's ledger carries every value in its own derivation.
2. **Collapse the two insolation formulas into one** shared function the fact and `worldgen` both call.
3. **Build `hornvale explain --world w.json sky`** — a narrator that reads *only the ledger* and reconstructs the physical derivation chain, validating that the ledger is self-describing.

Non-goals (named to bound the campaign): the full TOOL-1 trace-replay tier (capturing stream draws down to the tie-breaking roll) is future work this campaign seeds, not delivers; `explain` targets only `sky` this campaign; climate's dataflow is not re-wired (it keeps reading insolation through `worldgen`, now via the shared formula).

## 2. The fact surface, part one — star & anchor (additive, functional)

Seven new **functional** facts committed on the world entity in `facts::genesis`, alongside the existing class/year/obliquity/forcing facts. All are `Value::Number`, `day: Some(0.0)`, provenance `"astronomy"`, quantized on commit like every numeric object.

```
predicate              value (source)                       kind
---------------------  -----------------------------------  ---------
star-mass-solar        system.star.mass                     drawn
star-luminosity-solar  system.star.luminosity  (M^3.5)      derived
hab-zone-inner-au      star.habitable_zone.inner  (0.95√L)  derived
hab-zone-outer-au      star.habitable_zone.outer  (1.37√L)  derived
anchor-mass-earth      system.anchor.mass                   drawn
anchor-orbit-au        system.anchor.orbit                  drawn (or pinned via year)
insolation-rel         insolation_rel(star, anchor)         derived
```

Purely additive: no existing fact is touched, no epoch. Each predicate is registered in `register_concepts` with `functional = true` and a one-line doc, and each constant carries a `type-audit: bare-ok(identifier-text)` tag matching the existing fact-name constants.

**`insolation-rel` is defined precisely** (so the self-describing number is unambiguous): the top-of-atmosphere stellar flux at the anchor's orbit relative to Earth's, `L / a²` with `L` in solar luminosities and `a` in AU (Earth = 1 by construction). It is a **global, annual-mean, genesis-time scalar** — it deliberately does *not* carry the seasonal (obliquity) or deep-time (eccentricity) variation the forcing parameters model. The `explain` narrator (§6) labels it as such.

## 3. The fact surface, part two — the shared insolation formula

Add to the astronomy domain (the layer that owns the physics):

```rust
/// Top-of-atmosphere stellar flux at the anchor's orbit, relative to
/// Earth's (L / a², Earth = 1). Global annual mean; genesis-time scalar.
pub fn insolation_rel(star: &Star, anchor: &Anchor) -> f64
```

`facts::genesis` commits its result as `insolation-rel`. `windows/worldgen/src/lib.rs` (the open-coded `luminosity / (orbit * orbit)` near line 377) is refactored to call it. Two copies of `L/a²` collapse to one — the committed fact and climate's input can no longer silently diverge. `worldgen`'s downstream dataflow is otherwise unchanged; this is a DRY refactor, not a re-wiring, and the composition root's stream-consumption order is untouched (the function draws nothing).

## 4. The fact surface, part three — moons (additive parallel)

Three new **non-functional** facts per moon, committed in the existing distance-sorted order, matching the shipped `moon-period`/`moon-tide`/`moon-inclination` parallel-list pattern:

```
predicate              value (source)                  kind
---------------------  ------------------------------  -------
moon-mass-lunar        moon.mass                       drawn
moon-distance-mm       moon.distance                   drawn
moon-angular-size-rel  moon.angular_diameter_rel       derived
```

Additive: the shipped moon facts are untouched, no epoch. Registered `functional = false`. Association across the parallel lists is by position (the distance sort is deterministic), exactly as the shipped moon facts already work — see §7 for why moons stay flat this campaign.

## 5. The fact surface, part four — neighbors become entities (epoch)

Neighbors are the one place the current ledger is not merely *incomplete* but *opaque*: a single `notable-neighbor` `Value::Text` blob (`"a warm yellow star at 12 light-years"`) with nothing queryable. They are redesigned from a clean slate into first-class entities, mirroring `settlement::genesis`.

**The shape.** In `facts::genesis`, for each neighbor (in the existing brightest-first order), mint an entity and commit facts *about that entity*:

```
predicate               value (source)              kind      functional
----------------------  --------------------------  --------  ----------
is-neighbor  (Flag)     true                        —         true
neighbor-class  (Text)  class_name(n.class)         drawn/pin  true
neighbor-distance-ly    n.distance                  drawn     true
neighbor-brightness-rel n.apparent_brightness (L/d²) derived  true
neighbor-declination-deg n.declination              drawn     true
neighbor-ra-deg         n.right_ascension           drawn     true
```

Discovery mirrors settlement's `is-settlement`: scan for the `is-neighbor` flag; brightest-first order is preserved by mint order. Neighbors get no `NAME` — naming the sky is SKY-12/MAP-3, out of scope.

**The epoch.** `notable-neighbor` is retired. Per the save-format contract, retirement uses no rename; the predicate is removed from `register_concepts` and `facts::genesis`, and the neighbor entities are the superseding representation. (The prose windows never read the fact — see blast radius — so no `/v2` suffix collision arises; the epoch is the removal itself, recorded in the chronicle and the decision of §7.)

**Blast radius (enumerated, regenerated in one commit).** The almanac, star-chart, orrery, and lab metrics all read neighbors from the **in-memory `system.neighbors` struct**, never from the `notable-neighbor` fact (`worldgen:2320`, `provider.rs`, `render.rs`, `lab/metrics.rs`). So **no rendered artifact drifts.** What changes: `world.json` ledger content; the `facts.rs` neighbor test (which asserts the Text blob count) is rewritten to assert the entity representation; any golden world-snapshot byte-baseline is re-frozen. CI's drift check confirms nothing else moved.

## 6. The `explain` verb — `windows/explain` + `hornvale explain`

A new standalone window (`windows/explain`, crate `hornvale-explain`) and CLI verb, sibling to `almanac` and `scout` per TOOL-1's framing.

**The core idea: `explain = derivation-DAG ⋈ ledger-values`.** The ledger stores *values*; it does not store whether a value was *rolled*, *forced by a formula*, or *pinned* — that classification is structural knowledge. The narrator owns the derivation DAG (the fixed topology of the sky: `mass → luminosity → zone`, `luminosity, orbit → insolation`, `anchor-mass, distance → moon period`, …, and which leaves are drawn); the ledger supplies the numbers at each node; the explanation is the join. This is precisely why the narrator **reads facts rather than re-deriving**: re-derivation would reproduce the values, but the provenance classification comes from code either way — so reading the committed facts both produces the narration *and* validates that the ledger is self-describing. If a required fact is absent, `explain` says so (a named, actionable gap), rather than silently recomputing it.

**Output.** `hornvale explain --world w.json sky` prints the sky's derivation chain as prose, each node tagged with its provenance and its value read from the ledger. Illustrative (values are examples):

> This world receives **1.24× Earth's sunlight** (insolation, global annual mean).
> Its star is a **yellow-white dwarf (F)** — mass **1.18 M☉** *(rolled)* — giving
> luminosity **1.79 L☉** *(derived, L = M³·⁵)* and a habitable zone of **1.27–1.83 AU**
> *(derived, 0.95√L–1.37√L)*. The anchor world — mass **1.4 M⊕** *(rolled)* — orbits at
> **1.20 AU** *(rolled)*, so insolation = 1.79 / 1.20² = **1.24** *(derived, L/a²)*, near
> the cold edge of the zone. Two moons *(rolled count)* raise a combined **1.6× tide**…

Values are labeled **"genesis / annual-mean"** where the underlying quantity actually varies with time, leaving a clean seam for TOOL-1's future time-aware replay tier. Pinned quantities are tagged *(pinned)* by cross-referencing the committed `scenario-pin` facts against the DAG's leaf nodes.

**Layering.** `windows/explain` depends on the kernel and on the astronomy domain's fact-name constants (to read predicates by name); it reads the `World` ledger and composes prose. It presents a domain, so a window is the correct layer — consistent with the facts.rs contract that domains never read their own committed facts. The CLI (`cli/`) re-exports it as the `explain` subcommand.

**Artifact.** One new drift-checked artifact: `book/src/gallery/explain-seed-42-sky.md`, generated by `hornvale explain --world <seed-42> sky` and added to CI's "Artifacts are current" step.

## 7. Determinism, save-format, and the entity-hood decision

- **Additive facts (star, anchor, moons)** change the seed-42 `world.json` bytes but nothing about generation order; determinism (same seed → byte-identical world) holds by construction because every new fact is a pure read of an already-computed quantity.
- **The neighbor epoch** is a deliberate representation change, recorded in the chronicle and as a decision. No stream draws move (positions and classes are already drawn; only their *committal* changes), so every existing world's *generation* is byte-preserved; only the serialized fact representation changes.
- **The entity-hood rule (new decision).** Collections become entities; singletons stay flat on the world entity. One star, one anchor ⇒ flat facts. Many neighbors, many moons ⇒ entities. This campaign applies the rule to neighbors (a clean-slate redesign) but **grandfathers moons flat** to avoid a second epoch on already-shipped moon facts (`moon-period`/`moon-tide`/`moon-inclination`). The debt is explicit: a future campaign may promote moons to entities under an epoch. The spec records this so the flat-moons/entity-neighbors asymmetry is a documented choice, not an accident.

## 8. Testing

- **Fact presence & value (per new predicate):** `facts.rs` unit tests assert each new fact is committed with the quantized value of its source, for both locked and spinning, single- and multi-moon worlds.
- **Neighbor entities:** assert one `is-neighbor` entity per generated neighbor, in brightest-first order, each carrying the five neighbor facts; assert `notable-neighbor` is absent.
- **Insolation single-source:** a test asserting `insolation_rel(star, anchor)` equals `worldgen`'s threaded insolation for a sample of seeds (the two can never diverge because they are one function).
- **`explain` reads only the ledger:** the narrator is exercised against a `World` whose ledger is present but whose `System` is *not* in scope — proving it needs nothing but facts. A golden test pins `explain-seed-42-sky.md`.
- **Determinism:** the existing `genesis_is_deterministic` / byte-identity tests extend to cover the new facts; a re-frozen golden world snapshot if one exists.
- **Provenance:** every new astronomy-committed fact (including those on neighbor entities) carries `"astronomy"` provenance.

## 9. Book & Definition of Done

- **Chronicle entry** (`book/src/chronicle/`) recording the campaign: the completed fact surface, the neighbor epoch, the `explain` verb.
- **Registry flips:** SKY-15 → `shipped`; TOOL-1 → `spec'd` (fact-reading tier delivered; the trace-replay tier remains open, pointed at this spec). Add a note on SKY-19's row that edge-of-zone is now answerable from the ledger (hab-zone facts + insolation).
- **Confidence Gradient re-score** of any bet in `open-questions.md` that SKY-15 or TOOL-1 sits on (decision 0030), as part of the freshness sweep.
- **Freshness sweep** of the sky chapter(s) and the concept-registry chapter (new predicates) so the book never lags merged reality.
- **Decision record** (`docs/decisions/`) for the entity-hood rule (collections → entities, singletons → flat) and the precise `insolation-rel` definition.
- **Retrospective** (`docs/retrospectives/`) — process lessons.
- **Artifacts** regenerated and drift-checked (the new `explain-seed-42-sky.md`; `world.json` fact content; CI's artifact step updated for the new command).

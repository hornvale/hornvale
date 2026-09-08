# The Staple D3B — decision ledger

Campaign: **The Staple D3B** — gradient sufficiency before specialization.
Branch: `campaign/the-staple-d4`. Decision block: **0926–0935**.

## #1 [G1] — Where should the specialization prerequisite live?

**Question:** Should the gradient-sufficiency investigation become D4, or
should it be a continuation of D3?

**Decision:** Treat it as **D3B**, a diagnostic continuation of D3, while
leaving the metaplan's D4 rung unchanged as “places specialize.” D3B measures
whether Hornvale computes materially distinct local value regimes before any
specialization role, function label, production flow, epoch, or census work is
attempted.

**Why:** D3's corrected diagnostic produced `C=N` across 17,740 standing
relations, but that result can mean saturation or insufficient upstream
variation; it does not show that specialization is impossible. The existing
code has four coarse subsistence labels, but subsistence is an exact function
of biome class and coast. D2 has two typed complementary stocks and local
bilateral exchange, but communities still share a common demand basket. These
are real differences in vocabulary and local inputs, not yet evidence of
comparative advantage. A D3 continuation is therefore the least disruptive
place to test the missing prerequisite while preserving D4's intended subject.

The SugarScape comparison supplies the structural lens, not an implementation
template: heterogeneous local opportunities, heterogeneous needs or
capabilities, local movement and limited visibility, production that changes
the field, and bilateral exchange can generate differentiated positions
without assigning occupations. The fantasy-world extension keeps the same
shape while admitting hazardous, rare, transformative, knowledge-gated, or
anomalous sources of value, including ruins, relics, magical phenomena, and
other Roadside Picnic/Stalker-like extraction opportunities.

**Alternatives discarded:**

- **Rename D4 to gradient sufficiency:** rejected because it would make the
  metaplan's specialization rung carry a prerequisite rather than its stated
  question and would blur diagnosis with mechanism.
- **Proceed directly to `Function` or portfolio labels:** rejected because
  labels could differ while the underlying value gradients remain uniform.
- **Add fantasy resources first:** rejected because it would make the first
  falsifier depend on an unmeasured new substrate and would turn a diagnostic
  campaign into an implementation campaign.
- **Treat D3's `C=N` as proof specialization is impossible:** rejected because
  the result is compatible with both measurement saturation and absent
  upstream variation.

**Ideonomy passes / overturns:** two passes, no overturn. Pass 1 used
negation, tree-finding, substitution, and a periodic-grid/state-machine
organon; it separated absent gradients from saturated observation and exposed
the need to test axis coverage. Pass 2 used cross-domain re-instantiation,
substitution, and combination; it connected the model to ecological niches,
local search, and exchange networks, and expanded the future fantasy axis set
without changing the D3B recommendation.

**Capture actions:**

- Keep D4 in the metaplan as the post-sufficiency specialization rung.
- Open D3B with a preregistered Task 0 falsifier.
- Compare candidate measures before choosing one; reject nonzero-only,
  label-only, and hub-dominated measures.
- Preserve two dead poles: no usable gradient, and measurement saturation.
- Capture fantasy/anomaly axes as deferred design space, not as present-day
  evidence.

## Verified substrate findings

The repository already contains richer non-agricultural value surfaces, but
they are not yet settlement-economic inputs:

- `windows/worldgen/src/energy.rs` derives seven subterranean energy sources,
  including detrital import, serpentinization, iron reduction, radiolysis,
  sulphide oxidation, methanogenesis, and geothermal supply. The field is
  consumed by subterranean habitat and species-fit readings.
- `domains/alchemy` derives material-quality vectors from commodities, rock,
  soil, ore grade, and life, then resolves reachable transformations through
  authored production processes. This is a capability/materiality surface,
  not a settlement stock or exchange flow.
- Surface settlement subsistence remains the coarse exact biome/coast mapping,
  and D2's live exchange remains a two-resource complementary basket.

This is evidence for an existing cross-domain substrate to inspect, not proof
that settlements can currently exploit it. D3B must distinguish “the world
contains a differentiated source” from “a community can access, transform,
need, and exchange that source.”

## #2 [G2] — What unit and denominator should Task 0 count?

**Question:** Should gradient sufficiency be counted over standing tribute
relations, settlements, or another existing population?

**Decision:** Count one **live `BakeOccupation` record at `History::now`** as
the Task 0 unit. Pool those units over the fixed seed roster, with the
denominator supplied by the existing `census(h).alive_at_now` count. Use the
occupation's existing site and people fields to join source and community
evidence. Do not use `tribute_relations_at_now` as the denominator.

**Why:** Gradient sufficiency is a property of a community's local
opportunity/capability position, so it is node-local. A tribute relation is an
edge and can be absent precisely where a site still has a meaningful value
gradient. `History` already exposes live occupation records, and the existing
census already names their live population; choosing that surface adds no new
identity or relation denominator. The first falsifier can therefore be
read-only and can measure source variation even before D3's return flow or D4
specialization exists.

**Vacuity checks:** assert the pooled denominator is non-zero; assert every
counted unit is live at `History::now`; assert the fixed-roster aggregation
reports empty seeds explicitly rather than silently dropping them; and assert
that the live-record identity/site join is total. If the production surface
ever permits two live records to carry one community identity, report that as
an integrity branch rather than silently treating the duplicate as a second
community.

**Alternatives discarded:**

- **Standing tribute relations:** rejected as edge-biased and unable to see
  un-subordinated or relation-free communities.
- **Emitted settlement entities alone:** rejected because they omit live bake
  state and do not carry the full node-local dynamics identity needed for a
  source/capability join.
- **All historical occupation records:** rejected because ended occupations
  would mix past opportunity regimes with the standing world D3B is meant to
  test.

**Ideonomy passes / overturns:** one focused measure-design pass, no overturn.
Dimension identification separated source variation, capability variation,
need variation, connectivity, and side effects; the map/scale organons made
the node-versus-edge and local-versus-pooled distinction visible; cross-domain
re-instantiation in ecology and network flow confirmed that niche contrast is
node-local while exchange access is an edge-derived secondary measure.

**Capture actions:** the candidate-measure comparison now uses live occupation
records as its common unit and `alive_at_now` as its denominator. Any future
fantasy source must first prove a total join onto that unit before it can count
as D3B evidence.

## #3 [G2] — What shape should the gradient-sufficiency falsifier take?

**Question:** Should Task 0 choose one scalar diversity metric, or compare
source variation with the variation preserved by the current settlement
projection?

**Decision:** Use a **paired instrument** over the same live occupation units:

1. **Source-support readout:** the materially occupied, mechanism-backed local
   opportunity axes available at each unit's site. This asks whether the world
   supplies distinct local regimes before roles or portfolios are named.
2. **Projection readout:** the current settlement/economic value view that can
   actually consume those axes—subsistence, stocks, access, relations, or
   other existing projections. This asks whether current dynamics preserve
   the distinctions or collapse them into one ranking/portfolio.

The falsifier does not select a single scalar in advance. It reports both
readouts with equal unit weighting and a branch table:

```text
source support has no materially occupied bands
    -> dead pole 1: no usable gradient exists

source support varies, but the current projection has one ranking/portfolio
    -> dead pole 2: projection/measurement saturation

source support varies, but no community-specific capability or need can use it
    -> qualified capability gap; gradient exists, specialization pressure does not

source support and projected positions both vary across multiple regimes
    -> gradient sufficiency established; D4 may study emergence
```

**Why:** A scalar over pooled values can pass because values are merely
nonzero, because labels differ, or because one hub contributes most of the
mass. A source/projection pair makes those failures visible. The source layer
can show genuine ecological, material, route, or anomalous opportunity
variation even when the current economy ignores it; the projection layer can
show whether the existing model turns variation into different effective
positions. Equal weighting by live unit prevents a high-population hub from
deciding the whole result. Axis bands and thresholds must be frozen from the
verified code semantics before the readout, not tuned after seeing the output.

**Alternatives discarded:**

- **One scalar occupied-band count:** rejected because it cannot distinguish
  source absence from projection saturation.
- **Categorical subsistence diversity:** rejected as label variation; today's
  subsistence is an exact biome/coast mapping and does not establish
  comparative advantage.
- **Portfolio divergence alone:** rejected because identical rankings can hide
  materially different magnitudes, while different labels can be decorative.
- **Pairwise pooled distance without equal-unit normalization:** rejected
  because a dominant hub or large population can create a false gradient.
- **Assigning `Function` labels first:** rejected because it measures the
  proposed reading rather than the variation that would justify it.

**Ideonomy passes / overturns:** one measure-selection pass, no overturn.
Dimension identification separated materiality, connectivity, cyclicity,
side effects, and polarity; the map organon exposed local regimes and border
zones; the scale organon exposed the two collapse extremes. Cross-domain
re-instantiation in ecology treated source support as niche opportunity and in
network flow treated projection as reachable effective capacity. Both
translations supported a paired source-versus-realization instrument rather
than a single diversity score.

**Capture actions:** the Task 0 design must enumerate the exact source and
projection axes from current code, freeze their bands and thresholds, and
retain the four-way branch table. No role, function, or portfolio label is
permitted in the instrument.

## Verified axis roster for Task 0

The current code audit classifies candidate axes as follows:

### Eligible source-support axes

- **Per-people local capacity:** `Bake::eff_capacity` is the existing
  settlement-affecting value of a vertex to a particular people and era. It is
  a real source axis, not a categorical label, and already controls founding,
  relocation, and growth.
- **Water and fertility opportunity:** river proximity, biome class/fertility,
  moisture, coast, and the seasonal harvest curve already feed settlement
  production or carrying capacity. These must be retained only where the
  readout follows the actual consuming path rather than a decorative
  re-derivation.
- **Local connectivity:** the connection graph and the one-hop adjacency used
  by D2 exchange can measure whether a source is locally reachable. It is a
  transmission axis, not evidence of source value by itself.
- **Occupied site and people:** `Occupation::site` and `Occupation::people`
  are the stable join fields for all source/projection observations.

### Projection axes eligible with an observation seam

- **Typed subsistence stock:** `SubsistenceInventory` A/B, same-typed demand,
  and shortfall are real live values, but are not part of emitted occupation
  records. A later Task 0 implementation may need a zero-impact diagnostic
  seam; the design must not pretend the values are available through the
  ledger today.
- **Exchange realization:** local exchange adjacency, proposals, deliveries,
  refusals, and conservation are eligible as a projection of source/need
  mismatch. They do not replace the source-support readout.
- **Stores and relation structure:** stores, assessment, patronage, and local
  relation connectivity are eligible as downstream transmission/context
  axes, not as the source gradient itself.

### Deferred until a total settlement join is proven

- **Alchemy:** commodity/rock/soil/life substrates, quality vectors, and
  reachable productions already provide rich material gradients, but the
  current path is a material study and does not automatically make those
  values available to surface settlement stocks or exchange.
- **Underworld energy:** the seven `EnergySource` mechanisms are genuine
  fantasy-relevant opportunity axes, but they currently feed subterranean
  habitat/species fit rather than surface settlement economics.

### Rejected as Task 0 evidence

- `Function` and `Notability`, because the current production path hardcodes or
  saturates them.
- Categorical subsistence labels, because they are exact biome/coast labels
  and can differ without comparative advantage.
- Textual `settlement_site_concepts`, because lexical variation is a reading
  surface, not an economic value gradient.
- Population alone, because magnitude without opportunity composition cannot
  distinguish specialization from carrying-capacity scale.

## Follow-ups

- Verify the current ecology, stock, need, capability, movement, relation, and
  exchange identifiers and denominators before drafting Task 0.
- Build a candidate-measure comparison covering opportunity contrast,
  occupied value bands, portfolio divergence, comparative advantage, and
  exchange-access asymmetry.
- Use ideonomy with explicit cross-domain re-instantiation again when the
  Task 0 measure is selected.
- Do not author `Function` labels or a specialization implementation until
  D3B clears gradient sufficiency.

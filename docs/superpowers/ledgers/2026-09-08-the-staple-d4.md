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

## #4 [G2] — What counts as an occupied source regime?

**Question:** Should D3B invent distribution cuts from the probe output, use
one global diversity statistic, or reuse semantic bars already present in the
world?

**Decision:** Use a preregistered **source-regime readout** made from existing
field semantics. Do not derive cuts from the observed distribution, and do not
let a nonzero value, a changed label, or a single exceptional site establish a
regime.

The first diagnostic comparison uses these candidate bands:

| source axis | occupied bands | code meaning |
| --- | --- | --- |
| local surplus | `<= 0.4`, `(0.4, 0.6]`, `> 0.6` | existing culture structure gates for shaman/artisan and the high-surplus gate |
| river access | `0`, `(0, 1)`, `1` | existing `river_proximity` endpoint/interior semantics, with `RIVER_REACH = 3` defining the field |
| local capacity | `< 150`, `[150, 200)`, `>= 200` | existing hamlet ceiling and longhouse floor, applied to capacity rather than realized population |

These are measurement bands, not new domain constants. A field remains
continuous inside a band; the bands only make the preregistered readout
auditable. The categorical biome/coast subsistence function remains excluded:
it is a label-producing lookup, not a comparative-value surface.

**Materiality guard:** source support is not sufficient merely because one
axis has two occupied bands. The positive branch requires at least two
independent eligible axes to occupy at least two bands each, and at least two
joint source signatures to be occupied by live units. The readout reports the
per-axis counts and joint signatures rather than collapsing them into one
diversity scalar. Each live occupation contributes one unit, so a large hub
cannot gain extra weight from population or degree. A singleton signature is
reported but cannot by itself carry the positive interpretation; the per-band
counts remain visible for the G3 review rather than being hidden by an
aggregate.

This is intentionally a conservative **sufficiency guard**, not a claim that
two axes are the complete ecology of a fantasy world. It tests whether the
current world has enough joined, occupied contrast to justify asking how
specialization emerges. Future anomaly, material-transformation, subterranean,
hazard, knowledge, or access axes must first prove a total join to the live
occupation unit and pass the same nonzero/label/hub checks.

**Dead-pole separation:**

- If no eligible axis has more than one occupied semantic band, the result is
  **dead pole 1: no materially usable gradient exists**.
- If source regimes pass the guard but the projected settlement/economic
  observation gives every live unit the same effective ranking or portfolio,
  the result is **dead pole 2: projection/measurement saturation**.
- If source regimes pass but the current world has no joined capability, need,
  access, or exchange path that can act on them, report a **capability gap**;
  this is not saturation and does not license role labels.
- Only varied source regimes plus varied, causally joined projected positions
  clear D3B and open D4's emergence question.

The projection-side signature is not frozen by this ruling. Current typed
stocks and exchange are live dynamics but are not fully present on the emitted
occupation record, so the next design pass must choose an observation seam
before claiming that projection is uniform. That seam must be zero-impact and
must be tested by cross-domain re-instantiation, just as D3's return witness
was.

**Alternatives discarded:**

- **Quantile bands:** rejected because every sufficiently populated world could
  manufacture occupied bands even when the absolute field is saturated.
- **Entropy or one scalar distance:** rejected because it hides which source
  axis varied, permits a dominant hub to decide the result, and cannot separate
  source absence from projection collapse.
- **Biome/coast diversity:** rejected as label diversity and exact lookup
  variation, not materially distinct opportunity.
- **Existing role thresholds as a specialization test:** rejected because
  `Function`/role gates are downstream projections; reusing them would measure
  the answer rather than its prerequisite.

**Ideonomy passes / overturns:** one pass (seed 4099), using abstraction-lift,
cross-domain re-instantiation, and tree-finding with spectrum/cycle organons.
The lift treated D3B as a distributed regime-detection problem rather than an
occupation classifier; ecology, network flow, and temporal-cycle readings all
supported separate source and projection layers. No overturn. The pass added
the explicit singleton/hub guard and the requirement that a positive result
span more than one source axis.

**Capture actions:** retain existing semantic bars as the only initial band
anchors; add no fantasy source to the current evidence set without a total
occupation join; next choose and test the projection-side signature; keep the
Task 0 falsifier read-only and pre-role.

## #5 [G2] — What can show projection saturation?

**Question:** Can the existing emitted history and whole-bake exchange census
show whether source variation survives into local economic positions?

**Decision:** Not by themselves. The current public `History` surface carries
live occupation records, tribute relations, and the D3 return sidecar. D2's
`ExchangeCensus` is explicitly a whole-bake aggregate, while the typed
`SubsistenceInventory`, per-community shortfalls, delivery attempts, and
exchange outcomes remain private bake state. The emitted occupation record has
peak population, but not the typed stock or local exchange state. A global
exchange count would therefore be unable to distinguish varied communities
from one dominant hub or a uniform population-wide treatment.

The valid projection seam is consequently a **zero-impact, per-live-unit
diagnostic witness**, analogous to D3's return witness, captured from the
existing D2 path rather than reconstructed from labels. Its candidate
components are:

- typed stock coverage for A and B against that unit's typed demand;
- typed shortfall exposure over the exchange phases;
- local delivery/attempt outcomes, including impossible, refused, partial, and
  settled requests; and
- the unit's actual one-hop exchange access, kept separate from source value.

The witness should be phase-integrated or explicitly phase-indexed. A single
closing stock is not enough: the Granary's seasonal curve makes transient
surplus and persistent access different phenomena. The projection signature
must be derived from these continuous realized quantities before any category
or portfolio name is considered.

**Saturation test candidate:** compare the source-regime signature and the
per-unit projection signature on the same live occupation denominator. The
measurement must report both the number of distinct projected signatures and
the per-signature counts. The dead-pole-2 claim is permitted only when the
source guard passes but the projection readout collapses to one effective
signature/ranking under the preregistered comparison. If the source varies and
the projection varies, but the variation is not aligned with capability,
need, or reachable exchange, report the capability-gap branch instead.

**Vacuity checks:** a diagnostic witness must prove total one-to-one joining
from live `BakeOccupation.community` to the per-community D2 snapshot; report
missing and duplicate joins; reject an all-zero witness caused by disabled
exchange treatment as “no projection” rather than “uniform projection”; and
retain conservation and non-negativity checks. Aggregate `ExchangeCensus`
values may accompany the witness as a checksum, but cannot be its denominator
or verdict.

**Alternatives discarded:**

- **`History::exchange` alone:** rejected as an aggregate that loses local
  identity and can be dominated by a hub.
- **Peak population or stores alone:** rejected because both are magnitude
  summaries and do not identify typed opportunity or exchange direction.
- **D3 return classes/bands:** rejected as an already-saturated downstream
  diagnostic; reusing its labels would make D3B circular.
- **Recomputing stocks from biome labels:** rejected because it would measure
  a source proxy, not the realized projection, and would erase exchange and
  seasonal effects.

**Ideonomy passes / overturns:** one pass (seed 4100), using dimension
identification, negation, and abstraction-lift with state-machine/notation
organons. It identified the anti-seam—one closing snapshot that falsely says
“uniform”—and the required lifecycle of phase production, exchange, consume,
and carry-forward. No overturn.

**Capture actions:** do not draft a projection metric from the current
aggregate census; next compare phase-integrated shortfall, typed-coverage
vectors, and exchange-access asymmetry as candidate signatures; any eventual
sidecar must remain diagnostic-only until Nathan's G3 spec review.

## #6 [G2] — Which projection measure survives the candidate comparison?

**Question:** Among realized shortfall, typed stock coverage, and exchange
access, which measure can expose projection saturation without simply copying
the source field?

**Decision:** Make the primary projection signature the **phase-integrated,
typed coverage/shortfall vector**, with exchange-access asymmetry as a required
companion readout rather than folding it into one scalar.

The primary vector is two-dimensional by construction: A and B are compared
against the same community's fixed complementary demand, whose existing share
is `0.5`/`0.5`. Report both coverage and shortfall (or their exact complement)
per typed resource, averaged over the existing twelve phase steps. This keeps
three distinctions visible:

- balanced adequacy (`A` and `B` both covered);
- typed comparative imbalance (one covered, the other short); and
- persistent deprivation (both short).

The values are normalized by that unit's demand, not by total world stock,
population share, or patron degree. The existing D2 pressure adapter's
`[1, 2]` shortfall multiplier is a useful boundedness check, but it is not a
new D3B threshold and does not become a specialization score.

The companion access readout records the same unit's local request and delivery
outcomes by type: proposed, accepted, settled, partial, refused, and
impossible. It answers a different question—whether a deficit can be reached
through the one-hop conductance-positive network. Keeping it separate avoids
calling an isolated community's zero deliveries a production specialty, or a
hub's many deliveries a universal comparative advantage.

| candidate | useful signal | failure mode | ruling |
| --- | --- | --- | --- |
| phase-integrated typed shortfall | persistent realized need mismatch, including seasonal recovery | can hide whether A/B imbalance or total scarcity caused it if scalarized | retain as primary vector |
| typed coverage vector | preserves A/B balance and magnitude relative to demand | closing snapshot is transient; must be phase-integrated | retain with shortfall as the same primary vector |
| exchange-access asymmetry | shows whether local network can transmit a missing type | zero demand, isolation, or hub degree can masquerade as specialization | retain as companion, never alone |
| whole-bake exchange totals | confirms treatment conservation and overall activity | no community identity; cannot detect local saturation | reject as projection measure |
| population/stores/tribute totals | shows scale or extraction | magnitude and power are not typed opportunity | reject as projection measure |

**Why no scalar:** A scalar shortfall would merge the two typed deficiencies;
a scalar coverage would turn a balanced half-basket into the same reading as a
single-resource windfall. D3B needs to know whether the current projection
preserves a local portfolio difference, so the vector remains the observation
unit. Any later distance or ranking is a secondary analysis over the emitted
vector, with its dead-pole comparison preregistered before results are seen.

**Source/projection separation:** production inputs may contribute to the
source-support vector, but the projection vector is read after production,
exchange, and consumption from the live typed state. Recomputing it from
biome, coast, or the harvest curve alone is invalid. Conversely, the access
companion is not source value: it measures the path by which a deficit can be
relieved.

**Vacuity and saturation guards:**

- disabled exchange treatment must be reported as an uninstantiated
  projection, not as universal equality;
- a unit with zero demand cannot contribute a coverage ranking;
- every live unit must have a phase-complete A/B observation or be counted in
  an explicit missing-observation branch;
- conservation and non-negativity remain companion invariants;
- projection equality must be evaluated over the vector and its occupied
  signatures, never over a downstream `Function`, `Subsistence`, or role label.

**Alternatives discarded:**

- **Shortfall scalar alone:** rejected because it destroys typed portfolio
  shape.
- **Coverage at `now` only:** rejected because the existing twelve-phase
  seasonal loop makes a transient store snapshot non-representative.
- **Access asymmetry alone:** rejected because topology can explain the result
  without any local opportunity contrast, while high-degree hubs can dominate.
- **A derived portfolio label:** deferred until D3B succeeds; naming the
  vector now would turn the falsifier into D4 implementation.

**Ideonomy passes / overturns:** one pass (seed 4101), using
dimension-identification, tree-finding, and substitution with state-machine
and atlas organons. The lifecycle reading rejected the closing-snapshot
shortcut; the atlas separated material adequacy, economic realization, and
network access; no overturn. It reinforced keeping typed coverage and
shortfall together while leaving exchange access as a companion axis.

**Capture actions:** the next pass must specify how vector signatures are
compared without quantile tuning, and must test the source/projection join
across domains. No implementation, census, epoch, or `Function` conversion is
licensed by this ruling.

## #7 [G2] — How are projection vectors compared without invented bands?

**Question:** How can D3B distinguish genuine projected regimes from tiny
floating-point differences, while preserving variation that a coarse banding
would hide?

**Decision:** Keep two layers in the diagnostic:

1. the **raw continuous A/B vector**, retained for the readout and any later
   review; and
2. a small **structural signature** derived only from existing endpoint and
   ordering semantics, used for the dead-pole branch.

For each typed coverage component, the structural band is `0`, `(0, 1)`, or
`1`, using the same clamped coverage domain already established by D2's
shortfall calculation. The signature also records whether A is below, equal
to, or above B after phase integration. This distinguishes balanced adequacy,
typed imbalance, and persistent absence without assigning names such as
farmer, fisher, or trader.

The raw vector remains authoritative for seeing within-band spread. Where a
committed artifact needs a stable equality comparison, it uses the repository's
existing eight-significant-digit `quantize` boundary; quantization is never
introduced into the compute path or used as a new world threshold.

The branch table is therefore:

```text
source support has no occupied multi-band contrast
    -> dead pole 1: no usable source gradient

source support varies, projection vectors are equal at the committed
observation precision, and every structural signature is the same
    -> dead pole 2: realized projection collapse

source support varies, structural signatures are all the same, but raw
projection vectors vary at committed precision
    -> measurement saturation: report the hidden spread; D3B does not pass
       until a finer defensible signature is chosen

source support varies and multiple structurally or materially distinct
projection vectors occur across joined units
    -> projection variation exists; test capability/need/access alignment
```

This explicitly distinguishes a uniform world from an observation instrument
whose bands are too coarse. It also prevents “every value is nonzero” from
passing: nonzero interior values share a structural band until their raw
variation is independently shown to be material at the committed boundary.

**Cross-domain re-instantiation:**

- In ecology, source regime is habitat opportunity; projection is realized
  intake of two required nutrients. Identical intake portfolios despite varied
  habitats are projection collapse; varied intake with no movement path is a
  capability/access gap.
- In a local market, source regime is local production possibility; projection
  is the typed basket actually covered after bilateral trade. A high-volume
  market cannot erase the per-community vector by being counted once per edge.
- In network routing, source regime is local capacity; projection is delivered
  flow by type over the reachable neighborhood. A route that is merely
  connected but never delivers remains access failure, not specialization.

All three translations preserve the same invariant: source variation is
measured before realization, realization is measured per node, and topology
explains transmission rather than value. No cross-domain translation
overturned the paired instrument.

**Alternatives discarded:**

- **Quantile or equal-width output bins:** rejected because their occupancy is
  distribution-dependent and can manufacture variation or hide it.
- **Raw `f64` equality only:** rejected because libm/last-bit noise could create
  false regimes; emitted equality must use the existing quantization boundary.
- **Quantize during computation:** rejected by the kernel's emit-only contract.
- **Typed-order signature alone:** rejected because it loses magnitude and
  confuses a tiny imbalance with persistent deprivation.
- **A new tolerance constant:** rejected until a domain-specific semantic bar
  exists; D3B should not smuggle a specialization threshold into a diagnostic.

**Vacuity checks:** require a nonempty, phase-complete joined denominator;
report missing and duplicate communities; exclude zero-demand units; retain
the raw vector alongside every structural signature; and make the
measurement-saturation branch observable rather than silently promoting it to
success.

**Ideonomy passes / overturns:** one pass (seed 4102), using negation,
abstraction-lift, and organon-construction with tree/map organons. The
negation exposed “same signature, hidden raw spread” as a separate anti-pole;
ecology, market, and routing re-instantiations preserved the node-local
invariant. No overturn.

**Capture actions:** the Task 0 design can now state a non-quantile comparison
rule; next freeze the full falsifier branch table and its cross-seed pooling
unit before G3 packaging.

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

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

## #8 [G2] — How should seeds and underpowered worlds be pooled?

**Question:** Should D3B decide from one pooled numerator/denominator, or can
large seeds and large communities conceal a dead pole in smaller worlds?

**Decision:** Produce both a per-seed table and pooled totals, but make the
verdict **per nonempty seed**, not pooled-only. The fixed seed roster remains
the campaign's denominator roster. For each seed, count live occupation units
against that seed's existing `census(h).alive_at_now`; report empty seeds
explicitly. Pooling is a descriptive secondary view and must not allow one
seed's population to decide another seed's verdict.

For each nonempty seed `s`, the readout records:

- `N_s = census(h).alive_at_now`;
- the count of occupied bands on every eligible source axis;
- the number and sizes of joint source signatures;
- the number and sizes of structural projection signatures;
- the number of phase-complete projection witnesses; and
- missing, duplicate, zero-demand, disabled-treatment, and conservation
  branches.

The seed is **underpowered**, not dead, when `N_s < 2` or when fewer than two
phase-complete joined units remain after vacuity exclusions. A one-community
world cannot prove either gradient sufficiency or saturation. It remains in
the fixed roster, is reported, and prevents an all-seed “success” verdict until
the campaign defines a deliberate treatment for underpowered seeds.

Among adequately powered seeds:

```text
no eligible source axis has >1 occupied band on every adequate seed
    -> dead pole 1: no usable gradient exists

source guard passes, but every adequate seed has one committed projection
vector/signature after the measurement-saturation branch is resolved
    -> dead pole 2: projection/measurement saturation

source and projection guards pass on every adequate seed
    -> D3B clears gradient sufficiency; D4 may ask how portfolios emerge

some adequate seeds pass and some fail, or any seed is underpowered/missing
    -> MIXED / underpowered; no specialization design and no D4 claim
```

The exact first line is evaluated with the seed table, not with pooled counts:
one varied seed cannot rescue nineteen flat seeds. The pooled table remains
useful for describing total occupied regimes and for comparing the campaign
with D3's pooled `N`, but it is not a license to average away a dead pole.

**Why this is the existing denominator rather than a new one:** `alive_at_now`
already names the live occupation population used by the bake census. The
source and projection witnesses join to those same live records. No population
weight, tribute-edge count, exchange-attempt count, or hub degree enters the
verdict denominator.

**Cross-domain re-instantiation:**

- In an ecological survey, one large habitat cannot erase several sampled
  habitats with no niche contrast; empty and undersampled plots remain
  explicit.
- In a market panel, national trade volume cannot prove local specialization
  when every town has the same basket; town-level panels must clear first.
- In a routing graph, a large connected component cannot make isolated or
  singleton components evidence of differentiated flow; component-level
  adequacy is reported before network pooling.

These translations preserve the anti-hub and anti-vacuity rule. No overturn.

**Alternatives discarded:**

- **Pooled-only verdict:** rejected because seed size and hub composition would
  weight the answer invisibly.
- **Treat empty seeds as zeros:** rejected because no live unit is evidence of
  a flat value field.
- **Treat one-unit seeds as success:** rejected because a single point cannot
  occupy a contrast or demonstrate projection saturation.
- **Drop underpowered seeds silently:** rejected because it converts missing
  evidence into a positive campaign result.

**Ideonomy passes / overturns:** one pass (seed 4103), using
organon-construction, negation, and cross-domain re-instantiation with
timeline/graph organons. The timeline exposed seed adequacy as a precondition
rather than an outcome; ecological, market, and routing graphs all rejected
pooled-only inference. No overturn.

**Capture actions:** freeze the per-seed table and explicit underpowered branch
in the Task 0 design; retain pooled totals only as a descriptive companion;
then assemble the G3 package without writing implementation code.

## #9 [G2] — Self-review before G3 packaging

The design was re-read against the campaign brief, the D3 result, the
SugarScape material, and the live code rather than against the hoped-for D4
outcome.

**Challenge: are the source bands secretly specialization labels?** No. The
initial bands are surplus gates, river-proximity endpoints, and capacity bars
already present in code. They describe opportunity scale or access; they do
not name an occupation or portfolio. Biome/coast subsistence labels remain
explicitly excluded.

**Challenge: can a single fantastic substrate rescue a flat present world?**
No. Alchemy, underworld energy, hazards, relics, and anomalous extraction are
deferred until they can join a live occupation and its actual consuming path.
Their existence is recorded as design space, not counted as present economic
variation.

**Challenge: can D3B repeat D3's saturation error?** The paired instrument,
per-unit projection witness, raw-vector retention, structural signatures, and
measurement-saturation branch prevent a universal diagnostic class from being
mistaken for specialization. Whole-bake exchange totals are explicitly
disallowed as the verdict instrument.

**Challenge: can pooling hide a dead world?** No. The verdict is per adequate
seed, with empty, singleton, missing, duplicate, disabled-treatment, and
conservation branches visible. Pooled totals cannot rescue a seed.

**Challenge: is “two independent axes” an arbitrary implementation target?**
It is an authored conservative sufficiency guard, not a claim about the final
world ontology. The reason is falsificatory: one axis can be a uniform rescale
or a single decorative contrast, while two joined axes test whether local
regimes have enough dimensionality to support later portfolios. The criterion
is exposed for G3 review rather than hidden as an implementation detail.

**Challenge: has the brainstorm accidentally authorized a dynamics rung?** No.
The only proposed future seam is a zero-impact diagnostic sidecar. No stock
flow, epoch label, census re-baseline, history pin conversion, `Function`
state, or specialization implementation is authorized before the written
spec is approved and the falsifier establishes sufficiency.

**G2 result:** the brainstorm is converged enough for G3 packaging. The open
items are now spec questions, not unresolved direction questions: exact
sidecar shape, exact source-axis join path, and the implementation/test
acceptance checks. Per campaign policy, those belong in the written spec and
the G3 Nathan review; they are not to be silently decided in code.

**Ideonomy passes / overturns:** the prior passes for source bands, projection
seam, vector comparison, and seed pooling were re-read as a combined graph;
no new overturn. The combined review preserved the two dead poles and added
no new axis or role vocabulary.

**Capture actions:** package G3 with the four-way falsifier, the per-seed
denominator table, the source/projection witness contract, SugarScape's
heterogeneity-and-exchange rationale, and the fantasy-axis deferral. Stop
before authoring the spec until Nathan reviews the G3 package.

## #10 [G3] — Design approval

Nathan approved the D3B design draft on 2026-09-08. Proceed to implementation
planning and execution under the approved scope: diagnostic witness and Task 0
falsifier only; no specialization labels, persistent portfolios, dynamics rung,
epoch, or census work.

## Follow-ups

## #11 [Implementation] — Task 1 witness accepted

Task 1 added the typed, per-community D2 diagnostic witness and its public
read-only History accessor. The independent review found no issues. The
witness remains outside save emission and exchange behavior; the timing
follow-up was regenerated and committed with the gate evidence.

**Evidence:** implementation `78f9ecad6`; timing follow-up `3df3c02e7`.

## #12 [Implementation] — Task 2 signatures accepted after provenance correction

Task 2 added pure source-band and typed projection-signature helpers. The
first review found one important semantic-drift risk: capacity thresholds were
retyped instead of anchored to the existing hamlet bar. The correction now
uses `HAMLET_POPULATION_CEILING` and a documented Hidage-style mirror for the
private longhouse floor. The re-review approved the correction with no
remaining findings.

**Evidence:** implementation `f18ff7cf6`; correction `3b790782f`.

## #13 [Ruling] — Task 3 requires the same-run History seam

The first Task 3 probe attempt was blocked before editing. The enabled
`build_world_with_exchange_treatment` path computed `History` and then
discarded it after emission, returning only `World` and aggregate
`ExchangeCensus`; the existing `history_for` path hardcodes disabled exchange.
Reconstructing the witness from labels or aggregate counts would violate the
approved falsifier. Amend Task 3 to expose the same-run `History` through the
existing `ExchangeTreatmentBuild` boundary as a read-only diagnostic seam.
This remains zero-impact to simulation and save emission: the returned clone
is an observation of the already-computed bake, and the probe alone consumes
the sidecar.

**Rejected alternative:** derive per-community values from emitted labels or
whole-bake totals. Rejected because it loses typed local identity and repeats
D3's diagnostic saturation risk.

## #14 [Ruling] — Gate roster is part of the probe boundary

Task 3's focused tests and clippy passed, but `gate-commit` refused the new
probe because its one real world build was absent from
`cli/tests/fixtures/world-build-sites.tsv`. Add the required `identity:1`
roster row as mechanical gate accounting. This is not a census fixture and
does not authorize running the ignored fixed-roster report.

- Verify the current ecology, stock, need, capability, movement, relation, and
  exchange identifiers and denominators before drafting Task 0.
- Build a candidate-measure comparison covering opportunity contrast,
  occupied value bands, portfolio divergence, comparative advantage, and
  exchange-access asymmetry.
- Use ideonomy with explicit cross-domain re-instantiation again when the
  Task 0 measure is selected.
- Do not author `Function` labels or a specialization implementation until
  D3B clears gradient sufficiency.

## #15 [G5] — Task 3 fixes the probe as two deterministic live-unit joins

**Question:** How should the fixed-roster probe recover source support and D2
realization without reconstructing either from emitted role labels or allowing
pooled activity to rescue a flat seed?

**Decision:** For each seed, sort the live `BakeOccupation` population by
`BakeId`, require its count to equal `census(history).alive_at_now`, and make
two independent one-to-one joins on `(community, site)`:

```text
L_s = sorted live BakeOccupation records
S_s : BakeId -> (site, raw surplus, raw river access, raw present capacity)
P_s : BakeId -> (site, phase count, raw A/B coverage and shortfall,
                 typed attempts/proposed/accepted/settled/partial/refused/impossible)

valid unit c iff c occurs exactly once in L_s, S_s, and P_s; sites agree;
                demand is positive; phases are complete; vectors are finite
                and non-negative; access statuses cohere.
```

`S_s` reads fertility-times-moisture and `river_proximity` directly, before
any `Subsistence`, `Function`, `Notability`, or portfolio derivation. Its
capacity field follows the existing Hidage public reproduction of the bake's
present-era per-people capacity, including the same settling-kind order,
biome affinity, habitat realm, and delve-seating multiplier. The Hidage-known
temporal shadow remains visible: a living site whose present capacity is zero
occupies the below-hamlet band; it is not dropped or replaced by its last
historical era. `P_s` consumes the same-run enabled `History` seam approved in
#13. The fixed complementary demand is positive on both typed components, and
an absent witness remains a missing join rather than fabricated zero coverage.

The reducer retains every joined raw source and projection vector beside its
structural signatures and typed access counters. Equality of projection
vectors is tested only at the existing eight-significant-digit committed
boundary; raw values never feed back into the simulation. A positive source
guard requires two varied axes and at least two source signatures occupied by
more than one unit each, so a singleton outlier cannot establish sufficiency.
Verdicts are computed seed by seed. Pooled signatures, raw spread, and access
totals are descriptive only; a mixed roster remains mixed even when its pooled
surface varies.

**Why:** This is the smallest observation that preserves the approved
source-before-realization separation and uses the same live denominator on
both sides. Missing, duplicate, orphan, site-mismatched, disabled,
zero-demand, phase-incomplete, non-finite/non-negative, incoherent-access, and
non-conserving states each remain visible branches. Uniform committed vectors
and hidden within-signature raw spread therefore cannot collapse into the same
scientific result.

**Alternatives discarded:** Reconstructing realization from emitted
subsistence or `Function` labels was rejected as circular; using the whole-bake
`ExchangeCensus` as the projection was rejected because it loses community
identity; using only a closing stock was rejected because it erases the
twelve-step seasonal cycle; evaluating pooled totals was rejected because it
weights large seeds and hubs into the verdict; treating a singleton source
signature as sufficient was rejected because one outlier would carry the
entire contrast.

**Ideonomy passes / overturns:** one pass (seed 4104), no overturn. The pass
used abstraction-lift and negation in a notation organon, stressing cyclicity,
visibility, and homogeneity. The lifted form was “node-local opportunity joined
to periodically integrated realization through an explicit transmission
path.” Its negations exposed three invalid probes: hidden source values
replaced by visible labels, a periodic process reduced to a one-shot closing
snapshot, and heterogeneous nodes reduced to one homogeneous pooled total.

Cross-domain re-instantiation preserved the same notation in all three
required readings: ecology maps `S_s` to habitat opportunity and `P_s` to
realized two-nutrient intake; a local market maps them to production
possibility and the post-trade typed basket; routing maps them to node capacity
and delivered typed flow. In each reading transmission/access explains what
can move but never becomes source value, and one large habitat, market, or hub
cannot rescue a locally flat sample.

**Capture actions:** encoded the notation and all negated branches in
`staple_d3b_probe`; kept the 200-seed report ignored; added no role vocabulary,
dynamics, epoch, specialization behavior, or census action.

## #16 [Implementation] — Task 3 falsifier accepted after anti-saturation correction

The fixed-roster probe and its same-run History seam are accepted after
independent review. The review found and the correction closed a sub-quantum
escape: equal committed projection vectors now force `ProjectionCollapse`
before exact structural ordering can produce a false `Cleared` verdict. The
regression retains distinct raw vectors/signatures while proving the emitted
comparison remains non-clearing. The fixed roster remains ignored locally.

**Evidence:** probe `37493b324`; anti-saturation correction `937d169969`;
timing follow-up `83c9f2c6d`.

## #17 [G6-ready] — Verification complete; hold for merge/close review

Final verification found no remaining issue after reconciling the approved
G3 status in the spec and campaign reconciliation. The branch is clean, the
focused D3B/D2/runtime checks and documentation checks are green, the ignored
fixed-roster report was only listed and not run, and no census was run.

**Evidence:** final verification on `27ca6767f`; docs-consistency 41/41;
focused D3B 15/15; D2 10/10; history-bake 100/100; commit gate and all
4,280 subfloor tests passed.

## #18 [Post-run ruling] — Zero-phase live units must remain joinable

The first local fixed-roster run failed at seed 1's integrity assertion:
313 live units matched the existing `alive_at_now` denominator, but 13 live
units had no projection witness. Root-cause tracing found that
`diagnostic_subsistence_at_now` omitted every live accumulator with
`phase_count == 0`. This is not a scientific dead pole; it is a producer-side
join defect because the falsifier's denominator is intentionally the existing
live census.

**Decision:** under enabled treatment, retain every live community in the
sidecar. A zero-phase witness carries an explicit non-evidentiary zero
sentinel and is rejected by the probe's phase-completeness branch before any
coverage value is consumed. Disabled treatment remains an empty sidecar.

**Evidence:** local fixed-roster run, seed 1, 313 live / 300 joined / 13
missing projections; no census run. **Cost if wrong:** rerun the full roster;
if zero-phase units are scientifically ineligible rather than incomplete, the
denominator contract must be revisited at the next design gate.

## #19 [Implementation] — Enabled zero-phase witnesses retain incompleteness

Task 5 implements #18 by separating treatment selection from observation
completeness. `diagnostic_subsistence_at_now` now returns no witnesses when
the treatment is disabled and translates every live accumulator when it is
enabled, preserving the existing `BakeId` sort. A zero-phase accumulator
translates to `phase_count == 0` with `[0.0, 0.0]` non-evidentiary sentinels
for both coverage and shortfall; positive-phase averaging is unchanged. The
probe already rejects the zero-phase branch before consuming either vector.

The regression first failed with `left: []` against the required explicit
zero-phase witness, while its disabled-path assertion passed. After the
bounded correction, the regression passed; the full `history_bake` unit
module passed 101/101, and the non-ignored Staple D2/D3B suite passed 25/25.
Workspace clippy with warnings denied passed, as did all four `gate-commit`
subfloor chunks before this ledger update. No probe denominator, join
severity, role/portfolio/dynamic/epoch, save/emission path, or census fixture
changed. The ignored 200-seed report and every census command remained
unrun, per the Task 5 implementation boundary.

## #20 [Post-run ruling] — Explicit incompleteness is non-clearing, not malformed

The required post-Task-5 fixed-roster rerun confirmed that the producer-side
join defect is gone: the previously missing projection witnesses are present
and `missing_projections` is empty. The probe then stopped at its own final
assertion because live zero-phase units populated `phase_incomplete_units`,
which was still included in `InvalidMeasurement`.

**Decision:** classify phase incompleteness separately as a non-clearing
`IncompleteMeasurement` verdict. Malformed numeric values, incoherent access,
conservation failures, duplicate/missing joins, and other integrity failures
remain fatal. The explicit zero sentinel is never consumed as evidence. The
fixed roster must report this state rather than panic.

**Evidence:** post-Task-5 local fixed-roster run, seed 1; `missing_projections`
empty and `phase_incomplete_units` populated. No census run.

## #21 [Review finding] — Nonzero partial phases remain fatal

Independent review found that Task 6's first correction grouped zero-phase
observations with nonzero phase counts that are not complete epoch multiples.
That would let malformed counts such as 11 or 13 escape the fixed-roster
integrity assertion as `IncompleteMeasurement`.

**Decision:** only `phase_count == 0` is the explicit incomplete observation
branch. A nonzero count that is not divisible by the epoch phase count remains
`InvalidMeasurement`; restore a regression test for that severity boundary.

**Evidence:** scoped review of `c90c440fa`; no roster or census run after the
finding.

## #22 [Task 6 complete] — Fixed roster reports gradient variation without clearing the roster

After the severity correction, the local ignored fixed-roster diagnostic
completed 200/200 seeds in 573.56 seconds. The producer join defect remained
absent: no seed had missing projections, invalid projections, incoherent
access, invalid sources, zero demand, or conservation residuals. The verdict
distribution was 12 `Cleared` and 188 `IncompleteMeasurement`; the latter is
the explicit zero-phase live-unit branch and is non-clearing.

The descriptive aggregate is materially varied before any role assignment:
52,676 live denominator units, 49,578 joined units, 9 source signatures, 6
projection signatures, 13,565 committed projection vectors, and raw coverage
spread of approximately `[0.104, 0.497]` to `[1.0, 1.0]`, with nonzero
shortfall spread. The roster therefore does not support a dead-pole claim of
no usable gradient, but it also does not authorize specialization dynamics:
zero-phase incompleteness keeps the overall result `MixedOrUnderpowered`.

**Evidence:** `/tmp/hv-d3b-fixed-roster-task6.txt`; focused probe 17/17,
independent blocker re-review with no findings, gate-commit green, no census.

## #21 [Implementation] — Explicit incompleteness remains reportable

Task 6 implements #20 with a distinct `IncompleteMeasurement` seed verdict.
Fatal malformed measurements retain priority: invalid source or projection
values, zero demand, and incoherent access still resolve to
`InvalidMeasurement`; join and conservation failures retain their existing
higher-severity branches. A seed whose only exclusion is explicit phase
incompleteness now resolves to `IncompleteMeasurement` before any underpowered
or scientific verdict can clear it. The fixed-roster reducer therefore remains
`MixedOrUnderpowered`, and its final integrity assertion can print the seed and
pooled report without treating expected incompleteness as a fatal panic.

The focused zero-phase regression first failed behaviorally with `left:
InvalidMeasurement` and `right: IncompleteMeasurement`, then passed against
the new verdict. The complete non-ignored D3B probe module passed 16/16; the
ignored 200-seed report was listed as ignored and was not run. No producer,
denominator, source band, role, specialization, save/emission path, or census
behavior changed, and no census command ran.

## #23 [Integration] — Main absorbed; stage gate remains the next boundary

The campaign branch absorbed local `main` at `b096c7a7b` in merge commit
`a9727eadf`. The merge required preserving the D3B History sidecars through
main's new `history_from_bake` seam, carrying main's outbreak exports, and
registering epidemiology concepts in the D3B save-inert helper. The focused
D2/D3B suite passed 27/27 after those resolutions; the full local commit gate
passed all four subfloor chunks, and the calibration-pin census check passed.

This is an integration update, not a G6 close: no chronicle, retrospective,
book freshness/gradient closeout, registry flip, census refresh, or merge
sluice submission has occurred. The branch is ready for `make sluice-stage`
once the updated documentation commit is made and pushed; it is not ready for
the final `make sluice` merge request.

## #24 [Stage gate] — D3B stage product passed on the canonical box

The stage request `req-2ae088604e78-20260909T010248Z` completed with all four
stage phases green (`rc=0`) in 1,508 seconds. The chamber merged the candidate
against the then-current `main` and left `main` unchanged. The operator found
no census pins, heavy calibrations, world fixtures, registry surfaces, or
scene-schema changes requiring follow-up on this campaign.

**Decision:** treat D3B as stage-gate-passed and continue the campaign at the
design boundary. This is not a G6 close or authorization for a final merge;
the next work remains brainstorming/specification for specialization emergence
and must not invent `Function` labels before that design converges.

**Evidence:** sluice status row for `req-2ae088604e78-20260909T010248Z`; no
census refresh run.

## #25 [G2] — D4 specialization-emergence design drafted

The post-D3B brainstorm converged on a neutral intermediate vocabulary and a
measurement-only next rung. A `portfolio profile` is an adequately observed,
time-windowed typed vector; a `portfolio regime` is a recurring cluster of
profiles with a defensible relationship to opportunity, capability, need,
access, relation, or coercion. Neither is a `Function` label or persistent
history state.

The draft D4 falsifier uses the existing live `BakeOccupation` denominator
and D2 history seam. It retains production, voluntary exchange, imports,
shortfall, coercive transfer, and protection access as separate channels. One
complete window establishes only a provisional profile; same-phase recurrence
establishes a seasonal regime candidate; cross-phase recurrence establishes a
persistent regime candidate. The diagnostic compares portfolio composition
with opportunity-adjusted position, reports capability and need as companion
explanations, and records axis debt when fantasy dimensions lack a joined
consumer path.

Rejected alternatives include output-only vectors, one signed scalar,
one-cycle specialization, all-window averaging, persistent state first,
discarding coercive flows, one-to-one source mapping, and `Function` labels.
The written draft is ready for G3 review; no code, implementation plan,
dynamics rung, census action, or final merge is authorized.

**Evidence:** `docs/superpowers/specs/2026-09-09-the-staple-d4-design.md`;
ideonomy passes on temporal windows, seasonal regimes, mechanism separation,
and portfolio-regime vocabulary.

## #26 [G3] — D4 design approved

Nathan approved the D4 portfolio-regime design on 2026-09-09. The approved
boundary is a zero-impact diagnostic over existing dynamics: profiles and
recurring regimes may be observed, but no `Function`, occupation, learning,
movement rule, persistent specialization state, new fantasy extraction path,
epoch, census, or save-facing history change is authorized by the approval.

The implementation plan must preserve phase identity, keep voluntary and
coercive channels comparable but separate, report capability/need as
companion explanations, and expose axis debt rather than fabricating missing
mechanisms as zero. Execution proceeds under subagent-driven development;
the campaign stops before the fixed-roster report and final merge boundary.

**Evidence:** `docs/superpowers/specs/2026-09-09-the-staple-d4-design.md`;
`docs/superpowers/plans/2026-09-09-the-staple-d4.md`.

## #27 [Review ruling] — D4 normalization must reject aggregate overflow

Task 1 review found that `d4_normalize_profile` validated each finite,
non-negative channel but accepted a finite-input sum that overflowed to
infinity. That could divide every component by infinity and manufacture a
zero composition marked complete, violating the explicit non-evidentiary
vacuity rule.

**Ruling:** reject a non-finite accumulated total before division and add a
regression covering finite-input overflow. Keep the fix inside the pure D4
helper; no runtime path or world behavior changes.

**Evidence:** Task 1 review at
`.superpowers/sdd/2026-09-09-the-staple-d4/task-1-review.md`.

## #28 [Task 1 review] — D4 pure portfolio model approved after overflow fix

The Task 1 reviewer found one important numerical-integrity issue in
`d4_normalize_profile`: finite channel inputs could overflow their aggregate
to infinity and fabricate a zero composition. The scoped fix rejects a
non-finite total and adds direct non-finite and finite-input-overflow tests.

The re-review approved the fix with no remaining findings. Task 1 is complete
and produces the pure D4 vocabulary only; the history observation seam remains
the next task.

**Evidence:** commits `aeb4a82bf` and `c4e7616cf`; focused D4 tests (6/6),
`cargo fmt --check`, and `make gate-commit` green; review and re-review in
`.superpowers/sdd/2026-09-09-the-staple-d4/`.

## #29 [Task 2 ruling] — Do not phase-fake epoch-level coercion

The Task 2 seam audit confirmed that D2 production, exchange, and consumption
already occur inside the twelve-phase loop, while tribute collection and its
protection relation are observed at an epoch-level boundary. A phase-resolved
portfolio witness must not attach an epoch-level remittance to an arbitrary
phase or represent an unjoined mechanism as zero.

**Ruling:** record coercive/protection values in a phase record only when the
existing path supplies a defensible phase identity. Otherwise preserve an
explicit mechanism/temporal availability debt in the sidecar and leave the
value absent. This keeps the sidecar honest and lets the later probe report
causal/temporal debt rather than manufacturing phase evidence.

**Evidence:** Task 2 code-boundary verification of
`produce_subsistence_phase`, `clear_subsistence_phase`,
`consume_subsistence_phase`, `record_exchange_clearing`, and
`collect_tribute` in `windows/worldgen/src/history_bake.rs`.

## #31 [Task 2 review] — Live D4 seam needs a successful typed-delivery witness

Task 2 review found that the live-path regression used zero capacity and only
impossible requests. That proves the sidecar can retain an unsatisfied path,
but not that nonzero typed production or a successful voluntary delivery
reaches the D4 witness. The hand-populated accumulator test cannot cover that
runtime wiring.

**Ruling:** add one focused live-path fixture with nonzero typed production and
at least one successful typed delivery, asserting producer output/export,
recipient import, and typed shortfall. Keep the observation-only boundary and
all existing D2 behavior unchanged.

**Evidence:** Task 2 review at
`.superpowers/sdd/2026-09-09-the-staple-d4/task-2-review.md`.

## #32 [Task 2 review] — Phase-resolved portfolio seam approved

The live-delivery review finding was fixed with a runtime fixture that uses
positive typed production, successful voluntary clearing, producer export,
recipient import, settled delivery, and typed shortfall. The scoped re-review
approved the implementation with no new issues. Its isolated rerun encountered
an unrelated arm64 linker failure before test execution; the implementer's
completed focused run and D2/D3B suite were green, and the gate passed.

Task 2 is complete. The sidecar remains zero-impact, phase-resolved for D2
production/exchange/consumption, and explicit temporal debt for epoch-level
coercion/protection.

**Evidence:** commits `fd60f6e12` and `944ecfc52`; review and re-review in
`.superpowers/sdd/2026-09-09-the-staple-d4/`.

## #30 [Process ruling] — Re-dispatch the stalled Task 2 seam

The first Task 2 worker made a substantial partial edit but remained idle
through multiple status and continuation nudges without completing tests or a
commit. Its uncommitted diff was preserved at
`.superpowers/sdd/2026-09-09-the-staple-d4/task-2-partial.diff` before the
worker was closed. No partial work was discarded.

**Ruling:** continue from the preserved partial seam with a fresh,
more-capable integration worker and require a concrete DONE/BLOCKED report.
The approved Task 2 boundary and #29's no-phase-faking rule remain unchanged.

### Task 2 seam implementation clarification

The live sidecar uses `DiagnosticPortfolioValues`: production, delivered
voluntary exports/imports, and D2 typed shortfall ratios retain phase order.
Coercion and protection are `None` with explicit temporal debt, per #29;
they are not zeros in the pure model's numeric vector. Phase-local requests
retain the existing requester, counterparty, resource, quantities, and status
sequence, including impossible and refused outcomes. Complete phase records
are emitted in observation order (indices repeat each epoch); live communities
with no complete phases remain present with an empty phase vector. No tribute
flow, D2 accumulator, exchange census, or save-emission behavior is changed.

## #33 [Task 3 ruling] — D4 probe joins live units by BakeId and keeps recurrence per community

The Task 3 falsifier uses the existing `census(history).alive_at_now`
denominator and joins each live `BakeOccupation` to exactly one same-run
`DiagnosticPortfolioWitness` and site. It retains raw typed vectors beside
normalized signatures, compares each community's phase sequence before any
descriptive pooling, and reports missing, duplicate, disabled, site-mismatch,
incomplete, and unavailable-mechanism branches as non-clearing evidence.

**Ruling:** equal normalized composition with different raw scale is not a
distinct regime; one complete window is transient, same-phase recurrence is
seasonal, and cross-phase recurrence is a persistent candidate only. Tribute-
only evidence remains coercive/mixed rather than voluntary specialization.
The fixed-roster report remains ignored and is not run locally.

**Evidence:** the non-ignored D4/D2/D3B focused suite passed 34/34; the D4
probe's live seed-11 build was deterministic and its diagnostic sidecar was
save-inert under emitted ledger-byte comparison. No census or fixed-roster
report was run.

## #40 [Task 4 corrective pass] — Seasonal cycles and structural joins are explicit

The corrective pass recognizes repeated multi-phase cycles while retaining
phase identity, adds probe-side structural vacuity controls for isolation,
hub dominance, and single-type dominance, rejects orphan source rows, and
retains original optional channel values beside the numeric profile projection.
Unavailable channels remain incomplete and cannot enter a positive branch.

**Evidence:** D4 library tests passed 11/11; focused D2/D3B/D4 tests passed
36/36 (one pre-existing leaky test). No census, fixed-roster report, stage, or
merge was run.

## #36 [Task 3 re-review] — Mixed coercion and partial joins remain non-clearing

Scoped re-review found three residual gaps: mixed voluntary/coercive evidence
could still clear because vacuity required every community to be voluntary-free;
source accounting was skipped when witness or live-site joins failed; and
declared mechanism debt was not retained when a witness was duplicated or its
site failed to match.

**Ruling:** any observed coercive realization in an otherwise positive candidate
is vacuous/mixed until causal separation is available, source results must be
recorded for every live denominator unit independently of the other joins, and
all available witness rows must contribute their declared axis debt before join
acceptance. Re-review is required again after this correction.

**Evidence:** scoped re-review of the Task 3 probe; no files were changed by the
reviewer, and no census or fixed-roster report was run.

## #34 [Task 3 review] — Do not accept synthetic or Frankenstein gradient evidence

The first Task 3 review found that the probe re-derived mechanism availability
instead of consuming the sidecar declaration, supplied a constant synthetic
source vector for live builds, silently dropped missing or duplicate source
rows, used a narrow all-phases coercion vacuity check, and could combine
recurrence from one community with regime contrast from another. These are
measurement false-positive/false-negative risks, not implementation polish.

**Ruling:** the corrective pass must preserve declared axis debt, obtain source
or access variation from an existing live observation rather than `[1, 1]`,
make source joins denominator-bearing, require the same communities to support
the recurrence and contrast claim, and treat mixed/tribute-only evidence as
non-voluntary even when coercion is intermittent. Re-review is required before
Task 3 can be accepted.

**Evidence:** Task 3 review at
`.superpowers/sdd/2026-09-09-the-staple-d4/`; no census or fixed-roster report
was run.

## #35 [Task 3 review] — Corrective pass closes the false-positive paths

The corrective pass now consumes the sidecar's declared mechanism availability,
keeps exact axis debt, treats the absent live source/access observation as
explicit underpowered evidence instead of `[1, 1]`, and makes source joins
denominator-bearing. Recurrence and contrast must be supported by the same
communities; mixed recurrence is not collapsed into a transient positive.
Tribute-only evidence remains vacuous when coercion is intermittent.

**Evidence:** 35 focused D4/D2/D3B tests passed, including explicit missing and
duplicate source rows, mixed recurrence, intermittent coercion, deterministic
live sidecar emission, and save-inert ledger comparison. No census or ignored
fixed-roster report was run.

## #39 [Task 4 re-review] — Temporal and structural vacuity are not yet closed

The delayed cross-domain re-review found four remaining contract gaps. The
recurrence classifier only recognizes identical repeated signatures, so a
repeating multi-phase seasonal cycle can be labeled drifting or rotating. The
probe has no isolation, hub, or single-type dominance vacuity controls. Source
rows orphaned from the live denominator are silently ignored. Finally,
unavailable optional channels are represented as numeric zeroes in the raw
vector, requiring a second field to recover their status.

**Ruling:** the corrective pass must recognize repeated multi-phase seasonal
cycles without erasing phase identity, make isolation/hub/single-type
dominance non-clearing or explicitly debt-bearing, reject orphan source rows,
and preserve unavailable channels as unavailable in the diagnostic surface
rather than presenting them as observed zeroes. Re-review is required before
Task 4 can close.

**Evidence:** delayed Task 4 review; focused 35-test suite and D4 library tests
were green, but they did not cover these branches. No census, fixed-roster
report, stage gate, or merge was run.

## #38 [Task 4 review] — Cross-domain re-instantiation preserves the D4 boundary

The implementation was checked against three domains without widening the
model. In ecology, a typed opportunity/source vector is the required future
instantiation; because no phase-resolved live source observation exists today,
the probe reports source debt rather than fabricating one. In network flow,
BakeId/site joins, typed voluntary delivery, imports, and phase recurrence are
the measurable path; pooled degree or hub dominance cannot clear a branch. In
a non-economic domain, protection/coercion remains a separately declared
mechanism with explicit temporal/mechanism debt and cannot be relabeled as
specialization.

**Ruling:** D4 remains a measurement-only portfolio/regime reading. It assigns
no `Function`, writes no persistent role state, changes no save-facing history,
and does not add movement, learning, prices, or fantasy extraction dynamics.
The approved contract survives cross-domain re-instantiation; the absent
source/access seam is a recorded debt and the live branch is non-clearing.

**Evidence:** commit `0d6f9c332`; final local commit gate passed all four
sub-floor chunks and the focused non-ignored D4/D2/D3B suite passed 35/35. No
census, fixed-roster report, stage submission, or merge was run.

## #37 [Task 3 re-review] — Conservative mixed-mechanism and denominator closure

The final correction makes any observed coercive realization vacuity-bearing,
records source presence independently for every live unit, and records debt
from every available witness before accepting uniqueness or site agreement.
The earlier #35 closure is superseded by #36 and this ruling.

**Evidence:** the corrective focused D4/D2/D3B suite passed 35/35 after adding
mixed voluntary/coercive, partial-join, and debt-preservation coverage. No
census or ignored fixed-roster report was run.

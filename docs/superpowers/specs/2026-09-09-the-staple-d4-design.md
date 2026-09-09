# The Staple D4 — portfolio regimes before specialization

**Status:** G3 approved 2026-09-09; implementation complete; G6 merge review pending  
**Campaign:** `campaign/the-staple-d4`  
**Predecessor:** The Staple D3B, gradient sufficiency  
**Decision block:** 0946–0955

## 1. Purpose and boundary

D3B established that the current world contains materially varied local
source and projection regimes, while remaining mixed or underpowered because
some live units lack complete phase histories. D4 now asks a narrower question:

> Do existing dynamics produce recurrent, locally interpretable portfolio
> regimes before Hornvale adds role labels, learning, or persistent
> specialization state?

The SugarScape lesson remains structural: heterogeneous opportunities, needs,
capabilities, movement, and bilateral exchange can produce differentiated
positions without assigning occupations. Fantasy sources in the spirit of
*Roadside Picnic*/*Stalker*—anomalies, ruins, hazards, rare materials, and
knowledge-gated extraction—are eligible future axes only after they join the
same measurable settlement path.

D4 does not assign `Function`, create occupations, add persistent portfolios,
add learning or movement rules, change production or exchange behavior, add an
epoch, or authorize a census re-baseline. Its first deliverable is a
preregistered portfolio-regime falsifier over already-produced dynamics.

## 2. Vocabulary

A **portfolio profile** is an adequately observed, time-windowed vector for
one live community. It is a reading, not a saved fact.

A **portfolio regime** is a recurring cluster of portfolio profiles with a
defensible relationship to opportunity, capability, need, access, relation,
or coercion. It may be seasonal, rotating, drifting, or persistent. The word
does not imply an occupation or social identity.

**Specialization** is a later interpretation. A portfolio regime is evidence
that specialization may be worth designing; it is not itself a specialization
label.

The temporal vocabulary is:

```text
one-cycle difference
    -> transient contrast
repeated same-phase difference
    -> seasonal portfolio regime
cross-phase or cross-year persistence
    -> persistent specialization candidate
```

## 3. Task 0 unit and denominator

The counted unit is one live `BakeOccupation` record at the observation
instant, joined to its same-run D2 history sidecar by the existing community
and site identity. The denominator is the existing
`census(history).alive_at_now` population, not a new population, relation, or
trade-volume denominator.

For a one-cycle readout, an adequate unit has one complete, nonzero D2 phase
window. For recurrence, an adequate unit has at least two comparable complete
windows, with phase identity retained so seasonal contrast is not averaged
away. Empty, missing, duplicate, disabled-treatment, zero-demand, zero-phase,
and malformed joins remain explicit branches. A unit with only a zero vector
or no completed phase cannot establish a regime.

Verdicts are per seed. Pooled totals are descriptive and cannot rescue an
underpowered or flat seed. Equal unit weighting prevents population, degree,
or a high-throughput hub from determining the result.

## 4. Portfolio profile

The first profile is a typed, signed-but-structured vector over the existing
D2 flow types:

```text
Portfolio(c, window) = {
    realized_output[type],
    voluntary_exchange[type],
    recurring_import[type],
    persistent_shortfall[type],
    coercive_transfer[type],
    protection_access[type]
}
```

The direction is intentionally readable—outputs and voluntary exports point
outward, imports and unmet demand point inward—but the components remain
separate. Importing grain, failing to obtain grain, and exporting fish are not
the same fact. Raw typed vectors remain available beside normalized
composition vectors.

The profile must preserve the distinction between:

- local production or extraction;
- voluntary exchange;
- imports and dependency;
- persistent shortfall;
- coercive tribute or other imposed transfer; and
- protection- or patronage-mediated access.

Coercive flows are comparable evidence and a possible explanation, but they
cannot independently establish opportunity-driven specialization. A recurring
export profile sustained only by tribute is a mixed or imposed regime, not the
same result as a locally realized and voluntarily exchanged surplus.

## 5. Temporal readout

The diagnostic reports both a one-window profile and a recurrence class:

| recurrence | interpretation |
| --- | --- |
| no complete window | incomplete measurement |
| one complete window only | transient contrast or provisional profile |
| same-phase recurrence | seasonal portfolio regime candidate |
| cross-phase or cross-year recurrence | persistent regime candidate |
| changing profile across windows | rotating or drifting regime |

The diagnostic must retain phase identity and report the all-window aggregate
only as a companion. An all-time average must not erase an oscillating or
seasonal portfolio. An adaptive observation horizon is not permitted to choose
its own evidence threshold; the complete-window and recurrence rules are
frozen before measurement.

## 6. Paired interpretation instrument

D4 does not select one scalar diversity metric in advance. It retains a paired
instrument:

1. **Portfolio composition:** which typed outputs, exchanges, dependencies,
   shortfalls, and transfers occupy each community's normalized profile.
2. **Opportunity-adjusted position:** how that profile relates to the local
   source, access, capability, need, relation, and coercion observations
   already available through a joined path.

The comparison is relational rather than one-to-one. A source difference may
produce no portfolio difference because realization saturates. Similar sources
may produce different portfolios because capability, need, or access differs.
Neither case is silently collapsed.

The report must retain:

- raw typed profile vectors;
- normalized composition vectors;
- source and opportunity signatures;
- phase-specific and cross-phase comparisons;
- voluntary/coercive mechanism shares; and
- per-unit and per-seed counts.

The following are candidate descriptive views, not independent verdicts:

| view | use | exclusion risk |
| --- | --- | --- |
| occupied portfolio bands | auditable regime presence | cuts can be coarse |
| pairwise profile distance | graded divergence | distance choice can dominate |
| rank/order signature | survives magnitude saturation | ties and one-type dominance |
| comparative advantage | relates realization to opportunity | unstable at zero opportunity |
| community-by-type matrix | exposes directional dependency | sparse and phase-sensitive |

No view passes merely because values are nonzero, labels differ, one pair
differs, or one hub dominates the aggregate.

## 7. Preregistered branch table

The verdict is per adequately powered seed. The required dead poles and
qualified branches are:

```text
no adequate units have materially distinct portfolio profiles
    -> DEAD POLE 1: no realized differentiation

source/access conditions vary, but every adequate unit has the same
normalized portfolio ranking after saturation review
    -> DEAD POLE 2: realization or measurement collapse

profiles differ only through missing history, zero exposure, isolation,
hub scale, or a universally consumed type
    -> QUALIFIED FAILURE: vacuous differentiation

profiles differ but do not recur in comparable windows
    -> TRANSIENT CONTRAST: no portfolio regime established

profiles recur in the same phase, with source/access or other measured
conditions giving a defensible interpretation
    -> SEASONAL PORTFOLIO REGIME

profiles recur across phases or years with stable structure
    -> PERSISTENT REGIME CANDIDATE; specialization remains deferred

some seeds are adequate and positive while others are empty, incomplete,
underpowered, or flat
    -> MIXED/UNDERPOWERED: no campaign-wide specialization claim
```

The positive branch requires multiple adequate communities in multiple
regimes. A single exceptional site, one dominant type, one hub, or a pooled
whole-bake total cannot clear it.

## 8. Capability, need, and causal interpretation

Capability and need are companion explanatory axes, not mandatory evidence for
the first positive result. Requiring a complete theory of capability before
recognizing a recurring realized difference would turn D4 into a world-
completeness test and produce a false negative.

Interpretive evidence is tiered:

1. **Observed differentiation:** adequate communities have distinct,
   recurrent profiles.
2. **Opportunity alignment:** profiles correspond to measured ecological,
   material, access, or network differences.
3. **Mediated explanation:** capability, need, movement, relation, or
   coercion explains why similar opportunities produce different profiles.

Tier 1 is necessary. Tier 2 supports an opportunity-driven regime. Tier 3
supports a richer specialization theory. The diagnostic must report the tier
achieved rather than silently treating correlation as causation.

## 9. Axis debt

A flat or underpowered result must distinguish missing model dimensions from a
negative result about specialization. Axis debt is reported by kind:

- **source debt:** no joined opportunity field exists;
- **consumer debt:** an opportunity exists but no settlement path consumes it;
- **capability debt:** communities cannot differ in how they exploit it;
- **access debt:** movement or relations cannot transmit it;
- **temporal debt:** recurrence is not observable;
- **mechanism debt:** only labels or aggregate totals exist; and
- **causal debt:** voluntary and coercive mechanisms cannot be separated.

Future axes enter the portfolio witness only if they:

1. join the existing live-community denominator;
2. provide a raw material value rather than a decorative label;
3. have a typed consumer, transformation, exchange, or need path;
4. are observable across the required temporal window;
5. vary independently enough not to be a uniform rescale or duplicate;
6. separate voluntary, protected, and coercive realization where relevant;
7. survive zero, isolation, hub, and missing-join checks.

Anomaly, ruin, hazard, alchemical, subterranean, route, and knowledge axes
remain axis debt until they pass this contract. No axis may be added solely to
force a positive result.

## 10. Alternatives rejected during brainstorming

1. **Output-only portfolios.** Rejected because shared consumption can make a
   fishing and grain community look identical while their dependencies differ.
2. **One signed scalar.** Rejected because it conflates production, import,
   shortfall, voluntary exchange, and coercion.
3. **One-cycle specialization.** Rejected because a one-off shock or harvest
   cannot establish recurrence.
4. **Multi-cycle averaging only.** Rejected because it erases seasonal and
   oscillating regimes.
5. **Persistent history state first.** Rejected because it adds a dynamics
   rung before a derived reading proves nontrivial differentiation.
6. **Coercive flows excluded.** Rejected because tribute and protection are
   comparable explanations of the same observed profile; they must be
   stratified, not discarded.
7. **One-to-one source-to-portfolio mapping.** Rejected because capabilities,
   needs, and access can mediate the same opportunity differently.
8. **`Function` labels as the first output.** Rejected because labels would
   turn an observation into an authored occupation and repeat D3's saturation
   risk.

## 11. Proof obligations before implementation

- The denominator is an existing live-community stock and every counted unit
  has an explicit completeness state.
- A one-window profile cannot clear a recurrence branch.
- Phase identity survives normalization and aggregation.
- Raw typed vectors remain visible beside every band or signature.
- Voluntary and coercive channels remain separate through comparison.
- A fixture proves that equal outputs with different dependencies are not
  treated as equal portfolios.
- A fixture proves that a tribute-only recurring pattern is not classified as
  opportunity-driven specialization.
- A fixture proves that seasonal recurrence is not erased by all-window
  averaging.
- Pooled volume, population, degree, labels, and one outlier cannot clear a
  seed or campaign-wide dead pole.
- Cross-domain re-instantiation tests the same measurement shape in ecology,
  network flow, and another non-economic domain before any role vocabulary is
  introduced.
- The first implementation remains a zero-impact diagnostic reading over
  already-produced history; it does not alter stores, population, exchange,
  movement, functions, epoch output, or save emission.

## 12. Dynamics cost and deferred scope

The approved first step is a derived diagnostic only. If later work makes
portfolio profiles part of committed history or changes production, exchange,
movement, capability, or anomaly realization, it becomes a dynamics rung and
must budget an epoch, census re-baseline, and conversion of history-adjacent
pins into invariants.

Persistent specialization, learning, role identity, `Function` derivation,
new fantasy extraction mechanics, price formation, labor institutions,
currencies, and D5 city/notability work remain deferred.

## 13. G3 questions

Nathan's review is requested on:

1. the `portfolio profile` / `portfolio regime` vocabulary;
2. the signed-but-structured typed vector and mechanism separation;
3. the one-window versus recurrence evidence rules;
4. the paired composition/opportunity instrument without a single scalar;
5. the tiered interpretation of capability and need;
6. the axis-debt taxonomy and inclusion contract; and
7. the zero-impact diagnostic boundary before any dynamics rung.

No code or implementation plan should begin until this draft is approved.

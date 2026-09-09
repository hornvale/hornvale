# The Staple D5 — comparative flow convergence

## Status

Design drafted for G3 review on 2026-09-09. No implementation or mechanism
change is authorized by this document.

**Campaign:** `campaign/the-staple-d5`

**Predecessor:** The Staple D4, which established the boundary before
specialization by shipping a portfolio-regime diagnostic.

**Metaplan rung:** D5 — a city is the apex where flows converge, with
notability derived and comparative.

## 1. Purpose and boundary

D5 asks whether the current world already produces a city-like apex through
comparative flow convergence. The first deliverable is a falsifiable Task 0
probe over existing D2/D4 evidence.

D5 does not assign `Seat`, `Notability`, `Function`, or specialization. It does
not create a city producer, add persistent city state, alter production,
exchange, movement, or relation formation, add an epoch, or authorize a census
re-baseline. A positive result is evidence for a later mechanism campaign, not
permission to implement that mechanism in this campaign.

## 2. Definition of a comparative apex

A **comparative apex** is an alive settlement whose position in the existing
flow and relation structure is materially distinguished from peer settlements
by convergent, typed flows, rather than by an absolute size or throughput
threshold.

The definition requires all of the following to remain visible:

- multiple inbound sources or relations;
- typed inbound and outbound structure;
- comparison with the settlement's peers in the same world;
- phase or time identity;
- the distinction between voluntary and coercive or imposed flows;
- the difference between a recurrent position and a one-window spike.

The word **city** is reserved for the later interpretation. The probe reports
apex candidates and refusal or incomplete states, not city labels.

## 3. Counted unit and denominator

The counted unit is one alive settlement at the observation instant. The
denominator for each seed is the complete set of alive settlements at the
relevant settlement rung that can be joined to the existing typed flow and
relation evidence.

Each settlement is evaluated independently before any pooling. A pooled total
is descriptive only and cannot rescue a seed with no adequate units,
underpowered evidence, or a flat result.

The diagnostic must retain explicit branches for settlements that are empty,
missing, duplicated, malformed, disabled by treatment, isolated, or supported
only by zero or incomplete windows.

## 4. Existing evidence path

The probe consumes the already-produced D2/D4 evidence path. It may derive
read-only summaries, but it must not create a new world fact or alter the bake.

The joined record must preserve:

- settlement identity and settlement rung;
- source and destination identity;
- flow type and direction;
- raw magnitude or count in its existing unit;
- phase and time-window identity;
- voluntary, coercive, protection-mediated, or otherwise imposed status when
  the existing trace distinguishes it;
- provenance and refusal reason where a join cannot be made.

If an existing trace cannot support one of these fields, the probe reports the
missing evidence rather than silently imputing it.

## 5. Primary observable

The primary observable is the settlement's **comparative flow-convergence
profile**. The report retains raw typed vectors and derived views for:

1. the number of distinct inbound sources;
2. the diversity of inbound flow types;
3. the structure and direction of inbound versus outbound flows;
4. relative graph centrality or dominance among peer settlements;
5. recurrence of the position across comparable windows.

No single scalar city score is authoritative. Candidate summaries may include
occupied profile bands, pairwise profile distance, rank signatures, comparative
advantage, and a settlement-by-type matrix, but each is a view over the same
evidence and not an independent verdict.

## 6. Control variables

The diagnostic records the following controls without using them as the
definition of an apex:

- population;
- local density;
- total throughput;
- catchment size;
- settlement age;
- relation count.

The controls exist to expose false positives. A settlement that is merely the
largest, oldest, densest, busiest, or most connected must not pass solely for
that reason.

## 7. Discrimination requirements

Task 0 must distinguish at least these nearby interpretations:

- a large but isolated settlement;
- a high-throughput settlement dominated by one universal flow;
- a temporary flow spike;
- a patron or coercive hub whose prominence is imposed;
- a settlement with multiple convergent flows and comparative dominance.

The positive interpretation requires multiple adequate settlements in multiple
regimes within a seed or across the preregistered comparison set. One unusual
settlement, one dominant type, one hub, or one pooled whole-world total is
insufficient.

## 8. Temporal evidence

The probe uses the phase identity and complete-window rules established by D4:

| Evidence | Interpretation |
| --- | --- |
| no complete window | incomplete measurement |
| one complete window | transient or provisional profile |
| same-phase recurrence | seasonal apex candidate |
| cross-phase or cross-year recurrence | persistent apex candidate |
| changing profile across windows | rotating or drifting apex |

The observation horizon and minimum evidence threshold are frozen before
measurement. The diagnostic must not adapt its horizon after inspecting the
result.

## 9. Verdict table

Verdicts are assigned per seed:

| Condition | Verdict |
| --- | --- |
| no settlement has materially differentiated convergence | dead pole: no realized apex |
| convergence collapses to size, density, age, or throughput | dead pole: measurement collapse |
| apparent convergence is explained by missing history, zero exposure, isolation, hub scale, or coercion | qualified failure |
| convergence differs but does not recur | transient apex |
| same-phase convergence recurs with a defensible measured interpretation | seasonal apex candidate |
| convergence recurs across phases or years with stable structure | persistent apex candidate |
| seeds are empty, incomplete, flat, or underpowered in different ways | mixed/underpowered |

The report must state the evidence tier reached:

1. observed differentiation;
2. opportunity alignment;
3. mediated explanation through capability, need, movement, relation, or
   coercion.

Tier 1 is necessary for a positive apex candidate. Tiers 2 and 3 strengthen
interpretation but are not silently required for the first observation.

## 10. Attribution rules

The probe must distinguish authored, derived, and dynamically produced values.
It may describe association between a flow profile and an opportunity or
relation signature, but causal language is permitted only when the existing
evidence supports it.

In particular:

- coercive or protection-mediated flows are comparable evidence, not proof of
  opportunity-driven specialization;
- a source difference that produces no profile difference is a saturation
  result, not a missing observation;
- similar sources with different profiles are an observed contrast until a
  capability, need, access, movement, or relation mechanism is evidenced;
- a recurrent profile is not a social identity or occupation.

## 11. Evidence package

The Task 0 delivery must include:

- the frozen probe definition and branch table;
- the command or client path used to reproduce it;
- the revision, seed family, and time or frame selection;
- raw typed settlement profiles;
- normalized or summarized profile views with their derivation stated;
- control-variable comparisons;
- per-settlement and per-seed counts;
- missing, refusal, malformed, and incomplete branches;
- a discrimination witness for each nearby false interpretation;
- the strongest observation sentence justified by the result.

The result is a reading artifact. It must not alter save-facing world identity,
the census goldens, or the production path.

## 12. Consequences and non-goals

A positive D5 result authorizes a separate design question: whether and how to
add a city mechanism whose inputs, dynamics, persistence, and derived
notability are specified and probed independently. It does not choose the
mechanism, a population threshold, a catchment split rule, a price system, or a
district inventory.

A negative result must be recorded as a measured boundary. It must not be
repaired by lowering thresholds, renaming a large settlement a city, or
collapsing typed flows into one scalar merely to obtain a positive result.

The next campaign-stage decision is therefore: run the frozen Task 0 probe,
classify the result per seed, and decide whether a later city-mechanism
campaign is warranted.

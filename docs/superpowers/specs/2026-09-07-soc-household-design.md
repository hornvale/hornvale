# SOC-household — design

**Campaign:** SOC-household — the individual, household, kinship, lifecycle,
and social-relation substrate. **Branch:** `campaign/soc-household`.
**Decision block:** 0896–0905. **Status:** G3 design package; no code or
implementation plan has been approved.

SOC-household follows The Grammar's landed BIO-3 and SOC-2 successor contract.
It fills the household and biography boundary without making biology social
destiny or treating one culturally familiar family form as universal.

## 1. Classification and governing boundary

This is architectural work. It introduces a cross-layer substrate/projection
boundary consumed by demography, person records, history, cultural and
institutional interpretation, group projections, and The Lot.

The Murrain's population distinction remains load-bearing:

```text
BIO-3 / SOC-2 aggregate inputs
        |
        v
cohort transition and relation distributions
        |
        v
deterministic person/lifecycle projection
        |
        v
realized relation events
        |
        +--> kinship, care, descent, inheritance projections
        +--> household/group projections
        +--> cultural/institutional interpretations
        +--> The Lot observation
```

The aggregate population substrate is causally authoritative. A realized
person, household projection, composite case, or Lot draw is a consumer of
that substrate and cannot rewrite its biological or demographic truth.

The Grammar exposes reproductive roles, pathways, dependency and care
topology, descent, compatibility, and transition history. SOC-household
consumes that boundary; it does not reopen BIO-3 or assign social meaning to
its biological types.

## 2. Scope

### In scope

- A temporal relation/event substrate for realized persons and groups.
- Cohort-level distributions for lifecycle, care, descent, association,
  migration, dissolution, and inheritance transitions.
- A strict separation between sex traits, reproductive role, body plan,
  social gender, personal identity, and transition history.
- Derived kinship readings: parent/child, sibling, descent, and adoption.
- Derived household/group projections over residence, care, subsistence,
  property, authority, ritual, or institutional recognition.
- Separation, dissolution, migration, recomposition, parental death, and
  lifecycle transitions as historical events.
- Cultural and institutional interpretation of realized relations.
- Synthetic, test-only probe societies and anti-vacuity measurements.
- A future-compatible observation contract for The Lot.

### Not in scope

- A universal household class with one default family form.
- Full economy, property, or institutional simulation.
- Detailed anatomy, species taxonomy, or new canon for existing peoples.
- Magical sex or reproductive-state transition implementation.
- A complete psychology, affection, romance, or moral-prejudice system.
- Real-world racial, ethnic, religious, homophobic, transphobic,
  antisemitic, or white-supremacist structures as authored defaults.
- A full biography renderer or a replacement for The Lot's silence rule.

## 3. Architectural alternatives

### Household-first containers

Persons belong to household objects, with marriage, children, care, and
property attached to the container. This is simple to query but makes a
particular domestic form foundational, handles overlapping or changing groups
poorly, and makes residence look more fundamental than descent or care.
Rejected as the substrate; retained only as a possible projection.

### Person-to-person temporal event graph

Persons are nodes and typed, time-bounded relations are derived from an
append-only event history. Groups are projections over selected relation
topologies. This preserves provenance, overlapping relationships, migration,
death, separation, adoption, and recomposition while keeping the core small.
Adopted as the foundation.

### Role-slot or hypergraph model

Persons connect to roles and groups through multi-party edges, which is strong
for communal care, plural associations, and institutions. It is more abstract
than the first substrate requires, so SOC-household adopts hyperedge-like
multi-party events where pairwise edges are insufficient, especially for care,
residence, and inheritance, without replacing the event graph.

## 4. Truth classes and layers

Every fact is classified as one of:

```text
possibility  permitted by the biological or social grammar
typicality   statistically common in a cohort or context
realization  happened to a person, cohort, or group
recognition  interpreted or validated by a culture or institution
```

### Aggregate cohort substrate

The aggregate layer may carry distributions for sex traits, reproductive
roles, lifecycle states, dependency duration, care burden, association
formation and dissolution, descent and adoption pathways, migration,
recomposition, inheritance tendencies, and household/group topology.

It does not carry named biographies, infer gender from reproductive role, or
turn a statistical pattern into an individual fact.

### Realized person records

A projected person may carry stable identity, time-varying sex-trait
observations, reproductive-role history, personal gender identity claims,
social-recognition history, lifecycle state, origin/descent provenance, care
dependencies, associations, residence and migration, transfer/inheritance,
and death.

Sex traits, reproductive role, body plan, social gender, personal identity,
and transition history remain separate fields or event families. No mapping
between them is mandatory.

### Group and household projections

A household is a derived, time-bounded group projection. Its basis must be
declared, such as co-residence, shared subsistence, shared care, shared
property, recognized domestic status, ritual association, or temporary refuge.

Persons may belong to overlapping groups. A group may exist without a
partnership or parent-child relation. The projection exposes its basis and
provenance rather than asserting that every group is a household in one
universal sense.

### Historical events

The append-only event layer records `born` or `originated`, lifecycle
transitions, association, recognition, co-residence, care, dependency,
adoption or care transfer, descent, migration, separation, dissolution,
death, and inheritance/property transfer. Events carry time, participants,
context, provenance, and whether they are aggregate or realized.

Death and dissolution end the ability to create some future events; they do
not erase historical relations. A dead person's prior care, descent,
association, and inheritance edges remain observable.

### Cultural and institutional interpretation

This layer assigns names, categories, validity, duties, access, inheritance
eligibility, stigma, protection, ritual obligations, and authority to
realized relations. Interpretation is derived from modeled material and
historical conditions, institutions, and cultural processes, not from
biological destiny or imported real-world analogues.

## 5. Relation and operation vocabulary

The smallest foundational relation vocabulary is:

- `origin` — how a person or organism came into being;
- `descent` — derivational or inherited connection;
- `care` — support provided by a person or group;
- `dependency` — reliance of one person or group on another;
- `association` — a time-bounded connection with a declared form;
- `membership` — participation in a derived group;
- `residence` — co-location or domestic association;
- `custody` — recognized responsibility or authority;
- `transfer` — movement of care, property, status, or membership;
- `recognition` — institutional interpretation of another relation.

The smallest operation vocabulary is:

```text
form       create an association, group, or dependency
recognize  assign an institutional or cultural interpretation
associate  establish a relation with a declared form
care       provide support
depend     establish or change dependency
transfer   move care, property, status, or membership
migrate    move a person or group between places or groups
separate   end one association without erasing history
dissolve   end a group or institution
inherit    transfer a recognized claim after death
die        close a person's future lifecycle participation
reinterpret revise the social meaning of an existing relation
```

`parent`, `child`, `sibling`, `marriage`, and equivalent terms are derived
readings or cultural labels. They are not the only underlying relation types.

## 6. Material context

Subsistence, property, mobility, authority, religion, contact, and population
pressure enter as external context to transition and interpretation rules:

```text
form_association(persons, social_conditions)
assign_care(dependent, candidate_groups, resources, mobility)
recognize_relation(relation, institution, authority, doctrine)
derive_inheritance(deceased, claimants, property, descent_rules)
```

The household/group projection does not own these inputs. The causal path is
material conditions plus history plus institutions, then relation formation,
recognition, care, and inheritance, then group projection.

## 7. Synthetic probe panel

Probe names are test-only and do not assign canon to existing Hornvale
species.

1. **Independent-origin society:** multiple reproductive roles, no required
   pair-bond, communal care, and no marriage institution.
2. **Dual-descent society:** two descent lines with residence following
   neither line universally; exercises parentage, siblings, and inheritance.
3. **Care-cluster society:** overlapping groups jointly care for dependents;
   exercises multi-party care without a nuclear-household assumption.
4. **Recomposing mobility society:** migration, separation, serial
   associations, and household recomposition are frequent.
5. **Institutional-recognition society:** personal associations exist before
   or without institutional recognition.
6. **Lifecycle-transition society:** social roles and gender categories
   change across life stages independently of reproductive possibility.

Each probe must assert that the relevant path was exercised. An empty graph,
an unvisited transition, or a projection that passes only because nothing was
represented is not evidence of support.

## 8. The Lot contract

When supported by realized facts, The Lot may observe:

- sex traits at a relevant life interval;
- reproductive role without translating it into gender;
- personal gender identity and social recognition separately;
- association history and institutional interpretation;
- marriage or equivalent association labels when culturally warranted;
- children, siblings, descent, adoption, and care provenance;
- dependency and caregiving;
- group/household memberships over time;
- migration, recomposition, parental death, and inheritance;
- source facts and reasons for every filled or silent slot.

The Lot must never infer marriage from co-residence, gender from reproductive
role, or parenthood from care alone. Missing facts remain measured silences
under decision 0798.

## 9. G3 acceptance criteria

The design is ready for planning only when review confirms:

- the event graph is the substrate and households are projections;
- aggregate cohort truth is not confused with realized biography;
- sex, gender, reproductive role, body plan, identity, and transition remain
  distinct;
- dissolution and death preserve historical relations;
- material inputs remain external to household structure;
- synthetic probes cover non-pair, overlapping-care, migration, recognition,
  and lifecycle-transition cases;
- The Lot receives a sourced observation boundary and retains silence;
- no implementation work begins before these criteria are accepted.

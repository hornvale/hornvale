# The Staple R3 — Districts design

**Campaign:** The Staple, R3 Districts. **Branch:** `campaign/the-staple-r3`.
**Status:** Design approved 2026-09-08; implementation not started.
**Precedents:** The Staple metaplan, The Housemark, The Cruck, The
Connection Graph, The Murrain, and The Grammar.

R3 is the reading rung of The Staple's wheel: a finished world gains
districts without an epoch, ledger writes, seed draws, or new historical
facts. The district projection is a deterministic read over existing spatial,
settlement, cohort, access, movement, and exchange evidence. Its machinery is
independent of the dynamics arc, although D2 and later rungs will eventually
give it a richer subject.

## 1. Governing boundary

The Murrain's two-tier population model remains load-bearing:

```text
aggregate cohorts and world facts
        -> relation views
        -> district projection
        -> optional realized-person or narrative readout
```

The population/cohort substrate is causally authoritative. A district is a
derived observation, not a population fact. A district projection may expose
aggregate cohort participation, but it must never silently materialize persons,
households, biographies, or kinship.

R3 is not SOC-household. The Grammar's SOC-2 successor contract is an input
boundary for later household work, not a reason to reopen BIO-3. R3 supplies
social geography and relation topology; SOC-household will later supply
person, household, kinship, lifecycle, and institutional interpretation.

## 2. Scope

### In scope

- A narrow role-bearing relation assertion envelope.
- Local relation views for spatial adjacency, presence, access/movement, and
  exchange/flow.
- Aggregate cohort and spatial-locus endpoints.
- Directed, asymmetric, interval-bounded, and recurring relations.
- Basis-specific district projection.
- Persistent, recurring, transient, dissolved, and refused projection states.
- Overlap, bounded containment, and bridge-member annotations.
- Deterministic projection-local identity.
- Optional lifting of a successful district graph into existing Pattern
  composition machinery.
- Synthetic societies and cases that exercise the contract without adding
  canon to existing Hornvale species.

### Not in scope

- Household entities or household law.
- Partnership, marriage, or association semantics.
- Parentage, adoption, sibling, descent, or inheritance semantics.
- Sex, gender, identity, or reproductive interpretation.
- Person-level biography or lifecycle simulation.
- Cultural family forms or institutional treatment of kinship.
- A universal social graph or general agent scheduler.
- Field-based community detection or global modularity optimization.
- Fuzzy district identity matching.
- New dynamics, epochs, flows, prices, or settlement capacity mechanisms.
- New canon for existing Hornvale species.

## 3. Relation assertion envelope

The reusable substrate is a relation assertion, not a universal graph
authority. Its common shape is:

```text
relation_assertion:
    kind
    participants:
        reference
        role
    interval
    recurrence
    direction/order
    measure
    provenance
```

The envelope is intentionally semantic-light. Producers own the meaning of
their relation kinds; consumers may only interpret kinds they declare as
supported.

The participant list is role-bearing rather than permanently binary. R3 will
normally consume binary projections for spatial, access, presence, and
exchange relations. The broader shape prevents future SOC-household work from
having to retrofit parentage, care, group membership, or inheritance into a
binary-only substrate.

The envelope is not placed in the kernel and is not a second event ledger. It
belongs at the worldgen/population-facing composition boundary. Source
systems own raw facts and events; a relation adapter exposes a read view with
validity, recurrence, measure, and provenance.

## 4. R3 relation bases

R3 activates four bases:

1. **Spatial adjacency** — physical neighborhood or connected extent.
2. **Presence/residence** — aggregate or realized participation at a locus.
3. **Access/movement** — directed or undirected reachability over an interval.
4. **Exchange/flow** — repeated or directed movement of goods, people, or
   other already-modeled flows.

The relation envelope may later carry ritual, authority, defense, service,
care, dependency, or institutional relations. R3 does not interpret those
kinds.

Relation strength is basis-specific. Spatial distance, exchange frequency,
and access reliability are not one universal scalar. A relation therefore
retains its measure and evidence kind; a projection may normalize it only
within its own basis.

Direction is load-bearing. Spatial adjacency is normally symmetric; access,
exchange, authority, dependency, and care may be asymmetric. Each basis must
declare whether it uses weak connectivity, strong mutual connectivity, source
reachability, sink reachability, or reciprocal support.

## 5. District projection

A projection is requested for a basis and interval:

```text
project(relations, basis, interval, configuration)
```

The deterministic pipeline is:

1. Select valid relation assertions for the requested interval.
2. Build the basis-specific relation view.
3. Apply the basis's connectivity and support rule.
4. Form candidate clusters using ordered deterministic traversal.
5. Reject candidates below support or persistence requirements.
6. Annotate overlap, containment, and bridge participants.
7. Compare with neighboring intervals for continuity or recurrence.
8. Optionally hand the successful district graph to Pattern composition.

R3 uses connected components, bounded reachability, or recurrence matching as
appropriate. It does not use a global community-detection optimizer. A
district must be able to explain its membership by naming the relation
evidence that supported it.

### 5.1 Projection result states

The projection distinguishes:

- `resolved` — sufficient evidence supports one or more districts;
- `insufficient_evidence` — candidates exist but do not meet support;
- `contradictory_evidence` — supported interpretations cannot be reconciled;
- `disconnected` — no qualifying relation component exists;
- `transient_only` — a candidate exists only for a sub-threshold interval.

An empty result must not collapse these meanings into one value.

### 5.2 District shape

A projected district carries, conceptually:

```text
district_projection:
    basis
    interval
    members
    boundary/extent
    parent
    overlaps
    bridge_members
    evidence
    continuity
    result_status
```

Containment is acyclic and initially bounded to settlement → district →
subdistrict. Overlap is explicit and may occur between projections with
different bases. A singleton is not a district by default.

### 5.3 Persistence and identity

R3 uses three temporal distinctions:

- **event continuity:** uninterrupted evidence supports a persistent district;
- **recurrence:** repeated windows support a seasonal or periodic district;
- **transience:** a short-lived cluster is visible but not persistent.

Fuzzy structural continuity is deferred. R3 may report structural similarity
as a diagnostic, but it must not infer that two changing clusters are the same
district merely because their member sets overlap.

District identity is projection-local and derived from basis, interval, and
canonical graph anchors. It must not depend on random draws, generation order,
mutable serialized identifiers, or current member ordering. Cross-interval
continuity is an explicit derived relation such as `continues_as`, `recurs_as`,
`dissolves_at`, or `recomposes_into`.

## 6. Ownership and layering

| Concern | Owner |
|---|---|
| Raw settlement, movement, exchange, and cohort facts | Producing system |
| Event ordering and historical validity | History/event substrate |
| Normalized relation assertions | Relation-view adapter |
| Basis-specific clustering | R3 |
| District continuity and recurrence | R3 |
| Spatial Pattern composition | Existing Pattern machinery plus R3 adapter |
| Person, household, kinship, and lifecycle meaning | SOC-household |
| Cultural and institutional interpretation | Later social/institutional layers |

R3 may consume a relation view, but it must not create the relations that
justify its own districts. A district projection is read-only and reversible.

## 7. Synthetic validation societies

The implementation must cover at least these synthetic cases:

- **The Row:** continuous adjacent presence forms one persistent spatial
  district.
- **The Fork:** a bridge locus is annotated without forcing two branches into a
  hierarchy.
- **The Ring:** seasonal movement forms recurring, not continuous, districts.
- **The Reach:** distant exchange forms an exchange district while spatial
  districts remain separate.
- **The Gate:** one-way access does not become reciprocal membership.
- **The Drift:** migration dissolves one projection and creates successors.
- **The Weave:** residential and exchange projections overlap.
- **The Hollow:** unsupported proximity produces no district.
- **The Grain:** cohort-only input produces no person or household facts.
- **The Nested Row:** bounded acyclic subdistrict containment succeeds.
- **The Flicker:** a one-period candidate is transient or refused according to
  support configuration.
- **The False Bridge:** a weak accidental edge does not merge dense groups.
- **The Higher-Arity Relation:** an unsupported multi-participant relation is
  not flattened into misleading pairwise edges.

These cases are tests and probes, not new world canon.

## 8. The Lot readout boundary

The Lot may eventually observe:

- district existence and basis;
- members and aggregate participation;
- spatial extent and adjacency;
- containment, overlap, and bridge relations;
- interval, recurrence, dissolution, and recomposition;
- the evidence supporting a membership decision;
- explicit insufficiency or contradiction.

The Lot may not infer household, kinship, gender, inheritance, or biography
facts from district membership.

## 9. Invariants

- Same inputs and configuration produce byte-identical projection output.
- Projection consumes no seed stream, wall clock, epoch, or ledger mutation.
- Cohort inputs never silently expand into persons.
- Unsupported relation kinds are refused or ignored explicitly, never
  reinterpreted.
- Basis-specific measures are never compared as a universal scalar.
- Directed relations preserve direction through projection.
- Containment remains acyclic and bounded.
- Overlap is explicit rather than erased by forced partition.
- Weak or transient evidence cannot create persistent districts.
- Pattern composition cannot create a district that the relation projection
  refused.
- No R3 output assigns social, biological, reproductive, gendered, or cultural
  meaning to a participant.

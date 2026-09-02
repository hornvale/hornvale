# 0517. The kernel-membership criterion

**Status:** Accepted (2026-09-01) · **Decider:** Nathan · **Campaign:** The Hallmark

## Context

Decision 0044 states the only ratified placement test and scopes it to
coherent physical quantities. Decision 0216 extended it by analogy to one
non-unit roster (`Band`) without generalizing. No record says what belongs
in the kernel as such, and the Entity-Component program (metaplan
2026-07-14 §4.7) will force the question at scale: §4.7 places mechanisms
in the kernel and component registries in domains but is silent on the
types those registries' schemas are made of.

## Decision

A type belongs in the kernel when any clause holds:

- **(a)** More than one domain speaks it today — 0044's clause generalized
  from quantities to any type, qualified by 0216's forced-vs-deliberate
  test: a *forced* duplicate (one side exists only because layering forbids
  the import) qualifies; a *deliberate* projection (deleting one side would
  remove an independent answer, or the two carry different information)
  does not.
- **(b)** It originates in a kernel type — 0044's clause, unchanged.
- **(c)** It appears in the wire schema of a component or relationship
  registered for cross-domain query in the Entity-Component component
  catalog. Dormant until the catalog exists; ratified now so the EC
  campaigns inherit a settled rule. Its enforcement (an architecture test
  asserting every type in a registered schema resolves to
  `hornvale_kernel`) is those campaigns' deliverable.

**Modifier, all clauses — stability graduation:** a volatile type iterates
domain-side; its identity may be reserved kernel-side early; the type moves
when its shape settles. Kernel churn is the most expensive rebuild tier and
promotion is save-format-adjacent, so promotion is deliberate and
occasional, never a reflex.

**The split (0216's formula):** the kernel holds the type, the roster, and
the ordering; the domain holds the meaning — derivations, valences,
behavior — via local traits or free functions (orphan-rule-legal), with
`pub use` keeping call sites source-compatible.

**Refused:** placement by anticipation; closed kernel enums enumerating
per-domain content (adding a domain must never edit the kernel — open
registries carry growing vocabularies); any promotion that changes a
committed spelling without an epoch.

## Consequences

Amends 0044's scope (its mechanics stand). Amends metaplan §4.7 (mechanism
placement stands; wire types gain a rule). Resolves idea-registry row
`DOM-kernel-owns-vocabulary` (raw → ratified). Enforced forward by
`tools/placement-audit` (The Hallmark spec §3): a novelty ratchet on shape
twins, with reconsider-on-touch via shape fingerprints. The tool detects
and demands; it never decides — shape identity is not semantic identity.

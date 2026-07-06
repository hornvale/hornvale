# 0010. Predicate schema: value-kind enforced, subject-kind refused

**Status:** Accepted (2026-07-06) · **Decider:** Nathan

In the context of enriching the concept registry's predicate definitions,
facing the question of how much schema to enforce on facts, we decided that
**`PredicateDef` enforces a `value_kind` (checked in `Ledger::check`, so
mistyped facts are uncommittable) and carries an informational `unit` column,
but subject-kind (domain/range) constraints are deliberately refused**,
accepting that facts can name any subject while their *values* are typed.

**Context.** Value-kind enforcement is the fact-layer twin of typed quantities
([0008](0008-typed-quantities.md)) — it catches malformed facts at commit time.
But domain/range constraints on subjects are the ontology trap in another guise:
they would rigidly couple the fact layer to a fixed taxonomy of subject types,
exactly the brittleness [0002](0002-domains-depend-only-on-kernel.md) exists to
avoid.

**Consequence.** A serde default keeps old saves loadable. Property-flag
predicates follow the ratified naming rule (bare adjective, asserted only when
true; `tidally-locked` is the exemplar). The unit column is rendered in the
generated registry table.

**See also.** Campaign 2 close registry review; concept-registry reference
chapter.

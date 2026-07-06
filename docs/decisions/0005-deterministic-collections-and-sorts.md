# 0005. Deterministic collections and sorts

**Status:** Accepted (2026-07-05) · **Decider:** Nathan

In the context of enforcing determinism ([0001](0001-determinism-is-constitutional.md))
across everything serialized or ordered, facing the fact that `HashMap`
iteration order and naive float comparison are nondeterministic, we decided
that **no `HashMap` appears in anything ordered or serialized (`BTreeMap`/`Vec`
only), and float sorting uses `total_cmp` with explicit tie-breaks**, accepting
the minor ergonomic cost over `HashMap` and `sort_by`.

**Context.** A single `HashMap` in a serialized path produces worlds that
differ run-to-run despite identical seeds — a catastrophic determinism bug that
passes casual testing. `HashSet` is permitted only for membership checks that
never feed ordered output.

**Consequence.** Registry and ledger structures are `BTreeMap`-backed;
aggregation in the Lab is `BTreeMap`-ordered; all float sorts are `total_cmp`.
Reviewers grep for `HashMap` in new ordered/serialized code as a matter of
routine.

**See also.** Spec "Determinism"; `CLAUDE.md` "Determinism" section.

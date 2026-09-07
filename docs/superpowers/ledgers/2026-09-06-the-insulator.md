# The Insulator — decision ledger

**Status:** active; design and implementation plan approved on 2026-09-06.

## #1 [G3] Measure before restructuring

The campaign will first measure the current contributor build closure and only
prototype an island after the baseline identifies a narrow observer boundary.
The root correctness gates remain unchanged, and no production dependency split
is authorized by this ruling.

**Why:** The Counterpart found no selection benefit over Cargo. Compilation
and invalidation cost are the next untested source of process value.

**Capture:** Spec and plan are the Insulator design and implementation plan in
docs/superpowers/specs/ and docs/superpowers/plans/.

## #2 [G3] Use a measurement cell instead of a generic command runner

The recorder will execute only frozen named workloads inside a platform
enforced cell: the checkout is read-only, the target and evidence roots are
writable, and every other write is denied. It refuses before launch when the
host cannot enforce that policy and tests both denied and permitted writes.

**Why:** Three repair rounds showed that declared ownership did not enforce
anything for an arbitrary subprocess. A named workload plus an enforced cell
keeps the measurement claim smaller than the mechanism.

**Capture:** This ruling amends the Insulator spec and implementation plan.

## Follow-ups

- Baseline the existing tools/digest contributor graph and costs.
- Qualify one candidate boundary on Mac and lefford if baseline evidence
  supports it.
- Record a negative result without admitting an island if the boundary adds
  authority, loses output identity, or produces no repeatable reduction.

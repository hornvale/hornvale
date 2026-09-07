# The Insulator — decision ledger

**Status:** closed on 2026-09-07; qualified rejection, no production admission.

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

- Baseline the existing tools/digest contributor graph and costs — **resolved**
  by `results/baseline.json`, with 54 packages, 5 workspace members, and
  separate Mac cold/warm preparation, build, and test costs.
- Qualify one candidate boundary on Mac and lefford if baseline evidence
  supports it — **resolved as scoped rejection** by `results/comparison.json`;
  the complete Mac mismatch made Linux unnecessary and it is recorded as
  `not_run`/`not_required`.
- Record a negative result without admitting an island if the boundary adds
  authority, loses output identity, or produces no repeatable reduction —
  **resolved** by the candidate manifest, README, comparison, retrospective,
  and registry row. The candidate's 31-byte output cannot replace the
  authoritative 7,493-byte publication.

## #3 [G6] Close on qualified rejection

The campaign closes with the measurement cell, baseline, candidate manifest,
comparison, chronicle, retrospective, reconciled audit rows, and registry
evidence committed together. The candidate is rejected, and no production
dependency split or selective gate is admitted.

**Why:** The Mac record is complete and the output mismatch is decisive. The
candidate would need to import or duplicate `windows/lab` publication authority
to preserve the bytes, which violates the experiment's declared boundary.

**Alternatives discarded:** Running Linux after a decisive product mismatch
would add host evidence without changing the disposition; admitting the small
graph would turn a protocol probe into an unverified second authority.

**Capture:** The chronicle is wired in `book/src/SUMMARY.md`; the retrospective
records repairs and deferred work; the spec, audit row, and registry point to
the committed evidence under `tools/digest/experiments/the-insulator/`. The
completed implementation plan was removed under the project plan-hygiene rule.

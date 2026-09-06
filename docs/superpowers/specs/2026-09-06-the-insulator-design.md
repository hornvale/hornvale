# The Insulator: measured build boundaries for contributor tools

## Status

Draft approved in conversation on 2026-09-06. This specification defines a
bounded experiment. It does not authorize a production dependency split or a
change to the root correctness gates by itself.

## Question

Can Hornvale reduce the cost of building contributor-facing tools by isolating
an observer from dependencies it does not need, while preserving the current
product outputs and correctness boundaries?

The Counterpart found no demonstrated benefit from replacing Cargo's
dependency closure with a richer semantic selector. The next measured seam is
therefore compilation and preparation cost. The experiment begins with the
existing `tools/digest` workspace, whose contributor packages reach into the
simulation workspace and whose census publication package depends on
`windows/lab`.

## Hypotheses

The current contributor build closure contains an observer dependency that is
larger than the data it reads. A smaller build island may reduce cold and warm
build cost and reduce invalidation after unrelated edits.

The result may be negative. If the proposed island needs duplicated production
logic, creates a second authority, fails to preserve output bytes, or does not
produce a repeatable cost reduction, the island will not be admitted. The
existing Cargo graph and full correctness gates remain authoritative in either
case.

## Scope

The campaign measures and, if justified, prototypes one narrow contributor
tool boundary. It includes:

- the current `tools/digest` package graph and its transitive workspace reach;
- cold and warm builds on a Mac and on the canonical Linux host;
- invalidation after representative source, protocol, and unrelated workspace
  edits;
- byte and semantic comparison of baseline and candidate outputs;
- preparation, compilation, test, queue, and authoring costs as separate
  measurements;
- guards that keep the proposed boundary explicit and fail closed on drift.

It excludes:

- changes to simulation behavior or committed world/golden outputs;
- a new production semantic selector or verdict cache;
- permission to skip the root correctness gate;
- claims about natural contributor behavior, universal build throughput, or
  hostile authors;
- a general workspace restructuring before one boundary is measured.
- arbitrary command execution without a named workload and an enforced write
  boundary.

## Baseline and candidate

The baseline is the current tree at a frozen commit and merge base. The
controller records Cargo metadata, package and transitive dependency counts,
changed-package closure, target policy, host and toolchain identity, and the
outputs of representative digest commands.

The candidate is an experimental build island around the narrowest observer
that currently reaches `windows/lab`. The island may introduce a protocol
adapter or a data-only boundary. It may not copy production behavior into a
second implementation. The existing authoritative implementation remains the
comparison path. Every execution is selected by an identifier in the frozen
workload registry; the recorder does not accept arbitrary caller commands as
measurement authority.

Baseline and candidate runs are paired by commit, workload, host class, target
policy, and run order. Cold runs use an owned empty target and an explicitly
recorded dependency preparation step. Warm runs reuse only the target allowed
by the experiment contract. Preparation time is not folded into compilation
time, and queue time is never presented as build time.

## Measurement record

Each attempt retains a manifest containing:

- source commit, merge base, tree identities, toolchain, host class, profile,
  features, and Cargo lock identity;
- dependency graph identity, package counts, and invalidation set;
- command, exit status, deadline, bounded stdout/stderr, and cleanup result;
- cold or warm classification and preparation/build/test durations;
- output paths, sizes, hashes, and byte comparison results;
- queue and authoring costs, recorded separately or explicitly unavailable;
- failure reason and whether the attempt is valid evidence.
- canonical checkout, target, evidence roots, and the host enforcement method;
- the named workload identifier and its frozen command template.

No failed or incomplete attempt may be omitted. A failed preparation is an
operational failure, not a behavioral result. A product output mismatch is a
candidate failure until independently adjudicated.

## Success and rejection

The candidate can be recommended only if all conditions hold:

1. paired measurements show a repeatable reduction in the chosen contributor
   build or invalidation cost after baseline variation is accounted for;
2. baseline and candidate outputs agree byte-for-byte where the contract says
   they should, with any intentional representation difference documented;
3. representative relevant edits invalidate the candidate, while unrelated
   edits do not cause unexplained rebuild reach;
4. the candidate has one declared dependency boundary and no duplicated
   production authority;
5. Mac and canonical Linux records are complete, independently reviewable,
   and reproducible;
6. a repository guard detects an undeclared dependency or stale comparison
   artifact.

No numerical performance target is chosen before the baseline exists. A
negative result is successful qualification of the hypothesis and will be
recorded without admitting the candidate.

## Failure handling

The experiment fails closed when a required input, source identity, workload
record, output, cleanup result, or comparison is missing. An invalid run is
retained with its logs and owned paths. The controller stops later sampling
after uncertain cleanup rather than reusing a possibly contaminated target.

The execution wrapper makes the checkout read-only and grants writes only to
the owned target and evidence roots. On macOS it uses `sandbox-exec`; on Linux
it uses `bwrap`. It canonicalizes roots before creating the policy, tests both
blocked and allowed writes, and refuses before launch when the host cannot
provide the required enforcement. A path declaration by itself is never
treated as an isolation guarantee.

The candidate is rejected if it imports outside its declared boundary, relies
on an undocumented generated file, changes simulation outputs, or requires a
second implementation of an authoritative rule. These are design failures,
not reasons to loosen the comparison.

## Stages

### Stage 1: Baseline closure

Map the current digest and gate paths and collect paired cold/warm baseline
records. Freeze the workload, source identities, output contract, host
metadata, and analysis rules before candidate measurements.

### Stage 2: Island prototype

Build the smallest candidate boundary in an isolated worktree. Add tests for
dependency admission, invalidation, output identity, and absence of duplicated
authority. Keep the candidate outside production behavior until qualification.

### Stage 3: Two-host qualification

Run the frozen baseline and candidate panels on Mac and lefford. Retain all
attempts, inspect every output diff, and independently review the manifests,
source identities, dependency graph, and cost decomposition.

### Stage 4: Decision and integration

If the candidate satisfies the success conditions, prepare the smallest
tooling change and its guards for the normal stage and merge queues. If it
does not, publish the measured negative result and route any remaining ideas
to the registry. In both cases, the root full correctness gates remain
unchanged.

## Required artifacts

The campaign must leave a committed experiment README, baseline and candidate
manifests, raw bounded attempt records, output comparison, focused tests,
implementation or rejection report, retrospective, and registry updates.
The final review must verify that every conclusion names its source record and
that preparation, execution, queue, and authoring costs have not been merged
into one misleading total.

## Limits

This experiment cannot establish that a smaller build is always faster, that a
dependency graph captures semantic influence, or that a clean owned worktree
models concurrent authoring. It cannot authorize selective verification,
portable verdict reuse, or a general federation protocol. Those require
separate evidence.

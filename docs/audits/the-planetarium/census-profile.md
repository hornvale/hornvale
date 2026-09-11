# The Planetarium — census alarm attribution

The 2026-09-11T01:54:47Z census took 1366.487 s. Its 1320 s alarm triggered
a live inspection of the largest component at the same source revision,
`fa1223fd7e82d059b61788506f90abcaf271dc9c`. The inspection supports closing
this run's profiling referral with the following scope; it does not prove the
whole pipeline free of regressions or justify changing either timing limit.

## Actual measurement

On lefford, the operator ran `hornvale lab run studies/the-census.study.json`
under `perf record -F 99 -g --call-graph fp`, using a separate release build
with frame pointers enabled. The perf header confirms the executable, command,
host and 99 Hz setting. The worktree HEAD equals the census ref. The study
completed 1000 rows with zero refusals, capturing 2,789,455 samples with zero
lost samples. The header's sample interval is 738.886033 s; the operator
reports 744 s process wall and rc=0. These are distinct timing boundaries.

The profiled executable's build ID is
`af0930c22b4d47877f73f5d3e7643057e2702226`; SHA-256
`25667f088bc1113be84d1bb9974671b265bca4efb9ada573c514a1df6e04b8bf`.
No rendering build or capture provenance is changed.

The reported symbol table's largest self-time entry is `Fbm::sample` at
4.71%, followed by memory movement at 4.17% and `libm::exp` at 3.91%.
Inclusive cost includes `runner::build_row` at 82.83%, metric extraction at
49.61%, and `lot::context::assemble_from` at 12.26%. The symbol and DSO
reports are archived, including the allocator, route and mathematical work
below those entries. This is distributed work in the measured workload;
there is no single dominant self-time symbol.

## Limits and interpretation

This profiles **only the main census study**. It excludes
`census-of-the-meeting`, the chorus, generators and remaining pipeline work.
The original run's main-study measurement was 715.525 s, rather than the
full 1366.487 s alarm duration. The instrumented repeat is not a matched
speed comparison with the original release build.

Flat self-time does not exclude an expensive inclusive subtree, distributed
quadratic work, or a scaling defect that needs several input sizes to expose.
It also does not prove noise sampling irreducible. No regression was isolated
by this inspection, and no optimization or threshold change is made here.
Total CPU in the original complete pipeline was 42144.560 s, compared with
42220.324 s in the preceding run (-0.179%). That is useful context, not a
substitute for the live inspection.

The operator's proposed alarm recalibration is parked under
`PROC-census-budget-denominated-by-cpu-ratio`. The claim that 46.487 s of
overage lies inside a cited 34.365 s spread is not adopted; neither is the
claim that this is the first profiled yellow-log row. Earlier live profiles
are already recorded in that log.

## Preserved evidence and delivery

The durable directory
`/Users/nathan/Downloads/Hornvale Planetarium/census-profile-fa1223fd7/`
contains raw `perf.data`, the matching executable, symbol/DSO reports, run
log, full operator note, header/build IDs, source status and remote hashes.
All five copied original artifacts matched their remote SHA-256 values.
The eight-file manifest's SHA-256 is
`b6f6e90c18442496b77b5bbe67d684e16d65109f8bbc95df7e297b019904ea66`.
The 5.2 GiB derived `perf.script` remains on lefford; raw data and reports are
retained locally. External system-library binaries are not archived here.

The profile worktree's sole tracked change removes the `backfilled: true`
marker from the main study schema; this is recorded and not imported.
The original census delivery `88e8aeb7ff0e37dc13c996cd9512a8dc9bf17681`
contains six timing rows and two operational file-count changes, with no
scientific golden movement. That original delivery is incorporated alongside
the per-run yellow-log finding, through the normal commit hook.

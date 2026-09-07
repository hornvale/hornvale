# The Insulator — retrospective

The Insulator asked whether a narrow contributor build island could reduce
build invalidation while preserving the authoritative product output. It
ended with a valid rejection. The candidate could depend on
`digest-protocol`, but its publication probe emitted 31 bytes; the
authoritative `digest-census-publication` emitted 7,493 bytes. The recorded
hashes differ. Making those bytes agree would require importing or duplicating
the publication rules owned by `windows/lab`, which violates the proposed
boundary.

The evidence is asymmetric by design. Mac has the complete cold and warm
authoritative baseline, including preparation, build, test, graph, source,
cleanup, and output records. The candidate has its source, dependency,
authority scan, build result, and output mismatch. Canonical Linux was not run
because the Mac mismatch already decides the product question; it is marked
`not_run` and `not_required` in the comparison dossier rather than invented as
a result.

The measured baseline gives a useful cost shape even though it does not
support admission: cold publication was 35.923 seconds and warm publication
was 0.381 seconds; cold Thing was 1.423 seconds and warm Thing was 0.362
seconds. These are Mac observations for this frozen tree and workload set.
They do not establish a universal build improvement, a Linux cost, or a
benefit from any future candidate.

The process changed in three durable ways. The measurement cell now enforces
the write boundary at the host, so a path declaration is not mistaken for
isolation. Qualification records source identity, cleanup, graph identity,
output identity, and failure evidence before allowing a decision. The
decision helper treats a negative result as valid evidence while keeping
`admit` and `reject` as the only product verdicts. This keeps a rejected
hypothesis useful without letting a smaller graph become a product claim.

The repair history is part of the result. Early recorder versions accepted
declarative ownership without enforcing it, so the design was recast as a
named-workload measurement cell. The cell then required canonical roots,
read-only checkout handling, permitted-write tests, bounded timeout cleanup,
and platform-specific enforcement. macOS needed a narrow `sysctl-read` rule
for Rust and a constrained `mach-lookup` rule for `com.apple.bsd.dirhelper`;
the broad lookup permission was removed. The baseline work also repaired
reverse Cargo dependency traversal, path dependency discovery, cold-target
symlink handling, graph identity, and separate preparation/build/test costs.
Qualification repairs made persisted manifests carry workload and enforcement
provenance, retain failed phase evidence, require complete paired evidence, and
point the comparison at the committed candidate manifest.

The campaign also exposed limits that remain open. The candidate was not
qualified on Linux, repeated candidate savings were not measured, and the
invalidation matrix contains authoritative observations rather than a
two-host candidate series. A future attempt needs a new candidate that
preserves the publication authority through an explicit accepted API, then
must repeat the frozen Mac/Linux qualification. This campaign does not
authorize selective verification, a semantic selector, a verdict cache, or
production restructuring.

Deferred work has a defined outcome. Linux candidate execution and repeatable
candidate performance were unnecessary after the decisive Mac output mismatch
and remain open only for a future candidate that preserves publication
authority. A future candidate must expose that authority through an explicit
API, repeat the two-host qualification, and retain machine-readable attempt
evidence. No other minor or parked review finding remains open; the sandbox,
schema, graph, path, timeout, provenance, and evidence findings were repaired
and re-reviewed during Tasks 1–4.

The full evidence is in `tools/digest/experiments/the-insulator/results/`:
`baseline.json` is the combined Mac dossier, `mac-cold.json` and `mac-warm.json`
retain the raw panels, and `comparison.json` records the decision and the
unrun Linux scope.

# The Counterpart: frozen questions and raw evidence

This is a development experiment, outside the simulation workspace. It adds no
Digest enrollment, selector, cache, scheduler, admission privilege, or SLO.
All four questions in `contract.json` remain obligations on every valid arm.
Unknown changes and missing enrollment fall back to all questions. Refusal is
an observed subject result; missing data or an interrupted process is an
incomplete attempt. Satisfied checks do not prove unnecessary work.

## Writers and shared inputs

The campaign controller owns coordination, provenance, and the final dossier.
The foundation author owns this frozen question map, raw observer, and reusable
Charter measurement module. A separate checker author derives the four checks
from this contract and actual raw facts; its [derivation and disclosures](checker-derivation.md) are retained separately. Later owner-record and specimen authors
own their declared records and reconstructible specimens; the controller must
record their identities and freeze commits before revealing the reserved
compiling challenge. This foundation creates no specimens or checker logic.

Material shared inputs are the approved Counterpart task brief and shared schema,
`contract.json`, `domains/CLAUDE.md`, `domains/thing/src/lib.rs`,
`domains/settlement/src/lib.rs`, `kernel/src/registry.rs`, and the existing
Charter collector in `tools/digest/packages/thing/src/lib.rs`. The harness is
extracted from `scripts/charter-measure.sh`. This shared context is disclosed;
there is no security-blindness claim. The checker must not consume the candidate
contribution or Charter verdicts as an authority.

## Raw observer

Run `cargo run --manifest-path tools/digest/Cargo.toml -p digest-counterpart`.
The executable prints exactly one JSON object on stdout and exits zero after a
complete observation, including expected registration refusal. Unexpected roster
observation faults escape and fail the process. Rust panic hooks may print
refusal diagnostics on stderr; the harness preserves those bytes separately.

`digest_counterpart::snapshot() -> serde_json::Value` uses the verified production
APIs `ConceptRegistry::default()` and `digest_thing::contribution() ->
Result<Contribution, String>` (the design brief's `new` and `collect` names do
not exist in this checkout).

The top-level keys are `schema` (`counterpart-v1`), `facts`, and `candidate`.
Facts contain exactly these fields:

- `source_kinds`, `component_kinds`: arrays of observed names retaining duplicates.
- `borrowed`: array of `{name, owner}` declarations retaining duplicates.
- `before_concepts`, `after_concepts`: arrays of `{name, owner}`, sorted by name
  and owner, observed before and after Thing registration.
- `settlement_registration`, `thing_registration`: `{outcome, detail}`; outcome
  is `accepted` or `refused`, with original returned error or panic detail.

Settlement and Thing registration are separately caught. The partial registry
is still observed after refusal; the checker must report ownership unknown when
registration did not complete. Other questions may retain sufficient facts.
Candidate collection runs separately and yields `{outcome, detail, contribution}`;
contribution is the unchanged existing contribution or null after error/panic.
Candidate data never becomes facts. No simulation or golden changes are made.

## Process evidence and limits

`scripts/charter_measure.py` exposes the legacy ownership and measurement helpers
without installing handlers or performing work on import. The shell wrapper
resolves its own directory. Normal CLI measurements verify both executed source
files against committed HEAD. Legacy self-tests run with `--self-test`.

`measure(command, cwd, *, retain_output=False, output_limit_bytes=None,
deadline_seconds=3600)` retains legacy defaults and fields. With retention, exact
stream bytes are base64 encoded, both sizes and SHA256 hashes are recorded.
The optional limit applies independently to each stream; polling interrupts an
oversize writer and a final size check also invalidates fast oversize exits.
The bound is an observation/termination threshold, not a hard disk quota: a
writer can overshoot between polls or during cleanup. Captured bytes are never
silently truncated. Trusted children may not detach into unrelated sessions.
Uncertain cleanup retains files and the owned checkout; `record_sample` emits
the failed attempt before refusing further work. Callers using `measure` directly
must persist the result before stopping on cleanup failure. Timing/RSS are the
existing whole-process-phase observations, not aggregate concurrent-tree memory.

Only immutable owned checkouts with no concurrent writers are valid experiment
inputs. Before/after hashes establish integrity, not atomic capture. Canonical
minutes-scale runs use the ordinary serial claim; this package is not a census
entry point. The existing gates and Digest protocol v1 remain in force.

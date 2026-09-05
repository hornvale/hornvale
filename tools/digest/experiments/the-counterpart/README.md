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

## Shadow rules and bounded replay

`compare.py` proposes path, Cargo-reachability and owner-agreement question
sets using recorded inputs only. The path rule preserves absent enrollment.
Cargo uses the full `cargo metadata --locked --offline --format-version 1`
graph of the outboard Digest workspace, including its production dependencies,
for both the base and changed source. Package membership uses the deepest
manifest directory containing a changed Rust source or Cargo manifest; the
repository root is recorded separately because the outboard workspace root
is not the source repository root. Deleted packages can be mapped in the base.
Unresolved membership and non-Cargo inputs fall back to all four questions.

Agreement normalization is finite, manually integrated experiment work:
Thing variants declare source locators and addition deltas; Settlement variants
declare concept names and a complete post-variant supplied roster, compared
with its baseline supplied roster. The rules recognize Thing's `unclaimed:`
and `lender:` prefixes and Settlement's explicit `<name> is not claimed by
Thing or any earlier registration` sentences. Selection evidence retains
normalization modes and original negative-assumption quotes. Missing named
assumptions or unknown subjects remain unknown, with full fallback. Predictions
and author observations are never selector inputs. The supplemental imports-only
view deliberately omits negative assumptions, retains its raw omissions, and
still falls back to all four obligations; it is not a fourth primary selector.

Run focused checks without source compilation:

```sh
python3 tools/digest/experiments/the-counterpart/run.py --self-test
python3 tools/digest/experiments/the-counterpart/run.py --help
```

A full replay takes an immutable panel and creates a new owned directory:

```sh
python3 tools/digest/experiments/the-counterpart/run.py \
  --panel tools/digest/experiments/the-counterpart/panel.json \
  --output /absolute/new/owned-counterpart-run
```

Relative panel inputs resolve beside the panel. Existing output directories
are refused. The panel records exact source commits/trees, patch hashes, lock,
pin and source-input hashes, owner/patch file hashes, bundle identity and the
implementation commit. The implementation commit precedes the panel and
observations, avoiding a self-referential source identity. Source objects are
frozen-base-plus-patches; the checker and comparison implementation have
separate identities. `construct_sources` authors only owned experimental
`commit-tree` objects with the frozen base as parent, checks each patch's
scope/non-noop effect, and rejects textual conflicts without repair. Its
bundle excludes the explicit frozen-base prerequisite, which must already be
reachable in the replay repository. No experimental source object is admitted
to production.

The runner imports the bundle into an owned clone with scrubbed Git path
variables. It reuses that clone and one owned target sequentially, switching
source only between finished commands. It records preparation and observation
separately; queue wait and author labor are unavailable to this runner and
remain null instead of fabricated timings. Toolchain and allowlisted material
environment are recorded. Each measured command uses the reviewed Charter
session supervisor with a 3600-second deadline and independent 16-MiB stream
thresholds. The threshold can overshoot; full failed bytes remain in the owned
attempt directory. Do not commit unbounded failure output: retain that directory
and commit a bounded receipt naming it and its hashes.

Suggestions are written before builds or observations. Every completed arm
executes all four checker questions from raw facts, with candidate outcomes
separately labeled. Attempts and intermediate manifests are persisted before
reacting to cleanup uncertainty; interruption, failure, invalid JSON, output
bounds or stale inputs prevent aggregate completion. The runner retains its
owned directories for inspection. `summarize` requires the entire declared arm
roster, exact question results, raw stream hashes and failure flags, and
re-evaluates the raw facts. A captured production registration refusal is a
complete negative observation, not a harness crash or a safe-source verdict.

Full minutes-scale panels run under the canonical serial claim. A deliberately
smaller qualification must use a separately retained derived panel with explicit
scope and parent-panel SHA256, never delete results from the full dossier and
claim it completed. Neither the finite panel nor a passing selection proves
that satisfied checks are unnecessary, or that production tests can be skipped.

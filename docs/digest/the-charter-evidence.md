# The Charter: composition and quality evidence

Status: **integrated Mac functional and cost evidence recorded; independent
usability PASS; canonical qualification and Linux cost evidence pending**. This is an
evidence ledger, not an overall passing verdict. The binding acceptance properties
are [design §7–8](../superpowers/specs/2026-09-04-the-charter-design.md).
The [tool guide](../../tools/digest/README.md) explains the actual command and caps.
Saved reports are examples; production context never reloads them as fresh evidence.

## Sources and independent enrollment

| Item | Exact source / observation |
|---|---|
| Frozen bootstrap and both branch merge bases | `6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f` |
| Task 5 preparation base | `28fafd81c14ae08cfc686b30199264d9d4e47187` |
| Reviewed Thing commit | `275bc5d2b31374ec98dfa3bb8fd8936c8b96b5fd` |
| Reviewed census commit | `11edfaabe918b168e74ac669fe9e851a18b9cbc3` |
| Peer merge base | `6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f` |
| Reviewed diagnostic cleanup/evidence fix | `9294b9ca765f83e82bd8ac1054053b0aff0163e0` |
| Composed implementation and functional qualification source | `e3355f441db045f0960576ead12b0f8acc56ca7d` |
| Measured Mac script/host SHA | `e3355f441db045f0960576ead12b0f8acc56ca7d`; 36 samples, 0 failures |
| Canonical diagnostic candidate SHA | `9ea2dc9abb38970c03a5b536645d9d8429b3d37a`; request `req-9ea2dc9abb38-20260905T041951Z` |
| Canonical tested merge-product SHA and complete log | Pending completed queue job |
| Diagnostic-only diff | Exactly five added lines in `scripts/lane-outboard.sh`; reviewed diagnostic script unchanged, exact diff below |
| Later evidence documentation commit | Pending; distinct from measured source |

The controller recorded these full branch comparisons in
`independent-branches.json` in campaign scratch. Exact Git comparisons:

```sh
git merge-base 275bc5d2b31374ec98dfa3bb8fd8936c8b96b5fd 11edfaabe918b168e74ac669fe9e851a18b9cbc3
git diff --name-only 6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f..275bc5d2b31374ec98dfa3bb8fd8936c8b96b5fd
git diff --name-only 6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f..11edfaabe918b168e74ac669fe9e851a18b9cbc3
```

The complete Thing changed-file roster is:

```text
tools/digest/Cargo.lock
tools/digest/packages/thing/Cargo.toml
tools/digest/packages/thing/README.md
tools/digest/packages/thing/src/lib.rs
tools/digest/packages/thing/src/main.rs
```

The complete census changed-file roster is:

```text
tools/digest/Cargo.lock
tools/digest/packages/census-publication/Cargo.toml
tools/digest/packages/census-publication/README.md
tools/digest/packages/census-publication/src/lib.rs
tools/digest/packages/census-publication/src/main.rs
```

Both independently reviewed branches added only their package and lockfile.
No adopter changed semantic host/protocol/source dispatch. Their only common
tracked path was the permitted Cargo.lock. Ordinary hook measurements were
retained separately in `thing-timings.patch` and `census-timings.patch`, then
reconciled by the controller: Thing prewarm 419.445 s and commit gate 88.033 s;
census prewarm 411.766 s and commit gates 94.898/54.121 s. Those are workflow
costs, not context samples. Controller main absorption and diagnostic fixes are
separate work, not concealed adopter changes.

The joined lock was resolved through Cargo. A Python `tomllib` comparison of
`git show 6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f:tools/digest/Cargo.lock`
with the integrated lock found **all 22 external package records identical**,
including versions, sources, checksums and dependency rosters. No local package
was removed. The original local packages were `digest`, `digest-protocol` and
`hornvale-kernel`; these **27 local packages** were added:

```text
digest-census-publication digest-thing hornvale-alchemy hornvale-almanac
hornvale-astronomy hornvale-book hornvale-climate hornvale-culture
hornvale-demography hornvale-explain hornvale-hearsay hornvale-historiography
hornvale-history hornvale-lab hornvale-language hornvale-locale
hornvale-paleoclimate hornvale-person hornvale-religion hornvale-scene
hornvale-settlement hornvale-species hornvale-terrain hornvale-thing
hornvale-topology hornvale-vessel hornvale-worldgen
```

This is dependency/build expansion, not a claim that context executes those
packages' simulation entry points. The actual observations call registry and
pure guard APIs; they construct no world and run no census.

## Commands and integrated verification

The controller ran these exact commands sequentially, exit 0:

```sh
HV_TEST_OK=1 cargo test --manifest-path tools/digest/Cargo.toml --workspace --locked --offline
cargo clippy --manifest-path tools/digest/Cargo.toml --workspace --all-targets --locked --offline -- -D warnings
cargo fmt --manifest-path tools/digest/Cargo.toml --all --check
make context-prepare
```

`HV_TEST_OK` satisfied a local shell guard that misclassified this outboard
`--workspace`; it did not omit tests. `composed-tests-final.log` records **95
passed, 0 failed**: Digest library 52, CLI integration 13, census library/binary
7/2, protocol 12, Thing library/binary 7/2. Doctest targets contain zero tests.
The clippy log ends with successful completion; fmt emitted no diagnostics.
These supplied logs were read, not rerun during functional qualification.

Fresh functional qualification used the actual compiled host and contributors
at source `e3355f441db045f0960576ead12b0f8acc56ca7d`:

```sh
tools/digest/target/debug/digest context domains/thing
tools/digest/target/debug/digest context windows/lab/src/publish.rs
tools/digest/target/debug/digest context .
tools/digest/target/debug/digest-thing collect --repo-root "$PWD"
tools/digest/target/debug/digest-census-publication collect --repo-root "$PWD"
```

All five commands exited 0. The reports correctly disclose a dirty primary
checkout: the controller's existing timing row was uncommitted. Its tracked
diff was byte-identical before/after qualification. No primary source, lock,
artifact, index or timing row was modified by these experiments. The doc itself
was edited only after that comparison completed.

| Captured output | Exit | Bytes | SHA-256 |
|---|---:|---:|---|
| Thing scoped report | 0 | 4078 | `7301667690c7a17ed2ef4f5a419a5412ee37310919aca7d3a0dcd5bb5a8b99e7` |
| Census scoped report | 0 | 7756 | `d85baf39b9a14a335b3ccce4fe41c8aec59419c791726318b29fa9de0939479d` |
| Combined scoped report | 0 | 11738 | `b12314cc0090f7403f1c86c92d2591c3c49ab5b33c14bb2390be0a3c7c44a47f` |
| Thing envelope | 0 | 3722 | `bbe284ed770269ae47d5a135681874200f0b816fd904481d0970033cd4aafbcf` |
| Census envelope | 0 | 7493 | `9571178d3943687ed348ad3f0f6fed783e967329286789ad58f0a2e595063306` |
| Isolated roster-disagreement report | 1 | 4071 | `9e95b3423a6871fdaf18cf19fe2646fdf734b18272a308d629d993299aa47a1e` |

Thing's actual envelope contains three satisfied observations: registration,
component-roster agreement and concept ownership. Census has eight satisfied
observations: seven named allowed/refused guard cases plus source-host agreement.
Its queued-authoring requirement is explicitly **authored only (unchecked)**;
the pure predicate does not establish that queue authoring happened.

### Recomposition and fresh-input counterexample

A temporary Rust caller used the existing public signature
`digest::context::compose(&CheckoutContext, &[Contribution])`, with fixed
`CheckoutContext { revision: "e3355f441db045f0960576ead12b0f8acc56ca7d".into(),
dirty: true }`. It parsed the two actual envelopes, composed the pair twice and
in reverse contributor order, asserted equality of complete `ContextReport`s,
compared the serialized inputs before/after, and checked that every original
observation detail string remained verbatim in Markdown. Output:

```text
actual_envelopes=2 both_orders_equal=true repeated_identical_equal=true input_order_unchanged=true semantic_detail_text_preserved=true bytes=11738
real_combined_cli_equals_public_compose=true
```

The temporary harness used its own Cargo workspace/target, depended on the
existing Digest/protocol paths, generated its own lock offline and built with
`--locked --offline`. This fixture calls public composition directly; it does
not add a saved-envelope input route to production context. The source and
exact argv are retained in campaign scratch under `functional-compose/` and
`functional-commands.json`.

A fresh owned detached checkout at the same committed source retained the Rust
pin and began without `tools/digest/target`. The already-built primary host was
invoked from that checkout with `context domains/thing`; it built only the
selected Thing contributor there and produced a successful baseline. The
fixture then removed exactly one `"loom",` entry from the live
`domains/thing/src/lib.rs` `THING_KINDS` roster, leaving the real component
registry unchanged. Assertions verified the replacement occurred before the
same host command rebuilt/recollected from the fixture. The compiler completed
successfully; the host exited 1 with an actionable report:

```text
hornvale.thing:component-roster — contradicted
extra in component registry: loom
Result: **not satisfied**
local_roster_mutation: compiled=true registration=satisfied component-roster=contradicted concept-ownership=satisfied
```

The report names `hornvale.thing:registry-contract` and the owning Thing,
Settlement and domain instruction sources. This is a behavioral contradiction,
not compilation failure or dirty-state-only output movement.

Next, only the adapter's live `REQUIREMENT_STATEMENT` in that same failing
fixture was changed to `The ownership requirement is satisfied.`. Rebuild and
recollection still exited 1, the new statement appeared, and the entire actual
observation array remained equal to the prior failing envelope:

```text
authored_assertion_change: applied=true observations_unchanged=true report_exit=1
owned_fixture_removed_after_process_cleanup=true
primary_tracked_diff_unchanged=true
```

Every fixture command ran in an owned session using the reviewed session/nested-
group cleanup before its checkout was removed. No prewarm, broad simulation
suite, world construction, live census or benchmark was part of this experiment.
Exact source mutations are recorded in `functional-fixture-mutation.diff` and
`functional-fixture-final.diff` in campaign scratch.

Production saved-report refusal was exercised separately:
`digest context domains/thing <saved-report-path>` exited 2, stdout empty,
stderr `usage: digest context <scope>`. It cannot replay that saved report.

### Existing renderers and generated bytes

Actual `digest render doctor`, `digest render decisions`, and `digest render
delta` commands all exited 0. Doctor emitted 1560 bytes. Decisions emitted
25538 bytes, exactly equal to committed `docs/digest/decisions-in-force.md`;
delta emitted 487 bytes, exactly equal to committed
`docs/digest/intent-vs-reality.md`. This verifies those current renderer paths
and two actual artifact identities; it does not certify every generated file.
The controller separately regenerated after main absorption and observed no
Book/fixture/audit content drift, only inventory counts. Joined canonical
artifact validation remains pending.

## Acceptance properties

| Design §7 property | Command / evidence | Observation and limitation |
|---|---|---|
| Independent enrollment | Exact branch comparisons and complete rosters above | Both reviewed branches share the frozen bootstrap and edit only their package/lock; actual join collected both without semantic host edits. |
| Real Thing evidence | Integrated suite: `missing_lender_refuses_registration`, `wrong_lender_refuses_registration`, `missing_owned_kind_fails_ownership_check`, `extra_registry_kind_fails_reverse_roster_check`; real fixture above | All passed. Real rebuild removed a roster entry and contradicted the reverse comparison; authored success text did not change observations. No worldgen/save claim. |
| Guard discrimination | Census tests `finite_panel_rejects_an_always_accepting_guard`, `finite_panel_rejects_an_always_refusing_guard`, `source_host_disagreement_is_an_actionable_collection_error`; actual census envelope | All passed; eight actual observations satisfied. Relative suffix and census-of prefix omission tests also passed. No queue execution claim. |
| No vacuous success | Protocol/CLI tests including `missing_required_observation_is_rejected`, `unknown_protocol_version_is_rejected`, `malformed_missing_and_disagreeing_envelopes_fail_without_partial_stdout`, `no_match_and_invalid_scope_fail_before_build` | All passed in the 95-test run, alongside duplicate identity/namespace and wrong-kind/cross-owner reference tests. |
| Compositional determinism | Actual public-compose harness above; `outer_records_are_sorted_without_reordering_semantic_lists` and discovery-order tests | Both actual envelope orders and repeat produced identical 11738-byte output; semantic detail/input order retained. Timings excluded. |
| Fresh invocation boundary | Same host across real isolated roster mutation; saved-report argv rejection | Mutation applied, build green, observation contradicted; saved-report extra argument exits 2. This is not complete compile provenance. |
| Existing behavior | Three actual render commands, two byte comparisons, all-package tests/clippy/fmt | Passed on Mac. Canonical artifact/stage result pending. |
| Process lifecycle | Integrated process timeout/interruption/retained-pipe/overflow tests; reviewed eight-test diagnostic self-suite | Local process tests passed, with direct-child wait and live descendant checks. Linux confirmation pending; no portable grandchild reaping claim. |
| Useful cost | Dependency expansion/output bytes above; all 36 Mac samples below | Pure observations do not construct worlds/censuses. Linux costs remain pending; no cross-host latency target is earned yet. |

## Quality scenarios

| Design §8 scenario | Exact evidence | Qualification and limits |
|---|---|---|
| `charter.isolation` | Passed CLI `reuses_host_across_current_roots_ignoring_git_and_target_overrides`, symlink-target/member refusal and snapshot tests; real fixture above | Two-root hostile-path tests pass locally; primary tracked diff unchanged. Canonical repetition pending. |
| `charter.determinism` | Actual public-compose experiment and named composition tests above | Order and repeated-render equality observed for actual envelopes; no universal provenance or concurrent-source snapshot claim. |
| `charter.recovery` | Passed `timeout_terminates_tree_and_joins_readers`, `interruption_terminates_tree_and_joins_readers`, `exited_parent_cannot_leave_inherited_pipes`, overflow/error tests | Mac lifecycle evidence present; canonical suite pending. Harness additionally checks separately grouped children and failed-sample retention. |
| `charter.resource-bounds` | Passed stdout/stderr overflow and deadline tests; actual combined stderr sequences build/execution for each contributor | Caps remain authored policy: 1 MiB/64 KiB/5 s contributors, 16 MiB/600 s machine phases, 250 ms host grace. No aggregate memory/concurrency guarantee; canonical checks pending. |
| `charter.cost-attribution` | `bash scripts/charter-measure.sh`; 36 Mac samples, 0 failures, raw JSONL below | Linux series pending; whole-phase lock waiting versus compilation unavailable. Functional invocation costs are not benchmark samples. |
| `charter.extension` | Exact independently reviewed branch rosters, merge bases and actual composed reports | Package/lock extension succeeded without contributor-specific host/protocol changes. External dependencies unchanged; 27 local packages added. |
| `charter.diagnostics` | Passed CLI metadata/envelope/no-match/execution/build cases; actual roster contradiction and saved-report refusal above | Real failed report identifies the obligation, failed observation, extra kind and owning sources. Independent reader passed the readability exercise below. |
| `charter.portability` | Mac 95-test suite/clippy/fmt; canonical outboard result pending | Supported qualification hosts are macOS and Linux. Full supported-host qualification is not claimed. |
| `charter.usability` | Two real scoped reports and one real roster-failure report prepared | Independent reader PASS; bounded presentation/provenance suggestions recorded below. This does not verify implementation correctness. |

## Measurement method and all samples

Run `bash scripts/charter-measure.sh` from the committed candidate. It emits
`charter-measure` JSON lines, including source, pinned and selected toolchain,
host, load, exact argv/cwd, elapsed, exit status, raw time/phase diagnostics and
available RSS. It scrubs all six inherited Git path overrides for script-owned
Git operations. Each workload receives a fresh owned detached worktree; its
tracked Rust pin remains present and `tools/digest/target` starts absent.

Each series is **one host build + one selected cold request + ten warm requests**.
Do not count `make context-prepare` as a selected-workload cold sample: it builds
all Digest members. Cold here does not mean an empty dependency or OS cache.
Warm attempts carry `prepared_before_request`; failures cannot silently establish
preparation. No run deletes or redirects an active checkout's target cache.

### Mac samples (all attempts retained)

The controller ran `bash scripts/charter-measure.sh` at the measured source above;
exit 0, completion `failures: 0`. The [complete Mac records](the-charter-mac-samples.jsonl)
contain all 44 emitted JSON objects: environment, three workload/toolchain records,
36 samples, three unchanged-tracked-integrity results and completion. Extraction
removed only each line's `charter-measure ` prefix from `mac-measurements.log`;
no attempt, raw time output or diagnostic was omitted. File SHA-256:
`8abbc11bdcef1e8af7a3514e46767d5086d700ecff96ba8cf497149b6176e288`.

There are exactly three host builds, three cold selected requests and ten warm
requests numbered 1–10 for each workload. All warm requests record successful
prior preparation; all 36 direct children were waited, with no interruption,
deadline, launch or cleanup error. All three owned checkouts remained clean.
Functional runs earlier in this document are separate from these cost samples.

Host: `MacBookPro.local`, 10 CPUs, macOS 15.6.1 arm64; Python 3.13.5.
The retained pin and selected compiler are Rust 1.96.1
(`31fca3adb283cc9dfd56b49cdee9a96eb9c96ffd`, LLVM 22.1.2),
Cargo 1.96.1 (`356927216`, 2026-06-26), target `aarch64-apple-darwin`.
Sample start times span `2026-09-05T04:19:22Z`–`04:20:25Z`. The controller's
prewarm and functional qualification compilation had finished before this run;
that does not establish an otherwise idle host. Sampled 1/5/15-minute load
ranges were **10.508–14.150 / 15.928–17.555 / 20.787–21.699** on ten CPUs.

Elapsed seconds below retain every attempt; displayed figures are rounded,
while JSONL retains the recorded precision. Cold total is the sum of the
separate host-build and selected-request samples. Warm median is over ten
whole-request elapsed values, with the observed minimum and maximum shown.

| Workload | Cold host | Cold selected | Cold total | Warm median | Warm min | Warm max |
|---|---:|---:|---:|---:|---:|---:|
| thing | 5.832 | 2.357 | 8.190 | 0.396 | 0.385 | 0.405 |
| census | 5.779 | 11.552 | 17.331 | 0.513 | 0.498 | 0.521 |
| combined | 5.604 | 11.510 | 17.114 | 0.621 | 0.604 | 0.651 |

The stderr phase records separately retain selected Cargo build versus contributor
execution. Warm per-request phase medians (combined sums the two sequential
contributors within each request) are:

| Workload | Metadata median s | Build median s | Execution median s | Cold selected build s | Cold execution s |
|---|---:|---:|---:|---:|---:|
| thing | 0.044 | 0.067 | 0.171 | 1.848 | 0.170 |
| census | 0.044 | 0.072 | 0.167 | 10.916 | 0.169 |
| combined | 0.044 | 0.147 | 0.312 | 10.834 | 0.338 |

These phase medians need not sum to the whole-request median. Git identity/status,
launch and scheduling also consume time; lock waiting versus compilation remains
unattributed inside each whole phase. Timing stderr is absent from report text.

Available Mac peak RSS, in **bytes** (the original utility units), is:

| Workload | Cold host RSS | Cold selected RSS | Warm minimum RSS | Warm maximum RSS |
|---|---:|---:|---:|---:|
| thing | 375259136 | 383254528 | 31506432 | 32522240 |
| census | 373325824 | 749879296 | 32014336 | 33587200 |
| combined | 382222336 | 751730688 | 32849920 | 33734656 |

Every sample has available RSS; raw `/usr/bin/time -l` output, including its
other counters and footprint value, is preserved rather than conflated with
RSS. These figures do not measure simultaneous aggregate process-tree memory.

### Canonical series pending

| Host | Workload / scope | Cold host build | Cold selected request | Warm 1–10 |
|---|---|---|---|---|
| lefford Linux | Thing / `domains/thing` | Pending | Pending | Pending completed log |
| lefford Linux | census / `windows/lab/src/publish.rs` | Pending | Pending | Pending completed log |
| lefford Linux | combined / `.` | Pending | Pending | Pending completed log |

Mac `/usr/bin/time -l` RSS is bytes; Linux GNU Time RSS is KiB. The diagnostic
retains raw values and normalizes to bytes. These are each utility's available
maximums, not aggregate simultaneous process-tree memory. Missing memory on a
failed/interrupted run is recorded as unavailable. Per-phase timing does not
separate lock waiting, compilation and scheduling. The diagnostic's wall timer
includes process launch/polling and excludes its subsequent ownership cleanup;
it is not a substitute for the raw utility output.

A failed host build makes that workload's requests unavailable; other workloads
still run. A failed request remains a numbered sample. SIGINT/SIGTERM terminate
owned work across the measurement session, including nested process groups,
before worktree removal. A cleanup failure emits the complete available failed
sample before aborting later work, and retains its raw files and the owned
worktree with explicit paths. Output is retained whenever a sample is unsuccessful,
including zero-exit interruption. Children must not detach into another session.
The diagnostic does not weaken the host's 5 s contributor / 600 s machine-phase
caps or its 250 ms termination grace. Its own one-hour command safety deadline
is separately labeled.

Canonical measurements are pending ordinary serialized stage-only queue work.
Candidate `9ea2dc9abb38970c03a5b536645d9d8429b3d37a` differs from measured
implementation `e3355f441db045f0960576ead12b0f8acc56ca7d` in exactly one file,
with these five added lines in `scripts/lane-outboard.sh`:

```sh
# Charter qualification transport only: this branch is stage-only and never merged.
if [ "$fails" -eq 0 ]; then
    run "Charter qualification" bash -c 'bash scripts/charter-measure.sh --self-test && bash scripts/charter-measure.sh'
fi

```

Request: `req-9ea2dc9abb38-20260905T041951Z`. The ordinary integrated Stage 2
job is `sluice-e3355f441db0-20260905T041451Z`; neither request implies a passing
result. The canonical tested merge-product SHA and completed log remain pending.
No side-channel SSH compute job is part of this evidence. The diagnostic-only
branch is never merged back; unavailable logs remain unavailable results.

No steady-state performance claim is earned yet. Propose a target only after
both-host series are present, retaining workload/host spread and all failures.
The approved operational caps remain unchanged.

## Independent report usability review

The independent reader saw only the three actual stdout reports and their paired
stderr files, without this conversation, implementation, spec/history, other
reviews or any cited source. The result was **PASS (usability)**. The historical
report snapshots are preserved byte-for-byte, including their original revision
and dirty-state disclosure:

- [Thing report](the-charter-thing-example.md): three satisfied named checks.
- [Census report](the-charter-census-example.md): eight satisfied observations;
  queued authoring explicitly unchecked.
- [Actual roster failure](the-charter-failure-example.md): compiled successfully,
  then contradicted the component-roster obligation because `loom` was extra.

The reader identified the authored requirements/instructions, all actual named
observations and their limits, the exact `loom` mismatch/direction, and the
appropriate owning source/symbols. For this failure, the first location was
Thing's `THING_KINDS` and `thing_registry`; the reader did not confuse successful
registration/ownership observations with overall requirement satisfaction.
For census, the reader distinguished the pure guard/source-host panel from proof
that publication callers use it or that queue authoring happened. No cited
source needed to be opened to judge those locators usable.

The controller accepted the PASS and deferred three presentation/provenance
suggestions, without changing production in this campaign: put “pure guard
predicate” nearer the census requirement result; clarify the overlapping but
nonidentical Thing observation subjects and supplied composition order near the
summary; consider a richer dirty-state identifier in future provenance work.
The reports already disclose those limits; a dirty flag plus revision is not
an exact reproducibility key. The review does not validate contributor code or
the truth of the checked obligations. Full reader findings remain in campaign
scratch `usability-review.md`.

The original paired stderr, `functional-commands.json`,
`functional-qualification.log`, real envelopes, mutation diffs and temporary
Rust caller retain the exact qualification commands/results under
`.superpowers/sdd/2026-09-04-the-charter/`. All published snapshots and samples
are historical evidence, never inputs to a later production context request.

Canonical/Linux cost logs, canonical stage/artifact results and final review
remain pending. Mac data establishes an observed baseline only: no cross-host
steady-state target or change to operational caps is earned yet. The controller
owns final inventory/artifact freshness for these four new evidence files.

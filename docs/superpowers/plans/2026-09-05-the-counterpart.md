# The Counterpart Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a replayable, independently challenged comparison of which checks two concept owners need when they change separately and together.

**Architecture:** A small outboard Rust executable emits raw production registry facts and a separately labeled Charter result. Independent Python interpretation and three frozen suggestion rules operate over the same four questions; every question executes. Reuse Charter's diagnostic supervision and preserve source objects, identities, attempts and interpretation as development-only experiment data.

**Tech Stack:** Existing Rust toolchain, existing serde_json dependency, Python standard library (compatible with canonical Python 3.11), Git bundles and the existing serial stage queue.

**Spec:** `docs/superpowers/specs/2026-09-05-the-counterpart-design.md` (G3 approved 2026-09-05).

## Global Constraints

- No proposed selection skips an accepted test. All frozen obligations execute on every valid arm.
- Digest protocol v1 and existing gates remain in force. No live selector, cache, admission privilege, new scheduler or SLO.
- Deliberately changed simulation sources remain disposable specimens, retained as reconstructible inputs; they are never included in the production campaign's source or goldens.
- No domain acquires a sibling-domain dependency. No shipped simulation/save/epoch change.
- A satisfied check is not proof of unnecessary work; unknown or no enrollment falls back to all questions.
- Attempt validity and subject outcome are separate dimensions. Captured expected production refusal is a valid negative observation; missing output, interruption, timeout or uncertain cleanup is incomplete.
- Preserve original failures, corrections and misses. Freeze questions before owners finish; freeze owner records, checker and comparator identities before revealing a reserved compiling challenge.
- Another author derives the checker from the accepted ownership contract and raw facts, never the Charter verdict or candidate declaration. Shared files/context are disclosed; no security-blindness claim.
- Use immutable owned checkouts with no concurrent writers. Before/after hashes are only integrity checks, not atomic-capture proof.
- Minutes-scale canonical experiments run through the ordinary serial claim. No local census or full-workspace intermediate test. Ordinary commit hooks, stage gates at stage boundaries, one preclose canonical census, and G6 before merge remain required.
- Independent specimen authors use separate worktrees and disjoint source scopes, overriding generic same-worktree serial-author advice. Stagger compilation; never share an index.
- Human prose needs no tests. Test behavioral contracts with hand-derived expectations; a compile failure is not a behavioral kill. Three failed attempts on an issue require documented reassessment.

## File responsibilities and stages

| Path | Responsibility |
| --- | --- |
| `scripts/charter-measure.sh`, `scripts/charter_measure.py` | Stable Charter CLI and reusable reviewed measurement implementation |
| `scripts/test_charter_measure.py` | Raw retention/bounds and import compatibility tests; preserve existing harness tests |
| `tools/digest/packages/counterpart/Cargo.toml`, `src/lib.rs`, `src/main.rs` | Development-only raw facts executable, no contributor enrollment metadata |
| `tools/digest/Cargo.lock` | Explicitly reviewed lock reconciliation |
| `tools/digest/experiments/the-counterpart/contract.json`, `README.md` | Frozen questions, raw schema, authority, input mapping and replay instructions |
| `tools/digest/experiments/the-counterpart/checker.py`, `test_checker.py` | Independently derived finite judgments and hand-checked behavioral cases |
| `tools/digest/experiments/the-counterpart/owners/{thing,settlement}.json`, `patches/` | Independently authored declarations and production-source specimens as data |
| `tools/digest/experiments/the-counterpart/compare.py`, `test_compare.py` | Pure path, Cargo reachability and owner-agreement suggestions and scoring |
| `tools/digest/experiments/the-counterpart/run.py`, `test_run.py` | Owned reconstruction, observation, identity/attempt validation and dossier CLI |
| `tools/digest/experiments/the-counterpart/panel.json`, `specimens.bundle`, `freeze.json` | Finite arms, retained objects and chronology identities |
| `tools/digest/experiments/the-counterpart/evidence/` | Bounded immutable attempts, raw outputs and independent replay receipts |
| `docs/superpowers/ledgers/2026-09-05-the-counterpart.md` | Rulings, findings, interpretation and all deferred work |
| `docs/retrospectives/the-counterpart.md`, `book/src/chronicle/the-counterpart.md`, `book/src/SUMMARY.md` | Reviewed close narrative and process lessons, prepared before G6 |

Stage 1: Tasks 1–2, reviewed observation/checker freeze, stage submission.
Stage 2: Tasks 3–5, independent specimens and complete unreserved assay, stage submission.
Stage 3: Tasks 6–7, reserved challenge, supported-host qualification, independent replay and G6 package.
Track stage status in `IMPLEMENTATION_PLAN.md`; remove it when stages finish.

## Shared interfaces and frozen questions

All JSON is UTF-8; duplicate keys, unknown IDs, duplicate question results and missing required fields are invalid. Schema identity is `counterpart-v1`, a development format unrelated to Digest's protocol number. Four question IDs are fixed:

| ID | Question / accepted authority | Relevant packages | Inputs |
| --- | --- | --- | --- |
| `registration` | Does Settlement then Thing registration complete without refusal? | `hornvale-settlement`, `hornvale-thing` | both registration functions and actual namespace |
| `components` | Do Thing source and component rosters agree in both directions, with no duplicates? | `hornvale-thing` | `THING_KINDS`, `thing_registry` |
| `borrowing` | Are borrowing declarations unique, members of the Thing roster, non-self, and supplied by the stated owner before Thing registration? | `hornvale-settlement`, `hornvale-thing` | `BORROWED`, lender registry |
| `ownership` | Does the completed registry give every Thing name its stated owner, contain no extra Thing-owned names, and preserve lender owners? | `hornvale-settlement`, `hornvale-thing` | both registries, Thing roster and borrowing |

All map to contributor `hornvale.thing`, current scope `domains/thing`, with source locators as named above. This question map is experiment input, not an expanded enrollment declaration. Changes outside mapped subjects are unknown; additions/deletions within the two owned source files remain mapped. Unknown ownership after refusal is explicitly unknown; registration still reports violated. Questions are evaluated independently where sufficient raw facts remain.

The raw executable prints one JSON object and exits zero when observation completed, including an expected captured refusal. An uncontrolled process failure is nonzero. Public Rust surface:

```rust
pub fn snapshot() -> serde_json::Value;
```

Its object has `schema`, `facts`, and `candidate`. `candidate` is the existing `digest_thing::collect()` contribution, separately captured with error/refusal information if it panics; it is never checker input. `facts` has exactly:

```json
{
  "source_kinds": ["hearth", "key"],
  "component_kinds": ["hearth", "key"],
  "borrowed": [{"name":"hearth", "owner":"settlement"}],
  "before_concepts": [{"name":"hearth", "owner":"settlement"}],
  "after_concepts": [{"name":"hearth", "owner":"settlement"}, {"name":"key", "owner":"thing"}],
  "settlement_registration": {"outcome":"accepted", "detail":""},
  "thing_registration": {"outcome":"accepted", "detail":""}
}
```

Arrays retain duplicates and observed names; concept ordering is sorted for readable evidence. `accepted`/`refused` are observation outcomes, distinct from checker outcomes `satisfied`/`violated`/`unknown`. Source/component observation faults cannot be replaced by empty lists: abort the observation or explicitly fail the attempt. Missing/invalid facts are a `ValueError`, never a satisfied report.

Python interfaces (standard library types):

```python
# checker.py; requires the exact frozen question IDs
# Return one outcome and reason per question, independently from candidate.
def evaluate(facts: dict, contract: dict) -> dict[str, dict[str, str]]: ...
# compare.py; proposed, unknown and effective are lists; rules contain version identity.
def suggest(kind: str, changed_paths: list[str], metadata: dict,
            owners: list[dict], contract: dict) -> dict: ...
def score(selection: dict, outcomes: dict) -> dict: ...
# charter_measure.py; preserve legacy defaults and result fields.
def measure(command, cwd, *, retain_output=False,
            output_limit_bytes=None, deadline_seconds=3600): ...
# run.py; validates persisted attempts and all listed arms, returns summary.
def summarize(dossier: dict, panel: dict, contract: dict) -> dict: ...
```

Here ellipses denote interface signatures only; implementations are specified in their owning tasks below. No task may silently change a signature. A change requires a controller ruling carried to its consumers.

### Task 1: Observe real APIs and retain bounded raw process evidence

**Files:** Create the raw package, `contract.json`, `README.md`, `scripts/charter_measure.py`, `scripts/test_charter_measure.py`; modify the Charter wrapper and Digest lock.
**Interfaces:** Consumes production `ConceptRegistry::new`, `concepts`, `hornvale_settlement::register_concepts`, `hornvale_thing::{THING_KINDS,BORROWED,thing_registry,register_concepts}`, `digest_thing::collect`. Produces `snapshot()` and the exact facts above; importable legacy `controlled_env`, `git`, `OwnedWorktree`, `measure`, `successful`, `request_stop`, `unsafe_cleanup` in the Python module; writes the frozen question contract.

- [ ] Read the production signatures and three precedents: Charter Thing collector, Charter harness, Digest package manifests. Read relevant directory guidance. Confirm `collect` return type before use.
- [ ] Add a behavioral retention test against the existing harness operation, initially showing successful stdout cannot be recovered. Refactor import mechanics only as needed to exercise the same implementation, then capture the red before adding retention:

```python
sample = measure([sys.executable, "-c", "import sys; sys.stdout.buffer.write(b'\\x00ok'); sys.stderr.write('diagnostic')"], root, retain_output=True)
self.assertEqual(base64.b64decode(sample["stdout_base64"]), b"\x00ok")
self.assertEqual(base64.b64decode(sample["stderr_base64"]), b"diagnostic")
```

- [ ] Move the existing embedded Python implementation and its self-tests to `scripts/charter_measure.py`; the bash wrapper resolves its own directory and execs Python. Import has no argv mutation, signal handler installation, output, or running work. `main` validates both committed wrapper and module rather than checking the wrong file. Keep existing CLI flags/output/default measurement behavior. Extend measurement to retain exact base64 bytes plus SHA256/size for both streams when requested; check output file sizes while polling and interrupt on a requested bound. Preserve deadline, interruption, launch and cleanup flags. Do not add a second supervisor. Test oversize output and cleanup failure, retaining a failed attempt before stopping. Legacy defaults remain backward compatible.

```python
# Within the existing measure implementation after reading raw bytes:
if retain_output:
    sample["stdout_base64"] = base64.b64encode(output).decode("ascii")
    sample["stderr_base64"] = base64.b64encode(diagnostic_bytes).decode("ascii")
    sample["stderr_bytes"] = len(diagnostic_bytes)
    sample["stderr_sha256"] = hashlib.sha256(diagnostic_bytes).hexdigest()
```

- [ ] Implement `snapshot` with separately captured production registration and candidate collection. Use `catch_unwind(AssertUnwindSafe(...))` for expected refusals; serialize actual lists, lender concepts before Thing, composed concepts after. A helper maps `Result<Result<(), RegistryError>, Box<dyn Any + Send>>` into the documented registration record, preserving error text. Test a literal real-base property: `hearth` owner is `settlement`, `key` owner is `thing`, and borrowing names `hearth`; do not use the function under test to derive expectations. No new contributor metadata. Dependencies are existing `hornvale-kernel`, `hornvale-settlement`, `hornvale-thing`, `digest-thing`, `serde_json` paths/versions; review new local lock record separately from external drift.

```rust
fn main() {
    println!("{}", digest_counterpart::snapshot());
}
```

- [ ] Write `contract.json` with the exact question map above and source authority pointers. README names every writer, material shared input, facts schema and limits. No checker derivation is included in raw code.
- [ ] Run focused package tests, `cargo fmt --manifest-path tools/digest/Cargo.toml --all --check`, selected package clippy, `bash scripts/charter-measure.sh --self-test`, Python tests and shellcheck on the wrapper. Capture actual results and differences; any simulation/golden diff stops for inspection. Commit ordinary passing tooling changes and report the freeze SHA.

### Task 2: Independently derive the checker

**Files:** Create `checker.py`, `test_checker.py` in the experiment directory; extend README with derivation disclosure only.
**Interfaces:** Consumes exact `facts` and frozen contract above; produces `evaluate(facts: dict, contract: dict) -> dict[str, dict[str,str]]`. The author may read production ownership contract and public API, but must not read or call the Charter collector/verdict derivation or other authors' records/patches. Record actual disclosures.

- [ ] Independently translate each frozen question from accepted contract to raw facts. Return exactly four `{outcome, reason}` objects. Validate required shapes/IDs first. Registry refusal violates registration; incomplete composition makes ownership unknown, not satisfied. `borrowing` can still judge lender state if Settlement completed. Membership and reverse directions are load-bearing.
- [ ] Write hand-derived tests before implementation using the literal two-name example in Shared interfaces. Include legitimate borrowing, missing/wrong lender, absent borrowing declaration over a collision, extra Thing-owned concept, extra component, duplicate/unused/self borrowing, unknown after refusal, and missing field. Mutation-test an actual wrong branch after green, ensuring the mutation was applied and the fixture compiles/executes. Preserve red evidence rather than treating an import failure as behavioral proof.

```python
facts = {"source_kinds":["key"], "component_kinds":["key"], "borrowed":[],
         "before_concepts":[], "after_concepts":[{"name":"key","owner":"settlement"}],
         "settlement_registration":{"outcome":"accepted","detail":""},
         "thing_registration":{"outcome":"accepted","detail":""}}
self.assertEqual(evaluate(facts, contract)["ownership"]["outcome"], "violated")
```

- [ ] Implement each predicate independently with sets/counts over the observed arrays. Derive the owner expected for a Thing label from explicit borrowing or `thing`; inspect the observed owner separately. Verify every lender concept retains its owner. Never accept candidate-declared affected IDs or Charter observations as the answer. Reject unknown question IDs rather than dropping them.
- [ ] Run `python3 -m unittest discover -s tools/digest/experiments/the-counterpart -p 'test_checker.py' -v`; self-review, ordinary commit, and provide full test/derivation report. Do not choose or reveal the reserved challenge yet; it is Task 6 after the complete freeze. Controller reviews and records checker SHA. Complete Stage 1 stage submission with reviewed Tasks 1–2.

### Task 3: Independently author Thing specimens and its record

**Files:** Create `owners/thing.json`, `patches/thing-*.patch`; only experiment data ships. Disposable authored changes may touch `domains/thing/src/lib.rs` and, for a correlated candidate-check fault, its owned `tools/digest/packages/thing/src/lib.rs`; no Settlement or raw-probe/checker writes.
**Interfaces:** Consumes Task 1 frozen source base, question IDs and raw probe. Produces patch files applying to that base plus owner JSON with `owner`, `base`, `owned_paths`, `supplies`, `consumes`, `positive_assumptions`, `negative_assumptions`, `affected_questions`, `unknowns`, `authority`, and per-variant patch path and observed effect. `negative_assumptions` explicitly distinguishes unclaimed new names from the list of known imports.

- [ ] Work in a separately owned worktree at the frozen base, with task-context separation from Settlement author. Read actual Thing code and domain guidance. Do not read the other author's patch or record.
- [ ] Find and retain real-source changes demonstrating: an observable safe additive Thing kind, a prospective joint claim of a new name, and a legitimate unusual borrowing of an existing lender name. Name choice for the joint candidate is a disclosed shared fixture input, fixed to `counterpart-marker`; both authors receive that input independently, neither implementation. Do not assume a string replacement works: assert its target and qualify real observations.
- [ ] Store unified binary-capable Git patches as data. For each variant, compile and run the raw executable in an owned source checkout; record source diff and actual outcomes, including rejected/incomplete attempts. No full-domain correctness claim follows; these are experimental mutations. Restore the owned source files before committing the patch artifacts through ordinary hooks. Source commit objects are generated as experiment data by Task 5, never admitted production commits.

```python
completed = subprocess.run(["git", "diff", "--binary", "--", "domains/thing/src/lib.rs"],
                           cwd=owned_source, capture_output=True, check=True)
patch_path.write_bytes(completed.stdout)
if not completed.stdout:
    raise RuntimeError("invalid unchanged specimen")
```

- [ ] Author the owner record before integration. State candidate predictions separately from observed facts. Capture meaningful supplied/consumed names and negative assumption IDs; no declaration becomes an oracle. Report authored elapsed time and source-sharing disclosures, not invented wall-clock overlap. Run JSON parse/patch application checks and ordinary artifact commit; return actual compile/probe evidence. Controller reviews task and integrates only artifact paths.

### Task 4: Independently author Settlement specimens and its record

**Files:** Create `owners/settlement.json`, `patches/settlement-*.patch`; disposable source changes only `domains/settlement/src/lib.rs`.
**Interfaces:** Consumes Task 1 frozen source base/question IDs and shared input `counterpart-marker`. Produces the same owner record schema as Task 3, independently, and base-applicable patches.

- [ ] Work in a separate worktree, read Settlement source/guidance; do not inspect Thing author's patches/record.
- [ ] Find real-source changes demonstrating an observable safe addition with a distinct name, a prospective new claim of shared `counterpart-marker`, and removal/renaming or wrong ownership of a lender that Thing consumes. Record properties and observed outputs, not a prescribed unverified mutation. Preserve invalid attempts.
- [ ] Store the exact nonempty source patches and independently authored positive/negative assumptions. Qualify compilation and raw observations in owned disposable sources. Dependency failure must be a base-green/solo-violating observation, never relabeled joint-only. No production source changes ship.

```python
completed = subprocess.run(["git", "diff", "--binary", "--", "domains/settlement/src/lib.rs"],
                           cwd=owned_source, capture_output=True, check=True)
patch_path.write_bytes(completed.stdout)
if not completed.stdout:
    raise RuntimeError("invalid unchanged specimen")
```

- [ ] Restore the owned source files before committing artifacts; verify no Thing file was written, parse JSON and verify patch paths. Record actual commands, effects and author time. Controller reviews and integrates only artifact paths. Lock reconciliation is explicit if required; do not infer an empty lock diff.

### Task 5: Run the full panel and compare suggestions honestly

**Files:** Create `compare.py`, `test_compare.py`, `run.py`, `test_run.py`, `panel.json`, `specimens.bundle`; update README. Controller maintains `freeze.json` after review.
**Interfaces:** Consumes `evaluate`, `measure`, owner records/patches/contract. Produces `suggest`, `score`, `summarize` above and CLI `python3 tools/digest/experiments/the-counterpart/run.py --panel <panel.json> --output <new-directory>`. Relative panel inputs resolve against panel parent; refuse an existing output directory. Provide `--self-test` and `--help`, neither runs the full assay. Evidence JSON is written incrementally before cleanup can fail.

- [ ] Build pure comparator tests first. A path request under `domains/settlement` has no enrollment and falls back to all four questions. A Thing path selects all mapped contributor questions. Unknown paths/non-Cargo inputs also fall back. Cargo uses actual full `cargo metadata --locked --offline` graph, changed-file package membership, and reverse dependent closure in each arm; additions/deletions consider base and changed states. Agreement uses only frozen owner records and changed supplied/consumed subjects, including the named negative assumption; missing scope/subject/assumption produces unknown. No behavioral result may influence a suggestion.

```python
selection = suggest("path", ["domains/settlement/src/lib.rs"], {}, [], contract)
self.assertTrue(selection["unknown"])
self.assertEqual(set(selection["effective"]), {"registration","components","borrowing","ownership"})
result = score({"proposed":["components"], "unknown":[], "effective":["components"]},
               {"components":{"outcome":"satisfied","reason":"equal"},
                "registration":{"outcome":"violated","reason":"refused"},
                "borrowing":{"outcome":"satisfied","reason":"supplied"},
                "ownership":{"outcome":"unknown","reason":"partial"}})
self.assertEqual(result["raw"]["unselected_violating"], ["registration"])
```

- [ ] Implement selectors as pure functions over recorded metadata and declarations. `score` emits both raw and effective `selected_violating`, `selected_satisfied`, `unselected_violating`, and `unknown` question IDs, plus fallback reasons. Include selected-unknown explicitly. Passing selections are never called waste. Preserve reasons known before observing outputs. Freeze the rules/map bytes and implementation commit identity.
- [ ] Construct all real four-arm source objects from the fixed base and exact author patches in an owned scratch clone/index. Apply each side alone and then both with no silent conflict resolution. Use Git write-tree/commit-tree for experimental source objects with recorded parents/metadata; this authors test data, not an admitted code commit. Retain source object IDs and a `git bundle` with explicit base prerequisite. Bundle verification and replay recover objects without original author worktrees. Reject unchanged arms; report any conflict. At least a safe pair, joint-collision pair, lender dependency failure, missing-negative declaration comparison and correct unusual borrowing are in the panel. Classes may share arms but remain explicitly classified.
- [ ] Implement runner reconstruction in an owned temporary clone/worktree, import bundle with scrubbed Git env, verify full SHAs/tree identities, and never reset the caller. Freeze writers before observing. Build `digest-counterpart` with `--locked --offline`, explicit owned target, then execute the resulting binary. Metadata/toolchain/config/input capture is preparation. Run all four checker questions even if a selector proposes fewer. Candidate and checker outcomes remain separately labeled. Record subject/checker/rule/roster identities, SHA256/size of inputs/locks/pin, features/profile, material environment (allowlisted, no secrets), host/toolchain, commands, wall/preparation/execution/queue/author costs separately, exact raw stream bytes and all failure flags. Bound each command's retained output to 16 MiB and deadline to the existing 3600 s; these are resource bounds, no performance target. Never overwrite attempts.
- [ ] Write fail-closed dossier validation with tests for an omitted arm/question, duplicate result, stale checker/rule/source hash, mismatched arm attribution, tampered output, absent bundle prerequisite, invalid JSON, compile failure, timeout and uncertain cleanup. A valid captured refusal is a completed observation with violated registration, not failed harness execution. No run can render completed when one required attempt is incomplete. Stop later sampling on interruption/unsafe cleanup; preserve logs and owned directories. Reuse existing Charter fault tests for process behavior rather than copying them. Test reconstruction with small real Git fixtures and real child processes; fixture mocks must preserve actual staging/output side effects.

```python
# Completion requires all manifest arms, not merely all rows that happened to emit.
missing = set(panel["arms"]) - set(dossier["arms"])
if missing:
    raise ValueError("missing arms: " + ", ".join(sorted(missing)))
```

- [ ] Run focused Python tests and real unreserved CLI panel; retain invalid attempts and qualify actual class properties. If realistic safe/joint cases cannot be constructed, record the spec's feasibility result and return for design amendment, not a passing score. Review the implementation and lock reconciliation, then commit. Freeze identities for owners, checker, question/map/rules, unreserved panel and challenge-author context before Task 6. Complete Stage 2 stage submission; dependent qualification uses its green result.

### Task 6: Reveal the reserved challenge and qualify both hosts

**Files:** Add `patches/reserved-*.patch`, append panel/bundle/evidence with new identities, `freeze.json`, ledger receipts; stage-only diagnostic branch modifies `scripts/lane-outboard.sh` without merging it.
**Interfaces:** Consumes the complete reviewed freeze, runner CLI, checker and raw contract. Produces a preregistered challenge and original scored results, plus Mac/canonical Linux raw dossiers and queue receipts.

- [ ] Resume the independent checker author with only its own earlier work, accepted contract and freeze identities. It chooses at least one compiling behavioral counterexample not present in the unreserved panel. Include a correlated wrong-answer case: candidate declaration/check agreement alongside an actual independently judged behavioral violation. An always-satisfied record alone is insufficient. Author source patch and expected behavioral property before scoring; disclose any already known case. Preserve compile failures or checker misses as original attempts. Never modify frozen checker/rules to score the original challenge green.
- [ ] Controller imports challenge artifact after its identity is recorded, assembles actual source object and runs original frozen checker/comparators. Independently adjudicate any mismatch from contract/raw facts. A correction gets a new identity and supplemental run, preserving original. Check legitimate unusual borrowing for over-refusal.
- [ ] Run focused actual CLI/report exercises on Mac under local cost discipline; separate tool preparation from execution and avoid simultaneous expensive builds. Route full minutes-scale qualification to canonical. Create an isolated diagnostic-only branch from reviewed campaign commit, adding self-tests and the explicit runner invocation after ordinary outboard checks using Charter's existing precedent. Submit `make sluice-stage BRANCH=<diagnostic-branch> REF=<full-sha>` through the ordinary queue. No new persistent gate or scheduler and no merge of diagnostic-only invocations. Preserve exact merge-product SHA and diagnostic diff, retrieve all raw evidence, verify every phase status. The routine production candidate still gets its own stage boundary gate.
- [ ] Validate hashes/IDs/arm counts/cleanup flags in both retained host dossiers. Report unknowns and failures; no portable-verdict-reuse claim. Ordinary commits retain bounded records; costs from queue, preparation and authored effort have separate sources/units. Task review verifies the population and chronology before proceeding.

### Task 7: Independent replay, interpretation and the G6 package

**Files:** Independent replay receipt in evidence; ledger interpretation; close narrative/retrospective and Book summary; reconciliation status; final removal of `IMPLEMENTATION_PLAN.md` when done.
**Interfaces:** Consumes committed README/panel/bundle/contract/checker/rules/raw evidence, never originating task scratch. Produces independently reconstructed source IDs, observed outcome comparison, explicit limitations and reviewable G6 package.

- [ ] Dispatch a fresh replay author in a separately owned directory. It follows only committed instructions, imports source objects from retained bundle/prerequisite, runs the finite panel, and compares source identity and outcomes, separating environmental cost variation. Missing undocumented input or inaccessible prerequisite is a replay failure to fix/document; retain original failure. Minutes-scale canonical execution uses the ordinary stage-only mechanism, not an unclaimed SSH job.
- [ ] Interpret primary and comparative hypotheses separately. Verify non-no-op safe pair and joint-only result on actual base/A/B/composition, classify solo failures separately, name existing production/Charter detection, misses, fallback frequency, before/after selection confusion counts and preparation/coordination costs. If agreement matches Cargo/full set, report zero demonstrated selection benefit and no justification to expand the mechanism. No natural fault frequency or throughput claim from manufactured examples.
- [ ] Write concise reader-facing evidence and limitations in the ledger, close narrative and retrospective; preserve all seven Charter followups and Nathan's profiling ownership. Do not mark broader federation ideas shipped. Inventory writers and confirm no accidental inherited generated author. Reconcile spec/plan/narrative status through existing table rules.
- [ ] Run necessary focused final checks, ordinary commit gate, final whole-branch independent review, and the Stage 3 candidate stage gate. All simulation/golden diffs require explicit inspection; no silent rebaseline. Queue one preclose census through `make sluice-census` on the full candidate SHA and record returned branch/changed columns separately; any moved golden landing waits for G6.
- [ ] Resolve review findings under SDD and the three-attempt reassessment rule. Present the post-G3 ledger digest and concrete results at G6, with all rulings/cost-if-wrong, residual issues, tested SHAs and gates. Stop before merge/close; campaign-autopilot explicitly requires Nathan's G6 approval. No worktree deletion or publication before that approval.

## G4 self-review map

Spec §§1–4: bounded raw API/independent checker Tasks 1–2; fixed map above.
§5: independent Task 3–4 authors, four-arm Task 5, reserved Task 6, replay Task 7.
§6: pure comparator/fallback/confusion scoring Task 5; null interpretation Task 7.
§7: shared supervisor Task 1, source bundle/attempt validation Task 5, independent replay Task 7.
§§8–9: separate authors, staged freeze, resource/attempt rules across Tasks 1–7.
§§10–11: supported hosts, final review and evidence, unchanged authority and G6 Tasks 6–7.
No source mutation is pre-certified. Every source variant is qualified by behavior after compilation. All shared interfaces have an owning task; actual process/API signatures were read before drafting. The deliberate interface ellipses above are type declarations, not deferred implementation steps.

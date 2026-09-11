# Eclipse Rhythm and View — Decision Ledger

## #1 [G4] — approved implementation boundary

**Question:** What should the approved Eclipse Rhythm and View design become in implementation?

**Decision:** Implement the combined recurrence and observer-view campaign over the shipped Eclipse Seasons core, replacing the active eclipse scene contract with `scene/eclipses/v3`.

**Why:** The approved design makes recurrence and observer results first-class, and Nathan explicitly selected B + C with pre-alpha freedom to replace v2. Existing node draws, closed-form event evaluation, and quantize-at-emit remain authoritative.

**Alternatives discarded:** A closeout-only response would leave the requested observability unfinished; preserving v2 would add compatibility work with no current consumer; a new physics engine would violate the focused astronomy boundary.

**Ideonomy passes / overturns:** One G1 ideonomy pass; no overturn. It surfaced the need to distinguish a physical geographic region from an observer result and to treat the already-shipped Eclipse Seasons work as the parent rather than duplicate it.

**Capture actions:** This ledger, the approved spec, and the implementation plan record the boundary. Deferred standstills, transits, tidal braking, lunar shading, partiality, and unrelated sky phenomena remain in the frontier.

## Plan preflight

| Task or shared interface | Check | Ruling |
|---|---|---|
| Task 1 self-consistency | Domain tests consume existing `eclipse_events`, `GroundTrack`, and sight helpers; produced recurrence and observer types feed Task 2 and Task 4. | Consistent. |
| Task 2 self-consistency | Scene v3 consumes Task 1 outputs and produces the schema consumed by Tasks 3–5; exact ticks and quantizers match current scene conventions. | Consistent. |
| Task 3 self-consistency | CLI and WASM both call the Task 2 producer and validate observer arguments at their boundaries. | Consistent. |
| Task 4 self-consistency | Almanac and reference text consume the same Task 1/2 vocabulary and do not introduce a second physics calculation. | Consistent. |
| Task 5 self-consistency | Artifact review follows all producers and gates only the files the implementation actually changes. | Consistent. |
| Tasks 1 and 2: astronomy API → scene mapping | Task 1 owns physical recurrence/visibility decisions; Task 2 serializes them and keeps region separate from observer result. | No conflict. |
| Tasks 2 and 3: scene signature → CLI/WASM | Task 2 supplies optional observer input; Task 3 passes explicit validated coordinates and updates v3 labels. | No conflict. |
| Tasks 2 and 4: v3 vocabulary → almanac/reference | Both use the same event, region, recurrence, and observer terms; the client does not re-derive decisions. | No conflict. |
| Tasks 3 and 5: generated output → verification | Task 5 checks native/WASM and committed artifacts after surface wiring; no census refresh is assumed. | No conflict. |

Plan self-review found no placeholders, unresolved type-name mismatch, or requirement without a task. The plan and this ledger are committed before implementation begins.

## #2 [G4] — Task 1 generated-report correction

**Question:** May Task 1 include generated reports required by the commit gate?

**Decision:** Yes. The plan's prohibition on generated artifacts was too broad. A public API addition must update the prescribed type-audit report, and timed verification may append the required `docs/timings.md` row. These are gate artifacts, not census regeneration.

**Why:** The worker's focused tests passed, while `make gate-commit` failed closed because `docs/audits/type-audit-report.md` was stale. The repository hook requires that report to be current and forbids bypassing the hook.

**Alternatives discarded:** Bypassing the hook is prohibited. Deferring the report would leave a known gate failure. Regenerating census artifacts remains out of scope because no world-generating or census metric change occurred.

**Capture actions:** Task 1 now explicitly owns the type-audit report refresh and preserves the timing ledger output; the worker resumes from its uncommitted diff.

## #3 [G5] — long-duration ground-track review finding

**Question:** Does observer visibility remain correct when a legal short-day world rotates more than 180 degrees during one eclipse crossing?

**Decision:** Fix the track membership calculation to use the directed rotation sweep derived from `duration_days / day_length`, treating a full rotation as global coverage, and test endpoint membership independently of the event midpoint's day-side classification.

**Why:** The task review produced a legal counterexample with a four-hour day and a nearly one-day eclipse crossing. The current shortest-arc calculation classified the middle of a physically covered track as `Bitten`, and midpoint day-side gating could reject track endpoints.

**Alternatives discarded:** Keeping shortest-arc semantics is physically wrong for long crossings. Expanding the campaign into a new shadow integrator is unnecessary; the existing track duration and rotation direction already determine the required directed arc.

**Capture actions:** Task 1 fix round adds the long-sweep regression and updates the domain implementation before scene work continues.

## #4 [G5] — scene wire tick boundary review finding

**Question:** Can the v3 scene producer accept any finite `StdInstant` without panicking while emitting exact `i64` wire ticks?

**Decision:** Validate and convert both requested window bounds to `WorldTime` before eclipse enumeration, return a `SceneError` for either out-of-range bound, and reuse the validated ticks in the document.

**Why:** The reviewed producer used `expect` while converting event instants. A finite bound just outside the representable tick range reached that conversion and panicked, violating the public `Result` contract and the v3 exact-tick requirement.

**Alternatives discarded:** Retaining the `expect` leaves a reachable panic in a query surface. Clamping would silently change the requested window and violate exact bounds.

**Capture actions:** Task 2 fix round adds lower and upper boundary tests and propagates conversion errors before event enumeration.

## #5 [G5] — CLI v3 error/report review findings

**Question:** Do all native eclipse query failures identify the v3 contract, and does the task retain its required verification evidence?

**Decision:** Prefix missing-window CLI errors with `scene/eclipses/v3:` and add both missing-flag assertions. Preserve the worker's required Task 3 report with red/green and gate output before the task is accepted.

**Why:** The observer and WASM paths already emitted versioned errors, but the native parser's missing `--from`/`--until` branch still returned an unversioned message. The implementation worker also omitted the SDD report, leaving the review package without the required test evidence.

**Alternatives discarded:** Treating the missing report as clerical would weaken the required review record. Leaving parser errors unversioned makes clients distinguish failures inconsistently.

**Capture actions:** Task 3 fix round updates the parser tests/error envelope and writes the report as a scratch SDD artifact; no product scope expands.

## #6 [G5] — stale generated artifact review findings

**Question:** Do the committed almanac and scene examples agree with the v3 producer and the new almanac vocabulary?

**Decision:** Regenerate the locked seed-42 almanac and scene examples using the active artifact commands, then repair the campaign spec's dead v2 reference.

**Why:** Review found `almanac-seed-42-locked.md` still carried the former compact recurrence prose and `scene-eclipses-seed-42.json` still declared `scene/eclipses/v2`, despite the v2 reference being removed and the generators now emitting the v3 contract.

**Alternatives discarded:** Leaving either artifact stale would make committed examples contradict the code and documentation. Hand-editing generated output would bypass the prescribed generator and weaken artifact provenance.

**Capture actions:** Task 4 fix round runs the existing non-census artifact regeneration path once, inspects the complete diff, and updates only the dead spec link alongside required generated output.

## #7 [G5] — campaign-wide contract review findings

**Question:** Does the integrated Eclipse Rhythm and View contract preserve recurrence closure, long-track geometry, exact query semantics, multi-moon rhythm, and truthful observer prose across every surface?

**Decision:** Extend the fix round to expose accumulated surface-longitude closure for exeligmos, preserve directed sweep magnitude and full-turn coverage in scene data, enumerate from snapped wire bounds, publish multi-moon coincidence summaries, and distinguish event-wide track visibility from midpoint side in almanac wording.

**Why:** Cross-slice review found five inconsistencies that isolated task reviews could not see: node-phase slip was mislabeled as terrestrial closure; wrapped track endpoints discarded direction and turn count; identical emitted bounds could query different event sets; `coincidence_days` was unreachable; and long-track central visibility could be rendered as a contradictory night-side claim.

**Alternatives discarded:** Keeping node slip does not describe repeated shadow placement. Wrapped endpoints are insufficient for directed or global tracks. Hidden sub-tick residues violate exact wire semantics. Omitting coincidence summaries leaves the approved multi-moon observable unavailable. Midpoint wording cannot stand for an event-wide passage.

**Capture actions:** A cross-slice fix worker will update the smallest shared domain/scene/almanac contracts and their tests, then regenerate affected examples and repeat the focused and campaign-wide review. Deferred astronomy seams remain deferred.

## #8 [G5] — final review status finding

**Question:** Does the committed design document state its actual campaign status after approval and implementation?

**Decision:** Replace the stale “Draft for G3 review” label with an approved/implemented status that still makes the pending canonical gate explicit.

**Why:** Final re-review found no remaining behavioral or contract issue, but the spec header still contradicted the G4 approval, shipped local implementation, and reconciliation state.

**Capture actions:** Update the status line, run the prose/documentation checks and commit gate, then request final campaign-close review. No product behavior changes.

## Task 5 — local artifact review and verification

The one full non-census artifact run exited 0. Its timing records are
`census-tail-chorus` (`95.603s`, rc 0) and `rebaseline` (`280.714s`, rc 0),
both at `0cea68974`. The complete remaining content diff contained only seven
eclipse-count lines in `book/src/gallery/generated/the-lot-seed-42.md`; that
Lot exhibit output is outside this campaign and was restored byte-for-byte to
the branch's committed version. The required Eclipse almanac and scene
fixtures had already been producer-refreshed and committed in `0cea68974`, so
the full run produced no further Eclipse contract output. No census ran.

Focused verification after resolving the artifact drift exited 0:

- `cargo test -p hornvale-astronomy`: 286 unit and 45 integration tests passed.
- `cargo test -p hornvale-scene eclipses`: 6 unit and 1 fixture test passed.
- `cargo test -p hornvale-almanac eclipse_`: 6 focused tests passed.
- `HV_TEST_OK=1 cargo test -p hornvale --test suite scene_eclipses_cli -- --nocapture`: 7 focused CLI tests passed.

The required local gates also exited 0:

- `make quick`: `29.777s`; formatter, clippy, type audit, placement audit, and plumb checks passed.
- `make world-check`: `65.444s`; native/WASM system, tile, region, and eclipse scenes were byte-identical, observer normalization/error envelopes passed, and the WASM measured 468,547 bytes gzipped (1,233,557 raw) against the 524,288-byte limit.
- `make gate-commit`: `59.210s`; all four argv-safe subfloor chunks passed, with the terminal chunk reporting 445 passed and 5,831 skipped.
- `cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)'`: 41 passed and 393 skipped after the reconciliation and plan-status edits.
- `make docs-tests`: all 75 prose-subject tests passed and 359 were skipped.

Task 5's local campaign review is complete; G6 and the canonical Sluice gate
remain pending. This worker was explicitly instructed not to spawn subagents
and not to submit, push, merge, or close the campaign.

### Ledger #7 integrated fix evidence

The final review round resolved all five campaign-wide findings without new
genesis draws, stream allocation, epochs, save facts, or eclipse categories.
The domain now keeps exeligmos surface-longitude closure separate from orbital
node slip, and ground tracks carry a signed unwrapped sweep plus explicit
global coverage. Scene v3 enumerates from its snapped `i64` bounds and exposes
the existing deterministic multi-moon coincidence count. Almanac prose treats
central passage as event-wide while retaining `side` as the event-midpoint
hemisphere.

The full non-census artifact run exited 0 in `365.861s` at `d143b15f2` and
explicitly reported `censuses SKIPPED`. The changed artifacts are the scene v3
fixture/gallery, both almanac examples, the type-audit report, and the Lot
exhibit's eclipse-count lines. The Lot movement is now required output: those
counts consume corrected directed ground-track membership. The scene fixture
and gallery parse to identical JSON; their only byte difference is the
gallery's established trailing newline.

Focused and affected verification exited 0:

- `cargo test -p hornvale-astronomy`: 286 unit and 45 integration tests passed.
- `cargo test -p hornvale-scene`: 111 unit and 18 integration tests passed; one declared heavy test remained ignored.
- `cargo test -p hornvale-almanac`: 73 unit and 25 integration tests passed.
- `HV_TEST_OK=1 cargo test -p hornvale --test suite scene_eclipses_cli -- --nocapture`: 7 focused CLI tests passed.
- `make docs-tests`: 75 prose-subject tests passed and 359 were skipped.
- `make quick`: formatter, clippy, type audit, placement audit, plumb, and report-freshness checks passed in `40.719s`.
- `make world-check`: native/WASM scene byte identity and observer/error checks passed in `70.751s`; WASM measured 471,112 bytes gzipped and 1,240,416 raw.
- Foreground `make gate-commit`: all four argv-safe subfloor chunks passed in `147.722s`; the terminal chunk reported 445 passed and 5,834 skipped.

The integrated fix was committed as `47e8a8dfb`. Its commit hook repeated the
four-chunk gate successfully in `121.152s`; that post-staging timing row is
carried by the following documentation artifact commit.

No census, Sluice submission, push, merge, or campaign close was performed.

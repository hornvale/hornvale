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

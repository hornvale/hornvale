# The Housemark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make a dwelling's threshold composition carry a deterministic, spatially legible signature derived from its living people's authored society vector.

**Architecture:** `windows/vessel` owns a pure `SocietyVector -> Housemark` reduction and carries the optional result on `Brief`. The existing order-sensitive `Pattern` inventory remains the sole composer input; chamber selection intersects its existing gates with a chamber-only `HousemarkGate`, while locale selection ignores that gate.

**Tech Stack:** Rust workspace, `hornvale-species` society registry, `hornvale-thing` kind constants, consolidated vessel integration tests, canonical artifact scripts.

**Status:** Complete — implemented and locally verified 2026-09-04; campaign
close artifacts added before census and sluice submission.

**Spec:** `docs/superpowers/specs/2026-09-04-the-housemark-design.md`

## Global Constraints

- Concept accession epoch 20 is approved for additive `bench` registry/root/world-golden drift; ledger-fact, census, lab-golden, stream, or unrelated world drift remains a stop condition under spec §7.
- Do not add a stream, consume randomness, serialize `Interior`, or change locale-band `selection(built, cold)` behavior.
- Keep one `Pattern` inventory, one order-sensitive draw, one composer, and one validator; no cultural side table or post-compose mutation.
- Append cultural patterns at the end with `at_locale: false`; never insert or reorder existing inventory rows.
- Derive culture from `SocietyVector`, never from a species-name match; do not read `status_basis`.
- Synthetic and unoccupied briefs carry `housemark: None`, which admits universal patterns only.
- Run focused tests while iterating and `make gate-commit` before each implementation commit; expensive stage/merge work belongs to the sluice.

---

### Task 1: Derive the vessel-owned housemark

**Files:**
- Create: `windows/vessel/src/housemark.rs`
- Modify: `windows/vessel/src/lib.rs`
- Modify: `IMPLEMENTATION_PLAN.md`

**Interfaces:**
- Consumes: `hornvale_species::{Sociality, SocietyVector, society_registry}`.
- Produces: `AuthorityMark`, `ThresholdPosture`, `Housemark`, `HousemarkError`, and `Housemark::try_from_society(SocietyVector) -> Result<Housemark, HousemarkError>`.

- [ ] **Step 1: Write boundary and refusal tests before production behavior**

  Cover `0.0`, `0.35`, `0.5`, `0.6`, `0.65`, `1.0`; both open gaps; below zero and above one. Assert `Hierarchic -> Command`, `Communal -> Common`, and error text containing the rejected radius and neighboring admitted bands.

- [ ] **Step 2: Capture behavioral RED**

  Run `cargo test -p hornvale-vessel housemark -- --nocapture`. An unresolved type is only scaffolding RED; rerun after the types exist until the failing assertion names unimplemented classification.

- [ ] **Step 3: Implement the pure reduction**

  Use independent enums and inclusive comparisons:

  ```rust
  let authority = match society.sociality {
      Sociality::Hierarchic => AuthorityMark::Command,
      Sociality::Communal => AuthorityMark::Common,
  };
  let threshold = match society.in_group_radius {
      x if (0.0..=0.35).contains(&x) => ThresholdPosture::Inward,
      x if (0.5..=0.6).contains(&x) => ThresholdPosture::Plain,
      x if (0.65..=1.0).contains(&x) => ThresholdPosture::Outward,
      x => return Err(HousemarkError::UnclassifiedRadius(x)),
  };
  Ok(Housemark { authority, threshold })
  ```

  Derive `Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord` on the value types. Implement `Display` and `Error` for the error; do not round its value.

- [ ] **Step 4: Add the live-registry H1 census**

  Derive every `society_registry()` row into a `BTreeMap<(AuthorityMark, ThresholdPosture), Vec<KindId>>`. Assert six populated cells, no cell holds all rows, and at least three cells hold multiple witnesses. Print the roster; do not freeze proper-name membership.

- [ ] **Step 5: Verify and commit**

  Run `cargo fmt --all`, the focused test, and `make gate-commit`. Mark Stage 1 complete/Stage 2 in progress. Commit `feat(the-housemark): derive the dwelling housemark`.

### Task 2: Admit culturally diagnostic patterns through the shared grammar

**Files:**
- Modify: `domains/thing/src/lib.rs`
- Modify: `windows/vessel/src/interior/pattern.rs`
- Modify: `windows/vessel/src/interior/derive.rs`
- Modify: `windows/vessel/tests/suite/the_blocking.rs`
- Modify: `IMPLEMENTATION_PLAN.md`

**Interfaces:**
- Consumes: Task 1's `Housemark`, `AuthorityMark`, and `ThresholdPosture`.
- Produces: `HousemarkGate::{Universal, Authority(AuthorityMark), Threshold(ThresholdPosture)}` and `selection_for(role, built, cold, populous, housemark: Option<Housemark>)`.

- [ ] **Step 1: Write H2/H4 tests**

  Generate all six marks for a built, warm `Role::Threshold`. Assert ground and threshold in every composition, `permits`, no duplicate kind, matching authority relation, and matching posture relation; Plain admits no posture pattern.

- [ ] **Step 2: Freeze locale behavior before mutation**

  Census names for every `(built, cold)` through `selection`. Run the existing Blocking locale test and record its green baseline.

- [ ] **Step 3: Capture the cross-product RED**

  Run the new focused test. Require an assertion naming an absent authority/posture relation, not a missing signature.

- [ ] **Step 4: Extend admission**

  Add `housemark_gate: HousemarkGate` to `Pattern`; existing rows are `Universal`. Chamber `draw_from`/`selection_for` admits typed gates only on a matching `Some(mark)`. Leave `selection(built, cold)` without a housemark input or cultural filter.

- [ ] **Step 5: Append the vocabulary**

  Add `BENCH` to thing kinds. Narrow the existing screen row to `Threshold(Inward)` for chamber admission; locale selection ignores that gate and must retain its current screen output. Append exactly three new `at_locale: false` rows: threshold command-seat beside threshold, common bench beside ground, and guest-water vessel beside threshold. Use distinct stable pattern names for hall/threshold seats and store/guest water.

- [ ] **Step 6: Prove mutation sensitivity**

  Assert the exact typed gate target exists, temporarily change one gate to the wrong variant, and require H2 to fail by assertion. Restore and rerun green; a compile error or no-op substitution is not proof.

- [ ] **Step 7: Verify and commit**

  Update existing `selection_for` callers with `None` temporarily. Run formatting, focused tests, and `make gate-commit`. Mark Stage 2 complete/Stage 3 in progress. Commit `feat(the-housemark): gate threshold patterns by culture`.

### Task 3: Carry the housemark through `Brief` and chamber derivation

**Files:**
- Modify: `windows/vessel/src/brief.rs`
- Modify: `windows/vessel/src/interior/derive.rs`
- Modify: `windows/vessel/src/session.rs`
- Modify: `windows/vessel/src/lib.rs`
- Modify: all vessel fixtures found by `rg 'Brief::from_parts|brief_of\\(' windows/vessel`
- Modify: `IMPLEMENTATION_PLAN.md`

**Interfaces:**
- Consumes: `society_registry()` and Task 1's derivation.
- Produces: `Brief::housemark: Option<Housemark>` and a fallible production brief path whose error names an unregistered living `KindId`.

- [ ] **Step 1: Write brief tests**

  Assert synthetic construction has no housemark; a production-shaped living occupation matches direct society-row derivation; a missing society row fails with the people id.

- [ ] **Step 2: Capture behavioral RED**

  Run `cargo test -p hornvale-vessel brief::tests -- --nocapture`; after field/signature scaffolding, require a failing behavior assertion.

- [ ] **Step 3: Add explicit optional state**

  Add `housemark` beside `people` and an explicit `housemark` argument to `from_parts`. Update ordinary fixtures with `None`; do not infer culture in this synthetic constructor.

- [ ] **Step 4: Resolve once in `brief_of`**

  For a living occupation, look up `o.core.people`, derive its mark, and return a contextual error on missing row or refused radius. For no occupation, return no people and no mark. Never use `SocietyVector::MANIKIN`.

- [ ] **Step 5: Propagate failure**

  Map failure into the existing contextual vessel error boundary, without panic or swallowing. Update direct test callers deliberately; verify all sites with `rg 'brief_of\\(' windows/vessel`.

- [ ] **Step 6: Feed chamber selection**

  Pass `brief.housemark` into `selection_for`. Add a chamber-index-zero test across all six marks so neither axis relies on a second chamber.

- [ ] **Step 7: Verify and commit**

  Run formatting, brief/interior/pattern tests, and `make gate-commit`. Mark Stage 3 complete/Stage 4 in progress. Commit `feat(the-housemark): derive culture on living briefs`.

### Task 4: Prove the living surface is distinguishable

**Files:**
- Create: `windows/vessel/tests/suite/housemark_readout.rs`
- Modify: `windows/vessel/tests/suite.rs`
- Modify: `windows/vessel/src/brief.rs`
- Modify: `windows/vessel/src/session.rs`
- Modify: the existing settlement-room/occupation indexing helper at the narrowest owning layer
- Modify: `IMPLEMENTATION_PLAN.md`

**Interfaces:**
- Consumes: seeds `[42, 13, 7, 1, 100]`, distinct production settlement rooms, room-keyed living occupations, production brief derivation, and threshold composition.
- Produces: an ignored H3 readout over ordered `(kind, relation-to-required-kind)` signatures.

- [ ] **Step 1: Build the ignored readout**

  First capture the failing 1,275-record probe: 531 settlement rooms reverse through `containing_vertex` to a direct neighbor, producing 222 absent and 41 wrong people plus 16 intentional same-room rung collisions. Replace the lossy vertex reverse lookup with an occupation index keyed by the exact production settlement-room address and the same deterministic first-settlement order used by room names. Then follow the shallowest vessel world scaffold and enumerate every distinct built settlement room. Build its production brief and chamber zero, reduce it to stable structural tuples, and print seed, people, mark, signature plus built/inhabited/unoccupied/collision totals.

- [ ] **Step 2: Assert cell recovery**

  Build `BTreeMap<signature, Housemark>`, fail if a signature maps to two marks, and assert `correct == inhabited`. Assert the room census accounts for every distinct built room and reports all intentional collisions; do not require proper-name uniqueness.

- [ ] **Step 3: Run H3 in the foreground**

  Run `cargo test -p hornvale-vessel --test suite -- housemark_readout --ignored --nocapture`. Below 100% means change relations or reject the design; never rescue it with labels. Record totals in the ledger.

- [ ] **Step 4: Verify and commit**

  Run focused ordinary tests, formatting, and `make gate-commit`. Mark Stage 4 complete/Stage 5 in progress. Commit `test(the-housemark): prove the living cultural readout`.

### Task 5: Classify artifacts and bind the campaign record

**Files:**
- Create/Modify: the needed records in reserved decision block `0746..=0755`
- Modify: `book/src/frontier/idea-registry.md`
- Modify: `docs/superpowers/ledgers/2026-09-04-the-housemark.md`
- Modify: committed generated outputs only when permitted by spec §7
- Delete: `IMPLEMENTATION_PLAN.md` after all stages complete

**Interfaces:**
- Consumes: integrated implementation and actual regeneration output.
- Produces: durable decisions, shipped R1 registry state, classified artifacts, green local gate.

- [ ] **Step 1: Regenerate and classify actual drift**

  Run documented artifact commands, then inspect status/stat. Re-pin chamber transcripts and additive `bench` concept/root/world-golden drift under accession epoch 20. Ledger-fact, census, lab-golden, stream, or unrelated world movement still stops the campaign. No movement is acceptable only with green H2/H3.

- [ ] **Step 2: Bind decisions and registry**

  Record the implemented axes, refusal gaps, shared-inventory gate, threshold survivability, living-occupation ownership, and observed epoch classification in 0746–0755. Mark only R1 shipped in `SOC-staple-ladder`.

- [ ] **Step 3: Finish records**

  Ledger task reviews, H3 totals, artifact paths, rejected branches, and captures. Mark Stage 5 complete and delete `IMPLEMENTATION_PLAN.md`.

- [ ] **Step 4: Final local verification**

  Run `cargo fmt --check`, focused Housemark tests, ignored H3 once, and `make gate-commit`. Inspect the full diff for unrelated changes and untracked TODOs.

- [ ] **Step 5: Commit without closing**

  Commit `docs(the-housemark): bind the dwelling grammar`. Do not merge or push main; G6 remains Nathan's hard stop after whole-branch review and campaign-close flow.

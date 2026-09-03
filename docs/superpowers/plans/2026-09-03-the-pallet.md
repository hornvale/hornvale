# The Pallet Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A body chooses which thing in its room it sleeps on, that choice is committed as a fact, and how much the choice helps varies by species — plus the four inherited constants that denominate a sleep in standard days instead of the world's own.

**Architecture:** Selection is a pure function over a room's already-derived `Interior`, returning an `AnchorId` or `None`. Its result feeds three consumers that exist or are added here: the existing `SiteGrade` fold, a new `SLEPT_ON` fact, and a new per-species grade table in `domains/species`. Nothing moves between crates and no signature crosses a layer boundary.

**Tech Stack:** Rust 2024, std-only plus `serde`/`serde_json`/`libm`. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-09-03-the-pallet-design.md` — read it; this plan argues from it and does not restate §3's constitutional reasoning.

## Global Constraints

- **Rust edition 2024.** Every crate sets `#![warn(missing_docs)]`; every public item, field and variant gets a one-line doc comment.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by `clippy.toml`. Float sorting uses `total_cmp` with a deterministic tie-break.
- **No wall-clock time.** `Instant` is banned, including in tests.
- **Layering is constitutional:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain crate depends on `hornvale-kernel` and **never** a sibling domain.
- **Quantize at emit only**, never in the compute path.
- **Decision 0069:** an entity's persisted position is its **room**. No committed fact may carry an anchor's identity. A `KindId` is not an anchor identity — it is a registered concept — and is legal.
- **Every authored numeric constant needs a `plumb:` rung tag** (decision 0586) or `make plumb` fails the commit gate. Syntax: `/// plumb: per-species(reason)`, last line of the doc block. **A reason must name the AXIS the value varies along, never where the number came from.** A reason built on the word "not" is the tell that it answers the wrong question.
- **`git add -A` is banned.** Commit with explicit pathspecs.
- **`make gate-commit` must pass before every commit**, with an explicit `timeout: 3600000`.
- **The commit gate cannot see byte-goldens.** `make rebaseline` does not write them and `affect_trace` has no sub-floor roster entry. Every task below that changes behaviour runs them **by name**; this is stated per task, deliberately, rather than once here.

---

### Task 1: Group A — denominate the sleep path in the local day

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`next_awake_day`'s two bounds, `SLEEP_BOUT`, `WAKE_SCAN_STEP`)
- Delete: the inverse-assertion test named in Step 4
- Modify: `docs/audits/plumb-roster.md` (regenerated, never hand-edited)

**Interfaces:**
- Consumes: `clock::ticks_per_local_day(day: Option<TickSpan>) -> i64` (`windows/vessel/src/clock.rs:62`), which returns `BASE_TICKS_PER_STD_DAY` when there is no day. Follow this convention; do not invent another.
- Produces: nothing new. `next_awake_day`'s signature is unchanged — it already carries `terrain: &dyn Terrain`.

- [ ] **Step 1: Read the four constants and their consumers**

`SCAN_LIMIT` and `ONE_DAY` are declared inside `next_awake_day`; `WAKE_SCAN_STEP` and `SLEEP_BOUT` are file-level. Read each one's doc and its call sites before editing. `SLEEP_BOUT` is a **floor** applied only to a body going under while awake (`act_span`'s `Action::Sleep` arm uses `cycle.max(SLEEP_BOUT)`), not the sleep span — a controller ruling was once wrong about exactly this, so do not infer its role from its name.

- [ ] **Step 2: Convert all four to the local day**

Each becomes its current fraction **of the local day** rather than of the standard day, via `ticks_per_local_day`. Preserve each one's fraction: `SCAN_LIMIT` is 3/2, `ONE_DAY` is 1, `WAKE_SCAN_STEP` is 1/20, `SLEEP_BOUT` is 2/5.

- [ ] **Step 3: Retag all four**

Each currently carries a `plumb:` tag. Update the reasons to describe the converted state. `WAKE_SCAN_STEP`'s existing reason cites "~3.3 samples/local-day at the legal 4-standard-hour PeriodHours minimum" as the *defect*; after conversion the sample rate is constant per local day, so the reason must say what it now varies with rather than describing a fixed defect.

- [ ] **Step 4: Delete the inverse assertion — this is the success condition**

`a_rest_still_outlasts_the_sleep_scans_give_up_fallback_at_the_100_hour_legal_extreme` (in `liveness.rs`'s own test module) asserts the **current wrong** ordering: that a rest outlasts the sleep fallback at the 100-hour extreme. The Plumb shipped it running rather than `#[ignore]`d precisely so this conversion would redden it.

**Its reddening is the proof the task worked.** Delete it — its own doc instructs the reader who fixes this to delete rather than adjust it. Do not weaken it, do not `#[ignore]` it.

Then remove the `falsifier:` clause from `SCAN_LIMIT`'s and `ONE_DAY`'s `plumb:` reasons, which name it. A citation pointing at a deleted test is worse than none.

- [ ] **Step 5: Add the replacement assertion**

Write a test asserting the ordering that should now hold — a rest is shorter than a sleep — **at a world whose day is far from one standard day**, since a `L = 1` test cannot discriminate. Use `RotationPin::PeriodHours`; its legal range is 4–100 standard hours (`domains/astronomy/src/pins.rs`).

**Name the property, then find a mutation that demonstrates it yourself.** Do not take a prescribed mutation from this plan — a plan author does not know which of these four constants your test is actually sensitive to, and the last campaign's prescribed mutations were nulls twice. Report which constant you reverted and which assertion caught it.

- [ ] **Step 6: Regenerate, gate, run byte-goldens BY NAME, commit**

```bash
cargo run --quiet --manifest-path tools/plumb/Cargo.toml -- report > docs/audits/plumb-roster.md
cargo fmt && make gate-commit
HV_TEST_OK=1 cargo nextest run -p hornvale-lab -p hornvale-vessel -p hornvale
```

**Branch table for the artifact check** (run `make rebaseline`, then diff the declared paths):
- nothing moved → say so and proceed;
- `book/src/gallery/` or `clients/game/core/tests/fixtures/` moved → expected, this changes sleep spans; re-pin and name the mechanism;
- a **census** column moved → report and STOP; the refresh is the controller's, at pre-merge close.

Byte-goldens are separate and are the ones the artifact diff cannot see. If `affect-trace-seed-42.txt` moved, **adjudicate it with a measured breakdown** (label transitions, how many land in `Lost`) in `affect_trace_golden.rs`'s module doc, as its three prior moves each did. A bare `REBASELINE=1` accept is not acceptable.

---

### Task 2: `select_sleep_site` — the body picks

**Files:**
- Create: `windows/vessel/src/sleep_site.rs`
- Modify: `windows/vessel/src/lib.rs` (module declaration)
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: `interior::{Interior, AnchorId, Anchor}` — `Interior::ids() -> Vec<AnchorId>`, `Interior::anchor(id) -> &Anchor`, `Anchor { kind: KindId, within: Option<AnchorId> }`. Read `windows/vessel/src/interior/anchor.rs` first.
- Consumes: `affordance::OfferedVerb::Sleep` and the room-level check at `liveness.rs:3247` that today asks whether *any* anchor offers it. Read that site — it is the thing this task refines.
- Produces: `pub(crate) fn select_sleep_site(interior: &Interior, body: &Body) -> Option<AnchorId>`. Confirm `Body`'s real path and whether it is the right parameter before committing to this signature; if the offer check needs something else, use what the existing call site uses and say so in your report.

- [ ] **Step 1: Write the failing test first**

Three cases, and the third is the one that matters:

1. a room with a `kinds::BED` anchor that offers `Sleep` → returns that anchor;
2. a room whose anchors offer nothing → returns `None`;
3. a room with **two** anchors that both offer `Sleep` → returns a deterministic one, and the test states which and why.

Case 3 exists because "the best" is not yet defined — Task 4 adds the grade. Until then the tie-break must be **deterministic and documented** (ascending `AnchorId` is the obvious choice and matches `Interior::ids()`'s own order). Assert the order, not just that it returns something.

- [ ] **Step 2: Run it, watch it fail, paste the failure**

- [ ] **Step 3: Implement**

Range over `interior.ids()`, keep those whose anchor offers `OfferedVerb::Sleep`, return the first by the documented rule. **Within-room only** — never propose movement, never look at another room (spec §4a; Nathan ruled a body must not wander off to find a bed).

- [ ] **Step 4: The chooser must be ABLE to choose badly — document that it is**

Spec §4b: a chooser that always takes the best site can never produce the tuning signal Nathan asked for. Write that constraint into the function's own doc, naming it, so a later campaign that adds cleverness knows what it must preserve. There is no assertion for this; the doc is the artifact.

- [ ] **Step 5: gate, byte-goldens by name, commit**

This task alone changes no behaviour — nothing calls the new function yet — so the artifact and byte-golden runs are expected to move nothing. **Run them anyway and say so**; a null that was never checked is not a null.

---

### Task 3: `SLEPT_ON` — commit what the body slept on

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (the predicate const, its `_fact` builder, the registration site)
- Modify: wherever `SLEPT` is registered by the session — grep `register_predicate(SLEPT` for the full set; there are several harnesses (`windows/lab/src/synthetic.rs`, `windows/lab/src/health.rs`, and test files) and **missing one is the likely defect in this task**
- Test: in-module, plus wherever `record_sleep` is exercised

**Interfaces:**
- Consumes: `select_sleep_site` (Task 2), and `Anchor::kind` to get the `KindId`.
- Consumes: `kernel::ledger::{Fact, Value}` — `Value::Text(String)` and `Fact.place: Option<EntityId>` both already exist.
- Produces: `pub const SLEPT_ON: &str`, and a fact builder beside `slept_fact`.

- [ ] **Step 1: Write the failing test**

A body that slept on a `kinds::BED` commits a fact with predicate `SLEPT_ON`, object `Value::Text("bed")`, and `place` set to the room. A body that slept on bare ground commits **nothing** — absence is the record for the road, and the test asserts the absence.

- [ ] **Step 2: Run it, watch it fail, paste the failure**

- [ ] **Step 3: Implement, following `SLEPT`'s own shape**

`SLEPT_ON` is registered **by the session, not at genesis**, exactly as `SLEPT` and `RESTED` are — their doc comments say so explicitly. Additive: do not touch `SLEPT`'s object.

- [ ] **Step 4: Find every registration site**

Grep the **observable** — `register_predicate(SLEPT` — not the function you happened to open. A brief that enumerates call sites is asserting completeness, and completeness is what enumeration gets wrong. Report the count you found.

- [ ] **Step 5: gate, artifacts, byte-goldens by name, commit**

A new registered predicate moves the concept registry and its generated reference page. That is an ordinary regen, **not** an epoch — no seed-derivation label is added. Same branch table as Task 1 Step 6.

---

### Task 4: the grade becomes `per-species`

**Files:**
- Modify: `domains/species/src/lib.rs` (a new registry function)
- Modify: `windows/vessel/src/liveness.rs` (`AFFORDED_REST_GAIN` and its consumer, `SiteGrade::gain`)
- Test: coverage ratchet in `domains/species`, behaviour test in `windows/vessel`

**Interfaces:**
- Consumes: nothing new.
- Produces: `pub fn sleep_grade_registry() -> ComponentStore<KindId, f64>`, following `fatigue_rise_registry`'s shape exactly (`domains/species/src/lib.rs:4118`) — read it first.
- **The key is the SLEEPER'S SPECIES, not the site's kind.** The table answers *"how much does an afforded site help a body of this species?"* — a xorn's row is the floor because a xorn gains nothing from a bed. It does **not** answer *"how good is a bed versus bracken"*: that is a `(species, thing)` matrix, it is the `per-people` rung, and it needs the kind-to-kind edges Campaign C builds. Keeping the site binary (afforded / bare, as `SiteGrade` already is) is what makes this task a one-dimensional table rather than Campaign C's work done early and badly.

- [ ] **Step 1: Fix the wrong verdict, and understand why it was wrong**

`AFFORDED_REST_GAIN` carries
`plumb: universal(a uniform multiplier … bounded rather than derived — not a species property)`.
"Bounded rather than derived" answers **where the number came from**; "not a species property" is a **negation**. Both are the tells decision 0586 records for a reason that answers a neighbouring question. A xorn gains nothing from a bed.

- [ ] **Step 2: Build the table, with a coverage ratchet**

Follow `fatigue_rise_registry`: explicit rows, plus a `DEFAULT_` constant in the consumer for unlisted kinds (`liveness.rs:3665,3699` shows the pattern). Add the coverage test that fails when a kind in the species roster has no row — `fatigue_rise_registry` has one; find it and follow it.

- [ ] **Step 3: AUTHOR DIFFERENTIATED VALUES — this is the point of the task**

The Plumb's headline finding is that The Wicket built the per-species mechanism for `FATIGUE_RISE` and left **every value identical at `0.3`** — a mechanism without a difference, which the audit correctly reported as an outstanding defect. Shipping a uniform sleep-grade table would be that same defect with this campaign's name on it.

Values are a **fidelity judgement**: reason from each kind's existing modelled traits (`ThermalStrategy`, habitat, body plan) rather than inventing biology, and **write the reasoning next to each row or in the function's doc**. A xorn (`ThermalStrategy::Absent`) is the clear floor. Where you cannot justify a difference, say so in the doc rather than silently repeating a number.

**Report the distinct-value count.** A table of 39 rows carrying 2 distinct values is a finding, not a success, and the controller wants to see the number.

- [ ] **Step 4: Make `SiteGrade::gain` species-aware**

`gain()` currently returns `1.0` or the constant. It needs the body's kind. Follow the existing call path rather than threading a new parameter through unrelated frames; if that requires a signature change, make it and say which frames moved.

- [ ] **Step 5: Declare the two rungs this campaign does NOT build**

Spec §4d requires the remaining rungs be *declared, tagged seams* rather than
TODO comments — that is the mechanism decision 0586 exists to provide. Tag the
site-kind dimension `per-people` (which thing a people tends to sleep on, needing
Campaign C's kind-to-kind edges) and the idiosyncratic preference
`per-individual` (needing Campaign D's `Lineage`-derived values), each with a
reason naming the axis. They then appear in the committed roster's Fidelity
findings table, which is where a future campaign will find them.

If there is no constant to hang a tag on, say so in your report rather than
inventing one — a tag on a constant that exists only to carry it is worse than
a sentence in the function's doc.

- [ ] **Step 6: gate, artifacts, byte-goldens by name, commit**

Differentiated recovery rates change behaviour on every world. Expect goldens; same branch table as Task 1 Step 6, and the same adjudication requirement for `affect-trace-seed-42.txt`.

---

### Task 5: Definition of Done

**Files:** chronicle, retrospective, registry, decisions, freshness sweep.

- [ ] **Step 1: Read `.claude/skills/closing-a-campaign/SKILL.md` and follow it.** It carries rules this plan does not repeat, including that the retrospective is written from the ledger rather than from memory, and that the worktree is **left in the pool**, never removed.

- [ ] **Step 2: Chronicle** — `book/src/chronicle/the-pallet.md`, wired into `book/src/SUMMARY.md`. **Registry IDs may not appear outside `book/src/frontier/`** — `docs_consistency` enforces it.

- [ ] **Step 3: Retrospective** — `docs/retrospectives/the-pallet.md`, with a **deferred-minors section naming where each minor landed.** The Plumb's ledger contained zero entries tagged "deferred minor" across 38 entries while four review rounds raised them; ledger each as it occurs, not at close.

- [ ] **Step 4: Decisions.** Reserve a block with `make decision-block` — **never `max + 1`**, which collides silently. At minimum the per-species sleep grade and the local-day conversion of the sleep path warrant records; judge whether they are one decision or two.

- [ ] **Step 5: Registry, Gradient, freshness sweep.** Grep `book/src/open-questions.md` for this campaign's domains before concluding no bet moved. `MAP-one-kind-model` stays `elaborated` — addition three (`Lineage`-derived) is still unstarted.

- [ ] **Step 6: Regenerate everything, gate, byte-goldens by name, submit.** Docs-only commits skip `make gate-commit` while `docs_consistency` still reads `docs/` and `book/`, so run it explicitly:

```bash
cargo test -p hornvale --test suite -- docs_consistency generated_paths
```

Then `submitting-to-the-sluice`, which requires an authored `Sluice-Headline:` trailer in the range's last paragraph.

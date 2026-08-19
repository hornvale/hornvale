# The Touchstone Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the myth thread's belief-delta instrument — a first-class library measure that reports which components of a holder's held telling change between two transmission arms — and preregister the demonstration that it separates a belief-rewriting change from an inert one, where the divergence aggregate cannot.

**Architecture:** Promote the route/width-carrying walk out of the private copy in `undertow_readout.rs` into `windows/hearsay/src/traced.rs` (a sibling of the pinned `variants_about_accumulating`, guarded byte-identical). Add `windows/hearsay/src/touchstone.rs`, a per-holder belief-delta over two arms' traced tellings, rolled up with tail counts and a people-pair cut. Validate via a `#[ignore]`d heavy battery running two preregistered controls.

**Tech Stack:** Rust (edition 2024), `cargo nextest`, `windows/hearsay` window crate. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-08-19-the-touchstone-design.md` — read it before Task 1; the plan argues from it.

## Global Constraints

Copied verbatim from the spec and the crate's contracts. Every task's requirements implicitly include this section.

- **Window-only.** All new code lives under `windows/hearsay/`. No kernel edit — the kernel `Claim` is untouched.
- **No epoch, no save-format contract.** `Claim` is never `Serialize` (decision 0100 rule 5). No stream label, no stream-order slot, no `/v2`.
- **The pinned walk stays byte-identical.** `derive::variants_about_accumulating` keeps its exact behaviour and its `Vec<Claim>` signature. The traced walk is a *sibling*; a test asserts `traced(...).map(|t| t.claim) == variants_about_accumulating(...)` holder-for-holder.
- **Preregistration precedes measurement code (decision 0016).** Task 1 freezes the controls and the numeric success criterion into the spec *before* Task 3 (the belief-delta module) exists. Nothing is retuned to rescue a prediction; a falsified prediction ships as the finding.
- **Dependencies:** `serde`, `serde_json`, `libm` only. No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec`; float sorts use `total_cmp`.
- **Every `pub` item, field, variant gets a one-line doc comment** (`#![warn(missing_docs)]`); every primitive at a `pub` boundary gets a `type-audit:` tag.
- **Run `cargo fmt` as the final step of every commit.** Run `make gate-commit` before pushing.
- **Re-derive, never transcribe.** Every published figure comes from an assertion failing on this tree, not copied from `main`. **Absorb `main` at every plan-stage boundary** (`make sluice-stage BRANCH=campaign/the-touchstone REF=<full-sha>`) — a textually-clean merge can hide a moved substrate (the Undertow lost every number to this).
- **`make gate-commit` is an allow-list.** It runs none of this crate's new test files until the roster is rebuilt at merge. Run new tests **by name**; a green gate is not evidence they ran.
- **Implementers are empowered to override the plan and must say so.** Where a step names a *mutation*, it names the *property* the mutation must demonstrate — find a discriminating one by reading the code; the outside guess is usually a null (six such overrides were right across the last two Myth campaigns). Prescribe the property, not the mutation.

---

### Task 1: Preregistration — choose and freeze the two controls

**Files:**
- Create: `windows/hearsay/tests/touchstone_controls_probe.rs` (a probe; may be `#[ignore]`d heavy if it reads worlds)
- Modify: `docs/superpowers/specs/2026-08-19-the-touchstone-design.md:§4` (write the frozen control identities and reachability evidence)

**Interfaces:**
- Consumes: `derive::variants_about_accumulating` (the shipped, route-blind walk, `Vec<Claim>`), `lineage::Lineage`, `derive::people_of`-equivalent people reads, `contact::contact_of`.
- Produces: two frozen control definitions, referenced by name in Tasks 3–4:
  - `POSITIVE` — a `(config_a, config_b)` pair with prior/measured **high held-telling churn** (Claim inequality rate ≥ ~40% on some population) and **low aggregate** (`mutually_exclusive` delta ≤ 4 of ~100).
  - `NEGATIVE` — a `(config_a, config_b)` pair plus a **structurally-identified sub-population** on which the two configs are provably identical (zero churn by a code theorem), non-empty on the panel.

**This task freezes the hypothesis. It must complete and update the spec before Task 3.**

- [ ] **Step 1: Establish the NEGATIVE control's provable-zero theorem in code.**

The recommended NEGATIVE is `(Contact::Descent, Crossing::Free)` vs `(Contact::Descent, Crossing::ContactWeighted)` on the sub-population of holders whose **entire ancestry shares one people** ("people-homogeneous-ancestry" holders). The theorem: under `Contact::Descent` the only route is descent; for a people-homogeneous holder every descent step has `people_of(teller) == people_of(hearer)`, so `crossing_penalty` returns 0 on every step (`derive.rs:124`, the `from == to` guard); identical width ⇒ identical rung, day, hops, route ⇒ **every component identical**. Write a test that *constructs* a tiny people-homogeneous lineage fixture and asserts the two configs produce bit-identical `Claim`s for every holder.

*Property the fixture must demonstrate:* the assertion reddens if the `from == to` guard is deleted. Verify by mutation (temporarily delete the guard, watch it fail, restore). **You are empowered to choose a different NEGATIVE if this one proves unreachable or vacuous** — but it must be non-trivial (not arm A = arm B) and provably zero by a stated theorem, not by a measurement. Say so in your report.

- [ ] **Step 2: Confirm the NEGATIVE population is non-empty on the panel.**

Read the 12-seed panel worlds (`const PANEL: [u64;12]` mirroring `probe_tiebreak_rules.rs:146`). For each foreign ending, count holders whose ancestry is people-homogeneous. Assert the panel-summed count is **> 0** and record it. A zero here means the control is vacuous — pick another population.

Run: `cargo test -p hornvale-hearsay --test touchstone_controls_probe -- negative_population --nocapture`
Expected: prints a positive count; PASS.

- [ ] **Step 3: Confirm the POSITIVE control's prior signature on the panel.**

The POSITIVE control is the **selection-rule / ordering-key swap** — the only known change that rewrites a large fraction of held tellings (prior: **41.9%**) while leaving the aggregate ≤4/100. Its machinery already exists in `probe_tiebreak_rules.rs`: `enumerate` (`:509`) produces every candidate telling reaching every holder over simple paths, and `select(rule, cands)` (`:362`) realizes one selection rule. Arm A = today's rule (smallest width → fewest hops → witness, `:269`); arm B = an alternative `Selection` variant. Over the 12-seed panel, reproduce the held-telling-change rate (`Claim` inequality per holder) and the `mutually_exclusive` aggregate delta between the two rules, and confirm they match the prior signature (~40%+ churn, ≤4/100 aggregate). This is **re-deriving a prior measurement**, not selecting on new data.

**The accumulation-rule swap (Additive↔Multiplicative) is NOT a valid positive control and must not be used** — the aggregate moves 11→43 under it (Undertow §6.3 table), so the aggregate is *not* blind to it; it is not a dissociation case. Any positive control must carry the ≤4/100 aggregate signature.

Run: `cargo test -p hornvale-hearsay --test touchstone_controls_probe -- positive_signature --nocapture`
Expected: churn ≥ ~40% with aggregate delta ≤ 4; PASS. If it does not reproduce (e.g. the substrate moved the number), **that is a finding — report it and stop** rather than lowering the floor.

- [ ] **Step 4: Freeze both controls into the spec.**

Edit `spec §4` to name both controls concretely: the POSITIVE as the two `Selection` rules (with the file:line of `enumerate`/`select` and the re-derived churn/aggregate numbers); the NEGATIVE as `(Descent, Free)` vs `(Descent, ContactWeighted)` on people-homogeneous-ancestry holders, with its theorem and the measured non-empty population count. Restate the frozen success criterion: `positive_tail ≥ 20%` and `negative_tail ≤ 1%` (the negative is expected **exactly 0** on its provable sub-population; the ≤1% is slack for the measurement framing).

**Choosing a strong POSITIVE stimulus is correct experimental design, not selection-on-data** — the hypothesis under test is "the *instrument* sees churn the aggregate misses", not "this control has churn." The detector is what must not be tuned; the stimulus is chosen to be strong on purpose. **You are empowered** to substitute a different POSITIVE with the same property (≥~40% churn, ≤4/100 aggregate) if the `Selection`-swap machinery proves impractical to feed the instrument — but re-measure and re-freeze its signature here first, and say so.

- [ ] **Step 5: Commit.**

```bash
cd .claude/worktrees/the-touchstone
cargo fmt
git add windows/hearsay/tests/touchstone_controls_probe.rs docs/superpowers/specs/2026-08-19-the-touchstone-design.md
git commit -m "test(touchstone): preregister the two controls and freeze the discrimination criterion"
```

---

### Task 2: The traced walk — promote route + width into the library

**Files:**
- Create: `windows/hearsay/src/traced.rs`
- Modify: `windows/hearsay/src/lib.rs` (add `pub mod traced;`)
- Test: `windows/hearsay/tests/traced_walk.rs`

**Interfaces:**
- Consumes: the same inputs as `derive::variants_about_accumulating` — `transmission::Walk`, `ladder::PeopleLadders`, `durations::PeopleDurations`, `accumulate::Accumulation`, `EntityId`, `&str`.
- Produces:

```rust
/// Whether a winning-route step descended the founding tree or rode a raid seam.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Carrier { Descent, Seam }

/// One cross-people step of a winning route.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Crossed { /// raid edges priced into this crossing
    pub edges: u32, /// how the step travelled
    pub carrier: Carrier }

/// A held telling with the route and width the shipped `Claim` discards.
#[derive(Clone, Debug, PartialEq)]
pub struct HeldTelling {
    /// The account exactly as the shipped walk reports it.
    pub claim: hornvale_kernel::Claim,
    /// The originating witness this telling descends from (the walk's key.2).
    pub witness: hornvale_kernel::ledger::EntityId,
    /// Accumulated damage width at emit, full precision.
    pub width: f64,
    /// The winning route's cross-people steps, in traversal order.
    pub crossings: Vec<Crossed>,
}

/// The seam-aware relaxation, carrying the route and width the shipped
/// `variants_about_accumulating` drops. Projects to that function exactly.
pub fn traced_variants_about_accumulating(
    walk: &crate::transmission::Walk,
    ladders: &crate::ladder::PeopleLadders,
    durations: &crate::durations::PeopleDurations,
    rule: crate::accumulate::Accumulation,
    subject: hornvale_kernel::ledger::EntityId,
    predicate: &str,
) -> Vec<HeldTelling>;
```

- [ ] **Step 1: Write the agreement test (failing).**

The load-bearing test. On a real seed-42 world (build via the crate's existing test helpers in `tests/common/mod.rs`), for **every** `Accumulation` rule and **both** `Contact` arms and **both** `Crossing` arms, assert `traced_variants_about_accumulating(...).into_iter().map(|t| t.claim).collect::<Vec<_>>() == variants_about_accumulating(...)` for a sample of subjects. This is the guard that the sibling has not drifted from the pinned walk — the same guard `undertow_readout.rs` pays today, now in the library.

Run it; expect FAIL (`traced_variants_about_accumulating` not defined).

- [ ] **Step 2: Implement `traced.rs` by lifting the readout's private walk copy.**

The relaxation and its `Held`/`Telling`/`Crossed`/`Carrier` types already exist as a private copy in `windows/hearsay/tests/undertow_readout.rs:16-460`. Lift that copy into `src/traced.rs` as the public `HeldTelling`/`Crossed`/`Carrier` + `traced_variants_about_accumulating`, keeping the relaxation logic identical to `derive.rs`'s (width-first key, strict replacement, clock/witness rules). Add doc comments and `type-audit:` tags on every `pub` primitive (`width`, `edges`).

- [ ] **Step 3: Run the agreement test to green, all arms × rules.**

Run: `cargo test -p hornvale-hearsay --test traced_walk -- --nocapture`
Expected: PASS for every rule × Contact × Crossing combination.

- [ ] **Step 4: Prove the route field is load-bearing (mutation).**

*Property:* a test that asserts, on a fixture where a holder is reachable from two distinct witnesses of different peoples, that `HeldTelling.witness` and/or `crossings` distinguish the two routes — and reddens if `traced.rs` hard-codes `witness` to a constant or drops `crossings`. Find the discriminating fixture by reading the walk; do not accept a fixture where both routes share a witness (that was the Undertow's Task-1 null). Verify the mutation reddens, then restore.

- [ ] **Step 5: Commit.**

```bash
cargo fmt && cargo clippy -p hornvale-hearsay --all-targets -- -D warnings
git add windows/hearsay/src/traced.rs windows/hearsay/src/lib.rs windows/hearsay/tests/traced_walk.rs
git commit -m "feat(hearsay): traced walk exposing witness, width, and route the Claim discards"
```

**Stage-boundary absorption:** after Task 2, absorb `main` — `make sluice-stage BRANCH=campaign/the-touchstone REF=$(git rev-parse HEAD)`.

---

### Task 3: The belief-delta module

**Files:**
- Create: `windows/hearsay/src/touchstone.rs`
- Modify: `windows/hearsay/src/lib.rs` (add `pub mod touchstone;`)
- Test: `windows/hearsay/tests/touchstone.rs`

**Interfaces:**
- Consumes: `traced::HeldTelling` from Task 2.
- Produces:

```rust
/// Which components of one holder's held telling changed between two arms.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BeliefDelta {
    /// The holder this delta is about.
    pub holder: hornvale_kernel::ledger::EntityId,
    /// The winning witness or crossing sequence differs.
    pub route_changed: bool,
    /// The remembered day (`claim.object`) differs, bit-exact.
    pub day_changed: bool,
    /// The remembered rung (`claim.precision`) differs.
    pub rung_changed: bool,
    /// The hop depth differs.
    pub hops_changed: bool,
    /// The accumulated width differs, bit-exact.
    pub width_changed: bool,
}
impl BeliefDelta { /// Any tracked component moved.
    pub fn any_changed(&self) -> bool; }

/// Per-holder deltas over the holders reached under BOTH arms, ascending by
/// holder. Holders reached under only one arm are reported separately by
/// [`tail_counts`], never silently dropped.
pub fn belief_deltas(arm_a: &[HeldTelling], arm_b: &[HeldTelling]) -> Vec<BeliefDelta>;

/// Tail counts: the denominator and each component's mover count.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TailCounts {
    /// Holders reached under both arms (the denominator).
    pub reached_both: usize,
    /// Holders reached under only arm A / only arm B (appeared/vanished).
    pub only_a: usize, pub only_b: usize,
    /// Movers per component.
    pub any: usize, pub route: usize, pub day: usize,
    pub rung: usize, pub hops: usize, pub width: usize,
}
/// `any as f64 / reached_both as f64`; the campaign's `changed_tail`.
pub fn changed_tail(t: &TailCounts) -> f64;
pub fn tail_counts(arm_a: &[HeldTelling], arm_b: &[HeldTelling]) -> TailCounts;

/// The same counts keyed on (holder's people, subject's people).
pub fn tail_by_people_pair(
    ledger: &hornvale_kernel::ledger::Ledger,
    subject: hornvale_kernel::ledger::EntityId,
    arm_a: &[HeldTelling], arm_b: &[HeldTelling],
) -> std::collections::BTreeMap<(String, String), TailCounts>;
```

- [ ] **Step 1: Write failing unit tests for `belief_deltas` on hand-built `HeldTelling`s.**

Build two small `Vec<HeldTelling>` by hand (no world). Cover: a holder identical on both arms (all flags false); a holder whose `witness` differs (route_changed true, others as constructed); a holder whose `claim.object` differs by one bit (day_changed true); a holder present only in arm B (`only_b += 1`, not in the delta list). Assert `route_changed` uses `witness != witness || crossings != crossings`, `day_changed`/`width_changed` are **bit-exact** (`to_bits`), `rung_changed`/`hops_changed` are field inequality. Run; expect FAIL.

- [ ] **Step 2: Implement `touchstone.rs`.**

Align by `holder` (both inputs are ascending by holder — a merge join). Emit a `BeliefDelta` for each holder in both; tally `only_a`/`only_b` for the rest. `day_changed`/`width_changed` compare `to_bits()` (a float `==` would call two NaNs unequal and two `-0.0/0.0` equal — bit-exact is the determinism-safe comparison and matches `undertow_readout.rs`'s `day_bits`).

- [ ] **Step 3: Run unit tests to green.**

Run: `cargo test -p hornvale-hearsay --test touchstone`
Expected: PASS.

- [ ] **Step 4: Prove each component flag is load-bearing (mutation, one per flag).**

*Property:* for each of the five flags, a test whose fixture moves exactly that component and asserts exactly that flag fires; it must redden if the flag is hard-wired to `false`. Assert the target text exists before any mutation (`assert old in s`). A flag wired to a constant that still "passes" is the Parley deferral this task exists to prevent — the counter must be shown able to move.

- [ ] **Step 5: Commit.**

```bash
cargo fmt && cargo clippy -p hornvale-hearsay --all-targets -- -D warnings
git add windows/hearsay/src/touchstone.rs windows/hearsay/src/lib.rs windows/hearsay/tests/touchstone.rs
git commit -m "feat(hearsay): per-holder belief-delta with tail and people-pair cuts"
```

---

### Task 4: The Touchstone heavy battery — the discrimination result

**Files:**
- Create: `windows/hearsay/tests/touchstone_readout.rs`
- Modify: `windows/hearsay/tests/undertow_readout.rs` (retire its private walk copy; import `hornvale_hearsay::traced` instead — **only if** the agreement test (Task 2) makes this behaviour-preserving; if it risks moving an Undertow baseline, leave the copy and note it as a followup)

**Interfaces:**
- Consumes: `traced::traced_variants_about_accumulating`, `touchstone::{tail_counts, changed_tail, tail_by_people_pair}`, the two frozen controls from Task 1.

- [ ] **Step 1: Write the battery, `#[ignore]`d into the heavy tier.**

Header the file `#[ignore = "heavy: live-worldgen battery; deferred to the heavy set (decision 0132)"]` on the entry test, matching `undertow_readout.rs:1055`. Over the frozen 12-seed panel, for each foreign ending:
  - **POSITIVE**: realize the two `Selection` rules into two sets of held tellings via the `enumerate`/`select` machinery (arm A = today's rule, arm B = the frozen alternative), map each selected route into a `HeldTelling` (witness, width, crossings are already on the probe's `Telling`), run the instrument's `tail_counts` → accumulate `positive_tail`;
  - **NEGATIVE**: run the traced walk under `(Descent, Free)` and `(Descent, ContactWeighted)`, **restricted to the frozen people-homogeneous sub-population**, run `tail_counts` → accumulate `negative_tail`;
  - also compute the `mutually_exclusive` aggregate delta for **both** controls (reuse `tally_divergence` from `undertow_readout.rs`, or lift it into the library) so the dissociation prints side by side: the aggregate ~0 for both, the instrument high for POSITIVE and 0 for NEGATIVE.

- [ ] **Step 2: Assert the frozen discrimination criterion.**

```
assert!(positive_tail >= 0.20, "instrument blind to a change the aggregate also misses: {positive_tail}");
assert!(negative_tail <= 0.01, "instrument fires on a provably-inert change: {negative_tail}");
assert!(aggregate_delta_positive <= 4, "positive control moved the aggregate more than expected: {aggregate_delta_positive}");
```

Print the full table (both controls × {instrument changed_tail per component, aggregate delta}) as the headline result. **A falsified prediction is the finding** — if `positive_tail < 0.20`, do not retune; report it as the headline and stop for controller review.

- [ ] **Step 3: Run the battery (heavy — pilot first).**

Run a 3-seed pilot to confirm cost (`~2.3 s/seed` expected), then the full panel:
`cargo nextest run -p hornvale-hearsay --run-ignored all -E 'test(touchstone_readout)' --nocapture`
Expected: PASS, with `positive_tail` in the tens of percent and `negative_tail` at 0.

- [ ] **Step 4: If retiring the readout copy (optional), confirm no Undertow baseline moved.**

Run the full Undertow battery after the import swap and confirm its asserted constants are unchanged:
`cargo nextest run -p hornvale-hearsay --run-ignored all -E 'test(undertow_readout)'`
Expected: PASS, byte-identical assertions. If anything moves, revert the swap — the copy stays, banked as a followup.

- [ ] **Step 5: Commit.**

```bash
cargo fmt
git add windows/hearsay/tests/touchstone_readout.rs windows/hearsay/tests/undertow_readout.rs
git commit -m "test(touchstone): the discrimination result — the instrument sees what the aggregate cannot"
```

**Stage-boundary absorption** after Task 4.

---

### Task 5: Definition of Done — book, retrospective, registry

**Files:**
- Create: `book/src/chronicle/the-touchstone.md`, `docs/retrospectives/the-touchstone.md`
- Modify: `book/src/chronicle/SUMMARY.md` (or the chronicle ToC), `book/src/frontier/idea-registry.md` (status flips), `docs/retrospectives/README.md` (index line)

- [ ] **Step 1: Chronicle entry.** Write `book/src/chronicle/the-touchstone.md` at the book's altitude (technical, comprehensible without the code): the aggregate's three-fold blindness, the instrument, and the discrimination result with its real numbers (re-derived from the battery, never transcribed). Grep the claim, not the file — no gloss about a measurement that the measurement does not support (the Undertow's freshness-sweep defect).

- [ ] **Step 2: Registry status flips.** Flip `KNOW-selection-aggregate-dissociation` to reflect the instrument now existing; add rows for the deferred followups (walk unification as a `TOOL-` row; committed readout artifact). Run `cargo test -p hornvale --test suite -- docs_consistency` to green.

- [ ] **Step 3: Retrospective.** Write `docs/retrospectives/the-touchstone.md` — process lessons only, and add its line to `docs/retrospectives/README.md`. Promote the `.superpowers/sdd/` ledger's material entries here **before** teardown (the scratch dies with the worktree).

- [ ] **Step 4: Freshness sweep (decision 0013).** Check the book for chapters this campaign's result dates; re-score any Confidence-Gradient bet it moves (`book/src/open-questions.md`).

- [ ] **Step 5: Verify no generated-artifact drift.** `make rebaseline` then `git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')`. Believed empty (window draws nothing) — but verify by Parley's call-site method first: grep this crate under `windows/lab/` and the census extractors and read down the call chain. Commit any legitimate drift in the same commit.

- [ ] **Step 6: Commit, then close via the sluice** (`submitting-to-the-sluice` skill) — after the G6 controller review.

## Self-Review

- **Spec coverage:** §1 (problem) → chronicle in Task 5; §2 (route discarded) → Task 2; §3 (instrument) → Task 3; §3.1 (library items) → Tasks 2–3; §4 (preregistration) → Task 1; §5 (validation discipline) → the mutation steps in Tasks 1–4 and the absorption notes; §6 (layering/no-epoch) → Global Constraints; §7 (non-goals) → not implemented, by design; §8–9 (decisions/open questions) → Task 1 resolves the negative control; delivery-form and population are resolved in Global Constraints / Task 4.
- **Placeholder scan:** no TBDs; every mutation step names a *property* deliberately (the domain rule), not a vague "add a test".
- **Type consistency:** `HeldTelling`/`Crossed`/`Carrier` defined in Task 2 are consumed unchanged in Tasks 3–4; `TailCounts`/`BeliefDelta`/`changed_tail` defined in Task 3 are consumed unchanged in Task 4.

# The Witness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Repair three branches that could never fire — `Hydro::Spring`/`Aquifer`
(F5), a leading `Tonogenesis` (F7), and the lab's stale duplicate of worldgen's
`Steeped` rules (F13) — and build the guard that makes the shape detectable:
derive the checklist from the type, leave the predicate authored.

**Architecture:** Three independent repairs in three crates
(`domains/terrain`, `domains/language`, `windows/lab`), each followed by the
witness guard that would have caught it. The guards follow the pattern
`windows/worldgen/tests/exposure.rs` already establishes — sweep a small fixed
set of seeds, require every member of a **derived** checklist to be observed at
least once — extended from a hand-written concept list to an enum's own
variants.

**Tech Stack:** Rust edition 2024, no new dependencies (`serde`, `serde_json`,
`libm` only). `cargo nextest` for tests.

**Spec:** `docs/superpowers/specs/2026-07-30-the-witness-design.md`

## Global Constraints

- **No new dependencies.** The allowlist is `ALLOWED_EXTERNAL` in
  `cli/tests/architecture.rs`: `serde`, `serde_json`, `libm`.
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain crate
  depends on `hornvale-kernel` and **nothing else** — never a sibling domain.
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only. Float sorts use
  `total_cmp` with a deterministic tie-break.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Every crate sets `#![warn(missing_docs)]`** — every `pub` item, field and
  variant gets a one-line doc comment.
- **type-audit tag grammar:** every primitive at a `pub` boundary carries
  `/// type-audit: bare-ok(<class>: <field>)` — the class first, then the field
  name, comma-separated on one line. A malformed tag has recurred from plan text
  three campaigns running; copy the grammar, do not improvise it.
- **`cargo fmt` is the final step before every commit.** fmt-gate skips are the
  most common review finding.
- **Run cost-ordered.** `cargo fmt --check` and `cargo clippy` first, then
  `cargo test -p <crate>`, and `make gate` only at a task boundary. Capture once
  and grep the file — never re-run a suite to read a second line:
  `cargo nextest run -p <crate> 2>&1 | tee /tmp/hv-witness-<task>.txt`
- **Host placement (decision 0086):** everything in this plan runs on the **Mac**
  except the census regen in Task 11, which runs on **lefford**.
- **One gating agent at a time on the Mac.** A single `make gate` already reports
  `cpu_ratio` 8.25–8.50 on ten cores. Never start a second one concurrently.

---

## The epoch this campaign owes — read before Task 7

**One epoch: `language/<species>/lexicon/cascade/v2`.** Not
`name/settlement/v4`, and not `lexicon/root/v4`. This was re-derived from the
ratified records after the spec was approved, and it **reverses the spec's
flagged item ①**, which named the other two. The chain:

1. **0084** — a committed derivation moves, so an epoch *is* owed; UNDECLARED is
   "the one unforgivable outcome."
2. **0083** — a label per algorithm. The algorithm that changes is `draw_rule`,
   which lives under `language/<species>/lexicon/cascade`. That leg is
   **unsuffixed** today.
3. **0089, the `ROOT_EPOCH` half** — `lexicon/root/v3` was bumped to `v4` on
   2026-07-27 and **withdrawn** on 2026-07-29, because that campaign "changed the
   phonology the assignment algorithm draws from, not the algorithm — which
   reseeds every root at any label and **has never owed a bump**." This campaign
   changes the *cascade the merger-aware assignment evaluates against*, not the
   assignment algorithm. Same case exactly. Minting `root/v4` would re-make a
   mistake the repo corrected three days ago.
4. **`Namer::glossed_name` is untouched.** 0089 freezes `/v3` against "the next
   campaign that changes what `Namer::glossed_name` **consumes**." Verified in
   source: `Namer::wear` (`domains/language/src/naming.rs:568`) takes
   `&self, segments, frequency` — **no `&mut Stream`**. It draws nothing. The
   wear cascade is drawn on a different stream
   (`.../lexicon/cascade/wear`). `glossed_name` pulls the same draws in the same
   order before and after; only its *input* changes. No `/v4`.
5. **The `draw_phonotactics` precedent** (same crate, cited by 0089) is the house
   form for this exact situation: an unsuffixed leg whose consumption changes
   owes `<leg>/v2`.

**Draw-count invariance is preserved.** `Stream::pick`
(`kernel/src/seed.rs:240`) is `self.next_u64() % items.len()` — **one draw
regardless of slice length**. Picking from 5 kinds instead of 6 consumes exactly
what it consumes today; only the value differs. No draw-count variance is
introduced, so the pin-isolation contracts in
`domains/language/tests/` are unaffected in shape.

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `.superpowers/sdd/baseline-report.md` | frozen pre-repair measurements + H1 | 1 |
| `windows/lab/src/metrics.rs` | the lab's restated gate table + roster | 2, 5 |
| `windows/lab/tests/roster_parity.rs` *(new)* | keystone guard 3 | 3 |
| `domains/terrain/src/lithology.rs` | `porosity`, `hydrogeology`, their tests | 4, 5 |
| `windows/worldgen/src/lib.rs` | `is_spring_cell` — drop the Karst proxy | 5 |
| `domains/terrain/tests/hydro_witness.rs` *(new)* | keystone guard 1 | 6 |
| `domains/language/src/etymology.rs` | `draw_rule` position-awareness | 7 |
| `domains/language/src/streams.rs` | the `CASCADE_V2` epoch leg | 7 |
| `domains/language/src/lib.rs` | `stream_labels()` manifest rows | 7 |
| `domains/language/tests/rule_witness.rs` *(new)* | keystone guard 2 | 8 |
| `book/`, `docs/decisions/`, `docs/retrospectives/` | the documentation half | 10 |

---

### Task 1: Freeze the baseline

**Nothing in this task changes production code.** Its output is the number every
later task is measured against, and H1 frozen before the code that would move it
(decision 0016).

**Files:**
- Create: `.superpowers/sdd/baseline-report.md` (worktree scratch — promoted into
  the retrospective at Task 10, per the scratch-dies-with-the-worktree rule)

**Interfaces:**
- Produces: the four baseline figures Tasks 4, 5, 7 and 9 assert against.

- [ ] **Step 1: Re-derive F5's census columns from the committed artifact**

These are the register's claims and must be re-derived, not inherited — two of
The Wearing's five closed follow-ups were wrong about themselves.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-witness
CSV=book/src/laboratory/generated/the-census/rows.csv
head -1 "$CSV" | tr ',' '\n' | grep -n 'aquifer-fraction\|karst-fraction'
awk -F, 'NR>1{print $34}' "$CSV" | sort -u | head
awk -F, 'NR>1{print $33}' "$CSV" | sort -u | wc -l
```

Expected: column 34 is `aquifer-fraction` and prints exactly `0`; column 33 is
`karst-fraction` and prints `1000`. **If the column indices differ, re-derive
them from the header — do not trust the numbers 33/34.**

- [ ] **Step 2: Re-derive F7's cascade statistics on this tree**

Write a throwaway test in `domains/language/tests/` (deleted in Step 5, not
committed) that, for each of the placed species, draws the wear cascade and
reports: how many contain `Tonogenesis`, how many *lead* with it, and how many
are `Tonogenesis` alone.

```rust
#[test]
fn baseline_wear_cascade_shape() {
    use hornvale_language::etymology::{draw_wear_cascade, RuleKind};
    let species = ["goblin", "kobold", "gnoll", "orc", "human"];
    let (mut total, mut contains, mut leads, mut only) = (0, 0, 0, 0);
    for seed in 0u64..4 {
        for sp in species {
            let c = draw_wear_cascade(&hornvale_kernel::Seed(seed), sp);
            total += 1;
            if c.rules.iter().any(|r| r.kind == RuleKind::Tonogenesis) {
                contains += 1;
            }
            if c.rules.first().map(|r| r.kind) == Some(RuleKind::Tonogenesis) {
                leads += 1;
                if c.rules.len() == 1 {
                    only += 1;
                }
            }
        }
    }
    panic!("BASELINE total={total} contains={contains} leads={leads} only={only}");
}
```

Run: `cargo test -p hornvale-language baseline_wear_cascade_shape -- --nocapture 2>&1 | tee /tmp/hv-witness-t1-cascade.txt`
Expected: a deliberate panic carrying the four counts. **The exact species list
must be the placed roster on this tree — check it against
`windows/worldgen/tests/exposure.rs`'s `placed_peoples` rather than assuming the
five above.**

- [ ] **Step 3: Re-derive F13's false-reading counts**

```bash
CSV=book/src/laboratory/generated/the-census/rows.csv
head -1 "$CSV" | tr ',' '\n' | grep -n 'exposure-sound'
# then, for each matched column index N:
awk -F, -v n=N 'NR>1 && $n=="false"{c++} END{print c}' "$CSV"
```

Expected: roughly 767 (goblin) and 759 (kobold) of 1000. Record the actual
numbers; they are Task 2's success criterion.

- [ ] **Step 4: Re-derive LANG-11's survival funnel**

Locate the 14/650 measurement — grep the chronicle and The Wearing's spec for
`650` and follow it to the code that produced it:

```bash
grep -rn "650" book/src/chronicle/the-wearing.md docs/superpowers/specs/2026-07-27-the-wearing-design.md | head
```

Reproduce it on this tree and record the number. If it is not reproducible from a
committed test or study, record **that** — an unreproducible headline figure is
itself a finding, and Task 9 needs a reproducible baseline or it has nothing to
compare against.

- [ ] **Step 5: Write the baseline report, delete the throwaway test, commit**

`.superpowers/sdd/baseline-report.md` records all four figures, the date, the
commit SHA they were measured at, and **H1 verbatim**:

> **H1.** Removing the unconditioned tonogenesis draw raises the wear cascade's
> match rate, and therefore raises toponymic name survival above the baseline
> recorded in Step 4. **The null — survival does not move — is publishable** and
> means LANG-11's bottleneck is downstream of the cascade, not in it.

Delete the throwaway test file.

```bash
cargo fmt
git add -A
git commit -m "measure(the-witness): freeze the pre-repair baseline and H1"
```

The report is gitignored scratch; the commit records the deletion of the
throwaway and any note files that are tracked. **If `git status` shows nothing to
commit, that is correct — say so and move on rather than forcing a commit.**

---

### Task 2: F13 — teach the lab duplicate what it never learned

**Files:**
- Modify: `windows/lab/src/metrics.rs` (`independently_steeped_concepts`, ~line 4795)

**Interfaces:**
- Consumes: Task 1's Step-3 false-reading counts.
- Produces: a repaired `independently_steeped_concepts`; Task 3 guards it.

- [ ] **Step 1: Enumerate the drift, do not trust the list**

F13 names The Watershed's six staples, but the entry also records that this is
the *third* recurrence — so the audit enumerates rather than trusts. Compare the
concept set worldgen classifies against the set the lab considers:

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-witness
grep -n "steeped.insert\|universal_stratum\|_pack()" windows/lab/src/metrics.rs | sed -n '1,40p'
grep -n "ExposureClass::Steeped" windows/worldgen/src/lib.rs
```

Write the two sets side by side in the task report. Every concept worldgen can
class `Steeped` that the lab never inserts is drift.

- [ ] **Step 2: Write the failing test**

Add to `windows/lab/src/metrics.rs`'s test module:

```rust
#[test]
fn the_independent_reading_covers_every_staple_worldgen_can_steep() {
    // The Watershed's staples reach `Steeped` through the pack ladder, and
    // this reading never learned them — F13, the third recurrence of the
    // duplicate going stale. Named individually so ADDING a staple that the
    // lab does not know about reds this test rather than slipping past it.
    let v = full_view_for_seed(42);
    let steeped = independently_steeped_concepts(&v, "goblin")
        .expect("goblin is placed at seed 42");
    for staple in STAPLE_CONCEPTS {
        assert!(
            steeped.contains(*staple),
            "the lab's independent reading does not steep {staple}, which \
             worldgen does — the duplicate is stale again"
        );
    }
}
```

**`STAPLE_CONCEPTS` is the list Step 1 derived**, declared as a `const` beside
the gate table with a doc comment naming The Watershed. `full_view_for_seed` is
the existing helper — check its real name in the test module and use that.

- [ ] **Step 3: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-lab the_independent_reading_covers_every_staple 2>&1 | tee /tmp/hv-witness-t2.txt`
Expected: FAIL, naming the first missing staple.

- [ ] **Step 4: Repair `independently_steeped_concepts`**

Extend the pack-ladder loop to include the staple pack(s) Step 1 identified,
following the existing shape exactly:

```rust
for entry in hornvale_language::color_pack()
    .iter()
    .chain(hornvale_language::body_pack())
    .chain(hornvale_language::kin_pack())
    .chain(hornvale_language::staple_pack())   // NEW — The Watershed
{
    if hornvale_language::in_ladder(entry, &depths) {
        steeped.insert(entry.concept.to_string());
    }
}
```

**Verify the real accessor name** (`staple_pack` is the expected form; confirm
against `domains/language/src/packs.rs`) rather than assuming it.

- [ ] **Step 5: Run the test and the lab suite**

Run: `cargo nextest run -p hornvale-lab 2>&1 | tee /tmp/hv-witness-t2b.txt`
Expected: PASS, and no other lab test reddened.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/lab/src/metrics.rs
git commit -m "fix(lab): discharge F13 — the independent reading learns the staples

exposure-sound-{goblin,kobold} read false on ~767/759 of 1000 worlds because
independently_steeped_concepts never learned The Watershed's staples. Third
recurrence of one defect; Task 3 adds the guard that ends the series."
```

---

### Task 3: Keystone guard 3 — parity of roster, independence of predicate

**Files:**
- Create: `windows/lab/tests/roster_parity.rs`

**Interfaces:**
- Consumes: Task 2's repaired `independently_steeped_concepts`.
- Produces: the guard that makes a fourth recurrence impossible.

The lab duplicate's *independence* is load-bearing and must survive this task —
the module note is explicit that importing worldgen's predicates "would turn the
check into an echo of the thing it exists to check." This guard therefore checks
only that the two consider the **same set of concepts**, never that they agree on
any concept's verdict.

- [ ] **Step 1: Write the failing test**

```rust
//! Roster parity between worldgen's exposure classification and the lab's
//! deliberately-independent second opinion.
//!
//! The lab restates worldgen's `Steeped` rules rather than importing them, so
//! the metric is a SECOND OPINION and not an echo — that is the design, and it
//! has caught real bugs twice. Its one actual failure mode, three campaigns
//! running (The Wearing's toponymic concepts, The Toponym's variants, The
//! Watershed's staples), is that the duplicate's ROSTER silently loses
//! entries while its PREDICATES stay fine.
//!
//! Nothing in the independence argument requires the roster to be
//! hand-maintained too. This guard asserts parity of the concept SET while
//! leaving every predicate independently restated: adding a concept worldgen
//! can steep now reds this test instead of quietly reading false on three
//! quarters of the census.

#[test]
fn the_lab_considers_every_concept_worldgen_can_steep() {
    let worldgen_can_steep = /* derived from the concept registry, the way
        exposure.rs derives its list from `concept_domain` — NOT a literal */;
    let lab_considers = hornvale_lab::steepable_concept_roster();
    let missing: Vec<_> = worldgen_can_steep
        .difference(&lab_considers)
        .cloned()
        .collect();
    assert!(
        missing.is_empty(),
        "the lab's independent reading does not consider {missing:?} — \
         worldgen can steep them, so the second opinion is blind there"
    );
}
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-lab --test roster_parity 2>&1 | tee /tmp/hv-witness-t3.txt`
Expected: FAIL — `steepable_concept_roster` does not exist yet.

- [ ] **Step 3: Expose the lab's roster**

Add to `windows/lab/src/metrics.rs`, `pub` and re-exported from the crate root:

```rust
/// Every concept the independent reading is capable of steeping — the roster
/// half of `independently_steeped_concepts`, exposed so a test can check it
/// against worldgen's without importing either side's predicates.
///
/// Roster parity, predicate independence: this returns WHAT is considered,
/// never HOW any of it is decided.
/// type-audit: bare-ok(identifier-text: return)
pub fn steepable_concept_roster() -> std::collections::BTreeSet<String> { /* … */ }
```

Refactor `independently_steeped_concepts` so the roster is derived from this
single source rather than restated — otherwise the guard checks a copy of the
list and this task has built the very defect it is closing.

- [ ] **Step 4: Verify the guard can actually fail (mutation)**

**Do not skip this step.** The Timekeeper found eight instances of an assertion
that could never fire, four of them inside the machine built to detect them, and
only a mutation step caught the one that mattered.

Temporarily remove one staple from the lab roster, re-run, and confirm **RED**
naming that staple. Then restore it and confirm **GREEN**. Record both outputs in
the task report.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-lab --all-targets -- -D warnings
git add windows/lab/
git commit -m "test(lab): roster parity ends F13's recurrence series

Parity of roster, independence of predicate: the second opinion keeps every
property it was built for, and loses the only failure mode it has suffered.
Mutation-verified — removing a staple reds it."
```

---

### Task 4: F5 — sweep for the porosity coefficient

**Measurement task. No committed behaviour change.** `k_g` is calibrated here,
not chosen in the spec, because terrain's constants "were chosen from data sweeps
against worst-case seeds" (decision 0057) and changing one "is a retune, not a
cleanup" (`domains/terrain/CLAUDE.md`).

**Files:**
- Create (throwaway, deleted in Step 4): `domains/terrain/tests/porosity_sweep.rs`

**Interfaces:**
- Produces: the chosen `k_g`, with the four-metric table that justifies it.

- [ ] **Step 1: Write the sweep**

For `k_g` in `[0.10, 0.15, 0.20, 0.25, 0.30]`, over seeds `0..8`, report **four**
metrics — not one:

1. fraction of land cells reading `Hydro::Aquifer`
2. fraction reading `Hydro::Spring`
3. fraction reading `Hydro::Karst` (**the guard rail** — this must stay within
   its present distribution)
4. the `RockClass::Sandstone` / `Shale` split (porosity also gates
   `lithology.rs:222`, `porosity < 0.2 && silica > 0.5`)

The candidate formula:

```rust
let porosity = (0.5 * buf_carbonate
    + k_g * grain * (1.0 - induration)
    + 0.3 * (1.0 - metamorphic_grade))
    .clamp(0.0, 1.0);
```

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-terrain porosity_sweep -- --nocapture 2>&1 | tee /tmp/hv-witness-t4.txt`

- [ ] **Step 3: Choose `k_g` against a written criterion**

**Write the criterion down before reading the table** — post-hoc selection is the
metric-chasing the preregistration discipline exists to contain:

> Choose the **smallest** `k_g` at which both `Spring` and `Aquifer` are
> witnessed on at least two of the eight seeds, subject to `karst-fraction`'s
> mean moving by less than one bucket width (the census bucket edges are
> `[0.0, 0.05, 0.1, 0.2, 0.3, 0.4]`, so one width at the low end is 0.05).

If no candidate satisfies both, **stop and report** — that is a finding (the two
constraints are in tension and the branch cannot be revived by this term alone),
not a licence to widen the sweep until something passes.

- [ ] **Step 4: Record, delete the throwaway, commit the report**

Record the full table in the task report, delete `porosity_sweep.rs`, and append
the chosen value and criterion to `.superpowers/sdd/baseline-report.md`.

---

### Task 5: F5 — give porosity its missing terms and retire the Karst proxy

**Files:**
- Modify: `domains/terrain/src/lithology.rs` (`porosity` at ~line 445; the
  `hydrogeology` unit tests at ~lines 930–957)
- Modify: `windows/worldgen/src/lib.rs` (`is_spring_cell`, ~line 3894)
- Modify: `windows/lab/src/metrics.rs` (`lab_is_spring_cell`, ~line 4747)

**Interfaces:**
- Consumes: Task 4's `k_g`.
- Produces: reachable `Hydro::Spring`/`Aquifer`; Task 6 guards it.

- [ ] **Step 1: Write the failing test against the REAL derivation**

This is the whole point of the campaign: the existing test at `lithology.rs:950`
hand-builds `porosity = 0.7, carbonate = 0.05`, which the real derivation cannot
produce. The new test must build a **world**.

```rust
#[test]
fn a_real_world_produces_a_porous_non_carbonate_cell() {
    // The defect this closes: `hydrogeology_reads_porosity_and_carbonate` and
    // `high_porosity_with_flow_and_low_carbonate_reads_as_spring` both pass
    // today on hand-built MaterialBuffers that the derivation CANNOT emit —
    // porosity was a function of carbonate alone, so a porous non-carbonate
    // rock (sandstone, the archetypal aquifer) did not exist. Asserted here
    // against a derived buffer, never a constructed one.
    let terrain = build_terrain_for_seed(0);
    let found = terrain
        .geosphere()
        .cells()
        .filter(|&c| !terrain.is_ocean(c))
        .any(|c| {
            let h = terrain.hydro_at(c);
            h == hornvale_terrain::Hydro::Aquifer || h == hornvale_terrain::Hydro::Spring
        });
    assert!(
        found,
        "no land cell on seed 0 reads Aquifer or Spring — the branch is still \
         unreachable from the real derivation"
    );
}
```

Use the terrain-building helper the existing terrain integration tests use;
check `domains/terrain/tests/tectonic_properties.rs` for its real name.

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-terrain a_real_world_produces_a_porous 2>&1 | tee /tmp/hv-witness-t5.txt`
Expected: FAIL — "the branch is still unreachable".

- [ ] **Step 3: Apply the porosity terms**

At `domains/terrain/src/lithology.rs:445`, replacing the one-line formula and its
comment:

```rust
// Porosity: dissolution in carbonate (karst), packing in loose coarse
// grain, and recrystallisation closing pores in metamorphics. The grain
// term is The Witness's repair (F5): porosity was previously a function of
// carbonate ALONE, which made `porosity > 0.5` imply `carbonate > 0.4` and
// so put every porous cell inside the Karst branch's pre-emption — the
// `Spring`/`Aquifer` arms were unreachable on all 1000 census seeds. A
// porous NON-carbonate rock is sandstone, the archetypal aquifer, and the
// model could not represent one. `GRAIN_POROSITY_GAIN` is calibrated by
// sweep (Task 4), not chosen: it is the smallest value that witnesses both
// arms while leaving `karst-fraction` inside its present bucket.
let porosity = (0.5 * carbonate
    + GRAIN_POROSITY_GAIN * grain * (1.0 - induration)
    + 0.3 * (1.0 - metamorphic_grade))
    .clamp(0.0, 1.0);
```

with the constant declared beside `SPRING_DRAINAGE_THRESHOLD`:

```rust
/// How much loose, uncemented coarse grain contributes to porosity.
/// Calibrated by sweep against `aquifer-fraction`, `karst-fraction`, the
/// sandstone/shale split and `cave_proneness` (The Witness, Task 4) — a
/// retune of this value is a retune, not a cleanup (decision 0057).
const GRAIN_POROSITY_GAIN: f64 = /* Task 4's value */;
```

- [ ] **Step 4: Re-derive the two hand-built unit tests**

`hydrogeology_reads_porosity_and_carbonate` and
`high_porosity_with_flow_and_low_carbonate_reads_as_spring` stay — a pure
function deserves pure unit tests — but each gains a comment stating that its
buffer is **synthetic and does not certify reachability**, pointing at the new
world-derived test and at Task 6's guard. That comment is the campaign's lesson
written where the next reader will hit it.

- [ ] **Step 5: Retire the Karst proxy in both places**

`windows/worldgen/src/lib.rs:3894` — `is_spring_cell` reads the real variant:

```rust
fn is_spring_cell(terrain: &GeneratedTerrain, cell: hornvale_kernel::CellId) -> bool {
    terrain.hydro_at(cell) == hornvale_terrain::Hydro::Spring
}
```

Rewrite its doc comment: delete the `Hydro::Spring is structurally unreachable`
paragraph and the `spring ⊆ river` disclosure (the 132-of-137 measurement), and
say instead that `Spring` became reachable in The Witness and that `spring` is now
an independent signal rather than `river` partitioned by rock type.

`windows/lab/src/metrics.rs:4747` — `lab_is_spring_cell` restates the *new*
reading independently, and its comment says why it is no longer the Karst proxy.

- [ ] **Step 6: Run terrain, worldgen and lab**

```bash
cargo nextest run -p hornvale-terrain -p hornvale-worldgen -p hornvale-lab \
  --no-fail-fast 2>&1 | tee /tmp/hv-witness-t5b.txt
```

Expected: green. **`windows/worldgen/tests/exposure.rs`'s sweep is the one to
watch** — it is the test that caught `spring` the first time, and Task 2 landed
before this one precisely so it does not redden here.

- [ ] **Step 7: Re-baseline the artifacts drifted by the terrain change**

```bash
SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

**Review the diff, do not commit it blind** — the drift check exists to make you
look. The elevation map and the seed-42 almanacs are expected to move; a moved
`docs/audits/type-audit-report.md` means a `pub` signature changed and wants
explaining.

- [ ] **Step 8: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A
git commit -m "fix(terrain): discharge F5 — porosity gains the terms it lacked

porosity was 0.5*carbonate + 0.3*(1-metamorphic_grade), whose second term
caps at 0.3 — so porosity > 0.5 forced carbonate > 0.4 and the Karst branch
pre-empted Spring/Aquifer on every seed. Verified on the committed census:
aquifer-fraction took exactly one distinct value, 0, across 1000 seeds.

The model could not represent a porous non-carbonate rock. It can now, and
is_spring_cell drops the Karst proxy it has stood on since The Wearing."
```

---

### Task 6: Keystone guard 1 — every `Hydro` variant must be witnessed

**Files:**
- Create: `domains/terrain/tests/hydro_witness.rs`

Follow `windows/worldgen/tests/exposure.rs`'s established shape: a small fixed
seed sweep, an early break once every member is found, and a **derived**
checklist. Its own doc comment records that deriving the list (from
`concept_domain`) rather than hardcoding it was checked *by injection* and
matters in the dangerous direction — adding an unreachable member must red the
test, not slip past it. Same discipline here, with `Hydro`'s variants as the
checklist.

- [ ] **Step 1: Write the failing test**

```rust
//! Every `Hydro` variant must be witnessed in a real derivation.
//!
//! `Hydro::Spring` and `Hydro::Aquifer` were unreachable on every seed for
//! the whole life of the lithology model, and every test that touched them
//! passed — because each hand-built a `MaterialBuffer` the derivation cannot
//! emit. A unit test over a constructed input certifies the FUNCTION; it
//! cannot certify that anything ever calls it with those values. This does.

#[test]
fn every_hydro_variant_is_witnessed_on_a_real_world() {
    let all = hornvale_terrain::Hydro::ALL;
    let mut witnessed: std::collections::BTreeSet<hornvale_terrain::Hydro> =
        std::collections::BTreeSet::new();
    for seed in 0u64..8 {
        let terrain = build_terrain_for_seed(seed);
        for cell in terrain.geosphere().cells() {
            witnessed.insert(terrain.hydro_at(cell));
            if witnessed.len() == all.len() {
                break;
            }
        }
        if witnessed.len() == all.len() {
            break;
        }
    }
    let missing: Vec<_> = all.iter().filter(|v| !witnessed.contains(v)).collect();
    assert!(
        missing.is_empty(),
        "no seed in 0..8 produces {missing:?} — the variant is unreachable \
         from the real derivation, and no sweep width saves it"
    );
}
```

`Hydro` needs `Ord` for the `BTreeSet` (the `HashSet` ban is workspace-wide) and
an `ALL` roster:

```rust
impl Hydro {
    /// Every variant, so a witness test derives its checklist from the type
    /// rather than from an author. Adding a variant enrolls it automatically.
    pub const ALL: [Hydro; 5] = [
        Hydro::Aquifer,
        Hydro::Aquitard,
        Hydro::Spring,
        Hydro::Runoff,
        Hydro::Karst,
    ];
}
```

Derive `PartialOrd, Ord` on `Hydro` alongside its existing derives.

- [ ] **Step 2: Verify it fails before Task 5's fix**

```bash
git stash && cargo nextest run -p hornvale-terrain every_hydro_variant 2>&1 | tee /tmp/hv-witness-t6-red.txt
git stash pop
```

Expected RED naming `Spring` and `Aquifer`. **If it passes on the pre-fix tree,
the test is not measuring what it claims** — stop and diagnose. Record the RED
output in the task report; it is the evidence that the guard works.

- [ ] **Step 3: Run it on the fixed tree**

Run: `cargo nextest run -p hornvale-terrain every_hydro_variant 2>&1 | tee /tmp/hv-witness-t6.txt`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-terrain --all-targets -- -D warnings
git add domains/terrain/
git commit -m "test(terrain): every Hydro variant must be witnessed, not merely constructible"
```

---

### Task 7: F7 — a cascade may not draw a tonogenesis it cannot condition

**Read the epoch section at the top of this plan before starting.** One epoch:
`language/<species>/lexicon/cascade/v2`.

**Files:**
- Modify: `domains/language/src/etymology.rs` (`draw_rule` ~188,
  `draw_cascade_with_regime` ~216, `draw_wear_cascade` ~244, `RULE_KINDS` ~132)
- Modify: `domains/language/src/streams.rs` (declare the epoch leg)
- Modify: `domains/language/src/lib.rs` (`stream_labels()` rows, ~578–583)

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn no_drawn_cascade_leads_with_a_tonogenesis() {
    // A leading Tonogenesis is PROVABLY the identity: `evolve` opens with
    // `pending = None` and only a FIRED merger ever sets it, so
    // `apply_tonogenesis` returns unchanged on every word in every language.
    // In the 1-2 rule WEAR regime that can be the entire cascade. The test
    // fixture that masked this (`merge_then_tonogenize`) puts the merger
    // first, so the absent-conditioning case was never exercised.
    for seed in 0u64..64 {
        for sp in ["goblin", "kobold", "gnoll"] {
            for cascade in [
                draw_cascade(&Seed(seed), sp),
                draw_wear_cascade(&Seed(seed), sp),
            ] {
                let mut seen_merger = false;
                for rule in &cascade.rules {
                    if rule.kind == RuleKind::Tonogenesis {
                        assert!(
                            seen_merger,
                            "seed {seed} / {sp}: a Tonogenesis is drawn with no \
                             prior merger, so it is the identity by construction"
                        );
                    }
                    if matches!(
                        rule.kind,
                        RuleKind::ClusterSimplify | RuleKind::FinalLoss
                    ) {
                        seen_merger = true;
                    }
                }
            }
        }
    }
}
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-language no_drawn_cascade_leads 2>&1 | tee /tmp/hv-witness-t7.txt`
Expected: FAIL at a low seed.

- [ ] **Step 3: Make `draw_rule` position-aware**

```rust
/// The kinds available before any merger has been drawn — every kind except
/// [`RuleKind::Tonogenesis`], which needs a prior merger's dropped feature to
/// condition on and is otherwise the identity (`evolve` opens with no pending
/// conditioning). The model claim, not merely the optimisation: a language does
/// not innovate tone before it has a merger to feed it — tonogenesis is a
/// CONSEQUENCE of segmental loss.
const RULE_KINDS_UNCONDITIONED: [RuleKind; 5] = [
    RuleKind::Lenition,
    RuleKind::Fortition,
    RuleKind::VowelShift,
    RuleKind::ClusterSimplify,
    RuleKind::FinalLoss,
];

/// Draw one rule: a kind, then a param, in that order. `seen_merger` selects
/// the kind roster — see [`RULE_KINDS_UNCONDITIONED`]. Draw COUNT is identical
/// either way (`Stream::pick` is one `next_u64()` regardless of slice length),
/// so this changes drawn values, never consumption.
fn draw_rule(stream: &mut Stream, seen_merger: bool) -> SoundRule {
    let kinds: &[RuleKind] = if seen_merger {
        &RULE_KINDS
    } else {
        &RULE_KINDS_UNCONDITIONED
    };
    let kind = *stream.pick(kinds).expect("both rosters are non-empty");
    let param = stream.range_u32(RULE_PARAM_RANGE.0, RULE_PARAM_RANGE.1);
    SoundRule { kind, param }
}
```

Both call sites thread the flag, replacing the `map` with a fold that tracks it:

```rust
let mut rules = Vec::with_capacity(count as usize);
let mut seen_merger = false;
for _ in 0..count {
    let rule = draw_rule(&mut stream, seen_merger);
    if matches!(rule.kind, RuleKind::ClusterSimplify | RuleKind::FinalLoss) {
        seen_merger = true;
    }
    rules.push(rule);
}
```

Apply to **both** `draw_cascade_with_regime` and `draw_wear_cascade` — the shared
repair is decision #4 and Nathan approved it at G3.

- [ ] **Step 4: Mint the epoch leg**

In `domains/language/src/streams.rs`, beside `CASCADE`:

```rust
/// The epoch-2 suffix leg for the sound-change cascade, one level below
/// [`CASCADE`] and above [`WEAR`].
///
/// The Witness (2026-07-30) makes the cascade draw position-aware:
/// [`crate::etymology::RuleKind::Tonogenesis`] is no longer offered at a
/// position where no merger has been drawn, because `evolve` opens with no
/// pending conditioning and such a rule is provably the identity. Draw COUNT
/// is unchanged (`Stream::pick` is one draw at any slice length); the drawn
/// VALUES move, so every cascade in every world is reseeded. Deliberate
/// regeneration uses an epoch suffix, never a rename — the save-format
/// contract — so `v2` reseeds every cascade and v1's forms are gone by
/// design, regenerated with the world.
///
/// **Why this leg and not `name/settlement/v4` or `lexicon/root/v4`.** 0083
/// puts a label on the algorithm that changed, and the algorithm that changed
/// is `draw_rule`, which lives here. `Namer::glossed_name` consumes exactly
/// what it consumed before (`Namer::wear` takes no `&mut Stream` — it draws
/// nothing), so 0089's freeze on `name/settlement/v3` is not tripped. And
/// `ROOT_EPOCH` stays at `v3` on 0089's own precedent: a `v4` minted for
/// exactly this reason — an input to the assignment moving, not the
/// assignment itself — was withdrawn on 2026-07-29.
/// type-audit: bare-ok(identifier-text: return)
pub const CASCADE_V2: StreamLabel<'static> = StreamLabel::from_static("v2");
```

Insert `.derive(streams::CASCADE_V2)` after `.derive(streams::CASCADE)` in
**both** draw functions, and update each one's doc-comment derivation path.
`WEAR` hangs below it, so the wear stream becomes
`…/lexicon/cascade/v2/wear` — which is intended, and which `split_version`
already supports (`room/layout/v1/rectilinear` is the precedent for a version
segment mid-path).

- [ ] **Step 5: Update the generated manifest rows**

`domains/language/src/lib.rs:578` and `:582` — the label strings become
`language/<species>/lexicon/cascade/v2` and
`language/<species>/lexicon/cascade/v2/wear`. **A `stream_labels()` change owes a
manifest regen step, and forgetting it is a standing memory note:**

```bash
bash scripts/regenerate-artifacts.sh
git diff book/src/reference/stream-manifest-generated.md
```

- [ ] **Step 6: Confirm the epoch is stamped**

The mechanical test from 0089 — the epoch must actually reach a saved world, or
the bump is a declaration with no referent:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-witness-w.json
grep -o '"language/<species>/lexicon/cascade": *"v2"' /tmp/hv-witness-w.json \
  || python3 -c "import json;print(json.load(open('/tmp/hv-witness-w.json'))['derived_under'])"
```

Expected: the stamp carries `v2` for the cascade stem. **If it does not, the
label is not reaching `versioned_labels()`** and the bump is inert — stop and fix
that before proceeding.

- [ ] **Step 7: Run the language crate and the pin-isolation batteries**

```bash
cargo nextest run -p hornvale-language --no-fail-fast 2>&1 | tee /tmp/hv-witness-t7b.txt
cargo test -p hornvale-astronomy --test genesis_properties
cargo test -p hornvale-terrain --test tectonic_properties
```

The astronomy/terrain pin-isolation tests should be untouched; run them anyway,
because "a pin must consume the same draws as the unpinned path" is the contract
most easily broken from a distance.

- [ ] **Step 8: Re-baseline and commit**

```bash
SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh
git diff --stat book/ docs/audits/
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A
git commit -m "feat(language): discharge F7 — a cascade cannot draw a tonogenesis it cannot condition

evolve() opens with pending=None and only a fired merger sets it, so a
Tonogenesis drawn before any merger is provably the identity on every word in
every language. In the 1-2 rule WEAR regime it can be the whole cascade.

Epoch: language/<species>/lexicon/cascade/v2 — the leg whose algorithm moved
(0083). NOT name/settlement/v4 (glossed_name's consumption is unchanged;
Namer::wear draws nothing) and NOT lexicon/root/v4 (0089's own withdrawn-v4
precedent: an input to the assignment moved, not the assignment).

Draw-count invariance holds: Stream::pick is one next_u64() at any slice
length, so only the drawn values move."
```

---

### Task 8: Keystone guard 2 — every `RuleKind` must be witnessed firing

**Files:**
- Create: `domains/language/tests/rule_witness.rs`

- [ ] **Step 1: Write the test**

```rust
//! Every `RuleKind` must be witnessed CHANGING a word.
//!
//! A rule that is drawn, applied, and never changes anything is
//! indistinguishable from a rule that works — `AppliedRule.changed` records
//! it, and nothing read that record. `Tonogenesis` was in that state for
//! every cascade that drew it before a merger. This is the rule-shaped twin
//! of `domains/terrain/tests/hydro_witness.rs`: a variant is only alive if
//! some real derivation exercises it.

#[test]
fn every_rule_kind_is_witnessed_changing_a_word() {
    let mut fired: std::collections::BTreeSet<RuleKind> =
        std::collections::BTreeSet::new();
    for seed in 0u64..32 {
        for sp in ["goblin", "kobold", "gnoll"] {
            let ph = /* draw the species' phonology */;
            let cascade = draw_cascade(&Seed(seed), sp);
            for concept in hornvale_language::universal_stratum() {
                let proto = proto_root(&Seed(seed), sp, concept.concept, &ph);
                let d = evolve(&proto, &cascade, &ph);
                for step in &d.steps {
                    if step.changed {
                        fired.insert(step.rule.kind);
                    }
                }
            }
        }
    }
    let missing: Vec<_> = RuleKind::ALL.iter().filter(|k| !fired.contains(k)).collect();
    assert!(
        missing.is_empty(),
        "no rule of kind {missing:?} ever changed a word across the sweep — \
         the kind is inert in practice, whatever its implementation says"
    );
}
```

Add `RuleKind::ALL` with the same doc rationale as `Hydro::ALL`, and derive
`PartialOrd, Ord` on `RuleKind`.

- [ ] **Step 2: Verify it fails on the pre-Task-7 tree**

```bash
git stash && cargo nextest run -p hornvale-language every_rule_kind_is_witnessed 2>&1 | tee /tmp/hv-witness-t8-red.txt
git stash pop
```

**Expected: this may well PASS pre-fix** — a Tonogenesis drawn *after* a merger
fires normally, and the sweep is wide. If it passes, that is not a failure of the
guard; record it plainly and note that this guard covers the *inert-kind* case
(a kind that never fires anywhere) rather than the *inert-position* case Task 7
fixed. Do not weaken the test to manufacture a red.

- [ ] **Step 3: Run on the fixed tree, then commit**

```bash
cargo nextest run -p hornvale-language every_rule_kind_is_witnessed 2>&1 | tee /tmp/hv-witness-t8.txt
cargo fmt && cargo clippy -p hornvale-language --all-targets -- -D warnings
git add domains/language/
git commit -m "test(language): every RuleKind must be witnessed firing, not merely drawn"
```

---

### Task 9: Unblind H1

**Files:**
- Modify: `.superpowers/sdd/baseline-report.md` (the readout section)

- [ ] **Step 1: Re-run Task 1's Step-4 measurement on the repaired tree**

Same command, same tree shape, recorded the same way.

- [ ] **Step 2: Compare against the frozen prediction**

Report the post-repair survival figure against the baseline, and state plainly
whether **H1 is supported or falsified**. Report the cascade statistics too:
leading-Tonogenesis count must now be **zero** by construction, and the freed rule
slots go somewhere — say where.

- [ ] **Step 3: If H1 is falsified, do not retune**

A falsified prediction is a finding, and several campaigns have shipped the null
as the headline. **Do not adjust `WEAR_FLOOR`, the regime bounds, or any constant
to rescue the prediction after unblinding.** If a retune looks warranted, the
argument for it is committed *first*, separately, and labelled as post-unblinding
— that is the standing rule and the memory note behind it is explicit.

- [ ] **Step 4: Commit the readout**

```bash
git add -A && git commit -m "measure(the-witness): H1 readout"
```

---

### Task 10: The documentation half

**Definition of Done for every merged plan includes the project book.**

**Files:**
- Create: `book/src/chronicle/the-witness.md`
- Create: `docs/retrospectives/the-witness.md`
- Create: `docs/decisions/00NN-parity-of-roster-independence-of-predicate.md`
- Modify: `book/src/SUMMARY.md`, `book/src/open-questions.md`,
  `book/src/frontier/idea-registry.md`

- [ ] **Step 1: Check the decision number against `origin/main`, not locally**

Decision numbers collide across parallel sessions. `make preflight`'s GO is a
snapshot, not a lease:

```bash
git fetch origin
git rev-list --count HEAD..origin/main
ls docs/decisions/ | tail -3
git ls-tree origin/main docs/decisions/ --name-only | tail -3
```

Take the next free number **on `origin/main`**, and re-check immediately before
pushing.

- [ ] **Step 2: Write the decision record**

Subject: *parity of roster, independence of predicate*. It is the campaign's
durable finding and generalises past the lab metric — the governing rule for any
deliberately-duplicated second opinion. Cite 0016 (preregistration), and
`windows/lab/src/metrics.rs`'s module note as the argument it refines rather than
overturns. Follow the house form: Status/Decider/Refines header, Context, the
decision, Consequences, See also.

- [ ] **Step 3: Write the chronicle entry**

At the book's deliberate altitude: technical and mathematical, comprehensible
without reading the code. The spine is the shape, not the three bugs — *a check
whose input is authored cannot witness reachability*. Lead with the census
evidence (one distinct value across a thousand seeds) because it is the most
legible instance.

**`docs_consistency.rs` bans registry IDs outside `book/src/frontier/`** — name
the concept ("the name cycle's opacification phase"), never `LANG-11`, in
chronicle prose. This has bitten before.

- [ ] **Step 4: Update the idea registry and the Confidence Gradient**

`book/src/frontier/idea-registry.md` — LANG-11's row gets Task 9's result. **If
H1 was falsified, the row's "the lever is the cascade's match rate" claim is
itself falsified and must be rewritten, not softened.**

`book/src/open-questions.md` — re-score any bet this campaign moved (decision
0030). If the grep finds none in scope, record *that*, as The Wearing did.

- [ ] **Step 5: Write the retrospective and promote the follow-up register**

`docs/retrospectives/the-witness.md`, with the follow-up table as its durable
record (that is what `9a77afc9` established). **Promote `F14`, `F15`, `F16` from
`.superpowers/sdd/followups.md` verbatim** — the scratch dies with the worktree,
and a lost follow-up is the failure this campaign's own subject warns about.

Process lessons to record, at minimum:
- the epoch answer was **reversed after G3** by reading the ratified records
  rather than the approved package;
- three of the campaign's guards were mutation-verified, and one (Task 8) was
  honestly reported as passing pre-fix rather than weakened to manufacture a red.

- [ ] **Step 6: Regenerate, verify the book builds, commit**

```bash
bash scripts/regenerate-artifacts.sh
mdbook build book
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
cargo nextest run -p hornvale --test docs_consistency
git add -A && git commit -m "docs(the-witness): chronicle, retrospective, decision 00NN"
```

---

### Task 11: Close — absorb, gate, regenerate, merge

**The census regen is authorised, runs on lefford, and is the LAST act.**

- [ ] **Step 1: Absorb `main` and re-run preflight**

```bash
git fetch origin
git rev-list --count HEAD..origin/main
make preflight
```

On an ancestry NO-GO, merge `main` **into** the branch and re-run the gate there.
**Read the other branches' chronicles, not just their diffs** — `the-pigment`
also touches `MaterialBuffer` and preflight cannot see a semantic collision.

- [ ] **Step 2: Full gate on the Mac**

```bash
make gate 2>&1 | tee /tmp/hv-witness-gate.txt
```

Not `make ci` — that box must be quiet for the timing alarm to mean anything, and
this one will not be.

- [ ] **Step 3: The checks `make gate` never runs**

Enumerate before trusting green — this is a standing memory note:

```bash
make census-check
bash -c 'shellcheck scripts/*.sh || true'
make vessel-check
make world-check
```

- [ ] **Step 4: Heavy tier on lefford**

```bash
make heavy-remote REF=$(git rev-parse HEAD)   # a full SHA, never a branch name
```

- [ ] **Step 5: Census regen on lefford — after the final absorb, not before**

A mid-run epoch poisons a regen and **nothing reddens**. Absorb first, verify
HEAD, then:

```bash
bash scripts/census-run.sh status          # is a heavy run already holding the box?
bash scripts/census-run.sh
```

Pass a resolved `HV_CENSUS_WORKTREE` and `HV_CENSUS_REF=<full SHA>`, and **verify
the worktree's HEAD before starting** — the regen worktree on lefford is shared;
ask before reusing it.

- [ ] **Step 6: Commit the goldens SEPARATELY and IMMEDIATELY**

```bash
make lab-diff STUDY=the-census        # review what moved
git add book/src/laboratory/generated/
git commit -m "chore(census): regenerate on the post-Witness tree (lefford, 0063/0086)"
```

Then the pins, in their own commit. **A `make census-check` block means the pins
and `golden-pins.sql` must land in ONE commit** — a census re-pin touches four
files, and the column order is `(live, pinned)`.

**Never `git commit --no-verify`** — a hook enforces it.

- [ ] **Step 7: Re-check ancestry immediately before pushing**

```bash
git fetch origin && git rev-list --count HEAD..origin/main
```

Non-zero means someone landed while the regen ran. Absorb and re-gate; do not
push over it.

- [ ] **Step 8: G6 — present the post-G3 ledger digest to Nathan**

Save-format/epoch entries lead it, and the reversal of the spec's flagged item ①
leads those. **Do not merge before this stop.** Then run `closing-a-campaign`.

---

## Self-Review

**Spec coverage.** §2.1 → Tasks 1, 4, 5. §2.2 → Tasks 1, 7. §2.3 → Tasks 1, 2.
§3.1 → Tasks 4, 5. §3.2 → Task 7. §3.3 → Task 2. §4 (keystone, three guards) →
Tasks 3, 6, 8. §5 flagged ① → **superseded** by the epoch section above, and the
supersession is itself a Task-10 retrospective item. §5 ② → unchanged; F5 bumps
nothing. §5 ③ → Task 11 Step 1. §5 ④ → Task 11 Steps 5–6. §6 → Tasks 1 and 9.
§7 → the task order. §8 non-goals → F14/F15/F16 carried to Task 10 Step 5.

**Placeholders.** Three deliberate holes, each with the command that fills it and
a criterion for accepting the answer: `GRAIN_POROSITY_GAIN` (Task 4 Step 3), the
decision number (Task 10 Step 1), and the concept lists in Tasks 2–3 (derived in
Task 2 Step 1). None is a "TBD" — each is a measurement whose value must not be
guessed by the plan.

**Type consistency.** `Hydro::ALL` and `RuleKind::ALL` are declared in Tasks 6
and 8 and used only there. `steepable_concept_roster()` is declared in Task 3
Step 3 and consumed in Task 3 Step 1. `draw_rule(stream, seen_merger)` gains its
parameter in Task 7 Step 3 and both call sites are updated in the same step.
`STAPLE_CONCEPTS` is declared in Task 2 Step 2.

**Known risk the plan does not remove.** Task 8's guard may pass pre-fix. That is
stated in the task rather than hidden, and the honest report is the deliverable —
weakening the test to manufacture a red would be the exact defect this campaign
exists to close.

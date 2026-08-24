# The Gossan Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split `MetabolicClass` into `ThermalStrategy` and `TrophicMode` so a
chemotroph is expressible, changing no number in any world.

**Architecture:** The mapping is bijective, so the campaign is one axis renamed
1:1 plus a second axis nothing reads. Two goldens are captured on `main`'s
behaviour and land GREEN before the type is touched; the split then lands as a
change to ~100 compiler-found sites that moves zero golden bytes.

**Tech Stack:** Rust edition 2024, `cargo nextest`, `hornvale_kernel::golden`'s
`REBASELINE=1` fixture idiom, `scripts/mutate.py` for the positive controls.

**Spec:** `docs/superpowers/specs/2026-08-24-the-gossan-design.md`
**Program:** `docs/superpowers/specs/2026-08-24-the-underworld-larder-metaplan.md`

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` only. No new crates.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only.
- Every crate sets `#![warn(missing_docs)]`; every public item, field and
  variant gets a one-line doc comment.
- `cargo fmt` is the final step before every commit.
- `make gate-commit` must pass before every commit.
- **No number in any world may move.** If a golden drifts, stop — that is a
  finding, not a rebaseline.

## Two corrections to the spec, found while gathering signatures

**C-1. The spec's `None` variant name is a footgun; use `Absent`.**
Spec §4.1 names the fourth value on both axes `None`. The existing test
`rise_at_couples_heat_to_thirst_per_metabolic_class`
(`windows/vessel/src/liveness.rs:8261`) opens with `use MetabolicClass::*;`,
and a glob-imported `None` variant collides with `Option::None` in that scope.
**Both axes use `Absent` instead.** Task 3 updates the spec's §4.1/§4.2 tables
in the same commit that introduces the types, so the spec and the code never
disagree.

**C-2. Instrument 2 already exists and needs extending, not building.**
Spec §5.2 describes "THE DRIVE PIN" as something this campaign creates.
`rise_at_couples_heat_to_thirst_per_metabolic_class` already asserts `rise_at`
for `Endotherm`, `Ectotherm` and `Autotroph`, and is **already in
`docs/timings/subfloor-roster.tsv`**, so it already runs in `gate-commit`. It
does **not** cover `Ametabolic`. Task 2 adds that one case; Task 3 updates the
spec.

## A hazard the executor must know about

**A new test is NOT in `gate-commit`.** `docs/timings/subfloor-roster.tsv`
selects by exact test name, and a test with no recorded baseline duration is
excluded from the commit gate by design. The golden this plan creates in Task 1
will therefore **not** run under `make gate-commit` until a green stage gate
rewrites the roster. Every task below runs its own tests explicitly with
`cargo nextest run -p <crate> ...`; do not infer from a green `gate-commit`
that the golden ran.

## File Structure

```
CREATE  domains/species/tests/suite/life_history_golden.rs     instrument 1
CREATE  domains/species/tests/fixtures/life-history-all-kinds.txt   its fixture
CREATE  domains/species/tests/suite/metabolic_pairs.rs         the §4.4 guard

MODIFY  domains/species/tests/suite.rs            register the two new modules
MODIFY  domains/species/src/lib.rs                the types, the field, the table
MODIFY  domains/species/src/allometry.rs          3 readers (2 matches + 1 eq)
MODIFY  domains/species/tests/suite/coverage.rs   the witnesses table splits
MODIFY  windows/vessel/src/liveness.rs            rise_at + 3 matches! + fixtures
MODIFY  windows/sentiment/src/lib.rs              PeopleTraits carries both axes
MODIFY  windows/sentiment/src/axes.rs             1 read site
MODIFY  windows/lab/src/synthetic.rs              2 construction sites
MODIFY  windows/almanac/src/lib.rs                1 read site
MODIFY  windows/worldgen/tests/suite/underworld_separation.rs   2 sites
MODIFY  windows/lab/tests/suite/hearth_population_calibration.rs  1 site
MODIFY  docs/superpowers/specs/2026-08-24-the-gossan-design.md    C-1 and C-2
```

---

### Task 1: The life-history golden (instrument 1)

Captures `main`'s behaviour for every kind before anything changes. This task
must land GREEN with no production code modified.

**Files:**
- Create: `domains/species/tests/suite/life_history_golden.rs`
- Create: `domains/species/tests/fixtures/life-history-all-kinds.txt` (via `REBASELINE=1`)
- Modify: `domains/species/tests/suite.rs`

**Interfaces:**
- Consumes: `hornvale_species::biosphere_registry()`, `hornvale_species::life_history(mass, class, schedule) -> LifeHistory`, `hornvale_kernel::golden::assert_golden`, `hornvale_kernel::quantize::quantize`
- Produces: the fixture file that Tasks 4-7 assert against unchanged.

- [ ] **Step 1: Write the golden test**

Create `domains/species/tests/suite/life_history_golden.rs`:

```rust
//! Every kind's full life-history profile, frozen.
//!
//! THE GOSSAN's instrument 1 (spec §5.2). This exists because the project's
//! own drift check is VACUOUS for a change to `MetabolicClass`: committed
//! artifacts carry life-history numbers for `goblin` and `kobold` only, both
//! under `book/src/laboratory/generated/`, and those regenerate only under
//! the census flag — so `make rebaseline` never touches them and a local
//! `git diff --exit-code` comes back clean whether the mapping is right or
//! wrong (spec §5.1).
//!
//! # WHAT THIS CANNOT SEE
//!
//! `rise_at` (`windows/vessel/src/liveness.rs`) produces no species-level
//! life-history quantity, so a thermal mis-mapping that only changes thirst
//! is INVISIBLE here. That is instrument 2's job
//! (`rise_at_couples_heat_to_thirst_per_metabolic_class`), and control C1 in
//! spec §5.3 exists to prove this blind spot is real rather than assumed.
//!
//! Regenerate deliberately: `REBASELINE=1 cargo test -p hornvale-species
//! --test suite -- life_history_golden`, then read the diff as a change to
//! every creature in every world.

use hornvale_kernel::quantize::quantize;
use hornvale_species::{biosphere_registry, life_history};

/// One line per kind, tab-separated, quantized at the emit boundary exactly
/// as every other committed float in this project is (decision 0033) — a
/// golden file IS an emit boundary, and an unquantized one would differ in
/// the last ULP between platforms.
fn render() -> String {
    let mut out =
        String::from("kind\tbmr_w\tlifespan_y\tmaturity_y\ttempo\tgeneration_y\tpace\n");
    for (kind, bio) in biosphere_registry().iter() {
        let lh = life_history(bio.mass, bio.metabolic_class, bio.schedule);
        let opt = |v: Option<f64>| match v {
            Some(x) => format!("{}", quantize(x)),
            None => "-".to_string(),
        };
        out.push_str(&format!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\n",
            kind.0,
            quantize(lh.basal_metabolic_rate_w),
            opt(lh.lifespan.map(|y| y.get())),
            opt(lh.age_at_maturity.map(|y| y.get())),
            opt(lh.reproductive_tempo),
            opt(lh.generation_length.map(|y| y.get())),
            quantize(lh.pace_of_life),
        ));
    }
    out
}

#[test]
fn every_kinds_life_history_is_frozen() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/life-history-all-kinds.txt"
        )),
        &render(),
        "a kind's life-history profile moved. THE GOSSAN claims to change no \
         number in any world, so during that campaign this drifting is a STOP, \
         not a rebaseline: re-read the bijection in spec §4.2 before touching \
         this fixture. Outside that campaign, a move here is a calibration \
         migration — accept it with REBASELINE=1 and review the diff.",
    );
}

/// The fixture is only a guard if it has rows and if the `Option` columns are
/// not all one value. A registry that yielded nothing, or a `life_history`
/// that returned `None` everywhere, would freeze a table of dashes and the
/// golden would be silently vacuous.
#[test]
fn the_life_history_table_is_not_vacuous() {
    let rendered = render();
    let rows: Vec<&str> = rendered.lines().skip(1).collect();
    assert!(
        rows.len() >= 20,
        "only {} kinds rendered — the registry is not being read",
        rows.len()
    );
    assert!(
        rows.iter().any(|r| r.contains("\t-\t")),
        "no row has an absent life-history column — the Ametabolic branch of \
         `life_history` is not represented, so this fixture cannot witness it"
    );
    assert!(
        rows.iter().any(|r| !r.contains("\t-\t")),
        "every row has an absent column — `life_history` is returning None for \
         everything and the table is a page of dashes"
    );
}
```

- [ ] **Step 2: Register the module**

In `domains/species/tests/suite.rs`, add in alphabetical position (after the
`instance_lens` block):

```rust
#[path = "suite/life_history_golden.rs"]
mod life_history_golden;
```

- [ ] **Step 3: Run to verify it fails on the missing fixture**

Run: `cargo nextest run -p hornvale-species -E 'test(life_history_golden)'`
Expected: `every_kinds_life_history_is_frozen` FAILS (no fixture file);
`the_life_history_table_is_not_vacuous` PASSES.

If the vacuity test also fails, STOP — the registry read is wrong and the
fixture would freeze nonsense.

- [ ] **Step 4: Capture the fixture**

Run: `REBASELINE=1 cargo test -p hornvale-species --test suite -- life_history_golden`

- [ ] **Step 5: Read the fixture before trusting it**

Run: `head -5 domains/species/tests/fixtures/life-history-all-kinds.txt && wc -l domains/species/tests/fixtures/life-history-all-kinds.txt`

Confirm by eye: one header line plus one line per kind, numbers that look like
years and watts rather than zeros or `NaN`, and at least one row with `-`
columns (that is `xorn`, the `Ametabolic` kind).

- [ ] **Step 6: Run again to verify it passes**

Run: `cargo nextest run -p hornvale-species -E 'test(life_history_golden)'`
Expected: both tests PASS.

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt && make gate-commit
git add domains/species/tests/suite/life_history_golden.rs \
        domains/species/tests/fixtures/life-history-all-kinds.txt \
        domains/species/tests/suite.rs docs/timings.md
git commit -m "test(the-gossan): freeze every kind's life-history profile

Instrument 1 of two, captured on main's behaviour before the type moves. The
project's own drift check is vacuous for this change: committed artifacts
carry life-history numbers for goblin and kobold only, and those regenerate
only under the census flag, so a local drift check comes back clean whether
the mapping is right or wrong.

Quantized at the emit boundary like every other committed float — a golden
file is an emit boundary. Carries its own non-vacuity guard: a registry that
yielded nothing, or a life_history returning None everywhere, would freeze a
table of dashes and read as a healthy fixture."
```

---

### Task 2: Extend the drive pin (instrument 2)

**Files:**
- Modify: `windows/vessel/src/liveness.rs:8261-8297` (the existing test)

**Interfaces:**
- Consumes: `rise_at(temp: f64, class: MetabolicClass, p: &DriveParams) -> f64`, `SUSTENANCE`
- Produces: an assertion that `Ametabolic` couples flat, which Task 4 renames to `ThermalStrategy::Absent`.

- [ ] **Step 1: Add the missing case**

In `windows/vessel/src/liveness.rs`, in
`rise_at_couples_heat_to_thirst_per_metabolic_class`, replace:

```rust
        // Autotroph flat; an unreadable cell couples as neutral.
        assert!((rise_at(80.0, Autotroph, &p) - base).abs() < 1e-12);
        assert!((rise_at(f64::INFINITY, Endotherm, &p) - base).abs() < 1e-12);
    }
```

with:

```rust
        // Autotroph flat; an unreadable cell couples as neutral.
        assert!((rise_at(80.0, Autotroph, &p) - base).abs() < 1e-12);
        assert!((rise_at(f64::INFINITY, Endotherm, &p) - base).abs() < 1e-12);
        // Ametabolic flat, in BOTH directions. `rise_at` never reaches here in
        // production (a construct has no thirst drive) and the arm is kept
        // total; asserting it anyway is what makes this test an instrument for
        // THE GOSSAN, which needs every thermal branch pinned before the type
        // splits. Autotroph and Ametabolic share this arm today, and that is
        // exactly the grouping `basal_metabolic_rate_w` does NOT use — the
        // disagreement `ThermalStrategy::Unmodelled` exists to express.
        assert!((rise_at(80.0, Ametabolic, &p) - base).abs() < 1e-12);
        assert!((rise_at(-100.0, Ametabolic, &p) - base).abs() < 1e-12);
    }
```

- [ ] **Step 2: Run to verify it passes**

Run: `cargo nextest run -p hornvale-vessel -E 'test(rise_at_couples_heat_to_thirst_per_metabolic_class)'`
Expected: PASS. (This asserts existing behaviour, so a red here means
`rise_at`'s `Autotroph | Ametabolic => base` arm is not what the source says.)

- [ ] **Step 3: Prove the new assertions can fail**

Run:
```bash
python3 scripts/mutate.py windows/vessel/src/liveness.rs \
  "        MetabolicClass::Autotroph | MetabolicClass::Ametabolic => base," \
  "        MetabolicClass::Autotroph => base,
        MetabolicClass::Ametabolic => base * 2.0,"
cargo nextest run -p hornvale-vessel -E 'test(rise_at_couples_heat_to_thirst_per_metabolic_class)'
git checkout -- windows/vessel/src/liveness.rs
```
Expected: RED, then the tree restored. Record the failure message in the
commit. If it stays green, the new assertions are not reaching `rise_at` and
the task is not done.

- [ ] **Step 4: Gate and commit**

```bash
cargo fmt && make gate-commit
git add windows/vessel/src/liveness.rs docs/timings.md
git commit -m "test(the-gossan): pin rise_at's Ametabolic branch, the one thermal case nobody asserted

Instrument 2 of two. The spec described this as a pin to be BUILT; it already
exists and is already in the subfloor roster, so it already runs every commit.
It covered Endotherm, Ectotherm and Autotroph and not Ametabolic.

Mutation control, run and reverted: splitting the shared arm and doubling the
Ametabolic rate reddens it. Without that, a green here would only have meant
the assertion never reached the function."
```

---

### Task 3: The two types and the bijection (additive — nothing breaks)

**Files:**
- Modify: `domains/species/src/lib.rs` (the `MetabolicClass` block, around line 2260)
- Modify: `docs/superpowers/specs/2026-08-24-the-gossan-design.md` (C-1, C-2)

**Interfaces:**
- Produces: `ThermalStrategy`, `TrophicMode`, `MetabolicClass::split(self) -> (ThermalStrategy, TrophicMode)`. Task 4 consumes all three.

- [ ] **Step 1: Write the failing bijection test**

Add at the end of `domains/species/src/lib.rs`'s existing `#[cfg(test)] mod tests`
block (if the crate has none at that location, create
`#[cfg(test)] mod gossan_split_tests { ... }` immediately after the
`TrophicMode` definition):

```rust
#[cfg(test)]
mod gossan_split_tests {
    use super::{MetabolicClass, ThermalStrategy, TrophicMode};

    /// The four old variants map onto four distinct pairs, and the map is
    /// INJECTIVE. If two old variants collapsed to one pair, the split would
    /// silently merge two behaviours — which is exactly the failure
    /// `ThermalStrategy::Unmodelled` was added to prevent.
    #[test]
    fn the_split_is_injective_over_every_old_variant() {
        let all = [
            MetabolicClass::Endotherm,
            MetabolicClass::Ectotherm,
            MetabolicClass::Autotroph,
            MetabolicClass::Ametabolic,
        ];
        let mut seen: Vec<(ThermalStrategy, TrophicMode)> = Vec::new();
        for c in all {
            let pair = c.split();
            assert!(
                !seen.contains(&pair),
                "{c:?} maps to {pair:?}, which another variant already claims — \
                 the split is not injective and two behaviours have merged"
            );
            seen.push(pair);
        }
        assert_eq!(seen.len(), 4);
    }

    /// The specific mapping of spec §4.2, asserted rather than merely
    /// implemented. `Autotroph -> Unmodelled` is the load-bearing row: it is
    /// NOT `Endothermic`, because `rise_at` groups `Autotroph` with
    /// `Ametabolic` while `basal_metabolic_rate_w` groups it with `Endotherm`,
    /// so no single existing value preserves both.
    #[test]
    fn the_split_matches_the_specs_table() {
        assert_eq!(
            MetabolicClass::Endotherm.split(),
            (ThermalStrategy::Endothermic, TrophicMode::Heterotrophic)
        );
        assert_eq!(
            MetabolicClass::Ectotherm.split(),
            (ThermalStrategy::Ectothermic, TrophicMode::Heterotrophic)
        );
        assert_eq!(
            MetabolicClass::Autotroph.split(),
            (ThermalStrategy::Unmodelled, TrophicMode::Phototrophic)
        );
        assert_eq!(
            MetabolicClass::Ametabolic.split(),
            (ThermalStrategy::Absent, TrophicMode::Absent)
        );
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo nextest run -p hornvale-species -E 'test(gossan_split_tests)'`
Expected: FAIL to compile — `ThermalStrategy` and `TrophicMode` do not exist.

- [ ] **Step 3: Add the two types and the bijection**

In `domains/species/src/lib.rs`, immediately after the closing brace of
`pub enum MetabolicClass { ... }`:

```rust
/// How a species regulates body temperature — the **demand** axis, and the
/// only one allometry reads.
///
/// Split out of [`MetabolicClass`] by THE GOSSAN. That enum conflated this
/// with the supply axis ([`TrophicMode`]): its own doc says its job is to
/// select B₀ and the pace multiplier, and `Autotroph` — a supply value —
/// ended up grouped with `Endotherm` in `basal_metabolic_rate_w` because
/// allometry had nothing else to do with it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ThermalStrategy {
    /// Warm-blooded (mammal/bird analogue): high, temperature-stable basal rate.
    Endothermic,
    /// Cold-blooded (reptile/amphibian analogue): ~1/8 the basal rate; longer
    /// life per kg. Realized rate couples to ambient temperature.
    Ectothermic,
    /// Has a metabolism; its thermal behaviour is **not modelled**.
    ///
    /// Not a placeholder — it names a distinction shipped code already made
    /// and had no word for. `basal_metabolic_rate_w` groups the old
    /// `Autotroph` with `Endotherm`; `rise_at` groups it with `Ametabolic`.
    /// No single existing value preserves both, so the honest answer is a
    /// value that says the modelling call was never made. That call is
    /// tracked as BIO-autotroph-physics and is deliberately not this
    /// campaign's.
    Unmodelled,
    /// No metabolism at all (construct/undead analogue): no life-history.
    ///
    /// Named `Absent` rather than `None` because
    /// `rise_at_couples_heat_to_thirst_per_metabolic_class` glob-imports this
    /// enum's variants, where a `None` would collide with `Option::None`.
    Absent,
}

/// Where a species gets its energy — the **supply** axis.
///
/// Split out of [`MetabolicClass`] by THE GOSSAN, and **nothing reads it
/// yet**: making a chemotroph expressible is the whole of that campaign, and
/// giving this axis a consumer is rung 2 of the Underworld Larder.
///
/// An axis nobody reads is how [`MetabolicClass`] rotted, so the guard in
/// `tests/suite/metabolic_pairs.rs` is a genuine reader of every kind's value
/// on every commit-gate run, not merely a widening check.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TrophicMode {
    /// Eats other organisms — prey, detritus, or their remains.
    Heterotrophic,
    /// Energy from light (plant-folk/fungal analogue).
    Phototrophic,
    /// Energy from chemical gradients in rock or water — a hydrothermal vent
    /// community, and the underworld's only possible productive base.
    ///
    /// **Declared, not witnessed:** no kind carries it, and
    /// `tests/suite/metabolic_pairs.rs` asserts exactly that. Rung 2's
    /// success condition is that this assertion has to change.
    Chemotrophic,
    /// No metabolism at all. See [`ThermalStrategy::Absent`] for the naming.
    Absent,
}

impl MetabolicClass {
    /// This class as the two axes it conflates (spec §4.2). The map is
    /// injective, which is what makes THE GOSSAN's byte-identity structural
    /// rather than argued.
    pub fn split(self) -> (ThermalStrategy, TrophicMode) {
        match self {
            MetabolicClass::Endotherm => {
                (ThermalStrategy::Endothermic, TrophicMode::Heterotrophic)
            }
            MetabolicClass::Ectotherm => {
                (ThermalStrategy::Ectothermic, TrophicMode::Heterotrophic)
            }
            MetabolicClass::Autotroph => {
                (ThermalStrategy::Unmodelled, TrophicMode::Phototrophic)
            }
            MetabolicClass::Ametabolic => (ThermalStrategy::Absent, TrophicMode::Absent),
        }
    }
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo nextest run -p hornvale-species -E 'test(gossan_split_tests)'`
Expected: both tests PASS.

- [ ] **Step 5: Correct the enum's stale autotroph count**

In `domains/species/src/lib.rs`, in `MetabolicClass::Autotroph`'s doc, replace:

```
    /// this class `B0_ENDOTHERM` and a pace multiplier of 1.0, so the two
    /// shipped autotrophs (treant, twig-blight) are computed exactly as
    /// endotherms of the same mass. The class was witnessed by The Menagerie
```

with:

```
    /// this class `B0_ENDOTHERM` and a pace multiplier of 1.0, so the three
    /// shipped autotrophs (treant, twig-blight, shrieker) are computed
    /// exactly as endotherms of the same mass. (It said TWO until THE GOSSAN
    /// counted them: `shrieker` was added later and this line was not
    /// updated. `shrieker` is also a fungus and so not a phototroph at all —
    /// a corpus error left standing on purpose, because a data fix inside a
    /// structural rename hides both.) The class was witnessed by The Menagerie
```

- [ ] **Step 6: Correct the spec (C-1 and C-2)**

In `docs/superpowers/specs/2026-08-24-the-gossan-design.md`:

1. In §4.1's table and §4.2's mapping, replace the fourth value `None` with
   `Absent` on both axes.
2. Immediately after §4.1's table, add:

```
**`Absent`, not `None`.** The plan found that
`rise_at_couples_heat_to_thirst_per_metabolic_class` glob-imports the enum's
variants (`use MetabolicClass::*;`), where a `None` variant collides with
`Option::None`. The name changed before any code was written.
```

3. In §5.2, replace `2. THE DRIVE PIN        rise_at across the four thermal values`
   with:

```
2. THE DRIVE PIN        rise_at across the four thermal values
                        ALREADY EXISTS as `rise_at_couples_heat_to_thirst_
                        per_metabolic_class`, and is already in the subfloor
                        roster, so it already runs every commit. It covered
                        Endotherm/Ectotherm/Autotroph and NOT Ametabolic;
                        the campaign extends it rather than building it.
```

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt && make gate-commit
git add domains/species/src/lib.rs \
        docs/superpowers/specs/2026-08-24-the-gossan-design.md docs/timings.md
git commit -m "feat(the-gossan): ThermalStrategy and TrophicMode, plus the bijection

Additive: MetabolicClass is untouched and every reader still compiles. The
split is asserted two ways — injective over all four old variants (so no two
behaviours can silently merge) and equal to the spec's specific table.

Absent, not None: rise_at's own test glob-imports the variants, where a None
would collide with Option::None. Spec §4.1 updated in this commit rather than
left to disagree.

Also corrects the enum's own doc, which said two shipped autotrophs where
three ship. shrieker was added later and is a fungus, so it is not a
phototroph either — left standing deliberately, with the finding recorded."
```

---

### Task 4: The field swap and the ~100 sites

The only non-mechanical judgement here is already made (Task 3's bijection).
Everything else the compiler enumerates.

**Files:**
- Modify: `domains/species/src/lib.rs`, `domains/species/src/allometry.rs`, `domains/species/tests/suite/coverage.rs`, `windows/vessel/src/liveness.rs`, `windows/sentiment/src/lib.rs`, `windows/sentiment/src/axes.rs`, `windows/lab/src/synthetic.rs`, `windows/almanac/src/lib.rs`, `windows/worldgen/tests/suite/underworld_separation.rs`, `windows/lab/tests/suite/hearth_population_calibration.rs`

**Interfaces:**
- Consumes: `ThermalStrategy`, `TrophicMode`, `MetabolicClass::split` from Task 3.
- Produces: `BiosphereTraits { thermal_strategy: ThermalStrategy, trophic_mode: TrophicMode, .. }` and `PeopleTraits` carrying both. Tasks 5-6 consume these.

- [ ] **Step 1: Swap the field on `BiosphereTraits`**

In `domains/species/src/lib.rs`, replace:

```rust
    /// Metabolic strategy — drives life-history allometry (spec BIO-2).
    pub metabolic_class: MetabolicClass,
```

with:

```rust
    /// How this species regulates body temperature — the axis life-history
    /// allometry reads (spec BIO-2).
    pub thermal_strategy: ThermalStrategy,
    /// Where this species gets its energy. **Nothing reads this yet** (THE
    /// GOSSAN); `tests/suite/metabolic_pairs.rs` is its only consumer and
    /// exists so the axis cannot rot the way `MetabolicClass` did.
    pub trophic_mode: TrophicMode,
```

- [ ] **Step 2: Rewrite every construction site mechanically**

Run:

```bash
python3 - <<'PY'
import subprocess, re
files = [f for f in subprocess.run(['git','grep','-l','metabolic_class'],
         capture_output=True, text=True).stdout.split() if f.endswith('.rs')]
total = 0
for f in files:
    s = open(f).read()
    new, n = re.subn(
        r'metabolic_class: (MetabolicClass::(\w+))\s*,',
        lambda m: (f'thermal_strategy: {m.group(1)}.split().0,\n'
                   f'trophic_mode: {m.group(1)}.split().1,'),
        s)
    if n:
        open(f, 'w').write(new)
        total += n
        print(f'{f}: {n}')
print('TOTAL:', total)
PY
```

This deliberately routes every site through `split()` rather than writing the
pair by hand, so **no construction site can disagree with the bijection Task 3
asserted**. It is a scaffold, not the final form: Step 7 folds every one of
them to a literal before `MetabolicClass` is deleted, and deleting the type
first would break all ~81 of them at once.

- [ ] **Step 3: Compile and let the compiler list the rest**

Run: `cargo check --workspace --all-targets 2>&1 | grep -E '^error|-->' | head -40`

Work the list until it is empty. The remaining errors are read sites
(`.metabolic_class`) and `PeopleTraits`. For each:

- `windows/sentiment/src/lib.rs:66` — replace `pub metabolic_class: MetabolicClass,`
  with the same two fields, and at `:135` replace
  `metabolic_class: bio.metabolic_class,` with
  `thermal_strategy: bio.thermal_strategy,` and `trophic_mode: bio.trophic_mode,`.
- `windows/sentiment/src/axes.rs:220` — `a.metabolic_class` becomes `a.thermal_strategy`.
- `windows/almanac/src/lib.rs:301` and any other `.metabolic_class` read —
  becomes `.thermal_strategy` (every read site feeds allometry).

- [ ] **Step 4: Repoint the three exhaustive matches**

`domains/species/src/allometry.rs`, `pace_multiplier` — change the signature to
`fn pace_multiplier(class: ThermalStrategy) -> f64` and the arms to:

```rust
    match class {
        ThermalStrategy::Endothermic => 1.0,
        ThermalStrategy::Ectothermic => MAX_PACE_MULTIPLIER,
        // `Unmodelled` takes the endotherm's 1.0 because the old `Autotroph`
        // did — this is the rename, not a modelling decision.
        ThermalStrategy::Unmodelled => 1.0,
        // `life_history` nulls the biological traits for `Absent` before any
        // of the four time laws are called; the bare `lifespan` (etc.) called
        // directly on an ametabolic mass returns a number that means nothing.
        ThermalStrategy::Absent => 1.0,
    }
```

`basal_metabolic_rate_w` — signature to `(mass: Mass, class: ThermalStrategy)`,
arms to:

```rust
    let b0 = match class {
        // `Unmodelled` groups with `Endothermic` here and with `Absent` in
        // `rise_at`. That disagreement is why the value exists.
        ThermalStrategy::Endothermic | ThermalStrategy::Unmodelled => B0_ENDOTHERM,
        ThermalStrategy::Ectothermic => B0_ENDOTHERM * ECTOTHERM_METABOLIC_FRACTION,
        ThermalStrategy::Absent => return 0.0,
    };
```

`windows/vessel/src/liveness.rs`, `rise_at` — signature to
`(temp: f64, class: ThermalStrategy, p: &DriveParams)`, and:

```rust
        ThermalStrategy::Endothermic => { /* body unchanged */ }
        ThermalStrategy::Ectothermic => { /* body unchanged */ }
        // Unmodelled: a deferred seam (transpiration is its own later work).
        // Absent: never reaches here (no thirst drive); arm kept total.
        ThermalStrategy::Unmodelled | ThermalStrategy::Absent => base,
```

- [ ] **Step 5: Repoint the remaining reads mechanically**

`allometry.rs:141` becomes `if class == ThermalStrategy::Absent {`, and the
three `matches!(.., MetabolicClass::Ametabolic)` in `liveness.rs` become
`matches!(.., ThermalStrategy::Absent)`. Task 5 consolidates them; this step
only makes them compile.

`life_history`'s signature becomes
`pub fn life_history(mass: Mass, class: ThermalStrategy, schedule: LifeSchedule) -> LifeHistory`,
and `reproductive_tempo`'s and `pace_of_life`'s likewise. Every caller passes
`bio.thermal_strategy` in place of `bio.metabolic_class`.

- [ ] **Step 6: Split `coverage.rs`'s witnesses table**

In `domains/species/tests/suite/coverage.rs`, change `metabolic_witnesses` to
read the new axis:

```rust
fn thermal_witnesses(strategy: ThermalStrategy) -> Vec<&'static str> {
    biosphere_registry()
        .iter()
        .filter(|(_, b)| b.thermal_strategy == strategy)
        .map(|(k, _)| k.0)
        .collect()
}
```

and update the `expected` table's first column from `MetabolicClass::X` to the
corresponding `ThermalStrategy` value, leaving every witness list byte-identical
(`Autotroph`'s row becomes `ThermalStrategy::Unmodelled` with
`&["shrieker", "treant", "twig-blight"]` unchanged).

`autotroph_is_computed_as_an_endotherm_today` keeps its name and its meaning —
update its body to construct `ThermalStrategy::Unmodelled` and add one line to
its comment: `THE GOSSAN renamed the value; the divergence it pins is
unchanged.`

- [ ] **Step 7: Fold the scaffold, THEN delete `MetabolicClass`**

Step 2 left ~81 sites calling `MetabolicClass::X.split().0`. Deleting the enum
with those in place breaks every one of them, so fold them to literals first:

```bash
python3 - <<'FOLD'
import subprocess, re
PAIR = {
    'Endotherm':  ('Endothermic', 'Heterotrophic'),
    'Ectotherm':  ('Ectothermic', 'Heterotrophic'),
    'Autotroph':  ('Unmodelled',  'Phototrophic'),
    'Ametabolic': ('Absent',      'Absent'),
}
files = [f for f in subprocess.run(['git','grep','-l','MetabolicClass::'],
         capture_output=True, text=True).stdout.split() if f.endswith('.rs')]
total = 0
for f in files:
    s = open(f).read()
    def fold(m):
        t, r = PAIR[m.group(1)]
        return f'ThermalStrategy::{t}' if m.group(2) == '0' else f'TrophicMode::{r}'
    new, n = re.subn(r'MetabolicClass::(\w+)\.split\(\)\.([01])', fold, s)
    if n:
        open(f, 'w').write(new)
        total += n
        print(f'{f}: {n}')
print('TOTAL folded:', total)
FOLD
```

Then run `cargo check --workspace --all-targets` and confirm it is green
**before** removing anything — a green check here means every scaffold site
folded and none was missed.

Now remove `pub enum MetabolicClass` and `impl MetabolicClass`. Task 3's
`gossan_split_tests` reference the deleted type, so they go with it; Task 6's
`SANCTIONED` table carries the mapping from here on. **The split test's job is
finished once no caller can construct the old type** — it existed to make the
mechanical rewrite safe, and the rewrite is done.

Confirm nothing survives in code:

```bash
git grep -n 'MetabolicClass' -- '*.rs' || echo "no references remain"
```

A surviving mention inside a doc comment is fine and expected — several docs
explain what the type used to be, and Task 3 deliberately wrote some of them.
A surviving mention in code is not.

- [ ] **Step 8: THE PROOF — run both instruments**

Run:
```bash
cargo nextest run -p hornvale-species -E 'test(life_history_golden)'
cargo nextest run -p hornvale-vessel -E 'test(rise_at_couples_heat_to_thirst_per_metabolic_class)'
cargo nextest run --workspace
git diff --stat -- domains/species/tests/fixtures/life-history-all-kinds.txt
```

Expected: both instruments PASS, the workspace PASSES, and the fixture diff is
**empty**. A drifted fixture here is a STOP, not a rebaseline — it means the
bijection is wrong, and the fix is in the mapping, never in the golden.

- [ ] **Step 9: Gate and commit**

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "feat(the-gossan): split metabolic_class into thermal_strategy and trophic_mode

~100 sites, every one compiler-found. Both instruments green and the
life-history fixture byte-identical, which is the campaign's whole claim.

Every construction site was rewritten through MetabolicClass::split() rather
than by hand, so no site could disagree with the bijection Task 3 asserted.

Unmodelled groups with Endothermic in basal_metabolic_rate_w and with Absent
in rise_at. That disagreement is not a wart — it is the reason the value
exists, and it is what a naive Autotroph -> Endothermic mapping would have
silently destroyed."
```

---

### Task 5: `is_ametabolic` consolidates four spellings

**Files:**
- Modify: `domains/species/src/lib.rs` (add the predicate)
- Modify: `domains/species/src/allometry.rs:141`, `windows/vessel/src/liveness.rs` (3 sites)

**Interfaces:**
- Consumes: `ThermalStrategy`, `TrophicMode`.
- Produces: `pub fn is_ametabolic(thermal: ThermalStrategy, trophic: TrophicMode) -> bool`.

- [ ] **Step 1: Write the failing test**

In `domains/species/src/lib.rs`'s test module:

```rust
    /// The predicate takes BOTH axes deliberately. `thermal == Absent` alone
    /// would be correct only while the corpus contains no unsanctioned pair,
    /// and the type admits twelve of them (spec §4.4) — relying on the
    /// invariant here would make this the first place a bad pair goes wrong.
    #[test]
    fn is_ametabolic_requires_both_axes_absent() {
        use super::{ThermalStrategy as T, TrophicMode as M, is_ametabolic};
        assert!(is_ametabolic(T::Absent, M::Absent));
        assert!(!is_ametabolic(T::Absent, M::Heterotrophic));
        assert!(!is_ametabolic(T::Endothermic, M::Absent));
        assert!(!is_ametabolic(T::Unmodelled, M::Phototrophic));
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo nextest run -p hornvale-species -E 'test(is_ametabolic_requires_both_axes_absent)'`
Expected: FAIL to compile — `is_ametabolic` is not defined.

- [ ] **Step 3: Add the predicate**

```rust
/// Whether these two axes describe something with **no metabolism at all** —
/// the question four separate sites used to ask in four spellings, in two
/// crates, with no shared name (spec §4.3). That is the same drift that
/// produced the mixed enum this campaign split.
pub fn is_ametabolic(thermal: ThermalStrategy, trophic: TrophicMode) -> bool {
    thermal == ThermalStrategy::Absent && trophic == TrophicMode::Absent
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo nextest run -p hornvale-species -E 'test(is_ametabolic_requires_both_axes_absent)'`
Expected: PASS.

- [ ] **Step 5: Repoint the four sites**

- `domains/species/src/allometry.rs:141` — `life_history` already has both axes
  only if its signature carries them. It takes `class: ThermalStrategy` alone,
  so **leave this site as `class == ThermalStrategy::Absent` and add a comment
  saying why**: widening `life_history`'s signature to take the trophic axis
  would give it a parameter it never reads, which is the opposite of this
  campaign's point. The predicate is for callers that hold a whole
  `BiosphereTraits`.
- `windows/vessel/src/liveness.rs:326`, `:3830`, `:4385` — each holds an NPC or
  its traits, so each becomes
  `hornvale_species::is_ametabolic(npc.thermal_strategy, npc.trophic_mode)`.

- [ ] **Step 6: Run the affected suites**

Run: `cargo nextest run -p hornvale-species -p hornvale-vessel`
Expected: PASS, including the Task 1 golden and the Task 2 drive pin.

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "refactor(the-gossan): one predicate for the question four sites spelled four ways

allometry keeps its own comparison and does NOT adopt the predicate, because
life_history takes only the thermal axis and widening its signature to take a
parameter it never reads is the opposite of this campaign's point. The comment
at that site now says so. The three liveness sites, which hold whole traits,
adopt it."
```

---

### Task 6: The pair table — the guard that is also the reader

**Files:**
- Create: `domains/species/tests/suite/metabolic_pairs.rs`
- Modify: `domains/species/tests/suite.rs`

**Interfaces:**
- Consumes: `biosphere_registry()`, `ThermalStrategy`, `TrophicMode`.

- [ ] **Step 1: Write the test**

Create `domains/species/tests/suite/metabolic_pairs.rs`:

```rust
//! Which (thermal, trophic) pairs the corpus may contain.
//!
//! THE GOSSAN split one four-variant enum into two four-variant ones, so the
//! type now admits **sixteen** pairs where four are meaningful — it can say
//! "no metabolism, eats other life" (spec §4.4). This is the declared list,
//! and adding to it is a deliberate edit, the same ratchet shape as
//! `tropes check` and the type-audit waivers.
//!
//! **It is also the READER that keeps `TrophicMode` honest** (spec §4.5).
//! Nothing else consumes that axis in this campaign, and an axis nobody reads
//! is exactly how `MetabolicClass` rotted: its own doc records `Autotroph`
//! being "witnessed by The Menagerie without the modelling decision ever being
//! made", and still called itself an "unused seam" three campaigns later.

use hornvale_species::{ThermalStrategy as T, TrophicMode as M, biosphere_registry};

/// Every pair a kind is allowed to carry. Rung 1 of the Underworld Larder
/// declares four; rung 2 adds a `Chemotrophic` row, and that edit is the
/// visible moment the underworld gains a productive base.
const SANCTIONED: &[(T, M)] = &[
    (T::Endothermic, M::Heterotrophic),
    (T::Ectothermic, M::Heterotrophic),
    (T::Unmodelled, M::Phototrophic),
    (T::Absent, M::Absent),
];

#[test]
fn every_kind_carries_a_sanctioned_pair() {
    let mut checked = 0;
    for (kind, bio) in biosphere_registry().iter() {
        let pair = (bio.thermal_strategy, bio.trophic_mode);
        assert!(
            SANCTIONED.contains(&pair),
            "{} carries {pair:?}, which is not a sanctioned pair. The split \
             admits 16 combinations and only these are meaningful; if this one \
             genuinely is, add it to SANCTIONED deliberately and say why.",
            kind.0
        );
        checked += 1;
    }
    assert!(
        checked >= 20,
        "only {checked} kinds checked — the registry is not being read and this \
         guard is vacuous"
    );
}

/// `Chemotrophic` is DECLARED and not WITNESSED — the variant exists, no kind
/// carries it. Rung 2's success condition is that this test has to change.
#[test]
fn chemotrophic_is_declared_and_unwitnessed() {
    let carriers: Vec<&str> = biosphere_registry()
        .iter()
        .filter(|(_, b)| b.trophic_mode == M::Chemotrophic)
        .map(|(k, _)| k.0)
        .collect();
    assert!(
        carriers.is_empty(),
        "{carriers:?} carry TrophicMode::Chemotrophic. THE GOSSAN ships that \
         variant declared-but-unwitnessed on purpose: authoring a chemotroph \
         needs an energy field to feed it, which is rung 2. If rung 2 has \
         landed, this assertion is what you came to delete."
    );
    assert!(
        !SANCTIONED.iter().any(|(_, m)| *m == M::Chemotrophic),
        "SANCTIONED already admits a Chemotrophic pair while no kind carries \
         one — the declaration and the roster have drifted apart"
    );
}
```

- [ ] **Step 2: Register the module**

In `domains/species/tests/suite.rs`:

```rust
#[path = "suite/metabolic_pairs.rs"]
mod metabolic_pairs;
```

- [ ] **Step 3: Run to verify it passes**

Run: `cargo nextest run -p hornvale-species -E 'test(metabolic_pairs)'`
Expected: both tests PASS.

- [ ] **Step 4: Prove the guard can fail**

Run:
```bash
python3 scripts/mutate.py domains/species/src/lib.rs \
  "                thermal_strategy: ThermalStrategy::Endothermic,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.50), (ANIMAL_PREY, 0.50)]).unwrap()," \
  "                thermal_strategy: ThermalStrategy::Absent,
                trophic_mode: TrophicMode::Heterotrophic,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.50), (ANIMAL_PREY, 0.50)]).unwrap(),"
cargo nextest run -p hornvale-species -E 'test(every_kind_carries_a_sanctioned_pair)'
git checkout -- domains/species/src/lib.rs
```
Expected: RED naming `goblin` and the unsanctioned pair, then the tree
restored. If the mutation does not apply, the `cargo fmt` in Task 4 wrapped
these lines differently — read the actual text and adjust the pattern rather
than skipping the control.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "test(the-gossan): the sanctioned-pair table, which is also TrophicMode's only reader

The split admits 16 pairs where 4 are meaningful, and that is the real cost of
splitting rather than adding a variant. This declares the four.

Its second job matters more: TrophicMode has no production consumer in this
campaign, and an axis nobody reads is precisely how MetabolicClass rotted —
its own doc records Autotroph being witnessed without the modelling decision
ever being made, and still called itself an unused seam three campaigns later.
This table consults every kind's value on every run.

Chemotrophic ships declared-and-unwitnessed, asserted both ways. Rung 2's
success condition is that this test has to change.

Mutation control run and reverted: setting goblin to (Absent, Heterotrophic)
reddens it by name."
```

---

### Task 7: The three positive controls, run and recorded

Spec §5.3. These are measurements: record what actually happened, and treat a
surprise as a finding rather than something to re-run until it agrees.

**Files:**
- Modify: `docs/superpowers/specs/2026-08-24-the-gossan-design.md` (§5.3 gains results)

- [ ] **Step 1: C1 — the blind spot**

Mutate `rise_at` **only**. Allometry is deliberately left alone: the whole
point of this control is that a purely thermal-*behaviour* defect is invisible
to instrument 1, and mutating allometry as well would redden both and prove
nothing about the blind spot.

```bash
python3 scripts/mutate.py windows/vessel/src/liveness.rs \
  "        ThermalStrategy::Unmodelled | ThermalStrategy::Absent => base," \
  "        ThermalStrategy::Absent => base,
        ThermalStrategy::Unmodelled => {
            let excess = (temp - THERMONEUTRAL_C).max(0.0);
            base * (1.0 + ENDOTHERM_HEAT_K * excess / HEAT_SCALE_C)
        }"
cargo nextest run -p hornvale-species -E 'test(life_history_golden)'
cargo nextest run -p hornvale-vessel -E 'test(rise_at_couples_heat_to_thirst_per_metabolic_class)'
git checkout -- windows/vessel/src/liveness.rs
```

This gives `Unmodelled` the endotherm's thirst model — exactly the defect a
naive `Autotroph -> Endothermic` mapping would have introduced.

Expected: **species golden GREEN, drive pin RED.**

**If the drive pin comes back GREEN, STOP.** Instrument 2 does not work and the
campaign's central claim is unproven. Do not proceed to Step 4.

- [ ] **Step 2: C2 — instrument 1 can fail**

```bash
python3 scripts/mutate.py domains/species/src/allometry.rs \
  "        ThermalStrategy::Ectothermic => B0_ENDOTHERM * ECTOTHERM_METABOLIC_FRACTION," \
  "        ThermalStrategy::Ectothermic => B0_ENDOTHERM,"
cargo nextest run -p hornvale-species -E 'test(life_history_golden)'
git checkout -- domains/species/src/allometry.rs
```

Expected: **species golden RED**, naming `kobold` among the moved rows.

- [ ] **Step 3: C3 — the pair guard is not decorative**

Already run as Task 6 Step 4. Re-state its result here rather than re-running.

- [ ] **Step 4: Record the results in the spec**

Replace spec §5.3's predicted block with what actually happened — the command,
the verdict, and the failure message for each red. If any result differed from
the prediction, say so plainly and explain what it means; a control that
surprised you is the most valuable line in the document.

- [ ] **Step 5: Verify the tree is clean**

Run: `git status --short`
Expected: only `docs/superpowers/specs/2026-08-24-the-gossan-design.md` and
`docs/timings.md` modified. **Any modified `.rs` file means a control was not
reverted** — restore it before committing.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "docs(the-gossan): the three positive controls, with results

Recorded as measurements rather than predictions. C1 is the one that matters:
it gives Unmodelled the endotherm's thirst model — the exact defect a naive
Autotroph -> Endothermic mapping would have introduced — and the species
golden stays GREEN while the drive pin goes RED. That is the blind spot the
spec claimed instrument 1 has, demonstrated rather than argued, and it is why
this campaign has two instruments instead of one."
```

---

## Definition of Done

- [ ] Both instruments green; `domains/species/tests/fixtures/life-history-all-kinds.txt` byte-identical to its Task 1 capture.
- [ ] `cargo nextest run --workspace` and `cargo test --workspace --doc` green.
- [ ] `make rebaseline && git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')` clean. **Expected to be clean for two reasons and only one of them is good** — the change moves no number, AND the census half of that check does not regenerate locally (spec §5.1). Do not read this as independent confirmation.
- [ ] The three controls run, with results recorded in spec §5.3.
- [ ] Chronicle entry in `book/src/chronicle/the-gossan.md`.
- [ ] Retrospective in `docs/retrospectives/the-gossan.md`, promoting `.superpowers/sdd/followups.md` F-1..F-5 before the worktree is swept.
- [ ] `BIO-chemotrophy` flipped `raw` → `shipped` with its Where cell pointing at the chronicle. **Nathan's call, not the executor's** — a moved status is his.
- [ ] `make sluice-stage` at the Task 4/5 boundary, and `make sluice` to merge.

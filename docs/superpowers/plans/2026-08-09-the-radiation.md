# The Radiation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Author six elves — Wood, High, Drow, Sea, Desert, Snow — onto the
existing biome-affinity and realm-gate mechanisms, measure the five
preregistered predictions and the stated null, and close the peoples programme
at fifteen peoples.

**Architecture:** This campaign builds **no new mechanism**. Every one of the
six is expressible with what The Warren, The Delvers and The Range already
shipped. The work is authoring plus measurement, sequenced so that the one
irreversible act — appending an accession cohort — lands first and alone, with
its additivity proven by mutation; then the eight per-kind registries in one
commit (a `Settled` biosphere row without its peopled cluster hard-fails
`assemble()` workspace-wide); then the affinity rows; then the readouts.

**Tech Stack:** Rust 2024, `hornvale-species` and `hornvale-language`
(domains), `hornvale-worldgen` (composition root), `hornvale-lab` (census
metrics). No new dependencies.

Spec: `docs/superpowers/specs/2026-08-09-the-radiation-design.md` (binding).
Ledger: `.superpowers/sdd/decision-ledger.md` (git-ignored; promote into the
retrospective before teardown).

---

## Global Constraints

Every task's requirements implicitly include this section.

- **Dependencies:** `serde`, `serde_json`, `libm` only, workspace-wide. No new
  crates (no rand, chrono, clap, thiserror). The allowlist is `ALLOWED_EXTERNAL`
  in `cli/tests/architecture.rs`.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only, enforced
  workspace-wide by `clippy.toml` `disallowed-types`. Float sorting uses
  `total_cmp`.
- **No wall-clock time anywhere, including in test code.**
  `std::time::Instant` is a banned type workspace-wide; time is
  `WorldTime { day: f64 }`.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a
  one-line doc comment.
- **Rust edition 2024. `cargo fmt` is the final step before every commit.**
  Fmt-gate skips are the project's most common review finding.
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain crate
  depends on `hornvale-kernel` and nothing else — this is why `BiomeAffinity`
  is keyed by the biome's **stable name string** and resolved at the
  composition root, and why `HabitatRealm` is not `hornvale_climate::Realm`.
- **`docs/audits/type-audit-report.md` is regenerated in the SAME commit that
  drifts it.** Any new `pub` boundary primitive drifts it. Run
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`.
- **The generated-artifact drift check has SEVEN paths and FIVE copies across
  the repo.** The paths:
  ```
  book/src/gallery/  book/src/reference/  book/src/laboratory/  docs/audits/
  docs/digest/  book/src/domesday/  clients/game/core/tests/fixtures/
  ```
  The five copies that must agree: root `CLAUDE.md:259`, `cli/CLAUDE.md:76`,
  `scripts/CLAUDE.md:45`, `windows/CLAUDE.md:72`, `.github/workflows/ci.yml:96`
  (brought into agreement by `58fcbecc`). Do not edit one without the others.
- **Regenerate artifacts in the commit that drifts them, never at the close.**
  Every task below that moves the world ends with `make rebaseline` + the
  seven-path diff, so each commit is internally consistent. A red gate freezes
  the artifact it writes, and the drift check then passes *because nothing
  moved* — the failure mode this rule exists to prevent.
- **Census is lefford-only and authorization-gated.** Never run a census here,
  never set `HV_CENSUS=1`; `census-run.sh` fails closed on the hostname
  (decisions 0063 / 0079 / 0081 / 0086). Budget ~15 min per run (776 / 887 /
  921 s measured on lefford 2026-08-09).
- **Determinism:** same seed + pins → byte-identical worlds. This campaign
  introduces **no new seed-derivation label**: every elf's roots and lexemes
  draw on the existing `language/<species>/…` and `language/<family>/…` label
  *patterns* with a new concrete `<species>` / `<family>` value, which is a new
  draw on a new subject, not a re-ordered draw on an existing one.
  `ROOT_EPOCH` stays at `v3`. **If any task finds itself adding a label or
  editing an existing cohort, STOP and escalate — that is an epoch.**
- **Append a cohort; never edit one.** `domains/language/src/accession.rs`'s
  rule is absolute and its own doc records a withdrawn exception. A concept
  that changed epoch re-sorts and moves every assignment after it.
- **The affinity ceiling is `1.00` and the floor is not `0.0`.** A row is a
  *mask*: every undeclared kind carries an implicit `1.0`, so a declared row can
  only subtract. `0.0` is a hard exclusion (genesis filters its founding pool on
  `caps_now()[pidx].at(c) > 0.0`), not a strong preference.
- **A uniform affinity row is a provable placement no-op.** Genesis and
  `best_home` rank cells in the kind's own units, so a constant factor reorders
  nothing. Only the *shape* across biomes carries information.
- **Never both routes on one kind.** A kind may carry a biome affinity only if
  `elevation.devotion < sovereignty_floor(mass, potency)`, which makes the
  Liebig minimum discard its temperature/moisture/insolation curves everywhere.
  Otherwise the row double-counts a preference the model already applies.
- **Do not author a correlation between articulation and environment, and do
  not report one.** The same hand authors both; any correlation measures the
  authoring convention. No sentence of the form "the Drow tongue is harsh
  *because* the Underdark is" may appear in a comment, a chronicle or a
  retrospective (spec §6).
- **`make gate` is ~8 min since decision 0113.** One gating agent at a time on
  this box; before starting a gate, wait out any running one:
  `until ! pgrep -f "cargo-nextest|cargo nextest" >/dev/null; do sleep 30; done`.

---

## The roster's identifiers — fixed here so every task agrees

These strings are used verbatim in six files. They are settled; do not vary
them.

| `KindId` | concept id | gloss | family | realm | affinity strongholds |
| --- | --- | --- | --- | --- | --- |
| `desert-elf` | `desert-elf-kind` | `a desert elf` | `elf` | Surface | `desert` (+ `savanna`, `shrubland` out-bands) |
| `drow` | `drow-kind` | `a drow` | `elf` | **Subterranean** | Wood's surface shape, unchanged |
| `high-elf` | `high-elf-kind` | `a high elf` | `elf` | Surface | **identical to Wood's row** |
| `sea-elf` | `sea-elf-kind` | `a sea elf` | `elf` | Surface | `coral-reef`, `kelp-forest`, `upwelling`, `epipelagic` |
| `snow-elf` | `snow-elf-kind` | `a snow elf` | `elf` | Surface | `tundra`, `ice` (+ `taiga` out-band) |
| `wood-elf` | `wood-elf-kind` | `a wood elf` | `elf` | Surface | `temperate-forest` (+ out-bands) |

`ComponentStore` key order is lexicographic, so the family scatters:
`desert-elf` < `drow` < `high-elf` < `sea-elf` < `snow-elf` < `wood-elf`, and
`desert-dwarf` < `desert-elf` < `dire-wolf`.

**All six carry a `biome_affinity` row (six rows, not five).** Spec §3.7's
parenthetical reads "biome_affinity (sparse, five of six)" and is not
reconcilable with the spec's own predictions: P3(a)'s primary arm is that
Wood's and High's capacity fields are **bit-identical**, which is only possible
if High carries the same row as Wood; and P4 says Drow's separation from Wood
disappears without the realm row "**up to Drow's own biome and curve
authoring**", which presupposes Drow has biome authoring. Six rows is the
reading the predictions require; §3.7's count is treated as an erratum and is
corrected in the chronicle.

---

## Where each task absorbs main

The branch has already taken **83 commits in two absorptions today**, and the
second one (The Quire) added `clients/game/core/tests/fixtures/` to the drift
list — a path the spec had to be amended for at G3. Assume main moves again.

- **Before Task 1**, and **at every task boundary from Task 2 onward**: run
  `make preflight` from this branch. On an ancestry NO-GO, merge main INTO the
  branch and re-run the gate here.
- **Two exceptions.** Do not absorb between Task 3's baseline capture and Task
  4's readout — a preregistered study's baseline and its readout must see the
  same physics; finish the readout first. Do not absorb while main's checkout
  shows another session mid-landing (preflight peeks and warns).
- **After any absorption, re-run `make rebaseline` and re-diff the seven
  paths.** A generated artifact has no merge: it is regenerated, not
  reconciled.
- **Read the other branches' chronicles, not just their diffs.** Preflight
  mechanizes only the checkable half; The Tumult and The Waterline collided
  semantically with a clean GO.

---

### Task 1: The six elf concepts, in one appended cohort

The campaign's one irreversible act, landed first and **alone**, so its
additivity is proven with nothing else in the diff. Six concepts join
`KIND_CONCEPTS` and one new cohort joins the end of `EPOCH_COHORTS`.

**Why one cohort and not two.** A cohort is appended as one `&[…]` to the end
of `EPOCH_COHORTS` and **never edited** — sorting by epoch first makes a new
concept land strictly last, "the one position that provably displaces
nothing". Splitting the six across two cohorts would permanently separate the
family's roots and could not be undone. The module's own history is the reason
the rule is absolute: before cohorts existed, twelve kinds added at once left
ten free while `treant` moved 5 facts and `otyugh` 65.

**Note what this task does NOT do.** It does not add `family_of` rows.
`domains/species/src/lib.rs`'s in-module test asserts
`assert_eq!(bio_ids, fam_ids, "family covers exactly the biosphere set")`, so
a `family_of` row without a matching `biosphere` row reddens immediately —
`family_of` and `family_proto` therefore land in Task 2 with the biosphere
rows. (The spec's §3.7 groups them all under "the authoring cost"; the
*landing order* is constrained separately.)

**Files:**
- Modify: `domains/species/src/lib.rs:3533-3579` (`KIND_CONCEPTS`, append six)
- Modify: `domains/language/src/accession.rs:78-396` (`EPOCH_COHORTS`, append
  cohort 10 at the end — do not touch cohorts 0–9)
- Test: `domains/language/tests/accession_properties.rs` (append one test)
- Regenerate: `book/src/reference/` (the `hornvale concepts` dump),
  `docs/audits/`, `docs/digest/`, and the rest of the seven paths

**Interfaces:**
- Consumes: `hornvale_language::EPOCH_COHORTS`,
  `hornvale_language::concept_epoch(&str) -> u32`,
  `hornvale_language::assign_proto_roots_with_epoch_for_test(seed: &Seed,
  family: &str, proto_ph: &Phonology, concepts: &[&str], daughters: &[Daughter],
  epoch_of: impl Fn(&str) -> u32) -> BTreeMap<String, Vec<Segment>>`,
  `hornvale_language::draw_phonology(&Seed, &str, &Envelope) -> Phonology`.
- Produces: six new concept ids registered and accessioned, listed in the
  roster table above. Task 2 relies on the `*-kind` ids existing so
  `hornvale_species::kind_concept("wood-elf")` resolves.

- [ ] **Step 1: Capture the pre-campaign world hashes (P1's true baseline)**

Do this **before any edit**. Three seeds, the set The Delvers and The Range
both used, so the numbers are comparable with their published tables.

```bash
mkdir -p .superpowers/sdd/baselines
for s in 42 7 1234; do
  cargo run -q -p hornvale -- new --seed "$s" --out ".superpowers/sdd/baselines/pre-campaign-$s.json"
done
shasum -a 256 .superpowers/sdd/baselines/pre-campaign-*.json \
  | tee .superpowers/sdd/baselines/pre-campaign.sha256
```

Paste all three hashes into this task's commit message. `.superpowers/sdd/` is
git-ignored and **must never be force-added** — a committed ledger silently
clobbers every parallel session's on absorption, raising no conflict. Promote
the hashes into the chronicle before teardown.

- [ ] **Step 2: Write the failing additivity test**

Append to `domains/language/tests/accession_properties.rs`:

```rust
/// THE RADIATION (C2d): appending a cohort leaves every PRE-EXISTING
/// concept's proto-root untouched, and folding the same six concepts into an
/// earlier cohort does not.
///
/// This is the campaign's save-format guard. `concept_epoch` sorts by epoch
/// first, so an appended concept lands strictly last — the one position that
/// provably displaces nothing. The alternative is not hypothetical: before
/// this module existed, twelve species kinds added at once left ten free while
/// `treant` moved 5 facts and `otyugh` 65, and omitting the cohort entirely
/// changes which proto-root a concept draws (commit `ee4e6a00`).
///
/// **The test discriminates by construction.** Two arms differ only in the
/// epoch function: the shipped one (elf concepts at the new last cohort) and a
/// mutant that reports epoch 0 for them, i.e. exactly what folding them into
/// cohort 0 would do. The shipped arm must reproduce the no-elf assignment
/// EXACTLY on every seed; the mutant must break it on at least one. Without
/// the second clause the first is satisfiable by an assignment that ignores
/// epochs altogether.
#[test]
fn appending_the_elf_cohort_displaces_no_existing_proto_root() {
    const ELF_CONCEPTS: [&str; 6] = [
        "desert-elf-kind",
        "drow-kind",
        "high-elf-kind",
        "sea-elf-kind",
        "snow-elf-kind",
        "wood-elf-kind",
    ];

    // The whole accessioned universe, which `cli/tests/accession.rs` pins
    // equal to the concept registry in both directions — so this is the real
    // population, not a hand-picked slice.
    let all: Vec<&'static str> = hornvale_language::EPOCH_COHORTS
        .iter()
        .flat_map(|cohort| cohort.iter().copied())
        .collect();
    assert!(
        ELF_CONCEPTS.iter().all(|c| all.contains(c)),
        "the elf cohort has not been appended yet — this test measures the \
         appended table against a synthetic no-elf control, so it cannot run \
         before the cohort exists"
    );
    let without_elves: Vec<&'static str> = all
        .iter()
        .copied()
        .filter(|c| !ELF_CONCEPTS.contains(c))
        .collect();
    assert_eq!(
        without_elves.len() + ELF_CONCEPTS.len(),
        all.len(),
        "the six elf concepts must appear exactly once each in the table"
    );

    let shipped_epoch = hornvale_language::concept_epoch;
    let folded_epoch = |c: &str| {
        if ELF_CONCEPTS.contains(&c) {
            0
        } else {
            hornvale_language::concept_epoch(c)
        }
    };

    let mut folded_moved_somewhere = false;
    for raw in 1u64..=8 {
        let seed = Seed(raw);
        let ph = draw_phonology(&seed, "goblin", &permissive_envelope());

        let control = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &without_elves,
            &[],
            shipped_epoch,
        );
        let appended = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &all,
            &[],
            shipped_epoch,
        );
        let folded = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &all,
            &[],
            folded_epoch,
        );

        for concept in &without_elves {
            assert_eq!(
                control.get(*concept),
                appended.get(*concept),
                "seed {raw}: appending the elf cohort moved `{concept}`'s \
                 proto-root. Appending must be additive BY CONSTRUCTION — if \
                 this fires, the six concepts are not in the LAST cohort, or \
                 an existing cohort was edited. Do not re-pin this; fix the \
                 table."
            );
            if control.get(*concept) != folded.get(*concept) {
                folded_moved_somewhere = true;
            }
        }
    }

    assert!(
        folded_moved_somewhere,
        "ANTI-VACUITY: folding the six elf concepts into cohort 0 moved no \
         existing proto-root on any of eight seeds, so the additivity clause \
         above proves nothing — the epoch ordering is not reaching the \
         assignment at all. Investigate before trusting this test."
    );
}
```

This reuses `permissive_envelope()` (line 28) and the `Seed` /
`draw_phonology` / `assign_proto_roots_with_epoch_for_test` imports already at
the top of that file. No new imports are needed.

- [ ] **Step 3: Run it and confirm it fails for the stated reason**

```bash
cargo test -p hornvale-language --test accession_properties \
  appending_the_elf_cohort_displaces_no_existing_proto_root
```

Expected: FAIL on the first assertion — *"the elf cohort has not been appended
yet"*. That is the correct red: the control arm is synthesised from the real
table, so the test genuinely cannot run before the table grows.

- [ ] **Step 4: Append the six concepts to `KIND_CONCEPTS`**

At the end of `domains/species/src/lib.rs`'s `KIND_CONCEPTS` (after the three
dwarf rows at :3576-3578):

```rust
    // THE RADIATION (C2d): the elf family's six — the roster's largest family,
    // and the programme's last. These six ids are what
    // `domains/language/src/accession.rs`'s epoch-10 cohort lists;
    // `cli/tests/accession.rs` checks the two agree in BOTH directions, and
    // commit `ee4e6a00` records that omitting the cohort also changes which
    // proto-root each concept draws. Glosses are authored, not derived from
    // the id, so `drow` reads as "a drow" and not as its own key.
    ("desert-elf-kind", "a desert elf"),
    ("drow-kind", "a drow"),
    ("high-elf-kind", "a high elf"),
    ("sea-elf-kind", "a sea elf"),
    ("snow-elf-kind", "a snow elf"),
    ("wood-elf-kind", "a wood elf"),
```

- [ ] **Step 5: Append cohort 10 to `EPOCH_COHORTS`**

At the very end of `domains/language/src/accession.rs`'s `EPOCH_COHORTS`
(after the epoch-9 dwarf cohort at :395), **without touching any earlier
cohort**:

```rust
    // Epoch 10 — The Radiation (C2d, 2026-08-09): the elf family, six kinds.
    // ONE cohort, not six: the campaign is one arrival event and one epoch,
    // and six cohorts would assert six successive language epochs — a far
    // stronger claim about the world's history than this campaign makes, and
    // one that would permanently separate the family's roots. Appended rather
    // than folded into an earlier peopled-kind cohort (0, 2, 8, 9), per this
    // module's absolute rule.
    //
    // The roster's largest family: six daughters against goblinoid's three and
    // dwarf's three. `domains/language/tests/accession_properties.rs`'s
    // `appending_the_elf_cohort_displaces_no_existing_proto_root` measures the
    // additivity this placement buys, and measures the mutant that would lose
    // it.
    &[
        "desert-elf-kind",
        "drow-kind",
        "high-elf-kind",
        "sea-elf-kind",
        "snow-elf-kind",
        "wood-elf-kind",
    ],
```

- [ ] **Step 6: Run the additivity test and the parity checks**

```bash
cargo test -p hornvale-language --test accession_properties
cargo test -p hornvale-language --lib accession
cargo test -p hornvale --test accession
```

Expected: all PASS. `cli/tests/accession.rs` is the bidirectional parity gate —
`every_registered_concept_has_an_accession_epoch` and
`every_accessioned_concept_is_actually_registered` both green means
`KIND_CONCEPTS` and cohort 10 agree exactly. If only one is red, one of the two
lists has a typo and the other list's real name is silently at epoch 0.

- [ ] **Step 7: Measure what the six concepts alone did to the world**

```bash
for s in 42 7 1234; do
  cargo run -q -p hornvale -- new --seed "$s" --out ".superpowers/sdd/baselines/post-t1-$s.json"
done
shasum -a 256 .superpowers/sdd/baselines/post-t1-*.json \
  | tee .superpowers/sdd/baselines/post-t1.sha256
```

Expected: the hashes **DIFFER** from Step 1's. Six registered concepts enter
every people's exposure map (`exposure_of_impl` closes with a loop over every
registered concept), so the ledger grows. That is expected and is *not* P1 —
P1 is measured against **this** hash in Task 2, so that "the roster moved the
world" is not confounded with "the concepts moved the world". Record both hash
sets in the commit message.

- [ ] **Step 8: Regenerate artifacts, gate, commit**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ \
  docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
cargo fmt
until ! pgrep -f "cargo-nextest|cargo nextest" >/dev/null; do sleep 30; done
make gate
```

`book/src/reference/` holds the `hornvale concepts` registry dump and **will**
drift on six new concepts. `docs/audits/` and `docs/digest/` are the commonly
missed two.

```bash
pwd && git branch --show-current
git add domains/species/src/lib.rs domains/language/src/accession.rs \
  domains/language/tests/accession_properties.rs \
  book/ docs/audits/ docs/digest/ clients/game/core/tests/fixtures/
git commit -F <message-file> -- domains/species/src/lib.rs \
  domains/language/src/accession.rs \
  domains/language/tests/accession_properties.rs \
  book/ docs/audits/ docs/digest/ clients/game/core/tests/fixtures/
```

The message must carry: the Step 1 pre-campaign hashes, the Step 7 post-Task-1
hashes, and the sentence "cohort 10 appended; cohorts 0-9 untouched".

---

### Task 2: The eight registries, in one commit

Six kinds × eight registries, plus `family_of`, `family_proto` and the sparse
`habitat_realm` row for Drow. **One commit, not six** — landing a `Settled`
biosphere row without its peopled cluster makes `WorldComponents::assemble()`
hard-fail workspace-wide (`"Settled kind {k:?} is missing a peopled
component"`), and the pre-commit hook runs `make quick` workspace-wide
regardless of staged paths, so this is not something a plan can split.

**The complete admission checklist**, read straight off `check_integrity`
(`windows/worldgen/src/components.rs:244-345`). Each elf must satisfy all
seven:

1. `articulation` and `lexicon` key-sets stay **equal** — add to both.
2. In `perception` ⇒ in `psyche`.
3. In `articulation` ⇒ in `psyche`, in `perception`, and in `family_of`.
4. In `psyche` ⇒ in `biosphere`.
5. `biosphere[k].social_form == Settled` ⇒ in all four of `psyche`,
   `perception`, `articulation`, `lexicon`.
6. `society` key-set **exactly equals** `{k ∈ biosphere : social_form.is_social()
   && psyche.contains(k)}` — so each elf needs a society row, no more and no
   fewer.
7. `family_of` label `"elf"` reaches six members ⇒ `family_proto` must carry
   `KindId("elf")`. It becomes mandatory at the **second** row, so it lands in
   this same commit because it must.

Two in-module invariants outside `check_integrity` also bite, and both are in
`domains/species/src/lib.rs`'s test module:
`assert_eq!(bio.len(), 33, …)` must become 39, and
`assert_eq!(bio_ids, fam_ids, "family covers exactly the biosphere set")` means
`family_of` must gain exactly the same six keys. The alphabetical roster list
in `registry_is_ordered_alphabetically_and_kobold_contrasts` (:3900-3937) must
gain the six names in lexicographic position.

**Files:**
- Modify: `domains/species/src/lib.rs` — six `*_condition_niche()` helpers
  (beside `hill_dwarf_condition_niche` at :1897); `biosphere_registry` (:2321);
  `psyche_registry` (:2847); `dispersion_registry` (:2999); `society_registry`
  (:3134); `perception_registry` (:3274); `family_of` (:3446);
  `habitat_realm_registry` (:2052, Drow only); the three in-module test pins
  above
- Modify: `domains/language/src/lib.rs` — `articulation_registry` (:275),
  `lexicon_registry` (:483), `family_proto` (:647)
- Test: `windows/worldgen/tests/radiation_admission.rs` (create)
- Test: extend `windows/worldgen/src/lib.rs`'s in-module
  `cascade_regime_of_matches_the_authored_regime_map` (:9316) — N1's home

**Interfaces:**
- Consumes: everything Task 1 produced; `hornvale_kernel::sovereignty_floor(mass: Mass, potency: f64) -> f64`;
  `hornvale_species::life_history(mass, class, schedule) -> LifeHistory`;
  `hornvale_species::{BiosphereTraits, MindVector, SocietyVector, PerceptionVector, Dispersion, ConditionNiche, LifeSchedule, MetabolicClass, SocialForm, HabitatRealm}`;
  `hornvale_kernel::{MARINE_FORAGE, PLANT_FORAGE, ANIMAL_PREY, DETRITUS, ResourceVector, Mass, ConditionResponse}`;
  `hornvale_language::{ArticulationVector, ExoticManner, speech::Lexicon}`.
- Produces: six `Settled` peopled kinds with family label `"elf"`;
  `hornvale_language::family_proto()` carries `KindId("elf")`;
  `hornvale_species::habitat_realm_registry()` carries
  `(KindId("drow"), HabitatRealm::Subterranean)`. Task 3 relies on each elf's
  `mass`, `potency` and `condition_niche.elevation.devotion` satisfying the
  affinity precondition.

**What the plan fixes and what it leaves to authoring.** The numbers — masses,
the four scalar devotions, the articulation vectors, the lexicon words — are
**not** fixed here. The spec fixes strategy and constraints; the values are
authored against measured occupancy
(`windows/worldgen/tests/fixtures/occupancy.csv`, 386 rows) and the
settleable-land percentile table in `ConditionNiche`'s doc (p15=142, p25=621,
p35=1004, p50=1561, p65=2166, p75=2651, p85=3251, p95=4148 m ASL; moisture
p5=0.24 / p50=0.49 / p95=0.70; temperature p5=3.27 / p50=14.59 / p95=31.59 °C;
insolation p5=0.19 / p50=0.25 / p95=0.31). What the plan fixes is *what must be
true of them*, and Step 1 is the test that catches a bad one.

- [ ] **Step 1: Write the failing admission test**

Create `windows/worldgen/tests/radiation_admission.rs`:

```rust
//! THE RADIATION (C2d): the six elves' structural admission.
//!
//! Every claim here is a precondition for a LATER task, asserted now so that a
//! bad authored value fails on the value rather than on the measurement three
//! tasks downstream. Nothing in this file measures the world; it reads the
//! registries.

use hornvale_kernel::KindId;
use hornvale_species::{HabitatRealm, SocialForm};

/// The six, in ascending `KindId` order — the order every `ComponentStore`
/// iterates in, so a slice built from this list is index-aligned with one
/// built from `wc.biosphere`.
const ELVES: [&str; 6] = [
    "desert-elf",
    "drow",
    "high-elf",
    "sea-elf",
    "snow-elf",
    "wood-elf",
];

/// The whole peopled cluster, in one place. `WorldComponents::assemble()`
/// already enforces the lattice and would fail the whole workspace if a
/// component were missing — this test exists to say WHICH one, on a kind
/// nobody has run a world for yet, instead of leaving an implementer to read
/// `"Settled kind KindId(\"sea-elf\") is missing a peopled component"` and
/// guess.
#[test]
fn every_elf_carries_the_full_peopled_cluster() {
    let wc = hornvale_worldgen::WorldComponents::assemble()
        .expect("canonical registries are well-formed");
    for name in ELVES {
        let k = KindId(name);
        assert!(wc.biosphere.contains(&k), "{name}: no biosphere row");
        assert!(wc.psyche.contains(&k), "{name}: no psyche row");
        assert!(wc.society.contains(&k), "{name}: no society row");
        assert!(wc.perception.contains(&k), "{name}: no perception row");
        assert!(wc.articulation.contains(&k), "{name}: no articulation row");
        assert!(wc.lexicon.contains(&k), "{name}: no lexicon row");
        assert!(wc.family_of.contains(&k), "{name}: no family_of row");
        assert_eq!(
            wc.family_of.get(&k),
            Some(&"elf"),
            "{name} must carry the family label \"elf\" — the six share one \
             proto, and `family_proto` is keyed by the LABEL"
        );
        assert_eq!(
            wc.biosphere.get(&k).expect("checked above").social_form,
            SocialForm::Settled,
            "{name} must be Settled: only Settled kinds enter the bake's \
             roster, and a non-Settled elf is authored and inert (P1' would \
             correctly read it as byte-neutral)"
        );
    }
    assert!(
        wc.family_proto.contains(&KindId("elf")),
        "a family label carried by six kinds requires a `family_proto` entry; \
         `check_integrity` makes this mandatory at the SECOND row"
    );
}

/// **The affinity precondition, asserted per elf BEFORE Task 3 may proceed.**
///
/// `tolerance_liebig` floors temperature/moisture/insolation by
/// `sovereignty_floor(mass, potency)` and floors elevation by `0.0`. A floored
/// axis never reads below its floor; the unfloored one peaks at its own
/// `devotion`. So `elevation.devotion < sovereignty_floor` makes elevation the
/// Liebig minimum at every cell of every world, and the other three curves
/// contribute exactly nothing — which is what makes a biome-affinity row a
/// preference the model was THROWING AWAY rather than one it already applies.
///
/// The spec (§3.1) records the satisfiable band: 0.4133 at 45 kg and 0.4477 at
/// 70 kg, both at potency 0. Floors are computed LIVE here, never copied from
/// a plan — The Delvers' plan table was wrong in the fourth decimal for two of
/// three.
///
/// This duplicates, deliberately, what
/// `range_readout.rs::every_occupant_has_climate_curves_the_minimum_currently_discards`
/// will assert once the rows exist. That one iterates the affinity registry and
/// is therefore silent about a kind with no row yet; this one names the six and
/// fires before Task 3 has written a line.
#[test]
fn every_elf_clears_the_affinity_precondition() {
    let biosphere = hornvale_species::biosphere_registry();
    for name in ELVES {
        let bio = biosphere
            .get(&KindId(name))
            .unwrap_or_else(|| panic!("{name} has no biosphere row"));
        let floor = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
        let devotion = bio.condition_niche.elevation.devotion;
        println!(
            "   {name:<12} mass {:>6.1} kg  potency {:.2}  floor {floor:.6}  \
             elev devotion {devotion:.2}",
            bio.mass.kilograms(),
            bio.potency,
        );
        assert!(
            devotion < floor,
            "{name} may NOT take a biome affinity: its elevation devotion \
             ({devotion}) is at or above its sovereignty floor ({floor}), so \
             the Liebig minimum does NOT discard its temperature/moisture/\
             insolation curves. An affinity row on top of them is the \
             DOUBLE-COUNT the spec's §3.1 forbids, and the movement it \
             produced would be unattributable. Lower the devotion or raise \
             the mass — do not add the row."
        );
    }
}

/// Drow is the ONLY elf in the sparse realm store, and every other elf must be
/// absent from it (absence means `Surface`). Asserted in both directions
/// because the Wood/Drow contrast — the family's realm isolate — is only a
/// single-variable contrast if exactly one of the two carries a realm row.
#[test]
fn drow_alone_is_subterranean() {
    let realm = hornvale_species::habitat_realm_registry();
    assert_eq!(
        realm.get(&KindId("drow")),
        Some(&HabitatRealm::Subterranean),
        "drow must be Subterranean: the realm gate is its ONLY authored \
         separation from the surface elves (spec §3.5)"
    );
    for name in ELVES.iter().filter(|n| **n != "drow") {
        assert!(
            realm.get(&KindId(*name)).is_none(),
            "{name} appears in the habitat-realm store; every elf but drow \
             must be absent from it (absence means Surface), or Wood/Drow \
             stops isolating the realm variable"
        );
    }
}

/// **Drow encodes NO depth in its elevation curve.** Depth below the surface
/// and height above sea level are different quantities: a deep chamber under a
/// mountain sits high above the sea, a shallow cave in a marsh sits low. The
/// Delvers committed exactly this fake and caught it — duergar authored at a
/// 300 m optimum to mean *deep* selected lowland marshes, and its toponymy came
/// back as an emergent finding until one question dissolved it. **The toponymy
/// was reporting the authoring.**
///
/// Pinned as an equality with wood-elf rather than as a range, because a range
/// invites the next author to argue their number is inside it. Drow's elevation
/// curve is Wood's curve, and if it ever stops being Wood's curve that is a
/// decision to record.
#[test]
fn drows_elevation_curve_is_woods_and_says_nothing_about_depth() {
    let biosphere = hornvale_species::biosphere_registry();
    let drow = biosphere.get(&KindId("drow")).expect("drow row");
    let wood = biosphere.get(&KindId("wood-elf")).expect("wood-elf row");
    assert_eq!(
        drow.condition_niche.elevation, wood.condition_niche.elevation,
        "drow's elevation response must be IDENTICAL to wood-elf's. The \
         elevation axis is metres above sea level and cannot say \"deep\"; \
         encoding depth into it is The Delvers' withdrawn duergar fake, and \
         the finding it produced was the authoring read back."
    );
}

/// The sea elf must actually be able to eat at sea. `marine_forage_supply_field`
/// keys productivity off the biome class, and a kind with no `MARINE_FORAGE`
/// weight draws zero supply on every water cell no matter what its affinity
/// says — it would be authored, admitted, and void.
#[test]
fn the_sea_elf_draws_on_the_marine_supply_axis() {
    let biosphere = hornvale_species::biosphere_registry();
    let sea = biosphere.get(&KindId("sea-elf")).expect("sea-elf row");
    assert!(
        sea.niche.weight(hornvale_kernel::MARINE_FORAGE) > 0.0,
        "sea-elf has no MARINE_FORAGE weight, so it draws zero supply on every \
         ocean cell and its shelf affinity multiplies zero. Killer whale, \
         giant squid and reef shark are the authoring precedent (all 1.0)."
    );
    for name in ELVES.iter().filter(|n| **n != "sea-elf") {
        let bio = biosphere.get(&KindId(*name)).expect("elf row");
        assert_eq!(
            bio.niche.weight(hornvale_kernel::MARINE_FORAGE),
            0.0,
            "{name} weights MARINE_FORAGE; only sea-elf may, or Sea stops \
             being the family's marine isolate"
        );
    }
}

/// Wood and High are the family's MIND isolate (spec §3.6): same realm, same
/// biomes, differing only in psyche, society and language. That contrast is
/// only single-variable if their BIOSPHERE rows agree exactly — mass moves the
/// sovereignty floor, and the resource vector moves supply, either of which
/// would silently make P3 a two-variable comparison.
#[test]
fn wood_and_high_differ_in_mind_and_not_in_body() {
    let biosphere = hornvale_species::biosphere_registry();
    let wood = biosphere.get(&KindId("wood-elf")).expect("wood-elf row");
    let high = biosphere.get(&KindId("high-elf")).expect("high-elf row");
    assert_eq!(
        wood.mass.kilograms(),
        high.mass.kilograms(),
        "wood-elf and high-elf must share a mass: mass sets the sovereignty \
         floor, and a differing floor makes P3(a) a comparison of two \
         variables"
    );
    assert_eq!(wood.potency, high.potency, "same potency, same floor");
    assert_eq!(
        wood.condition_niche, high.condition_niche,
        "wood-elf and high-elf must share a condition niche — §3.6 says High \
         diverges in MIND and SOCIETY, not in environment"
    );
    assert_eq!(
        wood.niche, high.niche,
        "wood-elf and high-elf must share a resource vector, or supply \
         separates them and P3 measures diet rather than mind"
    );

    let psyche = hornvale_species::psyche_registry();
    assert_ne!(
        psyche.get(&KindId("wood-elf")),
        psyche.get(&KindId("high-elf")),
        "wood-elf and high-elf carry IDENTICAL psyche rows, so High is not a \
         control — it is a duplicate. §3.6's whole claim is that the two \
         differ in mind."
    );
    let society = hornvale_species::society_registry();
    assert_ne!(
        society.get(&KindId("wood-elf")),
        society.get(&KindId("high-elf")),
        "wood-elf and high-elf carry identical society rows; High's identity \
         lives in psyche, society and language and nowhere else"
    );
}
```

- [ ] **Step 2: Run it and confirm every test fails**

```bash
cargo test -p hornvale-worldgen --test radiation_admission
```

Expected: **all six FAIL**, each on its own first missing row (`"desert-elf: no
biosphere row"`, `"desert-elf has no biosphere row"`, `"drow must be
Subterranean"`, …). Confirm the failure *messages*, not just the count — a
compile error here would mean an API name is wrong, which is a different
problem and not the red this step wants.

Each test's discriminating break, for the record:
- `every_elf_carries_the_full_peopled_cluster` — reddens if any of the eight
  rows is omitted, if an elf is authored non-`Settled`, or if `family_proto`
  is missed. Prove it by deleting one `lexicon` row and re-running.
- `every_elf_clears_the_affinity_precondition` — reddens if a mass or an
  elevation devotion is edited so the inequality flips. Prove it by
  temporarily raising one elf's `elevation.devotion` to `0.60`.
- `drow_alone_is_subterranean` — reddens if Drow's realm row is dropped, or if
  a second elf is added to the store. Prove it by adding `wood-elf` to the
  store.
- `drows_elevation_curve_is_woods_…` — reddens if anyone encodes depth. Prove
  it by setting Drow's elevation optimum to `-300.0`.
- `the_sea_elf_draws_on_the_marine_supply_axis` — reddens if Sea is authored
  terrestrial. Prove it by swapping `MARINE_FORAGE` for `PLANT_FORAGE` in
  Sea's vector.
- `wood_and_high_differ_in_mind_and_not_in_body` — reddens both ways: if their
  bodies diverge, and if their minds do not. Prove it by copying Wood's psyche
  row onto High.

Run each mutation, capture the red, revert, and record the six red messages in
the commit message. A test that cannot fail is worse than no test, and this
programme has shipped one.

- [ ] **Step 3: Author the six condition niches**

Add six `fn <kind>_condition_niche() -> ConditionNiche` helpers in
`domains/species/src/lib.rs`, immediately after `hill_dwarf_condition_niche`
(:1930), under a block comment in the style of the C2c block at :1718-1768.
The block comment must state the family rule as a table with **live-computed**
floors:

```
//   kind          mass   sov. floor   dev_el   mode     identity carried by
//   wood-elf      ??.?     0.4???       ?.??   below    biome affinity
//   high-elf      ??.?     0.4???       ?.??   below    psyche/society/language
//   drow          ??.?     0.4???       ?.??   below    the realm gate
//   sea-elf       ??.?     0.4???       ?.??   below    biome affinity (shelf)
//   desert-elf    ??.?     0.4???       ?.??   below    biome affinity
//   snow-elf      ??.?     0.4???       ?.??   below    biome affinity
```

Constraints the authored values must satisfy — each one is checked by a test
named above, so a violation is caught rather than reviewed:

- **Every elf is on the biome-affinity route.** `elevation.devotion <
  sovereignty_floor(mass, potency)` for all six. The dwarves' `0.30` clears
  both ends of the 45–70 kg band with margin and is the safe default.
- **Never both routes.** No elf may be authored above its floor. The
  climate-curve arm already exists in merged work (desert-dwarf, measured to
  bind on 67–91 % of land and buy almost no separation); do not author a
  people badly to manufacture a control.
- **Wood and High share their entire `ConditionNiche` and their entire
  `BiosphereTraits` except nothing** — same mass, same potency, same niche,
  same curves. High's divergence is mind and society only.
- **Drow's `elevation` is Wood's `elevation`, byte for byte.** Its
  `insolation` response *is* authored for cave-dark — and is **dormant by
  measurement, not by omission**; say so in the doc comment, and cite
  `windows/worldgen/tests/warren_readout.rs`'s tripwire as the thing that
  reddens when it stops being dormant.
- **Sea's elevation optimum is negative** (metres above sea level; in the
  ocean, depth *is* −height, so the curve can honestly say "shallow"). Killer
  whale (−40 m) and giant squid (~−1263 m) are the precedent. Centre it on the
  productive shelf, not the abyss.
- Every doc comment on a below-floor kind must say **PREPARED: never binds**
  against its temperature/moisture/insolation curves, as the two below-floor
  dwarves do. Authoring an honest curve the model discards is the model; implying
  a preference it will never read is not.

- [ ] **Step 4: Author the biosphere rows**

Six entries in `biosphere_registry`, following the dwarf rows at :2764-2836:

- `mass` in the 45–70 kg band the spec's floor table covers, `Mass::new(x).unwrap()`.
- `metabolic_class: MetabolicClass::Endotherm`.
- `niche: ResourceVector::new(&[…]).unwrap()`, summing to 1.00 as every other
  row does. Sea uses `MARINE_FORAGE`; nobody else may.
- `potency: 0.0` — the peoples carry zero.
- `social_form: SocialForm::Settled` for all six.
- `schedule: LifeSchedule::paced(f).unwrap()` with `f >= 4.0` (elves are the
  roster's longest-lived). **N1 (Step 8) is what makes this honest**: pacing
  harder than the dwarves buys nothing in language, and the plan requires that
  to be asserted rather than assumed.

Then fix the three in-module pins:
`assert_eq!(bio.len(), 33, …)` → `39` with the reason text extended; the
alphabetical roster vector at :3900-3937 gains the six names in position; the
`bio_ids == fam_ids` assertion needs no edit but will only pass once Step 7
lands.

- [ ] **Step 5: Author psyche, dispersion, society and perception**

Four registries, six rows each, in the style of the dwarf blocks at :2938-2986,
:3090-3122, :3215-3260 and :3387-3435. Requirements:

- `psyche_registry` and `society_registry`: **Wood's and High's rows must
  differ** — that is the entire content of §3.6, and
  `wood_and_high_differ_in_mind_and_not_in_body` asserts it in both directions.
- `dispersion_registry`: one row per elf; human stays the widest overall on
  every axis (its own campaign's claim, not to be disturbed here).
- `perception_registry`: `sky_attention` is **celestial vs terrestrial
  attention, not aerialness** (`perception_lens.ambient = 1.5 - sky_attention`).
  Drow's `activity` and `night_vision` are where its cave adaptation is legible
  today — the perception consumers (the hue ladder in `pack_depths`, the
  exposure lens) read them even though the capacity model does not.
- No shipped kind's rows are touched. Moving an existing people's capacity in
  the same change that adds six new ones destroys the attribution of both
  (spec §7).

- [ ] **Step 6: Author articulation, lexicon and the elf proto**

In `domains/language/src/lib.rs`:

- `articulation_registry` (:275) — six rows appended after the dwarves
  (:415-470). **Each must diverge from the `elf` proto vector**; six identical
  daughters would be six names for one tongue, and P5's divergence clause would
  correctly read false.
- `lexicon_registry` (:483) — six rows appended after the dwarves (:591-631),
  same six keys in the same order (the `articulation.ids == lexicon.ids`
  invariant). Each kind's `top` rung should name what its
  `SocietyVector.status_basis` says earns standing, as the dwarf block does.
- `family_proto` (:647) — one row, `KindId("elf")`, appended after
  `KindId("dwarf")`:

```rust
            // THE RADIATION (C2d): proto-Elvish, ancestor of all six daughters
            // in `articulation_registry` — the roster's largest family, six
            // against goblinoid's three and dwarf's three. Mandatory the moment
            // the label is carried twice: `check_integrity` requires a proto for
            // every family label held by >= 2 kinds, and this one is held by six,
            // so this row lands in the same commit as the `family_of` rows
            // because it must.
            //
            // **A STAR, NOT A TREE** (spec §8, LANG-53). With no time-since-
            // split, all six daughters are equidistant from this vector. The
            // model can say six tongues descend from proto-Elvish; it cannot say
            // Drow split before Snow, and there is no field in which that
            // sentence could be written. The tree stays blocked on
            // LANG-split-time-from-history, with this family as its motivating
            // case.
            (
                KindId("elf"),
                ArticulationVector { /* authored; see the block comment */ },
            ),
```

- [ ] **Step 7: Author `family_of` and Drow's realm row**

Six rows in `family_of` (:3446), appended after the dwarves at :3502-3504:

```rust
        // THE RADIATION (C2d): six kinds, ONE label — the roster's largest
        // family, and the programme's last. `family_proto` in
        // `hornvale_language` carries the matching `KindId("elf")` row in this
        // same commit, because `check_integrity` requires one the moment a
        // label is carried by >= 2 kinds.
        (KindId("desert-elf"), "elf"),
        (KindId("drow"), "elf"),
        (KindId("high-elf"), "elf"),
        (KindId("sea-elf"), "elf"),
        (KindId("snow-elf"), "elf"),
        (KindId("wood-elf"), "elf"),
```

One row in `habitat_realm_registry` (:2052), appended after the xorn row and
before the closing Delvers comment:

```rust
        // THE RADIATION (C2d): the drow — the store's first PEOPLED occupant,
        // and the first row whose consumer is settlement placement rather than
        // a readout (The Range carried the gate to `per_species_capacity_at`).
        // This is Drow's ONLY authored separation from the surface elves, and
        // it is deliberately its only one: the trap is not authoring a
        // subterranean kind, it is distinguishing two kinds by DEPTH, which
        // nothing in the model can say. One cave kind needs only to differ from
        // the surface, and the gate does that measurably.
        (KindId("drow"), HabitatRealm::Subterranean),
```

- [ ] **Step 8: N1 — the stated null, asserted where the function lives**

`cascade_regime_of` is private to `windows/worldgen/src/lib.rs`, so N1 lives in
that file's in-module test module beside
`cascade_regime_of_matches_the_authored_regime_map` (:9316). Add:

```rust
    /// THE RADIATION (C2d), N1 — **longevity is silent in language drift, and
    /// pacing elves harder than dwarves changes nothing.**
    ///
    /// `cascade_regime_of` switches a `Settled` people onto the slow regime at
    /// `LIFESPAN_THRESHOLD_YEARS = 120.0` and is **binary** there. The dwarves
    /// already clear it with a wide margin (paced at 4.0, all three land near
    /// 270 y). So no elf-specific tongue-slowness may be attributed to elves —
    /// and a later reader looking at long-lived elves and slow-drifting elf
    /// tongues will otherwise connect them, because the connection is exactly
    /// the shape a finding has.
    ///
    /// Three clauses, and the third is what makes the first two mean anything:
    /// the elves are on the slow regime; raising the pacing factor further
    /// moves NOTHING; and dropping to pure allometry DOES move it — so the
    /// threshold is live and the null is a null rather than a dead branch.
    #[test]
    fn pacing_elves_harder_than_dwarves_changes_no_drift_regime() {
        let biosphere = hornvale_species::biosphere_registry();
        let slow = hornvale_language::CascadeRegime::new(1, 2);
        let mut checked = 0usize;
        for name in [
            "desert-elf",
            "drow",
            "high-elf",
            "sea-elf",
            "snow-elf",
            "wood-elf",
        ] {
            let bio = biosphere
                .get(&hornvale_kernel::KindId(name))
                .unwrap_or_else(|| panic!("{name} has a biosphere row"));

            assert_eq!(
                cascade_regime_of(bio),
                slow,
                "{name} is not on the slow drift regime; the authored pacing \
                 factor does not clear LIFESPAN_THRESHOLD_YEARS"
            );

            // Clause 2: harder pacing is a NO-OP. The regime is binary at the
            // threshold, so doubling the factor cannot move it.
            let mut faster = bio.clone();
            faster.schedule = hornvale_species::LifeSchedule::paced(8.0).unwrap();
            assert_eq!(
                cascade_regime_of(&faster),
                slow,
                "{name}: doubling the pacing factor moved the drift regime. \
                 `cascade_regime_of` is BINARY at 120 y — if this fires, the \
                 threshold model changed and N1 must be re-stated, not re-pinned"
            );

            // Clause 3 (anti-vacuity): the threshold is live. Pure allometry on
            // a ~45-70 kg endotherm reads ~69 y, well under 120.
            let mut unpaced = bio.clone();
            unpaced.schedule = hornvale_species::LifeSchedule::Allometric;
            assert_eq!(
                cascade_regime_of(&unpaced),
                hornvale_language::CascadeRegime::SETTLED,
                "{name}: dropping to pure allometry did NOT move the regime, so \
                 clauses 1 and 2 above are satisfied by a branch that never \
                 fires and prove nothing"
            );
            checked += 1;
        }
        assert_eq!(checked, 6, "all six elves must be checked");
    }
```

Also extend `cascade_regime_of_matches_the_authored_regime_map`'s own roster
list to include the six, so the authored regime map covers fifteen peoples.

- [ ] **Step 9: Run the admission battery and the integrity gate**

```bash
cargo test -p hornvale-worldgen --test radiation_admission
cargo test -p hornvale-worldgen --lib components
cargo test -p hornvale-worldgen --lib cascade_regime
cargo test -p hornvale-species --lib
cargo test -p hornvale-worldgen --test non_void_roster
```

Expected: all PASS. `non_void_roster` is the one with teeth for Drow — a kind
confined to the ~12 % of land holding an enterable cave must still be viable on
at least one cell across seeds 1/7/42/99, **with no allowlist** (there is none:
the assertion compares against `Vec::<&'static str>::new()`). If Drow appears in
the void list, the trait values need re-authoring. **Adding an allowlist entry
would be authoring the failure it exists to detect** (spec §9 flag 6).

- [ ] **Step 10: P1 — the rung-5 test**

```bash
for s in 42 7 1234; do
  cargo run -q -p hornvale -- new --seed "$s" --out ".superpowers/sdd/baselines/post-t2-$s.json"
done
shasum -a 256 .superpowers/sdd/baselines/post-t2-*.json
diff <(cut -d' ' -f1 .superpowers/sdd/baselines/post-t1.sha256) \
     <(shasum -a 256 .superpowers/sdd/baselines/post-t2-*.json | cut -d' ' -f1)
```

**Expected: the hashes DIFFER from Task 1 Step 7's.** That is P1, measured
against the post-concepts baseline so it is not confounded with Task 1.

**Falsifier: byte-identical.** If P1 falsifies, **STOP the campaign here.** Some
part of the assembly is not admitting the elves at all. The Range's commit 1
was byte-neutral on the shipped roster for exactly this reason — the bake
filters to `SocialForm::Settled` and both `Subterranean` kinds were fauna. Check
`social_form` first, then whether the bake's roster assembly reaches the new
kinds. Do not proceed to Task 3 on a falsified P1; nothing downstream would be
trustworthy.

- [ ] **Step 11: Regenerate, gate, commit**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ \
  docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
cargo run --manifest-path tools/type-audit/Cargo.toml -- report \
  > docs/audits/type-audit-report.md
cargo fmt
until ! pgrep -f "cargo-nextest|cargo nextest" >/dev/null; do sleep 30; done
make gate
```

Expect a **wide sweep of red pinned counts and rosters** here — this is the
campaign's leading risk (spec §9 flag 1), and the plan sequences it as a sweep
rather than discovering it. The Range reddened twenty tests across five crates
from **one** authored row; this commit adds six competitors. Work the failures
as a batch:

```bash
cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-radiation-t2.txt
grep -E "^\s+FAIL" /tmp/hv-radiation-t2.txt | sort
```

Run once, inspect many. For each red, decide explicitly whether it is a
**witness of the old world** (re-pin, in this commit, with the new number and a
note saying six peoples entered the contest) or a **claim that has become
false** (a finding — do not re-pin; write it up). Refreshing a fixture exposes
what it froze: re-pin a witness, never a claim.

Commit with explicit paths. The message carries the Step 10 hashes, the P1
verdict, and the count of re-pinned witnesses.

---

### Task 3: The six biome-affinity rows

**Files:**
- Modify: `domains/species/src/lib.rs:2176-2253` (`biome_affinity_registry`)
- Modify: `windows/worldgen/tests/warren_readout.rs` (extend the tripwire's
  scope note; add Drow's dormancy assertion)
- Test: `windows/worldgen/tests/radiation_affinity.rs` (create)

**Interfaces:**
- Consumes: `hornvale_species::BiomeAffinity { pub default: f64, pub by_biome: Vec<(&'static str, f64)> }`
  and `BiomeAffinity::factor(&self, biome_name: &str) -> f64`;
  `hornvale_climate::biome::ALL` and `Biome::name()`.
- Produces: six rows in `biome_affinity_registry()`, which
  `range_readout.rs::every_authored_affinity_row_is_well_formed` and
  `…::every_occupant_has_climate_curves_the_minimum_currently_discards` then
  cover automatically (both iterate the whole registry).

The 22 legal biome name strings, verbatim from `domains/climate/src/biome.rs:195-220`
— a misspelling is **silently inert**, because `factor` falls back to `default`
for an unknown key:

```
ice  tundra  taiga  temperate-grassland  shrubland  temperate-forest
temperate-rainforest  desert  savanna  tropical-seasonal-forest
tropical-rainforest  alpine  sea-ice  coral-reef  kelp-forest
hydrothermal-vent  hadal-trench  upwelling  epipelagic  mesopelagic
bathypelagic  abyssal
```

- [ ] **Step 1: Capture the no-affinity baseline BEFORE the rows exist**

This is the preregistration freeze. Run the Task 4 readout's tally helper
against the current tree — the elves exist (Task 2) but carry no affinity — and
record the numbers. They go into Task 4's test doc comments as the frozen
baseline, so a prediction fails on the prediction and not on its setup.

```bash
cargo test -p hornvale-worldgen --test radiation_admission -- --nocapture \
  2>&1 | tee .superpowers/sdd/baselines/t3-preconditions.txt
cargo run -q -p hornvale -- new --seed 42 --out /tmp/radiation-noaffinity-42.json
python3 - <<'PY' | tee .superpowers/sdd/baselines/t3-stronghold-shares.txt
import json, collections
w = json.load(open('/tmp/radiation-noaffinity-42.json'))
c = collections.Counter(
    f['object'] for f in w['ledger']['facts'] if f['predicate'] == 'peopled-by'
)
for k, v in sorted(c.items()):
    print(f"{k:<16} {v}")
PY
```

Record, per elf: settlement count, and the share of that elf's settlements
sited in the biomes its Step 3 row will name. **Do not skip this.** Once the
rows land there is no way back to this arm except by mutation, and a share
measured after unblinding is not a baseline.

- [ ] **Step 2: Write the failing shape test**

Create `windows/worldgen/tests/radiation_affinity.rs`:

```rust
//! THE RADIATION (C2d): the six elves' biome-affinity rows, checked for the
//! four SILENT failure modes before anything measures them.
//!
//! `range_readout.rs::every_authored_affinity_row_is_well_formed` already
//! enforces non-uniformity, key spelling, the `> 0.0` floor, the `<= 1.0`
//! ceiling and no duplicate keys, over EVERY row in the registry — so the six
//! elf rows inherit all of it. This file adds only what is specific to the
//! family: that all six are present, that Sea is confined to the shelf, and
//! that High's row is Wood's.

use hornvale_kernel::KindId;

const ELVES: [&str; 6] = [
    "desert-elf",
    "drow",
    "high-elf",
    "sea-elf",
    "snow-elf",
    "wood-elf",
];

/// **Sea takes the PRODUCTIVE SHALLOW BAND, not the whole ocean.**
///
/// The campaign's second premise-check and its correction. "The ocean is 2.7x
/// the land" (29,896 cells against 11,066) is a real number with the wrong
/// denominator: no elf gets all the land either. On the shelf band Sea gets
/// **1,425 cells** — larger than Desert's 241, smaller than Snow's 4,633 — and
/// the roster comes out balanced at a 19x spread with no outlier. The runaway
/// exists ONLY if Sea is authored to the whole ocean.
///
/// This test is the thing that keeps it from being. The deep classes must not
/// be lifted above the row's default; the four shelf classes must be, and
/// strictly.
#[test]
fn the_sea_elf_is_confined_to_the_shelf_band() {
    const SHELF: [&str; 4] = ["coral-reef", "kelp-forest", "upwelling", "epipelagic"];
    const DEEP: [&str; 5] = [
        "mesopelagic",
        "bathypelagic",
        "abyssal",
        "hadal-trench",
        "sea-ice",
    ];
    let registry = hornvale_species::biome_affinity_registry();
    let sea = registry
        .get(&KindId("sea-elf"))
        .expect("sea-elf carries a biome affinity row");

    for b in SHELF {
        assert!(
            sea.factor(b) > sea.default,
            "sea-elf's {b} factor ({}) is not above its default ({}); the \
             shelf band must be a STRONGHOLD, and a factor at the default is \
             indistinguishable from silence",
            sea.factor(b),
            sea.default
        );
    }
    for b in DEEP {
        assert!(
            sea.factor(b) <= sea.default,
            "sea-elf's {b} factor ({}) is above its default ({}). A SETTLED \
             people needs shallow productive water — which is also why human \
             settlement is coastal — and `marine_forage_supply_field` already \
             grades the water that way (Upwelling 1.0, reef/kelp 0.85, \
             epipelagic 0.45, mesopelagic 0.15, bathypelagic 0.05). The \
             affinity SHARPENS that ranking; it must not contradict it, and it \
             must not open 25,640 cells of deep ocean.",
            sea.factor(b),
            sea.default
        );
    }
}

/// High's row is Wood's row, exactly. §3.6's contrast — Wood vs High isolates
/// MIND — is only single-variable if their environment is identical, and the
/// affinity is the loudest environmental channel either of them has.
#[test]
fn high_elfs_affinity_is_wood_elfs() {
    let registry = hornvale_species::biome_affinity_registry();
    let wood = registry.get(&KindId("wood-elf")).expect("wood-elf row");
    let high = registry.get(&KindId("high-elf")).expect("high-elf row");
    assert_eq!(
        wood, high,
        "high-elf's affinity differs from wood-elf's. High diverges in \
         psyche, society and language ONLY (spec §3.6, §4); an environmental \
         divergence makes P3 a two-variable comparison and High stops being a \
         control."
    );
}

/// Drow's row is Wood's row too — its authored separation is the realm gate
/// and nothing else, so that P4's mutation (remove the realm row, watch the
/// separation vanish) isolates the gate rather than the gate plus a biome
/// difference.
#[test]
fn drows_affinity_is_wood_elfs() {
    let registry = hornvale_species::biome_affinity_registry();
    let wood = registry.get(&KindId("wood-elf")).expect("wood-elf row");
    let drow = registry.get(&KindId("drow")).expect("drow row");
    assert_eq!(
        wood, drow,
        "drow's affinity differs from wood-elf's. Its only authored \
         separation from the surface elves is the REALM GATE (spec §3.5); a \
         second difference would make P4's mutation uninterpretable."
    );
}

/// All six carry a row. Spec §3.7's parenthetical says "five of six" and is
/// treated as an erratum: P3(a)'s primary arm needs High to carry Wood's row
/// (bit-identical fields are impossible otherwise) and P4's wording — the
/// separation vanishes "up to Drow's own biome and curve authoring" —
/// presupposes Drow has biome authoring.
#[test]
fn all_six_elves_carry_an_affinity_row() {
    let registry = hornvale_species::biome_affinity_registry();
    for name in ELVES {
        assert!(
            registry.contains(&KindId(name)),
            "{name} has no biome-affinity row; all six are authored on the \
             affinity route (spec §3.1)"
        );
    }
}
```

- [ ] **Step 3: Run to verify failure**

```bash
cargo test -p hornvale-worldgen --test radiation_affinity
```

Expected: all four FAIL with `"sea-elf carries a biome affinity row"` /
`"wood-elf row"` panics — the rows do not exist.

- [ ] **Step 4: Author the six rows**

In `domains/species/src/lib.rs`'s `biome_affinity_registry`, appended after
`woolly-mammoth` (:2249). Author each on **the ladder the registry doc already
fixes**, so the six are read against one another rather than each tuned by eye:

```
  1.00  stronghold  the biome `classify_land` returns for the kind's OWN
                    authored (temperature, moisture) reading
  0.70  near        one band out, still recognisably the kind's country
  0.45  marginal    two bands out, or the right climate in the wrong form
  0.25  default     everything else
```

Constraints, each with the test that catches a violation:

| constraint | caught by |
| --- | --- |
| non-uniform across the 22-biome catalog, resolved through `factor` | `range_readout::every_authored_affinity_row_is_well_formed` |
| every `by_biome` key is a real biome name | same |
| every factor `> 0.0` (no hard exclusions) and `<= 1.0` (mask, not boost) | same, both `by_biome` and `default` |
| no biome listed twice (`factor` returns the FIRST match) | same |
| `elevation.devotion < sovereignty_floor` for each | `radiation_admission::every_elf_clears_the_affinity_precondition` and `range_readout::every_occupant_has_climate_curves_the_minimum_currently_discards` |
| Sea on the shelf only | `radiation_affinity::the_sea_elf_is_confined_to_the_shelf_band` |
| High's row == Wood's, Drow's row == Wood's | `radiation_affinity::high_elfs_affinity_is_wood_elfs`, `…::drows_affinity_is_wood_elfs` |

Each row's doc comment must **derive** its stronghold rather than theme it: name
the biome `classify_land` returns at the kind's own authored (temperature,
moisture) reading, as gnoll's row does (`29.0 °C, moisture 0.12 → Desert`).
Update the registry's own doc block (:2103-2175) — the two-occupant sentence at
:2104-2105, and the admission table at :2139-2143, which must gain six rows with
live-computed floors.

- [ ] **Step 5: Author Drow's dormancy tripwire**

Drow's dark adaptation is authored, **dormant, and preregistered as dormant**.
The Warren measured that going underground improves a kind's moisture
(.585 → .787) and insolation (.467 → .840) readings and that the Liebig minimum
never sees the improvement, because the unfloored elevation axis is scarcer.
Generalised: *a non-lethal preference cannot matter while an unfloored axis is
scarcer.*

`windows/worldgen/tests/warren_readout.rs` already holds the tripwire — a
deliberately inverted assertion pinning `rm_ratio == 1.000` and
`xorn_ratio == 1.000` that goes RED the day The Tense's two-tier tolerance
starts to bind. Extend it to Drow, in the same file, inside
`the_blast_radius_readout` (which is `#[ignore = "heavy: …"]`, so it runs under
`make gate-full`, not the commit gate):

- compute Drow's before/after mean the same way rust-monster's and xorn's are
  computed (the only difference between the arms is
  `wc_before.habitat_realm = ComponentStore::new()`), and
- assert `(drow_ratio - 1.0).abs() < 1e-9` with a message that names The
  Radiation, says the dark-adaptation authoring is expected to contribute
  exactly nothing today, and instructs the reader **not to relax it** — a
  moved ratio means the two-tier tolerance came alive and Drow's authored
  insolation preference just started to bind, which is a finding requiring a
  re-measurement of §4 and §5's P4 companion null, not a nudge.

Extend the file's header comment to say the tripwire now covers a **peopled**
kind, which is new: every prior occupant was fauna.

- [ ] **Step 6: Run the shape tests and the inherited guards**

```bash
cargo test -p hornvale-worldgen --test radiation_affinity --test range_readout
cargo test -p hornvale-worldgen --test non_void_roster
```

Expected: PASS. If `every_authored_affinity_row_is_well_formed` fires, read the
message — it names the exact silent failure mode (misspelled key, duplicate
key, zero, above-ceiling, uniform) and each of those would otherwise have
produced a row that compiled, ran, and did nothing.

- [ ] **Step 7: Prove the guards discriminate**

For each of the four new tests, make the break, capture the red, revert:

```bash
# 1. Sea into the deep: set ("bathypelagic", 1.00) on sea-elf's row.
#    Expect: the_sea_elf_is_confined_to_the_shelf_band FAILS.
# 2. High diverges: change one factor on high-elf's row.
#    Expect: high_elfs_affinity_is_wood_elfs FAILS.
# 3. Drow diverges: change one factor on drow's row.
#    Expect: drows_affinity_is_wood_elfs FAILS.
# 4. Drop snow-elf's row entirely.
#    Expect: all_six_elves_carry_an_affinity_row FAILS.
cargo test -p hornvale-worldgen --test radiation_affinity 2>&1 | tail -40
```

Paste the four red messages into the commit message. A mutation proves only
what it perturbs, so run all four — not one as a representative.

- [ ] **Step 8: Regenerate, gate, commit**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ \
  docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
cargo fmt
until ! pgrep -f "cargo-nextest|cargo nextest" >/dev/null; do sleep 30; done
make gate
```

Expect a second sweep of red pinned counts — six authored rows redistribute the
placement again. Same discipline as Task 2 Step 11: witness vs claim, decided
explicitly per failure.

Also regenerate the occupancy readout, which is **not** part of `make
rebaseline` and is written by a hand-run test:

```bash
cargo test -p hornvale-worldgen --test occupancy_readout \
  regenerate_occupancy_readout -- --ignored
git diff --stat windows/worldgen/tests/fixtures/occupancy.csv
```

It grows by six kinds and its drift check (`occupancy_readout_is_current`,
`#[ignore = "heavy: …"]`) must be rewritten **in the same commit as the change
that drifted it**. The readout's own doc table (:254-258, the EC6 region
summary) should be re-read against the new fixture and corrected if six new
occupants changed a region's top occupant.

Commit with explicit paths.

---

### Task 4: The rung-5 step and the placement readouts

The campaign's measurement task. Five predictions, each reported against its
falsifier, each per elf and never pooled — pooling six kinds would hide exactly
the per-kind result §4 exists to make legible.

**Reproduce §4's visibility table at the top of the file**, before any test, so
a reader meets it before meeting High's empty result:

```
  Wood / Desert / Snow / Sea   PLACEMENT. The biome affinity moves where they
                               live; the settlement distribution is the probe.
  Drow                         PLACEMENT, via the REALM GATE (~1,667 cells).
                               Its dark-adaptation authoring is DORMANT and
                               contributes nothing — by measurement, not by
                               omission. Guarded by the Warren tripwire.
  High                         NOT PLACEMENT, by design. Its identity lives in
                               psyche, society and language facts only. A
                               placement readout is the wrong instrument for it
                               and will correctly show nothing.
```

Written down in advance, High is a control. Discovered afterwards, High is a
failed elf and Drow's dormant half is a bug. The difference between those two
readings is this table and its date.

**Files:**
- Test: `windows/worldgen/tests/radiation_readout.rs` (create)

**Interfaces:**
- Consumes: `hornvale_worldgen::{WorldComponents, build_world, per_species_capacity,
  terrain_of, climate_of, sky_of, SkyChoice, SettlementPins}`;
  `hornvale_species::{BiomeAffinity, HabitatRealm}`.
- Produces: nothing consumed by later tasks. Its output is the chronicle's
  evidence.

**Scaffolding — copy the shape from `range_readout.rs`, which already solved
every part of this.** Specifically: `SEEDS` (:190, `[42, 7, 1234]` — The
Delvers' set, so the numbers are comparable with two published tables), the
`enum Arm` / `fn components(arm: Arm) -> WorldComponents` pattern (:195, :222),
`fn tally` (:276 — **`BuildDepth::Full` is required**; `Settlements` depth
commits no `peopled-by` fact), `fn placement` (:335), and `fn pearson` (:376,
with its stated scope limit: Pearson is scale-invariant, so it measures how the
fields *sort* cells, not how large they are).

The arms this campaign needs, as an `enum`:

```rust
/// Which authored rows the arm's `WorldComponents` carries. Every arm is built
/// by REMOVING rows from the canonical registries, never by re-authoring them —
/// so an arm can never disagree with the shipped roster about anything except
/// the one thing it is defined to remove.
#[derive(Clone, Copy, Debug)]
enum Arm {
    /// The shipped roster, exactly as `assemble()` returns it.
    Shipped,
    /// Every elf's affinity row removed; the elves still compete.
    NoElfAffinity,
    /// One named elf removed from the roster entirely (P1').
    WithoutElf(&'static str),
    /// Drow's `HabitatRealm::Subterranean` row removed (P4).
    NoDrowRealm,
}
```

- [ ] **Step 1: P1′ — the per-elf rung-5 test**

For each of the six, build the committed world with that elf's rows present and
with them removed, on all three seeds, and compare the sha256 of the serialized
world (or, equivalently and more cheaply, the full sorted `(species, cell)`
placement list plus the fact count).

**Prediction:** *each* elf's presence changes the committed world on at least
one of three seeds.

**Falsifier:** some elf is byte-neutral in both directions — that kind is
authored and inert, rung 2 on that kind, and it is **a finding about that kind
rather than about the mechanism**. Report it; do not retune to rescue it.

This is where the roster's two riskiest kinds are actually tested. Drow is
confined to the ~12 % of land holding an enterable cave and must clear the
roster floor with no allowlist. Sea competes on water nobody has ever settled.

- [ ] **Step 2: P2 — each elf concentrates in its authored biomes**

**Axis:** the **share** of that elf's settlements sited in its authored
stronghold biomes, `Arm::Shipped` against `Arm::NoElfAffinity`, on three seeds.
Freeze Task 3 Step 1's baseline shares in the test's doc comment as the
preregistration, with the date.

**Falsifier:** the share is flat or falling **while the count also falls**, on a
majority of seeds.

The share is preregistered rather than the count, following The Range's P1″: a
falling count with a **rising** share is *success* (relocation), and both
falling is the failure mode. Report per elf, never pooled.

**High is exempt by design** (§4). It has no stronghold of its own — its row is
Wood's — and is predicted to show no concentration relative to Wood. That is not
a falsification of P2, and the test must say so in its own assertion message so
a later reader does not "fix" it.

**A pre-committed diagnosis, so a falsification is a finding and not a retune.**
If P2 falsifies for an elf whose habitat is small and contested, the diagnosis
is The Range's P1‴ — a downward-only mask can suppress without relocating — and
the repair is an affinity permitted **above 1.0**, or complementary occupants
vacating the destination. **Not a retuned constant.** The Range pre-committed
that repair and did not need it; if this campaign needs it, that is the first
evidence for it and it is recorded as such, in the chronicle, as a decision.

- [ ] **Step 3: P3 — Wood and High do NOT separate (the null control)**

Two axes, because they can disagree.

**(a) Capacity fields.** Wood's and High's per-cell capacity fields over land,
compared with `to_bits()` equality. **Prediction:** bit-identical.

**And an honest limit, stated before measurement:** if they come out
bit-identical, (a) is a **wiring check with no information in it**. Say so in
the test's doc comment. The information is in (b).

If their fields are *not* bit-identical, the prediction degrades to: they are
the highest-correlating pair in the entire roster under the pairwise Pearson
instrument The Range used. Report which authored field carried the difference
(mass through the sovereignty floor, dispersion, society feeding back through
the contest) — that is the result, not a failure.

**(b) Placements.** The `(people, cell)` settlement lists. The bake's contest is
**not a pure function of the capacity field** — iteration order, tie-breaks,
migration and the raid comparison all participate — so two kinds with identical
fields *can* still place differently. If they do, that is a finding about the
contest, and the campaign reports it as such rather than as a fact about elves.

- [ ] **Step 4: P4 — Drow separates by the realm gate alone**

Two clauses and two falsifiers.

1. With `HabitatRealm::Subterranean` present: the share of Drow settlements on
   cells holding an enterable cave is **1.00** (the gate is a hard zero
   elsewhere), and Drow's capacity field is separated from Wood's.
2. With the row removed (`Arm::NoDrowRealm`): that separation **disappears**, up
   to Drow's own biome and curve authoring — which Task 3 pinned equal to
   Wood's, so "up to" should here mean "entirely".

**Falsifier A:** Drow's field is not separated from Wood's *with* the row — the
gate did not reach identity after all, contradicting The Range's repair.
**Falsifier B:** it is *still* separated *without* the row — something other
than the gate is doing the work, and §4's attribution is wrong.

**The companion null, preregistered:** Drow's dark-adaptation authoring
contributes **zero** to its placement. Perturb Drow's `insolation.devotion`
alone (in a locally-mutated `WorldComponents`, not in the registry) and assert
the committed world is byte-identical. **Falsifier:** it moves — which means
the two-tier tolerance has started to bind, and the Warren tripwire (Task 3
Step 5) should have reddened first. If this fires while the tripwire is green,
the tripwire is broken and that is the first thing to fix.

- [ ] **Step 5: Cost the battery and tier it**

These arms rebuild worlds; the battery is minutes, not seconds. Tier it out of
the commit gate with the **verbatim** heavy-tier token — `cli/tests/heavy_tier.rs`
matches the reason string exactly:

```rust
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
```

Any test in this file that does *not* build a world (a shape check) stays in the
gate. Do not tier a cheap test out; do not leave an expensive one in.

- [ ] **Step 6: Run the battery, record whatever it says**

```bash
cargo test -p hornvale-worldgen --test radiation_readout -- --ignored --nocapture \
  2>&1 | tee .superpowers/sdd/baselines/t4-readout.txt
```

**Do not retune.** A falsified prediction is a finding; several campaigns have
shipped the null as the headline. Copy the full output into the chronicle,
per elf, with the falsifier restated beside each number.

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt
until ! pgrep -f "cargo-nextest|cargo nextest" >/dev/null; do sleep 30; done
make gate
```

This task authors no world change, so no rebaseline is expected — **but diff
the seven paths anyway**, because an absorption may have landed since Task 3.

---

### Task 5: The language products

**Files:**
- Modify: `windows/lab/src/metrics.rs` — `ELF_DAUGHTERS` beside
  `GOBLINOID_DAUGHTERS` (:6912) and `DWARF_DAUGHTERS` (:6917); a match arm in
  `family_daughters_of` (:6922); a `monophyly-elf` metric beside
  `monophyly-dwarf` (:3287-3303); `divergence_real` (:7198) generalised
- Test: `windows/worldgen/tests/radiation_language.rs` (create)

**Interfaces:**
- Consumes: `hornvale_worldgen::{proto_phonology_of, family_daughter_kinds}`;
  the lab's `FullView`, `MetricValue`, `lex`, `in_roster`, `root_concepts`.
- Produces: the census metric `monophyly-elf`.

**The blast radius, stated before the edit.** Nine studies use
`"metrics": "all"` (`the-census`, `census-of-the-meeting`, `census-of-eyes`,
`-faiths`, `-lands`, `-peoples`, `-tongues`, `-words`, `the-pyx-probe`), so
**every new metric adds a column to all nine** and reddens roughly 34 tests
until both census fixtures refresh. Add the **minimum** number of metrics that
carries P5, and measure the rest in the probe test below where it costs one
column of nothing.

- [ ] **Step 1: Add `ELF_DAUGHTERS` and the `monophyly-elf` metric**

In `windows/lab/src/metrics.rs`, after `DWARF_DAUGHTERS` (:6917):

```rust
/// THE RADIATION (C2d): the elf family's six daughters — the roster's third
/// and largest multi-member family, and `monophyly-elf`'s subject. Same shape,
/// same rationale and the same drift guard as [`GOBLINOID_DAUGHTERS`]: an
/// authored MEMBERSHIP CLAIM, deliberately not derived from
/// [`hornvale_worldgen::family_daughter_kinds`], because a derived list changes
/// value on the Lab's synthetic rosters and moving a null control's value is a
/// deliberate act rather than a refactor.
const ELF_DAUGHTERS: [&str; 6] = [
    "desert-elf",
    "drow",
    "high-elf",
    "sea-elf",
    "snow-elf",
    "wood-elf",
];
```

and the match arm in `family_daughters_of` (:6922):

```rust
        "elf" => &ELF_DAUGHTERS,
```

and the metric, after `monophyly-dwarf` (:3303):

```rust
        Metric {
            // THE RADIATION (C2d): the roster's third multi-member family and
            // the first with six daughters — the family the language machinery
            // has been waiting for. Same check, not a third implementation.
            name: "monophyly-elf",
            doc: "Whether every elf daughter's (desert-elf, drow, high-elf, sea-elf, \
                   snow-elf, wood-elf) Root derivation.proto matches an INDEPENDENT \
                   re-draw of the shared \"elf\" family proto-root for that concept \
                   (spec §3: cognates share a proto ancestor) — never reading the \
                   family proto back from a sibling's own recorded derivation; Absent \
                   if no elf daughter minted a Root",
            summary: SummaryKind::Flag,
            domain: Domain::Language,
            role: Role::Invariant,
            extract: Extractor::Full(|v: &FullView| monophyly(v, "elf")),
        },
```

The drift guard `authored_daughter_lists_match_the_default_rosters_family_membership`
(metrics.rs test module) will fail if `ELF_DAUGHTERS` disagrees with
`family_daughter_kinds(wc, "elf")` — that is the intended coupling, and it is
what stops the constant going stale in silence.

- [ ] **Step 2: Generalise `divergence_real` past goblinoid**

`divergence_real` (:7198) is hardcoded to `GOBLINOID_DAUGHTERS` — it is the
seed-swept stemmatics guard, and **it does not generalise on its own**.
Refactor it to take a family, exactly as `monophyly` already does:

```rust
fn divergence_real_for(v: &FullView, family: &str) -> MetricValue {
    let daughters = family_daughters_of(family);
    if !daughters.iter().all(|s| in_roster(v, s)) {
        return MetricValue::Absent;
    }
    let lexes: Vec<hornvale_language::Lexicon> =
        daughters.iter().filter_map(|s| lex(v, s).ok()).collect();
    if lexes.len() < daughters.len() {
        return MetricValue::Absent;
    }
    let Some((first, rest)) = lexes.split_first() else {
        return MetricValue::Absent;
    };
    let shared: Vec<&str> = root_concepts(first)
        .into_iter()
        .filter(|c| rest.iter().all(|lex| root_concepts(lex).contains(c)))
        .collect();
    if shared.is_empty() {
        return MetricValue::Absent;
    }
    let diverges = shared.iter().any(|c| {
        let forms: Vec<&[Segment]> = lexes
            .iter()
            .map(|lex| match lex.entry(c) {
                Some(LexEntry::Root { derivation, .. }) => derivation.modern.as_slice(),
                _ => unreachable!("{c} confirmed rooted in every daughter above"),
            })
            .collect();
        !forms.windows(2).all(|w| w[0] == w[1])
    });
    MetricValue::Flag(diverges)
}
```

Keep `divergence-real`'s **name and doc string byte-identical** — a metric's
name and doc are a published contract, and renaming it would drift every
census column header for no gain. Point its extractor at
`divergence_real_for(v, "goblinoid")`. Do **not** register a
`divergence-real-elf` metric; the six-daughter divergence is measured in Step 3's
probe, where it costs no census column.

- [ ] **Step 3: Write the P5 probe**

Create `windows/worldgen/tests/radiation_language.rs`, tiered
`#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]`.
Four clauses, from spec §5's P5:

1. **`monophyly-elf`.** Every elf daughter's `Root.derivation.proto` matches an
   **independent re-draw** of the shared `elf` family proto-root for that
   concept — never read back from a sibling's own recorded derivation.
   *Falsifier:* any daughter mismatches, meaning the proto is being sourced from
   a sibling and the family is not monophyletic in the way the metric claims.
2. **Divergence is real at six.** Some concept rooted in **all six** daughters
   has ≥ 2 distinct present-day forms. *Falsifier:* all six coincide on every
   commonly-rooted concept. A null here is strong precisely because six draws
   have more room to differ than three — descent is proven by shared
   *innovations*, not by a shared ancestor alone, and a family of six silent
   aliases must read false.
3. **Homophony does not leak the sibling count.** Per-daughter homophony counts
   (and the core / confusable subsets) for the six elf daughters are **not
   systematically above** those of the two three-daughter families. *Falsifier:*
   they are — a defect in the metric or in the draw, since homophony is a
   *within-daughter* property and the number of siblings should not enter it.
   Compute these in the probe by calling the lab's homophony functions per
   species; do **not** register 24 new census metrics for it (see Step 4).
4. **Name transparency and blind attribution stay in band.** Census-wide, blind
   attribution stays above its **0.75** floor, and name transparency's span does
   not collapse from below. *Falsifier:* either moves outside its band — a
   language-wide product moved under roster growth, and it must be **attributed
   before the campaign closes rather than re-pinned**. This clause is read off
   Task 6's census diff, not computed here; state that in the file header so it
   is not mistaken for unmeasured.

- [ ] **Step 4: Record what is NOT measured, and where**

The spec's DoD says: *"The elf-family language metrics extended past
`GOBLINOID_DAUGHTERS` / `ALL_DAUGHTERS`, or the campaign states in the chronicle
that elf was not measured on them."* Take the second branch deliberately, for
two of the three surfaces, and write these three sentences into the chronicle:

- **`ALL_DAUGHTERS` no longer exists.** The Delvers replaced it with the derived
  `fn all_daughters(v: &FullView)` (metrics.rs:6947), which reads
  `v.components().lexicon.ids()` — so `lexicon-regular-family` **already covers
  every elf daughter** with no edit at all. The spec's reference is to a
  constant that was retired before this campaign began.
- **The per-daughter `inventory-closure-*` and homophony metric families are
  frozen at four kinds** (`goblin`, `hobgoblin`, `bugbear`, `kobold`). The
  dwarves never got them either, so P5's homophony clause **cannot compare elf
  against both three-daughter families as census columns** — dwarf has none.
  Extending them to nine kinds would add 24+ metrics × nine `metrics: "all"`
  studies. Measured in the Step 3 probe instead, and stated here as a
  deliberate scope call.
- **`divergence-real` stays goblinoid-scoped as a census column** and the
  six-daughter divergence is a probe result, for the same column-count reason.

- [ ] **Step 5: Run, gate, commit**

```bash
cargo test -p hornvale-lab --lib metrics
cargo test -p hornvale-worldgen --test radiation_language -- --ignored --nocapture
cargo run -q -p hornvale -- lab list-metrics | grep -c .
cargo fmt
until ! pgrep -f "cargo-nextest|cargo nextest" >/dev/null; do sleep 30; done
make gate
```

`monophyly-elf` adds one column to nine studies. Expect the lab's metric-count
pins and both census fixtures to redden; the census refresh is Task 6 and the
gate **cannot** be green before it. Re-pin the metric-count assertions here,
in this commit; leave the census fixtures red and say so in the commit message.

---

### Task 6: Census, artifacts, and the close

**This campaign pays BOTH census fixtures** — the metaplan budgeted a roster
campaign at two wholesale rewrites, The Range paid one, and this is the case
where both move:

- **`the-census`** refreshes wholesale, 1000 of 1000 rows, because a new
  settling people re-decides settlement placement on every seed. Six of them
  certainly will.
- **`census-of-the-meeting`** is structurally near-immune to competition — its
  rosters are `goblin-solo` and `goblin-twin-solo`, so a new kind never competes
  in it — but it rewrites **every row textually on a column change**, and
  `monophyly-elf` is a column change. So it moves too.

- [ ] **Step 1: Absorb main, regenerate everything, diff all seven paths**

```bash
make preflight
make rebaseline
cargo run --manifest-path tools/type-audit/Cargo.toml -- report \
  > docs/audits/type-audit-report.md
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ \
  docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

`clients/game/core/tests/fixtures/` is the newest path and the one this
campaign is guaranteed to drift: those are committed seed-42 session snapshots
that most `hornvale-game-core` tests read instead of paying for genesis, and
**six new peoples move the world those snapshots record**. The precedent is
already on main — The Range's world change forced `b16005b7` (re-pin the session
fixtures) and `86bb7244` (the most-populous settlement is Nenagabo now, not Toa)
as follow-ups by another campaign. Do the sweep here.

- [ ] **Step 2: Census regen on lefford — ASK FIRST**

Authorization-gated (decisions 0079 / 0081 / 0086). **Do not run without an
explicit go.** Never here: `census-run.sh` fails closed on the hostname.

```bash
git push origin the-radiation
git rev-parse HEAD                       # a FULL SHA, never a branch name
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_WORKTREE=canonical \
  HV_CENSUS_REF=<full-sha> scripts/census-run.sh'
```

Budget ~15 min per run. Commit the regenerated goldens **on lefford** — the
canonical box authors them — then push and fast-forward locally. On lefford the
regeneration worktree is **shared**: ask before reusing it, verify its HEAD, and
sweep orphans rather than assuming it is parked where you left it.

```bash
make lab-diff STUDY=the-census      # which metrics moved vs HEAD — the review surface
make census-check                   # analysis harness (needs duckdb + python3)
```

**Prove the regen is additive where it should be.** Every CSV line will read as
"changed" because a column was added; that is not evidence physics moved. Diff
the **shared columns** to separate the two, and report P5 clause 4 (blind
attribution above 0.75, name transparency's span not collapsing) off that diff.
`book/src/domesday/` is a pure read over the committed census and drifts with
it, with no other code change at all.

- [ ] **Step 3: The heavy tier, dispatched to lefford**

```bash
make heavy-remote REF=<full-sha>
```

**Two tests are red at main** for written, measured, non-Radiation reasons
(`scene_cost`, `session_cost`). A **third** heavy failure during this campaign
is this campaign's. The heavy tier is an *authoring* path — three of its tests
write committed artifacts — which is why it carries the same canonical-host
guard a census does.

- [ ] **Step 4: Chronicle**

`book/src/chronicle/the-radiation.md`. It must carry:

- **§4's per-elf visibility table, reproduced verbatim**, so a later reader
  meets it before meeting High's empty placement result.
- **Every prediction in §5 reported against its falsifier, including the
  nulls** — P1, P1′, P2 (per elf, never pooled), P3(a) and P3(b), P4 and its
  companion null, P5's four clauses, and N1. Ship the null as the headline if
  that is what the measurement says.
- **The corrections this campaign owes.** §3.7's "biome_affinity (sparse, five
  of six)" is an erratum — six rows shipped, and the reason (P3(a) and P4 both
  require it). The metaplan's roster table reads *five peoples become seventeen*
  with dwarf ×5; The Delvers shipped dwarf ×3, so the shipped outcome is
  **fifteen peoples**, with Mountain-dwarf and Duergar owed to the campaign that
  gives the underworld biomes.
- **The three not-measured statements from Task 5 Step 4.**
- **The prohibition, honoured.** No sentence of the form "the Drow tongue is
  harsh *because* the Underdark is". Any correlation between articulation and
  environment measures the authoring convention, not the world; the project has
  shipped that error once (duergar's authored 300 m optimum returning as an
  emergent toponymic finding) and one question dissolved it.
- **The programme closes here.**

- [ ] **Step 5: Retrospective and the book freshness sweep**

`docs/retrospectives/the-radiation.md` — process lessons, not product.
**Promote `.superpowers/sdd/decision-ledger.md`'s nine entries into it BEFORE
the worktree is torn down**; the ledger is git-ignored and dies with the
worktree. Two entries earn their place: #6 (a real number with the wrong
denominator reads exactly like a finding — the ocean-scale correction) and #2
(the Sea-elf premise-check that a single measurement settled after The Range
spent an hour on a confident causal story).

Book freshness sweep: every chapter this campaign made stale. Re-score the
Confidence Gradient (`book/src/open-questions.md`) if this moves one of its
bets — a fifteen-peoples roster and a six-daughter family plausibly do.

- [ ] **Step 6: Frontier bookkeeping**

Grep `book/src/frontier/idea-registry.md` before editing; a row's status is
self-reported and IDs are permanent.

- `LANG-53` → **`partial`**, with the star delivered and the tree blocked on
  `LANG-split-time-from-history` (which gains this six-daughter family as its
  motivating case). Its **Where** cell carries this spec and the deferred half.
- `BIO-elf-radiation` — unchanged, re-affirmed as blocked.
- `BIO-kind-authoring-seam` — re-checked against what six kinds actually cost.
- `PROC-readout-is-not-identity` — reviewed for a status flip now that rung 5
  has been exercised by a campaign that did not invent it.
- `LANG-split-time-from-history` — **Where** cell gains this family.

Do not cite registry IDs outside `book/src/frontier/`; `cli/tests/docs_consistency.rs`
enforces that, and DoD prose leaking a registry ID is a known way to fail it.

- [ ] **Step 7: Final gate and close**

```bash
make preflight
cargo fmt
until ! pgrep -f "cargo-nextest|cargo nextest" >/dev/null; do sleep 30; done
make gate
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ \
  docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

A regen-after-fix needs a full re-gate; do not close on a gate that predates the
last artifact write. Then use `superpowers:finishing-a-development-branch` and
the `closing-a-campaign` skill.

---

## Self-review

**Spec coverage.** §1–§2 (premise-checks) → Task 3 Step 2's Sea shelf test and
its doc comment carry the measured correction. §3.1 (one route) → Task 2 Step 1's
`every_elf_clears_the_affinity_precondition`. §3.2 (vocabulary) → Task 3's
22-name list and the inherited well-formedness guard. §3.3 (the six) → the
roster table. §3.4 (Sea's shelf) → Task 3 Step 2. §3.5 (Drow) → Task 2 Step 1's
two Drow tests plus Task 3 Step 5's tripwire. §3.6 (High) → Task 2 Step 1 and
Task 3 Step 2, both directions. §3.7 (authoring cost) → Task 2's seven-point
checklist. §4 (visibility) → Task 4's file header and the chronicle. §5 P1 →
Task 2 Step 10; P1′/P2/P3/P4 → Task 4; P5 → Task 5; N1 → Task 2 Step 8. §6
(what is not measured) → Global Constraints and Task 6 Step 4. §7 (non-goals) →
no task touches them. §8 (LANG-53) → Task 2 Step 6's proto comment and Task 6
Step 6. §9 (costs) → the sweep discipline in Tasks 2/3 and Task 6. §10 (DoD) →
Task 6.

**Two spec items this plan could not turn into a step as written**, both
recorded above rather than silently resolved: §3.7's "five of six" affinity
count (corrected to six, with the P3/P4 argument), and §5/§10's
`ALL_DAUGHTERS` (a constant that no longer exists; the live surface already
covers elf, and the frozen per-daughter homophony families cannot carry P5's
homophony clause as census columns).

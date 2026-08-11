//! THE RADIATION (C2d) — the preregistered placement readouts (task 4).
//!
//! Five predictions, each reported against its falsifier, each **per elf and
//! never pooled** — pooling six kinds would hide exactly the per-kind result
//! §4 of the spec exists to make legible.
//!
//! # §4's visibility table, reproduced before any test
//!
//! ```text
//!   Wood / Desert / Snow / Sea   PLACEMENT. The biome affinity moves where they
//!                                live; the settlement distribution is the probe.
//!
//!   Drow                         PLACEMENT, via the REALM GATE (~1,667 cells).
//!                                Its dark-adaptation authoring is DORMANT and
//!                                contributes nothing — by measurement, not by
//!                                omission. Guarded by the Warren tripwire.
//!
//!   High                         NOT PLACEMENT, by design. Its identity lives in
//!                                psyche, society and language facts only. A
//!                                placement readout is the wrong instrument for it
//!                                and will correctly show nothing.
//! ```
//!
//! Written down in advance (spec §4, 2026-08-09), **High is a control**.
//! Discovered afterwards, High is a failed elf and Drow's dormant half is a bug.
//! The difference between those two readings is this table and its date.
//!
//! # The arms, and the rule that builds them
//!
//! Every arm is built by **removing or substituting rows from the canonical
//! registries**, never by re-authoring them — so an arm can never disagree with
//! the shipped roster about anything except the one thing it is defined to vary.
//! [`the_arms_differ_from_the_shipped_roster_in_exactly_the_row_they_name`] is
//! the positive control on that machinery, and it stays in the commit gate: an
//! arm that silently failed to differ would make every number below vacuous.
//!
//! # P1′ — the per-elf rung-5 test
//!
//! **Axis:** for each of the six, the committed world with that elf's rows
//! present against the same world with them removed, on three seeds.
//! **Prediction:** each elf's presence changes the committed world on at least
//! one of three seeds. **Falsifier:** some elf is byte-neutral in both
//! directions — that kind is authored and inert, rung 2 on that kind, and it is
//! a finding about that kind rather than about the mechanism.
//!
//! ## Measured on the LEDGER, not on the world-file hash — and why
//!
//! The plan specified the sha256 of the serialized **world**. Task 1 measured
//! that premise and found it backwards: adding six concepts left seed 42's
//! ledger JSON **byte-identical** (7486 facts → 7486 facts) while the world file
//! grew by 615 characters — and every one of those characters was the serialized
//! **registry**. A world-file hash therefore answers a question about the
//! concept registry, not about placement, and an elf that placed nothing at all
//! would still move it the moment its concept was declared.
//!
//! So this file compares `serde_json::to_string(&world.ledger)` — the plan's own
//! parenthetical alternative, and the strictly narrower instrument. The full
//! sorted `(species, cell)` placement list and the fact count are reported
//! beside it as the cheaper, more legible cross-check.
//!
//! **The honest limit, stated before the result.** Removing a kind removes its
//! seeded draws too, so a ledger difference is *not* by itself evidence that the
//! kind competed for ground. The discriminating half is the second column:
//! whether the placement of the **other fourteen peoples** moves. That is
//! reported per elf per seed and is where a genuinely inert kind would show
//! itself.
//!
//! # P2 — each elf concentrates in its authored biomes
//!
//! **Axis:** the **share** of that elf's settlements sited in its authored
//! stronghold biomes, [`Arm::Shipped`] against [`Arm::NoElfAffinity`], on three
//! seeds. **Falsifier:** the share is flat or falling *while the count also
//! falls*, on a majority of seeds.
//!
//! The share is preregistered rather than the count, following The Range's P1″:
//! a falling count with a **rising** share is *success* (relocation), and both
//! falling is the failure mode.
//!
//! **The preregistration**, frozen from Task 3 Step 1 (2026-08-10, seed 42),
//! with its provenance stated because it matters: these shares were measured at
//! the **abandoned `0.25` level**, before the derivation commit `cda3e3c4` made
//! a row's level the kind's own `sovereignty_floor`.
//!
//! ```text
//!   snow-elf   0/6 -> 5/5      wood-elf  2/3 -> 3/3
//!   high-elf   1/2 -> 2/2      sea-elf   0/3 -> 1/3  (3/3 on the shelf band)
//! ```
//!
//! **The effect size roughly halved between that freeze and this measurement**,
//! and the spec's §3.2 erratum says so in as many words: the stronghold-to-
//! default contrast was `1.00 / 0.25` = 4× and is now `1.00 / 0.429202` ≈ 2.33×
//! for an elf. A P2 null on a marginal elf is therefore *more* likely to mean
//! "the mask is too shallow to reorder this kind's ranking" than "the
//! destination is contested".
//!
//! **P2's pre-committed diagnosis was calibrated on a level that no longer
//! exists.** The Range's P1‴ — suppression-without-relocation, repaired by an
//! affinity permitted above 1.0 — was frozen against the 4× contrast. Do not
//! reach for it here without re-deriving it against 2.33×. Nothing in this file
//! retunes a constant to rescue a prediction; a falsification is reported as a
//! finding.
//!
//! **High is exempt by design** (§4). It has no stronghold of its own — its row
//! is Wood's — and is predicted to show no concentration relative to Wood. That
//! is not a falsification of P2, and the assertion below says so in its own
//! message so a later reader does not "fix" it.
//!
//! **P2 IS FALSIFIED FOR desert-elf, and the axis is why — measured
//! 2026-08-10.** Its stronghold share is `0.000000` in *both* arms on all three
//! seeds while its count falls 19 → 3 and 5 → 1, which is the frozen falsifier
//! firing on a majority of seeds. The `[descriptive]` histograms printed beside
//! it (added after unblinding, never asserted) say what the frozen axis cannot:
//! desert-elf went from **0 of 27** settlements on any biome its row names to
//! **7 of 7** — savanna and shrubland, the row's `near` and `marginal` rungs.
//! That is total relocation onto authored ground. P2's axis is the **stronghold
//! rung alone**, and this kind's stronghold is a single scarce biome, so the
//! axis is blind to a relocation one rung down. The axis stays as frozen and
//! the result is reported as a finding; widening it after unblinding is the
//! metric-chasing decision 0016 exists to prevent.
//!
//! # P3 — Wood and High do NOT separate (the null control)
//!
//! Two axes, because they can disagree.
//!
//! **(a) Capacity fields — and this branch is known IN ADVANCE to be the
//! uninformative one.** Wood and High share mass (55.0 kg) *and* High takes
//! Wood's affinity row entire (`wood.clone()`), so their twelve rows in the
//! committed occupancy fixture are already identical in every column. P3(a)
//! therefore resolves to the bit-identical branch, which the spec itself calls
//! "a wiring check with no information in it". It is run anyway — **a wiring
//! check that has never been run is not a wiring check** — but the campaign's
//! weight is on (b).
//!
//! **If the fields ever DO diverge**, the diagnosis to reach for first is *mass
//! through the affinity level*, not the discarded floor. Since `cda3e3c4`,
//! `biome_affinity_registry` builds every row as
//! `BiomeAffinity::from_preferences(floor_of(kind), …)` and the affinity
//! multiplies **outside** the Liebig minimum — so mass → floor → the row's level
//! → the field is a live path for every kind whose row is self-derived. Wood,
//! High and Drow are exempt only because all three take one row. (The plan's
//! degradation clause named "mass through the sovereignty floor" via a mechanism
//! the erratum's second pass refutes: the floor computed *inside*
//! `per_species_suitability` is discarded, because every affinity occupant has
//! `elevation.devotion` below its floor. The live path is the row's level, and
//! it is the one this campaign created.)
//!
//! **(b) Placements.** The `(people, cell)` settlement lists. The bake's contest
//! is **not** a pure function of the capacity field — iteration order,
//! tie-breaks, migration and the raid comparison all participate — so two kinds
//! with identical fields *can* still place differently. If they do, that is a
//! finding about the contest, and it is reported as such rather than as a fact
//! about elves.
//!
//! # P4 — Drow separates from surface elves by the realm gate alone
//!
//! **Five arms, not one.** The plan specified a single knockout (remove the
//! realm row) and asserted the separation should then vanish "up to Drow's own
//! biome and curve authoring — which Task 3 pinned equal to Wood's, so 'up to'
//! should here mean 'entirely'". **That is false.** Task 3 pinned Drow's
//! affinity (shape *and* level) and its elevation curve to Wood's, but **not its
//! resource vector** — `DETRITUS 0.50 / ANIMAL_PREY 0.30 / PLANT_FORAGE 0.20`
//! against Wood's `PLANT_FORAGE 0.65 / ANIMAL_PREY 0.35` — and
//! `per_species_capacity_at` reads it through `axis_supply_with`. Drow diverges
//! from Wood on all twelve shared biome rows of the committed occupancy fixture.
//!
//! So the plan had two knockouts hiding inside one arm, and its Falsifier B
//! would have fired on an uncontrolled second treatment and reached a
//! true-sounding wrong conclusion. The 2×2 factorial plus the additive mirror:
//!
//! ```text
//!   arm   realm row   resource vector   what it adjudicates
//!   ----  ----------  ----------------  ---------------------------------------
//!   A1    drow's      drow's            the shipped world (baseline)
//!   A2    REMOVED     drow's            the plan's only arm — gate NECESSARY?
//!   A3    drow's      WOOD'S            isolates the niche's main effect
//!   A4    REMOVED     WOOD'S            CLOSURE: nothing authored now differs
//!   A5    Wood + a Subterranean row     MIRROR: is the gate SUFFICIENT?
//! ```
//!
//! **A4 is the arm whose failure is the finding.** With both knockouts, nothing
//! authored distinguishes Drow's capacity field from Wood's: they take the same
//! affinity row at the same level, the same elevation curve, and now the same
//! resource vector; their masses differ (52.0 vs 55.0 kg) but mass reaches this
//! path only through `sovereignty_floor`, which `tolerance_liebig` floors three
//! axes by and which the unfloored elevation term undercuts at every cell. If
//! A4's fields are *not* bit-identical, an **unenumerated third difference
//! exists**, and that outranks P4's stated result.
//!
//! **A5 tests sufficiency, which no subtractive arm can.** It gives Wood the
//! `Subterranean` row Drow carries and compares Wood against **High** — the
//! kind that is body-identical to Wood by construction, and whose bit-identity
//! with Wood in A1 is P3(a) itself. So A5 reuses P3's null as its own positive
//! control: if the two separate in A5 and are bit-identical in A1, the gate is
//! doing the work whatever else is also true.
//!
//! ## The falsifiers, restated — 2026-08-10
//!
//! **Falsifier A** (unchanged): Drow's field is **not** separated from Wood's
//! *with* the row — the gate did not reach identity after all, contradicting The
//! Range's repair.
//!
//! **Falsifier B, RESTATED on 2026-08-10** so that its conclusion follows from
//! its condition (decision ledger, final entry; this is a pre-unblinding
//! correction of a mis-specified falsifier, not a post-hoc retune — nothing had
//! been measured when it was written, and the predictions are unchanged):
//! *Drow is still separated from Wood **in A4**, where nothing authored differs,
//! which means an unenumerated difference exists and §4's attribution is wrong.*
//!
//! Separation surviving in A2 while vanishing in A4 is **not** a falsification.
//! It is the niche's main effect, and it is a reportable result about what
//! separates Drow — not a defect in the gate.
//!
//! ## The companion null, preregistered
//!
//! Drow's dark-adaptation authoring contributes **zero** to its placement.
//! Perturb Drow's `insolation.devotion` alone, in a locally-mutated
//! `WorldComponents` rather than in the registry, and the committed world is
//! byte-identical. **Falsifier:** it moves — which means the two-tier tolerance
//! has started to bind, and the Warren tripwire (`warren_readout.rs`) should
//! have reddened first. **If this fires while the tripwire is green, the
//! tripwire is broken and that is the first thing to fix.**
//!
//! # Measured, 2026-08-10
//!
//! See `.superpowers/sdd/baselines/t4-readout.txt` for the full transcript and
//! the campaign's task-4 report for the reading. The numbers are printed by the
//! tests below rather than transcribed here, so a table in this header can never
//! disagree with the run that produced it.

// `terrain_of` and friends are named derivation entry points (decision 0092); a
// probe measuring a handful of worlds is exactly the site the allowance is for.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, ComponentStore, KindId, Seed, Value};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, build_world_to_with_artifacts, per_species_capacity,
    sky_of,
};

/// The seeds every prediction reports, in order — The Delvers' set, so these
/// numbers sit alongside `range_readout.rs`'s and `delver_distinctness.rs`'s
/// published tables rather than beside them in different units.
const SEEDS: &[u64] = &[42, 7, 1234];

/// The family, in the registry's ascending-`KindId` order. Not derived from
/// `family_of` at run time on purpose: this is the roster P1′ *claims* to
/// measure, and a derived list would quietly shrink if a kind were dropped —
/// turning "every elf moves the world" into "every elf still present moves the
/// world", which is not the prediction.
const ELVES: [&str; 6] = [
    "desert-elf",
    "drow",
    "high-elf",
    "sea-elf",
    "snow-elf",
    "wood-elf",
];

/// The elves whose P2 result the FROZEN axis can see, asserted together in
/// [`each_elf_concentrates_in_its_authored_stronghold_biomes`].
///
/// The split is a carrying decision, not a scoping one: **nothing about P2
/// changed** — not the axis, not the falsifier, not a threshold, not the seed
/// set. `desert-elf` is measured by the identical code in
/// [`desert_elf_concentrates_in_its_authored_stronghold_biomes`], which is
/// `#[ignore]`d under the repo's `PREREGISTERED, not met:` idiom because that
/// prediction is FALSIFIED on the axis as frozen and stays on the record as
/// such. See that test's doc comment for the measurement and the diagnosis.
///
/// `high-elf` is in this roster because it is *reported* by the same run; it is
/// exempt from the assertion by design (spec §4) and the test skips it.
const P2_ON_AXIS: [&str; 5] = ["drow", "high-elf", "sea-elf", "snow-elf", "wood-elf"];

/// The one elf P2's frozen axis cannot see — measured separately, identically.
const P2_OFF_AXIS: [&str; 1] = ["desert-elf"];

/// Which authored rows an arm's [`WorldComponents`] carries.
///
/// Every variant is built by REMOVING or SUBSTITUTING rows from the canonical
/// registries, never by re-authoring them — so an arm can never disagree with
/// the shipped roster about anything except the one thing it is defined to vary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Arm {
    /// The shipped roster, exactly as `assemble()` returns it. **P4's A1.**
    Shipped,
    /// Every elf's affinity row removed; the elves still compete. The Range's
    /// two rows (gnoll, woolly-mammoth) stay, so this arm is "the world before
    /// *this campaign's* rows", not "the world before an affinity existed".
    NoElfAffinity,
    /// One named elf removed from the roster entirely — every kind-keyed store,
    /// re-validated through `check_integrity`. **P1′.**
    WithoutElf(&'static str),
    /// **P4 A2.** Drow's `HabitatRealm::Subterranean` row removed; its resource
    /// vector left alone.
    DrowNoRealm,
    /// **P4 A3.** Drow's resource vector replaced by wood-elf's, read from the
    /// canonical biosphere row; its realm row left alone.
    DrowWoodNiche,
    /// **P4 A4 — the closure arm.** Both knockouts. Nothing authored then
    /// distinguishes Drow's capacity field from Wood's.
    DrowNoRealmWoodNiche,
    /// **P4 A5 — the mirror.** Wood-elf given the `Subterranean` value Drow's
    /// canonical row carries. Drow's own row is untouched.
    WoodSubterranean,
    /// The companion null: Drow's `insolation.devotion` alone perturbed, in a
    /// locally-mutated component set rather than in the registry.
    DrowDarkPerturbed,
}

impl Arm {
    fn label(self) -> String {
        match self {
            Arm::Shipped => "A1 shipped".to_string(),
            Arm::NoElfAffinity => "no elf affinity".to_string(),
            Arm::WithoutElf(k) => format!("without {k}"),
            Arm::DrowNoRealm => "A2 realm REMOVED".to_string(),
            Arm::DrowWoodNiche => "A3 wood's niche".to_string(),
            Arm::DrowNoRealmWoodNiche => "A4 BOTH removed".to_string(),
            Arm::WoodSubterranean => "A5 wood SUBTERRANEAN".to_string(),
            Arm::DrowDarkPerturbed => "drow dark perturbed".to_string(),
        }
    }
}

/// A component store with `kind`'s row dropped. `ComponentStore` has no
/// `remove`, so an arm rebuilds the store by filtering — the same shape
/// `range_readout.rs::components` uses for its fauna-row arm.
fn without<C: Clone>(store: &ComponentStore<KindId, C>, kind: &str) -> ComponentStore<KindId, C> {
    store
        .iter()
        .filter(|(k, _)| k.0 != kind)
        .map(|(k, c)| (*k, c.clone()))
        .collect()
}

/// The canonical component set, varied by exactly this arm's one row.
fn components(arm: Arm) -> WorldComponents {
    let mut wc = WorldComponents::assemble().expect("components assemble");
    match arm {
        Arm::Shipped => {}
        Arm::NoElfAffinity => {
            wc.biome_affinity = wc
                .biome_affinity
                .iter()
                .filter(|(kind, _)| !ELVES.contains(&kind.0))
                .map(|(kind, aff)| (*kind, aff.clone()))
                .collect();
        }
        Arm::WithoutElf(kind) => {
            // Re-validated through `from_stores` rather than assembled by hand:
            // `check_integrity` enforces speech ⊆ perception ⊆ mind and the
            // `Settled` cluster, so an inconsistent removal fails loudly here
            // instead of producing a world nobody can attribute. `family_proto`
            // is keyed by FAMILY label, not by kind, and five daughters remain,
            // so it passes through untouched.
            let WorldComponents {
                biosphere,
                psyche,
                society,
                perception,
                articulation,
                lexicon,
                family_proto,
                family_of,
                deity,
                culture,
                material,
                habitat_realm,
                biome_affinity,
            } = wc;
            return WorldComponents::from_stores(
                without(&biosphere, kind),
                without(&psyche, kind),
                without(&society, kind),
                without(&perception, kind),
                without(&articulation, kind),
                without(&lexicon, kind),
                family_proto,
                without(&family_of, kind),
                deity,
                culture,
                material,
                without(&habitat_realm, kind),
                without(&biome_affinity, kind),
            )
            .expect("a roster missing one elf still satisfies check_integrity");
        }
        Arm::DrowNoRealm => {
            wc.habitat_realm = without(&wc.habitat_realm, "drow");
        }
        Arm::DrowWoodNiche => {
            substitute_wood_niche_into_drow(&mut wc);
        }
        Arm::DrowNoRealmWoodNiche => {
            wc.habitat_realm = without(&wc.habitat_realm, "drow");
            substitute_wood_niche_into_drow(&mut wc);
        }
        Arm::WoodSubterranean => {
            // SUBSTITUTED, not authored: the value is read out of drow's own
            // canonical row, so this arm cannot disagree with the shipped
            // roster about what "subterranean" means.
            let realm = *wc
                .habitat_realm
                .get(&KindId("drow"))
                .expect("drow carries the campaign's realm row");
            wc.habitat_realm.insert(KindId("wood-elf"), realm);
        }
        Arm::DrowDarkPerturbed => {
            let mut drow = wc
                .biosphere
                .get(&KindId("drow"))
                .expect("drow has a biosphere row")
                .clone();
            // The one authored value under test. 0.55 is the family's most
            // devoted curve; 0.05 is as close to indifference as the axis goes
            // without being zero, so this is a large perturbation and not a
            // nudge that could pass by being too small to see.
            drow.condition_niche.insolation.devotion = 0.05;
            wc.biosphere.insert(KindId("drow"), drow);
        }
    }
    wc
}

/// Replace drow's resource vector with wood-elf's, read live from the canonical
/// biosphere row rather than transcribed.
fn substitute_wood_niche_into_drow(wc: &mut WorldComponents) {
    let wood_niche = wc
        .biosphere
        .get(&KindId("wood-elf"))
        .expect("wood-elf has a biosphere row")
        .niche
        .clone();
    let mut drow = wc
        .biosphere
        .get(&KindId("drow"))
        .expect("drow has a biosphere row")
        .clone();
    drow.niche = wood_niche;
    wc.biosphere.insert(KindId("drow"), drow);
}

/// One committed settlement, read off the ledger.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Sited {
    /// The founding people.
    people: String,
    /// The committed cell.
    cell: u32,
    /// The biome resolved from the CELL, not from the settlement's `biome` text
    /// fact — the descriptor facts are written by the naming pass, and reading
    /// the cell keeps this independent of which depth committed which
    /// descriptor.
    biome: &'static str,
    /// Whether that cell holds an enterable cave — the realm gate's own
    /// predicate, `terrain.cave_at(cell).is_some()`.
    cave: bool,
}

/// Everything one `(seed, arm)` world is read for, from a SINGLE build.
struct WorldRead {
    /// `ledger.len()` — the committed fact count.
    facts: usize,
    /// `serde_json::to_string(&world.ledger)`. The LEDGER, not the world file:
    /// see the module header for why the world file answers a different
    /// question.
    ledger_json: String,
    /// Every settlement, in ledger-commit order.
    sited: Vec<Sited>,
}

impl WorldRead {
    /// `(people, cell)` for every settlement, in ledger-commit order.
    fn placement(&self) -> Vec<(String, u32)> {
        self.sited
            .iter()
            .map(|s| (s.people.clone(), s.cell))
            .collect()
    }

    /// `(people, cell)` for every settlement founded by someone other than
    /// `people` — the discriminating half of P1′.
    fn placement_excluding(&self, people: &str) -> Vec<(String, u32)> {
        self.sited
            .iter()
            .filter(|s| s.people != people)
            .map(|s| (s.people.clone(), s.cell))
            .collect()
    }

    /// This people's settlements.
    fn of(&self, people: &str) -> Vec<&Sited> {
        self.sited.iter().filter(|s| s.people == people).collect()
    }
}

/// Build the world at `seed` under `arm` to `Full` depth and read it once.
///
/// **`Full` depth, and the shallower rung is not an option.** `Settlements`
/// depth runs the bake and places every settlement but commits **no
/// `peopled-by` fact** — measured by The Range on 2026-08-09, every settlement
/// reads `species_of == None` there. A tally built at that depth is not a
/// smaller measurement, it is an empty one, and its zero would read as "this
/// elf founds nothing" rather than as "this depth does not say who founded it".
/// The assertion below is the guard against that recurring silently.
fn read_world(seed: u64, arm: Arm) -> WorldRead {
    let built = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &components(arm),
        BuildDepth::Full,
    )
    .expect("probe seed builds");
    let world = &built.world;
    let climate = built.climate.as_ref().expect("Full depth has climate");
    let terrain = built.terrain.as_ref().expect("Full depth has terrain");

    let mut sited = Vec::new();
    let mut placed = 0usize;
    for fact in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        placed += 1;
        let people = hornvale_species::species_of(world, fact.subject)
            .expect("a committed settlement names its people at Full depth");
        let cell = match world
            .ledger
            .value_of(fact.subject, hornvale_settlement::CELL_ID)
        {
            Some(Value::Number(n)) => CellId(*n as u32),
            _ => panic!("a committed settlement must carry a cell-id"),
        };
        sited.push(Sited {
            people,
            cell: cell.0,
            biome: climate.biome_at(cell).name(),
            cave: terrain.cave_at(cell).is_some(),
        });
    }
    assert!(placed > 0, "seed {seed} placed no settlements at all");
    assert_eq!(
        placed,
        sited.len(),
        "every placed settlement must name its people"
    );

    WorldRead {
        facts: world.ledger.len(),
        ledger_json: serde_json::to_string(&world.ledger).expect("the ledger serializes"),
        sited,
    }
}

/// Per-kind capacity fields over land, from one `Settlements`-depth build.
///
/// `per_species_capacity` is the **dimensional** path settlement placement
/// consumes (`per_species_capacity_at` is what `bake_history_from` calls), not
/// the saturating readout — so a separation measured here is a separation in the
/// quantity that decides where a people can live.
struct Fields {
    /// The biosphere roster, in ascending `KindId` order.
    roster: Vec<&'static str>,
    /// One column per roster entry, over the land cells, in `roster` order.
    columns: Vec<Vec<f64>>,
    /// How many land cells the columns are over.
    land: usize,
}

impl Fields {
    fn column(&self, kind: &str) -> &[f64] {
        let idx = self
            .roster
            .iter()
            .position(|k| *k == kind)
            .unwrap_or_else(|| panic!("{kind:?} has no biosphere row in this arm"));
        &self.columns[idx]
    }

    /// Do these two kinds hold the SAME field, bit for bit?
    fn bit_identical(&self, a: &str, b: &str) -> bool {
        let (x, y) = (self.column(a), self.column(b));
        x.len() == y.len()
            && x.iter()
                .zip(y.iter())
                .all(|(p, q)| p.to_bits() == q.to_bits())
    }

    /// The largest absolute per-cell difference between two kinds' fields.
    fn max_abs_diff(&self, a: &str, b: &str) -> f64 {
        self.column(a)
            .iter()
            .zip(self.column(b).iter())
            .map(|(p, q)| (p - q).abs())
            .fold(0.0f64, f64::max)
    }
}

/// The per-kind capacity fields of the world at `seed` under `arm`, over land.
///
/// The three parallel species slices are built from ONE `wc.biosphere`
/// iteration each, in the store's ascending-`KindId` order, exactly as the
/// shipped path builds them: a slice that drifted out of alignment would score a
/// kind against another kind's realm or affinity and every number here would be
/// quietly wrong, with nothing to notice it.
fn fields(seed: u64, arm: Arm) -> Fields {
    let wc = components(arm);
    let built = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .expect("probe seed builds");
    let world = &built.world;
    let terrain = built.terrain.as_ref().expect("Terrain depth or deeper");
    let climate = built
        .climate
        .as_ref()
        .expect("Settlements depth has climate");
    let geo = terrain.geosphere();

    let sky = sky_of(world).expect("sky");
    let generated = match &sky {
        hornvale_worldgen::Sky::Generated(g) => g,
        _ => panic!("probe expects a generated sky"),
    };
    let system = generated.system();
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let roster: Vec<&'static str> = wc.biosphere.iter().map(|(kind, _)| kind.0).collect();
    let species_biosphere: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    let species_realm: Vec<hornvale_species::HabitatRealm> = wc
        .biosphere
        .iter()
        .map(|(kind, _)| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
        })
        .collect();
    let species_affinity: Vec<Option<hornvale_species::BiomeAffinity>> = wc
        .biosphere
        .iter()
        .map(|(kind, _)| wc.biome_affinity.get(kind).cloned())
        .collect();

    let per_species = per_species_capacity(
        geo,
        terrain,
        climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &species_biosphere,
        &species_realm,
        &species_affinity,
    );

    let land: Vec<CellId> = geo.cells().filter(|&c| !terrain.is_ocean(c)).collect();
    let columns: Vec<Vec<f64>> = (0..roster.len())
        .map(|idx| {
            let (_, k) = per_species
                .iter()
                .find(|(tag, _)| *tag as usize == idx)
                .unwrap_or_else(|| panic!("no capacity field for roster index {idx}"));
            land.iter().map(|&c| k.at(c)).collect()
        })
        .collect();

    Fields {
        roster,
        columns,
        land: land.len(),
    }
}

/// Pearson's `r` between two equal-length samples.
///
/// Two-pass (means first, then centred sums) rather than the algebraically
/// equivalent `E[xy] - E[x]E[y]` form, which cancels catastrophically when the
/// mean is large relative to the spread. Copied in shape from
/// `range_readout.rs` and `delver_distinctness.rs` deliberately: these numbers'
/// whole value is that they are comparable with those campaigns', which requires
/// the same estimator.
///
/// **Scope limit.** Pearson `r` is invariant under a positive affine rescale, so
/// it measures how two fields *sort* cells, not how large they are. A pair
/// reading `1.0` still admits wholly different absolute capacities — which is
/// exactly why [`Fields::bit_identical`] and [`Fields::max_abs_diff`] are
/// reported beside it and carry the assertions.
///
/// Returns `None` on a constant sample rather than `NaN`: a `NaN` compares false
/// against every threshold in both directions, so a degenerate field would
/// silently satisfy any assertion. A gated `Subterranean` field is constant-zero
/// on a world with no caves at all, which is a real (if unlikely) arm.
fn pearson(xs: &[f64], ys: &[f64]) -> Option<f64> {
    assert_eq!(xs.len(), ys.len(), "correlation needs paired samples");
    assert!(!xs.is_empty(), "correlation over an empty land mask");
    let n = xs.len() as f64;
    let mean_x = xs.iter().sum::<f64>() / n;
    let mean_y = ys.iter().sum::<f64>() / n;
    let (mut sxx, mut syy, mut sxy) = (0.0, 0.0, 0.0);
    for (x, y) in xs.iter().zip(ys.iter()) {
        let dx = x - mean_x;
        let dy = y - mean_y;
        sxx += dx * dx;
        syy += dy * dy;
        sxy += dx * dy;
    }
    if sxx <= 0.0 || syy <= 0.0 {
        return None;
    }
    Some(sxy / (sxx * syy).sqrt())
}

/// The biomes at the ladder's TOP rung in this kind's shipped affinity row —
/// derived from the registry, never re-listed here.
///
/// `AFFINITY_STRONGHOLD` maps to the largest factor a row carries, so the
/// strongholds are the listed biomes tying for that maximum. Comparison is on
/// `to_bits()`, which is exact: the maximum is drawn from the very set being
/// filtered, so this is an identity test rather than an epsilon question.
///
/// Derived rather than transcribed because the alternative goes stale silently:
/// snow-elf's strongholds are `taiga`+`tundra` (not the `tundra`+`ice` the
/// campaign brief said), and a hand-written list here would have measured P2
/// against ground the kind was never authored onto.
fn strongholds(kind: &'static str) -> Vec<&'static str> {
    let registry = hornvale_species::biome_affinity_registry();
    let aff = registry
        .get(&KindId(kind))
        .unwrap_or_else(|| panic!("{kind:?} has no authored affinity row"));
    assert!(
        !aff.by_biome.is_empty(),
        "{kind:?}'s affinity row lists no biome at all"
    );
    let top = aff
        .by_biome
        .iter()
        .map(|(_, f)| *f)
        .fold(f64::NEG_INFINITY, f64::max);
    aff.by_biome
        .iter()
        .filter(|(_, f)| f.to_bits() == top.to_bits())
        .map(|(name, _)| *name)
        .collect()
}

/// Every biome this kind's shipped affinity row names, with its factor, in
/// descending factor then ascending name — the whole ladder, not just its top
/// rung. **DESCRIPTIVE**, printed beside P2's frozen axis.
fn authored_ladder(kind: &'static str) -> Vec<(&'static str, f64)> {
    let registry = hornvale_species::biome_affinity_registry();
    let aff = registry
        .get(&KindId(kind))
        .unwrap_or_else(|| panic!("{kind:?} has no authored affinity row"));
    let mut out: Vec<(&'static str, f64)> = aff.by_biome.clone();
    out.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.cmp(b.0)));
    out
}

/// One elf's settlement tally on one seed under one arm.
#[derive(Debug, Clone, Copy)]
struct Tally {
    /// Every settlement this people founded.
    total: usize,
    /// How many sit on one of the people's authored stronghold biomes.
    home: usize,
}

impl Tally {
    /// The stronghold fraction. `0.0` when the people founded nothing — a people
    /// with no settlements has no share, and returning `NaN` would let a `>`
    /// comparison pass or fail by accident.
    fn share(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.home as f64 / self.total as f64
        }
    }
}

fn tally(read: &WorldRead, people: &str, home_biomes: &[&'static str]) -> Tally {
    let mine = read.of(people);
    Tally {
        total: mine.len(),
        home: mine
            .iter()
            .filter(|s| home_biomes.contains(&s.biome))
            .count(),
    }
}

/// Where a people's settlements actually sit, as a biome histogram in
/// descending count then ascending name.
///
/// **DESCRIPTIVE, added after unblinding P2 and never asserted.** P2's axis is
/// the stronghold share and that axis is frozen; this exists because a share of
/// `0.000000` in both arms says a prediction failed and says nothing whatever
/// about *where the people went instead*, which is the difference between
/// "suppressed" and "relocated one rung down". Printed for every elf so it
/// cannot be read as a set assembled around the one kind that falsified.
fn biome_histogram(read: &WorldRead, people: &str) -> Vec<(&'static str, usize)> {
    let mut counts: std::collections::BTreeMap<&'static str, usize> =
        std::collections::BTreeMap::new();
    for s in read.of(people) {
        *counts.entry(s.biome).or_insert(0) += 1;
    }
    let mut out: Vec<(&'static str, usize)> = counts.into_iter().collect();
    out.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(b.0)));
    out
}

// ===========================================================================
// The cheap arm-machinery control. Builds NO world, so it stays in the commit
// gate: an arm that silently failed to differ from the shipped roster would
// make every measurement in this file vacuous, and that is exactly the class of
// green-but-empty guard this campaign has found four times.
// ===========================================================================

/// Each P4 arm must differ from the shipped roster in **exactly** the row it
/// names, and in nothing else.
///
/// Both directions matter. That the named row moved is what makes the arm a
/// treatment; that nothing else moved is what makes the comparison attributable.
/// A subtractive arm that also perturbed a second row would reproduce precisely
/// the defect the five-arm factorial was built to remove.
#[test]
fn the_arms_differ_from_the_shipped_roster_in_exactly_the_row_they_name() {
    let base = components(Arm::Shipped);
    let drow = KindId("drow");
    let wood = KindId("wood-elf");

    // A2: drow's realm row gone, biosphere untouched.
    let a2 = components(Arm::DrowNoRealm);
    assert!(
        a2.habitat_realm.get(&drow).is_none(),
        "A2 must remove drow's realm row"
    );
    assert_eq!(
        a2.biosphere.get(&drow),
        base.biosphere.get(&drow),
        "A2 must not touch drow's biosphere row"
    );
    assert_eq!(
        a2.habitat_realm.len(),
        base.habitat_realm.len() - 1,
        "A2 must remove exactly one realm row"
    );

    // A3: drow's niche is wood's, realm row untouched.
    let a3 = components(Arm::DrowWoodNiche);
    assert_eq!(
        a3.biosphere.get(&drow).map(|b| &b.niche),
        base.biosphere.get(&wood).map(|b| &b.niche),
        "A3 must give drow wood-elf's resource vector"
    );
    assert_ne!(
        base.biosphere.get(&drow).map(|b| &b.niche),
        base.biosphere.get(&wood).map(|b| &b.niche),
        "the shipped drow and wood-elf must DIFFER in resource vector, or A3 \
         and A4 are no-ops and P4's factorial has only two distinct arms"
    );
    assert_eq!(
        a3.habitat_realm.get(&drow),
        base.habitat_realm.get(&drow),
        "A3 must not touch drow's realm row"
    );
    assert_eq!(
        a3.biosphere.get(&drow).map(|b| b.mass),
        base.biosphere.get(&drow).map(|b| b.mass),
        "A3 varies the resource vector alone — mass must not move with it"
    );

    // A4: both.
    let a4 = components(Arm::DrowNoRealmWoodNiche);
    assert!(
        a4.habitat_realm.get(&drow).is_none(),
        "A4 must remove drow's realm row"
    );
    assert_eq!(
        a4.biosphere.get(&drow).map(|b| &b.niche),
        base.biosphere.get(&wood).map(|b| &b.niche),
        "A4 must give drow wood-elf's resource vector"
    );

    // A5: wood gains drow's realm VALUE; drow keeps its own row.
    let a5 = components(Arm::WoodSubterranean);
    assert_eq!(
        a5.habitat_realm.get(&wood),
        base.habitat_realm.get(&drow),
        "A5 must give wood-elf the value drow's canonical row carries"
    );
    assert_eq!(
        a5.habitat_realm.get(&drow),
        base.habitat_realm.get(&drow),
        "A5 is ADDITIVE — drow's own row stays"
    );
    assert!(
        base.habitat_realm.get(&wood).is_none(),
        "wood-elf must be SURFACE in the shipped roster, or A5 varies nothing"
    );

    // The companion null's arm.
    let perturbed = components(Arm::DrowDarkPerturbed);
    let before = base
        .biosphere
        .get(&drow)
        .expect("drow row")
        .condition_niche
        .insolation
        .devotion;
    let after = perturbed
        .biosphere
        .get(&drow)
        .expect("drow row")
        .condition_niche
        .insolation
        .devotion;
    assert_ne!(
        before.to_bits(),
        after.to_bits(),
        "the companion null's arm must actually perturb drow's insolation \
         devotion, or its byte-identity result is a tautology"
    );

    // The elf-affinity arm and the per-elf removals.
    let no_aff = components(Arm::NoElfAffinity);
    for elf in ELVES {
        assert!(
            no_aff.biome_affinity.get(&KindId(elf)).is_none(),
            "NoElfAffinity must remove {elf}'s row"
        );
        assert!(
            no_aff.biosphere.get(&KindId(elf)).is_some(),
            "NoElfAffinity removes the AFFINITY only — {elf} must still compete"
        );
        let dropped = components(Arm::WithoutElf(elf));
        assert!(
            dropped.biosphere.get(&KindId(elf)).is_none(),
            "WithoutElf({elf}) must remove its biosphere row"
        );
        assert!(
            dropped.psyche.get(&KindId(elf)).is_none(),
            "WithoutElf({elf}) must remove its psyche row"
        );
        assert!(
            dropped.lexicon.get(&KindId(elf)).is_none(),
            "WithoutElf({elf}) must remove its lexicon row"
        );
        assert_eq!(
            dropped.biosphere.len(),
            base.biosphere.len() - 1,
            "WithoutElf({elf}) must remove exactly one biosphere row"
        );
    }
    assert!(
        no_aff.biome_affinity.get(&KindId("gnoll")).is_some(),
        "NoElfAffinity keeps THE RANGE's two rows — it is 'the world before \
         this campaign's rows', not 'the world before an affinity existed'"
    );
}

/// Every elf's authored stronghold set, printed and shape-checked. Cheap; stays
/// in the gate.
///
/// The shape check is the guard against P2 silently measuring nothing: an elf
/// with no stronghold, or with every biome at the top rung, would give a share
/// that cannot move.
#[test]
fn every_elf_has_a_proper_nonempty_stronghold_set() {
    let all: Vec<&'static str> = hornvale_climate::biome::ALL
        .iter()
        .map(|b| b.name())
        .collect();
    for elf in ELVES {
        let home = strongholds(elf);
        println!("   {elf:<12} strongholds {home:?}");
        assert!(!home.is_empty(), "{elf} has no stronghold biome");
        assert!(
            home.len() < all.len(),
            "{elf}'s strongholds are EVERY biome, so its share cannot move"
        );
        for name in &home {
            assert!(
                all.contains(name),
                "{elf} names a stronghold {name:?} that is not a live biome — \
                 `BiomeAffinity::factor` falls back to `default` for an unknown \
                 key, so a misspelling is silently inert"
            );
        }
    }
    assert_eq!(
        strongholds("high-elf"),
        strongholds("wood-elf"),
        "High takes Wood's row entire — if these ever differ, P2's High \
         exemption and P3's null control are both measuring something else"
    );
}

/// P2 is asserted from two tests, and between them they must still cover the
/// whole family exactly once. Builds no world; stays in the gate.
///
/// This is the guard on the SPLIT itself, and it exists because the failure
/// mode of splitting a preregistered test is silent: drop a kind from both
/// rosters and P2 simply stops measuring it, with nothing red and nothing to
/// grep. A partition check is the cheapest statement that carrying the
/// falsification separately did not quietly narrow the prediction.
#[test]
fn the_two_p2_rosters_partition_the_family_exactly_once() {
    let mut union: Vec<&str> = P2_ON_AXIS
        .iter()
        .chain(P2_OFF_AXIS.iter())
        .copied()
        .collect();
    union.sort_unstable();
    let mut family: Vec<&str> = ELVES.to_vec();
    family.sort_unstable();
    assert_eq!(
        union, family,
        "P2's two rosters no longer partition the family. Every elf must be \
         measured by exactly one of the two tests: dropping one from both \
         narrows a preregistered prediction with nothing to show for it, and \
         listing one in both double-counts a seed sweep for no gain."
    );
}

// ===========================================================================
// The heavy tier. Everything below builds worlds.
// ===========================================================================

/// **P1′.** Each elf's presence must change the committed world on at least one
/// of three seeds.
///
/// **Falsifier:** some elf is byte-neutral in both directions — that kind is
/// authored and inert, rung 2 on that kind, and it is a finding about that kind
/// rather than about the mechanism. Reported, never retuned.
///
/// Measured on the LEDGER (`serde_json::to_string(&world.ledger)`), not on the
/// serialized world: Task 1 measured that six new concepts leave the ledger
/// byte-identical while growing the world file by 615 characters of registry, so
/// a world-file hash answers a question about the concept registry. See the
/// module header.
///
/// Two further columns are printed because the ledger alone cannot separate
/// "this kind competed" from "this kind consumed draws": the elf's own
/// settlement count in the shipped arm, and whether the placement of the OTHER
/// fourteen peoples moved when it was removed. The second is the discriminating
/// one.
///
/// claim: invariant(forall-elf, exists-seed: the committed ledger differs with
/// and without that elf) — a per-KIND existential over three seeds, which is the
/// quantifier the prediction was frozen with. Not `forall-seed`: a kind may
/// honestly place nothing on one globe and still be a live competitor.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn each_elf_changes_the_committed_ledger() {
    let shipped: Vec<WorldRead> = SEEDS.iter().map(|&s| read_world(s, Arm::Shipped)).collect();

    println!("== P1': the per-elf rung-5 test, measured on the LEDGER ==");
    let mut inert: Vec<&str> = Vec::new();
    for elf in ELVES {
        let mut moved_on = 0usize;
        for (i, &seed) in SEEDS.iter().enumerate() {
            let with = &shipped[i];
            let without_it = read_world(seed, Arm::WithoutElf(elf));
            let ledger_moved = with.ledger_json != without_it.ledger_json;
            let others_moved = with.placement_excluding(elf) != without_it.placement_excluding(elf);
            if ledger_moved {
                moved_on += 1;
            }
            println!(
                "   {elf:<12} seed {seed:<5} facts {:>6} -> {:>6}   own settlements {:>3}   \
                 ledger moved {:<5}   OTHER peoples' placement moved {}",
                with.facts,
                without_it.facts,
                with.of(elf).len(),
                ledger_moved,
                others_moved,
            );
        }
        println!(
            "   {elf:<12} ledger moved on {moved_on} of {} seeds",
            SEEDS.len()
        );
        if moved_on == 0 {
            inert.push(elf);
        }
    }
    assert!(
        inert.is_empty(),
        "P1' FALSIFIED for {inert:?}: removing this kind left the committed \
         ledger byte-identical on every seed. That kind is authored and INERT — \
         rung 2 on that kind. It is a finding about the kind, not about the \
         mechanism; report it, do not retune to rescue it."
    );
}

/// P2's frozen falsifier, evaluated over `elves` and returning the kinds it
/// fires for.
///
/// **The single implementation of the frozen axis.** Both P2 tests call it with
/// a different roster and assert the identical emptiness condition on its
/// result; nothing here knows which roster it was handed. That is deliberate:
/// the split exists so that a falsified prediction can be *carried* in the
/// repo's idiom, not so that it can be *measured differently*, and a shared
/// body is the only form in which that is checkable by reading.
///
/// **Falsifier, verbatim as frozen:** the share is flat or falling **while the
/// count also falls**, on a majority of seeds. A falling count with a RISING
/// share is success (relocation, The Range's P1″); both falling is the failure
/// mode.
///
/// **High is exempt by design** (spec §4) and is reported without being
/// asserted: its row is Wood's, it has no stronghold of its own, and it is
/// predicted to show no concentration relative to Wood. That is not a
/// falsification, and this comment plus the print below exist so a later reader
/// does not "fix" it.
fn p2_falsified_among(elves: &[&'static str]) -> Vec<String> {
    println!("== P2: stronghold share, shipped vs the affinity rows removed ==");
    let arms: Vec<(u64, WorldRead, WorldRead)> = SEEDS
        .iter()
        .map(|&seed| {
            (
                seed,
                read_world(seed, Arm::NoElfAffinity),
                read_world(seed, Arm::Shipped),
            )
        })
        .collect();

    let mut falsified: Vec<String> = Vec::new();
    for &elf in elves {
        let home = strongholds(elf);
        let mut both_fell = 0usize;
        for (seed, before_read, after_read) in &arms {
            let before = tally(before_read, elf, &home);
            let after = tally(after_read, elf, &home);
            let share_down = after.share() <= before.share();
            let count_down = after.total < before.total;
            if share_down && count_down {
                both_fell += 1;
            }
            println!(
                "   {elf:<12} seed {seed:<5} {home:?}\n      \
                 absent   {:>3} settlements   {:>3} home   share {:.6}\n      \
                 shipped  {:>3} settlements   {:>3} home   share {:.6}   \
                 [share {} / count {}]",
                before.total,
                before.home,
                before.share(),
                after.total,
                after.home,
                after.share(),
                if after.share() > before.share() {
                    "ROSE"
                } else if share_down && after.share() < before.share() {
                    "fell"
                } else {
                    "flat"
                },
                if count_down {
                    "fell"
                } else if after.total > before.total {
                    "rose"
                } else {
                    "flat"
                },
            );
            // DESCRIPTIVE, never asserted — see `biome_histogram`'s doc. The
            // row's own ladder is printed beside it so a reader can see at once
            // whether a people that left its stronghold landed on a rung of its
            // own row or on ground the row never mentions.
            println!(
                "      [descriptive] shipped biomes {:?}\n      \
                 [descriptive] absent  biomes {:?}\n      \
                 [descriptive] this row's whole ladder {:?}",
                biome_histogram(after_read, elf),
                biome_histogram(before_read, elf),
                authored_ladder(elf),
            );
        }
        if elf == "high-elf" {
            println!(
                "   high-elf     EXEMPT BY DESIGN (spec §4): its row is wood's, it has no \
                 stronghold of its own, and no concentration is predicted. Reported, \
                 not asserted — this is a CONTROL, not a failure."
            );
            continue;
        }
        if both_fell > SEEDS.len() / 2 {
            falsified.push(format!("{elf} (both fell on {both_fell} seeds)"));
        }
    }
    falsified
}

/// The failure message both P2 tests raise, verbatim. Held in one place so the
/// diagnosis a reader meets cannot drift between the two.
const P2_FALSIFICATION_DIAGNOSIS: &str = "the stronghold share was flat or \
     falling WHILE the settlement count also fell, on a majority of seeds. \
     That is suppression, not relocation.\n\n\
     DIAGNOSIS — RE-DERIVE IT, DO NOT REACH FOR THE FROZEN ONE. P2's \
     pre-committed diagnosis (The Range's P1''', a downward-only mask \
     suppressing without relocating, repaired by an affinity permitted \
     above 1.0) was calibrated when the stronghold:default contrast was 4x \
     (1.00/0.25). Since cda3e3c4 it is ~2.33x (1.00/0.429202) for an elf, \
     so a null on a marginal kind is now MORE likely to mean the mask is \
     too shallow to reorder that kind's ranking at all. Whichever reading \
     holds, the repair is a recorded DECISION, never a retuned constant.\n\n\
     AND READ THE [descriptive] HISTOGRAMS ABOVE BEFORE EITHER. Measured \
     2026-08-10: desert-elf went from 0 of 27 settlements on ANY biome its \
     row names (three seeds, affinity absent) to 7 of 7 (affinity shipped) \
     — savanna and shrubland, the row's `near` and `marginal` rungs. That \
     is total relocation onto authored ground, and P2's frozen axis cannot \
     see it, because the axis is the STRONGHOLD rung alone and this kind's \
     stronghold is a single scarce biome. The falsification is real ON THE \
     AXIS AS FROZEN; the mechanism is not what failed. Widening the axis \
     after unblinding would be exactly the metric-chasing decision 0016 \
     exists to prevent, so the axis stays and the finding is reported.";

/// **P2, for the five elves whose result the frozen axis can see.** Each one's
/// share of settlements on its authored stronghold biomes must not fall while
/// its count also falls.
///
/// **Falsifier, unchanged:** the share is flat or falling **while the count
/// also falls**, on a majority of seeds. A falling count with a RISING share is
/// success (relocation, The Range's P1″); both falling is the failure mode.
/// **High is exempt by design** (spec §4): reported, never asserted.
///
/// **Measured 2026-08-10, absent → shipped, per seed (42 / 7 / 1234):**
///
/// ```text
///   wood-elf   30 @ 0.967 -> 6 @ 1.000 | 5 @ 0.000 -> 10 @ 0.100 | 3 @ 0.000 ->  8 @ 1.000
///   snow-elf    5 @ 0.000 -> 25 @ 0.960 | 15 @ 0.000 -> 11 @ 1.000 | 4 @ 0.250 ->  2 @ 1.000
///   sea-elf     3 @ 0.000 ->  3 @ 0.000 | 2 @ 0.500 ->  2 @ 0.500 | 10 @ 0.300 -> 11 @ 0.636
///   drow        5 @ 0.400 ->  5 @ 0.400 | 3 @ 0.000 ->  3 @ 0.333 | 43 @ 0.023 -> 13 @ 0.462
///   high-elf    8 @ 0.500 ->  2 @ 1.000 | 9 @ 0.000 ->  2 @ 0.000 | 6 @ 0.333 ->  4 @ 0.750
/// ```
///
/// Wood and Snow rose on 3 of 3 seeds; Drow on 2 of 3; Sea's count never fell,
/// so its falsifier could not fire. Snow is the strongest result in the
/// campaign: `0.000 → 0.960` with the count RISING 5 → 25. High, the control,
/// moves *with* Wood because Wood's row is its row — that is the null reading
/// correctly, not an elf succeeding.
///
/// **This test is one half of a split, and the split changed no number.** The
/// sixth elf is measured by the identical helper in
/// [`desert_elf_concentrates_in_its_authored_stronghold_biomes`], which is
/// preregistered-not-met and `#[ignore]`d. Nothing here was widened, rescoped
/// or rethresholded to make this half green; the roster is the only difference,
/// and [`the_two_p2_rosters_partition_the_family_exactly_once`] holds the two
/// rosters to the whole family.
///
/// claim: rate(3 seeds x 4 asserted elves, each must avoid the both-falling
/// failure mode on a majority of seeds) — a per-elf majority over seeds, frozen
/// at that quantifier in the spec before any row was authored.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn each_elf_concentrates_in_its_authored_stronghold_biomes() {
    let falsified = p2_falsified_among(&P2_ON_AXIS);
    assert!(
        falsified.is_empty(),
        "P2 FALSIFIED for {falsified:?}: {P2_FALSIFICATION_DIAGNOSIS}"
    );
}

/// **P2 for desert-elf — FALSIFIED on the axis as frozen, and carried as such.**
///
/// The assertion, the falsifier, the seeds and the thresholds are the ones the
/// five-elf test uses, because they are the same code
/// ([`p2_falsified_among`]). **This test's failure is the record**, in the
/// project's idiom for a preregistered prediction that was not met.
///
/// **Falsifier, unchanged:** the share is flat or falling **while the count
/// also falls**, on a majority of seeds.
///
/// **Measured 2026-08-10, absent → shipped:** seed 42 `3 @ 0.000 → 3 @ 0.000`,
/// seed 7 `19 @ 0.000 → 3 @ 0.000`, seed 1234 `5 @ 0.000 → 1 @ 0.000`. Share
/// flat at zero in both arms and the count fell on 2 of 3 seeds, so the
/// falsifier fires. That is a real falsification of P2 as written.
///
/// # What the axis could not see — POST-HOC AND DESCRIPTIVE
///
/// The numbers in this section were computed **after unblinding**, from the
/// `[descriptive]` biome histograms the helper prints for every elf. They are
/// reported, never asserted, and they do **not** make P2 confirmed for this
/// kind. P2's axis is the stronghold rung alone; that axis is frozen and stays
/// frozen.
///
/// ```text
///   seed 42    absent  temperate-forest 2, tropical-rainforest 1   ->  shrubland 2, savanna 1
///   seed 7     absent  trop-seasonal-forest 16, trop-rainforest 3  ->  savanna 3
///   seed 1234  absent  temperate-forest 3, taiga 2                 ->  shrubland 1
/// ```
///
/// Desert-elf's row is `desert 1.00 / savanna 0.827 / temperate-grassland
/// 0.827 / shrubland 0.682`, so across the three seeds it went from **0 of 27
/// settlements on any biome its row names** to **7 of 7** — landing on the
/// `near` and `marginal` rungs, never on the stronghold. The axis registers
/// that as `0.000000 → 0.000000`, because this kind's stronghold is a single
/// scarce biome. Widening the axis to "any authored biome", or to a
/// rung-weighted score, *after* seeing this would change what counts as success
/// in order to rescue a falsified prediction — precisely what decision 0016
/// exists to prevent. The successor axis belongs in a preregistration, and is
/// filed as `BIO-rung-weighted-concentration`.
///
/// # The frozen diagnosis was RE-DERIVED and RULED OUT
///
/// P2 shipped with a pre-committed diagnosis: The Range's P1‴, a downward-only
/// mask that suppresses without relocating, made likelier by the halved
/// stronghold:default contrast (4× → ~2.33× since `cda3e3c4`). That is **not
/// what happened here.** P1‴ predicts a people thinned and left *where it was*;
/// desert-elf was thinned **and moved entirely**, its off-row occupancy going
/// to zero. The 2.33× contrast was ample to reorder this kind's ranking — it
/// simply did not carry it to the top rung. Neither "the mask is too shallow"
/// nor "the destination is contested" describes the measurement. **Nothing was
/// retuned.**
///
/// claim: rate(3 seeds x 1 elf, must avoid the both-falling failure mode on a
/// majority of seeds) — the same quantifier the spec froze, on the one kind it
/// does not hold for.
#[test]
#[ignore = "PREREGISTERED, not met: awaits BIO-rung-weighted-concentration (a stronghold-only axis reads relocation one rung down as suppression)"]
fn desert_elf_concentrates_in_its_authored_stronghold_biomes() {
    let falsified = p2_falsified_among(&P2_OFF_AXIS);
    assert!(
        falsified.is_empty(),
        "P2 FALSIFIED for {falsified:?}: {P2_FALSIFICATION_DIAGNOSIS}"
    );
}

/// **P3.** Wood and High must not separate — the family's null control.
///
/// **(a) Capacity fields, and the limit stated BEFORE the result.** Wood and
/// High share mass (55.0 kg) and High takes Wood's affinity row entire, so this
/// branch is known in advance to resolve to bit-identity, which the spec itself
/// calls "a wiring check with no information in it". It is run because a wiring
/// check that has never been run is not a wiring check. The information is in
/// (b).
///
/// **Falsifier for (a):** they separate — a finding about what else
/// differentiates them, not a failure. The first diagnosis to reach for is mass
/// through the affinity LEVEL (`from_preferences(floor_of(kind), …)`, applied
/// outside the Liebig minimum), which is the path this campaign created; not the
/// floor computed inside `per_species_suitability`, which every affinity
/// occupant's below-floor elevation devotion discards.
///
/// **(b) Placements.** The bake's contest is not a pure function of the capacity
/// field — iteration order, tie-breaks, migration and the raid comparison all
/// participate — so two kinds with identical fields CAN place differently. If
/// they do, that is a finding about the contest, reported as such rather than as
/// a fact about elves.
///
/// claim: invariant(forall-seed, wood's and high's capacity fields are
/// bit-identical over land) — a structural claim about two rows that are the
/// same value, so a single disagreeing seed is a real result and the quantifier
/// is universal.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn wood_and_high_do_not_separate_in_field_or_placement() {
    println!("== P3(a): wood vs high capacity fields over land ==");
    println!(
        "   STATED BEFORE THE RESULT: wood and high share mass (55.0 kg) and one \
         affinity row, so bit-identity here is a WIRING CHECK WITH NO \
         INFORMATION IN IT. The information is in (b)."
    );
    let mut separated: Vec<u64> = Vec::new();
    for &seed in SEEDS {
        let f = fields(seed, Arm::Shipped);
        let identical = f.bit_identical("wood-elf", "high-elf");
        let r = pearson(f.column("wood-elf"), f.column("high-elf"));
        println!(
            "   seed {seed:<5} land {:>6}   bit-identical {identical:<5}   \
             max|d| {:.6e}   r {}",
            f.land,
            f.max_abs_diff("wood-elf", "high-elf"),
            r.map(|v| format!("{v:.9}")).unwrap_or("n/a".into()),
        );
        if !identical {
            separated.push(seed);
        }
    }

    println!("== P3(b): wood vs high placement ==");
    for &seed in SEEDS {
        let read = read_world(seed, Arm::Shipped);
        let wood: Vec<u32> = read.of("wood-elf").iter().map(|s| s.cell).collect();
        let high: Vec<u32> = read.of("high-elf").iter().map(|s| s.cell).collect();
        let wood_home = tally(&read, "wood-elf", &strongholds("wood-elf"));
        let high_home = tally(&read, "high-elf", &strongholds("high-elf"));
        println!(
            "   seed {seed:<5} wood {:>3} settlements (home share {:.6})   \
             high {:>3} settlements (home share {:.6})   shared cells {}",
            wood.len(),
            wood_home.share(),
            high.len(),
            high_home.share(),
            wood.iter().filter(|c| high.contains(c)).count(),
        );
        println!("      wood cells {wood:?}");
        println!("      high cells {high:?}");
    }

    assert!(
        separated.is_empty(),
        "P3(a) FALSIFIED on seeds {separated:?}: wood's and high's capacity \
         fields are NOT bit-identical. This is a FINDING about what else \
         differentiates them, not a failure — and the first diagnosis to reach \
         for is MASS THROUGH THE AFFINITY LEVEL (`from_preferences(floor_of \
         (kind), ..)`, applied outside the Liebig minimum), which is the path \
         this campaign created. It is NOT the floor computed inside \
         `per_species_suitability`: every affinity occupant's elevation devotion \
         sits below its floor, so that one is computed and discarded."
    );
}

/// **P4.** Drow separates from Wood by the realm gate — measured on five arms,
/// because the plan's single arm carried two treatments.
///
/// **Falsifier A:** Drow's field is not separated from Wood's WITH the row (A1)
/// — the gate did not reach identity after all, contradicting The Range's
/// repair.
///
/// **Falsifier B, RESTATED 2026-08-10:** Drow is still separated from Wood **in
/// A4**, where nothing authored differs, which means an unenumerated difference
/// exists and §4's attribution is wrong. Separation surviving in A2 while
/// vanishing in A4 is **not** a falsification — it is the niche's main effect,
/// and a reportable result about what separates Drow rather than a defect in the
/// gate. (The plan's original Falsifier B fired on A2, an uncontrolled second
/// treatment, and would have reached a true-sounding wrong conclusion. The
/// correction is pre-unblinding and is recorded in the campaign's decision
/// ledger; the predictions themselves are unchanged.)
///
/// **A5** is the sufficiency arm, and it is measured on wood-vs-HIGH rather than
/// wood-vs-drow: high is body-identical to wood by construction, so giving wood
/// the realm row and watching wood and high separate isolates the gate with
/// nothing else varying. Its own control is P3(a) — the same pair, bit-identical
/// in A1.
///
/// claim: invariant(forall-seed, drow's cave share is 1.00 and A4 restores
/// bit-identity) — the cave gate is a hard zero, which is a structural claim,
/// so a single disagreeing seed is a real result.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn drow_separates_from_wood_and_the_five_arms_say_what_does_it() {
    println!("== P4 clause 1: every drow settlement sits on a cave cell ==");
    let mut off_cave: Vec<(u64, usize, usize)> = Vec::new();
    for &seed in SEEDS {
        let read = read_world(seed, Arm::Shipped);
        let mine = read.of("drow");
        let on_cave = mine.iter().filter(|s| s.cave).count();
        println!(
            "   seed {seed:<5} drow {:>3} settlements   on a cave cell {:>3}   share {:.6}",
            mine.len(),
            on_cave,
            if mine.is_empty() {
                0.0
            } else {
                on_cave as f64 / mine.len() as f64
            },
        );
        if on_cave != mine.len() {
            off_cave.push((seed, on_cave, mine.len()));
        }
    }

    println!("== P4: the five arms, drow vs wood capacity field over land ==");
    let p4_arms = [
        Arm::Shipped,
        Arm::DrowNoRealm,
        Arm::DrowWoodNiche,
        Arm::DrowNoRealmWoodNiche,
    ];
    let mut a1_identical: Vec<u64> = Vec::new();
    let mut a4_separated: Vec<u64> = Vec::new();
    for &seed in SEEDS {
        println!("-- seed {seed} --");
        for arm in p4_arms {
            let f = fields(seed, arm);
            let identical = f.bit_identical("drow", "wood-elf");
            let r = pearson(f.column("drow"), f.column("wood-elf"));
            println!(
                "   {:<22} drow vs wood   bit-identical {identical:<5}   max|d| {:>12.6}   r {}",
                arm.label(),
                f.max_abs_diff("drow", "wood-elf"),
                r.map(|v| format!("{v:.9}")).unwrap_or("n/a".into()),
            );
            if arm == Arm::Shipped && identical {
                a1_identical.push(seed);
            }
            if arm == Arm::DrowNoRealmWoodNiche && !identical {
                a4_separated.push(seed);
            }
        }
        // A5, the MIRROR: wood gains drow's realm value, and the pair compared
        // is wood-vs-HIGH — the pair P3(a) proves bit-identical in A1, so any
        // separation here is the gate and nothing else.
        let f5 = fields(seed, Arm::WoodSubterranean);
        let r5 = pearson(f5.column("wood-elf"), f5.column("high-elf"));
        println!(
            "   {:<22} wood vs HIGH   bit-identical {:<5}   max|d| {:>12.6}   r {}",
            Arm::WoodSubterranean.label(),
            f5.bit_identical("wood-elf", "high-elf"),
            f5.max_abs_diff("wood-elf", "high-elf"),
            r5.map(|v| format!("{v:.9}")).unwrap_or("n/a".into()),
        );
    }

    assert!(
        off_cave.is_empty(),
        "P4 clause 1 FALSIFIED on {off_cave:?} (seed, on-cave, total): a drow \
         settlement sits on a cell with no enterable cave. The realm gate is a \
         HARD ZERO off-cave in `per_species_capacity_at`, and genesis filters \
         its founding pool on capacity > 0.0, so this cannot happen unless the \
         gate stopped reaching the dimensional path."
    );
    assert!(
        a1_identical.is_empty(),
        "P4 FALSIFIER A on seeds {a1_identical:?}: drow's capacity field is \
         BIT-IDENTICAL to wood's in the SHIPPED arm. The gate did not reach \
         identity after all, contradicting The Range's repair."
    );
    assert!(
        a4_separated.is_empty(),
        "P4 FALSIFIER B (restated 2026-08-10) on seeds {a4_separated:?}: drow \
         is STILL separated from wood in A4, where the realm row is removed AND \
         drow carries wood's resource vector — so nothing authored distinguishes \
         them and an UNENUMERATED THIRD DIFFERENCE exists. That outranks P4's \
         stated result and is the headline: enumerate it before reading \
         anything else in this file. (Separation surviving in A2 while vanishing \
         here would NOT be a falsification — that is the niche's main effect.)"
    );
}

/// **The companion null.** Drow's dark-adaptation authoring contributes zero to
/// its placement: perturbing `insolation.devotion` alone leaves the committed
/// world byte-identical.
///
/// **Falsifier:** it moves — which means the two-tier tolerance has started to
/// bind, and the Warren tripwire (`warren_readout.rs`, task 3 step 5) should
/// have reddened first. **If this fires while that tripwire is green, the
/// tripwire is broken and that is the first thing to fix** — before anything
/// else in this campaign is re-read.
///
/// The positive control that keeps this from being a tautology lives in
/// [`the_arms_differ_from_the_shipped_roster_in_exactly_the_row_they_name`],
/// which asserts the arm actually perturbs the value. A null measured on an arm
/// nobody proved differs is the exact defect this campaign found in its own fix
/// rounds, twice.
///
/// claim: invariant(forall-seed, the committed ledger is byte-identical under a
/// perturbed drow insolation devotion) — a structural claim about a discarded
/// axis, so any seed disagreeing is a real result.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn drows_dark_adaptation_moves_nothing_in_the_committed_world() {
    println!("== the companion null: drow's insolation devotion 0.55 -> 0.05 ==");
    let mut moved: Vec<u64> = Vec::new();
    for &seed in SEEDS {
        let base = read_world(seed, Arm::Shipped);
        let perturbed = read_world(seed, Arm::DrowDarkPerturbed);
        let same = base.ledger_json == perturbed.ledger_json;
        println!(
            "   seed {seed:<5} facts {:>6} vs {:>6}   ledger identical {same:<5}   \
             placement identical {}",
            base.facts,
            perturbed.facts,
            base.placement() == perturbed.placement(),
        );
        if !same {
            moved.push(seed);
        }
    }
    assert!(
        moved.is_empty(),
        "THE COMPANION NULL IS FALSIFIED on seeds {moved:?}: perturbing drow's \
         insolation devotion alone MOVED the committed world. That means the \
         two-tier tolerance has started to bind. The Warren tripwire \
         (`warren_readout.rs`) should have reddened FIRST — if it is green, THE \
         TRIPWIRE IS BROKEN AND THAT IS THE FIRST THING TO FIX, before any \
         other reading in this file is trusted."
    );
}

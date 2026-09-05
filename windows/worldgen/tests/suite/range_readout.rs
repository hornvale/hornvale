//! THE RANGE — the preregistered readout (task 4): does declaring a biome
//! affinity actually move a people onto its authored ground?
//!
//! Tasks 1–3 repaired the habitat path and wired the [`BiomeAffinity`] store
//! into BOTH the readout (`per_species_suitability`) and the dimensional path
//! settlement placement consumes (`per_species_capacity_at`), and proved the
//! wiring **inert**: an absent affinity is bit-identical to an explicit `1.0`
//! no-op (`range_affinity.rs`). This file is where the world is supposed to
//! move.
//!
//! # The preregistration
//!
//! Frozen here BEFORE the affinity rows were authored, and before any number in
//! this file's "Measured" tables existed. The two predictions are separate
//! because The Delvers established that **binding** and **differentiating** are
//! distinct properties, and that a kind can gain the first without the second.
//!
//! ## P1″ — suppression is not relocation
//!
//! Declaring gnoll a desert kind must raise the **arid share** of its
//! settlements, not merely lower its settlement count.
//!
//! - **Success:** arid share rises. A *falling* count with a *rising* share is
//!   success — the affinity has concentrated the people rather than merely
//!   thinned it.
//! - **Falsifier:** the count falls while the arid share is flat or falling.
//!   That is suppression: the mechanism reaches the world but only as a
//!   penalty, moving no one anywhere.
//!
//! ## P2 — does it DIFFERENTIATE?
//!
//! Gnoll's mean pairwise Pearson correlation against every other **peopled**
//! kind, over every land vertex, must **fall** when the affinity is declared.
//!
//! - **Falsifier:** it does not fall, or falls on fewer than 2 of the 3 seeds.
//!   That would mean the affinity binds (P1″) without buying spatial
//!   distinctness — The Delvers' result recurring one layer up.
//! - **Seeds 42, 7, 1234** — The Delvers' own seed set, so these numbers are
//!   comparable with its published table.
//! - **Scope limit.** Pearson `r` is invariant under a positive affine
//!   rescale, so it measures how two fields *sort* vertices, not how large they
//!   are. A pair reading `1.0` still admits wholly different absolute
//!   capacities. "Differentiates" here means "the world ranks vertices
//!   differently for these two kinds", nothing more.
//!
//! ## What "arid" means, fixed before the first measurement
//!
//! [`ARID`] is derived from `classify_land`'s own moisture thresholds
//! (`domains/climate/src/biome.rs`), not from the affinity rows. **Two clauses,
//! applied in order**, because the first alone is not sufficient and saying so
//! is cheaper than letting a reader re-derive a different set:
//!
//! 1. *Dry by the lookup's own cuts:* the biome's **moisture band tops out at
//!    ≤ 0.4** and its thermal band is **≥ 0 °C**. This admits **four**, not
//!    three — the three below plus `tundra`, which `classify_land` also returns
//!    on the 0–7 °C branch below moisture 0.30.
//! 2. *Dry because it is arid, not because it is frozen:* `tundra` is then
//!    excluded. `classify_land` reaches it by **cold** — it is the dry half of
//!    both sub-freezing branches and of the 0–7 °C branch — so its dryness is a
//!    consequence of the water being locked up rather than of aridity. A gnoll
//!    at −5 °C is not "at home in the desert", and counting a tundra settlement
//!    as an arid one would let P1″ pass on a relocation to the wrong pole.
//!
//! What survives both clauses is exactly three:
//!
//! ```text
//!   desert               hot        moisture < 0.20
//!   temperate-grassland  temperate  moisture < 0.25
//!   shrubland            temperate  moisture 0.25 – 0.40
//! ```
//!
//! `savanna` fails clause 1 (its band runs to 0.45 — the wet side of the hot
//! tier). `tundra` passes clause 1 and fails clause 2. Fixing all of this
//! before looking is what stops the set being widened until the prediction
//! passes.
//!
//! # Measured, 2026-08-09
//!
//! ## P1″ — CONFIRMED. Seed 42, `the_arid_share_of_gnoll_settlements_rises`
//!
//! ```text
//!   arm                settlements   arid   arid share
//!   affinity ABSENT             20      0     0.000000
//!   affinity SHIPPED             2      1     0.500000
//! ```
//!
//! **The ABSENT arm is the frozen baseline.** `20 settlements, 0 arid, share
//! 0.000000` was measured while `biome_affinity_registry()` still returned no
//! rows at all — this file was written and run RED before the rows existed, and
//! it failed on the prediction rather than on its setup. It is reproduced here
//! by handing the build an emptied `biome_affinity` store, which task 3's
//! `range_affinity.rs` proved bit-identical to the absent case.
//!
//! The count fell 20 → 2 while the share rose 0.000 → 0.500. The
//! preregistration calls that success and it is, but the count is the honest
//! headline alongside it: on seed 42 this affinity removes nine gnoll
//! settlements for every one it relocates. The two descriptive seeds are much
//! kinder, and they are why the seed-42 collapse should not be read as the
//! mechanism's general behaviour:
//!
//! ```text
//!   seed    before                  after
//!     42    20 settlements  0.000     2 settlements  0.500
//!      7     4 settlements  0.000     4 settlements  1.000   (no loss at all)
//!   1234     4 settlements  0.000     2 settlements  1.000
//! ```
//!
//! Seed 42's baseline of 20 is the outlier — the other two seeds start at 4 —
//! and on seed 7 the people keeps every settlement it had and moves all four
//! onto arid ground. Across all three seeds the arid share was **exactly zero
//! before and strictly positive after**.
//!
//! ### These numbers no longer REPRODUCE, and both predictions still pass
//!
//! **The Radiation (C2d), 2026-08-10.** That campaign added six elves to the
//! roster (task 2) and six affinity rows to the registry (task 3), and both
//! moved the world these tables were measured in. The tables are kept as
//! published — they are The Range's claim, not a fixture — but re-running this
//! file now prints different figures, and the difference is not drift:
//!
//! - The `Absent` arm empties the WHOLE registry, so it now removes eight rows
//!   rather than two. It is still "the world before an affinity existed"; it is
//!   no longer "the world before *this campaign*".
//! - Gnoll's seed-42 ABSENT count reads **13**, not 20 — six new peoples
//!   contest the same ground. Its SHIPPED count is still 2 and its arid share
//!   still 0.000 → 0.500, so P1″ passes on the same reading it was frozen on.
//! - P2's mean is over **fourteen** peoples now, not eight, so the mean itself
//!   is a different statistic. Seed 42 reads 0.818414 → 0.727692 (d −0.090722),
//!   a larger fall than the published −0.055475, and every seed still falls.
//! - The Delvers cross-check survives untouched: `gnoll vs kobold` on seed 42
//!   still reads `0.291124` in the ABSENT arm, to six places. That is the
//!   number that would have caught a mistake here, and it did not move.
//!
//! **Amended the same day, by the derivation of the ladder's level.** The
//! fourth ladder step stopped being the literal `0.25` and became each kind's
//! own `sovereignty_floor` (see `biome_affinity_registry`'s doc for the
//! derivation), which moves GNOLL'S OWN ROW as well as the six elf rows —
//! gnoll's default rose 0.25 → 0.495384, its near step 0.70 → 0.848615 and its
//! marginal step 0.45 → 0.722461. The bullets above are superseded by:
//!
//! ```text
//!   seed 42  arm                settlements   arid   arid share
//!            affinity ABSENT             13      0     0.000000
//!            affinity SHIPPED            40     33     0.825000
//! ```
//!
//! P1″ passes far more strongly than it was frozen to: the count now RISES
//! 13 → 40 while the share rises 0.000 → 0.825, where the published reading had
//! it fall 20 → 2 for a share of 0.500. The descriptive seeds move the same
//! way (seed 7: 67 → 31 at 0.645, against 67 → 6 at 1.000; seed 1234: 7 → 6 at
//! 1.000, against 7 → 1). The Range's own honest caveat — "this affinity
//! removes nine gnoll settlements for every one it relocates" — was a
//! consequence of the undeviced level, not of the mechanism, and it does not
//! survive the derivation. P2's seed-42 mean now reads 0.818414 → 0.794172
//! (d −0.024241) — still a fall on all three seeds, smaller because the mask is
//! shallower — and `gnoll vs kobold` in the ABSENT arm is still `0.291124`.
//!
//! ## P2 — CONFIRMED on 3 of 3 seeds
//!
//! Gnoll's mean pairwise Pearson `r` against the other eight peopled kinds,
//! over every land vertex:
//!
//! ```text
//!   seed    before     after     delta
//!     42  0.850738  0.795263  -0.055475
//!      7  0.790098  0.705700  -0.084398
//!   1234  0.857340  0.806109  -0.051230
//! ```
//!
//! Every one of the 24 individual pairs fell, on every seed. The instrument
//! cross-checks against The Delvers: `gnoll vs kobold` on seed 42 reads
//! `0.291124` in the ABSENT arm, which is that campaign's published value to
//! six places.
//!
//! The falls are real but modest — gnoll still correlates at 0.71–0.93 with
//! most peoples after the declaration. Two readings are consistent with that,
//! and this measurement does not separate them: the affinity may be a genuine
//! but partial differentiator, or the peoples may be so alike in their
//! *surviving* (elevation-only) tolerance that a biome factor cannot pull them
//! far apart. The Delvers' finding — that the peoples' live niches are much
//! less separated than their authoring suggests — makes the second worth
//! taking seriously.
//!
//! ## The unpredicted result: one row redistributes the WHOLE placement
//!
//! Neither prediction asked what happens to the *other* peoples, and the answer
//! is much larger than either measured effect. Total settlements barely move,
//! but who founds them does. Measured across [`SEEDS`] — 42, 7 and 1234 — **plus
//! seed 1, which is not in `SEEDS`**: it is `diachronic.rs`'s seed set, where the
//! same suppression showed up independently as gnoll losing its priesthood, and
//! it is quoted here because it is the largest cascade any world on the branch
//! produced. Its `gnoll 61 -> 13` is the same figure that file records at its
//! own line 262. Four seeds, not three, and the difference is named because "the
//! same seeds" would be false:
//!
//! ```text
//!   seed 7    total 274 -> 287     gnoll 4 -> 4  (unchanged)
//!             bugbear  49 -> 153   goblin 48 -> 7   kobold 38 -> 5
//!             hill-dwarf 2 -> 28   desert-dwarf 51 -> 23
//!   seed 1    total 265 -> 184     gnoll 61 -> 13
//!             desert-dwarf 39 -> 91   hobgoblin 42 -> 3   kobold 79 -> 24
//!   seed 42   total 145 -> 143     gnoll 20 -> 2
//!   seed 1234 total  53 ->  53     gnoll  4 -> 2
//! ```
//!
//! The bake is a multi-era **competition** for ground, so suppressing one
//! people frees vertices the rest immediately contest, and the cascade is far
//! larger than the suppression that started it — on seed 7 gnoll's own count
//! does not move at all while bugbear triples. This is the mechanism working
//! rather than misbehaving, but it means "declaring one kind's affinity" is not
//! a local edit, and a future occupant should expect to move every people's
//! numbers. It is also why `the_fauna_occupant_moves_no_settlement` exists: with
//! two rows landing in one commit, attribution needs one of them proven inert.
//!
//! ## THE GLASSHOUSE, `k` re-decided: P1″ IS UN-FALSIFIED, AND THE STATUS IS
//! ## THEREFORE **UNRESOLVED** — read this before the section below it
//!
//! Setting the thermostat's residual fraction to 0.3 warmed the population
//! and re-placed every settlement a fourth time. Measured at seed 42:
//!
//! ```text
//!   seed 42  arm                settlements   arid   arid share
//!            affinity ABSENT              2      0     0.000000
//!            affinity SHIPPED            11     11     1.000000
//! ```
//!
//! Both halves of the Task 4 falsifier are gone: the count RISES (2 → 11)
//! instead of falling, and the share rises 0.000000 → 1.000000 instead of
//! staying flat. The mechanism is exactly what §3.4 of the spec argued and
//! what the campaign was built to test — pre-Glasshouse the world was frozen,
//! so arid land barely existed and an arid-affine people had nowhere to
//! relocate TO; the affinity could only suppress it. Warm the world and the
//! destination exists, so relocation replaces suppression.
//!
//! **AN EXACT 1.000000 IS THE SHAPE A DEFAULT WEARS, so it was checked
//! against a control before being believed.** World-wide, across every
//! attributed settlement at seed 42: the affinity-ABSENT arm is 0.026 arid
//! (7 of 267) and the SHIPPED arm is 0.133 arid (32 of 241). Gnoll's 1.000
//! is therefore ~7.5× the world-wide rate it sits inside, not a world where
//! everything happens to be arid. Two independent corroborations came with
//! it: desert-elf, the roster's OTHER arid-affine people, moved 0.000 → 0.714
//! on the same change without being looked for; and declaring affinities at
//! all raised the world-wide arid share 5× (0.026 → 0.133), so the mechanism
//! moves peoples into arid land generally rather than gnoll specifically.
//!
//! **AND YET THE STATUS IS UNRESOLVED, NOT CONFIRMED.** This prediction has
//! now read FALSIFIED (Task 4) and then UN-FALSIFIED (this change) inside a
//! single campaign, on a single preregistered seed, with nothing between them
//! but one constant that no part of the affinity mechanism reads. A result
//! that inverts when an unrelated constant moves has not earned "confirmed" —
//! it has demonstrated that one world is too thin a basis for the claim, which
//! is the standing reading this file already applies to its own descriptive
//! seeds. The honest next step is a wider sweep, not a stamp.
//!
//! **THE ASSERTION IS THEREFORE BACK TO THE PREREGISTERED ONE** — the
//! DIRECTION (`after.share() > before.share()`), which is what P1″ actually
//! predicted — rather than the exact four-integer tuple Task 4 pinned. That
//! tuple was defensible when it recorded a falsification, but it has since
//! interrupted this campaign twice for reasons that had nothing to do with the
//! claim, because a directional prediction pinned to exact counts breaks on
//! every physics change. The numbers stay in the printed readout, where they
//! inform without gating.
//!
//! ## THE GLASSHOUSE, Stage B Task 4: P1″ is FALSIFIED at seed 42 (SUPERSEDED
//! ## by the section above; preserved because a falsification that was later
//! ## reversed is evidence about the claim's fragility, not a mistake to erase)
//!
//! The thermostat (a damped, greenhouse-forced insolation baseline replacing
//! the fixed 288 K blackbody one, plus Task 5's area-mean-zero latitude
//! profile) re-placed every settlement in every world again. Measured:
//!
//! ```text
//!   seed 42  arm                settlements   arid   arid share
//!            affinity ABSENT             10      0     0.000000
//!            affinity SHIPPED             2      0     0.000000
//! ```
//!
//! **This is the falsifier the module header names, not a re-confirmation.**
//! The count falls 10 → 2, exactly the shape The Range's own table showed
//! (20 → 2), but the share stays flat at 0.000000 instead of rising — no
//! arid land survives within reach of either arm's gnoll settlements at this
//! seed under the new climate. That is suppression, not relocation, by the
//! test's own definition, and it is asserted as the measured state below
//! rather than rescued: nothing in Tasks 4/5 touches `BiomeAffinity`,
//! `per_species_suitability`'s affinity wiring, or the arid classification
//! this file fixes before the first measurement — only the world's
//! temperature and (through it) which land reads arid moved. A falsified
//! prediction is a finding (spec's own standing rule), not a defect to patch
//! quietly.
//!
//! The two descriptive seeds still confirm the ORIGINAL prediction: seed 7
//! rises 0.000 → 0.555556 (23 → 9 settlements) and seed 1234 rises 0.000 →
//! 0.666667 (5 → 3 settlements) — both a falling count with a RISING share,
//! the success shape. So the mechanism itself still relocates gnoll onto arid
//! ground where arid ground exists; seed 42's specific geography, under this
//! climate, apparently no longer offers gnoll any to relocate onto. Read
//! seed 42 as a per-seed falsification of a `forall`-style hard claim, not as
//! evidence the mechanism broke — exactly the caveat P2's doc comment already
//! carries for its own majority threshold. Post-unblinding re-measure,
//! declared per decision 0016.

// `terrain_of` and friends are named derivation entry points (decision 0092);
// a probe measuring a handful of worlds is exactly the site the allowance is
// for.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{ComponentStore, Seed, Value, Vertex};
use hornvale_species::BiomeAffinity;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, build_world_to_with_artifacts, per_species_suitability, sky_of,
};

/// The arid land biomes, fixed before the first measurement — see the module
/// header for the rule that produced this list and why `savanna` and `tundra`
/// are not on it.
const ARID: &[&str] = &["desert", "shrubland", "temperate-grassland"];

/// The seeds P2 reports, in order — The Delvers' set
/// (`windows/worldgen/tests/delver_distinctness.rs`), so the correlations here
/// sit alongside its published table rather than beside it in different units.
const SEEDS: &[u64] = &[42, 7, 1234];

/// Which affinity store a measurement arm builds against.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Arm {
    /// The pre-campaign world: an EMPTY `biome_affinity` store, which task 3's
    /// `an_absent_affinity_is_bit_identical` proved is exactly the physics that
    /// shipped before this campaign.
    Absent,
    /// The world this task declares: `biome_affinity_registry()` as authored.
    Shipped,
    /// The shipped registry with the FAUNA occupant (`woolly-mammoth`) removed,
    /// leaving gnoll's row alone — the attribution control.
    PeopledRowOnly,
}

impl Arm {
    fn label(self) -> &'static str {
        match self {
            Arm::Absent => "affinity ABSENT",
            Arm::Shipped => "affinity SHIPPED",
            Arm::PeopledRowOnly => "gnoll row ONLY",
        }
    }
}

/// The canonical component set, with `biome_affinity` set to this arm's store.
/// Every other store is the shipped registry, so the arms differ in exactly one
/// component — which is what makes the comparison attributable.
fn components(arm: Arm) -> WorldComponents {
    let mut wc = WorldComponents::assemble().expect("components assemble");
    match arm {
        Arm::Absent => wc.biome_affinity = ComponentStore::new(),
        Arm::Shipped => {}
        Arm::PeopledRowOnly => {
            wc.biome_affinity = wc
                .biome_affinity
                .iter()
                .filter(|(kind, _)| kind.0 != "woolly-mammoth")
                .map(|(kind, aff)| (*kind, aff.clone()))
                .collect();
        }
    }
    wc
}

/// One arm's gnoll settlement tally on one seed.
#[derive(Debug, Clone)]
struct Tally {
    /// Every settlement `peopled-by` gnoll.
    total: usize,
    /// How many of those sit on an [`ARID`] biome.
    arid: usize,
}

impl Tally {
    /// The arid fraction of this people's settlements. `0.0` when the people
    /// founded nothing — a people with no settlements has no arid share, and
    /// returning `NaN` would let a `>` comparison pass or fail by accident.
    fn share(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.arid as f64 / self.total as f64
        }
    }
}

/// Build the world at `seed` under `arm` and tally `species`'s settlements by
/// biome.
///
/// Biome is resolved from the settlement's committed `cell-id` through
/// `climate.biome_at`, NOT from its `biome` text fact: the descriptor facts are
/// written by the naming pass, and reading the vertex keeps this measurement
/// independent of which build depth committed which descriptor.
///
/// **`Full` depth, and the shallower rung is not an option.** `Settlements`
/// depth runs the bake and places all 145 of seed 42's settlements, but commits
/// **no `peopled-by` fact** — measured 2026-08-09, every settlement reads
/// `species_of == None` there, and `Full` attributes the identical 145 across
/// nine peoples. A tally built at `Settlements` depth is therefore not a
/// smaller measurement, it is an empty one, and its zero would read as "gnoll
/// founds nothing" rather than as "this depth does not say who founded it".
fn tally(seed: u64, arm: Arm, species: &str) -> Tally {
    let built = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &components(arm),
        BuildDepth::Full,
    )
    .expect("probe seed builds");
    let world = &built.world;
    let climate = built
        .climate
        .as_ref()
        .expect("Settlements depth has climate");

    let mut t = Tally { total: 0, arid: 0 };
    let mut placed = 0usize;
    let mut attributed = 0usize;
    for fact in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        placed += 1;
        let founder = hornvale_species::species_of(world, fact.subject);
        if founder.is_some() {
            attributed += 1;
        }
        if founder.as_deref() != Some(species) {
            continue;
        }
        let vertex = match world
            .ledger
            .value_of(fact.subject, hornvale_settlement::VERTEX_ID)
        {
            Some(Value::Number(n)) => Vertex(*n as u32),
            _ => panic!("a committed settlement must carry a cell-id"),
        };
        t.total += 1;
        if ARID.contains(&climate.biome_at(vertex).name()) {
            t.arid += 1;
        }
    }
    // The depth trap, asserted so it cannot recur silently: at `Settlements`
    // depth every one of these settlements exists and NONE carries a founder,
    // so a share computed there is `0/0` dressed as a measurement.
    assert!(placed > 0, "seed {seed} placed no settlements at all");
    assert_eq!(
        placed,
        attributed,
        "every placed settlement must name its people; {} of {placed} did not, \
         which means this build depth commits placement without attribution and \
         the tally below is empty rather than small",
        placed - attributed
    );
    t
}

/// Every settlement of the world at `seed` under `arm`, as
/// `(founding people, vertex)` in ledger-commit order — the whole placement, not
/// one people's slice.
fn placement(seed: u64, arm: Arm) -> Vec<(String, u32)> {
    let built = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &components(arm),
        BuildDepth::Full,
    )
    .expect("probe seed builds");
    let world = &built.world;
    world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .map(|f| {
            let who = hornvale_species::species_of(world, f.subject)
                .expect("a committed settlement names its people at Full depth");
            let vertex = match world
                .ledger
                .value_of(f.subject, hornvale_settlement::VERTEX_ID)
            {
                Some(Value::Number(n)) => *n as u32,
                _ => panic!("a committed settlement must carry a cell-id"),
            };
            (who, vertex)
        })
        .collect()
}

/// Pearson's `r` between two equal-length samples.
///
/// Two-pass (means first, then centred sums) rather than the algebraically
/// equivalent `E[xy] - E[x]E[y]` form, which cancels catastrophically when the
/// mean is large relative to the spread. Copied in shape from
/// `delver_distinctness.rs` deliberately: P2's whole value is that its numbers
/// are comparable with that campaign's, which requires the same estimator.
///
/// Panics on a constant sample rather than returning `NaN`, for the reason that
/// file states: a `NaN` compares false against every threshold in both
/// directions, so a degenerate field would silently satisfy any assertion.
fn pearson(xs: &[f64], ys: &[f64]) -> f64 {
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
    assert!(
        sxx > 0.0 && syy > 0.0,
        "a field CONSTANT over land has no correlation with anything; \
         variances were {sxx:.6e} and {syy:.6e}"
    );
    sxy / (sxx * syy).sqrt()
}

/// Gnoll's mean pairwise correlation against every other peopled kind, over the
/// land vertices of the world at `seed`, under `arm`.
///
/// Returns `(mean_r, per_pair)` with `per_pair` ascending by the other kind's
/// name. The peopled set is `SocialForm::Settled` read off the biosphere store —
/// the same resolution `bake_history_from` performs for an unpinned world, so
/// this measures the kinds gnoll actually competes with for ground.
///
/// The three species slices are built from ONE `wc.biosphere` iteration each, in
/// the store's ascending-`KindId` order, exactly as the shipped path builds
/// them: a slice that drifted out of alignment would score a kind against
/// another kind's realm or affinity and every number here would be quietly
/// wrong, with nothing to notice it.
fn gnoll_mean_correlation(seed: u64, arm: Arm) -> (f64, Vec<(String, f64)>) {
    let wc = components(arm);
    let built = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
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
    let generated = sky.generated();
    let system = generated.system();
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning {
                day_std: day.as_std_days(),
            }
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
    let species_affinity: Vec<Option<BiomeAffinity>> = wc
        .biosphere
        .iter()
        .map(|(kind, _)| wc.biome_affinity.get(kind).cloned())
        .collect();

    let peoples: Vec<&'static str> = wc
        .biosphere
        .iter()
        .filter(|(_, b)| b.social_form == hornvale_species::SocialForm::Settled)
        .map(|(kind, _)| kind.0)
        .collect();

    let per_species = per_species_suitability(
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

    let land: Vec<Vertex> = geo.vertices().filter(|&c| !terrain.is_ocean(c)).collect();
    let column = |name: &str| -> Vec<f64> {
        let idx = roster
            .iter()
            .position(|k| *k == name)
            .unwrap_or_else(|| panic!("{name:?} has no biosphere row"));
        let (_, k) = per_species
            .iter()
            .find(|(tag, _)| *tag as usize == idx)
            .unwrap_or_else(|| panic!("no suitability field for {name:?}"));
        land.iter().map(|&c| *k.get(c)).collect()
    };

    let gnoll = column("gnoll");
    let mut pairs: Vec<(String, f64)> = peoples
        .iter()
        .filter(|n| **n != "gnoll")
        .map(|n| (n.to_string(), pearson(&gnoll, &column(n))))
        .collect();
    pairs.sort_by(|a, b| a.0.cmp(&b.0));
    let mean = pairs.iter().map(|(_, r)| *r).sum::<f64>() / pairs.len() as f64;
    (mean, pairs)
}

/// **P1″.** The arid share of gnoll's settlements must RISE once the affinity is
/// declared. A falling settlement count with a rising share is the success case;
/// a falling count with a flat or falling share is the falsifier, and would mean
/// the mechanism suppresses a people without relocating it.
///
/// Matched pair on one world: both arms build seed 42 from the same registries,
/// differing in the `biome_affinity` store alone.
///
/// claim: readout(3 seeds printed; the ASSERTION is seed 42 ALONE) — the loop
/// over the other two seeds prints and asserts nothing. Seed 42 is the
/// preregistered subject; 7 and 1234 were added after unblinding it, purely
/// because the post-declaration denominator there is two settlements, and are
/// reported so a reader can see whether the reading survives a second world.
///
/// **THE GLASSHOUSE, `k` re-decided — UN-FALSIFIED at seed 42, status
/// UNRESOLVED (module header has the full account).** Superseding the Task 4
/// note below: both halves of that falsifier are gone (count 2 -> 11, share
/// 0.000000 -> 1.000000 against a world-wide 0.133), and the assertion is
/// back to the PREREGISTERED direction rather than an exact tuple.
///
/// **THE GLASSHOUSE, Stage B Task 4 — FALSIFIED at seed 42 (superseded).** `before` falls 10 → `after` 2 settlements while the
/// arid share stays flat at 0.000000 — suppression, not relocation, by this
/// test's own definition. The assertion below is now PINNED AT THE
/// FALSIFICATION rather than at the original prediction, so a further change
/// that moves this measurement again is caught rather than silently believed
/// to still confirm P1″. Do not read a green run of this test as "P1″
/// holds" — read the printed numbers, which is exactly this file's own
/// standing instruction for a preregistered result.
#[test]
fn the_arid_share_of_gnoll_settlements_rises() {
    let before = tally(42, Arm::Absent, "gnoll");
    let after = tally(42, Arm::Shipped, "gnoll");

    println!("== P1\u{2033}: gnoll settlements by biome, seed 42 ==");
    println!("   arid biomes: {ARID:?}");
    for (arm, t) in [(Arm::Absent, &before), (Arm::Shipped, &after)] {
        println!(
            "   {:<16} settlements {:>4}   arid {:>4}   arid share {:.6}",
            arm.label(),
            t.total,
            t.arid,
            t.share()
        );
    }

    // DESCRIPTIVE, NOT ASSERTED. Seed 42 is the preregistered subject and the
    // assertion below is on it alone. These two are printed because the
    // post-declaration denominator on seed 42 is *two settlements*, and a share
    // over n=2 is thin enough that reporting it alone would be an anecdote —
    // "one world is an anecdote" cuts both ways, including when the world
    // agrees with you. Added after unblinding seed 42; they can only ever
    // weaken the reading, which is why they are here rather than absent.
    for &seed in &SEEDS[1..] {
        let b = tally(seed, Arm::Absent, "gnoll");
        let a = tally(seed, Arm::Shipped, "gnoll");
        println!(
            "   [descriptive] seed {seed:<5} before {:>3} settlements / arid \
             share {:.6}   after {:>3} / {:.6}",
            b.total,
            b.share(),
            a.total,
            a.share()
        );
    }

    assert!(
        before.total > 0,
        "the baseline arm must found some gnoll settlements, or this \
         measurement has no subject"
    );
    // A share over an empty denominator is not a measurement. Task 4's own
    // depth-trap comment above makes the same point about `Settlements`
    // depth; this is the arm-level version of it, and it is what stops the
    // directional assertion below from passing on `0/0 > 0/0`.
    assert!(
        after.total > 0,
        "the shipped arm founded no gnoll settlements at all, so its arid \
         share is 0/0 dressed as a measurement — re-measure before reading \
         anything from it"
    );

    // BACK TO THE PREREGISTERED ASSERTION (`k` re-decided; module header's
    // UN-FALSIFIED section carries the account). Task 4 pinned the exact
    // tuple (10, 0, 2, 0) because it was recording a falsification, which was
    // the right call then. But P1″ predicts a DIRECTION, and a directional
    // prediction pinned to four exact integers breaks on every physics change
    // — it has interrupted this campaign twice for reasons unrelated to the
    // claim. The tuple's job (make a movement visible) is done by the printed
    // readout above, which no reader can skip because this file's standing
    // instruction is to read the numbers rather than the pass.
    //
    // Measured here: before 2 settlements / 0 arid / 0.000000, after 11 / 11 /
    // 1.000000, against a world-wide arid share of 0.133 in the same arm. The
    // status is UNRESOLVED, not confirmed — see the module header for why a
    // prediction that read falsified and then un-falsified inside one campaign
    // on one seed needs a wider sweep rather than a stamp.
    assert!(
        after.share() > before.share(),
        "P1\u{2033} does not hold at seed 42: the arid share did not rise once \
         the affinity was declared ({:.6} -> {:.6} over {} -> {} settlements). \
         A FALLING count with a flat or falling share is the falsifier this \
         test was written for — suppression rather than relocation. Record it \
         in the module header as a finding; do NOT weaken this assertion",
        before.share(),
        after.share(),
        before.total,
        after.total
    );
}

/// **P2.** Gnoll's mean pairwise correlation against the other peopled kinds
/// must FALL on at least 2 of the 3 seeds. Reported in full either way — a
/// falsification here is the finding, and the numbers are the deliverable.
///
/// claim: rate(3 seeds, gnoll's mean pairwise r falls on >= 2 of 3) — a
/// majority threshold rather than `forall-seed`, frozen at that value before
/// the affinity rows existed. A per-seed sign is the right quantifier here:
/// one world's land mask can be arid enough (or barren enough) that a biome
/// factor reorders little, and the prediction is about the mechanism, not
/// about any one globe.
#[test]
fn the_affinity_differentiates_gnoll_from_the_other_peoples() {
    let mut fell = 0usize;
    println!("== P2: gnoll vs each other peopled kind, Pearson r over land ==");
    for &seed in SEEDS {
        let (before_mean, before_pairs) = gnoll_mean_correlation(seed, Arm::Absent);
        let (after_mean, after_pairs) = gnoll_mean_correlation(seed, Arm::Shipped);
        println!("-- seed {seed} --");
        for ((name, b), (_, a)) in before_pairs.iter().zip(after_pairs.iter()) {
            println!(
                "   gnoll vs {name:<16} before {b:>9.6}   after {a:>9.6}   d {:>9.6}",
                a - b
            );
        }
        println!(
            "   MEAN                      before {before_mean:>9.6}   after {after_mean:>9.6}   \
             d {:>9.6}",
            after_mean - before_mean
        );
        if after_mean < before_mean {
            fell += 1;
        }
    }
    assert!(
        fell >= 2,
        "P2 FALSIFIED: gnoll's mean pairwise correlation fell on only {fell} of \
         {} seeds. The affinity binds without buying spatial distinctness — The \
         Delvers' result recurring one layer up.",
        SEEDS.len()
    );
}

/// Every authored affinity row must be **non-uniform**, must spell its biome
/// keys correctly, must not contain `0.0` unless a hard exclusion is meant, and
/// must not exceed `1.0`.
///
/// All four are silent-failure modes, which is why they are asserted rather
/// than trusted:
///
/// 1. **Uniformity is a provable no-op for placement.** Genesis and `best_home`
///    rank vertices in the kind's own units, so a constant factor cannot reorder
///    anything — a uniform row would move the world's absolute numbers and
///    nobody's location.
/// 2. **A misspelled key is inert.** `BiomeAffinity::factor` falls back to
///    `default` for any name it does not find, so `"temperate grassland"` (space,
///    not hyphen) would compile, run, and quietly do nothing.
/// 3. **`0.0` is a hard exclusion, not a strong preference.** Genesis filters its
///    founding pool on `caps_now()[pidx].at(c) > 0.0` — "a proto-site a people
///    cannot feed is not a founding, it is a death two epochs later". Neither
///    row shipped here means *never*, so neither may carry a zero.
/// 4. **`1.0` is the ceiling, and crossing it changes what the mechanism IS.**
///    Below `1.0` an affinity is a *mask*: every other kind carries an implicit
///    unrestricted `1.0`, so a declared row can only ever subtract capacity from
///    the kind that declares it, and no row can lift a kind above the field the
///    world already gave it. A factor above `1.0` makes the same store a
///    *boost*, and every result this campaign measured — the arid-share rise, the
///    correlation fall, the whole-placement cascade — was measured under the mask
///    reading. The spec pre-commits permitting a boost as the repair path if
///    binding proves too weak, which makes it a spec-level decision to record,
///    not a literal to edit; `domains/species/src/lib.rs`'s registry doc states
///    it in the same words.
///
///    This test is the ONLY enforcement of that bound. `BiomeAffinity`'s fields
///    are `pub`, so a validating `new()` would be advisory — any caller can still
///    write the struct literal — and adding one would create the appearance of
///    enforcement without the fact of it. The registry is the only production
///    construction site, and this test reads the registry.
#[test]
fn every_authored_affinity_row_is_well_formed() {
    let names: Vec<&'static str> = hornvale_climate::biome::ALL
        .iter()
        .map(|b| b.name())
        .collect();
    let registry = hornvale_species::biome_affinity_registry();
    assert!(
        registry.iter().next().is_some(),
        "task 4 ships authored rows; an empty registry means the declaration \
         was lost"
    );

    for (kind, aff) in registry.iter() {
        for (key, factor) in &aff.by_biome {
            assert!(
                names.contains(key),
                "{kind:?} names a biome {key:?} that does not exist — \
                 `factor` falls back to `default` for an unknown key, so a \
                 misspelling is silently inert. Known: {names:?}"
            );
            assert!(
                *factor > 0.0,
                "{kind:?} sets {key:?} to {factor}; 0.0 is a HARD EXCLUSION \
                 (genesis filters its pool on capacity > 0.0), not a strong \
                 preference"
            );
            assert!(
                *factor <= 1.0,
                "{kind:?} sets {key:?} to {factor}, above the 1.0 CEILING. An \
                 affinity is a MASK: every undeclared kind carries an implicit \
                 1.0, so a row below the ceiling can only subtract capacity from \
                 the kind that declares it. A factor above 1.0 makes the same \
                 store a BOOST, which is a different mechanism — it lifts a kind \
                 above the field the world gave it, and every result this \
                 campaign measured was measured under the mask reading. The spec \
                 pre-commits permitting a boost as the repair path if binding \
                 proves too weak; taking it is a decision to RECORD, not a \
                 literal to edit"
            );
        }
        assert!(aff.default > 0.0, "{kind:?} has a zero default");
        assert!(
            aff.default <= 1.0,
            "{kind:?} has a default of {}, above the 1.0 CEILING — see the \
             per-biome message above; the default is the factor every one of the \
             ~20 unlisted biomes takes, so raising it past 1.0 boosts the kind \
             nearly everywhere at once",
            aff.default
        );

        // No biome may be listed twice. `BiomeAffinity::factor` returns the
        // FIRST match, so a duplicated key resolves deterministically to one
        // value and silently ignores the other — the identical silent-inertness
        // failure mode as the misspelling guarded above, and the reason that one
        // is guarded applies here unchanged. Left unguarded, an author who edits
        // `("savanna", 0.45)` to `0.70` by appending a second row rather than
        // changing the first gets no error and no effect.
        let mut seen: std::collections::BTreeSet<&'static str> = std::collections::BTreeSet::new();
        for (key, _) in &aff.by_biome {
            assert!(
                seen.insert(key),
                "{kind:?} lists {key:?} more than once; `factor` returns the \
                 FIRST match, so the later value is silently inert — exactly the \
                 misspelling failure mode, reached by a different route"
            );
        }

        // Non-uniform across the WHOLE catalog, resolved through `factor` — the
        // same accessor the pipeline uses, so this cannot pass on a row whose
        // overrides all happen to equal its default.
        let distinct: std::collections::BTreeSet<u64> =
            names.iter().map(|n| aff.factor(n).to_bits()).collect();
        assert!(
            distinct.len() >= 2,
            "{kind:?}'s affinity is UNIFORM across every biome, which is a \
             provable no-op for placement: genesis ranks vertices in the kind's \
             own units, so a constant factor reorders nothing"
        );
    }
}

/// **Attribution.** The fauna occupant must move NO settlement, so every
/// placement change this campaign produces is gnoll's row and gnoll's row only.
///
/// This is not a formality. Declaring gnoll's affinity redistributes the WHOLE
/// competitive placement, not just gnoll's share of it — measured on seed 7,
/// gnoll's own count is unchanged at 4 while bugbear goes 49 → 153, goblin
/// 48 → 7 and kobold 38 → 5. The bake is a multi-era competition for ground, so
/// suppressing one people frees vertices the rest then contest. With two occupants
/// declared in one commit, "which row caused that?" has no answer unless one of
/// them is proven placement-inert — which is exactly why the second occupant was
/// chosen from the fauna (`SocialForm::Gregarious`), outside the bake's
/// `Settled` roster.
///
/// Asserted on all three seeds because the claim is structural (a fauna kind is
/// not in the peopled roster at all), so a single seed agreeing could be luck
/// in a chaotic pipeline while the claim was false.
///
/// claim: invariant(forall-seed, placement is identical with and without the
/// fauna occupant's row) — a structural claim about roster membership, not a
/// statistical one.
#[test]
fn the_fauna_occupant_moves_no_settlement() {
    for &seed in SEEDS {
        let with_both = placement(seed, Arm::Shipped);
        let gnoll_only = placement(seed, Arm::PeopledRowOnly);
        println!(
            "   seed {seed:<5} {} settlements, identical with and without the \
             fauna row: {}",
            with_both.len(),
            with_both == gnoll_only
        );
        assert_eq!(
            with_both, gnoll_only,
            "seed {seed}: the woolly-mammoth row moved a settlement. It is \
             `SocialForm::Gregarious` and must never enter the bake's `Settled` \
             roster — if it does, this campaign's placement movement is no \
             longer attributable to gnoll alone"
        );
    }
}

/// **The admission test, enforced.** A row may only be granted to a kind whose
/// authored climate curves the Liebig minimum currently DISCARDS — otherwise
/// the affinity duplicates a preference the model already applies, and any
/// movement it produces is unattributable.
///
/// `tolerance_liebig` floors temperature/moisture/insolation by
/// `sovereignty_floor(mass, potency)` and floors elevation by `0.0`. A floored
/// axis never reads below its floor; the unfloored one peaks at its own
/// `devotion`. So `elevation.devotion < sovereignty_floor` makes elevation the
/// minimum at every vertex of every world, and the other three curves contribute
/// nothing at all.
///
/// Asserted rather than left to the authoring comment because the inputs live
/// three places apart: a future edit to a kind's `mass`, its `potency`, or its
/// `elevation.devotion` could turn a row into a double count without touching
/// the row, and nothing else in the workspace would notice.
///
/// Measured 2026-08-09, extended 2026-08-10 (The Radiation task 3 — six elves,
/// each clearing the bar by ≥ 0.12):
///
/// ```text
///   kind              mass kg   potency   floor      elev devotion
///   gnoll               136.1      0.00   0.495384        0.40
///   woolly-mammoth     6000.0      0.00   0.692367        0.50
///   desert-elf           50.0      0.00   0.421703        0.30
///   drow                 52.0      0.00   0.424802        0.30
///   high-elf             55.0      0.00   0.429202        0.30
///   sea-elf              58.0      0.00   0.433335        0.30
///   snow-elf             60.0      0.00   0.435955        0.30
///   wood-elf             55.0      0.00   0.429202        0.30
/// ```
#[test]
fn every_occupant_has_climate_curves_the_minimum_currently_discards() {
    let biosphere = hornvale_species::biosphere_registry();
    let registry = hornvale_species::biome_affinity_registry();
    let mut checked = 0usize;
    for (kind, _) in registry.iter() {
        let bio = biosphere
            .get(kind)
            .unwrap_or_else(|| panic!("{kind:?} has an affinity but no biosphere row"));
        let floor = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
        let devotion = bio.condition_niche.elevation.devotion;
        println!(
            "   {:<16} mass {:>8.1} potency {:.2}  floor {floor:.6}  elev devotion {devotion:.2}",
            kind.0,
            bio.mass.kilograms(),
            bio.potency,
        );
        assert!(
            devotion < floor,
            "{kind:?} may NOT carry a biome affinity: its elevation devotion \
             ({devotion}) is at or above its sovereignty floor ({floor}), so \
             the Liebig minimum does NOT discard its temperature/moisture/\
             insolation curves and this row would DOUBLE-COUNT a preference \
             the model already applies"
        );
        checked += 1;
    }
    assert!(checked > 0, "no occupant was checked; the guard is vacuous");
}

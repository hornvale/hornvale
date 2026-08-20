//! THE UNDERWORLD, Task 9 — the separation readout, and the gate it opens or
//! closes.
//!
//! Spec §5's H1–H5, plus the four requirements the "H2 STRENGTHENED
//! 2026-08-17, PRE-UNBLINDING, POST-TASK-8" block added, measured against
//! `mountain-dwarf` and `duergar` as **candidates in a test-local fixture**.
//! They are deliberately NOT in `hornvale_species`' registries while this file
//! measures them: admitting them first and measuring afterwards would make the
//! roster the thing under test, which is the failure The Delvers reached the
//! expensive way (`b3583640`).
//!
//! # What this file owns, and what it cites
//!
//! H1, H2 (floor and overlap), and H2c are measured here, because all three
//! need a world and two kinds that do not exist yet. H3, H4 and H5 are
//! properties of the water table and of the underworld corpus, already
//! instrumented and **asserted** elsewhere, and are cited rather than copied:
//!
//! - **H3** — `windows/worldgen/tests/underworld_water_table_probe.rs`.
//! - **H4** — `domains/climate/tests/underworld.rs::a3_the_underworld_lands_in_the_cave_region`.
//! - **H5** — `domains/climate/tests/underworld.rs::light_takes_at_most_two_distinct_values`.
//!
//! A second copy of any of those would be a second instrument that can drift
//! from the first, which is exactly what decision 0094 exists to refuse. The
//! campaign's report carries their numbers; this module carries the pointer.
//!
//! # The mandatory disclosure, and why the fit tables come first
//!
//! Spec §5 requires each candidate niche's 3×5 [`chamber_fit`] table and its
//! argmax to be computed and recorded **before any world is built**, because
//! `seat_at` takes `argmax chamber_fit` over the rungs a cave *reaches* — a
//! prefix of the ΔT-ascending ladder — so a kind's modal rung is its authored
//! argmax **censored by terrain**, and nothing else. If the measured modal
//! rung equals the fit-table argmax for both kinds, the world contributed
//! nothing and the floor was decided by authoring.
//!
//! [`the_candidate_fit_tables_and_the_elevation_precondition`] is that record.
//! It builds no world, is not `#[ignore]`d, and runs in the commit gate.
//!
//! **The disclosure discharges PER KIND, and only one kind carries the strong
//! form.** Stating it jointly ("neither kind's modal rung equals its argmax")
//! is true but flattens the difference that matters:
//!
//! - **duergar** — argmaxes `Nadir` / `Nadir` / `Underdeep`; measured modal
//!   rungs `Undercroft` / `Underdeep` / `Undercroft`. On two of three seeds the
//!   mode is `Undercroft`, which is **no formation's argmax at all**, so
//!   terrain did not merely pick between authored answers — it produced one
//!   that was not on offer. This is the strong form, and it is what makes the
//!   seed-1234 floor failure world-caused rather than authoring-caused.
//! - **mountain-dwarf** — argmaxes `Shallows` / `Shallows` / `Undercroft`;
//!   measured modal rungs `Shallows` / `Undercroft` / `Undercroft`. Every
//!   measured mode **is** one of its own three formation argmaxes, so terrain
//!   only ever selected among them. That is the weak form: consistent with
//!   truncation biting, and equally consistent with the formation mix deciding
//!   which authored answer won. For this kind the disclosure is NOT fully
//!   discharged, and a reader should treat mountain's mode as substantially
//!   an authoring artefact.
//!
//! # The elevation precondition (spec §4.3's amendment)
//!
//! `tolerance_liebig` floors temperature/moisture/insolation at
//! `sovereignty_floor(mass, potency)` and passes elevation a literal `0.0`, so
//! a kind whose elevation devotion sits below its own floor is the Liebig
//! minimum on elevation at **every** cell and no chamber condition can reach
//! its score. Drow sits at devotion 0.30 against a floor of 0.424802, which is
//! why `warren_readout`'s tripwire read ratio = 1.000 before and after Task 5.
//! Both candidates here are authored above their floor, and the assertion
//! computes the floor live from [`hornvale_kernel::sovereignty_floor`] rather
//! than reading it off a table — The Delvers' own plan table was wrong in the
//! fourth decimal for two of its three kinds.
//!
//! # THE READOUT ITSELF — measured 2026-08-18, seeds 42 / 7 / 1234
//!
//! **Every number the campaign's verdict turns on lives here, in the file that
//! produces it.** It did not, until this was added: the module doc recorded
//! the mandatory disclosure and the elevation precondition and no readout at
//! all, so the decisive figures existed only in a report and in a
//! `println!` inside a heavy-tier-ignored test. The campaign's own
//! stated remedy for its headline defect is *make the measurement re-runnable
//! from the tree rather than correct in a report*, and this file was the one
//! result it had not been applied to. Every sibling probe in this campaign
//! already carries its numbers in its doc (the water table's 141 lines,
//! chamber conditions' 215, the ladder's 123); this one carried none.
//!
//! Regenerate the whole block with:
//!
//! ```text
//! cargo test -p hornvale-worldgen --test suite --release \
//!     -- underworld_separation::the_separation_readout --ignored --nocapture
//! ```
//!
//! ## H1 — the ladder varies (needs ≥ 4 of 5 rungs, and ≤ 70% in one)
//!
//! ```text
//! seed    caves  chambers   Undercroft  Shallows    Deeps  Underdeep  Nadir     worst rung
//!   42      874      5602        31.3%     28.7%    23.3%       9.4%     7.2%        31.3%
//!    7     1681     11754        28.8%     27.0%    17.2%      14.7%    12.4%        28.8%
//! 1234     1266      9320        27.7%     25.1%    22.3%      13.9%    11.0%        27.7%
//! ```
//!
//! **HOLDS**: 5 of 5 rungs occupied on every seed, worst share 31.3%.
//!
//! **RE-PINNED 2026-08-20 for `chamber/v3`** (The Stope, Task 1). The address
//! gained a `floor`, `slot` became `branch`, and the deepest rung was renamed
//! `Sunless` → `Nadir` — all three spelled into `chamber_key`, so every
//! existence draw in every world relocated. Under `chamber/v2` this table read
//! 5604 / 11747 / 9384 chambers and 31.3 / 27.9 / 26.8% worst share; the
//! counts moved by −0.04 / +0.06 / −0.68%, which is a re-draw at the same
//! `EXISTENCE_DENSITY`, not a change of reach.
//!
//! **Nothing else in this readout moved, and that is a finding rather than a
//! coincidence.** Every H2 figure below — seated-rung histograms, the quartile
//! overlap, the Jaccards, the distinct-value counts, `maybe_raid`'s
//! co-seating — is byte-identical across the epoch, because [`seat_at`] is a
//! function of the RUNG and the column, never of which chamber addresses
//! exist. Chamber existence and delve seating turned out not to be joined at
//! all. The measurement here is a floor-0 slice of the new lattice, matching
//! how the probe enumerated the lattice before floors existed.
//!
//! ## H2 floor — the modal seated rung (needs the two kinds to differ)
//!
//! ```text
//! seed   mountain-dwarf              duergar                       differ?
//!   42   Shallows   (58.7%)          Undercroft (69.0%)            yes
//!    7   Undercroft (57.3%)          Underdeep  (50.1%)            yes
//! 1234   Undercroft (56.1%)          Undercroft (49.8%)            NO
//!
//! lattice band histograms (band 0 = Undercroft)
//!   42   {0: 361, 1: 513}            {0: 603, 1:  14, 3: 257}
//!    7   {0: 963, 1: 718}            {0: 823, 1:  13, 3: 843, 4: 2}
//! 1234   {0: 710, 1: 556}            {0: 631, 1:  32, 3: 598, 4: 5}
//! ```
//!
//! **FAILS on seed 1234.** Viability half passes everywhere by four to five
//! orders of magnitude (max suitability 0.113 / 0.113 / 0.093 mountain,
//! 0.080 / 0.078 / 0.076 duergar, against a floor of 1e-6).
//!
//! ## H2c — distinct seated rungs (needs > 1)
//!
//! **HOLDS on every seed for both kinds**: mountain 2 / 2 / 2, duergar
//! 3 / 4 / 4. This is the terrain property — prefix truncation genuinely
//! bites — and the one clause no choice of niche could have satisfied.
//!
//! ## H2 overlap — the decisive number, and it is WELL-DEFINED
//!
//! ```text
//! seed   quartile   |A ∩ B|   share of each   Jaccard   ≥ 20%?
//!   42        218        16            7.3%      3.8%   no
//!    7        420        69           16.4%      8.9%   no
//! 1234        316        52           16.5%      9.0%   no
//! ```
//!
//! Spec §5's tie guard anticipated a tie-dense capacity field and it is not
//! one: the composed field carries **862 / 1515 / 1180** distinct values
//! (mountain) and **870 / 1579 / 1255** (duergar), and the quartile boundary
//! is a **singleton — tie size 1 — on every seed for both kinds**, so
//! `boundary_splits_a_tie` is `false` throughout and the statistic is computed
//! rather than withheld. **The threshold was not widened. The kinds were not
//! admitted.**
//!
//! ### The broken-join arm — HISTORICAL, AND NOT REGENERABLE FROM THIS TREE
//!
//! ```text
//! seed                                   42       7    1234
//! H2 overlap, broken genus join       34.4%   93.3%   84.5%     <- pre-repair
//! H2 overlap, repaired genus join      7.3%   16.4%   16.5%     <- shipped
//! ```
//!
//! **This row cannot be re-run and must not be read as a live measurement.**
//! It was taken while `chamber_fit` filtered the corpus on
//! `CaveKind::name()` (`"karst"`) against genera spelled in the surface
//! corpus's vocabulary (`"karst-cave"`) — one of three agreed by coincidence,
//! so karst and fracture columns scored against a genus-blind fallback and
//! returned bit-identical fit tables. `genus_of` is exhaustive over `CaveKind`
//! with no wildcard and the defect is gone from the tree, which is exactly why
//! the number is unreproducible: recovering it would mean reintroducing the
//! bug. It is kept because it is the campaign's single most consequential
//! measurement — **measuring H2 on the broken instrument would have reported a
//! 20%-clearing pass that means nothing** — and a figure that cannot be
//! regenerated is recorded as history, with its date and its cause, never as
//! evidence about this tree.
//!
//! ## The attribution decomposition — which factor separated them
//!
//! ```text
//! seed   composed   condition niches only        the delve axis only
//!   42       7.3%   0.5%  (|A∩B| = 1)            UNDEFINED  4/5 distinct, ties 284 / 576
//!    7      16.4%   0.0%  (|A∩B| = 0)            UNDEFINED  4/4 distinct, ties 879 / 787
//! 1234      16.5%   UNDEFINED (tie 12)           UNDEFINED  4/5 distinct, ties 619 / 610
//! ```
//!
//! **The finding, and it is not the one the campaign expected.** The delve
//! seating multiplier — the only factor this campaign's Task 8 built — has no
//! quartile at all: four or five distinct values with 284–879 cells tied at
//! the boundary on every seed, spec §5's predicted failure mode arriving
//! exactly as written. The condition niches alone separate the two kinds
//! essentially completely on the two seeds where the statistic is defined, and
//! composing the multiplier on top *raises* overlap toward 16%, pulling the
//! kinds slightly back together.
//!
//! **Seed 1234's condition-niche cell is UNDEFINED, not 0.0%**, and the
//! three-seed range is therefore "0.0–0.5% on two of three seeds", never
//! "0.0–0.5%" flat. See the scope note on the control below: the unscaled arm
//! is **not** depth-free — it reads `subterranean_substrate`, which routes
//! chamber temperature through `temperature_at_depth` and derives moisture
//! from the water table, and the two candidates differ chiefly on exactly
//! those axes. What the control establishes is that *the seating multiplier
//! had no resolution and the depth-routed conditions did the separating* — not
//! that depth contributed nothing.
//!
//! ## Distinct capacity values — the anti-tie evidence, in full
//!
//! ```text
//! seed   mountain-dwarf   duergar        boundary tie size (both kinds)
//!   42              862       870        1
//!    7             1515      1579        1
//! 1234             1180      1255        1
//! ```
//!
//! **862–1579 distinct values** over 874–1681 cave-bearing cells. The unscaled
//! arm is similarly rich (862 / 1510 / 1177 mountain, 870 / 1578 / 1255
//! duergar, boundary tie 1 except seed 1234's duergar at 12); the multiplier
//! arm is the tie-dense one, at four or five values.
//!
//! ## `maybe_raid`'s own-rung lookup — the suppression is PARTIAL
//!
//! ```text
//! seed   co-seating columns            suppressed
//!   42   118 / 874  (13.5%)  — Undercroft 104, Shallows 14      86.5%
//!    7   133 / 1681 ( 7.9%)  — Undercroft 120, Shallows 13      92.1%
//! 1234   144 / 1266 (11.4%)  — Undercroft 112, Shallows 32      88.6%
//! ```
//!
//! `Bake::maybe_raid` resolves the target through `rung_for(pidx, n)`, which
//! is **per cell, not per people** — so "the two kinds have different modal
//! rungs" does not imply "they can never meet". Measured rather than inferred
//! from the modes: on 7.9–13.5% of shared cave-bearing columns the two
//! candidates would have been able to interact. An overlap that *had* cleared
//! 20% would not have demonstrated one family; it would have shown shared
//! ground between peoples the raid gate keeps apart on ~90% of columns.
//!
//! ## The works discount — how many seats are priced
//!
//! ```text
//! seed   mountain-dwarf          duergar
//!   42   189 / 874  (21.6%)      270 / 874  (30.9%)
//!    7   356 / 1681 (21.2%)      858 / 1681 (51.0%)
//! 1234   259 / 1266 (20.5%)      634 / 1266 (50.1%)
//! ```
//!
//! `seat_at` chooses on fit and charges the works afterwards, so a reader
//! comparing capacities needs to know how many seats were discounted. None of
//! these is a seat at `Undercroft`: `works` is structurally `false` at rank 0
//! (see `delve_seating::RungSeat::works`), so every count here falls on ranks
//! 1–4.
//!
//! ## THE VERDICT
//!
//! **The gate is CLOSED. `mountain-dwarf` and `duergar` are NOT authored.**
//! H2's floor fails on seed 1234 and H2's overlap fails on all three, against
//! a criterion frozen before any of it was measured. That is spec §5's
//! preregistered outcome and it is a result, not a failure.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_astronomy::SkyPins;
use hornvale_kernel::ecology::{ConditionResponse, ResourceVector};
use hornvale_kernel::{
    ANIMAL_PREY, CellId, DETRITUS, ENERGY, LIGHT, Mass, PHYSIOGNOMY, PLANT_FORAGE, SUBSTRATE, Seed,
    WATER, sovereignty_floor,
};
use hornvale_species::{
    AxisPreference, BiosphereTraits, ConditionNiche, EnvironmentNiche, HabitatRealm, LifeSchedule,
    MetabolicClass, SocialForm,
};
use hornvale_terrain::{CaveKind, DelveRung, TerrainPins, rungs, water_table_depth_m};
use hornvale_worldgen::chamber::{BRANCHES_PER_SYSTEM, ChamberAddr, chamber_exists, rung_rank};
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::delve_seating::{chamber_fit, seat_at, seating_for};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, build_world_to_with_artifacts, per_species_capacity,
    per_species_suitability, sky_of,
};

/// The seeds this campaign preregisters on (spec §5).
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The viability floor H2's own text names — `hornvale_demography::FLOOR`,
/// unchanged, and identical to `non_void_roster.rs`'s `VIABILITY_FLOOR`. Two
/// different floors would let a kind pass one test and fail the other.
const VIABILITY_FLOOR: f64 = hornvale_demography::FLOOR;

/// H2's preregistered minimum overlap between the two kinds' top-quartile
/// cells. Frozen in spec §5 before this file existed; it is a MINIMUM, because
/// the degenerate pass here is total separation rather than collision.
const H2_MIN_OVERLAP: f64 = 0.20;

// ---------------------------------------------------------------------------
// THE CANDIDATES — test-local, uncommitted to any registry.
//
// Both are 72.0 kg endotherms at potency 0.0, `Settled`, on
// `LifeSchedule::paced(4.0)`, exactly as The Delvers authored them and exactly
// as the three surviving dwarves still are: the schedule is a FAMILY trait
// (spec §4.7), so making it differ here would claim that living deep is what
// makes a dwarf long-lived.
//
// **They share one elevation curve, taken from one function, and that is the
// null control this measurement needs.** Spec §4.7 says each kind carries its
// identity on the delve rung and its community — so if the two kinds also
// differed on elevation, H2 could separate them without the delve axis
// contributing anything, and the readout could not tell which had done the
// work. The precedent is in the tree: `drow_condition_niche` takes
// `wood_elf_condition_niche().elevation` entire rather than restating it, for
// the same reason.
// ---------------------------------------------------------------------------

/// The elevation curve both candidates carry, and the **only** reason it is
/// authored live rather than declined: spec §4.3's amendment.
///
/// `devotion` is `0.70` against a `sovereignty_floor` of ≈0.4498 at 72.0 kg, a
/// margin of ≈0.25 — the same shape `desert_dwarf_condition_niche` uses (0.70
/// against 0.443252) and for the same measured reason. Below the floor,
/// elevation would be the Liebig minimum on every cell and the chamber
/// conditions Task 5 made vary would be unreachable.
///
/// `width` is deliberately **wide** — 2600 m against the three surviving
/// dwarves' 900–1400 m — because these two kinds state no preference about how
/// high the ground overhead is. Their identity is depth, and the model's
/// elevation axis is metres above sea level, which is precisely the confusion
/// The Delvers committed and withdrew over. A wide curve keeps the axis live
/// (so the chamber axes can be read) while saying almost nothing, which is the
/// honest statement of "this kind does not care".
///
/// `optimum` is 1200 m, between land's p35 (≈1004 m) and p75 (≈2651 m): near
/// enough to the middle of settleable land that the curve is flat across it.
/// AUTHORED, all three numbers.
fn delver_hall_elevation() -> ConditionResponse {
    ConditionResponse {
        optimum: 1200.0,
        width: 2600.0,
        devotion: 0.70,
    }
}

/// Mountain-dwarf's condition niche — the miner of the high stone, whose halls
/// trade with the daylight above.
///
/// Every value AUTHORED. The three chamber-facing curves read a chamber at the
/// cave's own depth reach (`subterranean_substrate`, since Task 5), so unlike
/// The Delvers' withdrawn version they are **live**, not prepared: the
/// elevation curve above clears the sovereignty floor, so whichever of these
/// three is scarcest is the Liebig minimum.
///
/// - **temperature 12 °C, width 15.0, devotion 0.55.** A shallow chamber, a
///   few kelvin above its surface datum. Task 1's reach table puts the
///   shallow decile at ΔT ≈ 5–6 K, so a mountain hall reads close to the
///   country outside it, which is the physical statement of "shallow".
/// - **moisture 0.35, width 0.30, devotion 0.50.** Dry halls. High stone
///   drains, and that is why a hall goes in a mountain.
/// - **insolation 0.0, width 0.12, devotion 0.40.** `subterranean_substrate`
///   reads insolation as exactly `0.0`, always; the width is wider than
///   duergar's because a mountain dwarf's halls open onto the daylight it
///   trades with.
/// - **elevation** — [`delver_hall_elevation`], shared with duergar.
fn mountain_dwarf_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 12.0,
            width: 15.0,
            devotion: 0.55,
        },
        moisture: ConditionResponse {
            optimum: 0.35,
            width: 0.30,
            devotion: 0.50,
        },
        insolation: ConditionResponse {
            optimum: 0.0,
            width: 0.12,
            devotion: 0.40,
        },
        elevation: delver_hall_elevation(),
    }
}

/// Duergar's condition niche — the deep people, with no surface to trade with.
///
/// Every value AUTHORED, and the two that differ from mountain-dwarf's differ
/// on the coordinate Task 5 built.
///
/// - **temperature 45 °C, width 25.0, devotion 0.55.** The deep is hot. Task
///   1's reach table puts the deep quartile at ΔT ≈ 40–58 K above the surface
///   datum, so a chamber at a duergar's working depth reads tens of kelvin
///   warmer than the country overhead. This is the campaign's depth apparatus
///   entering a condition niche.
/// - **moisture 0.90, width 0.25, devotion 0.50.** The deep is wet: Task 3
///   measured that the deepest rungs are dry on 0% of cave-bearing columns and
///   that no scale constant opens them. A duergar hall is kept dry by works
///   (`UNDERWORLD_WORKS_COST`), not by luck, so the chamber it settles is
///   authored saturated.
/// - **insolation 0.0, width 0.06, devotion 0.40.** True darkness, narrower
///   than mountain-dwarf's: a duergar's dark is not a lean but its whole
///   world.
/// - **elevation** — [`delver_hall_elevation`], shared with mountain-dwarf.
fn duergar_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 45.0,
            width: 25.0,
            devotion: 0.55,
        },
        moisture: ConditionResponse {
            optimum: 0.90,
            width: 0.25,
            devotion: 0.50,
        },
        insolation: ConditionResponse {
            optimum: 0.0,
            width: 0.06,
            devotion: 0.40,
        },
        elevation: delver_hall_elevation(),
    }
}

/// Mountain-dwarf's niche in the environment basis — five axes, all five
/// occupied, the same arity every row of the underworld corpus carries, so
/// `environment_fit`'s sparsity bias cannot reach a chamber ranking.
///
/// Every value AUTHORED, and every tolerance the neutral default, following
/// `drow_niche`'s stated refusal: a narrower tolerance would be a second
/// unmeasured calibration authored at the same moment as the preference it
/// modifies.
///
/// - `PHYSIOGNOMY` **0.6** (standing structure) — a hall needs something to
///   build in and on. Mountain-dwarf is a people first and a miner second.
/// - `ENERGY` **0.75** (rich) — detrital import. Spec §4.4: shallow chambers
///   are powered by organic matter arriving from above, and this kind's
///   resource vector carries `PLANT_FORAGE` precisely because it has a surface
///   to trade down.
/// - `WATER` **0.2** (arid) — high stone that drains. The deliberate
///   separation from drow's 0.4: drow live in the dry part of a wet karst
///   world, mountain-dwarf lives in stone that is dry to begin with.
/// - `SUBSTRATE` **0.6** (rock) — a `Class`, never a magnitude, because the
///   axis is `AxisValence::Nominal`.
/// - `LIGHT` **0.0** (aphotic).
fn mountain_dwarf_niche() -> EnvironmentNiche {
    EnvironmentNiche::new(&[
        (PHYSIOGNOMY, AxisPreference::graded(0.6)),
        (ENERGY, AxisPreference::graded(0.75)),
        (WATER, AxisPreference::graded(0.2)),
        (SUBSTRATE, AxisPreference::class(0.6)),
        (LIGHT, AxisPreference::graded(0.0)),
    ])
    .expect("five in-range values, class on the nominal axis")
}

/// Duergar's niche in the environment basis — same five axes, same value grid,
/// same neutral tolerances. Every value AUTHORED.
///
/// - `PHYSIOGNOMY` **0.2** (crust) — the deep void is scoured or crusted wall
///   with almost no floor. A duergar cuts its halls into unbroken stone rather
///   than settling a gallery something else decorated.
/// - `ENERGY` **1.0** (teeming) — chemolithotrophy off the geothermal
///   gradient, spec §4.4's energy inversion: "the deep is not poorer; it is
///   differently powered, and that is what makes a deep people viable rather
///   than merely stubborn". The corpus maximum is where the gradient's
///   chemistry is.
/// - `WATER` **0.8** (wet) — the deep is saturated, and staying dry there is
///   something a people *does*.
/// - `SUBSTRATE` **0.6** (rock) — a `Class`, as above.
/// - `LIGHT` **0.0** (aphotic).
fn duergar_niche() -> EnvironmentNiche {
    EnvironmentNiche::new(&[
        (PHYSIOGNOMY, AxisPreference::graded(0.2)),
        (ENERGY, AxisPreference::graded(1.0)),
        (WATER, AxisPreference::graded(0.8)),
        (SUBSTRATE, AxisPreference::class(0.6)),
        (LIGHT, AxisPreference::graded(0.0)),
    ])
    .expect("five in-range values, class on the nominal axis")
}

/// Mountain-dwarf's candidate biosphere row. `DETRITUS`-dominant with
/// `PLANT_FORAGE` second — the fungal food web The Delvers corrected dwarves
/// onto, plus the surface it trades down. AUTHORED.
fn mountain_dwarf_traits() -> BiosphereTraits {
    BiosphereTraits {
        mass: Mass::new(72.0).unwrap(),
        metabolic_class: MetabolicClass::Endotherm,
        niche: ResourceVector::new(&[(DETRITUS, 0.55), (PLANT_FORAGE, 0.25), (ANIMAL_PREY, 0.20)])
            .unwrap(),
        condition_niche: mountain_dwarf_condition_niche(),
        potency: 0.0,
        social_form: SocialForm::Settled,
        schedule: LifeSchedule::paced(4.0).unwrap(),
    }
}

/// Duergar's candidate biosphere row. `DETRITUS` and `ANIMAL_PREY` only — a
/// deep people has no surface to trade with, so nothing arrives from a
/// mountainside. AUTHORED.
fn duergar_traits() -> BiosphereTraits {
    BiosphereTraits {
        mass: Mass::new(72.0).unwrap(),
        metabolic_class: MetabolicClass::Endotherm,
        niche: ResourceVector::new(&[(DETRITUS, 0.60), (ANIMAL_PREY, 0.40)]).unwrap(),
        condition_niche: duergar_condition_niche(),
        potency: 0.0,
        social_form: SocialForm::Settled,
        schedule: LifeSchedule::paced(4.0).unwrap(),
    }
}

/// The two candidates, in one place, in the order every readout below prints
/// them.
fn candidates() -> Vec<(&'static str, BiosphereTraits, EnvironmentNiche)> {
    vec![
        (
            "mountain-dwarf",
            mountain_dwarf_traits(),
            mountain_dwarf_niche(),
        ),
        ("duergar", duergar_traits(), duergar_niche()),
    ]
}

/// The habitation rungs, shallowest first — the ladder minus `Surface`.
fn habitation_rungs() -> Vec<DelveRung> {
    rungs()
        .iter()
        .copied()
        .filter(|r| *r != DelveRung::Surface)
        .collect()
}

/// claim: structural(no seeds; a pure read over the authored corpus and the
/// two candidate niches) — **the mandatory disclosure of spec §5**: each
/// candidate's 3×5 `chamber_fit` table and its per-formation argmax, recorded
/// before any world is built, plus the elevation precondition of §4.3's
/// amendment.
///
/// This test builds nothing and asserts two things. The fit tables are
/// **printed**, not pinned: pinning them to literals would make this file a
/// golden of its own fixture, and the numbers that matter are compared against
/// the measured modal rungs in the campaign's report, not against a literal
/// here.
///
/// The assertion is the precondition, computed live.
#[test]
fn the_candidate_fit_tables_and_the_elevation_precondition() {
    println!("== the elevation precondition (spec §4.3's amendment) ==");
    for (name, traits, _) in candidates() {
        let floor = sovereignty_floor(traits.mass, traits.potency);
        let devotion = traits.condition_niche.elevation.devotion;
        println!(
            "  {name:<15} mass {:>5.1} kg  potency {:.1}  sovereignty_floor {floor:.6}  \
             devotion_elev {devotion:.4}  margin {:+.6}",
            traits.mass.kilograms(),
            traits.potency,
            devotion - floor,
        );
        assert!(
            devotion > floor,
            "{name} is authored with devotion_elev {devotion} at or below its own \
             sovereignty floor {floor}: elevation would be the Liebig minimum on \
             EVERY cell and this campaign's whole depth apparatus would be \
             invisible to it (spec §4.3's amendment)"
        );
    }

    println!();
    println!("== the 3x5 chamber_fit tables, BEFORE any world (spec §5's mandatory disclosure) ==");
    let ladder = habitation_rungs();
    println!(
        "  {:<15} {:<14} {}",
        "kind",
        "formation",
        ladder
            .iter()
            .map(|r| format!("{:>11}", format!("{r:?}")))
            .collect::<Vec<_>>()
            .join("")
    );
    for (name, _, niche) in candidates() {
        for kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            let fits: Vec<Option<f64>> = ladder
                .iter()
                .map(|r| chamber_fit(&niche, kind, *r))
                .collect();
            let cells: String = fits
                .iter()
                .map(|f| f.map_or("         --".into(), |v| format!("{v:>11.4}")))
                .collect();
            // `total_cmp`, never `>`: the argmax must be total, and the
            // tie-break must be the same one `seat_at` uses — shallowest wins.
            let argmax = ladder
                .iter()
                .zip(fits.iter())
                .filter_map(|(r, f)| f.map(|v| (*r, v)))
                .fold(None::<(DelveRung, f64)>, |best, (r, v)| match best {
                    None => Some((r, v)),
                    Some((_br, bv)) if v.total_cmp(&bv).is_gt() => Some((r, v)),
                    other => other,
                });
            println!("  {name:<15} {:<14}{cells}", kind.name());
            match argmax {
                Some((r, v)) => println!("      argmax {r:?} ({v:.4})"),
                None => println!("      argmax NONE — the corpus scores no rung here"),
            }
        }
    }
}

/// One kind's per-cell readings on one seed: everything the hypotheses below
/// are computed from, taken in a single pass so the seating and the capacity
/// cannot disagree about which cells they describe.
struct KindReadings {
    /// The seated rung at every cave-bearing land cell, in cell order.
    seated: Vec<(CellId, DelveRung)>,
    /// The seating-scaled capacity at every cave-bearing land cell — what the
    /// deep-history bake actually reasons in.
    capacity: Vec<(CellId, f64)>,
    /// The **unscaled** capacity — the same field before the seat's multiplier
    /// is applied. The first arm of the attribution control below: everything
    /// this campaign did NOT ship, i.e. the condition niches against the
    /// chamber substrate.
    capacity_unscaled: Vec<(CellId, f64)>,
    /// The seat's multiplier alone. The second arm: everything this campaign
    /// DID ship, i.e. `chamber_fit` times the works discount, and nothing else.
    multiplier: Vec<(CellId, f64)>,
    /// The dimensionless suitability field, the quantity `non_void_roster`
    /// checks H2's floor against. Not seating-scaled, exactly as that test
    /// takes it.
    suitability_max: f64,
}

/// Descending sort by value with a deterministic tie-break on `CellId`, so a
/// quartile boundary is a fact about the world rather than about iteration
/// order. `total_cmp`, never `>`.
fn rank_descending(mut rows: Vec<(CellId, f64)>) -> Vec<(CellId, f64)> {
    rows.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.0.cmp(&b.0.0)));
    rows
}

/// The top-quartile cell set of a descending ranking, together with the tie
/// evidence spec §5 requires: the boundary value, how many cells share it, and
/// whether the boundary falls **inside** that tie.
///
/// A boundary inside a tie means the quartile's membership was decided by the
/// tie-break rather than by any measured quantity, and spec §5 is explicit
/// that the overlap is then **undefined and must be reported as undefined,
/// never computed**.
struct Quartile {
    /// The cells above the boundary.
    cells: BTreeSet<u32>,
    /// The capacity value at the boundary.
    boundary: f64,
    /// How many cells in the whole ranking carry exactly that value.
    tie_size: usize,
    /// Whether the boundary splits that tie — some tied cells in, some out.
    boundary_splits_a_tie: bool,
    /// How many distinct capacity values the whole ranking carries.
    distinct_values: usize,
}

/// The top quartile of a descending ranking, with its tie evidence.
fn top_quartile(ranked: &[(CellId, f64)]) -> Quartile {
    let n = ranked.len();
    let take = n / 4;
    let cells: BTreeSet<u32> = ranked.iter().take(take).map(|(c, _)| c.0).collect();
    let boundary = if take == 0 {
        f64::NAN
    } else {
        ranked[take - 1].1
    };
    // Bitwise equality, not `==`: two capacities are the same value or they
    // are not, and this is a tie count rather than a physical comparison.
    let tie_size = ranked
        .iter()
        .filter(|(_, v)| v.to_bits() == boundary.to_bits())
        .count();
    let boundary_splits_a_tie =
        take > 0 && take < n && ranked[take].1.to_bits() == boundary.to_bits();
    let distinct_values: BTreeSet<u64> = ranked.iter().map(|(_, v)| v.to_bits()).collect();
    Quartile {
        cells,
        boundary,
        tie_size,
        boundary_splits_a_tie,
        distinct_values: distinct_values.len(),
    }
}

/// The modal entry of a histogram, largest count first, ties broken by the
/// key's own order so the answer is deterministic.
fn modal<K: Copy + Ord>(hist: &BTreeMap<K, usize>) -> Option<(K, usize)> {
    hist.iter()
        .fold(None::<(K, usize)>, |best, (k, n)| match best {
            Some((_, bn)) if bn >= *n => best,
            _ => Some((*k, *n)),
        })
}

/// claim: readout(off-gate, heavy:, prints H1, H2's floor and overlap, and
/// H2c per seed) — spec §5's separation readout, taken on two kinds that are
/// **not** in any registry.
///
/// It asserts nothing about the hypotheses. That is deliberate and is the
/// campaign's own discipline: H2's floor failing is a preregistered, legitimate
/// outcome under which the two kinds are simply not authored, so a red here
/// would misreport a finding as a defect. What it does assert is that the
/// instrument is not vacuous — every seed must carry cave-bearing cells, and
/// each candidate must produce a seating over them — because a readout over an
/// empty population satisfies every clause below perfectly.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_separation_readout() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let ladder = habitation_rungs();

    for seed_value in SEEDS {
        let seed = Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Settlements,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Settlements");
        let climate = artifacts
            .climate
            .expect("climate is Some at BuildDepth::Settlements");
        let sky = sky_of(&artifacts.world).expect("sky reconstructs");
        let system = sky
            .system()
            .unwrap_or_else(|| panic!("seed {seed_value} has a generated star system"));
        let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
        let obliquity = system.anchor.obliquity.get();
        let regime = match system.anchor.rotation {
            hornvale_astronomy::Rotation::Spinning { day, .. } => {
                hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
            }
            hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
        };
        let geo = terrain.geosphere();
        let sea = terrain.sea_level().get();

        println!("======== seed {seed_value} ========");

        // --------------------------------------------------------------
        // H1 — the ladder varies. The rung of every chamber that EXISTS,
        // enumerated over the whole lattice rather than inferred from the
        // caves' reach: H1 is a claim about chambers, and `chamber_exists`
        // is the only thing that knows which addresses are real.
        // --------------------------------------------------------------
        let mut chamber_hist: BTreeMap<usize, usize> = BTreeMap::new();
        let mut caves = 0usize;
        for cell in geo.cells() {
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            caves += 1;
            let gradient = terrain.geothermal_gradient_at(cell);
            for (rank, _) in ladder.iter().enumerate() {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    let addr = ChamberAddr {
                        cell,
                        entrance: 0,
                        band: rank as u8,
                        branch,
                        floor: 0,
                    };
                    if chamber_exists(seed, &cave, gradient, addr) {
                        *chamber_hist.entry(rank).or_default() += 1;
                    }
                }
            }
        }
        assert!(
            caves > 0,
            "seed {seed_value} has no cave-bearing cell — every clause below \
             would be vacuously satisfied"
        );
        let chambers: usize = chamber_hist.values().sum();
        let occupied_rungs = chamber_hist.values().filter(|n| **n > 0).count();
        let top_share = chamber_hist
            .values()
            .max()
            .map_or(0.0, |m| *m as f64 / chambers.max(1) as f64);
        println!("-- H1: the ladder varies --");
        println!("  caves {caves}  chambers {chambers}");
        for (rank, rung) in ladder.iter().enumerate() {
            let n = chamber_hist.get(&rank).copied().unwrap_or(0);
            println!(
                "    {rung:<12?} {n:>7}  ({:.1}%)",
                100.0 * n as f64 / chambers.max(1) as f64
            );
        }
        println!(
            "  H1 floor: {occupied_rungs} of {} rungs occur (needs >= {}); \
             anti-degenerate: largest rung holds {:.1}% (needs <= 70.0%)",
            ladder.len(),
            ladder.len() - 1,
            top_share * 100.0
        );

        // --------------------------------------------------------------
        // The candidates' fields, in one pass per kind.
        // --------------------------------------------------------------
        let cands = candidates();
        let bios: Vec<&BiosphereTraits> = cands.iter().map(|(_, b, _)| b).collect();
        let realms = vec![HabitatRealm::Subterranean; cands.len()];
        let affinities: Vec<Option<hornvale_species::BiomeAffinity>> = vec![None; cands.len()];

        let suitabilities = per_species_suitability(
            geo,
            &terrain,
            &climate,
            obliquity,
            insolation,
            &regime,
            &bios,
            &realms,
            &affinities,
        );
        let capacities = per_species_capacity(
            geo,
            &terrain,
            &climate,
            obliquity,
            insolation,
            &regime,
            &bios,
            &realms,
            &affinities,
        );

        let mut readings: Vec<(&'static str, KindReadings)> = Vec::new();
        for (index, (name, _, niche)) in cands.iter().enumerate() {
            let seating = seating_for(geo, &terrain, Some(niche));
            let (_, suit) = &suitabilities[index];
            let (_, cap) = &capacities[index];
            let mut seated = Vec::new();
            let mut capacity = Vec::new();
            let mut capacity_unscaled = Vec::new();
            let mut multiplier = Vec::new();
            let mut suitability_max = 0.0f64;
            for cell in geo.cells() {
                if terrain.cave_at(cell).is_none() {
                    continue;
                }
                seated.push((cell, *seating.rung.get(cell)));
                // The realm-aware capacity the bake reads: the dimensional
                // field, scaled by the seat's own multiplier. Reproduced here
                // exactly as `bake_history_from` composes it.
                capacity.push((cell, cap.at(cell) * seating.multiplier.get(cell)));
                capacity_unscaled.push((cell, cap.at(cell)));
                multiplier.push((cell, *seating.multiplier.get(cell)));
                suitability_max = suitability_max.max(*suit.get(cell));
            }
            assert!(
                !seated.is_empty(),
                "seed {seed_value}: {name} was seated at no cell at all"
            );
            readings.push((
                name,
                KindReadings {
                    seated,
                    capacity,
                    capacity_unscaled,
                    multiplier,
                    suitability_max,
                },
            ));
        }

        // --------------------------------------------------------------
        // H2's floor and H2c, per kind.
        // --------------------------------------------------------------
        println!("-- H2 floor / H2c: the seated rung --");
        let mut modes: Vec<(&'static str, Option<DelveRung>)> = Vec::new();
        for (name, r) in &readings {
            let mut hist: BTreeMap<usize, usize> = BTreeMap::new();
            for (_, rung) in &r.seated {
                let rank = ladder.iter().position(|x| x == rung).unwrap_or(usize::MAX);
                *hist.entry(rank).or_default() += 1;
            }
            let distinct = hist.len();
            let mode = modal(&hist).and_then(|(rank, _)| ladder.get(rank).copied());
            let peak_capacity = r
                .capacity
                .iter()
                .fold(0.0f64, |best, (_, v)| if *v > best { *v } else { best });
            println!("  {name}:");
            for (rank, rung) in ladder.iter().enumerate() {
                let n = hist.get(&rank).copied().unwrap_or(0);
                if n > 0 {
                    println!(
                        "    {rung:<12?} {n:>7}  ({:.1}%)",
                        100.0 * n as f64 / r.seated.len() as f64
                    );
                }
            }
            println!(
                "    modal rung {mode:?}  |  H2c distinct seated rungs {distinct} (needs > 1)"
            );
            println!(
                "    max suitability {:.6e} (H2 floor needs >= {VIABILITY_FLOOR:e})  \
                 max seated capacity {peak_capacity:.6e}",
                r.suitability_max
            );
            // THE BAND HISTOGRAM the `made_chambers` disclosure turns on. Task
            // 8 committed "every settled underworld column sits at band 1 and
            // band 0 holds zero"; a kind seating at `Undercroft` is band 0 and
            // makes that claim false.
            let undercroft = hist.get(&0).copied().unwrap_or(0);
            println!(
                "    lattice band histogram {:?}  (band 0 = Undercroft holds {undercroft})",
                hist.iter().map(|(k, v)| (*k, *v)).collect::<Vec<_>>()
            );
            modes.push((name, mode));
        }
        let modes_differ = modes.len() == 2 && modes[0].1 != modes[1].1;
        println!("  H2 floor, modal rungs differ: {modes_differ} ({modes:?})");

        // --------------------------------------------------------------
        // H2's overlap, guarded by its tie evidence.
        // --------------------------------------------------------------
        println!("-- H2 overlap: the top-quartile cells --");
        let quartiles: Vec<(&'static str, Quartile)> = readings
            .iter()
            .map(|(name, r)| {
                let ranked = rank_descending(r.capacity.clone());
                (*name, top_quartile(&ranked))
            })
            .collect();
        for (name, q) in &quartiles {
            println!(
                "  {name:<15} quartile {:>6} cells  boundary {:.6e}  tie at boundary {:>6}  \
                 distinct capacity values {:>6}  boundary splits a tie: {}",
                q.cells.len(),
                q.boundary,
                q.tie_size,
                q.distinct_values,
                q.boundary_splits_a_tie
            );
        }
        if quartiles.len() == 2 {
            let (a, b) = (&quartiles[0], &quartiles[1]);
            if a.1.boundary_splits_a_tie || b.1.boundary_splits_a_tie {
                println!(
                    "  H2 overlap: UNDEFINED — the quartile boundary falls inside a tie \
                     ({} / {} cells share it), so membership was decided by the tie-break \
                     rather than by any measured quantity. Spec §5: report undefined, \
                     never compute.",
                    a.1.tie_size, b.1.tie_size
                );
            } else {
                let inter = a.1.cells.intersection(&b.1.cells).count();
                let union = a.1.cells.union(&b.1.cells).count();
                let share_a = inter as f64 / a.1.cells.len().max(1) as f64;
                let share_b = inter as f64 / b.1.cells.len().max(1) as f64;
                let jaccard = inter as f64 / union.max(1) as f64;
                println!(
                    "  H2 overlap: |A n B| = {inter}  share of {} = {:.1}%  share of {} = {:.1}%  \
                     Jaccard = {:.1}%  (needs >= {:.0}%)",
                    a.0,
                    share_a * 100.0,
                    b.0,
                    share_b * 100.0,
                    jaccard * 100.0,
                    H2_MIN_OVERLAP * 100.0
                );
            }
        }

        // --------------------------------------------------------------
        // THE ATTRIBUTION CONTROL, and it separates the SEATING MULTIPLIER
        // from everything else — which is NOT the same as separating "depth"
        // from everything else. Read the scope note below before quoting a
        // number off it.
        //
        // The composed capacity above is `per_species_capacity_at(..) *
        // seating.multiplier`, and only the second factor is Task 8's work.
        // Two candidates authored with different condition niches would
        // separate on the FIRST factor alone — so an overlap taken on the
        // composed field cannot say which half did it. These two arms can:
        //
        //   unscaled   the condition niches against the chamber substrate,
        //              with the seating removed entirely.
        //   multiplier `chamber_fit` times the works discount and NOTHING
        //              else — the delve SEATING, alone.
        //
        // **SCOPE, and it is easy to overstate — this comment did, in its
        // first draft.** The unscaled arm is NOT a depth-free control. It
        // reads `subterranean_substrate`, which since Task 5 routes chamber
        // temperature through `temperature_at_depth` and derives moisture from
        // the water table — and the two candidate niches differ chiefly on
        // exactly those two axes (temperature 12 vs 45 °C, moisture 0.35 vs
        // 0.90). So the unscaled arm carries this campaign's depth work too,
        // through a different factor.
        //
        // What the control therefore establishes, stated at the width the
        // evidence supports: **the seating multiplier had no resolution, and
        // the depth-routed condition niches did the separating.** It does NOT
        // establish that depth contributed nothing — Task 5's half of the
        // depth apparatus is inside the arm that separated them, and a
        // genuinely depth-free control would need a third fixture whose
        // conditions do not vary with the chamber. This task does not build
        // one.
        //
        // The multiplier arm is expected to be tie-dense (a handful of
        // distinct values, per spec §5's own guard), and its quartile is
        // reported undefined when it is, rather than computed.
        // --------------------------------------------------------------
        // --------------------------------------------------------------
        // MAYBE_RAID'S OWN-RUNG LOOKUP, measured rather than inferred from
        // the modal rungs (spec §5 requires this task to state whether it
        // suppresses H2's one-family clause).
        //
        // `Bake::maybe_raid` resolves the target cell through
        // `rung_for(raider_pidx, n)`, which is PER CELL, not per people. So
        // "the two kinds have different modal rungs" does NOT imply "the two
        // kinds can never meet": they meet wherever they happen to seat at the
        // SAME rung in the same column, which the modal rung cannot tell you.
        // The suppression is therefore PARTIAL, and this is its size.
        // --------------------------------------------------------------
        println!("-- maybe_raid: where the two kinds could meet at all --");
        if readings.len() == 2 {
            let (a, b) = (&readings[0], &readings[1]);
            let mut same = 0usize;
            let mut total = 0usize;
            let mut by_rung: BTreeMap<usize, usize> = BTreeMap::new();
            for ((cell_a, rung_a), (cell_b, rung_b)) in a.1.seated.iter().zip(b.1.seated.iter()) {
                debug_assert_eq!(cell_a, cell_b, "the two seatings walk the same cells");
                total += 1;
                if rung_a == rung_b {
                    same += 1;
                    let rank = ladder
                        .iter()
                        .position(|x| x == rung_a)
                        .unwrap_or(usize::MAX);
                    *by_rung.entry(rank).or_default() += 1;
                }
            }
            println!(
                "  columns where {} and {} seat at the SAME rung: {same} / {total} ({:.1}%)",
                a.0,
                b.0,
                100.0 * same as f64 / total.max(1) as f64
            );
            for (rank, n) in &by_rung {
                let name = ladder
                    .get(*rank)
                    .map_or("?".to_string(), |r| format!("{r:?}"));
                println!("    {name:<12} {n}");
            }
            println!(
                "  so the own-rung lookup suppresses interaction on {:.1}% of shared \
                 cave-bearing columns, NOT on all of them",
                100.0 * (total - same) as f64 / total.max(1) as f64
            );
        }

        println!("-- attribution: which factor separated them --");
        for (label, pick) in [
            ("unscaled (condition niches only)", 0usize),
            ("multiplier (the delve axis only)", 1usize),
        ] {
            let qs: Vec<(&'static str, Quartile)> = readings
                .iter()
                .map(|(name, r)| {
                    let rows = if pick == 0 {
                        r.capacity_unscaled.clone()
                    } else {
                        r.multiplier.clone()
                    };
                    (*name, top_quartile(&rank_descending(rows)))
                })
                .collect();
            let split = qs.iter().any(|(_, q)| q.boundary_splits_a_tie);
            let detail: String = qs
                .iter()
                .map(|(n, q)| format!("{n} distinct={} tie={}", q.distinct_values, q.tie_size))
                .collect::<Vec<_>>()
                .join("  ");
            if split || qs.len() != 2 {
                println!("  {label:<34} overlap UNDEFINED (boundary inside a tie)  {detail}");
            } else {
                let inter = qs[0].1.cells.intersection(&qs[1].1.cells).count();
                let share = inter as f64 / qs[0].1.cells.len().max(1) as f64;
                println!(
                    "  {label:<34} overlap {:.1}%  |A n B| = {inter}  {detail}",
                    share * 100.0
                );
            }
        }

        // --------------------------------------------------------------
        // The seat's own cost, printed because `seat_at` chooses on fit and
        // charges the works afterwards — a reader comparing capacities needs
        // to know how many seats were discounted.
        // --------------------------------------------------------------
        println!("-- the works discount --");
        for (name, _, niche) in &cands {
            let mut works = 0usize;
            let mut total = 0usize;
            for cell in geo.cells() {
                let Some(cave) = terrain.cave_at(cell) else {
                    continue;
                };
                let table = water_table_depth_m(
                    terrain.drainage_at(cell),
                    terrain.material_at(cell).porosity,
                    terrain.elevation_at(cell).get() - sea,
                );
                if let Some(seat) =
                    seat_at(niche, &cave, terrain.geothermal_gradient_at(cell), table)
                {
                    total += 1;
                    if seat.works {
                        works += 1;
                    }
                }
            }
            println!(
                "  {name:<15} seats needing works {works} / {total} ({:.1}%)",
                100.0 * works as f64 / total.max(1) as f64
            );
        }
    }
}

/// claim: structural(no seeds; a pure read over `rung_rank` and the ladder) —
/// the vacuity guard the readout above depends on and cannot check itself.
///
/// H1 enumerates chambers by iterating the ladder's habitation rungs and using
/// each rung's **position** as the lattice `band`. That is only correct if
/// `rung_rank` agrees with that position for every rung; if it ever stopped
/// agreeing, H1 would histogram real chambers under the wrong rung names and
/// the readout would look perfectly healthy.
#[test]
fn the_lattice_band_is_the_ladder_position() {
    for (position, rung) in habitation_rungs().iter().enumerate() {
        assert_eq!(
            rung_rank(*rung),
            Some(position as u8),
            "{rung:?} sits at ladder position {position} but `rung_rank` calls it \
             {:?}; the readout's band enumeration would mislabel every chamber",
            rung_rank(*rung)
        );
    }
    assert_eq!(
        rung_rank(DelveRung::Surface),
        None,
        "the surface is not a lattice band"
    );
}

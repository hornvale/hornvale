//! ORE VIABILITY — can anything live where the ore is? (The Winze, Task 1b)
//!
//! **A measurement that can end the campaign before a spec is written.**
//! Changes no production code. Task 1
//! (`ore_separation_probe.rs`, `task-1-report.md`) measured that mineral
//! prospectivity does not separate settlements, and on two of three seeds
//! occupied vertices are *under*-represented in the top prospectivity decile.
//! `prospectivity` is high on plate boundaries and unrest; settlement founding
//! is a capacity-maximising ring search. Those are plausibly anti-correlated —
//! and if high-prospectivity vertices carry near-zero carrying capacity, a mine
//! founded there starves on arrival regardless of what founding rule chooses
//! it, and mines must be *satellites* supplied from elsewhere rather than
//! settlements in their own right.
//!
//! **The question, precisely: over land vertices, what is the joint distribution
//! of prospectivity and carrying capacity?**
//!
//! ## Capacity accessor
//!
//! [`per_species_capacity`] (present era) is used, not
//! [`per_species_capacity_at`] directly — the two are bit-identical at
//! [`EraAdjust::present`] (documented on `per_species_capacity_at`), and
//! `per_species_capacity` is the form that does not require building an era
//! series first. `Bake::eff_capacity` (`history_bake.rs`) is
//! `caps_now()[pidx].at(vertex) * factor(era, vertex)`, where `caps_now` is this
//! exact per-species-capacity field for the current era and `factor` is an
//! ice mask that is **currently inert** (`bake_eras` fills `era.ice` all-false
//! on every production path — see `Bake::factor`'s doc). So at genesis (era
//! 0, present), `eff_capacity == per_species_capacity`'s value exactly; this
//! probe reads the same number the bake actually founds settlements against,
//! without paying for a full history bake.
//!
//! The roster and its per-people `HabitatRealm`/`BiomeAffinity` are threaded
//! exactly as `bake_history_from` builds them for these six kinds
//! (`wc.habitat_realm`/`wc.biome_affinity`, sparse defaulting to
//! `Surface`/unrestricted) rather than the niche-breadth probe's deliberate
//! all-`Surface`/all-`None` control — this probe wants the field the bake
//! actually consumes, not an isolated condition-niche reading. For this
//! specific roster the two are identical in practice: `habitat_realm_registry`
//! lists only rust-monster, xorn and drow as `Subterranean`, none of which are
//! settling peoples in [`SETTLERS`].
//!
//! Capacity is reported **per people**, never aggregated across the roster —
//! an aggregate that averaged over peoples with different niches would be a
//! number that looks authoritative and means nothing (task brief). The one
//! place a single number is needed (the decisive fraction, and the "good
//! capacity" vertex set for the converse) uses **max over the roster**: a vertex
//! is viable if *any* people in the roster could live there, which is exactly
//! the disjunction "could a mine (settled by whichever people) exist here".
//!
//! ## The viability floor
//!
//! The brief names `VIABLE_MIN = 2.0` (`history_bake.rs`) as "the bake's own
//! floor" and asks whether it is the right comparison. Read closely: every
//! use of `VIABLE_MIN` in `history_bake.rs` compares it against a
//! **population**, never a capacity — `pop < VIABLE_MIN`, `arriving <
//! VIABLE_MIN`, `pop * (1.0 - WAR_LOSS) >= VIABLE_MIN`. It is the remnant/
//! cascade floor: how small a population can get before a broken community
//! dissolves. It is never compared to `eff_capacity` anywhere in the file.
//!
//! The floor that actually governs "can a **founding** survive here" is
//! `GENESIS_POP / COLLAPSE_PRESSURE = 10.0 / 2.0 = 5.0`
//! (`pressure = population * NEED / eff_capacity`, `NEED = 1.0`,
//! `COLLAPSE_PRESSURE = 2.0`, genesis opens at `GENESIS_POP = 10.0` —
//! `history_bake.rs`): a vertex whose capacity is below 5.0 starves a
//! ten-person genesis community above `COLLAPSE_PRESSURE` on the very epoch
//! it is founded. This is also the exact floor `niche_breadth_probe.rs`
//! independently derived and named `SURVIVE_K` for the identical question.
//! **This probe therefore reports the decisive number at both floors**:
//! `VIABLE_MIN` (2.0), verbatim as the brief specifies, and `SURVIVE_K`
//! (5.0), the floor this codebase's own founding arithmetic actually uses.
//! They are reported side by side rather than one silently substituted for
//! the other, so the report can say plainly where they agree and where they
//! don't.
//!
//! World-building idiom copied from `delver_depth_probe.rs`
//! (`build_world`/`terrain_of`, no `BuildDepth::Full`) and the stellar-input
//! resolution from `niche_breadth_probe.rs` (`per_species_capacity` needs
//! `obliquity_deg`/`insolation_scalar`/`regime`, which only the sky's
//! generated system carries).
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry points
//! directly to build its own world state, once per test — the sanctioned
//! test-fixture posture the weir's spec carves out.
//!
//! # RE-RUN AGAINST `main` (The Sources, Task 1, 2026-08-26)
//!
//! Harvested from `campaign/the-winze` (unmerged, 403 commits behind at the
//! time of this re-run) and re-measured against `main` at `7576eca00`, after
//! The Glasshouse's temperature re-centring. **Reproduces exactly**, n=3
//! seeds (42/7/1234): the panel minimum on both floors is still seed 1234 —
//! 71.49% (VIABLE_MIN) and 56.51% (SURVIVE_K), bit-for-bit. No doc, registry
//! or metaplan number changed.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, per_species_capacity, terrain_of};

/// Seeds the campaign states its preregistrations on.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The settling roster, in registry order — the same six kinds every other
/// probe in this line of work (`niche_breadth_probe.rs`,
/// `capacity_cost_probe.rs`, `keeping_probe.rs`, ...) uses, and the roster
/// `bake_history_from` places on an unpinned world (every `Settled`
/// biosphere kind).
const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// The brief's named floor, verbatim from `history_bake.rs`. Compared, in
/// that file, only ever to a **population**, never to a capacity — see the
/// module doc's discussion of why `SURVIVE_K` below is reported alongside it.
const VIABLE_MIN: f64 = 2.0;

/// `GENESIS_POP / COLLAPSE_PRESSURE` (`history_bake.rs`: `10.0 / 2.0`) — the
/// capacity a genesis founding needs to open below `COLLAPSE_PRESSURE` and
/// therefore survive its first epoch. The floor `niche_breadth_probe.rs`
/// independently names `SURVIVE_K`; reproduced here under the same name for
/// the same reason, not re-derived from first principles.
const SURVIVE_K: f64 = 10.0 / 2.0;

/// Nearest-rank percentile of an ascending-sorted slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// claim: readout(off-gate, heavy:, prints the joint table, asserts the
/// branch landed in) — the joint distribution of `prospectivity_at` and
/// per-species carrying capacity over land vertices, for seeds 42 / 7 / 1234.
/// Ruling 2: a decision instrument, not a pass/fail gate on a chosen cut
/// point — but the branch table IS a decision rule (task-1b-brief.md), so
/// this probe's assertions pin the branch actually landed in, not a
/// preferred one.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn ore_viability_probe() {
    // Per-seed decisive fractions, collected so the cross-seed summary at the
    // very end can state one branch for the whole panel rather than three
    // separate ones the report has to reconcile by hand.
    let mut top_decile_frac_viable_min: Vec<f64> = Vec::new();
    let mut top_decile_frac_survive_k: Vec<f64> = Vec::new();

    for seed_value in SEEDS {
        let wc = WorldComponents::assemble().expect("components assemble");
        let seed = Seed(seed_value);
        let world = build_world(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("probe seed builds");
        let terrain = terrain_of(&world).expect("terrain");
        let climate = hornvale_worldgen::climate_of(&world).expect("climate");
        let geo = terrain.geosphere();
        let sky = hornvale_worldgen::sky_of(&world).expect("sky");
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

        // Land vertices, in one fixed order shared by every vector below.
        let land: Vec<hornvale_kernel::Vertex> =
            geo.vertices().filter(|&c| !terrain.is_ocean(c)).collect();
        let n = land.len();

        // 1. `prospectivity_at` over land — item 1's field.
        let land_prosp: Vec<f64> = land.iter().map(|&c| terrain.prospectivity_at(c)).collect();

        // 1b. Per-species capacity over land, built exactly as
        // `bake_history_from` builds it for these six kinds (real
        // `habitat_realm`/`biome_affinity`, not the niche-breadth probe's
        // all-`Surface`/all-`None` control).
        let biosphere: Vec<&hornvale_species::BiosphereTraits> = SETTLERS
            .iter()
            .map(|n| {
                wc.biosphere
                    .get(&KindId(n))
                    .unwrap_or_else(|| panic!("settler '{n}' has biosphere traits"))
            })
            .collect();
        let realm: Vec<hornvale_species::HabitatRealm> = SETTLERS
            .iter()
            .map(|n| {
                wc.habitat_realm
                    .get(&KindId(n))
                    .copied()
                    .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
            })
            .collect();
        let affinity: Vec<Option<hornvale_species::BiomeAffinity>> = SETTLERS
            .iter()
            .map(|n| wc.biome_affinity.get(&KindId(n)).cloned())
            .collect();

        let caps = per_species_capacity(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
            &biosphere,
            &realm,
            &affinity,
        );
        assert_eq!(caps.len(), SETTLERS.len(), "one capacity map per settler");
        for (i, (tag, _)) in caps.iter().enumerate() {
            assert_eq!(
                *tag as usize, i,
                "per_species_capacity's tag must be its position in SETTLERS"
            );
        }

        // cap_vals[s][i] = SETTLERS[s]'s capacity at land[i].
        let cap_vals: Vec<Vec<f64>> = caps
            .iter()
            .map(|(_, map)| land.iter().map(|&c| map.at(c)).collect())
            .collect();
        // The disjunction: could ANYONE in the roster live at land[i]?
        let max_cap: Vec<f64> = (0..n)
            .map(|i| {
                cap_vals
                    .iter()
                    .map(|v| v[i])
                    .fold(f64::NEG_INFINITY, f64::max)
            })
            .collect();

        // 2. Bucket land vertices into deciles by RANK on prospectivity (equal-
        // sized buckets, ties broken by land order — stable sort).
        let mut order: Vec<usize> = (0..n).collect();
        order.sort_by(|&a, &b| land_prosp[a].total_cmp(&land_prosp[b]));
        let mut decile_of = vec![0usize; n];
        for (rank, &i) in order.iter().enumerate() {
            let d = (rank * 10 / n.max(1)).min(9);
            decile_of[i] = d;
        }

        println!("\n== seed {seed_value} ==  land vertices {n}");
        println!(
            "  [1] land prospectivity:  min {:.4}  p50 {:.4}  p90 {:.4}  max {:.4}",
            land_prosp.iter().cloned().fold(f64::INFINITY, f64::min),
            pct(
                &{
                    let mut v = land_prosp.clone();
                    v.sort_by(f64::total_cmp);
                    v
                },
                0.50
            ),
            pct(
                &{
                    let mut v = land_prosp.clone();
                    v.sort_by(f64::total_cmp);
                    v
                },
                0.90
            ),
            land_prosp.iter().cloned().fold(f64::NEG_INFINITY, f64::max),
        );

        // 2. The joint table: per decile bucket, per species median/p90
        // capacity, plus the bucket's vertex count.
        println!("  [2] joint table (decile 0 = lowest prospectivity, 9 = highest):");
        println!(
            "      {:>3} {:>6}  {:>44}",
            "dec", "vertices", "median / p90 capacity, per species"
        );
        println!(
            "      {:>3} {:>6}  {}",
            "",
            "",
            SETTLERS
                .iter()
                .map(|s| format!("{s:>10}"))
                .collect::<Vec<_>>()
                .join("")
        );
        for d in 0..10 {
            let idxs: Vec<usize> = (0..n).filter(|&i| decile_of[i] == d).collect();
            let vertices_in_bucket = idxs.len();
            let mut row = String::new();
            for species_caps in &cap_vals {
                let mut vals: Vec<f64> = idxs.iter().map(|&i| species_caps[i]).collect();
                vals.sort_by(f64::total_cmp);
                let med = pct(&vals, 0.50);
                let p90 = pct(&vals, 0.90);
                row.push_str(&format!("{med:>4.1}/{p90:>4.1} "));
            }
            // Sanity-check diagnostic (brief: "check the capacity accessor is
            // doing what you think before believing it" if the surprising
            // branch is landed in). Per-decile viable share, both floors — if
            // this does not decline as prospectivity rises, either the
            // formula's inferred anti-correlation was wrong or the accessor
            // is not reading what this probe thinks it reads.
            let v_vmin = idxs.iter().filter(|&&i| max_cap[i] > VIABLE_MIN).count();
            let v_sk = idxs.iter().filter(|&&i| max_cap[i] > SURVIVE_K).count();
            println!(
                "      {d:>3} {vertices_in_bucket:>6}  {row} viable[VMIN {:>5.1}% SK {:>5.1}%]",
                v_vmin as f64 / vertices_in_bucket.max(1) as f64 * 100.0,
                v_sk as f64 / vertices_in_bucket.max(1) as f64 * 100.0,
            );
        }

        // 3. THE DECISIVE NUMBER. Among the top prospectivity decile (9),
        // what fraction of vertices are viable for ANY people in the roster, at
        // each of the two floors discussed above?
        let top_idxs: Vec<usize> = (0..n).filter(|&i| decile_of[i] == 9).collect();
        let top_n = top_idxs.len();
        let top_viable_vmin = top_idxs
            .iter()
            .filter(|&&i| max_cap[i] > VIABLE_MIN)
            .count();
        let top_viable_sk = top_idxs.iter().filter(|&&i| max_cap[i] > SURVIVE_K).count();
        let frac_vmin = top_viable_vmin as f64 / top_n.max(1) as f64;
        let frac_sk = top_viable_sk as f64 / top_n.max(1) as f64;
        println!("  [3] top decile ({top_n} vertices): viable (max-over-roster capacity > floor):");
        println!(
            "      VIABLE_MIN ({VIABLE_MIN}): {top_viable_vmin} / {top_n}  ({:.2}%)",
            frac_vmin * 100.0
        );
        println!(
            "      SURVIVE_K  ({SURVIVE_K}): {top_viable_sk} / {top_n}  ({:.2}%)",
            frac_sk * 100.0
        );

        // 4. The converse, for context: among "good capacity" vertices (max
        // over roster above VIABLE_MIN — the brief's named floor), what is
        // the prospectivity distribution, against land overall?
        let good_idxs: Vec<usize> = (0..n).filter(|&i| max_cap[i] > VIABLE_MIN).collect();
        let mut good_prosp: Vec<f64> = good_idxs.iter().map(|&i| land_prosp[i]).collect();
        good_prosp.sort_by(f64::total_cmp);
        let mut land_prosp_sorted = land_prosp.clone();
        land_prosp_sorted.sort_by(f64::total_cmp);
        println!(
            "  [4] converse: {} / {n} land vertices ({:.2}%) are 'good capacity' (max-over-roster \
             > VIABLE_MIN); their prospectivity distribution vs. land overall:",
            good_idxs.len(),
            good_idxs.len() as f64 / n.max(1) as f64 * 100.0,
        );
        println!(
            "      good-capacity prospectivity:  min {:.4}  p50 {:.4}  p90 {:.4}  max {:.4}",
            good_prosp.first().copied().unwrap_or(f64::NAN),
            pct(&good_prosp, 0.50),
            pct(&good_prosp, 0.90),
            good_prosp.last().copied().unwrap_or(f64::NAN),
        );
        println!(
            "      land prospectivity (repeated):  min {:.4}  p50 {:.4}  p90 {:.4}  max {:.4}",
            land_prosp_sorted.first().copied().unwrap_or(f64::NAN),
            pct(&land_prosp_sorted, 0.50),
            pct(&land_prosp_sorted, 0.90),
            land_prosp_sorted.last().copied().unwrap_or(f64::NAN),
        );

        top_decile_frac_viable_min.push(frac_vmin);
        top_decile_frac_survive_k.push(frac_sk);
    }

    println!("\n== summary across seeds {SEEDS:?} ==");
    println!("  top-decile viable fraction (VIABLE_MIN): {top_decile_frac_viable_min:?}");
    println!("  top-decile viable fraction (SURVIVE_K):  {top_decile_frac_survive_k:?}");

    // BRANCH ASSERTION (task-1b-brief.md's decision rule). Landed branch:
    // "most top-decile-ore vertices are viable" at BOTH floors, on every
    // seed — the brief's SURPRISING branch, since it says ore and fertility
    // are not anti-correlated in a way the viability floor can see, even
    // though the per-decile MEDIAN capacity (item 2's table) does decline as
    // prospectivity rises on 2 of 3 seeds, matching the mechanism task 1
    // inferred. Measured minimum across the panel: 71.49% (VIABLE_MIN),
    // 56.51% (SURVIVE_K), both on seed 1234 — see task-1b-report.md for the
    // full numbers this pins. These assertions redden if a later change
    // moves the joint distribution enough to cross a branch boundary
    // (either down toward "a small but real fraction" or "~0%", or the
    // measured minimum drifting further), rather than staying silently
    // stale.
    for &f in &top_decile_frac_viable_min {
        assert!(
            f > 0.0,
            "top-decile viable fraction (VIABLE_MIN) hit exactly zero — branch table's \
             'a mine cannot be a settlement' verdict; task-1b-report.md's WRITABLE-but-\
             surprising call must be re-derived"
        );
        assert!(
            f > 0.30,
            "top-decile viable fraction (VIABLE_MIN) fell to {:.4}, below the measured \
             panel minimum's margin (71.49% on seed 1234) — no longer solidly in the \
             'most top-decile-ore vertices are viable' branch task-1b-report.md landed in; \
             re-derive the branch table before trusting that report",
            f
        );
    }
    for &f in &top_decile_frac_survive_k {
        assert!(
            f > 0.0,
            "top-decile viable fraction (SURVIVE_K) hit exactly zero — branch table's \
             'a mine cannot be a settlement' verdict; task-1b-report.md's WRITABLE-but-\
             surprising call must be re-derived"
        );
        assert!(
            f > 0.30,
            "top-decile viable fraction (SURVIVE_K) fell to {:.4}, below the measured \
             panel minimum's margin (56.51% on seed 1234) — no longer solidly in the \
             'most top-decile-ore vertices are viable' branch task-1b-report.md landed in; \
             re-derive the branch table before trusting that report",
            f
        );
    }
}

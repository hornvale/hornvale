//! **M2 and M7 — do the marine peoples place, and does the deep one take the
//! map?** THE TIDEMARK, Task 3.
//!
//! Both are preregistered in spec §8 and both are TWO-SIDED, which is the
//! whole reason they are here rather than in a one-off probe:
//!
//! - **M2.** Each of the five `Settled` marine kinds places at least one
//!   settlement and fewer than the surface total; `merfolk`, being
//!   `Gregarious`, places **exactly zero**. A zero for a `Settled` kind means
//!   the realm gate admits nothing for it and that kind has shipped
//!   unreachable; a non-zero for merfolk means `SocialForm` is not reaching
//!   placement — "a defect in the opposite direction and ... invisible to a
//!   one-sided floor".
//! - **M7.** The abyssal elf's held-vertex count, against the elf family's
//!   own order. The hazard is real and measured for a sibling: sea-elf
//!   "authored to the whole ocean ... would hold ~27,000 vertices against
//!   wood's ~800; on the shelf band it holds ~1,425"
//!   (`radiation_affinity::the_sea_elf_is_confined_to_the_shelf_band`). The
//!   deep bands are the LARGE ones, so the same hazard applies with the sign
//!   of the confinement reversed.
//!
//! # The instrument M7 uses, stated because the spec's figures were taken
//! with a different one
//!
//! "Held" here means **the strict argmax of per-species capacity over the
//! whole settling roster** — the vertices at which this kind, and no other,
//! is the roster's best occupant. That is a comparative reading, and it has
//! to be: a NON-comparative one (say, "vertices with non-zero capacity")
//! cannot answer M7's question at all, because a `ConditionResponse` is a
//! Gaussian and returns a small positive number arbitrarily far from its
//! optimum. Measured while writing this file: a `Marine` kind reads non-zero
//! at every vertex holding a water column — ~29,700 of 40,962 at seed 42 —
//! whatever its elevation optimum, so a non-zero count would have reported
//! "the abyssal elf holds thirty thousand vertices" for ANY authoring,
//! including one confined to a single band.
//!
//! So the family's own anchors are re-measured HERE with this same
//! instrument, in the same run, rather than compared against the spec's
//! quoted ~800 and ~1,425. That is the point: a cross-instrument comparison
//! of two numbers is not a comparison of two quantities.

#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed, Value};
use hornvale_species::{HabitatRealm, SocialForm, SocietyVector};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to, climate_of, terrain_of,
};

/// The three seeds the sea-elf confinement was measured over (spec §8, M7).
const SEEDS: [u64; 3] = [42, 7, 1234];

fn world(wc: &WorldComponents, seed: u64, depth: BuildDepth) -> hornvale_kernel::World {
    build_world_to(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        depth,
    )
    .unwrap_or_else(|e| panic!("seed {seed} builds to {depth:?}: {e:?}"))
}

/// One settlement count per people that peoples at least one settlement,
/// ascending by name, from the committed ledger — the same read
/// `demesne.rs::peopled_kinds` performs, counting rather than set-collecting.
fn settlements_per_people(world: &hornvale_kernel::World) -> Vec<(String, usize)> {
    let mut counts: std::collections::BTreeMap<String, usize> = std::collections::BTreeMap::new();
    for fact in world.ledger.find(hornvale_species::PEOPLED_BY) {
        if let Value::Text(name) = &fact.object {
            *counts.entry(name.clone()).or_insert(0) += 1;
        }
    }
    counts.into_iter().collect()
}

fn marine_kinds() -> Vec<KindId> {
    hornvale_species::habitat_realm_registry()
        .iter()
        .filter(|(_, r)| **r == HabitatRealm::Marine)
        .map(|(k, _)| *k)
        .collect()
}

/// **M2, both poles, at seed 42 and `BuildDepth::Full`.**
///
/// claim: readout(preregistered)
#[test]
fn m2_the_settled_marine_peoples_place_and_the_gregarious_one_does_not() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = world(&wc, 42, BuildDepth::Full);
    let counts = settlements_per_people(&world);
    let lookup: std::collections::BTreeMap<&str, usize> =
        counts.iter().map(|(k, v)| (k.as_str(), *v)).collect();

    let bio = hornvale_species::biosphere_registry();
    let realms = hornvale_species::habitat_realm_registry();
    let realm_of = |k: &KindId| realms.get(k).copied().unwrap_or(HabitatRealm::SURFACE);

    let mut surface_total = 0usize;
    let mut marine_total = 0usize;
    for (name, count) in &counts {
        let kind = KindId(
            bio.iter()
                .map(|(k, _)| k.0)
                .find(|k| *k == name.as_str())
                .unwrap_or_else(|| panic!("{name} peoples a settlement but has no biosphere row")),
        );
        match realm_of(&kind) {
            HabitatRealm::Marine => marine_total += count,
            _ => surface_total += count,
        }
    }

    println!("M2 — settlements per people at seed 42, BuildDepth::Full:");
    for (name, count) in &counts {
        println!("  {name:>16}  {count:>4}");
    }
    println!("M2 totals: surface+subterranean {surface_total}, marine {marine_total}");

    for kind in marine_kinds() {
        let placed = lookup.get(kind.0).copied().unwrap_or(0);
        let form = bio
            .get(&kind)
            .expect("a marine kind has a biosphere row")
            .social_form;
        match form {
            SocialForm::Settled => {
                assert!(
                    placed >= 1,
                    "M2 FAILS: {} is `Settled` and `HabitatRealm::Marine` but places {placed} \
                     settlements at seed 42. A zero for a Settled kind means the realm gate \
                     admits nothing for it and the kind has shipped UNREACHABLE (spec §8, M2).",
                    kind.0
                );
                assert!(
                    placed < surface_total,
                    "M2 FAILS: {} places {placed} settlements against a surface total of \
                     {surface_total}. Exceeding the surface total means the marine realm is \
                     outcompeting land — a placement defect, not a success (spec §8, M2).",
                    kind.0
                );
            }
            SocialForm::Gregarious => {
                assert_eq!(
                    placed, 0,
                    "M2 FAILS in the OPPOSITE direction: {} is `Gregarious` — it forms no fixed \
                     place — and yet {placed} settlements are peopled by it. `SocialForm` is not \
                     reaching placement. This is the pole a one-sided floor could not see \
                     (spec §8, M2).",
                    kind.0
                );
            }
            other => panic!(
                "{} carries {other:?}; the six marine peoples are Settled or Gregarious",
                kind.0
            ),
        }
    }

    // Anti-vacuity: a build that peopled nothing at all would satisfy the
    // merfolk pole and fail nothing else visibly if the Settled arm were ever
    // weakened. The surface roster is the control.
    assert!(
        surface_total > 0,
        "no land settlement at seed 42 — this world is not a world and M2 measured nothing"
    );
}

/// The vertices a kind HOLDS: the strict argmax of `per_species_capacity`
/// over the whole settling roster. See this file's header for why the
/// measurement is comparative.
fn held_vertices(seed: u64) -> std::collections::BTreeMap<&'static str, usize> {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = world(&wc, seed, BuildDepth::Terrain);
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();

    let peoples: Vec<KindId> = wc
        .biosphere
        .iter()
        .filter(|(_, b)| b.social_form == SocialForm::Settled)
        .map(|(k, _)| *k)
        .collect();
    let bios: Vec<&hornvale_species::BiosphereTraits> = peoples
        .iter()
        .map(|k| {
            wc.biosphere
                .get(k)
                .expect("a settling people has a biosphere row")
        })
        .collect();
    let realm: Vec<HabitatRealm> = peoples
        .iter()
        .map(|k| {
            wc.habitat_realm
                .get(k)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE)
        })
        .collect();
    let affinities = hornvale_species::biome_affinity_registry();
    let affinity: Vec<Option<hornvale_species::BiomeAffinity>> =
        peoples.iter().map(|k| affinities.get(k).cloned()).collect();

    let obliquity_deg = climate.obliquity_deg();
    let insolation_scalar = climate.insolation();
    let regime = climate.regime();
    let hoisted = hornvale_worldgen::EraInvariantSupply::build(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let caps = hornvale_worldgen::per_species_capacity_at(
        geo,
        &terrain,
        &climate,
        &hoisted,
        &hornvale_worldgen::EraAdjust::present(&terrain),
        &bios,
        &realm,
        &affinity,
    );

    let mut held: std::collections::BTreeMap<&'static str, usize> =
        peoples.iter().map(|k| (k.0, 0usize)).collect();
    for vertex in geo.vertices() {
        let mut best: Option<(usize, f64)> = None;
        let mut tied = false;
        for (tag, map) in &caps {
            let v = map.at(vertex);
            match best {
                None => {
                    best = Some((*tag as usize, v));
                    tied = false;
                }
                Some((_, b)) if v > b => {
                    best = Some((*tag as usize, v));
                    tied = false;
                }
                Some((_, b)) if v == b => tied = true,
                _ => {}
            }
        }
        // A strict argmax only: a tie names no holder, and a zero field is
        // nobody's ground.
        if let Some((tag, value)) = best
            && !tied
            && value > 0.0
        {
            *held.get_mut(peoples[tag].0).expect("tag indexes peoples") += 1;
        }
    }
    held
}

/// **M7, at the three seeds the sea-elf confinement was measured over.**
///
/// claim: readout(preregistered)
#[test]
fn m7_the_abyssal_elf_does_not_dominate_the_map() {
    let mut abyssal = Vec::new();
    let mut wood = Vec::new();
    let mut sea = Vec::new();
    for seed in SEEDS {
        let held = held_vertices(seed);
        let get = |k: &str| held.get(k).copied().unwrap_or(0);
        // The WHOLE distribution, descending, on every run. M7 asks whether
        // one kind has taken the map, and that is a question about a kind's
        // place in the roster's order — a three-name readout could show the
        // abyssal elf inside the family's order while it sat far outside the
        // roster's, which is the same domination by a different denominator.
        let mut ranked: Vec<(&str, usize)> = held.iter().map(|(k, v)| (*k, *v)).collect();
        ranked.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(b.0)));
        println!("M7 seed {seed} — held vertices (strict argmax over the settling roster):");
        for (kind, count) in &ranked {
            println!("    {kind:>16}  {count:>6}");
        }
        println!(
            "M7 seed {seed}: abyssal-elf {:>6}   wood-elf {:>6}   sea-elf {:>6}   drow {:>6}",
            get("abyssal-elf"),
            get("wood-elf"),
            get("sea-elf"),
            get("drow")
        );
        abyssal.push(get("abyssal-elf"));
        wood.push(get("wood-elf"));
        sea.push(get("sea-elf"));
    }
    let mean = |v: &Vec<usize>| v.iter().sum::<usize>() as f64 / v.len() as f64;
    let (a, w, s) = (mean(&abyssal), mean(&wood), mean(&sea));
    println!("M7 three-seed means: abyssal-elf {a}, wood-elf {w}, sea-elf {s}");

    // The family anchor is the LARGER of the two siblings measured in the
    // same run by the same instrument — never a literal from the spec, which
    // was taken with a different one. "An order larger" is read as 10x.
    //
    // **THE MAX IS LOAD-BEARING AND MEASURED, NOT DEFENSIVE.** Under this
    // instrument `wood-elf` holds ZERO at all three seeds: it is never the
    // strict argmax anywhere, because an argmax over the settling roster is
    // a comparative reading and wood-elf is outcompeted everywhere it is
    // authored for. That is a fact about the instrument rather than about
    // wood-elf (the spec's "~800" is a different quantity), and it means
    // `min` here would divide the family's order by an empty set. So the
    // anchor is sea-elf in practice, which is the right sibling anyway: it
    // is the family's OTHER marine member and the one whose confinement was
    // measured for exactly this hazard.
    let anchor = w.max(s);
    assert!(
        anchor > 0.0,
        "neither wood-elf nor sea-elf holds a vertex at any of {SEEDS:?} — the anchor is \
         empty and M7 measured nothing"
    );
    assert!(
        a <= anchor * 10.0,
        "M7 FAILS: the abyssal elf holds {a} vertices (three-seed mean) against the elf \
         family's {anchor} — an ORDER larger. The deep bands have handed a single kind the \
         map the way the whole ocean would have handed it to sea-elf, and the remedy is the \
         one that worked there: BAND CONFINEMENT with its own pinning test, never a tuning \
         of its niche weights after the fact (spec §8, M7)."
    );
    assert!(
        a > 0.0,
        "M7 FAILS in the other direction: the abyssal elf holds NO vertex at any of \
         {SEEDS:?}. A kind that holds nothing is not confined, it is unreachable."
    );
}

/// The abyssal elf's confinement, asserted on the AUTHORING rather than on a
/// measured count — the shape
/// `radiation_affinity::the_sea_elf_is_confined_to_the_shelf_band` uses, and
/// the mirror of that row.
///
/// M7's count is a measurement over three worlds and moves when the world
/// does; this is the invariant behind it, and it is what a future campaign
/// would have to break deliberately to un-confine the kind.
#[test]
fn the_abyssal_elf_is_confined_to_the_deep_bands() {
    const DEEP: [&str; 3] = ["abyssal", "bathypelagic", "hadal-trench"];
    const SHELF: [&str; 5] = [
        "coral-reef",
        "kelp-forest",
        "upwelling",
        "epipelagic",
        "sea-ice",
    ];
    let registry = hornvale_species::biome_affinity_registry();
    let deep_elf = registry
        .get(&KindId("abyssal-elf"))
        .expect("abyssal-elf carries a biome affinity row");

    for b in DEEP {
        assert!(
            deep_elf.factor(b) > deep_elf.default,
            "abyssal-elf's {b} factor ({}) is not above its default ({}); the deep bands \
             must be a STRONGHOLD, and a factor at the default is indistinguishable from \
             silence",
            deep_elf.factor(b),
            deep_elf.default
        );
    }
    for b in SHELF {
        assert!(
            deep_elf.factor(b) <= deep_elf.default,
            "abyssal-elf's {b} factor ({}) is above its default ({}). This kind is the \
             OBLIGATE deep counterpart to sea-elf's shelf confinement; lifting a shelf class \
             here would put the family's two marine members in competition for one band and \
             re-open exactly the dominance problem `the_sea_elf_is_confined_to_the_shelf_band` \
             closed.",
            deep_elf.factor(b),
            deep_elf.default
        );
    }

    // The two rows must PARTITION the ocean rather than merely differ: every
    // class one of them lifts, the other must leave at its default.
    let sea_elf = registry
        .get(&KindId("sea-elf"))
        .expect("sea-elf carries a biome affinity row");
    for b in DEEP.iter().chain(SHELF.iter()) {
        let deep_lifted = deep_elf.factor(b) > deep_elf.default;
        let shelf_lifted = sea_elf.factor(b) > sea_elf.default;
        assert!(
            !(deep_lifted && shelf_lifted),
            "both elves lift {b}: the family's two marine members must not share a \
             stronghold class"
        );
    }
}

/// A `Gregarious` people carries a society row and no settlement, which is
/// decision 0068's whole content. Asserted on the authoring so M2's zero has
/// something behind it that does not need a world built.
#[test]
fn merfolk_carry_a_society_and_no_settlement_form() {
    let bio = hornvale_species::biosphere_registry();
    let merfolk = bio
        .get(&KindId("merfolk"))
        .expect("merfolk has a biosphere row");
    assert_eq!(merfolk.social_form, SocialForm::Gregarious);
    let society: Option<SocietyVector> = hornvale_species::society_registry()
        .get(&KindId("merfolk"))
        .copied();
    assert!(
        society.is_some(),
        "a minded, social kind carries a society vector — decision 0068's gate is \
         `minded ∧ social`, not `Settled`"
    );
}

/// **W5 — does the kelp tender's `PHOTOSYNTHATE` weight actually move its
/// score?** Measured, never assumed, because the brief made it a condition:
/// "carry its phototrophy through `TrophicMode` and the `PHOTOSYNTHATE`
/// weight — then verify that weight actually moves its score rather than
/// assuming it. If it does not move, say so; that is a finding, not a
/// failure."
///
/// The instrument is the production entry point with ONE input changed: the
/// same world, the same roster, the same realm and affinity slices, and a
/// kelp tender whose 0.40 `PHOTOSYNTHATE` / 0.60 `MARINE_FORAGE` niche is
/// replaced by a pure 0.60 `MARINE_FORAGE` one — the SAME marine weight, so
/// the only difference between the arms is whether the photosynthate term is
/// summed in at all. (A re-normalised `MARINE_FORAGE 1.0` twin would have
/// changed two things at once and could not have answered the question.)
///
/// claim: readout(preregistered)
#[test]
fn w5_the_kelp_tenders_photosynthate_weight_is_measured_not_assumed() {
    use hornvale_kernel::{MARINE_FORAGE, PHOTOSYNTHATE, ResourceVector};

    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = world(&wc, 42, BuildDepth::Terrain);
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();

    let shipped = wc
        .biosphere
        .get(&KindId("kelp-tender"))
        .expect("kelp-tender has a biosphere row")
        .clone();
    assert!(
        shipped.niche.weight(PHOTOSYNTHATE) > 0.0,
        "this measurement is about a weight the kelp tender does not carry"
    );
    let mut ablated = shipped.clone();
    ablated.niche = ResourceVector::new(&[(MARINE_FORAGE, shipped.niche.weight(MARINE_FORAGE))])
        .expect("a single-axis vector is valid");

    let realm = vec![HabitatRealm::Marine];
    let affinities = hornvale_species::biome_affinity_registry();
    let affinity = vec![affinities.get(&KindId("kelp-tender")).cloned()];

    let obliquity_deg = climate.obliquity_deg();
    let insolation_scalar = climate.insolation();
    let regime = climate.regime();
    let hoisted = hornvale_worldgen::EraInvariantSupply::build(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let run = |bio: &hornvale_species::BiosphereTraits| {
        hornvale_worldgen::per_species_capacity_at(
            geo,
            &terrain,
            &climate,
            &hoisted,
            &hornvale_worldgen::EraAdjust::present(&terrain),
            &[bio],
            &realm,
            &affinity,
        )
    };
    let with = run(&shipped);
    let without = run(&ablated);

    let mut moved = 0usize;
    let mut wet = 0usize;
    let mut biggest = 0.0_f64;
    for vertex in geo.vertices() {
        let a = with[0].1.at(vertex);
        let b = without[0].1.at(vertex);
        if a > 0.0 || b > 0.0 {
            wet += 1;
        }
        if a.to_bits() != b.to_bits() {
            moved += 1;
            biggest = biggest.max((a - b).abs());
        }
    }
    println!(
        "W5 seed 42: the kelp tender's PHOTOSYNTHATE weight moves capacity at {moved} of \
         {wet} scoring vertices; largest absolute move {biggest}"
    );
    // BOTH POLES ARE RESULTS, and neither is asserted as the right one —
    // the assertion is only that the measurement happened over a real
    // population, so a null cannot come from an empty denominator.
    assert!(
        wet > 0,
        "the kelp tender scores nowhere at seed 42 — W5 measured nothing, and the null \
         below would be an artifact of an empty population rather than a finding"
    );
}

/// **I4 re-examined: is a BEHAVIOURAL guard possible now that real marine
/// kinds exist?**
///
/// `marine_ladder_vents::the_bake_hoists_the_vent_bearing_marine_habitat` is
/// a source-text scan, and its own doc records why: an outcome test was
/// measured unable to detect the `build_at` -> `build` reversion, because
/// the bake is not a capacity argmax and no real marine kind existed to
/// move. Six do now, and one of them (`vent-commensal`) weights
/// `CHEMOSYNTHATE` — so the question is re-opened HERE rather than assumed
/// closed, and this probe is the instrument that answers it.
///
/// It asks the narrowest sufficient question: **of the vertices the vent
/// commensal actually settles at seed 42, how many does the vent layer
/// improve?** A site the vent layer does not touch cannot move when the vent
/// layer is un-wired, so if that count is zero the reversion is invisible to
/// any siting assertion and the scan stays.
///
/// claim: readout(measured)
#[test]
fn i4_are_the_vent_commensals_sites_vent_lit() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = world(&wc, 42, BuildDepth::Full);
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();

    // The vent commensal's settled vertices, from the committed ledger.
    let mut sites: Vec<u32> = Vec::new();
    for fact in world.ledger.find(hornvale_species::PEOPLED_BY) {
        if fact.object != Value::Text("vent-commensal".to_string()) {
            continue;
        }
        let subject = fact.subject;
        for f in world.ledger.find(hornvale_settlement::VERTEX_ID) {
            if f.subject == subject
                && let Value::Number(n) = &f.object
            {
                sites.push(*n as u32);
            }
        }
    }
    sites.sort_unstable();
    sites.dedup();
    assert!(
        !sites.is_empty(),
        "the vent commensal settles nowhere at seed 42 — this probe has no population and \
         cannot answer I4 in either direction"
    );

    // The two hoists the reversion chooses between: the ambient marine
    // habitat (`build`) and the vent-bearing one (`build_at`, at genesis),
    // scored through the production capacity entry point for this one kind.
    let bio = wc
        .biosphere
        .get(&KindId("vent-commensal"))
        .expect("vent-commensal has a biosphere row");
    let realm = vec![HabitatRealm::Marine];
    let affinities = hornvale_species::biome_affinity_registry();
    let affinity = vec![affinities.get(&KindId("vent-commensal")).cloned()];
    let obliquity_deg = climate.obliquity_deg();
    let insolation_scalar = climate.insolation();
    let regime = climate.regime();

    let ambient = hornvale_worldgen::EraInvariantSupply::build(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let water = hornvale_worldgen::waterworld::waterworld_from(
        &world,
        &terrain,
        &climate,
        hornvale_worldgen::waterworld::WaterWorldConfig { enabled: true },
    );
    let vented = hornvale_worldgen::EraInvariantSupply::build_at(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &water,
        hornvale_kernel::WorldTime::GENESIS,
    );
    let run = |hoisted: &hornvale_worldgen::EraInvariantSupply| {
        hornvale_worldgen::per_species_capacity_at(
            geo,
            &terrain,
            &climate,
            hoisted,
            &hornvale_worldgen::EraAdjust::present(&terrain),
            &[bio],
            &realm,
            &affinity,
        )
    };
    let a = run(&ambient);
    let v = run(&vented);

    let mut improved_globally = 0usize;
    for vertex in geo.vertices() {
        if a[0].1.at(vertex).to_bits() != v[0].1.at(vertex).to_bits() {
            improved_globally += 1;
        }
    }
    let mut improved_sites = 0usize;
    for site in &sites {
        let vertex = hornvale_kernel::Vertex(*site);
        let (x, y) = (a[0].1.at(vertex), v[0].1.at(vertex));
        println!("  I4 site vertex {site}: ambient {x}, with vents {y}");
        if x.to_bits() != y.to_bits() {
            improved_sites += 1;
        }
    }
    println!(
        "I4 seed 42: the vent layer moves the vent commensal's capacity at {improved_globally} \
         vertices globally, and at {improved_sites} of its {} settled sites",
        sites.len()
    );
    // PRECONDITION, so a zero in the second number cannot be read as "the
    // vent layer does nothing" when the truth is "this probe is not looking
    // at the vent layer at all".
    assert!(
        improved_globally > 0,
        "the vent layer moves this kind's capacity NOWHERE — the probe's own instrument is \
         dead, and the site count below would be meaningless"
    );
    // **THE BEHAVIOURAL ASSERTION I4 ASKED FOR, AND IT FIRES.** Every site
    // this kind settles is a vertex the vent layer improves. That is an
    // OUTCOME consequence of `bake_history_from` hoisting `build_at`: under
    // the reversion placement scores the ambient field, in which these
    // vertices carry ~5.8 headcount rather than ~37, and the bake has no
    // reason to prefer them. The margin is what makes this a guard rather
    // than a coincidence — the vent layer improves 337 of 40,962 vertices
    // (0.82%), so three independent sites all landing on improved ground by
    // chance is ~6e-6.
    //
    // VERIFIED BY MUTATION, not argued: reverting
    // `EraInvariantSupply::build_at` to `build` inside `bake_history_from`
    // reddens THIS assertion (the sites move off the vent field entirely),
    // with the target text asserted present by `grep -c -F` before the
    // perturbation so a no-op could not pass as a result.
    assert_eq!(
        improved_sites,
        sites.len(),
        "the vent commensal settles {} sites and only {improved_sites} of them sit on ground \
         the vent layer improves. This kind's habitat IS the vent (spec §4) — if the bake is \
         siting it on ordinary seabed, the vent-bearing hoist is not reaching placement. Check \
         `bake_history_from`'s `EraInvariantSupply::build_at` first.",
        sites.len()
    );
}

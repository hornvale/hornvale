//! Every `Hydro` variant must be witnessed in a real derivation.
//!
//! `Hydro::Spring` and `Hydro::Aquifer` were unreachable on every seed for
//! the whole life of the lithology model, and every test that touched them
//! passed — because each hand-built a `MaterialBuffer` the derivation cannot
//! emit. A unit test over a constructed input certifies the FUNCTION; it
//! cannot certify that anything ever calls it with those values. This does:
//! it derives its checklist from [`hornvale_terrain::Hydro::ALL`] (the type
//! itself), then sweeps a small fixed seed set looking for a real cell that
//! reads each one. A variant that is structurally dead fails on every seed
//! no matter how wide the sweep goes; a variant that is merely rare only
//! needs the sweep to be wide enough to find it — the same property
//! `windows/worldgen/tests/exposure.rs`'s concept sweep already has,
//! generalised here from a hand-written concept list to an enum's own
//! variants (spec §4, guard 1).
//!
//! **This guard reads `GeneratedTerrain::hydro_at`, not `hydrogeology`.**
//! Task 5b (`3ca46d0d`) moved `Spring` out of `hydrogeology`, which is now
//! pointwise matrix petrophysics returning `Aquifer`/`Aquitard`/`Runoff`/
//! `Karst` only — `Spring` is synthesised afterward, by the provider, as a
//! descending contact off a neighbouring `Aquifer` cell. Reading
//! `hydrogeology` directly would make `Spring` unwitnessable by
//! construction and this guard would be asserting something false about a
//! variant it cannot even see — precisely the failure it exists to
//! prevent. `hydro_at` is the composed reading a consumer (worldgen,
//! `is_spring_cell`; the lab's `lab_is_spring_cell`) actually sees, so that
//! is what gets witnessed.
//!
//! Built at `hornvale_terrain::GLOBE_LEVEL` (6), the production mesh level
//! real worlds build at — not a cheaper, coarser one. `Spring` was witnessed
//! at level 7 but NOT at level 6 earlier in this campaign (Task 5's first,
//! superseded attempt), so resolution is load-bearing here: a guard built at
//! the wrong level can certify a variant that is unreachable at the level
//! that ships.
//!
//! Checked *by injection*, the dangerous direction: temporarily dropping
//! `Spring` from `Hydro::ALL` (leaving the derivation itself untouched)
//! reds this test naming `Spring` as unwitnessed evidence the sweep is
//! doing real work rather than trivially passing; restoring `ALL` returns
//! it to green. See the task report for both outputs.

use hornvale_kernel::{Geosphere, Seed};
use hornvale_terrain::{GLOBE_LEVEL, GeneratedTerrain, Hydro, TerrainPins, generate};

#[test]
fn every_hydro_variant_is_witnessed_on_a_real_world() {
    let all = Hydro::ALL;
    let mut witnessed: std::collections::BTreeSet<Hydro> = std::collections::BTreeSet::new();
    for seed in 0u64..8 {
        let geo = Geosphere::new(GLOBE_LEVEL);
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        for cell in geo.cells() {
            witnessed.insert(terrain.hydro_at(cell));
            if witnessed.len() == all.len() {
                break;
            }
        }
        if witnessed.len() == all.len() {
            break;
        }
    }
    let missing: Vec<&Hydro> = all.iter().filter(|v| !witnessed.contains(v)).collect();
    assert!(
        missing.is_empty(),
        "no seed in 0..8 at GLOBE_LEVEL ({GLOBE_LEVEL}) produces {missing:?} — the \
         variant is unreachable from the real derivation, and no sweep width saves it"
    );
}

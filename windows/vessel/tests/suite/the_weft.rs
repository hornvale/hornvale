//! The Weft: the walk band reports a ruin.

use hornvale_locale::LocaleContext;
use hornvale_vessel::brief::brief_of;

/// A facet holding a dead occupation reports a ruin signature. Before The
/// Weft, `brief_of` had no field for this at all: `brief.rs` omitted the
/// ruin signature on purpose, so a walker standing on a dead civilisation
/// was told only its biome.
#[test]
fn a_dead_occupation_reports_a_ruin_signature() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let occupations = hornvale_worldgen::occupations_by_vertex(&world);
    let ruin_vertex = occupations
        .iter()
        .find(|(_, recs)| recs.iter().any(|r| r.core.ended.is_some()))
        .map(|(v, _)| *v)
        .expect("seed 42 has at least one dead occupation");

    let geo = ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&ctx);
    let terrain = hornvale_vessel::liveness::LocaleTerrain::new(&ctx);
    let place = hornvale_kernel::Facet::containing(geo.position(ruin_vertex), walk);

    let brief = brief_of(
        &occupations,
        geo,
        ctx.nearest_index(),
        &place,
        &terrain,
        walk,
        world.seed,
        &ctx.strange_sites(),
        &ctx.terrain().cave_site_vertices(),
    );

    let ruin = brief
        .ruin
        .expect("a facet at a dead occupation carries a ruin signature");
    assert!(
        ruin.ended.is_finite(),
        "a ruin's end must be a real instant, got {}",
        ruin.ended
    );
}

/// The line names the ruin and its cause, and says nothing when there is no
/// cause on the record rather than inventing one.
#[test]
fn the_ruin_line_names_a_cause_only_when_the_record_has_one() {
    use hornvale_vessel::brief::RuinSignature;
    use hornvale_vessel::ruin_prose::ruin_line;

    let with = RuinSignature {
        cause: Some(hornvale_history::record::CauseOfEnd::Famine),
        ended: 100.0,
        by_hand: false,
    };
    let without = RuinSignature {
        cause: None,
        ended: 100.0,
        by_hand: false,
    };

    let a = ruin_line(&with);
    let b = ruin_line(&without);
    assert!(
        a.to_lowercase().contains("famine"),
        "cause must reach the prose: {a}"
    );
    assert!(
        !b.to_lowercase().contains("famine"),
        "no cause must invent none: {b}"
    );
    assert!(!b.is_empty(), "a causeless ruin is still a ruin: {b}");
}

//! The Warp, Task 2: the wetness word is cut by ONE threshold, and the axis
//! it cuts is readable outside `describe` through the same code path.

use hornvale_kernel::{Facet, WorldTime};
use hornvale_locale::{LocaleContext, wetness_axis};
use hornvale_worldgen::{Wetness, wetness_sign};

// `LocaleContext::build`, not `terrain_of`/`climate_from` directly: the
// latter two are decision-0092 derivation entry points, gated to named
// construction sites; `build` is already one, and `ctx.terrain()`/
// `ctx.climate()` below give every reader this test needs.
fn ctx() -> (hornvale_kernel::World, LocaleContext) {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds");
    (world, ctx)
}

/// One representative land facet per 137th vertex — the same stride the
/// Weft's own walk sample uses (`WEFT_ENCOUNTER_WALK_STRIDE`, `windows/lab`) —
/// so the check is over hundreds of rooms, not one.
///
/// **Filtered to the land arm's own predicate, not `is_ocean`.** The brief's
/// draft used `is_ocean` alone, reasoning that only the water arm's words
/// ("swept by a current"/"in slack water", `grep -n "fn water_micro_habitat"`)
/// needed excluding. That missed a second collision: a permanent ice sheet is
/// LAND (`Medium::AirOverRock`, so `is_ocean` is false) but renders through
/// `ice_micro_habitat`, whose wetness words are "drifted deep"/"scoured bare"
/// — neither contains "damp" or "dry". Filtering on
/// `wetness_is_grounded` (`expr.realm.medium == AirOverRock && formation !=
/// Ice`) is the actual dispatch predicate `micro_habitat` uses to route to
/// `land_micro_habitat`, so it excludes both collisions at once.
fn sample(ctx: &LocaleContext) -> Vec<Facet> {
    let geo = ctx.climate().geosphere();
    let depth = geo.depth() + 7;
    (0..geo.vertex_count())
        .step_by(137)
        .map(|v| Facet::containing(geo.position(hornvale_kernel::Vertex(v as u32)), depth))
        .filter(|f| {
            let weights = f
                .corner_weights(geo, ctx.nearest_index())
                .expect("below the globe level");
            let best = hornvale_locale::dominant_corner(&weights).0;
            let expr = ctx.climate().biome_expr_at(best);
            hornvale_locale::wetness_is_grounded(expr)
        })
        .collect()
}

#[test]
fn the_exposed_wetness_axis_is_the_one_the_descriptor_reads() {
    let (_, ctx) = ctx();
    let rooms = sample(&ctx);
    // Seed 42 draws a high ocean fraction (`ocean_fraction` draws 0.50-0.75,
    // `docs/audits/land-elevation-attribution.md`), so a stride-137 sample
    // restricted to the land arm's own predicate is measured at 82 rooms, not
    // the ~150 a lower ocean fraction would give. The floor guards against a
    // degenerate near-empty sample (a real regression: a `dominant_corner` or
    // `corner_weights` bug that silently excludes almost everything), not
    // against this specific count.
    assert!(
        rooms.len() >= 50,
        "fixture check: {} land rooms sampled",
        rooms.len()
    );
    for addr in &rooms {
        let loc = ctx.describe(addr, WorldTime::GENESIS).expect("describe");
        let axis = ctx.wetness_axis_at(addr).expect("below the globe level");
        assert_eq!(
            axis, loc.regime.micro.wetness,
            "room {addr:?}: two code paths, two answers"
        );
    }
}

#[test]
fn the_rendered_wetness_word_is_the_sign_functions_word() {
    let (_, ctx) = ctx();
    let mut damp = 0;
    let mut dry = 0;
    for addr in sample(&ctx) {
        let loc = ctx.describe(&addr, WorldTime::GENESIS).expect("describe");
        let d = &loc.regime.descriptor;
        match wetness_sign(loc.regime.micro.wetness) {
            Wetness::Damp => {
                assert!(d.contains("damp"), "{d:?}");
                damp += 1;
            }
            Wetness::Dry => {
                assert!(d.contains("dry"), "{d:?}");
                dry += 1;
            }
            Wetness::Mid => assert!(!d.contains("damp") && !d.contains(", dry"), "{d:?}"),
        }
    }
    assert!(
        damp > 0 && dry > 0,
        "fixture check: both words must occur in the sample (damp {damp}, dry {dry}) or the assertion above is vacuous on one arm"
    );
}

#[test]
fn the_free_function_agrees_with_the_context_method() {
    let (world, ctx) = ctx();
    let geo = ctx.climate().geosphere();
    for addr in sample(&ctx) {
        let weights = addr
            .corner_weights(geo, ctx.nearest_index())
            .expect("below the globe level");
        let free = wetness_axis(
            ctx.terrain(),
            ctx.climate(),
            ctx.nearest_index(),
            world.seed,
            &addr,
            &weights,
        );
        assert_eq!(Some(free), ctx.wetness_axis_at(&addr));
    }
}

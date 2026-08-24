//! The almanac reads a ledger that speaks **days** and renders prose that
//! speaks **years** (The Ell). Both crossings are on the read side of this
//! window, and neither is visible to the compiler: `Value::Number` is an
//! untyped `f64`, so dropping either one leaves the crate green and rewrites
//! the prose.
//!
//! The committed gallery page `book/src/gallery/history-seed-42.md` would catch
//! it — but only through the artifact drift ritual, which is **not** in
//! `make gate`, and CI is manual-only. Spec §6 requires the crossing itself to
//! have a test that fails when the conversion is dropped, so these are it.
//!
//! Hand-built rather than bake-driven: `hornvale-worldgen` cannot be a
//! dev-dependency here (it already depends on this crate — see
//! `flesh_id_invariance.rs`'s note), so the fixture mirrors
//! `history_emit::emit_history`'s encoding, crossing years into days at the
//! same boundary the real emit does.

use hornvale_history::{
    HISTORY_NOW, IS_OCCUPATION, OCC_FOUNDED, OCC_FOUNDED_FROM, OCC_FUNCTION, OCC_NOTABILITY,
    OCC_PEAK, OCC_PEOPLE, OCC_SITE, OCC_TECH,
};
use hornvale_kernel::{EntityId, Fact, Seed, Value, Vertex, World, WorldTime};

/// Bake year → standard day, the crossing `emit_history` performs. Shared with
/// production as a constant (`Years::DAYS_PER_YEAR`), not as a function.
fn cross(year: f64) -> f64 {
    year * hornvale_kernel::Years::DAYS_PER_YEAR
}

/// The bake year our fixture community was founded in.
const FOUNDED_YEAR: f64 = 500.0;
/// The bake year the fixture world's history closes at.
const NOW_YEAR: f64 = 2000.0;

/// A world holding exactly one **living** gnoll occupation at vertex 1400,
/// founded in year 500, with the present committed at year 2000 — every time
/// value crossed into days on the way in, as the real emit does.
///
/// Living on purpose: a ruin's span sentence reads its own `ended`, but a
/// living one's tenure is `now − founded`, which is the only route
/// `present_year` reaches the prose by.
fn world_with_a_living_community() -> World {
    let mut world = World::new(Seed(42));
    hornvale_history::register_concepts(&mut world.registry).expect("register history concepts");
    let id = EntityId::new(4242).expect("nonzero");
    let day = cross(FOUNDED_YEAR);

    let mut commit = |subject: EntityId, predicate: &str, object: Value, day: f64| {
        world
            .ledger
            .commit(
                Fact {
                    subject,
                    predicate: predicate.to_string(),
                    object,
                    place: Some(subject),
                    day: Some(WorldTime::from_std_days(day).expect("test fixture day is finite")),
                    provenance: "history-units-test".to_string(),
                },
                &world.registry,
            )
            .expect("fixture fact must be committable");
    };

    // `layers_at` reads the `is-occupation` index first, so without this the
    // site renders "nothing ever settled here" and both tests below would be
    // asserting on an empty page.
    commit(id, IS_OCCUPATION, Value::Flag(true), day);
    commit(id, OCC_PEOPLE, Value::Text("gnoll".to_string()), day);
    commit(id, OCC_SITE, Value::Number(1400.0), day);
    commit(id, OCC_FOUNDED, Value::Number(day), day);
    commit(id, OCC_PEAK, Value::Number(240.0), day);
    commit(id, OCC_TECH, Value::Text("classical".to_string()), day);
    commit(id, OCC_FUNCTION, Value::Text("agrarian".to_string()), day);
    commit(id, OCC_NOTABILITY, Value::Text("seat".to_string()), day);
    commit(id, OCC_FOUNDED_FROM, Value::Number(1400.0), day);
    // `history-now` is a world-scalar stamped at genesis, exactly as `emit_now`
    // commits it: the object is the crossed year, the stamp is day zero.
    commit(id, HISTORY_NOW, Value::Number(cross(NOW_YEAR)), 0.0);

    world
}

/// The founding renders as the **year** it was baked in.
///
/// Guards `record_of`'s `occ-founded` crossing. Drop it and the page reads
/// "Founded in the year 182625" — 500 × 365.25, a number no reader could tell
/// was wrong without knowing the bake's span.
#[test]
fn a_founding_renders_as_the_bake_year_not_the_stored_day() {
    let prose =
        hornvale_almanac::history::render_site(&world_with_a_living_community(), Vertex(1400));
    assert!(
        prose.contains("Founded in the year 500,"),
        "the founding must render as its bake year; got:\n{prose}"
    );
}

/// A living community's tenure is measured from the **committed present**, in
/// years.
///
/// Guards `present_year`'s `history-now` crossing — the one route by which that
/// read reaches rendered prose. Drop it and `now` is 730500 against a founding
/// of 500, so the tenure reads 730000 years instead of 1500. Nothing else in
/// the crate notices: `flesh_seed` never reads the present, and a ruin's span
/// sentence takes its end from its own `occ-ended`.
#[test]
fn a_living_communitys_tenure_is_measured_from_the_committed_present() {
    let prose =
        hornvale_almanac::history::render_site(&world_with_a_living_community(), Vertex(1400));
    let expected = (NOW_YEAR - FOUNDED_YEAR) as i64;
    assert!(
        prose.contains(&format!("it stands yet — {expected} years and counting")),
        "a living community's tenure must be `now − founded` in YEARS \
         ({expected}); got:\n{prose}"
    );
}

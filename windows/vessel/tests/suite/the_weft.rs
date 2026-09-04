//! The Weft: the walk band reports a ruin (Task 2) and a derived weft
//! feature (Task 8).

use hornvale_locale::LocaleContext;
use hornvale_vessel::brief::brief_of;
use hornvale_vessel::{PossessOpts, Session, Turn};

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

// --- Task 8: the derived surface reaches the walker -----------------------

/// Every compass word `Session::go` accepts, tried in a fixed order at each
/// step: whichever bearing actually moves the possession is used, so an
/// obstructed one (a cube corner, `CORNER_BEARING_REFUSAL`) never stalls the
/// walk — the next word in the list is tried instead, and the order restarts
/// at the top for the following step.
const COMPASS_WORDS: [&str; 8] = [
    "north",
    "northeast",
    "east",
    "southeast",
    "south",
    "southwest",
    "west",
    "northwest",
];

/// How many single-facet `go` attempts (successful or refused) the walk
/// below is allowed before giving up. Task 7's full-grid table (every
/// walk-depth facet over seed 42's 40,962 vertices) measured spring at
/// 0.984%, overhang at 2.058%, thicket at 3.703% and erratic at 1.045% —
/// roughly 7-8% combined per facet, ignoring correlation — so an expected
/// hit arrives within a few tens of steps; this budget is wide headroom, not
/// a tuned minimum, and a probe run found one after a single step.
const WALK_BUDGET: usize = 2_000;

/// Independently derive `session`'s own weft surface at its CURRENT
/// position — the same construction site `windows/worldgen/tests/suite/
/// weft_window.rs`'s `Fixture` uses, built once per test and reused across
/// every step so a 2,000-step walk pays one `terrain_of`/`climate_from`
/// pair, not one per step.
struct Oracle {
    world: hornvale_kernel::World,
    terrain: hornvale_terrain::GeneratedTerrain,
    pack: hornvale_worldgen::FieldPack,
    index: hornvale_kernel::NearestVertexIndex,
}

impl Oracle {
    // Named construction site (decision 0092): the same `terrain_of`/
    // `climate_from` pair `windows/worldgen/tests/suite/weft_window.rs`'s
    // own `Fixture::build` uses, for the same reason — an independent
    // oracle over seed 42's real terrain/climate, built once per test.
    #[allow(clippy::disallowed_methods)]
    fn build() -> Self {
        let world = hornvale_worldgen::seed_42_world();
        let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
        let climate =
            hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
        let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
        let index = hornvale_kernel::NearestVertexIndex::new(terrain.geosphere());
        Self {
            world,
            terrain,
            pack,
            index,
        }
    }

    /// The features [`hornvale_worldgen::all_features_at_cached`] derives at
    /// `session`'s own `position()` — `cache: None`, so this is a direct
    /// derivation and shares no state with the session's own private
    /// `weft_window`, which is the point: an independent oracle, not a
    /// second read of the same cache.
    fn features_at(&self, session: &Session<'_>) -> Vec<hornvale_worldgen::WeftFeature> {
        hornvale_worldgen::all_features_at_cached(
            &session.position(),
            self.terrain.geosphere(),
            &self.index,
            &self.pack,
            self.world.seed,
            None,
        )
    }
}

/// The walk band actually reports a derived feature. Anti-vacuity is the
/// point, not a nicety — "a prose test over a facet with nothing on it
/// passes for the wrong reason" is the exact failure this test rules out, so
/// the possession genuinely walks (through the real `Session::handle("go
/// ...")` verb loop, not a synthetic facet hop) until an INDEPENDENTLY
/// derived facet carries at least one weft feature, and only THEN asserts
/// the same session's own `look` output names it.
#[test]
fn the_walk_reports_a_derived_feature_when_it_finds_one() {
    let oracle = Oracle::build();
    let (mut s, _) =
        Session::start(&oracle.world, &PossessOpts::default()).expect("seed 42 possesses");

    let mut found = oracle.features_at(&s);
    let mut steps = 0usize;
    while found.is_empty() && steps < WALK_BUDGET {
        let before = s.position();
        for word in COMPASS_WORDS {
            match s.handle(&format!("go {word}")) {
                Turn::Out(_) => {}
                Turn::Released(t) => panic!("go must not release the session: {t}"),
            }
            if s.position() != before {
                break;
            }
        }
        steps += 1;
        found = oracle.features_at(&s);
    }
    assert!(
        !found.is_empty(),
        "the walk never reached a facet the independent oracle says carries a \
         derived feature, in {WALK_BUDGET} steps — either the walk is stuck \
         (every bearing refused at every step) or Task 7's measured density \
         (spring 0.984%, overhang 2.058%, thicket 3.703%, erratic 1.045% of \
         40,962 facets) has regressed"
    );

    let expected = hornvale_vessel::weft_prose::weft_clause(&found);
    assert!(
        !expected.is_empty(),
        "fixture check: a non-empty feature list must render non-empty prose, \
         or the assertion below is vacuous: {found:?}"
    );
    let reply = match s.handle("look") {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("look must not release the session: {t}"),
    };
    assert!(
        reply.contains(expected.trim()),
        "the walk-band prose must name every derived feature the independent \
         oracle found at this facet: expected {expected:?} inside {reply:?}"
    );
}

/// The converse: a facet the independent oracle finds EMPTY renders no weft
/// clause at all — silence, not a placeholder sentence. Uses the
/// possession's own START position, verified empty first (a FIXTURE CHECK,
/// the same discipline this file's `a_dead_occupation_reports_a_ruin_
/// signature` and `the_prospect.rs`'s several fixture checks already use) —
/// a red here means seed 42's start facet has moved, not that silence is
/// broken, and the message says so.
#[test]
fn a_facet_with_nothing_derived_stays_silent() {
    let oracle = Oracle::build();
    let (mut s, _) =
        Session::start(&oracle.world, &PossessOpts::default()).expect("seed 42 possesses");

    let features = oracle.features_at(&s);
    assert!(
        features.is_empty(),
        "fixture check: the possession's own start facet must carry no derived \
         feature, or this test proves nothing about silence: {features:?}"
    );

    let reply = match s.handle("look") {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("look must not release the session: {t}"),
    };
    for phrase in [
        "spring seeps",
        "overhang offers",
        "thicket presses",
        "boulder sits",
    ] {
        assert!(
            !reply.contains(phrase),
            "a facet with nothing derived must render no weft clause, but the \
             prose contains {phrase:?}: {reply}"
        );
    }
}

/// Carried from Task 7: the overhang's affordance proves the query path end
/// to end. `weft_offers` routes a `WeftKind` through the SAME `offered()`
/// subset query real placed things use through `offered_by`, so this is
/// evidence the query machinery answers correctly for a kind that was never
/// a `KindId` — not merely a restatement of `weft_object_registry`'s own
/// table.
#[test]
fn the_overhang_affords_shelter_and_warmth() {
    use hornvale_vessel::affordance::{OfferedVerb, weft_offers};
    use hornvale_worldgen::WeftKind;

    let overhang = weft_offers(WeftKind::Overhang);
    assert!(
        overhang.contains(&OfferedVerb::Sleep),
        "the overhang must offer shelter (Sleep, gated on SupportsRest): {overhang:?}"
    );
    assert!(
        overhang.contains(&OfferedVerb::Warm),
        "the overhang must offer warmth (Warm, gated on RadiatesHeat): {overhang:?}"
    );
    assert!(
        overhang.contains(&OfferedVerb::Examine),
        "Examine is universal (empty required set): {overhang:?}"
    );

    // The negative controls: spec §5.6 assigns the affordance claim to the
    // overhang alone, so the other three kinds must offer only the universal
    // `Examine` — nothing property-gated at all, and NOT the empty set
    // (`Examine`'s own required-property set is empty, so it is offered even
    // to a kind carrying zero properties — the same "universal" clause
    // `offered_by`'s own doc states for a `THING_KINDS` row absent from
    // `object_registry`).
    for kind in [WeftKind::Spring, WeftKind::Thicket, WeftKind::Erratic] {
        let offers = weft_offers(kind);
        assert_eq!(
            offers,
            [OfferedVerb::Examine].into_iter().collect(),
            "{kind:?} must offer nothing but the universal Examine -- only \
             the overhang carries a component bundle: {offers:?}"
        );
    }
}

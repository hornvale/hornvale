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
    let living_occupations = std::collections::BTreeMap::new();

    let brief = brief_of(
        &living_occupations,
        &occupations,
        geo,
        ctx.nearest_index(),
        &place,
        &terrain,
        walk,
        world.seed,
        &ctx.strange_sites(),
        &ctx.terrain().cave_site_vertices(),
    )
    .expect("the ruin facet has a valid production brief");

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
///
/// **Carries a `LocaleContext` as of fix round 1 (F5).** Not for the weft
/// derivation itself (`pack`/`world.seed` are still what
/// `all_features_at_cached` reads) — for [`Self::is_afloat`], which needs
/// [`hornvale_locale::LocaleContext::water_column_at`] to reproduce
/// `Session::column_here`'s own predicate independently.
struct Oracle {
    world: hornvale_kernel::World,
    ctx: LocaleContext,
    pack: hornvale_worldgen::FieldPack,
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
        // `build_from`, not `build`: reuses the `terrain`/`climate` pair
        // already derived above rather than re-sculpting a second copy
        // underneath it (its own doc: "byte-identical to `build` whenever
        // `terrain` equals `terrain_of(world)`", which is exactly the case
        // here).
        let ctx = LocaleContext::build_from(&world, &terrain, &climate);
        Self { world, ctx, pack }
    }

    /// The features [`hornvale_worldgen::all_features_at_cached`] derives at
    /// `session`'s own `position()` — `cache: None`, so this is a direct
    /// derivation and shares no state with the session's own private
    /// `weft_window`, which is the point: an independent oracle, not a
    /// second read of the same cache.
    fn features_at(&self, session: &Session<'_>) -> Vec<hornvale_worldgen::WeftFeature> {
        hornvale_worldgen::all_features_at_cached(
            &session.position(),
            self.ctx.climate().geosphere(),
            self.ctx.nearest_index(),
            &self.pack,
            self.world.seed,
            None,
        )
    }

    /// Whether `facet` renders AFLOAT — an independent reproduction of
    /// `Session::column_here`'s own predicate (fix round 1, F5): the SAME
    /// `corner_weights` array `weft::prevalence` reads, reduced to its
    /// single max-weight corner (`max_by_key`, matching
    /// `v.locale.corners.iter().max_by_key(|c| c.weight)`'s tie-break
    /// exactly, since `Locale::corners` is built from this identical array
    /// in this identical order — `windows/locale/src/lib.rs`'s own
    /// `corners: weights.iter().map(...)` construction site), then a
    /// `water_column_at` check on that one vertex. `false` (never afloat)
    /// for a facet shallower than the grid, matching `column_here`'s own
    /// `Ok(v) ... else return Vec::new()` fallback.
    fn is_afloat(&self, facet: &hornvale_kernel::Facet) -> bool {
        let geo = self.ctx.climate().geosphere();
        let Some(weights) = facet.corner_weights(geo, self.ctx.nearest_index()) else {
            return false;
        };
        let dominant = weights
            .iter()
            .max_by_key(|(_, w)| *w)
            .expect("corner_weights always returns four entries")
            .0;
        !self.ctx.water_column_at(dominant).is_empty()
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

    // **Also requires `!is_afloat` (fix round 1, F5).** Without this, the
    // walk could stop on one of the 28 measured coastal facets where
    // eligibility and vantage disagree (see `Session::describe_here`'s own
    // `weft_clause` doc) — a facet the independent oracle calls occupied but
    // the FIXED render correctly renders silent. This test's own claim is
    // about the ordinary case (a clean, on-land occurrence), so it walks
    // past a conflict facet rather than tripping on one; the conflict case
    // itself is `afloat_facets_never_render_a_weft_clause` below.
    let mut found = oracle.features_at(&s);
    let mut afloat = oracle.is_afloat(&s.position());
    let mut steps = 0usize;
    while (found.is_empty() || afloat) && steps < WALK_BUDGET {
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
        afloat = oracle.is_afloat(&s.position());
    }
    assert!(
        !found.is_empty() && !afloat,
        "the walk never reached a genuinely on-land facet the independent \
         oracle says carries a derived feature, in {WALK_BUDGET} steps — \
         either the walk is stuck (every bearing refused at every step) or \
         Task 7's measured density (spring 0.984%, overhang 2.058%, thicket \
         3.703%, erratic 1.045% of 40,962 facets) has regressed"
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

/// Fix round 1 (reviewer IMPORTANT, F5). `Session::describe_here`'s vantage
/// predicate (`Self::column_here`'s single max-weight-corner pick) and weft
/// eligibility (`land_eligible`'s four-corner bilinear blend, `>= 0.5`) are
/// two DIFFERENT tests over the same four corner weights, and they can
/// disagree at a coastal facet: the corner-pick sees ocean, the blend still
/// reads majority-land. Measured directly on seed 42, every walk-depth
/// facet over all 40,962 vertices: 29,713 facets are afloat by the
/// corner-pick test, and of those, exactly 28 (0.094% of afloat facets,
/// 0.068% of all facets) also carry >= 1 weft feature by the blend test —
/// real, not merely constructible, though rare.
///
/// **The Warp, Task 6 (2026-09-05): 35 -> 27 -> 30 -> 28.** The population is the same
/// one and the predicates are untouched; what moved is how often a weft
/// feature occurs at all on the coastal band. Spring and overhang read their
/// cause through a soft step with a zero floor now (spec §6.1, §6.2), so
/// neither occurs on ground with no cause — and a facet the corner-pick
/// calls afloat is, by construction, ground where the two sign kinds' causes
/// (karst-and-drainage, induration-and-slope) are weak. Thicket and erratic
/// keep the Weft's own expression bit for bit, so the 28 that remain are
/// theirs plus whatever sign features survive.
///
/// The three numbers in the middle are not typos, and the sequence says more
/// than its endpoint does. `27` is the reading at `OVERHANG_RATE = 0.16`,
/// which stood for one commit to satisfy an H2 between-kind clause withdrawn
/// from spec §7's gate the same day (ledger #11); `30` is the reading at
/// `0.75`, the highest rung holding overhang's own bands; `28` is the final
/// `0.50`. Four counts, one moving constant, neither predicate touched. Note
/// that it is NOT monotone in the rate — a reminder that this counts a
/// COINCIDENCE (afloat by one test, occupied by the other) rather than a
/// quantity, so it moves with which particular coastal facets happen to draw
/// an occurrence, not with how many occurrences there are.
///
/// This is the population `Session::describe_here`'s `if vantage.is_none()`
/// gate exists to protect: at every one of these 28 facets, the render must
/// suppress the weft clause. Reproduced against the free functions directly
/// (`Oracle::is_afloat` mirrors `Session::column_here`'s own predicate
/// exactly — see its own doc) rather than through a live `Session`, because
/// steering a live walk onto 28 specific facets scattered across 40,962
/// would need real geodesic pathfinding this file does not otherwise build;
/// `the_walk_reports_a_derived_feature_when_it_finds_one` above is the live
/// end-to-end witness for the (overwhelmingly more common) clean case.
///
/// The count is asserted EXACTLY, not as a floor: a population this test
/// measures shrinking to zero would make it vacuous, and growing would be
/// worth knowing about (a different eligibility/vantage relationship than
/// the one this doc describes).
#[test]
fn afloat_facets_never_render_a_weft_clause() {
    let oracle = Oracle::build();
    let geo = oracle.ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&oracle.ctx);

    let mut conflicts: Vec<hornvale_kernel::Facet> = Vec::new();
    for i in 0..geo.vertex_count() {
        let v = hornvale_kernel::Vertex(i as u32);
        let facet = hornvale_kernel::Facet::containing(geo.position(v), walk);
        if !oracle.is_afloat(&facet) {
            continue;
        }
        let features = hornvale_worldgen::all_features_at_cached(
            &facet,
            geo,
            oracle.ctx.nearest_index(),
            &oracle.pack,
            oracle.world.seed,
            None,
        );
        if !features.is_empty() {
            conflicts.push(facet);
        }
    }

    assert_eq!(
        conflicts.len(),
        28,
        "the vantage/eligibility disagreement moved from 28 facets to {} -- \
         update this count (and Session::describe_here's weft_clause gate \
         doc, which cites it) in the same commit as whatever changed the \
         underlying predicates",
        conflicts.len()
    );

    // What this test does NOT independently prove, said plainly: that
    // `Session::describe_here`'s `if vantage.is_none() { .. } else {
    // String::new() }` conditional actually fires at these 28 facets when a
    // live session stands on one. `describe_here` is a private method
    // reachable only through `Session::handle`, and reaching one specific
    // facet out of 28 scattered across 40,962 needs real pathfinding this
    // file does not build (see the doc above). What IS pinned: the
    // population the gate exists to protect is real and its size (28), so a
    // silent change to either predicate (`column_here`'s corner pick,
    // `land_eligible`'s blend) that grows or shrinks it reddens here rather
    // than going unnoticed.
}

/// Carried from Task 7: the overhang's affordance proves the query path end
/// to end. `weft_offers` routes a `WeftKind` through the SAME `offered()`
/// subset query real placed things use through `offered_by`, so this is
/// evidence the query machinery answers correctly for a kind that was never
/// a `KindId` — not merely a restatement of `weft_object_registry`'s own
/// table.
#[test]
fn the_overhang_affords_shelter_and_warmth() {
    use hornvale_vessel::affordance::{OfferedVerb, weft_offers, weft_offers_for};
    use hornvale_worldgen::WeftKind;

    // **Driven from a REALIZED occurrence (fix round 1, F6).** Spec §5.6
    // assigns the overhang the job of proving "the affordance path end to
    // end"; running the query from a bare `WeftKind` (as this test did
    // before) proves the query machinery answers correctly for a kind, but
    // never actually reaches a `WeftFeature` a derivation produced. Found by
    // direct scan (the same "every walk-depth facet over the geosphere's own
    // vertices" construction site Task 7's own full-grid table used) rather
    // than a live session walk — the claim here is about the query's answer
    // for a real occurrence, not that a possession can physically stand on
    // one, which `the_walk_reports_a_derived_feature_when_it_finds_one`
    // above already covers for the general (kind-agnostic) case.
    let oracle = Oracle::build();
    let geo = oracle.ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&oracle.ctx);
    let realized = (0..geo.vertex_count())
        .find_map(|i| {
            let facet = hornvale_kernel::Facet::containing(
                geo.position(hornvale_kernel::Vertex(i as u32)),
                walk,
            );
            hornvale_worldgen::features_at_cached(
                WeftKind::Overhang,
                &facet,
                geo,
                oracle.ctx.nearest_index(),
                &oracle.pack,
                oracle.world.seed,
                None,
            )
            .into_iter()
            .next()
        })
        .expect(
            "seed 42 must realize at least one Overhang occurrence -- Task 7's own \
             full-grid table measured 843 of 40,962, so this scan finding none would \
             mean that density has regressed to zero",
        );
    assert_eq!(
        realized.kind,
        WeftKind::Overhang,
        "the found feature must be the kind actually searched for"
    );

    let overhang = weft_offers_for(&realized);
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

/// Fix round 1 (reviewer IMPORTANT, F4): `weft_object_registry`'s own doc
/// claims its key space (`WeftKind`) is disjoint from `object_registry`'s
/// (`hornvale_thing::THING_KINDS`) "by construction" — a comment, not a
/// guarantee, until this test. Every `WeftKind`'s bare label (the primary
/// noun before the "/" in each variant's own doc, and the root segment of
/// its `derived/<name>/v1` stream label — `windows/worldgen/src/
/// streams.rs`'s `WEFT_SPRING`/`WEFT_OVERHANG`/`WEFT_THICKET`/
/// `WEFT_ERRATIC`) must not appear as a `THING_KINDS` row. If it ever did,
/// the two registries would represent two DIFFERENT concepts under one
/// string, which is exactly the confusion keeping the tables separate (spec
/// §5.6's `object_registry` route was closed by G-e, `windows/vessel/tests/
/// suite/kind_totality.rs`) was meant to avoid.
#[test]
fn no_weft_kind_label_appears_in_thing_kinds() {
    const WEFT_KIND_LABELS: [&str; 4] = ["spring", "overhang", "thicket", "erratic"];
    for label in WEFT_KIND_LABELS {
        assert!(
            !hornvale_thing::THING_KINDS.contains(&label),
            "{label:?} is both a WeftKind label and a THING_KINDS row -- the \
             two registries are no longer disjoint by construction, and \
             weft_object_registry must not be merged into object_registry \
             regardless (spec §5.6's amendment; see the ledger entry on the \
             registry split)"
        );
    }
}

/// Fix round 1 (reviewer IMPORTANT, F2): `windows/worldgen/tests/suite/
/// weft_ledger_guard.rs`'s `the_derived_surface_commits_no_facts` reads
/// `seed_42_world()` OFF DISK — no weft code runs in that test at all, so it
/// can only fail if someone rebaselines the golden, never because a derived
/// feature actually reached the ledger. It is a valid byte-golden backstop
/// wearing the wrong name (see its own doc, corrected in this fix round).
///
/// **This is the guard that CAN fail.** It runs the weft surface for real —
/// a live session walking a real, dense stretch of seed 42 (every step
/// prefills `Session::go`'s `weft_window` and every `look` reads
/// `describe_here`'s weft clause through it, at facets Task 7 measured
/// carry a feature roughly 7-8% of the time combined) — and asserts the
/// session's own committed ledger grew by EXACTLY one fact per successful
/// step (`Session::commit_agent_at`, decision 0069's `agent-at` trail) and
/// nothing else. A future change that wired any weft derivation into
/// `Ledger::commit` would move this count and this test would catch it,
/// which is exactly what the static fixture guard structurally cannot do —
/// `prevalence`/`occurs`/`WeftWindow`/`all_features_at_cached` take no
/// `Ledger`/`World` parameter anywhere in their signatures, so nothing here
/// proves that by inspection either; it proves it by RUNNING the surface and
/// counting.
#[test]
fn walking_through_dense_weft_facets_commits_only_agent_at_facts() {
    const STEPS: usize = 300;

    let world = hornvale_worldgen::seed_42_world();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");

    let before = s.committed_fact_count();
    let mut successful_steps = 0usize;
    for word in COMPASS_WORDS.iter().cycle().take(STEPS) {
        let pos_before = s.position();
        match s.handle(&format!("go {word}")) {
            Turn::Out(_) => {}
            Turn::Released(t) => panic!("go must not release the session: {t}"),
        }
        if s.position() != pos_before {
            successful_steps += 1;
        }
        // `go` already renders through `describe_here` once; a second,
        // explicit `look` here reads the weft clause AGAIN at the same
        // facet, exercising the window's cache-HIT path (the facet `go`
        // just prefilled) on top of the miss `go` itself paid.
        match s.handle("look") {
            Turn::Out(_) => {}
            Turn::Released(t) => panic!("look must not release the session: {t}"),
        }
    }
    let after = s.committed_fact_count();

    assert!(
        successful_steps > 0,
        "fixture check: at least one step must succeed, or this test exercises no weft \
         reads at all"
    );
    assert_eq!(
        after - before,
        successful_steps,
        "walking {successful_steps} successful steps (reading the weft window and the weft \
         clause twice per facet) must commit exactly one agent-at fact per step and nothing \
         else -- {} facts committed instead ({before} before, {after} after)",
        after - before
    );
}

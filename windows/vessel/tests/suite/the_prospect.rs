//! H1: the rename changes nothing enterable — and, since Task 5, that a site
//! has an ADDRESS rather than a 110 km neighbourhood.

use hornvale_kernel::{Facet, Geosphere, Seed, Vertex};
use hornvale_locale::{EnergySource, Kingdom, LocaleContext, StrangeSite};
use hornvale_vessel::liveness::LocaleTerrain;
use hornvale_vessel::site::{Site, SiteKind};
use hornvale_vessel::structure::structure_at;
use hornvale_vessel::{PossessOpts, Session, Turn, brief_of};
use hornvale_worldgen::{SiteReason, site_facet_for};

/// The seed-42 flagship is enterable before and after the gate swap. This is
/// the whole of H1 at this task: a real world, the real verb, the real answer.
///
/// Asserts a POSITIVE signal — the chamber's own `Ways on:` signature — rather
/// than the absence of the old refusal string: the old wording made the
/// negative assertion trivially true, so it could never have caught a broken
/// gate. This one goes red if `enter` stops descending at all.
#[test]
fn the_flagship_is_still_enterable_after_the_gate_swap() {
    let world = hornvale_worldgen::seed_42_world();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    let reply = match s.handle("enter") {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("enter must not release the session: {t}"),
    };
    assert!(
        reply.contains("Ways on"),
        "the flagship must stay enterable across the gate swap: {reply}"
    );
}

/// A site's facet is stable for a seed and distinct between neighbouring
/// vertices — the two properties that make it an ADDRESS rather than a
/// re-rolled guess.
///
/// **The signature is not the plan's.** The plan wrote
/// `site_facet_for(vertex: u32, seed: Seed, walk_depth: u32)`, which cannot be
/// implemented: a vertex index alone says nothing about where on the sphere the
/// vertex is, and every route from a vertex to a facet goes through
/// `Geosphere::position`. It also carries a `reason`, because caves and exotic
/// sites share this mechanism (ledger ruling #13) and a shared key would place
/// both at the identical facet wherever one vertex warrants both.
#[test]
fn a_sites_facet_is_stable_and_vertex_distinct() {
    let geo = Geosphere::new(6);
    let seed = Seed(42);
    let a1 = site_facet_for(Vertex(1953), SiteReason::Exotic, seed, &geo, 13);
    let a2 = site_facet_for(Vertex(1953), SiteReason::Exotic, seed, &geo, 13);
    let b = site_facet_for(Vertex(1954), SiteReason::Exotic, seed, &geo, 13);
    assert_eq!(
        a1, a2,
        "the same vertex must always place at the same facet"
    );
    assert_ne!(a1, b, "neighbouring vertices must not share a facet");
}

/// The point of the whole task: a placed exotic site is at ONE facet, and the
/// facet beside it holds nothing.
///
/// This is what separates an address from the defect
/// `CLIM-water-label-resolution-vs-walk-band` records — a per-vertex verdict
/// read at the walk band answers "yes" for every facet within ~55 km, which is
/// the same sentence as "no site anywhere". The neighbour assertion is the half
/// that would catch a regression to that shape; the positive assertion alone
/// would pass under it.
#[test]
fn an_exotic_site_stands_at_one_facet_and_not_at_its_neighbour() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let sites = ctx.strange_sites();
    assert!(
        !sites.is_empty(),
        "seed 42 must place at least one exotic site"
    );
    let geo = ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&ctx);
    let terrain = LocaleTerrain::new(&ctx);
    // The real roster, not an empty one: an empty slice would make the
    // neighbour assertion below pass for a reason this test is not about.
    let caves = ctx.terrain().cave_site_vertices();
    let placed = site_facet_for(
        Vertex(sites[0].vertex),
        SiteReason::Exotic,
        world.seed,
        geo,
        walk,
    );

    // These probes isolate placed-site identity; culture is intentionally
    // absent from both sides of each comparison.
    let occupations = std::collections::BTreeMap::new();
    let occupation_history = std::collections::BTreeMap::new();
    let here = brief_of(
        &occupations,
        &occupation_history,
        geo,
        ctx.nearest_index(),
        &placed,
        &terrain,
        walk,
        world.seed,
        &sites,
        &caves,
    )
    .expect("the placed exotic facet has a valid production brief");
    assert_eq!(
        // `site`, not `s`: `cli/tests/suite/claim_shape.rs` reads a closure
        // parameter named `s` as a seed binding and demands a `claim:` tag on
        // what it takes for a seed sweep. This test builds ONE world, so the
        // tag would be a false declaration; the honest fix is the name, which
        // is more accurate anyway.
        here.site.as_ref().map(|site| site.kind),
        Some(SiteKind::Exotic),
        "the placed facet must carry the site"
    );

    let next: Facet = placed.neighbors()[0].clone();
    let there = brief_of(
        &occupations,
        &occupation_history,
        geo,
        ctx.nearest_index(),
        &next,
        &terrain,
        walk,
        world.seed,
        &sites,
        &caves,
    )
    .expect("the neighbouring exotic facet has a valid production brief");
    assert_eq!(
        there.site, None,
        "the facet beside a site must hold nothing — a site is an address, \
         not a neighbourhood"
    );
}

/// **Task 4's production wiring, end to end.** A facet a cave was placed at
/// carries `SiteKind::Cave` in its brief, and `structure_at` — the enterability
/// gate Decision 0666 moved onto `site` — returns a structure there.
///
/// Both halves are load-bearing and the first alone would not have caught the
/// defect that matters. `brief_of` could set the site correctly while nothing
/// downstream read it, and the campaign's H1 is scoped around exactly this
/// number changing; asserting the brief without asserting the gate would let a
/// cave be "a site" that no walker can ever open.
///
/// The neighbour assertion is the resolution guard, the same one the exotic
/// test carries: a cave read at the nearest VERTEX rather than the placed facet
/// would answer yes for every facet within ~55 km.
///
/// Existence comes from `GeneratedTerrain::cave_at`, the world's one cave
/// predicate — the same answer the terrain map's cave glyph draws, so a walker
/// cannot enter a cave the map denies.
#[test]
fn a_placed_cave_is_a_site_and_is_enterable() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let geo = ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&ctx);
    let terrain = LocaleTerrain::new(&ctx);
    let caves = ctx.terrain().cave_site_vertices();
    assert!(!caves.is_empty(), "seed 42 must hold at least one cave");
    let sites = ctx.strange_sites();
    let placed = site_facet_for(caves[0], SiteReason::Cave, world.seed, geo, walk);

    // This probe isolates placed-site identity; culture is intentionally
    // absent from both sides of the comparison.
    let occupations = std::collections::BTreeMap::new();
    let occupation_history = std::collections::BTreeMap::new();
    let here = brief_of(
        &occupations,
        &occupation_history,
        geo,
        ctx.nearest_index(),
        &placed,
        &terrain,
        walk,
        world.seed,
        &sites,
        &caves,
    )
    .expect("the placed cave facet has a valid production brief");
    assert_eq!(
        here.site.as_ref().map(|site| site.kind),
        Some(SiteKind::Cave),
        "the placed facet must carry the cave"
    );
    assert!(
        !here.built,
        "this fixture wants an UNBUILT cave facet, so that the settlement arm \
         of the salience order is not what is being read"
    );
    assert!(
        structure_at(&placed, &here, world.seed, walk).is_some(),
        "a cave is enterable — Decision 0666 hangs the gate on the site"
    );

    let next: Facet = placed.neighbors()[0].clone();
    let there = brief_of(
        &occupations,
        &occupation_history,
        geo,
        ctx.nearest_index(),
        &next,
        &terrain,
        walk,
        world.seed,
        &sites,
        &caves,
    )
    .expect("the neighbouring cave facet has a valid production brief");
    assert_eq!(
        there.site, None,
        "the facet beside a cave must hold nothing — a cave mouth is an \
         address, not a 110 km neighbourhood"
    );
}

/// Where a facet holds more than one candidate site, `Site::salience`
/// decides which one `brief_of` reports (Ruling 29). **This is stated as
/// two things now, not one, and conflating them is the gap the combined
/// Task 6+7 review found**: `brief_of`'s own call site consults exactly one
/// AUTHORITY (`Site::salience`, `site.rs`) — the if/else chain that used to
/// restate the order independently is gone — but a second, unconsumed
/// STATEMENT of the identical order survives in `SiteKind`'s own
/// `derive(Ord)` (`Cave < Exotic < Settlement`), and nothing enforces that
/// the two agree if either is edited. Two statements, one authority.
///
/// The assertion below is written to catch a regression to a hardcoded
/// order that happens to AGREE with `Site::salience` on the two kinds this
/// fixture collides (settlement, cave) while silently disagreeing on the
/// exotic rung this fixture cannot exercise — comparing against a literal
/// `SiteKind::Settlement` cannot tell "consults `Site::salience`" apart from
/// "hardcodes settlement first" for that reason. So the expected winner is
/// computed here by calling `Site::salience` on the same two candidates
/// `brief_of` itself builds, and compared to `brief_of`'s actual answer —
/// pinning the DELEGATION, not merely this one outcome. It is still not
/// sufficient alone: see
/// `salience_decides_the_winner_at_an_exotic_cave_collision` below for the
/// fixture that actually reds under a settlement/exotic/cave hardcode that
/// disagrees with `Site::salience` only on the exotic rung.
///
/// The collision is REAL, not asserted from the type system: the same
/// facet a real seed-42 cave is placed at
/// (`a_placed_cave_is_a_site_and_is_enterable`'s own fixture) is
/// additionally forced BUILT through `LocaleTerrain`'s injected
/// settlement-territory set, so this facet genuinely carries two
/// candidates — a settlement and a cave — and the assertion is which one
/// `brief_of` reports.
#[test]
fn salience_decides_the_winner_when_a_facet_holds_two_sites() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let geo = ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&ctx);
    let caves = ctx.terrain().cave_site_vertices();
    assert!(!caves.is_empty(), "seed 42 must hold at least one cave");
    let sites = ctx.strange_sites();
    let placed = site_facet_for(caves[0], SiteReason::Cave, world.seed, geo, walk);

    // Force the SAME facet a real cave sits at to also read built, so it
    // genuinely holds two candidates rather than one asserted by fiat. The
    // injected map is `built_rooms`' own shape since Task 7 — room to the
    // settlement's name — so this fixture also names the forced settlement.
    let mut built_set = std::collections::BTreeMap::new();
    built_set.insert(
        placed.pack().expect("a walk-band facet packs"),
        "Testhollow".to_string(),
    );
    let terrain = LocaleTerrain::with_fields(&ctx, None, None, None, Some(&built_set), None);

    // This probe isolates site salience; culture is intentionally absent.
    let occupations = std::collections::BTreeMap::new();
    let occupation_history = std::collections::BTreeMap::new();
    let here = brief_of(
        &occupations,
        &occupation_history,
        geo,
        ctx.nearest_index(),
        &placed,
        &terrain,
        walk,
        world.seed,
        &sites,
        &caves,
    )
    .expect("the settlement-cave collision has a valid production brief");
    assert!(
        here.built,
        "fixture check: the forced facet must itself read built"
    );
    // Pin the DELEGATION: the expected winner is computed by calling the
    // real `Site::salience` on the same two candidates `brief_of` itself
    // assembles at this facet, not asserted as a literal `SiteKind`.
    let candidates = [
        Site::placed(SiteKind::Settlement, Some("Testhollow".to_string())),
        Site::placed(SiteKind::Cave, None),
    ];
    let expected = candidates.into_iter().max_by_key(Site::salience);
    assert_eq!(
        here.site, expected,
        "settlement salience (3) must beat cave salience (1) at a genuine \
         two-site collision: {:?}",
        here.site
    );
}

/// The gap `salience_decides_the_winner_when_a_facet_holds_two_sites` cannot
/// close on its own: that fixture collides settlement against cave, and a
/// hardcoded order agreeing with `Site::salience` on exactly those two kinds
/// while disagreeing on the untested exotic rung — e.g.
/// `Settlement => 3, Exotic => 0, Cave => 1` against the real
/// `Settlement => 3, Exotic => 2, Cave => 1` — produces the SAME winner
/// there (settlement dominates under both orderings) and so slips through
/// undetected. A settlement/exotic collision does not close the gap either:
/// settlement dominates exotic under both the real and that hardcoded
/// order too. Only an exotic/cave collision flips winners between the two
/// orderings (real: exotic beats cave; that hardcode: cave beats exotic),
/// so this fixture excludes settlement entirely and pits exotic against
/// cave alone.
///
/// The collision is constructed, not found by luck: `site_facet_for` fixes
/// the digits down to the placement quad from the vertex's own position and
/// randomizes only the tail below it (`PLACEMENT_DEPTH_BELOW_GRID`,
/// `windows/worldgen/src/placement.rs`) per `(seed, vertex, reason)`. Using
/// the SAME vertex under both `SiteReason::Exotic` and `SiteReason::Cave`
/// makes the two placements share every digit down to the quad already, so
/// they collide at the same facet whenever their two independent random
/// tails happen to agree — which a scan of the whole level-6 grid (40,962
/// vertices) finds many times over. Terrain is `LocaleTerrain::new`, which
/// injects no settlement-territory map at all, so this facet reads unbuilt
/// regardless of which vertex is chosen — the settlement rung of the order
/// plays no part here.
#[test]
fn salience_decides_the_winner_at_an_exotic_cave_collision() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let geo = ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&ctx);

    let (vertex, placed) = (0..geo.vertex_count() as u32)
        .find_map(|v| {
            let exotic_facet = site_facet_for(Vertex(v), SiteReason::Exotic, world.seed, geo, walk);
            let cave_facet = site_facet_for(Vertex(v), SiteReason::Cave, world.seed, geo, walk);
            (exotic_facet == cave_facet).then_some((v, exotic_facet))
        })
        .expect(
            "some vertex on seed 42's grid must place an exotic site and a \
             cave at the same facet",
        );

    let exotic_sites = [StrangeSite {
        vertex,
        energy: EnergySource::Sunlit,
        kingdom: Kingdom::PlantAnimal,
        endemic: false,
    }];
    let caves = [Vertex(vertex)];
    let terrain = LocaleTerrain::new(&ctx);

    // This probe isolates site salience; culture is intentionally absent.
    let occupations = std::collections::BTreeMap::new();
    let occupation_history = std::collections::BTreeMap::new();
    let here = brief_of(
        &occupations,
        &occupation_history,
        geo,
        ctx.nearest_index(),
        &placed,
        &terrain,
        walk,
        world.seed,
        &exotic_sites,
        &caves,
    )
    .expect("the exotic-cave collision has a valid production brief");
    assert!(
        !here.built,
        "fixture check: this collision must not also be a settlement, or \
         it would not isolate the exotic/cave rungs: {:?}",
        here.site
    );

    // Pin the DELEGATION, exactly as the settlement/cave fixture above
    // does, but over the ONE pair that actually distinguishes "consults
    // `Site::salience`" from the hardcode this finding names.
    let candidates = [
        Site::placed(SiteKind::Exotic, None),
        Site::placed(SiteKind::Cave, None),
    ];
    let expected = candidates.into_iter().max_by_key(Site::salience);
    assert_eq!(
        here.site, expected,
        "exotic salience (2) must beat cave salience (1) at a genuine \
         exotic/cave collision: {:?}",
        here.site
    );
}

/// Task 7: what you enter has a name, and the name is the PLACE's.
///
/// **A test at the default flagship could not tell those two apart**, which
/// is why this one stages a cast. A default possession starts in its OWN
/// village, so `Vantage::village` (the possessed body's home, via
/// `liveness::village_or_fallback`) and the settlement standing at the facet
/// are the same word — "Doaba" — and an implementation that resolved the
/// site's name from the creature would pass. It would then announce the
/// player's home village at every settlement they ever walked into: a
/// one-turn observable falsehood.
///
/// A staged cast is the divergence. `Tableau` bodies go through
/// `derive_staged_npcs`, which is `derive_wild_npcs`' shape — village-LESS —
/// so `village_or_fallback` reads "the wilds", while the cast is stood at
/// `settlement_room(village)`, the flagship settlement's own facet. So here
/// the possession says one thing and the place says another, and the test can
/// see which one the chamber names. The divergence is ASSERTED LIVE below,
/// not assumed: the walk-band line must actually say "the wilds" for the
/// chamber assertions to mean anything.
#[test]
fn entering_a_named_site_names_the_place_and_not_the_possession() {
    let world = crate::common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(hornvale_vessel::Tableau::new().with_cast(["drow"])),
        ..PossessOpts::default()
    };
    let (mut s, _) = Session::start(&world, &opts).expect("a staged session starts");

    let outdoors = match s.handle("look") {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("look must not release the session: {t}"),
    };
    // FIXTURE CHECK, and the load-bearing half of this test: the staged body
    // has no village, so the walk band names "the wilds" where a derived
    // possession would have named Doaba. Without this the two assertions
    // below could both hold in a world where the names simply agree.
    assert!(
        outdoors.contains("in the lands of the wilds"),
        "fixture check: a staged cast must be village-less, so the \
         possession-derived clause says \"the wilds\" and diverges from the \
         settlement at this facet: {outdoors}"
    );
    assert!(
        outdoors.contains("You can enter the settlement"),
        "fixture check: the staged cast must be standing in the flagship \
         settlement's own room, so there is a named site here at all: \
         {outdoors}"
    );

    let inside = match s.handle("enter") {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("enter must not release the session: {t}"),
    };
    assert!(
        inside.contains("Doaba"),
        "entering the settlement must name the SETTLEMENT: {inside}"
    );
    assert!(
        !inside.contains("the wilds"),
        "the chamber must not name the possessed creature's own home — that \
         is `village_or_fallback`'s answer, and it is the wrong one for a \
         place: {inside}"
    );
}

/// A site whose kind has no name keeps `None`, and its prose keeps the
/// sentence it always had — no borrowed name, and no empty clause.
///
/// The cave is the case that matters: it is enterable (Task 4) and it reaches
/// the same `describe_chamber` branch the settlement does, so a name derived
/// from anything but the site itself would surface here.
#[test]
fn a_cave_site_carries_no_name() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let geo = ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&ctx);
    let caves = ctx.terrain().cave_site_vertices();
    assert!(!caves.is_empty(), "seed 42 must hold at least one cave");
    let sites = ctx.strange_sites();
    let placed = site_facet_for(caves[0], SiteReason::Cave, world.seed, geo, walk);
    let terrain = LocaleTerrain::new(&ctx);

    // This probe isolates placed-site identity; culture is intentionally
    // absent.
    let occupations = std::collections::BTreeMap::new();
    let occupation_history = std::collections::BTreeMap::new();
    let here = brief_of(
        &occupations,
        &occupation_history,
        geo,
        ctx.nearest_index(),
        &placed,
        &terrain,
        walk,
        world.seed,
        &sites,
        &caves,
    )
    .expect("the named cave facet has a valid production brief");
    assert_eq!(
        here.site.as_ref().map(|site| site.kind),
        Some(SiteKind::Cave),
        "fixture check: this facet must hold the cave, or the name assertion \
         below is vacuous: {:?}",
        here.site
    );
    assert_eq!(
        here.site.as_ref().and_then(|site| site.name.as_deref()),
        None,
        "a cave has no name and must not be given one: {:?}",
        here.site
    );
}

/// The name `brief_of` attaches to a settlement site is the one keyed to the
/// ROOM in the injected settlement-territory map — the same entry
/// `Terrain::is_built` reads — and not any other name in the world.
///
/// "Nornholm" occurs nowhere in seed 42, so a `Some("Nornholm")` here can
/// only have come through the map this test injected. That is what makes the
/// assertion discriminating rather than a restatement of the fixture: a name
/// resolved from a body, a settlement roster, or the locale would produce
/// some other word (or none).
#[test]
fn a_settlement_sites_name_is_keyed_to_the_room() {
    let world = crate::common::build(42).expect("seed 42 builds");
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let geo = ctx.climate().geosphere();
    let walk = hornvale_locale::walk_depth(&ctx);
    let caves = ctx.terrain().cave_site_vertices();
    let sites = ctx.strange_sites();
    // A facet with no site of its own, forced built and named through the map.
    let plain =
        site_facet_for(caves[0], SiteReason::Cave, world.seed, geo, walk).neighbors()[0].clone();
    let mut rooms = std::collections::BTreeMap::new();
    rooms.insert(
        plain.pack().expect("a walk-band facet packs"),
        "Nornholm".to_string(),
    );
    let terrain = LocaleTerrain::with_fields(&ctx, None, None, None, Some(&rooms), None);

    // This probe supplies built/name state explicitly; culture is outside its
    // assertion and remains absent.
    let occupations = std::collections::BTreeMap::new();
    let occupation_history = std::collections::BTreeMap::new();
    let here = brief_of(
        &occupations,
        &occupation_history,
        geo,
        ctx.nearest_index(),
        &plain,
        &terrain,
        walk,
        world.seed,
        &sites,
        &caves,
    )
    .expect("the named settlement facet has a valid production brief");
    assert_eq!(
        here.site.as_ref().and_then(|site| site.name.as_deref()),
        Some("Nornholm"),
        "the site's name must be the one keyed to this room: {:?}",
        here.site
    );
}

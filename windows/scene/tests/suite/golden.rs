//! The scene/tiles/v1 byte pin: this fixture changing is the epoch
//! decision point (scene-protocol spec §2). Regenerate deliberately, never
//! casually: `REBASELINE=1 cargo test -p hornvale-scene --test suite -- golden`
//! (or `make rebaseline-goldens`) rewrites it, then review the diff as a
//! contract change.

use hornvale_scene::{
    TileFields, region_json, render_surrounds_ascii, scene_json, scene_json_selected,
    surrounds_json, surrounds_scene, tiles_region_scene, tiles_scene,
};

// Integration tests can't see #[cfg(test)] helpers, and the public API
// takes no test scaffolding — this 10-line duplicate of the lib tests'
// builder is the cheaper price.
fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(1),
        &Default::default(),
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 1 builds")
}

fn seed_1_json() -> String {
    scene_json(&tiles_scene(&world(), 16).unwrap())
}

#[test]
fn v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/tiles-seed-1-w16.json"
        )),
        &seed_1_json(),
        "scene/tiles/v1 bytes moved — this is the epoch decision point (scene-protocol \
         spec §2); accept deliberately and review the diff as a contract change",
    );
}

// The projection's own pin (The Winnowing; scene-protocol spec §3 item 4).
// The two lib tests either side of it check the projection at `all()` and
// check that an EMITTED field's bytes are unchanged — neither can observe an
// OVER-emission, so with only those a `TileFields::contains` that always
// returned `true` passed the whole suite while defeating the feature. What
// needs pinning is the composed shape: braces, commas, and key order with
// holes in it. A golden does that as bytes rather than as reasoning.
//
// The five selected layers are chosen to put a hole everywhere a hole can go:
// `plate`/`unrest` dropped between the metadata `biome_legend` and `features`,
// the four temperature/current layers dropped before `season_period_days`,
// `moisture` dropped between `circulation_bands` and `locked`, and `drainage`
// dropped between `water_legend` and the trailing `waterfalls`. So the fixture
// covers a hole at the head, in the middle, adjacent to metadata on both
// sides, and at the tail. Seed 1 at width 16 is 128 tiles — gate-cheap.
const PINNED_FIELDS: &[&str] = &["elevation_m", "ocean", "biome", "t_mean_c", "water"];

fn projected_seed_1_json() -> String {
    let scene = tiles_scene(&world(), 16).unwrap();
    let fields = TileFields::only(PINNED_FIELDS).expect("the pinned names are all known layers");
    scene_json_selected(&scene, &fields)
}

#[test]
fn projected_v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/tiles-seed-1-w16-projected.json"
        )),
        &projected_seed_1_json(),
        "the projected scene/tiles/v1 bytes moved — this pins the composed shape of a \
         partial document (key order with holes in it); if the unselected layers came \
         BACK, the projection has stopped projecting. Accept deliberately, re-run with \
         REBASELINE=1, and review the diff as a contract change",
    );
}

// The golden above would catch an over-emission, but only as "bytes moved".
// This says the property out loud and names the offending layer, so a failure
// is diagnosable without reading a fixture diff.
#[test]
fn the_projection_omits_unselected_layers() {
    let doc = projected_seed_1_json();
    for name in TileFields::ALL_NAMES {
        let key = format!("\"{name}\":");
        let selected = PINNED_FIELDS.contains(name);
        assert_eq!(
            doc.contains(&key),
            selected,
            "layer {name} is {} in the projected document but {} in the selection",
            if doc.contains(&key) {
                "present"
            } else {
                "absent"
            },
            if selected { "selected" } else { "unselected" }
        );
    }
    // Document metadata is never selectable and must survive the projection.
    for key in [
        "\"schema\":",
        "\"seed\":",
        "\"width\":",
        "\"height\":",
        "\"sea_level_m\":",
        "\"biome_legend\":",
        "\"features\":",
        "\"season_period_days\":",
        "\"locked\":",
        "\"water_legend\":",
        "\"waterfalls\":",
    ] {
        assert!(
            doc.contains(key),
            "metadata {key} was dropped by projection"
        );
    }
    // And the projection must actually save bytes — the point of the campaign.
    let full = scene_json(&tiles_scene(&world(), 16).unwrap());
    assert!(
        doc.len() < full.len(),
        "the projected document ({} bytes) is not smaller than the full one ({} bytes)",
        doc.len(),
        full.len()
    );
}

// scene/tiles-region/v1's own pin, the third sibling. Until this existed the
// region path's only in-repo byte evidence was the *mutual* equivalence test
// in the lib (`the_context_path_is_byte_identical_to_the_world_path`), which
// would pass if both paths moved together — an absolute pin is what makes a
// context refactor's byte-identity claim checkable at all. A seed-1 level-3
// patch at 8 samples is 81 nodes: small enough for the commit gate.
fn region_seed_1_json() -> String {
    region_json(&tiles_region_scene(&world(), 0, 3, 0, 0, 8).unwrap())
}

#[test]
fn region_v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/region-seed-1-f0-l3.json"
        )),
        &region_seed_1_json(),
        "scene/tiles-region/v1 bytes moved — this is the region path's absolute byte pin \
         (the projection is shared byte-for-byte with the orrery's cubeSphere.ts); accept \
         deliberately, re-run with REBASELINE=1, and review the diff as a contract change",
    );
}

// scene/surrounds/v1's own pin, sibling to the tiles pin above. Field order
// is this schema's headline contract (decision 0055: it becomes cross-repo
// once another repo parses it), and until this test existed nothing defended
// it. Radius 4 on a fixed observer guarantees both the "here" cell and
// several "sensed" cells appear, so the pinned bytes give the schema's
// unasserted contract fields (`orientation`, the three legends, and a
// `"sensed"` cell) their first coverage.
fn surrounds_seed_1_json() -> String {
    let w = world();
    // A fixed, arbitrary observer — the equator at the prime meridian — at a
    // shallow depth; this pin only needs a stable address, not a specific
    // biome or a seam. Same lat/lon -> unit-sphere conversion as the lib
    // tests' `observer` helper.
    let observer = hornvale_kernel::Facet::containing(
        hornvale_kernel::math::unit_sphere_from_lat_lon(0.0, 0.0),
        6,
    );
    surrounds_json(&surrounds_scene(&w, &observer, 4, hornvale_kernel::WorldTime::GENESIS).unwrap())
}

#[test]
fn surrounds_v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/surrounds-seed-1.json"
        )),
        &surrounds_seed_1_json(),
        "scene/surrounds/v1 bytes moved — field order is this schema's headline contract \
         (decision 0055: it becomes cross-repo once another repo parses it); accept \
         deliberately and review the diff as a contract change",
    );
}

// The seed-1 pin above sits in deep ocean: every cell is a single-word
// biome, `marks` is empty everywhere, and the legend has only two entries —
// so it gives no byte coverage to a multi-word biome index or to `Mark`'s
// field order (`noun, kind, datum, salience`), despite field order being
// this schema's headline contract. This second pin sits at seed 42's
// flagship settlement observer instead: a verified 31-cell, all-land,
// radius-4 neighbourhood carrying a settlement mark and the multi-word
// biome "tropical seasonal forest", so both gaps get real byte coverage.
fn seed_42_world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &Default::default(),
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds")
}

// Integration tests can't reach the lib's `pub(crate) place_latlon` helper,
// so this duplicates its handful of lines against the public
// `hornvale_settlement` facts directly — same idiom as `world()`/
// `surrounds_seed_1_json()` above.
fn flagship_latlon(world: &hornvale_kernel::World) -> (f64, f64) {
    let v = hornvale_settlement::village_info(world).expect("seed 42 has a village");
    let lat = match world.ledger.value_of(v.id, hornvale_settlement::LATITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => panic!("flagship settlement has no latitude fact"),
    };
    let lon = match world.ledger.value_of(v.id, hornvale_settlement::LONGITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => panic!("flagship settlement has no longitude fact"),
    };
    (lat, lon)
}

fn surrounds_seed_42_flagship_json() -> String {
    let w = seed_42_world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let depth = hornvale_locale::walk_depth(&ctx);
    let (lat, lon) = flagship_latlon(&w);
    let observer = hornvale_kernel::Facet::containing(
        hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon),
        depth,
    );
    surrounds_json(&surrounds_scene(&w, &observer, 4, hornvale_kernel::WorldTime::GENESIS).unwrap())
}

#[test]
fn surrounds_v1_land_and_mark_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/surrounds-seed-42-flagship.json"
        )),
        &surrounds_seed_42_flagship_json(),
        "scene/surrounds/v1 bytes moved (land/mark fixture) — field order is this schema's \
         headline contract (decision 0055: it becomes cross-repo once another repo parses \
         it); accept deliberately and review the diff as a contract change",
    );
}

// The two observers below (The Margin's gallery, `book/src/gallery/
// surrounds-seed-42.md`) are the two charts in the repo that reach ground the
// flagship pin does not: the flagship sits in a uniform biome, and neither
// the tiles-seed-1 nor the surrounds-seed-1 pin ever reaches a base-face
// seam. The gallery page itself is (correctly) excluded from CI's
// cross-platform drift check, since it's an ASCII rendering of the same
// libm-thresholded classifications the tiles gallery exclusion already
// covers — but that leaves these two renders pinned NOWHERE, unlike every
// other gallery chart. These platform-local byte pins restore that
// protection without reintroducing the cross-platform noise the CI exclusion
// exists to avoid, exactly as `surrounds_v1_bytes_are_pinned` above does for
// the JSON encoding.
//
// **THIS PARAGRAPH USED TO SAY "the only charts in the repo that actually
// show ocean meeting land", AND THAT HALF WAS ALREADY FALSE ON `main` BEFORE
// The Pavement TOUCHED IT.** The coastline observer's neighbourhood is open
// ocean at every room in its ball and has been for some time: `git show
// origin/main:book/src/gallery/generated/surrounds-seed-42/coastline.txt` is
// solid `~`, legend `bathypelagic`. The Pavement re-addressed both observers
// (below) and deliberately did NOT go hunting for a replacement coastline —
// re-pointing a showcase at different GROUND is a gallery-content decision,
// not an addressing one, and the epoch's diff is worth keeping readable as
// pure re-addressing. The claim is corrected here rather than carried
// forward; restoring a genuine land/water chart is open work.

/// The observer room a possession's own `map`/`scene surrounds --render
/// ascii` would draw for room 3015902083 — face 3, depth 13, at 17.1785 N,
/// 103.6835 W (the gallery's "The water east of the flagship").
///
/// **Re-minted by The Pavement, and the id it replaces does not decode.** It
/// was 897392747: face **11** of the icosphere, at depth 12. A room id packs
/// its base face in its low five bits, the occupancy lattice is a cube-sphere
/// now, and `FacetId::unpack` refuses any face >= 6 — decision 0189 working as
/// written, since a pre-flip address must fail loudly rather than decode into
/// a valid-looking cube address.
///
/// **The GROUND is unchanged**, which is the whole point: 3015902083 is
/// `Facet::containing` the same point at the walk band's new depth, so this
/// observer stands where it always stood and only its name moved.
fn coastline_room() -> hornvale_kernel::Facet {
    hornvale_kernel::FacetId(3015902083)
        .unpack()
        .expect("3015902083 is a valid packed room id (the gallery's coastline observer)")
}

/// The observer room for room 2290649216 — face 0, depth 13, at 10.7309 S,
/// 44.9945 W — whose radius-4 neighbourhood reaches across a base-face edge
/// for 36 of its 81 rooms (the gallery's "A seam, drawn").
///
/// **Re-minted by The Pavement, and unlike [`coastline_room`] this one could
/// not keep its ground.** It was 724698318: face **14** of the icosphere at
/// depth 12, addressing 10.0 S, 0.0 E — a point chosen because it sat on an
/// ICOSAHEDRON face edge. The cube's face boundaries are elsewhere entirely
/// (they are the great circles where two of |x|, |y|, |z| are equal, i.e. 45
/// degrees from each face's centre), and 10 S / 0 E is ~35 degrees inside the
/// +x face — thousands of kilometres from any seam, against a walk-band room
/// ~1.1 km across. Keeping the coordinate would have kept a green test that
/// no longer exercised the seam branch at all.
///
/// So this address is REACHED, not chosen: it is the first candidate the
/// `seam_observer` search in `windows/scene/src/surrounds.rs` returns — face 0
/// with the alternating path `0,1,0,1,…` at walk depth, which descends toward
/// a face EDGE (rather than a face CORNER, where three quads meet and the ball
/// holds 65 rooms rather than 81). Its longitude, -44.9945, is a fifth of a
/// room off the -45 degree meridian, which is exactly where a cube seam runs.
fn seam_room() -> hornvale_kernel::Facet {
    hornvale_kernel::FacetId(2290649216)
        .unpack()
        .expect("2290649216 is a valid packed room id (the gallery's seam observer)")
}

/// The `ways on:` footer text, computed exactly as `cmd_scene`'s `--render
/// ascii` path and the possession's own `map` verb both compute it: the
/// observer room's own lateral (`ExitKind::Edge`) exits, read from
/// `hornvale_locale`.
fn ways_on(ctx: &hornvale_locale::LocaleContext, room: &hornvale_kernel::Facet) -> Vec<String> {
    let locale = ctx
        .describe(room, hornvale_kernel::WorldTime::GENESIS)
        .expect("the gallery's pinned observers describe cleanly")
        .exits;
    locale
        .iter()
        .filter(|e| e.kind == hornvale_locale::ExitKind::Edge)
        .filter_map(|e| match e.direction {
            hornvale_locale::Direction::Compass(c) => Some(format!("{c:?}").to_uppercase()),
            _ => None,
        })
        .collect()
}

fn surrounds_ascii_coastline() -> String {
    let w = seed_42_world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let room = coastline_room();
    let scene = surrounds_scene(&w, &room, 4, hornvale_kernel::WorldTime::GENESIS).unwrap();
    render_surrounds_ascii(&scene, "terrain", &ways_on(&ctx, &room))
}

#[test]
fn surrounds_ascii_coastline_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/surrounds-ascii-seed-42-coastline.txt"
        )),
        &surrounds_ascii_coastline(),
        "the gallery's coastline observer's ASCII chart bytes moved (The Margin's showcase \
         page, room 3015902083) — platform-local pin (host-libm-sensitive biome/water/relief \
         classification, same exposure class as the scene-tiles exclusion); accept \
         deliberately, re-run with REBASELINE=1, and review the diff",
    );
}

fn surrounds_ascii_seam() -> String {
    let w = seed_42_world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let room = seam_room();
    let scene = surrounds_scene(&w, &room, 4, hornvale_kernel::WorldTime::GENESIS).unwrap();
    render_surrounds_ascii(&scene, "terrain", &ways_on(&ctx, &room))
}

#[test]
fn surrounds_ascii_seam_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/surrounds-ascii-seed-42-seam.txt"
        )),
        &surrounds_ascii_seam(),
        "the gallery's seam observer's ASCII chart bytes moved (The Margin's showcase page, \
         room 2290649216) — platform-local pin (host-libm-sensitive biome/water/relief \
         classification, same exposure class as the scene-tiles exclusion); accept \
         deliberately, re-run with REBASELINE=1, and review the diff",
    );
}

/// Spec §5.2 on the one REAL band in the repo that is known to reach across a
/// base-face seam: [`seam_room`]'s radius-4 neighbourhood, 36 of whose 81
/// rooms lie on the far side of one. Under the lattice projection such a room
/// had no coordinate and was counted in a footer instead of drawn. Bearing and
/// distance exist across a seam, so every room of the ball reaches the chart's
/// placement accounting now, the seam-side ones included.
///
/// Asserted here rather than left to the byte pin because a byte pin says
/// only "the render did not change since someone accepted it"; this says
/// what the render must CONTAIN, and it is what would catch a future
/// projection that quietly went back to dropping ground it could not place.
///
/// **The Pavement moved every number in this test and split one claim in
/// two.** The band was `(31, 12)` on the icosphere's triangle mesh and is
/// `(81, 36)` on the cube-sphere's quad mesh — a radius-4 ball is a 9x9 block
/// now, which is the campaign's thesis rather than corruption. The claim that
/// had to split is the old caption assertion, `31 of 31 drawn and 0 occluded`:
/// with 81 rooms on a character grid, two of them DO land in one box, and the
/// render says so (48 drawn, 33 occluded at the time of writing). Occlusion is
/// a property of squeezing a chart onto characters and is disclosed in the
/// caption; DROPPING ground for want of a coordinate is the defect this test
/// exists for. So the
/// accounting is asserted to be COMPLETE — `drawn + occluded` is the whole
/// ball — rather than asserted to be lossless, and neither figure is pinned to
/// a literal, since the byte golden beside this already does that job.
#[test]
fn every_cell_of_the_seam_band_is_drawn_including_the_seam_cells() {
    let w = seed_42_world();
    let room = seam_room();
    let scene = surrounds_scene(&w, &room, 4, hornvale_kernel::WorldTime::GENESIS).unwrap();
    let seams = scene.cells.iter().filter(|c| c.seam).count();
    assert_eq!(
        (scene.cells.len(), seams),
        (81, 36),
        "this test is only meaningful on a band that really does cross a face \
         seam — if these counts moved, re-point `seam_room` before touching \
         the assertions below"
    );
    let out = surrounds_ascii_seam();
    let caption = out
        .lines()
        .find(|l| l.contains("cells drawn"))
        .unwrap_or_else(|| panic!("the chart must caption its own placement: {out}"));
    // "… — 48 of 81 cells drawn, 33 occluded where two fell in one box …"
    let number_before = |needle: &str| -> usize {
        let head = &caption[..caption
            .find(needle)
            .unwrap_or_else(|| panic!("caption has no `{needle}`: {caption}"))];
        head.trim_end()
            .rsplit(|c: char| !c.is_ascii_digit())
            .next()
            .and_then(|d| d.parse::<usize>().ok())
            .unwrap_or_else(|| panic!("no count before `{needle}`: {caption}"))
    };
    let drawn = number_before(" of 81 cells drawn");
    let occluded = number_before(" occluded");
    assert_eq!(
        drawn + occluded,
        81,
        "the caption must account for every room of the ball — one that is \
         neither drawn nor occluded has been DROPPED, which is the defect this \
         test exists for: {out}"
    );
    assert!(
        !out.contains("beyond a face seam"),
        "the seam footer is retired; ground beyond a seam has an honest place \
         on a north-up chart: {out}"
    );
    // The picture, not just the caption's arithmetic: the drawn boxes must
    // actually be on the page. Counting glyphs is what separates "the
    // renderer believes it placed them" from "they are visible".
    let glyphs = out
        .lines()
        .filter(|l| !l.starts_with('[') && !l.contains(": "))
        .flat_map(str::chars)
        .filter(|c| !c.is_whitespace())
        .count();
    assert_eq!(
        glyphs, drawn,
        "the picture must carry the {drawn} glyphs its own caption claims: {out}"
    );
}

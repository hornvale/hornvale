//! The situated pole of the scene protocol: `scene/surrounds/v1`, an
//! egocentric neighbourhood of rooms around an observer, placed by exact
//! integer lattice coordinates. Semantic-only and FOG-FREE — this builder
//! never invents epistemic state; a session-owning consumer (the vessel)
//! overlays what it alone knows.

use crate::{Feature, SceneError, features_of};
use hornvale_kernel::{RoomAddr, World, WorldTime};
use hornvale_locale::{Locale, LocaleContext, biome_prose_name};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// The schema identifier this module emits.
/// type-audit: bare-ok(identifier-text)
pub const SURROUNDS_SCHEMA: &str = "scene/surrounds/v1";

/// The largest legal neighbourhood radius, in BFS rings. A ring-`k`
/// neighbourhood holds `1 + 3k(k+1)/2` cells, so 8 is 109 cells — past
/// what a coarse chart can say anything useful with.
/// type-audit: bare-ok(count)
pub const MAX_SURROUNDS_RADIUS: u32 = 8;

/// The relief catalog, in stable ascending order. Band boundaries are
/// contract: changing one mints `scene/surrounds/v2`.
/// type-audit: bare-ok(identifier-text)
pub const RELIEF_LEGEND: [&str; 6] = ["abyss", "shelf", "lowland", "upland", "highland", "alpine"];

/// Elevation (m) to an index into [`RELIEF_LEGEND`].
/// type-audit: bare-ok(index: return)
fn relief_band(elevation_m: f64) -> u32 {
    match elevation_m {
        e if e < -3000.0 => 0,
        e if e < 0.0 => 1,
        e if e < 300.0 => 2,
        e if e < 1000.0 => 3,
        e if e < 2500.0 => 4,
        _ => 5,
    }
}

/// Where the observer stands.
/// type-audit: bare-ok(index: room), bare-ok(index: face), bare-ok(count: depth), pending(wave-3: latitude), pending(wave-3: longitude)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsObserver {
    /// Packed room id.
    pub room: u64,
    /// Base icosahedron face.
    pub face: u8,
    /// Refinement depth.
    pub depth: u32,
    /// Centroid latitude, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub latitude: f64,
    /// Centroid longitude, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub longitude: f64,
}

/// A salience-ranked thing standing on a cell. `noun` is the examinable key
/// — it is what joins this chart to the prose's own noun catalog.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(identifier-text: kind), bare-ok(prose: datum), bare-ok(index: salience)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Mark {
    /// The examinable noun.
    pub noun: String,
    /// What kind of thing this is: `"settlement"` or `"agent"`.
    pub kind: String,
    /// One line about it — the datum `examine` prints.
    pub datum: String,
    /// Rank key; lower is more salient.
    pub salience: u32,
}

/// One `(noun, datum)` pair of the chart's catalog — deliberately the same
/// shape as the focalizer's `Focalized.nouns`, because that identity is what
/// makes map and prose two grains of one lens.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(prose: datum)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LegendEntry {
    /// The examinable noun.
    pub noun: String,
    /// What `examine` prints for it.
    pub datum: String,
}

/// One cell of the chart. Lattice coordinates are RELATIVE to the observer
/// and absent on a seam cell. Fine-grain fields are `null` at coarse grain —
/// a cell carries the detail its epistemic state warrants, which is what
/// makes the chart and the prose one lens rather than two.
/// type-audit: bare-ok(index: room), bare-ok(index: u), bare-ok(index: v), bare-ok(index: w), bare-ok(flag: up), bare-ok(flag: seam), bare-ok(identifier-text: state), bare-ok(index: biome), bare-ok(index: water), bare-ok(index: relief), bare-ok(prose: regime), bare-ok(diagnostic-value: temperature_c), bare-ok(ratio: moisture), waiver(elevation-convention: elevation_m), bare-ok(artifact: color)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsCell {
    /// Packed room id.
    pub room: u64,
    /// Lattice offset from the observer on axis 0; `null` on a seam cell.
    pub u: Option<i64>,
    /// Lattice offset on axis 1; `null` on a seam cell.
    pub v: Option<i64>,
    /// Lattice offset on axis 2; `null` on a seam cell.
    pub w: Option<i64>,
    /// Triangle orientation; `null` on a seam cell.
    pub up: Option<bool>,
    /// Set when this cell lies on a different base face than the observer,
    /// so the lattice bends and no honest local coordinate exists.
    pub seam: bool,
    /// `"here"`, `"sensed"`, or (written only by a session-owning consumer)
    /// `"remembered"`.
    pub state: String,
    /// Index into `biome_legend`.
    pub biome: u32,
    /// Index into `water_legend`.
    pub water: u32,
    /// Index into `relief_legend`.
    pub relief: u32,
    /// The strangeness overlay's descriptor — fine grain, `null` when coarse.
    pub regime: Option<String>,
    /// Annual-mean temperature, °C — fine grain, `null` when coarse.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub temperature_c: Option<f64>,
    /// Moisture — fine grain, `null` when coarse.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub moisture: Option<f64>,
    /// Elevation, metres — fine grain, `null` when coarse.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub elevation_m: Option<f64>,
    /// Display colour under the requested observer, absent unless this scene
    /// was built through [`surrounds_scene_colored_in`]. The key is skipped
    /// entirely when absent, so an uncoloured document is byte-for-byte what
    /// it was before the colour layer existed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color: Option<[u8; 3]>,
    /// Salience-ranked things standing here.
    pub marks: Vec<Mark>,
}

/// One `scene/surrounds/v1` document. Field order is the JSON key order and
/// is contract — never reorder.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed), bare-ok(diagnostic-value: day), bare-ok(count: radius), bare-ok(count: depth), bare-ok(identifier-text: orientation), bare-ok(identifier-text: biome_legend), bare-ok(identifier-text: water_legend), bare-ok(identifier-text: relief_legend)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsScene {
    /// Always `scene/surrounds/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The day observed.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub day: f64,
    /// Where the observer stands.
    pub observer: SurroundsObserver,
    /// Neighbourhood radius, in BFS rings.
    pub radius: u32,
    /// The refinement depth every cell sits at.
    pub depth: u32,
    /// Always `"lattice"`: the chart is lattice-aligned, never north-up. A
    /// consumer that wants north must ask the rooms for their bearings.
    pub orientation: String,
    /// The biome catalog, stable append-only order.
    pub biome_legend: Vec<String>,
    /// The water catalog, stable order.
    pub water_legend: Vec<String>,
    /// The relief catalog, stable ascending order.
    pub relief_legend: Vec<String>,
    /// The cells, ascending by `room`.
    pub cells: Vec<SurroundsCell>,
    /// The chart's noun catalog, ascending by `noun`.
    pub legend: Vec<LegendEntry>,
}

/// Build the `scene/surrounds/v1` document for `room` at `radius` rings,
/// reusing a `LocaleContext` the caller already built. Fog-free: every cell
/// but the observer's is `"sensed"`.
///
/// Prefer this over [`surrounds_scene`] whenever a `LocaleContext` is
/// already in hand (e.g. a session-owning caller building one chart per
/// player turn): measured in release, `LocaleContext::build` costs ~1.19 s
/// against ~2 ms of this function's own per-cell work, so building a fresh
/// context per call would make a radius-0 chart cost the same as a
/// radius-8 one.
/// type-audit: bare-ok(count: radius)
pub fn surrounds_scene_in(
    world: &World,
    ctx: &LocaleContext,
    room: &RoomAddr,
    radius: u32,
    at: WorldTime,
) -> Result<SurroundsScene, SceneError> {
    if radius > MAX_SURROUNDS_RADIUS {
        return Err(SceneError::SurroundsRadiusOutOfRange(radius));
    }
    let here = ctx
        .describe(room, at)
        .map_err(|e| SceneError::Build(e.to_string()))?;

    // Breadth-first over the mesh's edge-adjacency graph, out to `radius`
    // rings. BTreeSet/VecDeque only — no HashSet (determinism).
    let mut seen: BTreeSet<RoomAddr> = BTreeSet::new();
    let mut queue: VecDeque<(RoomAddr, u32)> = VecDeque::new();
    seen.insert(room.clone());
    queue.push_back((room.clone(), 0));
    let mut found: Vec<RoomAddr> = vec![room.clone()];
    while let Some((addr, ring)) = queue.pop_front() {
        if ring == radius {
            continue;
        }
        for n in addr.neighbors() {
            if seen.insert(n.clone()) {
                found.push(n.clone());
                queue.push_back((n, ring + 1));
            }
        }
    }

    let origin = room.face_lattice();
    let catalog = hornvale_climate::Biome::catalog();

    // Settlement marks, keyed by the room each settlement's coordinates land
    // in at this depth.
    let marks_by_room = settlement_marks(world, room.depth());

    let mut cells: Vec<SurroundsCell> = Vec::with_capacity(found.len());
    for addr in &found {
        let locale = ctx
            .describe(addr, at)
            .map_err(|e| SceneError::Build(e.to_string()))?;
        let is_here = addr == room;
        let seam = addr.face != room.face;
        let lat = if seam {
            None
        } else {
            Some(addr.face_lattice())
        };
        let key = addr
            .pack()
            .map_err(|e| SceneError::SurroundsUnaddressable(format!("{e:?}")))?
            .0;
        let mut marks = marks_by_room.get(&key).cloned().unwrap_or_default();
        marks.sort_by(|a, b| a.salience.cmp(&b.salience).then(a.noun.cmp(&b.noun)));
        cells.push(SurroundsCell {
            room: key,
            u: lat.map(|l| l.a - origin.a),
            v: lat.map(|l| l.b - origin.b),
            w: lat.map(|l| l.c - origin.c),
            up: lat.map(|l| l.up),
            seam,
            state: if is_here { "here" } else { "sensed" }.to_string(),
            biome: catalog
                .iter()
                .position(|e| *e == locale.biome_kind)
                .expect("every biome is in the catalog") as u32,
            water: u32::from(locale.fields.water.index()),
            relief: relief_band(locale.fields.elevation_m),
            regime: is_here.then(|| locale.regime.descriptor.clone()),
            temperature_c: is_here.then_some(locale.fields.temperature_c),
            moisture: is_here.then_some(locale.fields.moisture),
            elevation_m: is_here.then_some(locale.fields.elevation_m),
            // The default path never colours. `surrounds_scene_colored_in`
            // is the only writer, which is what keeps every committed
            // artifact byte-identical.
            color: None,
            marks,
        });
    }
    cells.sort_by_key(|c| c.room);

    let legend = legend_of(&cells, &here, catalog);

    let observer_room = room
        .pack()
        .map_err(|e| SceneError::SurroundsUnaddressable(format!("{e:?}")))?
        .0;

    Ok(SurroundsScene {
        schema: SURROUNDS_SCHEMA.to_string(),
        seed: world.seed.0,
        day: at.day,
        observer: SurroundsObserver {
            room: observer_room,
            face: room.face,
            depth: room.depth(),
            latitude: here.latitude,
            longitude: here.longitude,
        },
        radius,
        depth: room.depth(),
        orientation: "lattice".to_string(),
        biome_legend: catalog.iter().map(|b| b.name().to_string()).collect(),
        water_legend: hornvale_terrain::WaterKind::LEGEND
            .iter()
            .map(|s| s.to_string())
            .collect(),
        relief_legend: RELIEF_LEGEND.iter().map(|s| s.to_string()).collect(),
        cells,
        legend,
    })
}

/// Build the `scene/surrounds/v1` document for `room` at `radius` rings.
/// Fog-free: every cell but the observer's is `"sensed"`.
///
/// Builds a fresh `LocaleContext` per call — a caller that already holds
/// one (or that will make more than one surrounds query, e.g. once per
/// player turn) should call [`surrounds_scene_in`] instead and hold the
/// context itself, since the rebuild dominates this function's cost.
///
/// The radius bound is checked here too, BEFORE `LocaleContext::build` —
/// measured in release, that build costs ~1.2 s, so an invalid radius must
/// fail before it, not after, or rejecting a bad argument would cost as
/// much as building a whole chart. `surrounds_scene_in` repeats the same
/// check, since it is public and must validate its own arguments
/// independently of this wrapper.
/// type-audit: bare-ok(count: radius)
pub fn surrounds_scene(
    world: &World,
    room: &RoomAddr,
    radius: u32,
    at: WorldTime,
) -> Result<SurroundsScene, SceneError> {
    if radius > MAX_SURROUNDS_RADIUS {
        return Err(SceneError::SurroundsRadiusOutOfRange(radius));
    }
    let ctx = LocaleContext::build(world).map_err(|e| SceneError::Build(e.to_string()))?;
    surrounds_scene_in(world, &ctx, room, radius, at)
}

/// Build a `scene/surrounds/v1` document with a colour layer, as seen by
/// `observer` under the world star's daylight.
///
/// A separate entry point rather than a parameter on
/// [`surrounds_scene_in`]: every committed artifact goes through the
/// uncoloured path, and this way they cannot move.
///
/// Cells whose observer has no truthful sRGB image keep `color: None` — the
/// mapping for a non-standard observer is a false-colour choice the caller
/// must declare (RENDER-9), not one this builder may invent.
///
/// The light is the star's daylight spectrum, not its light at the
/// observer's own sun elevation: `scene/surrounds/v1` carries no sun angle,
/// and inventing one here would make the chart's colours disagree with a
/// prose renderer that knows the real hour. A caller with an elevation in
/// hand should say so — `hornvale_astronomy::illuminant::at_elevation` is
/// the seam, and the day it belongs on is a `scene/surrounds/v2` question.
/// type-audit: bare-ok(count: radius)
pub fn surrounds_scene_colored_in(
    world: &World,
    ctx: &LocaleContext,
    room: &RoomAddr,
    radius: u32,
    at: WorldTime,
    observer: &hornvale_kernel::color::Observer,
) -> Result<SurroundsScene, SceneError> {
    let mut scene = surrounds_scene_in(world, ctx, room, radius, at)?;
    let star = hornvale_astronomy::star::generate_star(
        world.seed.derive(hornvale_astronomy::streams::ROOT),
    );
    let light = hornvale_astronomy::illuminant::daylight(&star);
    for cell in scene.cells.iter_mut() {
        let addr = hornvale_kernel::RoomId(cell.room)
            .unpack()
            .map_err(|e| SceneError::SurroundsUnaddressable(format!("{e:?}")))?;
        let reflectance = ctx
            .reflectance_at(&addr)
            .map_err(|e| SceneError::Build(e.to_string()))?;
        cell.color = observer.to_srgb(&observer.sense(&reflectance, &light));
    }
    Ok(scene)
}

/// Settlement marks keyed by the packed room id their coordinates fall in at
/// `depth`. The flagship outranks the rest.
fn settlement_marks(world: &World, depth: u32) -> BTreeMap<u64, Vec<Mark>> {
    let mut out: BTreeMap<u64, Vec<Mark>> = BTreeMap::new();
    for f in features_of(world) {
        let Feature {
            name,
            kind,
            latitude,
            longitude,
        } = f;
        let position = hornvale_kernel::math::unit_sphere_from_lat_lon(latitude, longitude);
        let Ok(id) = RoomAddr::containing(position, depth).pack() else {
            continue;
        };
        let flagship = kind == "flagship";
        out.entry(id.0).or_default().push(Mark {
            datum: if flagship {
                format!("{name} — the settlement this possession was minted from.")
            } else {
                format!("{name} — a settlement of this world.")
            },
            noun: name,
            kind: "settlement".to_string(),
            salience: if flagship { 10 } else { 20 },
        });
    }
    out
}

/// The chart's noun catalog: every mark's noun, plus one entry per distinct
/// terrain class drawn, plus the observer's own room. Biome nouns use the
/// spaced prose name ([`biome_prose_name`]), not the kebab-case identifier
/// `biome_legend` indexes into — the legend is player-facing text, and using
/// the prose name here makes the biome a noun shared with the prose
/// renderer's own catalog, joining the two grains on one datum (The Margin).
fn legend_of(
    cells: &[SurroundsCell],
    here: &Locale,
    catalog: &'static [hornvale_climate::Biome],
) -> Vec<LegendEntry> {
    let mut acc: BTreeMap<String, String> = BTreeMap::new();
    for c in cells {
        for m in &c.marks {
            acc.insert(m.noun.clone(), m.datum.clone());
        }
        let biome = catalog
            .get(c.biome as usize)
            .map(|b| biome_prose_name(*b).to_string())
            .unwrap_or_default();
        acc.entry(biome.clone()).or_insert_with(|| {
            format!(
                "{biome} — {} of the {} cells in view.",
                cells.iter().filter(|d| d.biome == c.biome).count(),
                cells.len()
            )
        });
    }
    acc.insert(
        here.regime.descriptor.clone(),
        format!(
            "The ground where you stand: {} (strangeness {:.0}).",
            here.regime.descriptor, here.regime.strangeness
        ),
    );
    acc.into_iter()
        .map(|(noun, datum)| LegendEntry { noun, datum })
        .collect()
}

/// Serialize a `SurroundsScene` to compact JSON (mirrors `scene_json`).
/// type-audit: bare-ok(artifact: return)
pub fn surrounds_json(scene: &SurroundsScene) -> String {
    serde_json::to_string(scene).expect("a surrounds scene serializes")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::place_latlon;
    use hornvale_kernel::{Seed, WorldTime};
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn world() -> hornvale_kernel::World {
        build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    fn observer(w: &hornvale_kernel::World) -> RoomAddr {
        let ctx = hornvale_locale::LocaleContext::build(w).unwrap();
        let depth = ctx.globe_level() + 6;
        // The flagship settlement's own room — the same place a possession
        // mints its agent, so the gallery scene shows the walked ground.
        let v = hornvale_settlement::village_info(w).expect("seed 42 has a village");
        let (lat, lon) = place_latlon(w, v.id).expect("the flagship has coordinates");
        RoomAddr::containing(
            hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon),
            depth,
        )
    }

    #[test]
    fn a_radius_four_neighbourhood_holds_thirty_one_cells() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(s.schema, SURROUNDS_SCHEMA);
        assert_eq!(s.radius, 4);
        // Ball sizes in the triangular face-adjacency lattice are
        // 1 + 3k(k+1)/2: 1, 4, 10, 19, 31, ...
        assert_eq!(s.cells.len(), 31);
    }

    #[test]
    fn exactly_one_cell_is_here_and_it_sits_at_the_lattice_origin() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 3, WorldTime { day: 0.0 }).unwrap();
        let here: Vec<&SurroundsCell> = s.cells.iter().filter(|c| c.state == "here").collect();
        assert_eq!(here.len(), 1);
        assert_eq!(
            (here[0].u, here[0].v, here[0].w),
            (Some(0), Some(0), Some(0))
        );
        assert_eq!(here[0].room, s.observer.room);
        assert!(!here[0].seam);
    }

    // The flagship observer's own neighbourhood never crosses a base face at
    // radius 4 (a coincidence of where seed 42 places its village) — so this
    // test alone never enters the `if c.seam` branch. It stays as coverage
    // of the no-seam case; `a_seam_observer_carries_no_coordinate_on_seam_cells`
    // below is the real seam-handling test.
    #[test]
    fn every_non_seam_cell_carries_a_lattice_coordinate_and_seam_cells_carry_none() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime { day: 0.0 }).unwrap();
        for c in &s.cells {
            if c.seam {
                assert!(c.u.is_none() && c.v.is_none() && c.w.is_none() && c.up.is_none());
            } else {
                assert!(c.u.is_some() && c.v.is_some() && c.w.is_some() && c.up.is_some());
            }
        }
    }

    /// An observer verified to sit near a base-face seam (latitude -10°,
    /// longitude 0°, depth 12 lands on face 14), whose radius-4 neighbourhood
    /// genuinely crosses onto neighbouring faces — the real coverage for the
    /// seam branch the sibling test above never reaches. Uses the same
    /// lat/lon -> unit-sphere conversion as the `observer` helper above.
    #[test]
    fn a_seam_observer_carries_no_coordinate_on_seam_cells() {
        let w = world();
        let seam_observer = RoomAddr::containing(
            hornvale_kernel::math::unit_sphere_from_lat_lon(-10.0, 0.0),
            12,
        );
        assert_eq!(
            seam_observer.face, 14,
            "fixture observer must land on the verified face"
        );
        let s = surrounds_scene(&w, &seam_observer, 4, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(s.cells.len(), 31, "no cell was dropped");

        let seam_count = s.cells.iter().filter(|c| c.seam).count();
        assert_ne!(
            seam_count, 0,
            "fixture observer must actually see seam cells, or this test is vacuous again"
        );
        assert_eq!(
            seam_count, 12,
            "verified fixture: 12 of 31 cells are seam cells at radius 4"
        );

        for c in &s.cells {
            if c.seam {
                assert!(
                    c.u.is_none() && c.v.is_none() && c.w.is_none() && c.up.is_none(),
                    "seam cell {} carries a lattice coordinate",
                    c.room
                );
            } else {
                assert!(
                    c.u.is_some() && c.v.is_some() && c.w.is_some() && c.up.is_some(),
                    "non-seam cell {} is missing a lattice coordinate",
                    c.room
                );
            }
        }
    }

    #[test]
    fn the_document_is_byte_identical_on_rebuild() {
        let w = world();
        let o = observer(&w);
        let a = surrounds_json(&surrounds_scene(&w, &o, 4, WorldTime { day: 0.0 }).unwrap());
        let b = surrounds_json(&surrounds_scene(&w, &o, 4, WorldTime { day: 0.0 }).unwrap());
        assert_eq!(a, b);
        // "Rebuild from the ledger": there is no `hornvale_worldgen::rebuild`
        // helper (confirmed absent workspace-wide) — the established pattern
        // for this exact assertion is a save/load round trip through the
        // world's own JSON save format (see e.g.
        // `windows/worldgen/src/lib.rs`'s
        // `generated_sky_round_trips_through_save_and_load`,
        // `kernel/src/world.rs`'s own round-trip test, and
        // `windows/explain/src/lib.rs`).
        let rebuilt = hornvale_kernel::World::from_json(&w.to_json())
            .expect("a world rebuilds from its own ledger");
        let c = surrounds_json(&surrounds_scene(&rebuilt, &o, 4, WorldTime { day: 0.0 }).unwrap());
        assert_eq!(a, c, "same world + same query => byte-identical JSON");
    }

    #[test]
    fn the_radius_is_bounded_loudly() {
        let w = world();
        let e = surrounds_scene(
            &w,
            &observer(&w),
            MAX_SURROUNDS_RADIUS + 1,
            WorldTime { day: 0.0 },
        )
        .unwrap_err();
        assert_eq!(
            e,
            SceneError::SurroundsRadiusOutOfRange(MAX_SURROUNDS_RADIUS + 1)
        );
    }

    // `surrounds_scene_in` is public and must validate its own arguments
    // independently of the `surrounds_scene` wrapper's hoisted check above —
    // this pins that the inner function still rejects on its own, not just
    // because the wrapper happened to catch it first.
    #[test]
    fn the_inner_function_is_bounded_loudly_too() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let e = surrounds_scene_in(
            &w,
            &ctx,
            &observer(&w),
            MAX_SURROUNDS_RADIUS + 1,
            WorldTime { day: 0.0 },
        )
        .unwrap_err();
        assert_eq!(
            e,
            SceneError::SurroundsRadiusOutOfRange(MAX_SURROUNDS_RADIUS + 1)
        );
    }

    #[test]
    fn cells_are_ordered_by_room_id() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime { day: 0.0 }).unwrap();
        let ids: Vec<u64> = s.cells.iter().map(|c| c.room).collect();
        let mut sorted = ids.clone();
        sorted.sort_unstable();
        assert_eq!(ids, sorted, "cell order is contract: ascending room id");
    }

    fn colored(w: &hornvale_kernel::World, radius: u32) -> SurroundsScene {
        let ctx = hornvale_locale::LocaleContext::build(w).unwrap();
        surrounds_scene_colored_in(
            w,
            &ctx,
            &observer(w),
            radius,
            WorldTime { day: 0.0 },
            &hornvale_kernel::color::standard_observer(),
        )
        .unwrap()
    }

    #[test]
    fn the_uncolored_builder_leaves_every_cell_without_a_color() {
        // This is what keeps book/src/gallery/scene-surrounds-seed-42.json
        // byte-identical: the field is skipped when None.
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 2, WorldTime { day: 0.0 }).unwrap();
        for cell in &s.cells {
            assert!(
                cell.color.is_none(),
                "the default builder invented a colour on room {}",
                cell.room
            );
        }
    }

    #[test]
    fn the_colored_builder_gives_placed_cells_a_color() {
        let w = world();
        let s = colored(&w, 2);
        let with = s.cells.iter().filter(|c| c.color.is_some()).count();
        assert_eq!(
            with,
            s.cells.len(),
            "{with} of {} cells received a colour — the standard observer has a \
             truthful sRGB image, so every placed cell must",
            s.cells.len()
        );
    }

    #[test]
    fn the_uncolored_json_emits_no_color_key() {
        // serde skip_serializing_if means an absent colour emits no key at
        // all, so the committed gallery JSON cannot move.
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 1, WorldTime { day: 0.0 }).unwrap();
        let json = crate::surrounds_json(&s);
        assert!(
            !json.contains("\"color\""),
            "an absent colour still emitted a key"
        );
    }

    #[test]
    fn a_colored_document_does_emit_the_key() {
        // The negative test above is only meaningful if the key is emitted
        // when a colour IS present — otherwise `skip_serializing_if` could be
        // a blanket `serde(skip)` and nothing would notice.
        let w = world();
        let json = crate::surrounds_json(&colored(&w, 1));
        assert!(
            json.contains("\"color\""),
            "a coloured document dropped the key it was built to carry"
        );
    }

    #[test]
    fn coloring_is_deterministic_across_repeated_builds() {
        let w = world();
        let a: Vec<_> = colored(&w, 2).cells.iter().map(|c| c.color).collect();
        let b: Vec<_> = colored(&w, 2).cells.iter().map(|c| c.color).collect();
        assert_eq!(a, b);
    }

    /// A non-standard observer has no truthful sRGB image, so its cells keep
    /// `color: None` — a false-colour mapping is the caller's to declare
    /// (RENDER-9), never this builder's to invent.
    #[test]
    fn a_non_standard_observer_is_left_uncolored() {
        use hornvale_kernel::color::{Observer, Spectrum};
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        // Two channels: nothing sRGB can be made of.
        let dichromat = Observer::new(vec![
            Spectrum::new([1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap(),
            Spectrum::new([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0]).unwrap(),
        ])
        .unwrap();
        let s = surrounds_scene_colored_in(
            &w,
            &ctx,
            &observer(&w),
            2,
            WorldTime { day: 0.0 },
            &dichromat,
        )
        .unwrap();
        assert!(!s.cells.is_empty());
        for cell in &s.cells {
            assert!(
                cell.color.is_none(),
                "the builder invented an sRGB colour for a two-channel eye"
            );
        }
    }

    /// Everything else about a coloured chart must be the coloured chart's
    /// only difference: the colour layer is additive, and if it perturbed a
    /// biome index or a mark the committed artifacts would be at risk the
    /// moment anything switched builders.
    #[test]
    fn coloring_changes_nothing_but_the_color() {
        let w = world();
        let plain = surrounds_scene(&w, &observer(&w), 2, WorldTime { day: 0.0 }).unwrap();
        let mut stripped = colored(&w, 2);
        for cell in stripped.cells.iter_mut() {
            cell.color = None;
        }
        assert_eq!(plain, stripped);
    }

    /// **A colour chart at walking depth is one flat wash, and that is the
    /// honest answer.**
    ///
    /// Rock class is read from the room's dominant *canonical-grid* corner
    /// (`LocaleContext::reflectance_at`), and the vessel walks at
    /// `globe_level + 6` — rooms roughly 64× finer per axis than a globe
    /// cell. A radius-8 neighbourhood of those rooms (109 cells) lies inside
    /// a single grid cell, so it reports one rock, one biome, one water
    /// kind, one relief band — and now one colour. Measured on seed 42: at
    /// `globe_level + 6` every radius from 2 to 8 yields exactly one
    /// distinct sRGB value (`#828074`), while at `globe_level` a radius-4
    /// chart yields six.
    ///
    /// This test exists so a consumer cannot mistake the flatness for a bug
    /// in the colour layer. The colour is exactly as spatially resolved as
    /// every categorical field the chart already carried; a finer colour
    /// would need a finer lithology, not a different builder.
    #[test]
    fn the_color_is_no_finer_grained_than_the_chart_already_was() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let gl = ctx.globe_level();
        let v = hornvale_settlement::village_info(&w).expect("seed 42 has a village");
        let (lat, lon) = place_latlon(&w, v.id).expect("the flagship has coordinates");
        let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);

        let distinct = |depth: u32, radius: u32| -> (usize, usize) {
            let s = surrounds_scene_colored_in(
                &w,
                &ctx,
                &RoomAddr::containing(pos, depth),
                radius,
                WorldTime { day: 0.0 },
                &hornvale_kernel::color::standard_observer(),
            )
            .unwrap();
            let colors: BTreeSet<Option<[u8; 3]>> = s.cells.iter().map(|c| c.color).collect();
            let biomes: BTreeSet<u32> = s.cells.iter().map(|c| c.biome).collect();
            (colors.len(), biomes.len())
        };

        // At walking depth the whole neighbourhood is one grid cell.
        let (walk_colors, walk_biomes) = distinct(gl + 6, 8);
        assert_eq!(
            walk_colors, 1,
            "a radius-8 walking-depth chart drew {walk_colors} colours; the \
             fixture is one flat wash"
        );
        assert_eq!(
            walk_biomes, 1,
            "the fixture's premise moved: the biome is no longer constant here"
        );

        // At the grid's own level the same query crosses many cells, so the
        // colour varies — proving the flatness above is the GRAIN and not a
        // constant baked into the builder.
        let (coarse_colors, _) = distinct(gl, 4);
        assert!(
            coarse_colors > 1,
            "the builder returned one colour even across {coarse_colors} grid \
             cells — it is not reading lithology at all"
        );
    }
}

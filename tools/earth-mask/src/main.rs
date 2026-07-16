//! `earth-mask`: rasterize Earth's real land/ocean layout onto Hornvale's
//! canonical L6 hex mesh, producing the committed
//! `book/src/laboratory/generated/earth-mask-l6/rows.csv` fixture that
//! anchors `D_earth` (rift-and-fit spec §7 — the shoreline-development
//! band's anchor decontamination).
//!
//! This binary is deliberately outside the cargo workspace (mirroring
//! `tools/type-audit`): it is an offline authoring tool ("models author,
//! dice roll" — the sim core never runs a script like this at world-gen
//! time), so it may use ordinary `std` float arithmetic rather than
//! `hornvale_kernel::math`; in practice the point-in-polygon test below
//! needs no transcendentals at all, only comparisons and linear
//! interpolation, so the question is moot in this tool's own code. The one
//! transcendental-touching step — converting a cell's unit-sphere position
//! to latitude/longitude — is delegated entirely to
//! `hornvale_kernel::Geosphere::coord`, which already routes through the
//! portable `hornvale_kernel::math` (decision 0041).
//!
//! ## Data source
//!
//! Natural Earth 110m Land (public domain, no attribution required):
//! - URL: `https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_land.geojson`
//! - sha256: `9e0729ee253ca7d7a5c4ae9395fb1902264c5377c52e224d13dd85010e2835d9`
//! - Committed at `tools/earth-mask/data/ne_110m_land.geojson`. Fetched
//!   once by the campaign controller; this tool never re-downloads it.
//!
//! ## Usage
//!
//! ```text
//! cargo run --manifest-path tools/earth-mask/Cargo.toml --release \
//!   > book/src/laboratory/generated/earth-mask-l6/rows.csv
//! ```
//!
//! Deterministic: pure arithmetic over the committed GeoJSON and the
//! seed-independent L6 `Geosphere`, so re-running reproduces the fixture
//! byte-for-byte.
//!
//! ## Method
//!
//! For every L6 cell (ascending `CellId`), take its geographic center
//! (`Geosphere::coord`) and test it against every ring (outer boundaries
//! and holes, of every `Polygon`/`MultiPolygon` feature) with an even-odd
//! ray cast in longitude/latitude degrees: a horizontal ray from the point
//! toward increasing longitude, counting edge crossings. Each ring's own
//! crossing parity toggles a single running boolean — outer rings and
//! holes both toggle, so holes fall out for free, and disjoint landmasses
//! never interfere (a point can lie inside at most one feature's outer
//! ring, so XOR and OR agree). Longitude is normalized to `[-180, 180)`
//! before testing (the 110m dataset's rings already split at the
//! antimeridian, so no edge needs to wrap). Antarctica's own ring closes
//! over the south pole with an explicit flat edge from `(180, -90)` to
//! `(-180, -90)`, so the standard planar algorithm resolves polar cells
//! without any special-casing; each ring's precomputed latitude bounding
//! box is still checked first; purely an optimization, since a ray whose
//! latitude falls outside a ring's own latitude span cannot cross any of
//! that ring's edges. A cell center landing exactly on a ring edge is
//! whatever the ray cast happens to decide — determinism is the only
//! requirement here, not geodetic perfection (the same cell always gets
//! the same answer, because the mesh and the data are both fixed).

use hornvale_kernel::Geosphere;
use serde_json::Value;

/// The canonical globe subdivision level (`hornvale_terrain::GLOBE_LEVEL`).
/// Hardcoded rather than imported: this tool depends on `hornvale-kernel`
/// only (constitutional layering — a domain crate is off-limits to a
/// kernel-only tool), so it cannot read the domain's constant directly.
const GLOBE_LEVEL: u32 = 6;

/// The committed Natural Earth land geometry, embedded at compile time via
/// `CARGO_MANIFEST_DIR` so the binary runs correctly regardless of the
/// caller's working directory.
const DATA_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/data/ne_110m_land.geojson");

/// One polygon ring (an outer boundary or a hole) as `(longitude,
/// latitude)` degree pairs in GeoJSON order, plus its precomputed latitude
/// bounding box (the only pruning the even-odd ray cast needs: a ray at a
/// latitude outside `[min_lat, max_lat]` cannot cross any edge of this
/// ring).
struct Ring {
    points: Vec<(f64, f64)>,
    min_lat: f64,
    max_lat: f64,
}

impl Ring {
    fn new(points: Vec<(f64, f64)>) -> Ring {
        let min_lat = points
            .iter()
            .map(|&(_, lat)| lat)
            .fold(f64::INFINITY, f64::min);
        let max_lat = points
            .iter()
            .map(|&(_, lat)| lat)
            .fold(f64::NEG_INFINITY, f64::max);
        Ring {
            points,
            min_lat,
            max_lat,
        }
    }

    /// Even-odd ray-cast parity of `(lon, lat)` against this single ring: a
    /// horizontal ray toward increasing longitude, counting edge crossings.
    /// Standard planar point-in-polygon; nothing sphere-aware, matching the
    /// module doc's discussion of the Antarctica ring's explicit polar
    /// closure edge.
    fn contains_by_ray_cast(&self, lon: f64, lat: f64) -> bool {
        if lat < self.min_lat || lat > self.max_lat {
            return false;
        }
        let n = self.points.len();
        let mut inside = false;
        for i in 0..n {
            let (x1, y1) = self.points[i];
            let (x2, y2) = self.points[(i + 1) % n];
            if (y1 > lat) != (y2 > lat) {
                let x_intersect = x1 + (lat - y1) * (x2 - x1) / (y2 - y1);
                if lon < x_intersect {
                    inside = !inside;
                }
            }
        }
        inside
    }
}

/// Extract `(longitude, latitude)` rings from one GeoJSON `Polygon`
/// geometry's `coordinates` array: the first ring is the outer boundary,
/// any further rings are holes. Both toggle identically under the even-odd
/// rule (module doc).
fn rings_from_polygon_coordinates(coordinates: &Value) -> Vec<Ring> {
    coordinates
        .as_array()
        .expect("Polygon coordinates is a JSON array of rings")
        .iter()
        .map(|ring| {
            let points = ring
                .as_array()
                .expect("ring is a JSON array of points")
                .iter()
                .map(|point| {
                    let p = point.as_array().expect("point is a JSON array");
                    let lon = p[0].as_f64().expect("longitude is a number");
                    let lat = p[1].as_f64().expect("latitude is a number");
                    (lon, lat)
                })
                .collect();
            Ring::new(points)
        })
        .collect()
}

/// Extract every ring from one GeoJSON feature's geometry, `Polygon` or
/// `MultiPolygon` (the 110m land dataset is all `Polygon` today; the
/// `MultiPolygon` arm is future-proofing against a coarser or finer Natural
/// Earth release that isn't).
fn rings_from_geometry(geometry: &Value) -> Vec<Ring> {
    let geometry_type = geometry["type"]
        .as_str()
        .expect("geometry has a type string");
    match geometry_type {
        "Polygon" => rings_from_polygon_coordinates(&geometry["coordinates"]),
        "MultiPolygon" => geometry["coordinates"]
            .as_array()
            .expect("MultiPolygon coordinates is a JSON array of polygons")
            .iter()
            .flat_map(rings_from_polygon_coordinates)
            .collect(),
        other => panic!("unsupported geometry type in Earth mask data: {other}"),
    }
}

/// Load every ring (outer and hole, of every feature) from the committed
/// GeoJSON `FeatureCollection`, in file order — pure, deterministic
/// arithmetic over committed bytes.
fn load_rings() -> Vec<Ring> {
    let raw = std::fs::read_to_string(DATA_PATH)
        .unwrap_or_else(|e| panic!("read Earth mask data at {DATA_PATH}: {e}"));
    let doc: Value = serde_json::from_str(&raw).expect("Earth mask data is valid JSON");
    doc["features"]
        .as_array()
        .expect("FeatureCollection has a features array")
        .iter()
        .flat_map(|feature| rings_from_geometry(&feature["geometry"]))
        .collect()
}

/// Normalize a longitude to `[-180, 180)`. `Geosphere::coord` returns
/// longitude in `(-180, 180]`, so the only value this ever adjusts is the
/// single edge case `180.0`.
fn normalize_lon(lon: f64) -> f64 {
    if lon >= 180.0 { lon - 360.0 } else { lon }
}

/// Whether `(lon, lat)` (already-normalized degrees) falls on land: the
/// even-odd parity XORed across every ring of every feature (module doc).
fn is_land(rings: &[Ring], lon: f64, lat: f64) -> bool {
    rings.iter().fold(false, |land, ring| {
        land ^ ring.contains_by_ray_cast(lon, lat)
    })
}

fn main() {
    let rings = load_rings();
    let geo = Geosphere::new(GLOBE_LEVEL);
    let mut out = String::from("cell,land\n");
    for cell in geo.cells() {
        let coord = geo.coord(cell);
        let lon = normalize_lon(coord.longitude);
        let land = is_land(&rings, lon, coord.latitude);
        out.push_str(&format!("{},{}\n", cell.0, land as u8));
    }
    print!("{out}");
}

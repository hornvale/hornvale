//! Site facts: the local, nameable features of a settled cell (The
//! Shibboleth).
//!
//! Real toponymy names a place for what is *there* — a river, a coast, a
//! closed basin, high ground. These are the elements that make a name both
//! translatable and distinguishing, and they are the ones Hornvale's
//! settlements were previously denied: a name carried its biome and a random
//! stem, so the stem did all the distinguishing and none of the meaning.

use crate::{GeneratedTerrain, WaterKind};
use hornvale_kernel::CellId;

/// A nameable water feature at a cell.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Hydrology {
    /// A watercourse runs here.
    River,
    /// A lesser watercourse.
    Creek,
    /// The cell touches the sea.
    Coast,
    /// A closed basin, where water gathers and does not leave.
    Basin,
    /// No nameable water.
    Dry,
}

/// The relief a settlement sits at, relative to the land around it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Relief {
    /// Ground standing above its neighbours.
    Upland,
    /// Ground lying below its neighbours.
    Lowland,
    /// Neither notably high nor low.
    Level,
}

/// Flow accumulation at or above which a watercourse is a river rather than a
/// creek. The same saturating scale the exotic damp term uses; a settlement on
/// a continental drainage is on a river, one on a headwater is on a creek.
/// type-audit: bare-ok(count)
const RIVER_DRAINAGE: f64 = 24.0;

/// Flow accumulation at or above which a cell has a nameable watercourse.
/// type-audit: bare-ok(count)
const CREEK_DRAINAGE: f64 = 4.0;

/// The relief difference from the neighbourhood mean, in metres, at which a
/// cell reads as high or low ground rather than level.
/// type-audit: bare-ok(count)
const RELIEF_M: f64 = 120.0;

/// The nameable water at `cell`. Coast outranks a watercourse: a river mouth
/// on the sea is named for the sea, which is how coastal toponymy works.
pub fn hydrology_at(terrain: &GeneratedTerrain, cell: CellId) -> Hydrology {
    let g = terrain.globe();
    let geo = terrain.geosphere();
    if geo.neighbors(cell).iter().any(|n| terrain.is_ocean(*n)) {
        return Hydrology::Coast;
    }
    if *g.endorheic.get(cell) {
        return Hydrology::Basin;
    }
    let drainage = *g.drainage.get(cell);
    if *g.water_kind.get(cell) == WaterKind::River || drainage >= RIVER_DRAINAGE {
        return Hydrology::River;
    }
    if drainage >= CREEK_DRAINAGE {
        return Hydrology::Creek;
    }
    Hydrology::Dry
}

/// The relief at `cell`, measured against the mean of its own neighbours —
/// relative, because "high ground" means high *for here*, not high on the
/// globe.
pub fn relief_at(terrain: &GeneratedTerrain, cell: CellId) -> Relief {
    let g = terrain.globe();
    let geo = terrain.geosphere();
    let here = g.elevation.get(cell).get();
    let ns = geo.neighbors(cell);
    if ns.is_empty() {
        return Relief::Level;
    }
    let mean: f64 = ns.iter().map(|n| g.elevation.get(*n).get()).sum::<f64>() / ns.len() as f64;
    if here - mean >= RELIEF_M {
        Relief::Upland
    } else if mean - here >= RELIEF_M {
        Relief::Lowland
    } else {
        Relief::Level
    }
}

impl Hydrology {
    /// The registry key for this feature's concept; `None` for [`Hydrology::Dry`],
    /// which names nothing.
    /// type-audit: bare-ok(identifier-text)
    pub fn concept_name(self) -> Option<&'static str> {
        match self {
            Hydrology::River => Some("river"),
            Hydrology::Creek => Some("creek"),
            Hydrology::Coast => Some("coast"),
            Hydrology::Basin => Some("basin"),
            Hydrology::Dry => None,
        }
    }
}

impl Relief {
    /// The registry key for this relief's concept; `None` for [`Relief::Level`].
    /// type-audit: bare-ok(identifier-text)
    pub fn concept_name(self) -> Option<&'static str> {
        match self {
            Relief::Upland => Some("upland"),
            Relief::Lowland => Some("lowland"),
            Relief::Level => None,
        }
    }
}

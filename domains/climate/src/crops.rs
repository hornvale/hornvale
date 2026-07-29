//! What grows here: the staple a farming people would name a place for.
//!
//! A crop is a climate fact — a band of temperature and moisture on arable
//! ground — so it lives beside the biome that shares those inputs. Whether a
//! people has any *use* for it is not a climate question: exposure gates the
//! word, so a forager or a herder never learns the name of a grain, and only
//! a farming people names its home for the barley.

use crate::facets::Formation;
use hornvale_kernel::Temperature;

/// A staple crop, as a place would be named for it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Crop {
    /// A cold-tolerant grain.
    Barley,
    /// The temperate grain.
    Wheat,
    /// A hot, wet-ground grain.
    Rice,
    /// A hot, dry-ground grain.
    Millet,
    /// A cool, wet-ground root staple.
    Tuber,
    /// A warm, dry-ground fruiting vine.
    Vine,
}

impl Crop {
    /// The registry key for this crop's concept.
    /// type-audit: bare-ok(identifier-text)
    pub fn concept_name(self) -> &'static str {
        match self {
            Crop::Barley => "barley",
            Crop::Wheat => "wheat",
            Crop::Rice => "rice",
            Crop::Millet => "millet",
            Crop::Tuber => "tuber",
            Crop::Vine => "vine",
        }
    }

    /// Every crop, in declaration order.
    pub fn catalog() -> &'static [Crop] {
        &[
            Crop::Barley,
            Crop::Wheat,
            Crop::Rice,
            Crop::Millet,
            Crop::Tuber,
            Crop::Vine,
        ]
    }
}

/// Whether a formation is ground a crop could be raised on at all. Ice, bare
/// rock, and every marine formation are not.
fn arable(formation: Formation) -> bool {
    matches!(
        formation,
        Formation::TemperateGrassland
            | Formation::Savanna
            | Formation::TemperateForest
            | Formation::TemperateRainforest
            | Formation::TropicalSeasonalForest
            | Formation::TropicalRainforest
            | Formation::Shrubland
            | Formation::Taiga
            | Formation::Desert
    )
}

/// The staple that grows best at a cell, or `None` where nothing does.
///
/// Bands are the conventional ones: barley tolerates cold where wheat will
/// not, rice wants heat and standing water, millet heat without it, tubers
/// cool wet ground, and the vine warm dry ground. Deserts grow nothing
/// without a river, which the site facts report separately.
/// type-audit: bare-ok(ratio: moisture)
pub fn crop_at(formation: Formation, mean_c: Temperature, moisture: f64) -> Option<Crop> {
    if !arable(formation) {
        return None;
    }
    let t = mean_c.get();
    let wet = moisture >= 0.55;
    match (t, wet) {
        (t, _) if t < -5.0 => None,
        (t, true) if t < 8.0 => Some(Crop::Tuber),
        (t, false) if t < 8.0 => Some(Crop::Barley),
        (t, true) if t < 20.0 => Some(Crop::Wheat),
        (t, false) if t < 20.0 => Some(Crop::Vine),
        (_, true) => Some(Crop::Rice),
        (_, false) => Some(Crop::Millet),
    }
}

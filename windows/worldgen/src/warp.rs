//! The warp made legible: the words a walker is told about the continuous
//! macro fields underfoot, and the thresholds that cut them (The Warp,
//! spec §4). **One implementation per sign** (spec §4.4; the campaign's
//! close ratifies a record for this rule): `windows/locale`'s room sentence
//! and `windows/vessel`'s warp clause render these enums' words, and
//! `windows/lab`'s legibility instrument tabulates the same enums'
//! discriminants, so a threshold can never have a second copy — The Rill
//! named a second copy of a partition as the failure mode.
//!
//! Nothing here draws. Every function is a pure read of an existing field
//! (`RockClass`, `FieldPack.slope`, `MicroField.wetness`): no stream label,
//! no ledger fact, no epoch (spec §4.5).

use hornvale_kernel::math;
use hornvale_terrain::GORGE_SLOPE;
use hornvale_terrain::lithology::RockClass;

/// The cut every micro-habitat word makes on its `[-1, 1]` axis — the
/// value `grammar.rs`'s four axes have always used, now held once.
/// type-audit: bare-ok(ratio)
/// plumb: universal(an authored rendering threshold on a [-1,1] descriptor axis, the same in every world; The Rill's grounded-wetness model was measured against words cut here)
pub const MICRO_WORD_THRESHOLD: f64 = 0.33;

/// Below this saturated slope the ground reads *level*; on
/// `tanh(|slope| / GORGE_SLOPE)`, overhang's own saturation.
/// type-audit: bare-ok(ratio)
/// plumb: universal(an authored rendering threshold on the saturated slope register overhang/hollow already reads, fixed across every world)
pub const STEEP_LO: f64 = 0.25;

/// At or above this saturated slope the ground reads *steep*.
/// type-audit: bare-ok(ratio)
/// plumb: universal(an authored rendering threshold on the saturated slope register overhang/hollow already reads, fixed across every world)
pub const STEEP_HI: f64 = 0.60;

/// How hard the ground tilts, as the walker is told it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Steepness {
    /// Saturated slope below [`STEEP_LO`].
    Level,
    /// Between the two cuts.
    Sloping,
    /// At or above [`STEEP_HI`].
    Steep,
}

/// The wetness word's three values on `MicroField.wetness`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Wetness {
    /// Axis below `-MICRO_WORD_THRESHOLD`.
    Dry,
    /// Within the threshold either side of zero — no word rendered.
    Mid,
    /// Axis above `MICRO_WORD_THRESHOLD`.
    Damp,
}

/// The rock underfoot, as a walker is told it. Exhaustive — a new
/// `RockClass` variant must choose its word here, and the compiler says so.
/// type-audit: bare-ok(prose: return)
pub fn rock_word(class: RockClass) -> &'static str {
    match class {
        RockClass::Granite => "grey granite",
        RockClass::Gabbro => "dark gabbro",
        RockClass::Basalt => "black basalt",
        RockClass::Andesite => "grey-brown andesite",
        RockClass::Rhyolite => "pale rhyolite",
        RockClass::Sandstone => "red sandstone",
        RockClass::Shale => "crumbling shale",
        RockClass::Conglomerate => "pebbled conglomerate",
        RockClass::Evaporite => "salt-white evaporite",
        RockClass::Chert => "flinty chert",
        RockClass::Ironstone => "rust-red ironstone",
        RockClass::ReefLimestone => "pale limestone",
        RockClass::Coal => "black coal measures",
        RockClass::Slate => "blue-grey slate",
        RockClass::Schist => "glittering schist",
        RockClass::Gneiss => "banded gneiss",
        RockClass::Marble => "white marble",
        RockClass::Quartzite => "hard quartzite",
        RockClass::Alluvium => "soft river silt",
    }
}

/// Cut a raw slope (`FieldPack.slope`, metres of fall per radian, signed)
/// into a word, through the SAME saturation overhang/hollow's recipe uses.
/// type-audit: bare-ok(ratio: slope)
pub fn steepness_sign(slope: f64) -> Steepness {
    let saturated = math::tanh(slope.abs() / GORGE_SLOPE);
    if saturated >= STEEP_HI {
        Steepness::Steep
    } else if saturated >= STEEP_LO {
        Steepness::Sloping
    } else {
        Steepness::Level
    }
}

/// The steepness word, spliced mid-sentence.
/// type-audit: bare-ok(prose: return)
pub fn steepness_word(s: Steepness) -> &'static str {
    match s {
        Steepness::Level => "level ground",
        Steepness::Sloping => "sloping ground",
        Steepness::Steep => "a steep pitch",
    }
}

/// Cut the wetness axis at [`MICRO_WORD_THRESHOLD`], strictly — the exact
/// comparison `grammar.rs::land_micro_habitat` has always made.
/// type-audit: bare-ok(ratio: axis)
pub fn wetness_sign(axis: f64) -> Wetness {
    if axis > MICRO_WORD_THRESHOLD {
        Wetness::Damp
    } else if axis < -MICRO_WORD_THRESHOLD {
        Wetness::Dry
    } else {
        Wetness::Mid
    }
}

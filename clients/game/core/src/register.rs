//! The glyph register: every character the client may draw, and what it
//! means. ONE table, because the alternative is what shipped before The
//! Legend — each pane allocating privately, and `.`, `+` and `#` each
//! meaning two or three different things with nothing to notice.
//!
//! The allocation rule (spec §2): a glyph carries ORDER (ink ascends with
//! the quantity) or IDENTITY (the character is the referent's initial),
//! never an arbitrary category. Colour carries category and MAY FAIL, so
//! nothing a reader must trust lives only there.

/// Which population of referents a glyph belongs to. A character belongs to
/// exactly one, which is what makes double-binding detectable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Population {
    /// The possession's own cell. (lexicon: a terminal grid cell, an area, same sense as this crate's own cell module — never a mesh vertex.)
    Observer,
    /// Walk-band impedance: how hard this ground is to cross.
    Relief,
    /// Globe-scale elevation: how high this ground stands.
    Elevation,
    /// Surface water, by kind.
    Water,
    /// Built structure — wall, threshold.
    Structure,
    /// A living thing, drawn as its noun's initial.
    Creature,
    /// A discovered point site — settlement, cave mouth.
    PointSite,
    /// Interface furniture that is not part of the world.
    Chrome,
    /// RESERVED for Delving campaign 2. Deliberately unpopulated.
    Subterranean,
}

/// One character's single meaning.
#[derive(Debug)]
pub struct Binding {
    /// The character drawn.
    pub glyph: char,
    /// The population it belongs to.
    pub population: Population,
    /// What it means — for the legend, and for the double-binding panic.
    pub means: &'static str,
}

/// Every character the client may draw. Adding a row is how a campaign
/// claims a mark; the guard in `tests/register.rs` refuses a second claim on
/// a character already spoken for.
///
/// `Creature` is NOT enumerated: a creature draws its noun's initial, so its
/// codespace is `a-z`/`A-Z` by RULE rather than by row. Those letters are
/// therefore unavailable to every other population, which is exactly the
/// constraint the rule intends.
pub const REGISTER: &[Binding] = &[
    Binding {
        glyph: '@',
        population: Population::Observer,
        means: "you",
    },
    Binding {
        glyph: '>',
        population: Population::Chrome,
        means: "the command prompt",
    },
];

/// The binding for `glyph`, if the register claims it.
pub fn binding_of(glyph: char) -> Option<&'static Binding> {
    REGISTER.iter().find(|b| b.glyph == glyph)
}

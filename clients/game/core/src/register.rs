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
    /// A point site on the world map: a discovered settlement or cave
    /// mouth, or a landform (Task 7 widens this from "settlement, cave
    /// mouth" — a volcano, waterfall or river delta is the same kind of
    /// referent, a fixed point worth marking, whether or not it happens to
    /// be discovery-gated).
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
    // The Legend, Task 6: the world map's terrain vocabulary. Water
    // outranks elevation at a given vertex (`plate::glyph_and_color_for`),
    // so `WaterKind::DryLand` claims no glyph of its own here — only the
    // wet classes and the elevation ladder that shows through dry land do.
    Binding {
        glyph: '~',
        population: Population::Water,
        means: "ocean",
    },
    Binding {
        glyph: '=',
        population: Population::Water,
        means: "salt basin",
    },
    Binding {
        glyph: '"',
        population: Population::Water,
        means: "river",
    },
    // The elevation ladder (`hornvale_scene::RELIEF_LEGEND`'s six bands),
    // drawn only where the water class is dry land. Ink ascends with the
    // band (spec §2's allocation rule): a blank glyph is deliberate at the
    // floor, not an omission.
    Binding {
        glyph: ' ',
        population: Population::Elevation,
        means: "abyss",
    },
    Binding {
        glyph: '`',
        population: Population::Elevation,
        means: "shelf",
    },
    Binding {
        glyph: ',',
        population: Population::Elevation,
        means: "lowland",
    },
    Binding {
        glyph: ';',
        population: Population::Elevation,
        means: "upland",
    },
    // `means` is deliberately broad ("high or steep ground", not just
    // "highland"): globe-scale highland and the sim's own walk-band
    // impedance-4 mark are the same concept at two scales, and a later
    // task porting that ladder into this register should claim this row
    // rather than mint a second glyph for it.
    Binding {
        glyph: '^',
        population: Population::Elevation,
        means: "high or steep ground",
    },
    Binding {
        glyph: '%',
        population: Population::Elevation,
        means: "alpine",
    },
    // Task 7: point sites (Nathan's own glyph assignments, `progress.md`
    // 2026-08-30). `o`/`O` move off the letter's apparent creature-codespace
    // collision by Ruling AG — the world map never draws a creature's noun
    // initial (it draws the generic agent mark instead), so `o`/`O` are free
    // here even though the register's own rule reserves `a`-`z`/`A`-`Z` for
    // `Population::Creature` everywhere else.
    Binding {
        glyph: '*',
        population: Population::PointSite,
        means: "cave mouth",
    },
    Binding {
        glyph: 'o',
        population: Population::PointSite,
        means: "settlement",
    },
    Binding {
        glyph: 'O',
        population: Population::PointSite,
        means: "major settlement",
    },
    // Landforms (Task 7): a volcano is discovery-gated like a settlement or
    // cave; a waterfall and a river delta draw unconditionally, ground
    // truth like the relief/water ladders above — see
    // `hornvale_game::plate::draw_feature_layer`'s own doc for why that
    // split is deliberate. All three are still `PointSite`s here: this
    // table classifies WHAT a glyph refers to, not whether it happens to be
    // gated.
    Binding {
        glyph: '!',
        population: Population::PointSite,
        means: "volcano",
    },
    Binding {
        glyph: '|',
        population: Population::PointSite,
        means: "waterfall",
    },
    Binding {
        glyph: ':',
        population: Population::PointSite,
        means: "river delta",
    },
];

/// The binding for `glyph`, if the register claims it.
pub fn binding_of(glyph: char) -> Option<&'static Binding> {
    REGISTER.iter().find(|b| b.glyph == glyph)
}

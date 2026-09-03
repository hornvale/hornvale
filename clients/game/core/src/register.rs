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
    /// The GENERIC creature mark — every living thing at once, regardless of
    /// species, drawn where per-species initials are not (the world map's
    /// perception overlay, and the underground level's own resident marks).
    /// Deliberately distinct from [`Population::Creature`]: that population
    /// is a codespace rule (a-z/A-Z, by noun initial) and this is a single
    /// ordinary row, because a generic mark is not a noun initial and so is
    /// never covered by the rule that leaves `Creature` unenumerated. See
    /// [`REGISTER`]'s own row for the history of that gap.
    Agent,
    /// A point site on the world map: a settlement, a cave mouth, or a
    /// landform (Task 7 widens this from "settlement, cave mouth" — a
    /// volcano or waterfall is the same kind of referent, a fixed point
    /// worth marking). A river delta was a third landform here briefly and
    /// was removed in fix round 1 — see [`REGISTER`]'s own doc on the `:`
    /// collision that forced it out. The Prospect, Task 8 adds the placed
    /// EXOTIC site, the third of `hornvale_vessel`'s three `SiteKind`s — the
    /// population's membership rule is unchanged by it. **A settlement or
    /// cave mouth or exotic site draws whether or not it has been
    /// discovered** (The Prospect, Gate A ungating): only a volcano stays
    /// discovery-gated, because it is the one member also individuated as a
    /// `hornvale_terrain::landscape::FeatureClass` extent, and only its own
    /// PROPER NAME stays gated (a separate surface, the cursor readout —
    /// `windows/worldgen::resolve_chain_at` — never this one).
    PointSite,
    /// Interface furniture that is not part of the world.
    Chrome,
    /// A furnishing anchor within a chamber — a hearth, a bed, an alcove,
    /// and so on. Each carries a `hornvale_kernel::KindId`; the roster is
    /// `domains/thing`'s `THING_KINDS`.
    /// One glyph covers every kind (fix round 1, The Legend Task 10): see
    /// [`REGISTER`]'s own furnishing row for why a glyph per kind was
    /// rejected. Do not read a future furnishing kind as license to add a
    /// second row here — the whole point of this population is that it
    /// stays at one.
    Furnishing,
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
/// `Creature` is NOT enumerated **in the noun-initial direction only**: a
/// creature drawn by its own noun's initial owns the whole `a`-`z`/`A`-`Z`
/// codespace BY RULE rather than by row, and those letters are therefore
/// unavailable to every other population. That is the one thing this
/// omission says. It does NOT say every mark for a living thing goes
/// unclaimed — the world map's GENERIC agent mark (`&`, below) draws for
/// every creature regardless of species (Ruling AG), is not a noun initial,
/// and is claimed by an ordinary row like any other glyph. The gap this
/// paragraph used to leave open — a mark that is agent-shaped but not a
/// letter falls through BOTH the noun-initial rule and the "must be
/// enumerated" default, because nothing enumerated it and nothing was
/// enumerating it on purpose — is exactly how `&` went unclaimed here while
/// live in three call sites (`plate.rs::AGENT_GLYPH`, `level.rs::MARK_GLYPH`,
/// `windows/scene/src/surrounds_ascii.rs`'s own agent glyph) for long enough
/// to make a picked-glyph ruling ("`&` is free") confidently wrong (The
/// Prospect, fix round). The row below closes it.
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
    // The Prospect, fix round: the generic agent mark. Claimed here, not
    // when `&` was first written (`plate.rs::AGENT_GLYPH`, The Quadrat Task
    // 6) or reused (`level.rs::MARK_GLYPH`, The Gallery) — both predate this
    // row. Two client-side call sites plus one sim-side one
    // (`windows/scene/src/surrounds_ascii.rs`'s own agent glyph) all draw
    // `&` for the identical fact ("a living thing is here, species
    // unspecified") and none of them ever registered it, which is what let
    // a later campaign ask "is `&` free?" and get a wrong yes from this
    // table alone — see [`Population::Agent`]'s own doc.
    Binding {
        glyph: '&',
        population: Population::Agent,
        means: "a creature, regardless of species (the generic mark)",
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
    // The Legend, Task 8: the walk band's impedance ladder
    // (`chart.rs::impedance_glyph`, ported from
    // `windows/scene/src/surrounds_ascii.rs`). This is the "later task"
    // the comment above `^` was written for: `^` (impedance rung 4) is
    // NOT re-added here, its existing Elevation-population row above is
    // shared as-is, per that comment's own instruction.
    //
    // `.` is claimed here too, even though `plan.rs` also draws `.` for a
    // chamber floor — deliberately ONE row, `means` broadened the same way
    // `^`'s was: ordinary ground at walk-scale relief-2 and an ordinary
    // chamber floor are the same concept at two scales (spec's Ruling K).
    // `plan.rs` does not mint its own binding for it.
    Binding {
        glyph: '_',
        population: Population::Relief,
        means: "open, easy going",
    },
    Binding {
        glyph: '.',
        population: Population::Relief,
        means: "ordinary traversable ground",
    },
    Binding {
        glyph: ':',
        population: Population::Relief,
        means: "moderately rough going",
    },
    Binding {
        glyph: 'A',
        population: Population::Relief,
        means: "dense or steep going",
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
    // The Prospect, Task 8: the placed EXOTIC site — a fungal canopy, a
    // mineral-crystal flat, a place whose biota is found nowhere else
    // (`hornvale_locale`'s strangeness budget, 103 of them on seed 42).
    // Until that task the world map had no mark for one at all, so the
    // whole tier was generated and undrawable.
    //
    // `$` is a PICKED mark, not a derived one, and it is picked the same way
    // `!` and `|` were: the register's allocation rule offers ORDER or
    // IDENTITY, an exotic site has no ordinal to carry, and its noun's
    // initial (`s`, "site" — `hornvale_vessel`'s own `site_clause`) is in
    // the `a`-`z`/`A`-`Z` codespace `Population::Creature` owns by rule.
    // That leaves an evocative choice among unclaimed punctuation, and `$`
    // is the one mark in it that already reads, by long convention, as
    // "something here is worth the trip" — which is exactly the invitation
    // this campaign exists to put on the map. Nathan owns glyph assignments
    // (Task 7's `o`/`O`/`*` were his); this is a reviewable stand-in, and
    // moving it costs one line here and one in `plate.rs`.
    //
    // `means` is the bare noun phrase every sibling point-site row uses
    // ("cave mouth", "settlement", "volcano"), and it is the same noun
    // `hornvale_vessel::site::SiteKind::Exotic` and `hornvale locale
    // --strange` already give the thing. Its LENGTH is load-bearing in one
    // place: `bin/examples/specimen_sheet.rs` packs this whole table into a
    // fixed 80x24 sheet and asserts the height exactly, so a longer phrase
    // here spills the sheet — see that file's own packing comment.
    Binding {
        glyph: '$',
        population: Population::PointSite,
        means: "exotic site",
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
    // Landforms (Task 7): a volcano stays discovery-gated (it is also
    // individuated as a `FeatureClass::Volcano` extent, and that is the
    // SAME discovery fact `Driver::update_discovery` already records for any
    // landscape feature); a waterfall draws unconditionally, ground truth
    // like the relief/water ladders above — see
    // `hornvale_game::plate::draw_feature_layer`'s own doc for why that
    // split is deliberate. **This paragraph used to say "a volcano is
    // discovery-gated like a settlement or cave" — The Prospect's Gate A
    // ungating (fix round) made that false: a settlement, cave mouth or
    // exotic site now draws whether or not it has been discovered, exactly
    // like a waterfall, and a volcano is now the ONE point-site kind still
    // gated.** All four are still `PointSite`s here regardless: this table
    // classifies WHAT a glyph refers to, not whether it happens to be
    // gated.
    //
    // **A river delta was a third landform here, and fix round 1 removed
    // it outright** rather than re-picking its glyph: `:` collided with
    // `windows/scene/src/surrounds_ascii.rs`'s impedance band 3, and that
    // ladder cannot move (`the_shape_matches_the_sims_own_ascii_render`
    // pins it to the sim). Unlike `^` (globe-scale highland and walk-scale
    // steep going are genuinely the same concept at two scales, so sharing
    // one glyph is a merge, not a collision), a river delta and moderately
    // rough going are unrelated referents that cannot share a binding. The
    // campaign's own tier spike also measured that a third point-marker
    // kind read as noise rather than invitation — dropping the least
    // evocative of the three landforms serves the map's stated purpose,
    // not just the collision.
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
    // Task 9: the chamber-band floor plan's structure glyphs
    // (`plan.rs::glyph_of`), claimed here for the first time — `#` is the
    // very glyph whose collision with the settlement marker started this
    // campaign's whole collision thread. `.` is not re-claimed: it already
    // has a Relief-population row above, broadened by Ruling K to cover
    // "an ordinary chamber floor" too, and `@`/`you` is `Population::
    // Observer` already.
    Binding {
        glyph: '#',
        population: Population::Structure,
        means: "wall",
    },
    Binding {
        glyph: '+',
        population: Population::Structure,
        means: "threshold",
    },
    // The Legend, Task 10, fix round 1: `vessel/plan/v1` grew a
    // `"furnishing"` mark kind (a hearth, a bed, an alcove, …), and Task 10's
    // own commit shipped it all the way to the wire and never drew it — the
    // mark reached the client and `plan.rs::draw_mark`'s `match` still only
    // knew `"agent"`, so a furnishing fell into the settlement-style
    // structural no-op and redrew the ordinary floor underneath it.
    //
    // The fix is ONE glyph for every furnishing kind, not one row per
    // thing-kind (16 rostered and rising with Delving 2's own furniture).
    // `CLIENT-glyphs-22-rejected` already settled the general shape of this
    // question for biome glyphs: a nominal mark per kind does not
    // self-legend, and a reader would need a permanent key just to tell a
    // bed from an altar. Letters are unavailable here for a second reason
    // this campaign is specific to: `Population::Creature` owns the whole
    // `a`-`z`/`A`-`Z` codespace on the chamber-band plan (unlike the world
    // map's `o`/`O` carve-out — Ruling AG — creature initials ARE drawn on
    // this band), so a hearth drawn as `h` would collide with a human or
    // hobgoblin standing in the same room. And it matches Nathan's own
    // stated division of labour: the glyph says "something here is worth
    // attention," `examine` (the mark's own `datum`) says what it is.
    Binding {
        glyph: '?',
        population: Population::Furnishing,
        means: "a furnishing — a hearth, a bed, and every other kind alike",
    },
];

/// The binding for `glyph`, if the register claims it.
pub fn binding_of(glyph: char) -> Option<&'static Binding> {
    REGISTER.iter().find(|b| b.glyph == glyph)
}

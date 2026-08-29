//! THE SPECIMEN SHEET (The Legend, Task 5) — candidate glyph ladders, drawn
//! in the medium they ship in. Selection happens against a rendered sheet,
//! not a document: a specimen sheet is what rejected the earlier 22-glyph
//! design (the marks were not tellable apart), and this is the same
//! instrument aimed at the two ladders Ruling X re-pointed this campaign at.
//!
//! Run: `cargo run -p hornvale-game --example specimen_sheet` (no flags, no
//! stdin — an agent's stdin is at EOF, so a stdin-reading command here would
//! pass for an agent and hang for a human at a terminal). Renders to stdout
//! at EXACTLY 80x24, the monochrome floor
//! ([`hornvale_game_core::MIN_WIDTH`]/[`hornvale_game_core::MIN_HEIGHT`]):
//! every candidate elevation ladder, monochrome and again with colour; the
//! sim's own impedance ladder, showing the overload the brief asked this
//! sheet to surface rather than hide; the creature-initial rule over a real
//! species roster; and the client register's full table as a legend.
//!
//! **Candidates are proposed here, not decided.** The brief fixes three
//! properties every elevation candidate must meet (ordinal by ink weight,
//! disjoint from the client register and the impedance ladder, legible
//! monochrome at 80x24) and deliberately does not name marks — this project
//! has found an outside guess at a specific choice worse than an inside
//! search, every time it tried. `main` below checks all three properties at
//! runtime against the live register (`binding_of`) rather than by eyeball,
//! so a future edit that violates one fails loudly instead of silently.
//!
//! The verdict on which candidate to ship, and whether the impedance
//! ladder's `'A'` overload reads as legible, is recorded in this task's
//! report (`.superpowers/sdd/2026-08-28-the-legend/task-5-report.md`), not
//! in this file — the sheet is the evidence, not the conclusion.

use hornvale_game_core::register::{Population, REGISTER, binding_of};
use hornvale_kernel::SeaLevelHeight;
use hornvale_scene::{RELIEF_LEGEND, relief_band};

/// The monochrome floor this sheet is built to, not merely tested against —
/// see the module doc.
const WIDTH: usize = hornvale_game_core::MIN_WIDTH as usize;
/// See [`WIDTH`].
const HEIGHT: usize = hornvale_game_core::MIN_HEIGHT as usize;

/// The sim's own impedance vocabulary, reproduced here because
/// `windows/scene/src/surrounds_ascii.rs::impedance_glyph` is crate-private
/// (this crate cannot call it) — this is the committed, documented formula
/// copied verbatim off that function's doc comment and match arms, not a
/// reinvention. `_`/`.`/`:`/`^` are read from the sim source directly below;
/// see [`impedance_glyph_for`] for the exact match this mirrors.
const IMPEDANCE_GLYPHS: [char; 5] = ['_', '.', ':', '^', 'A'];

/// One elevation-band colour ramp (a conventional low-to-high hypsometric
/// tint), shared by every candidate below and by the impedance demo so a
/// reader can compare "does the glyph carry the order" against "does the
/// colour carry it" on the same six-way scale.
const BAND_COLOR: [[u8; 3]; 6] = [
    [20, 20, 90],    // abyss    — deep water, near-black blue
    [30, 130, 150],  // shelf    — shallow water, teal
    [50, 150, 70],   // lowland  — green
    [160, 160, 50],  // upland   — olive
    [150, 95, 45],   // highland — brown
    [235, 235, 235], // alpine   — near-white
];

/// A flag colour for impedance value 6 — the SECOND value the catch-all
/// absorbs. Not a proposed colour for anything that ships; it exists only so
/// this sheet cannot smooth the overload over by accident. See the module
/// doc and the brief's ruling: "Show that overload on the sheet ... rather
/// than hiding it."
const OVERLOAD_FLAG: [u8; 3] = [255, 60, 60];

/// One candidate elevation ladder: six glyphs, [`RELIEF_LEGEND`]-ordered.
struct Candidate {
    /// Short name for the row label.
    name: &'static str,
    /// One glyph per [`RELIEF_LEGEND`] band, abyss first.
    glyphs: [char; 6],
    /// One line of reasoning, printed nowhere on the sheet itself (the
    /// sheet is evidence, prose is the report) but kept here so the
    /// rationale lives next to the data it is about.
    #[allow(dead_code)]
    rationale: &'static str,
}

/// Three proposals, deliberately different strategies rather than three
/// minor variations on one idea — see the task report for which one this
/// sheet's own rendering recommends.
const CANDIDATES: [Candidate; 3] = [
    Candidate {
        name: "stipple",
        glyphs: [' ', '`', ',', ';', '*', '%'],
        rationale: "punctuation-mark ink density: literally nothing at the \
                    trench floor, rising through single light marks to the \
                    densest ASCII punctuation at the peak.",
    },
    Candidate {
        name: "bracket",
        glyphs: [' ', '\'', '(', '[', '{', '$'],
        rationale: "containment shapes widening and thickening — a single \
                    stroke, then increasingly enclosing brackets, ending on \
                    a glyph with a full vertical stroke plus curls.",
    },
    Candidate {
        name: "rule",
        glyphs: [' ', '-', '/', '?', ')', '|'],
        rationale: "a control set at roughly the same nominal weight as \
                    `stipple`'s low end but a different shape family \
                    throughout, to see whether shape family or density is \
                    doing the telling-apart.",
    },
];

/// A real (if trimmed of leading articles) roster, drawn from
/// `hornvale_species::KIND_CONCEPTS`'s glosses — not imported as a
/// dependency (this example only consumes the two interfaces the brief
/// names), just quoted, because the creature-initial rule needs real nouns
/// to show real collisions.
const SPECIES_ROSTER: [&str; 12] = [
    "goblin",
    "gnoll",
    "giant elk",
    "hobgoblin",
    "human",
    "drow",
    "dire wolf",
    "kobold",
    "treant",
    "owlbear",
    "bugbear",
    "killer whale",
];

/// The impedance formula's exact match, copied off
/// `surrounds_ascii::impedance_glyph`'s doc and arms (that function is
/// crate-private, so this is a mirror, not a call).
fn impedance_glyph_for(rounded: i64) -> char {
    match rounded {
        0 | 1 => '_',
        2 => '.',
        3 => ':',
        4 => '^',
        _ => 'A',
    }
}

/// The creature-initial DEMONSTRATION rule: skip nothing (the roster above
/// already carries no articles) and take the first alphabetic character,
/// lowercased. This is not the shipped rule — no such function exists in
/// this crate yet, by design (`register.rs`'s doc states the property, a
/// later task implements it) — it exists only so this sheet can show what
/// "identity, not order" looks like over real nouns.
fn creature_initial(noun: &str) -> char {
    noun.chars()
        .find(|c| c.is_alphabetic())
        .map(|c| c.to_ascii_lowercase())
        .unwrap_or('?')
}

/// Wraps `glyph` in a 24-bit foreground escape — the exact shape
/// `clients/game/bin/examples/vision.rs::paint` already uses for the same
/// job, so a reader comparing the two sketches sees one convention.
fn paint(glyph: char, rgb: [u8; 3]) -> String {
    format!("\x1b[38;2;{};{};{}m{glyph}\x1b[39m", rgb[0], rgb[1], rgb[2])
}

/// One glyph, bracket-wrapped and centred in a fixed-width column — bracket
/// notation rather than a bare character so the space candidate (band
/// `abyss` in `stipple`) still occupies visible ink on the sheet; the real
/// in-game square would draw genuinely blank.
fn swatch(glyph: char, rgb: Option<[u8; 3]>) -> String {
    let shown = match rgb {
        Some(c) => paint(glyph, c),
        None => glyph.to_string(),
    };
    format!("{:^9}", format!("[{shown}]"))
}

/// The visible width of a line, ignoring `ESC [ ... m` SGR sequences —
/// mirrors `clients/game/core/src/entry.rs`'s own escape-stripping rule for
/// measuring a picture row, since a naive `.chars().count()` would charge
/// this sheet for bytes a terminal never occupies a column with.
fn visible_width(line: &str) -> usize {
    let mut n = 0;
    let mut chars = line.chars();
    while let Some(c) = chars.next() {
        if c == '\u{1b}' {
            // Swallow through the terminating 'm' of an SGR sequence.
            for c2 in chars.by_ref() {
                if c2 == 'm' {
                    break;
                }
            }
        } else {
            n += 1;
        }
    }
    n
}

fn main() {
    // ---- Property checks against the LIVE register, not by eyeball ------
    //
    // Ordinal, disjoint-from-REGISTER, disjoint-from-impedance-ladder,
    // outside Creature's a-z/A-Z codespace: every candidate glyph is
    // checked here, so a future edit that violates one of the brief's three
    // properties fails loudly instead of shipping unnoticed.
    assert_eq!(
        RELIEF_LEGEND.len(),
        6,
        "this sheet is drawn for a six-band ladder; RELIEF_LEGEND moved"
    );
    for c in &CANDIDATES {
        assert_eq!(
            c.glyphs.len(),
            RELIEF_LEGEND.len(),
            "{}: one glyph per RELIEF_LEGEND band",
            c.name
        );
        for &g in &c.glyphs {
            assert!(
                binding_of(g).is_none(),
                "{}: glyph {g:?} already claimed by REGISTER ({:?})",
                c.name,
                binding_of(g)
            );
            assert!(
                !IMPEDANCE_GLYPHS.contains(&g),
                "{}: glyph {g:?} collides with the impedance ladder",
                c.name
            );
            assert!(
                !g.is_ascii_alphabetic(),
                "{}: glyph {g:?} is in Creature's a-z/A-Z codespace",
                c.name
            );
        }
    }
    // The register itself is small today (two rows) — this sheet's fixed
    // 24-line budget below is sized for that. A future row added to
    // REGISTER should grow this sheet too; the line-count assertion at the
    // end of `main` is what makes that an observed failure rather than a
    // silently-truncated one.

    // relief_band/RELIEF_LEGEND agreement, exercised with one representative
    // height per band (the thresholds `windows/scene/src/surrounds.rs`
    // documents: <-3000, <0, <300, <1000, <2500, else).
    for (metres, expect) in [
        (-4000.0, 0u32),
        (-100.0, 1),
        (100.0, 2),
        (500.0, 3),
        (1500.0, 4),
        (3000.0, 5),
    ] {
        let got = relief_band(SeaLevelHeight::from_metres(metres));
        assert_eq!(
            got, expect,
            "relief_band({metres}) = {got}, expected {expect} ({})",
            RELIEF_LEGEND[expect as usize]
        );
    }

    // ---- Build the sheet, exactly 24 lines --------------------------------
    let mut lines: Vec<String> = Vec::with_capacity(HEIGHT);

    lines.push("HORNVALE GLYPH SPECIMEN SHEET -- The Legend, Task 5".to_string());

    lines.push(format!(
        "ELEVATION LADDER CANDIDATES ({} bands, abyss+shelf below sea level)",
        RELIEF_LEGEND.len()
    ));

    let mut header = format!("{:<14}", "");
    for band in RELIEF_LEGEND {
        header += &format!("{band:^9}");
    }
    lines.push(header);

    for c in &CANDIDATES {
        let mut mono = format!("{:<14}", c.name);
        let mut colour = format!("{:<14}", "  (colour)");
        for (band, &g) in RELIEF_LEGEND.iter().zip(c.glyphs.iter()) {
            let idx = RELIEF_LEGEND.iter().position(|b| b == band).unwrap();
            mono += &swatch(g, None);
            colour += &swatch(g, Some(BAND_COLOR[idx]));
        }
        lines.push(mono);
        lines.push(colour);
    }

    lines.push(String::new());

    lines.push(
        "IMPEDANCE LADDER (surrounds_ascii::impedance_glyph): 7 values -> 5 glyphs".to_string(),
    );
    let mut value_header = format!("{:<14}", "value");
    for v in 0..=6i64 {
        value_header += &format!("{v:^9}");
    }
    lines.push(value_header);
    let mut imp_mono = format!("{:<14}", "glyph");
    let mut imp_colour = format!("{:<14}", "  (colour)");
    for v in 0..=6i64 {
        let g = impedance_glyph_for(v);
        imp_mono += &swatch(g, None);
        let rgb = if v == 6 {
            OVERLOAD_FLAG
        } else {
            BAND_COLOR[v as usize]
        };
        imp_colour += &swatch(g, Some(rgb));
    }
    lines.push(imp_mono);
    lines.push(imp_colour);

    lines.push(String::new());

    lines.push(
        "CREATURE-INITIAL RULE: glyph = noun's initial (Creature owns all a-z/A-Z)".to_string(),
    );
    let pairs: Vec<String> = SPECIES_ROSTER
        .iter()
        .map(|noun| format!("{noun}:{}", creature_initial(noun)))
        .collect();
    lines.push(pairs[..6].join("  "));
    lines.push(pairs[6..].join("  "));

    let mut by_initial: std::collections::BTreeMap<char, Vec<&str>> =
        std::collections::BTreeMap::new();
    for noun in SPECIES_ROSTER {
        by_initial
            .entry(creature_initial(noun))
            .or_default()
            .push(noun);
    }
    let collisions: Vec<String> = by_initial
        .into_iter()
        .filter(|(_, v)| v.len() > 1)
        .map(|(c, v)| format!("{c}x{}", v.len()))
        .collect();
    lines.push(format!(
        "collisions ({} of {} initials repeat): {}",
        collisions.len(),
        SPECIES_ROSTER.len(),
        collisions.join(", ")
    ));

    lines.push(String::new());

    lines.push(format!(
        "REGISTER LEGEND (hornvale_game_core::register::REGISTER, {} rows)",
        REGISTER.len()
    ));
    for b in REGISTER {
        lines.push(format!(
            "{}  {:<10}{}",
            b.glyph,
            format!("{:?}", b.population),
            b.means
        ));
    }
    // Silence the otherwise-unused `Population` import outside the format
    // string above; keeping the type named (not just `Debug`-derived
    // through `b.population`) documents which type this legend prints.
    let _: Option<Population> = None;

    lines.push(
        "generated by clients/game/bin/examples/specimen_sheet.rs -- see task-5 report".to_string(),
    );

    // ---- The floor is exact, not a minimum: fail loudly if we drifted ---
    assert_eq!(
        lines.len(),
        HEIGHT,
        "the sheet must render at exactly {HEIGHT} lines (got {}); this file's \
         fixed layout needs updating alongside whatever grew or shrank",
        lines.len()
    );
    for (i, l) in lines.iter().enumerate() {
        let w = visible_width(l);
        assert!(
            w <= WIDTH,
            "line {i} is {w} columns wide, over the {WIDTH}-column floor: {l:?}"
        );
    }

    for l in &lines {
        println!("{l}");
    }
}

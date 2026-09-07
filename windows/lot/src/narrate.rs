//! The narrator (spec §6.1): one [`Story`] rendered as the four staged
//! reveals of §2.1 — **When**, **Where**, **Life**, **Story** — each
//! sentence carrying the numbered references its answer rests on, and a
//! `Sources` list at the end resolving every number.
//!
//! **The narrator's only inputs are its three arguments.** It reads no
//! world, re-derives nothing, and computes no quantity of its own: every
//! sentence is a value the [`Story`] already holds, and the only numbers
//! formatted here are the birth year and the seed, both carried by the
//! arguments. That is the rule spec §2.1 states as *"a tile is absent,
//! never invented"*, enforced by construction rather than by review — there
//! is nothing in scope to invent from.

use crate::context::LotContext;
use crate::draw::{Curve, Life};
use crate::slots::{Silence, SlotValue, Source, Story};

/// The four staged reveals, and which slots each one tells.
///
/// **This is a partition of [`Story::slots`] into four CONTIGUOUS runs, and
/// the contiguity is load-bearing.** The numbering behind every `[n]`
/// ([`Story::citations`]) is a first-citation ordering over `Story::slots`;
/// the prose's own first-appearance order is this table's order. The two
/// agree only while this table walks the slots in their own sequence, so a
/// test pins that the concatenation of these key lists IS the story's slot
/// order. Re-grouping a slot into a different stage is legal; re-ordering
/// one past its neighbours is not, and fails that test rather than silently
/// numbering the prose one way and the payload another.
/// type-audit: bare-ok(identifier-text)
pub const STAGES: [(&str, &[&str]); 4] = [
    ("When", &["when"]),
    ("Where", &["where"]),
    (
        "Life",
        &[
            "people",
            "name",
            "community-size",
            "founded-from",
            "founder-kinship",
            "community-fate",
        ],
    ),
    (
        "Story",
        &[
            "tech",
            "function",
            "tongue",
            "belief",
            "held-true",
            "subsistence",
            "standing",
            "tribute",
            "dwelling",
            "mine",
            "climate",
            "sky",
            "ground",
            "diet",
            "sex",
            "reproductive-role",
            "gender-identity",
            "gender-recognition",
            "family",
            "associations",
            "children",
            "siblings",
            "descent",
            "adoption",
            "care",
            "group-membership",
            "migration",
            "parental-death",
            "inheritance",
            "work",
            "literacy",
        ],
    ),
];

/// The one sentence the two remaining by-design silences (spec §4.4) are folded
/// into. Four separate "nothing in the record says" sentences would read as
/// four failures of this world's record; they are one statement about what
/// no world here models at all, so they are said once, together.
const BY_DESIGN: &str = "The record keeps no trade or letters for anyone; those two silences are \
                         the world's, not this telling's.";

/// Tell one drawn life as prose: the four stages, then the sources.
///
/// Needs no `World` — [`Story`] resolved every caption at tell time.
/// type-audit: bare-ok(prose: return)
pub fn narrate(ctx: &LotContext, life: &Life, story: &Story) -> String {
    let (sources, per_slot) = story.citations();
    let mut out = String::new();
    out.push_str(&format!("# Lot {} — seed {}\n", life.index, ctx.seed));
    for (stage, keys) in STAGES {
        out.push_str(&format!("\n## {stage}\n\n"));
        if stage == "Story" {
            out.push_str(&disclaimer(ctx, life, story));
            out.push('\n');
        }
        let mut by_design = 0usize;
        for key in keys {
            let Some(at) = story.slots.iter().position(|slot| slot.key == *key) else {
                continue;
            };
            let slot = &story.slots[at];
            match &slot.value {
                SlotValue::Filled(value) => {
                    let mut line = sentence(key, value);
                    for number in &per_slot[at] {
                        line.push_str(&format!(" [{number}]"));
                    }
                    out.push_str(&line);
                    out.push('\n');
                }
                SlotValue::Silent(Silence::NoFact(reason)) => {
                    out.push_str(&format!("{}: {reason}.\n", question(key)));
                }
                SlotValue::Silent(Silence::ByDesign(_)) => by_design += 1,
            }
        }
        if by_design > 0 {
            out.push_str(BY_DESIGN);
            out.push('\n');
        }
    }
    out.push_str("\n### Sources\n\n");
    for (at, source) in sources.iter().enumerate() {
        out.push_str(&format!("- [{}] {}\n", at + 1, source_line(source)));
    }
    out.push('\n');
    out
}

/// The line that opens the Story stage: the disclaimer spec §2.1 requires,
/// naming the lot as a construction from a real world's statistics rather
/// than a claim about a person.
fn disclaimer(ctx: &LotContext, life: &Life, story: &Story) -> String {
    let who = match story.slot("name").map(|slot| &slot.value) {
        Some(SlotValue::Filled(name)) => name.as_str(),
        _ => "This",
    };
    let place = match story.slot("where").map(|slot| &slot.value) {
        Some(SlotValue::Filled(where_it_was)) => where_it_was.as_str(),
        _ => "a place the record does not name",
    };
    format!(
        "{who} is not a real person, but this life is drawn from the statistical reality of \
         {place} in year {} of seed {}.",
        life.birth_year.round() as i64,
        ctx.seed
    )
}

/// One Filled slot's sentence. The slot's own value is the whole content;
/// the arm supplies only the grammar that makes it a sentence about the
/// life. Four slots (`founder-kinship`, `community-fate`, `held-true`,
/// `sky`) already read as sentences about their own subject and take the
/// fall-through, which only capitalizes them.
fn sentence(key: &str, value: &str) -> String {
    match key {
        "when" => format!("They were {value}."),
        "where" => format!("They lived at {value}."),
        "people" => format!("Their people was the {value}."),
        "name" => format!("They were called {value}."),
        "community-size" => format!("The community held {value}."),
        "founded-from" => format!("It had been {value}."),
        "tech" => format!("They had {value}."),
        "function" => format!("The community was for {value}."),
        "tongue" => format!("They spoke {value}."),
        "belief" => format!("They held {value}."),
        "subsistence" => format!("Their subsistence was {value}."),
        "standing" => format!("They reckoned standing in {value}."),
        "tribute" => format!("They {value}."),
        "dwelling" => format!("They lived in {value}."),
        "mine" => format!("They dug {value}."),
        "climate" => format!("They lived in {value}."),
        "ground" => format!("The ground under them gave {value}."),
        "diet" => format!("They {value}."),
        "sex" => format!("Their observed sex traits were {value}."),
        "reproductive-role" => format!("Their reproductive role was {value}."),
        "gender-identity" => format!("Their recorded gender identity was {value}."),
        "gender-recognition" => format!("Their recorded gender recognition was {value}."),
        "family" => format!("Their recorded descent included {value}."),
        "associations" => format!("Their recorded associations included {value}."),
        "children" => format!("Their recorded descent included {value}."),
        "siblings" => format!("Their sibling relations were {value}."),
        "descent" => format!("Their descent record included {value}."),
        "adoption" => format!("Their adoption or custody record included {value}."),
        "care" => format!("Their care record included {value}."),
        "group-membership" => format!("Their group memberships included {value}."),
        "migration" => format!("Their residence history included {value}."),
        "parental-death" => format!("{value}."),
        "inheritance" => format!("Their inheritance record included {value}."),
        _ => format!("{}.", sentence_case(value)),
    }
}

/// The honest sentence's opening for a slot the ledger had no answer for —
/// the question that was asked, so the silence names its own subject. The
/// [`Silence::NoFact`] reason completes it.
fn question(key: &str) -> &'static str {
    match key {
        "name" => "No name survives",
        "founded-from" => "Nothing in the record says how the community came to stand there",
        "founder-kinship" => {
            "Nothing in the record says how its founder stood to the mother community's founder"
        }
        "tongue" => "Nothing in the record says what tongue they spoke",
        "belief" => "Nothing in the record names what they held sacred",
        "held-true" => "Nothing in the record says what the community held true about its own kin",
        "subsistence" => "Nothing in the record says how they fed themselves",
        "standing" => "Nothing in the record says what standing they were reckoned in",
        "tribute" => "Nothing in the record says who they answered to",
        "dwelling" => "Nothing in the record says what they lived in",
        "mine" => "Nothing in the record speaks of a working here",
        "climate" => "Nothing in the record says what country this was",
        "sky" => "Nothing in the record says what the sky did over this life",
        "ground" => "Nothing in the record says what the ground did under this life",
        "diet" => "Nothing in the record says where they got their food",
        "sex" => "Nothing in the record says what sex traits they had",
        "reproductive-role" => "Nothing in the record says what reproductive role they held",
        "gender-identity" => "Nothing in the record says how they identified",
        "gender-recognition" => "Nothing in the record says how others recognized them",
        "family" => "Nothing in the record says what descent relations they had",
        "associations" => "Nothing in the record says what associations they formed",
        "children" => "Nothing in the record says whether they had children",
        "siblings" => "Nothing in the record says whether they had siblings",
        "descent" => "Nothing in the record says what their descent was",
        "adoption" => "Nothing in the record says whether adoption or custody occurred",
        "care" => "Nothing in the record says who cared for whom",
        "group-membership" => "Nothing in the record says which groups they joined",
        "migration" => "Nothing in the record says whether they migrated",
        "parental-death" => "Nothing in the record says whether a parent died",
        "inheritance" => "Nothing in the record says whether inheritance occurred",
        _ => "Nothing in the record answers this",
    }
}

/// One entry in the `Sources` list: a fact's own registry caption with the
/// `(entity, predicate)` pair it was read from, or the named derivation and
/// the committed inputs it ran on.
fn source_line(source: &Source) -> String {
    match source {
        Source::Fact {
            entity,
            predicate,
            caption,
        } => format!("{caption} (entity {entity}, {predicate})"),
        Source::Derived { function, inputs } => format!("derived: {function} ({inputs})"),
    }
}

/// `text` with its first character upper-cased.
fn sentence_case(text: &str) -> String {
    let mut characters = text.chars();
    match characters.next() {
        Some(first) => first.to_uppercase().collect::<String>() + characters.as_str(),
        None => String::new(),
    }
}

// ---------------------------------------------------------------------
// The When graph, in text (spec §6.3)
// ---------------------------------------------------------------------

/// The century a table row bins into. The bake's epochs are 25 years
/// (`shape::EPOCH_YEARS`), so four of them make one row; the row is derived
/// from each epoch's own opening year rather than from a fixed 4:1 ratio, so
/// a bake with a different epoch length still bins correctly.
/// plumb: universal(a presentation bin width for the table a reader reads, not a property of any world)
const CENTURY_YEARS: f64 = 100.0;

/// The share of births the closing sentence calls "recently" — the last
/// quarter of the span. Below this the world's births are called flat
/// instead.
/// plumb: universal(the editorial threshold this renderer calls recent, held fixed so two worlds' pages can be compared)
const RECENT_SHARE: f64 = 0.33;

/// The world's souls-ever line, its births-per-century table, and one
/// sentence about the curve's own shape — the page that precedes the ten
/// lives (spec §6.3, "the When graph in text").
///
/// **The closing sentence is computed from this world's curve, never copied
/// from the site the campaign is modelled on.** Its threshold is
/// [`RECENT_SHARE`] over the span's last quarter, and the share it quotes is
/// summed from the rows the reader can see above it, so the sentence and the
/// table cannot disagree.
///
/// **"five hundred years" is exact for the 2,000-year bake and only for
/// it.** The span's last quarter is 500 years when the bake runs
/// `[0, 2000)`, which is what `BakeConfig::default_millennia` produces and
/// what every world reaching [`crate::context::assemble`] carries today. A
/// test asserts that span rather than leaving the phrase to rot: a shorter
/// bake reddens it, which is the moment to derive the number instead of
/// spelling it.
/// type-audit: bare-ok(prose: return)
pub fn curve_text(ctx: &LotContext, curve: &Curve) -> String {
    let total: f64 = curve.births_by_epoch.iter().sum();
    let mut out = String::new();
    out.push_str(&format!(
        "About {} lives have been lived in seed {} between year {} and year {}.\n\n",
        thousands(three_significant_figures(curve.souls_ever)),
        ctx.seed,
        curve.start_year.round() as i64,
        curve.present_year.round() as i64
    ));

    // One row per century, binned from each epoch's own opening year.
    let span = (curve.present_year - curve.start_year).max(0.0);
    let centuries = (span / CENTURY_YEARS).ceil().max(1.0) as usize;
    let mut births = vec![0.0; centuries];
    for (at, count) in curve.births_by_epoch.iter().enumerate() {
        let opened = at as f64 * curve.epoch_years;
        let row = ((opened / CENTURY_YEARS).floor() as usize).min(centuries - 1);
        births[row] += count;
    }

    out.push_str("| years | births | share |\n|---|---:|---:|\n");
    for (row, count) in births.iter().enumerate() {
        let from = curve.start_year + row as f64 * CENTURY_YEARS;
        out.push_str(&format!(
            "| {}–{} | {} | {:.1}% |\n",
            from.round() as i64,
            (from + CENTURY_YEARS).round() as i64,
            thousands(count.round().max(0.0) as u64),
            share(*count, total)
        ));
    }
    out.push('\n');

    // The span's last quarter, summed from the rows above so the sentence
    // and the table quote the same arithmetic.
    let quarter_opens = curve.present_year - span / 4.0;
    let recent: f64 = births
        .iter()
        .enumerate()
        .filter(|(row, _)| curve.start_year + *row as f64 * CENTURY_YEARS >= quarter_opens)
        .map(|(_, count)| count)
        .sum();
    if total > 0.0 && recent / total >= RECENT_SHARE {
        out.push_str(&format!(
            "Most of these lives were born recently: the last five hundred years hold {:.1}% \
             of them.\n",
            share(recent, total)
        ));
    } else {
        out.push_str(
            "This world's population stopped growing: a birth is about as likely in any \
             century.\n",
        );
    }
    out
}

/// `count` as a percentage of `total`, zero when there is nothing to divide.
fn share(count: f64, total: f64) -> f64 {
    if total > 0.0 {
        count / total * 100.0
    } else {
        0.0
    }
}

/// `value` rounded to three significant figures, as a whole number.
///
/// Integer arithmetic throughout: no `log10`, no `powf`, so this needs no
/// transcendental and cannot differ by a ULP between platforms — the same
/// reason `hornvale_kernel::quantize` formats and re-parses rather than
/// scaling by a computed power.
fn three_significant_figures(value: f64) -> u64 {
    let whole = value.round().max(0.0) as u64;
    let mut digits = 0u32;
    let mut left = whole;
    while left > 0 {
        left /= 10;
        digits += 1;
    }
    if digits <= 3 {
        return whole;
    }
    let scale = 10u64.pow(digits - 3);
    (whole + scale / 2) / scale * scale
}

/// A whole number with thousands separators.
fn thousands(value: u64) -> String {
    let digits = value.to_string();
    let mut out = String::with_capacity(digits.len() + digits.len() / 3);
    for (at, digit) in digits.chars().enumerate() {
        if at > 0 && (digits.len() - at).is_multiple_of(3) {
            out.push(',');
        }
        out.push(digit);
    }
    out
}

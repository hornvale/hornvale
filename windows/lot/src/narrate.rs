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
use crate::draw::Life;
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
            "family",
            "work",
            "literacy",
        ],
    ),
];

/// The one sentence the four by-design silences (spec §4.4) are folded
/// into. Four separate "nothing in the record says" sentences would read as
/// four failures of this world's record; they are one statement about what
/// no world here models at all, so they are said once, together.
const BY_DESIGN: &str = "The record keeps no sex, household, trade or letters for anyone; those \
                         four silences are the world's, not this telling's.";

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

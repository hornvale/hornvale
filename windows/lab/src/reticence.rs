//! The Reticence's own instrument: a committed, per-people readout of the
//! doctrine prior — which improvised name each culture reaches for when it
//! has no rider concept, and how much patience that prior buys before a
//! host's testimony worsens (`windows/vessel/src/doctrine.rs`,
//! `windows/vessel/src/stance.rs`).
//!
//! **This needs the full [`FullView`] sculpt, for the same structural
//! reason [`crate::render_confidant_report`] does, not merely by analogy to
//! it.** `improvised_name` reads `lexicon.entry("god")`/`entry("spirit")`,
//! and the only way to reach a real `hornvale_language::Lexicon` is
//! `hornvale_worldgen::lexicon_from_in`, whose signature takes a
//! `&GeneratedTerrain` and `&GeneratedClimate` — there is no lower-
//! `BuildDepth` overload that returns a `Lexicon`. So this report pays for a
//! full terrain+climate sculpt to answer a question
//! (`crate::render_confidant_report`'s own doc calls it out first) that is
//! *itself* world-invariant: a word's existence in a lexicon turns on
//! `MindVector`, an authored per-species constant, never a world-drawn one.
//! The cost is real and paid once per artifact regeneration, not per
//! metric — `Seed(42)` only because *some* seed must build the view.
//!
//! Doctrine-prior data only (Tasks 1-3): no live session, no override
//! count, no `hornvale_vessel::testimony`-shaped stance. A rider's actual
//! conduct is per-playthrough state this report cannot see and does not
//! attempt to summarize — see the doc on
//! [`hornvale_vessel::stance::stance_for`] for where that half lives.

use crate::FullView;
use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_vessel::doctrine::{
    ImprovisedName, Openness, cult_form_of, improvised_name, openness,
};
use hornvale_vessel::stance::patience;
use hornvale_worldgen::BuildError;

/// `cult-form` rendered for the table: the raw ledger value, or an em dash
/// when no belief at this people's site(s) carries one at all — the same
/// convention `crate::render_confidant_report`'s own private `confidant_field`
/// uses for its `Absent` case, so a reader learns the same glyph means
/// "nothing to report" everywhere in this crate's artifacts.
fn cult_form_field(cult_form: &Option<String>) -> &str {
    cult_form.as_deref().unwrap_or("—")
}

/// `improvised-name` rendered for the table. The [`ImprovisedName::Wordless`]
/// arm always starts with the literal `Wordless` (never reworded), because
/// this crate's own test
/// (`the_report_covers_every_people_and_names_no_doctrine_arm`) uses that
/// prefix to distinguish it from a hypothetical doctrine arm without
/// matching on the substring `"Doctrine"` against prose that might
/// legitimately mention the word.
fn improvised_name_field(name: &ImprovisedName) -> String {
    match name {
        ImprovisedName::God => "God".to_string(),
        ImprovisedName::Spirit => "Spirit".to_string(),
        ImprovisedName::Wordless { reason } => format!("Wordless ({reason})"),
    }
}

/// `prior-openness` rendered for the table.
fn openness_field(o: Openness) -> &'static str {
    match o {
        Openness::Guarded => "Guarded",
        Openness::Wary => "Wary",
        Openness::Open => "Open",
    }
}

/// Render The Reticence's report (Task 6): one row per
/// `hornvale_species::society_registry()` people (the same fifteen-people
/// roster [`render_confidant_report`] walks, alphabetical by
/// `KindId`/`BTreeMap` order), four columns — `cult-form`, `improvised-name`,
/// `prior-openness`, `patience` — computed once, at `Seed(42)`.
///
/// type-audit: bare-ok(artifact: return)
pub fn render_reticence_report() -> Result<String, BuildError> {
    let view = FullView::build(Seed(42), &SkyPins::default())?;
    let mut out = String::new();
    out.push_str(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab reticence`. -->\n\n",
    );
    out.push_str("# The Reticence: doctrine prior by people\n\n");
    out.push_str(
        "One row per one of the fifteen `hornvale_species::society_registry()` peoples — the \
         same roster The Confidant's report walks. Four columns, spec §3.2-3.5:\n\n",
    );
    out.push_str(
        "- **`cult-form`** — the raw `cult-form` fact held at this people's site(s) \
         (`organized`/`folk`), or `—` when no belief there carries one at all.\n",
    );
    out.push_str(
        "- **`improvised-name`** — what this people reaches for in place of a rider concept, \
         since none is registered in this world (`God`/`Spirit`/`Wordless (<reason>)`): \
         `cult-form == organized` collapses to `God`, everything else (`folk`, or no cult-form \
         fact at all) collapses to `Spirit`, and a people with no word for `god` or `spirit` at \
         all is `Wordless`.\n",
    );
    out.push_str(
        "- **`prior-openness`** — the doctrine prior's sign alone, before any conduct is folded \
         in (`Guarded`/`Wary`/`Open`): the essay's doctrine-holding people knows what to do \
         about a rider, so equipment reduces willingness rather than raising it.\n",
    );
    out.push_str(
        "- **`patience`** — how many times a host of this prior tolerates being overridden \
         before its stance worsens one step (`windows/vessel/src/stance.rs`'s `patience`).\n\n",
    );
    out.push_str(
        "Computed once, at `Seed(42)` — see this function's own module doc for why a full \
         sculpt is paid for here despite the felt-state half of the answer being \
         world-invariant.\n\n",
    );
    out.push_str("| people | cult-form | improvised-name | prior-openness | patience |\n");
    out.push_str("|---|---|---|---|---|\n");
    for kind in hornvale_species::society_registry().ids() {
        let people = kind.0;
        let lexicon = hornvale_worldgen::lexicon_from_in(
            view.world(),
            view.components(),
            people,
            view.terrain(),
            view.climate(),
        )?;
        let cult_form = cult_form_of(view.world(), people);
        let name = improvised_name(view.world(), &lexicon, people);
        let prior = openness(&name);
        out.push_str(&format!(
            "| {people} | {} | {} | {} | {} |\n",
            cult_form_field(&cult_form),
            improvised_name_field(&name),
            openness_field(prior),
            patience(prior),
        ));
    }
    Ok(out)
}

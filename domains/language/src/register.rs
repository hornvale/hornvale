//! The register renderer: the permanent content→render seam. `render_line`
//! voices a belief's structured [`LineContent`] into a tenet string,
//! parameterized by per-species [`VoiceParams`]. This module never imports
//! `hornvale-religion` — [`LineSentiment`] is language's own copy of the
//! eternal/cyclic/ambient distinction; the composition root maps religion's
//! `Sentiment` onto it.
//!
//! **This function signature is the campaign's permanent contribution.** v1
//! fills it with small, legible templates assembled purely from the
//! structured fields of `LineContent` under the three voice knobs; a future
//! generative oral-formulaic grammar reoccupies the same signature without
//! callers changing. `render_line` therefore only ever reads structured
//! fields — it never post-processes a finished English string.

/// Per-species voice knobs the composition root derives from the psychology
/// vector. Every field lives in `[0, 1]`; `0.5` on every field is the goblin
/// baseline (identity — a goblin-voiced line is what v1's templates default
/// to).
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct VoiceParams {
    /// Archaic vs. plain connectives: low picks plain, everyday phrasing;
    /// high picks archaic, ceremonial phrasing.
    pub formality: f64,
    /// Refrain echoing: low states the proposition once; high appends a
    /// short echoed refrain, lengthening the line.
    pub repetition: f64,
    /// How the epithet is introduced and how often the honorific repeats:
    /// low mentions it once, lightly; high stacks it and repeats it.
    pub epithet_density: f64,
}

/// Language's own copy of a belief's periodicity/character. The composition
/// root maps `hornvale_religion::Sentiment` onto this; language never
/// imports religion.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LineSentiment {
    /// The deity is watched, unchanging, always-present.
    Eternal,
    /// The deity returns on a period (see [`LineContent::period_days`]).
    Cyclic,
    /// The deity is felt rather than tracked — mood, not motion.
    Ambient,
}

/// The structured meaning of a tenet, as the composition root assembles it
/// from a belief's committed facts. `render_line` reads only these fields —
/// never a pre-rendered string.
/// type-audit: bare-ok(identifier-text: deity), bare-ok(identifier-text: epithet), pending(wave-3: period_days), bare-ok(flag: high_god)
#[derive(Clone, Debug, PartialEq)]
pub struct LineContent {
    /// The deity's generated name.
    pub deity: String,
    /// The deity's generated epithet (without any honorific prefix; density
    /// controls whether/how one is added at render time).
    pub epithet: String,
    /// The tenet's periodicity/character.
    pub sentiment: LineSentiment,
    /// For [`LineSentiment::Cyclic`], the period in standard days. `None`
    /// for eternal/ambient tenets.
    pub period_days: Option<f64>,
    /// Whether this deity is a high god (already committed via the
    /// `high-god` fact); high gods earn the most formal connective variant.
    pub high_god: bool,
}

/// A word chosen from either an archaic or a plain register, keyed by
/// `formality`. `formality < 0.5` picks `plain`; otherwise `archaic` — the
/// midpoint (the goblin baseline) resolves to the archaic half, matching a
/// goblin's honorific-dense, formal psychology vector (spec §6).
fn register_word(formality: f64, archaic: &str, plain: &str) -> String {
    if formality < 0.5 {
        plain.to_string()
    } else {
        archaic.to_string()
    }
}

/// The epithet clause: how many times and how the honorific is introduced,
/// keyed by `epithet_density`. Low density mentions the epithet once;
/// mid/high density stacks a repeated honorific ("the", doubled at the top
/// band) around it.
fn epithet_clause(epithet: &str, density: f64) -> String {
    if density < 0.34 {
        epithet.to_string()
    } else if density < 0.67 {
        format!("the {epithet}")
    } else {
        format!("the {epithet}, the {epithet}")
    }
}

/// The core proposition: names the deity, states the eternal/cyclic/ambient
/// character, and for cyclic tenets states the period. This is the part of
/// the line every voice must contain, regardless of knob settings.
fn core_proposition(content: &LineContent, voice: &VoiceParams) -> String {
    let is = register_word(voice.formality, "is ever", "is");
    let epithet = epithet_clause(&content.epithet, voice.epithet_density);
    let subject = format!("{} {epithet}", content.deity);
    match content.sentiment {
        LineSentiment::Eternal => {
            let watched = register_word(voice.formality, "watches unceasing", "is always watching");
            format!("{subject} {is}: {subject} {watched}.")
        }
        LineSentiment::Cyclic => {
            let period = content.period_days.unwrap_or(0.0);
            let returns = register_word(voice.formality, "returns", "comes back");
            format!("{subject} {returns} every {period} days.")
        }
        LineSentiment::Ambient => {
            let felt = register_word(
                voice.formality,
                "is felt, not counted",
                "is felt more than seen",
            );
            format!("{subject} {felt}.")
        }
    }
}

/// A short echoed refrain, appended when `repetition` runs high, and
/// lengthened further at the top band. Below the low threshold no refrain
/// is appended at all.
fn refrain(content: &LineContent, voice: &VoiceParams) -> String {
    if voice.repetition < 0.34 {
        return String::new();
    }
    let so = register_word(
        voice.formality,
        "So it was, so it is.",
        "That's how it's always been.",
    );
    if voice.repetition < 0.67 {
        format!(" {so}")
    } else {
        let rank = if content.high_god {
            "the highest"
        } else {
            "a great one"
        };
        format!(" {so} {so} {} is {rank} among the gods.", content.deity)
    }
}

/// Render a belief's structured content into a tenet string, voiced by
/// `voice`. **The permanent content→render seam** — pure and deterministic:
/// no streams, no wall-clock time, no randomness; the same `(content,
/// voice)` pair always produces the same string. v1 assembles small
/// templates from `content`'s structured fields under the three knobs; a
/// future generative grammar fills the same signature without callers
/// changing.
/// type-audit: bare-ok(prose)
pub fn render_line(content: &LineContent, voice: &VoiceParams) -> String {
    let mut line = core_proposition(content, voice);
    line.push_str(&refrain(content, voice));
    line
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base() -> VoiceParams {
        VoiceParams {
            formality: 0.5,
            repetition: 0.5,
            epithet_density: 0.5,
        }
    }
    fn cyclic(deity: &str, ep: &str) -> LineContent {
        LineContent {
            deity: deity.into(),
            epithet: ep.into(),
            sentiment: LineSentiment::Cyclic,
            period_days: Some(29.0),
            high_god: true,
        }
    }

    #[test]
    fn a_cyclic_line_names_the_deity_and_its_return() {
        let s = render_line(&cyclic("Xar", "the Tidewalker"), &base());
        assert!(s.contains("Xar") || s.contains("Tidewalker"));
        assert!(s.contains("29"), "a cyclic tenet states its period");
    }

    #[test]
    fn higher_repetition_lengthens_the_line() {
        let low = render_line(
            &cyclic("Xar", "the Tidewalker"),
            &VoiceParams {
                repetition: 0.0,
                ..base()
            },
        );
        let high = render_line(
            &cyclic("Xar", "the Tidewalker"),
            &VoiceParams {
                repetition: 1.0,
                ..base()
            },
        );
        assert!(high.len() > low.len(), "repetition echoes a refrain");
    }

    #[test]
    fn render_is_pure_and_deterministic() {
        assert_eq!(
            render_line(&cyclic("Xar", "the Tidewalker"), &base()),
            render_line(&cyclic("Xar", "the Tidewalker"), &base())
        );
    }

    #[test]
    fn goblin_baseline_and_contrasting_voice_produce_different_strings() {
        let content = cyclic("Xar", "the Tidewalker");
        let goblin_baseline = base();
        let contrasting = VoiceParams {
            formality: 1.0,
            repetition: 1.0,
            epithet_density: 1.0,
        };
        let baseline_line = render_line(&content, &goblin_baseline);
        let contrasting_line = render_line(&content, &contrasting);
        assert_ne!(
            baseline_line, contrasting_line,
            "the voice knobs must actually modulate the rendered surface"
        );
    }
}

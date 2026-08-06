//! The Focalizer seam interface: render a vantage as prose. Tier 0 is one
//! honest templated passage from real data. The examine contract: every
//! noun the prose mentions is in `nouns`, and only those are examinable.

use crate::Vantage;
use hornvale_kernel::SeaLevelHeight;

/// A rendered vantage: prose plus its noun catalog.
/// type-audit: bare-ok(prose: prose)
#[derive(Debug, Clone, PartialEq)]
pub struct Focalized {
    /// The passage `look` prints.
    pub prose: String,
    /// The examinable catalog, in prose order.
    pub nouns: Vec<Noun>,
}

/// Render a vantage as prose.
pub trait Focalizer {
    /// One focalized rendering of the vantage.
    fn render(&self, vantage: &Vantage) -> Focalized;
}

/// Words too small or too common to be a handle. Judgement, not a discovered
/// fact, and deliberately in one place so the judgement is visible.
const STOPWORDS: [&str; 14] = [
    "a", "an", "the", "of", "in", "on", "over", "under", "and", "by", "at", "with", "its", "into",
];

/// The shortest *derived* word that can be a handle. Three-letter words in
/// these phrases are adjectives and articles ("icy", "dry", "the"), not things,
/// so derivation starts at four.
///
/// A short **display name** is unaffected: `display` always enters `words`
/// verbatim, so the `sky` entry resolves as `sky` despite being three letters.
/// The floor governs what is split OUT of a phrase, never the phrase itself.
const MIN_WORD: usize = 4;

/// One examinable thing: what the prose called it, what `examine` prints, and
/// the words a player may type to reach it.
///
/// `display` is the single client-facing name — it is what
/// `snapshot::NounEntry` carries and what the chart legend shows. `words` is
/// **process-internal and never serialized**: putting aliases on the wire would
/// spray "forest, tropical, seasonal" into the browser client's legend beside
/// the real entry.
/// type-audit: bare-ok(identifier-text: display), bare-ok(prose: datum), bare-ok(identifier-text: words)
#[derive(Debug, Clone, PartialEq)]
pub struct Noun {
    /// The name the prose used, and the only one a client sees.
    pub display: String,
    /// What `examine` prints for it.
    pub datum: String,
    /// Lowercased words that resolve to this entry. Never serialized.
    pub words: Vec<String>,
}

impl Noun {
    /// Build an entry. `nameable` is the part of the name a player would say —
    /// for most entries the whole display name, but for a room descriptor only
    /// the noun phrase, because `a stream gully, shaded, in a hollow` is a
    /// sentence fragment and only `stream gully` is a thing.
    /// type-audit: bare-ok(identifier-text: display), bare-ok(identifier-text: nameable), bare-ok(prose: datum)
    pub fn new(display: &str, nameable: &str, datum: &str) -> Noun {
        let mut words: Vec<String> = vec![display.to_lowercase()];
        if !nameable.eq_ignore_ascii_case(display) {
            words.push(nameable.to_lowercase());
        }
        for w in nameable.split(|c: char| !c.is_alphanumeric()) {
            let w = w.to_lowercase();
            if w.chars().count() >= MIN_WORD
                && !STOPWORDS.contains(&w.as_str())
                && !words.contains(&w)
            {
                words.push(w);
            }
        }
        Noun {
            display: display.to_string(),
            datum: datum.to_string(),
            words,
        }
    }

    /// Whether `wanted` (already trimmed) names this entry, case-insensitively.
    /// type-audit: bare-ok(identifier-text: wanted), bare-ok(flag: return)
    pub fn matches(&self, wanted: &str) -> bool {
        let w = wanted.trim().to_lowercase();
        self.words.contains(&w)
    }
}

/// Capitalize the first character, leaving the rest alone — the biome noun now
/// opens the sentence.
/// type-audit: bare-ok(prose: s), bare-ok(prose: return)
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

/// A height as a reader-facing phrase. Sea level is derived per world and is
/// nowhere near 0 m on the isostatic datum, so a bare signed number is not
/// merely unhelpful — it reads as a depth. Saying the datum aloud is the prose
/// half of the discipline the type carries in code.
/// type-audit: bare-ok(prose: return)
fn height_phrase(h: SeaLevelHeight) -> String {
    let m = h.get();
    // Branch on what will be PRINTED, not on the raw sign. The room the bug
    // report came from sits 0.2 m under, and testing `m < 0.0` while formatting
    // `{:.0}` rendered it "0 m below sea level" — true, and unreadable. A place
    // within half a metre of the datum is simply at sea level, which is also the
    // honest thing to say about a shoreline whose height is a three-corner blend
    // and whose sign is not meaningful at that precision.
    if m.abs() < 0.5 {
        "at sea level".to_string()
    } else if m < 0.0 {
        format!("{:.0} m below sea level", h.depth())
    } else {
        format!("{m:.0} m above sea level")
    }
}

/// Tier 0: one templated passage. Repetitive across rooms by design — The
/// Uncommon Ground buys variety and absorbs into this surface.
pub struct TemplateFocalizer;

impl Focalizer for TemplateFocalizer {
    fn render(&self, v: &Vantage) -> Focalized {
        let biome = v.locale.biome.clone();
        let descriptor = v.locale.regime.descriptor.clone();
        let village = v.village.name.clone();
        let sky_noun = "sky".to_string();
        // A walker does not STAND in the sea. The verb follows the medium, and
        // the water column distinguishes floating on the surface from hanging
        // in the water below it — the same category error The Shoal fixed for
        // the descriptors, one clause up.
        // The narrator does not say what the occupant's body is doing.
        //
        // "You stand in coral reef" asserted a posture nothing had computed:
        // the renderer knows the medium and the band, and knows nothing about
        // legs, fins, wings, boats, or sleep. Adding "swim" and "walk the
        // floor" would only have made the unsourced claim more specific. So
        // the description describes the PLACE — the convention tabletop
        // read-aloud text arrived at for the same reason, that the body
        // belongs to whoever owns it.
        //
        // A sourced stance is still possible later; it wants the liveness
        // layer to supply a real activity, and this seam is where it would go.
        // On the surface, the depth zone beneath is not where the observer
        // is: the sea's own name for that place is simply the open water.
        let named = if v.locale.biome_kind.is_marine() && !v.submerged {
            "open water".to_string()
        } else {
            biome.clone()
        };
        let place = capitalize_first(&named);
        let prose = format!(
            "{place} — {descriptor} — in the lands of {village}. The {sky_noun} above: {}",
            v.sky
        );
        let mut nouns = vec![
            Noun::new(
                &biome,
                &biome,
                &format!(
                    "{:.1} °C the year round, moisture {:.2}, {}.",
                    v.locale.fields.temperature_c,
                    v.locale.fields.moisture,
                    height_phrase(v.locale.fields.height_asl_m)
                ),
            ),
            Noun::new(
                &descriptor,
                &v.locale.regime.descriptor_noun,
                &format!(
                    "The ground here: {} (strangeness {:.0}).",
                    v.locale.regime.descriptor, v.locale.regime.strangeness
                ),
            ),
            Noun::new(
                &village,
                &village,
                &format!("{} souls call it home.", v.village.population),
            ),
            Noun::new(&sky_noun, &sky_noun, &v.sky),
        ];
        // One entry per body the sky named — "the vast moon", "the sun" — so
        // a player can name what the sentence just said rather than only the
        // whole report. Two moons both yielding the word "moon" is expected;
        // `Noun::matches` and catalog order resolve it to the first.
        for (noun, datum) in &v.sky_bodies {
            nouns.push(Noun::new(noun, noun, datum));
        }
        Focalized { prose, nouns }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{mint_flagship, observable};
    use hornvale_kernel::{Seed, World, WorldTime};
    use hornvale_locale::LocaleContext;
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    #[test]
    fn significant_words_skip_stopwords_and_short_words() {
        let n = Noun::new(
            "bugbear of Goodogododaga",
            "bugbear of Goodogododaga",
            "a bugbear.",
        );
        assert!(n.words.contains(&"bugbear".to_string()));
        assert!(n.words.contains(&"goodogododaga".to_string()));
        assert!(
            !n.words.contains(&"of".to_string()),
            "stopword: {:?}",
            n.words
        );
    }

    #[test]
    fn a_noun_resolves_by_its_whole_name_and_by_each_significant_word() {
        let n = Noun::new(
            "tropical seasonal forest",
            "tropical seasonal forest",
            "warm.",
        );
        for w in ["tropical seasonal forest", "tropical", "seasonal", "forest"] {
            assert!(n.matches(w), "{w:?} must resolve: {:?}", n.words);
        }
        assert!(!n.matches("canopy"));
    }

    #[test]
    fn the_nameable_part_can_differ_from_the_display_name() {
        // Task 2 needs this: the descriptor displays the whole phrase but only its
        // noun phrase is nameable.
        let n = Noun::new(
            "a stream gully, shaded, in a hollow",
            "stream gully",
            "ground.",
        );
        assert!(n.matches("stream"));
        assert!(n.matches("gully"));
        assert!(
            n.matches("a stream gully, shaded, in a hollow"),
            "the display name still resolves"
        );
        assert!(!n.matches("shaded"), "a qualifier is not a noun");
    }

    fn seam_world() -> World {
        build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    fn vantage_at(day: f64) -> Vantage {
        let world = seam_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let agent = mint_flagship(&world, &ctx).unwrap();
        observable(&world, &ctx, &agent, WorldTime { day }).unwrap()
    }

    #[test]
    fn every_noun_appears_in_the_prose() {
        let f = TemplateFocalizer.render(&vantage_at(0.0));
        assert!(!f.prose.is_empty());
        let prose = f.prose.to_lowercase();
        for n in &f.nouns {
            assert!(
                prose.contains(&n.display.to_lowercase()),
                "noun '{}' must be mentioned by look",
                n.display
            );
            assert!(
                !n.datum.is_empty(),
                "noun '{}' must have a datum",
                n.display
            );
        }
    }

    #[test]
    fn the_focalization_is_deterministic() {
        let a = TemplateFocalizer.render(&vantage_at(0.0));
        let b = TemplateFocalizer.render(&vantage_at(0.0));
        assert_eq!(a.prose, b.prose);
        assert_eq!(a.nouns, b.nouns);
    }

    #[test]
    fn the_day_threads_through_to_the_sky() {
        let v = vantage_at(120.0);
        let f = TemplateFocalizer.render(&v);
        let sky = f
            .nouns
            .iter()
            .find(|n| n.display == "sky")
            .expect("sky is a noun");
        assert_eq!(
            sky.datum, v.sky,
            "the sky noun carries the day's sky report"
        );
    }

    #[test]
    fn the_biome_datum_reports_height_above_sea_level() {
        let v = vantage_at(0.0);
        let f = TemplateFocalizer.render(&v);
        let n = f
            .nouns
            .iter()
            .find(|n| n.display == v.locale.biome)
            .expect("the biome is a noun");
        let datum = &n.datum;
        // Seed 42's sea level is -2936.17 m. Before The Benchmark this line read
        // "-2936 m elevation" for a tropical forest at the shoreline.
        assert!(
            !datum.contains("-2936"),
            "the raw isostatic reading leaked into prose: {datum}"
        );
        assert!(
            datum.contains("sea level"),
            "a height must say what it is a height above: {datum}"
        );
    }

    #[test]
    fn a_height_that_rounds_to_zero_is_not_reported_as_zero_metres_below() {
        // The room the bug report came from is 0.2 m under, and the first fix
        // branched on the raw sign while printing `{:.0}` — rendering it as
        // "0 m below sea level". Both signs must collapse to the same honest
        // phrase inside the rounding boundary.
        assert_eq!(
            height_phrase(SeaLevelHeight::from_metres(-0.2)),
            "at sea level"
        );
        assert_eq!(
            height_phrase(SeaLevelHeight::from_metres(0.4)),
            "at sea level"
        );
        // …and outside it, the sign and the datum are both stated.
        assert_eq!(
            height_phrase(SeaLevelHeight::from_metres(-1200.0)),
            "1200 m below sea level"
        );
        assert_eq!(
            height_phrase(SeaLevelHeight::from_metres(1200.0)),
            "1200 m above sea level"
        );
    }

    #[test]
    fn each_body_the_sky_names_is_examinable_and_moon_is_not_ambiguous_at_runtime() {
        let v = vantage_at(0.0);
        let f = TemplateFocalizer.render(&v);
        let moons: Vec<&Noun> = f.nouns.iter().filter(|n| n.matches("moon")).collect();
        assert!(!moons.is_empty(), "the night sky names at least one moon");
        // Deterministic priority: the first entry wins, and it is a MOON's
        // datum, not the whole sky report.
        let first = moons[0];
        assert!(
            first.datum.contains("moon"),
            "moon resolves to a moon: {:?}",
            first.datum
        );
        assert_ne!(first.datum, v.sky, "and not to the whole sky report");
    }
}

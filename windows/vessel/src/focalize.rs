//! The Focalizer seam interface: render a vantage as prose. Tier 0 is one
//! honest templated passage from real data. The examine contract: every
//! noun the prose mentions is in `nouns`, and only those are examinable.

use crate::Vantage;
use hornvale_kernel::{EntityId, SeaLevelHeight};

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
/// plumb: universal(a text-derivation floor for handle phrases, a language-processing constant)
const MIN_WORD: usize = 4;

/// The coarse kind a completion-capable client may filter on. Closed set;
/// `Unknown` is the honest default where the sim claims nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NounKind {
    /// A living or animate being. No render-site noun carries this yet —
    /// creatures are tagged at later call sites (the session's present and
    /// underground nouns).
    Creature,
    /// A location: biome, regime descriptor, village.
    Place,
    /// An object or phenomenon that is neither of the above.
    Thing,
    /// No kind claimed.
    Unknown,
}

impl NounKind {
    /// The lowercase wire tag this kind serializes as, for the additive
    /// `kind` field on `snapshot::NounEntry`.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn tag(self) -> &'static str {
        match self {
            NounKind::Creature => "creature",
            NounKind::Place => "place",
            NounKind::Thing => "thing",
            NounKind::Unknown => "unknown",
        }
    }
}

/// One examinable thing: what the prose called it, what `examine` prints, and
/// the words a player may type to reach it.
///
/// `display` is the single client-facing name — it is what
/// `snapshot::NounEntry` carries and what the chart legend shows. `words` is
/// **process-internal and never serialized**: putting aliases on the wire would
/// spray "forest, tropical, seasonal" into the browser client's legend beside
/// the real entry.
///
/// **Nothing on this struct can reach the wire by accident, and the mechanism
/// is not a `serde(skip)`.** `Noun` derives no `Serialize` at all; the wire
/// type is a separate struct, [`crate::snapshot::NounEntry`], built
/// field-by-field in `Session::snapshot`. So a field added here — `words`,
/// `entity` — is invisible to every client until someone writes the line that
/// copies it across.
/// type-audit: bare-ok(identifier-text: display), bare-ok(prose: datum), bare-ok(identifier-text: words)
#[derive(Debug, Clone, PartialEq)]
pub struct Noun {
    /// The name the prose used, and the only one a client sees.
    pub display: String,
    /// What `examine` prints for it.
    pub datum: String,
    /// Lowercased words that resolve to this entry. Never serialized.
    pub words: Vec<String>,
    /// The coarse kind claimed for this entry. Defaults to `Unknown`; see
    /// `with_kind` for the construction sites that can claim one.
    pub kind: NounKind,
    /// The ledger entity this word names, where the catalog site knows one.
    ///
    /// **This is the name → entity lookup** (The Chattel, Task 10): before it,
    /// resolving a typed word yielded a `datum` — a String — and stopped, so
    /// two rooms' water jars were indistinguishable to every caller downstream
    /// of `matches`. `Some(id)` is the identity a verb can act on; `None` is
    /// the honest default for a site with no entity to claim (the biome, the
    /// sky, a chart-legend mark), not a placeholder to be invented.
    ///
    /// Defaults to `None`; see `with_entity` for the construction sites that
    /// can claim one.
    pub entity: Option<EntityId>,
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
            kind: NounKind::Unknown,
            entity: None,
        }
    }

    /// Attach a coarse kind, for construction sites that can claim one.
    pub fn with_kind(mut self, kind: NounKind) -> Noun {
        self.kind = kind;
        self
    }

    /// Attach the ledger entity this entry names, for construction sites that
    /// can claim one — the shape `with_kind` established, and for the same
    /// reason: `new`'s signature stays what every one of its callers already
    /// passes, and a site that knows nothing about entities says nothing.
    ///
    /// Takes an `EntityId` rather than an `Option<EntityId>`: a caller with
    /// nothing to claim does not call this at all, which is a stronger
    /// statement than passing `None` through a builder.
    pub fn with_entity(mut self, entity: EntityId) -> Noun {
        self.entity = Some(entity);
        self
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
    // honest thing to say about a shoreline whose height is a four-corner blend
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
            // **THE DISPLAY IS `named`, NOT `biome`, AND THAT WAS A LATENT
            // DEFECT UNTIL THE TIDEMARK.** The prose one block up renames a
            // surface reading of a marine biome to "open water" — the sea's
            // own name for the place an observer floating above a depth zone
            // actually is — and this noun went on carrying the raw class
            // ("bathypelagic"), so `look` printed a place the prose never
            // mentioned and `examine bathypelagic` was the only handle for a
            // word no player had been shown.
            //
            // It was unreachable for the life of that code: nothing put an
            // observer on a marine vertex until The Tidemark authored six
            // obligate marine peoples, at which point seed 42's FIRST
            // settlement became an abyssal-elf one and
            // `every_noun_appears_in_the_prose` went red on a real world.
            // An inert branch is inert only until someone reads it.
            //
            // `nameable` stays the raw class, so the class name is still a
            // typeable alias — the renaming narrows what is SHOWN, never
            // what can be reached.
            Noun::new(
                &named,
                &biome,
                &format!(
                    "{:.1} °C the year round, moisture {:.2}, {}.",
                    v.locale.fields.temperature_c,
                    v.locale.fields.moisture,
                    height_phrase(v.locale.fields.height_asl_m)
                ),
            )
            .with_kind(NounKind::Place),
            Noun::new(
                &descriptor,
                &v.locale.regime.descriptor_noun,
                &format!(
                    "The ground here: {} (strangeness {:.0}).",
                    v.locale.regime.descriptor, v.locale.regime.strangeness
                ),
            )
            .with_kind(NounKind::Place),
            Noun::new(
                &village,
                &village,
                &format!("{} souls call it home.", v.village.population),
            )
            .with_kind(NounKind::Place),
            Noun::new(&sky_noun, &sky_noun, &v.sky).with_kind(NounKind::Thing),
        ];
        // One entry per body the sky named — "the vast moon", "the sun" — so
        // a player can name what the sentence just said rather than only the
        // whole report. Two moons both yielding the word "moon" is expected;
        // `Noun::matches` and catalog order resolve it to the first.
        for (noun, datum) in &v.sky_bodies {
            nouns.push(Noun::new(noun, noun, datum).with_kind(NounKind::Thing));
        }
        Focalized { prose, nouns }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::observable;
    use hornvale_kernel::{EntityId, Seed, World, WorldTime};
    use hornvale_locale::LocaleContext;
    use hornvale_worldgen::{SettlementPins, build_world};

    #[test]
    fn a_noun_defaults_to_unknown_and_with_kind_sets_it() {
        let n = Noun::new("tropical seasonal forest", "forest", "warm.");
        assert_eq!(n.kind, NounKind::Unknown);
        let p = n.clone().with_kind(NounKind::Place);
        assert_eq!(p.kind, NounKind::Place);
    }

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
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    fn vantage_at(day: f64) -> Vantage {
        let world = seam_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let village = hornvale_settlement::village_info(&world).expect("seed 42 has a flagship");
        let entity = EntityId::new(1).expect("1 is a valid nonzero entity id");
        let npc = crate::liveness::body_at(&world, &ctx, &village, entity);
        let position = npc.home.clone();
        observable(
            &world,
            &ctx,
            &npc,
            &position,
            WorldTime::from_std_days(day).expect("a day value is finite"),
        )
        .unwrap()
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

    /// `Noun` derives `PartialEq`, so `a.nouns == b.nouns` covers every field
    /// this struct has — including `entity`, added by The Chattel's Task 10.
    /// **That coverage is vacuous today and worth saying so**: no site in
    /// `render` calls `with_entity`, so both sides carry `None` and the new
    /// field is compared trivially. The day a render site claims an entity,
    /// this assertion starts holding that the claim is a pure function of the
    /// vantage, with no edit here.
    #[test]
    fn the_focalization_is_deterministic() {
        let a = TemplateFocalizer.render(&vantage_at(0.0));
        let b = TemplateFocalizer.render(&vantage_at(0.0));
        assert_eq!(a.prose, b.prose);
        assert_eq!(a.nouns, b.nouns);
    }

    #[test]
    fn rendered_noun_kinds_match_each_entrys_role() {
        // Pins the four `.with_kind(...)` sites in `render()`: places are
        // Place, everything the sky contributes is Thing. Swapping any tag
        // must fail here.
        let v = vantage_at(0.0);
        let f = TemplateFocalizer.render(&v);
        let kind_of = |display: &str| {
            f.nouns
                .iter()
                .find(|n| n.display == *display)
                .unwrap_or_else(|| panic!("no noun named {display:?}"))
                .kind
        };
        // The biome noun is looked up by HANDLE, not by display: a surface
        // reading of a marine biome displays as "open water" while keeping
        // the class name as an alias (see `render`'s comment on that noun).
        // Looking it up by display would have made this test pass only on a
        // land vertex, which is how the defect that comment records stayed
        // invisible.
        let biome_noun = f
            .nouns
            .iter()
            .find(|n| n.matches(&v.locale.biome))
            .unwrap_or_else(|| panic!("no noun reachable as {:?}", v.locale.biome));
        assert_eq!(biome_noun.kind, NounKind::Place);
        assert_eq!(kind_of(&v.locale.regime.descriptor), NounKind::Place);
        assert_eq!(kind_of(&v.village.name), NounKind::Place);
        assert_eq!(kind_of("sky"), NounKind::Thing);
        for (noun, _) in &v.sky_bodies {
            assert_eq!(
                kind_of(noun),
                NounKind::Thing,
                "sky body {noun:?} must be a Thing"
            );
        }
        assert!(!f.nouns.iter().any(|n| n.kind == NounKind::Creature));
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
        // By HANDLE rather than by display — see
        // `rendered_noun_kinds_match_each_entrys_role` for why.
        let n = f
            .nouns
            .iter()
            .find(|n| n.matches(&v.locale.biome))
            .expect("the biome is reachable as a noun");
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
        // THE GLASSHOUSE, Stage B Task 4 (the thermostat) re-pin: day 0.0 ->
        // day 2.75. The damped, greenhouse-forced insolation baseline
        // re-places seed 42's flagship, and day 0.0 at the new site is a
        // persistently overcast night with no body visible at all (the flat
        // rain-deck blocks everything, not only the moon). Re-swept 0..100 in
        // 0.25-day steps: 46 of 400 sampled days show a moon; day 2.75 is the
        // first. Nothing about the claim changed — the night sky must name a
        // moon unambiguously whenever one is visible — only which day is a
        // clear-enough witness.
        let v = vantage_at(2.75);
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

//! Religion, tier 1: derive a pantheon from the salient phenomena a
//! community observes, structured by its society. This crate never learns
//! what produced a phenomenon — that ignorance is the trace protocol
//! working (spec §3.1.6).
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptDef, ConceptKind, ConceptRegistry, Correspondent, EntityId, Fact, LedgerError,
    Lexicalization, Manifest, Phenomenon, RegistryError, Value, Venue, Void, World,
};

/// Predicate marking an entity as a belief.
/// type-audit: bare-ok(identifier-text)
pub const IS_BELIEF: &str = "is-belief";
/// Predicate relating a belief to a community that holds it.
/// type-audit: bare-ok(identifier-text)
pub const HELD_BY: &str = "held-by";
/// Predicate giving a belief's tenet text. Retired: new genesis never
/// commits this fact (spec §6/§8 — structured content replaces it), but the
/// constant stays defined because pre-Tongues saves still carry `tenet`
/// facts and historiography still recounts them.
/// type-audit: bare-ok(identifier-text)
pub const TENET: &str = "tenet";
/// Predicate recording which phenomenon kind a belief mythologizes.
/// type-audit: bare-ok(identifier-text)
pub const DERIVED_FROM_PHENOMENON: &str = "derived-from-phenomenon";
/// Predicate: the presiding deity of a ranked pantheon (functional Flag).
/// type-audit: bare-ok(identifier-text)
pub const HIGH_GOD: &str = "high-god";
/// Predicate: the cult form of a belief — `organized` or `folk` (functional Text).
/// type-audit: bare-ok(identifier-text)
pub const CULT_FORM: &str = "cult-form";
/// Predicate: a belief's deity name, roman transcription (functional Text).
/// type-audit: bare-ok(identifier-text)
pub const DEITY_NAME: &str = "deity-name";
/// Predicate: a belief's deity name, IPA transcription (functional Text).
/// type-audit: bare-ok(identifier-text)
pub const DEITY_NAME_IPA: &str = "deity-name-ipa";
/// Predicate: a belief's epithet, roman transcription (functional Text).
/// type-audit: bare-ok(identifier-text)
pub const DEITY_EPITHET: &str = "deity-epithet";
/// Predicate: a belief's epithet, IPA transcription (functional Text).
/// type-audit: bare-ok(identifier-text)
pub const DEITY_EPITHET_IPA: &str = "deity-epithet-ipa";
/// Predicate: a belief's sentiment — `eternal`, `cyclic`, or `ambient`
/// (functional Text).
/// type-audit: bare-ok(identifier-text)
pub const SENTIMENT: &str = "sentiment";

/// Salience a phenomenon must reach to seat a deity in the pantheon.
const PANTHEON_FLOOR: f64 = 0.25;
/// Social strata at or above which the pantheon is ranked (a high god presides).
const RANKED_STRATA: usize = 4;

/// Every seed-derivation label this crate uses, with docs. Empty: religion
/// no longer draws from the seed directly — deity and epithet naming (and
/// the streams behind them) are the `DeityNamer` implementation's business,
/// which for language-backed names lives in `domains/language`.
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register religion's contribution to the concept registry.
///
/// The god/spirit concepts register through their correspondence [`Manifest`]:
/// each is a nameable supernatural presence, so its lexeme edge declares
/// `Expected`; religion emits no phenomenon kind for them, so the percept edge
/// is a `Gap`; and cognition voids to the future cognition wave.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_BELIEF, true, "subject is a belief")?;
    registry.register_predicate(HELD_BY, false, "a community holding a belief")?;
    registry.register_predicate(TENET, true, "the tenet text of a belief")?;
    registry.register_predicate(
        DERIVED_FROM_PHENOMENON,
        true,
        "phenomenon kind a belief mythologizes",
    )?;
    registry.register_predicate(HIGH_GOD, true, "the presiding deity of a ranked pantheon")?;
    registry.register_predicate(
        CULT_FORM,
        true,
        "the cult form of a belief (organized or folk)",
    )?;
    registry.register_predicate(DEITY_NAME, true, "a belief's deity name (roman)")?;
    registry.register_predicate(
        DEITY_NAME_IPA,
        true,
        "a belief's deity name (IPA transcription)",
    )?;
    registry.register_predicate(DEITY_EPITHET, true, "a belief's epithet (roman)")?;
    registry.register_predicate(
        DEITY_EPITHET_IPA,
        true,
        "a belief's epithet (IPA transcription)",
    )?;
    registry.register_predicate(
        SENTIMENT,
        true,
        "a belief's sentiment (eternal, cyclic, or ambient)",
    )?;

    for (name, doc) in [
        ("god", "a deity"),
        ("spirit", "a lesser or unseen supernatural presence"),
    ] {
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: name.to_string(),
                domain: "religion".to_string(),
                kind: ConceptKind::Social,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Present(Lexicalization::Expected),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }
    Ok(())
}

/// Religion as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Religion;

impl hornvale_kernel::Domain for Religion {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        crate::stream_labels()
    }
}

/// Authored kind-level traits of deity kinds — the religion-owned component.
/// Deliberately thin (spec §4.4): enrichment is content work for the
/// campaign that ships deity instances in worlds.
/// type-audit: bare-ok(flag)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DeityTraits {
    /// Whether the kind takes physical form in the world.
    pub manifest: bool,
}

/// The canonical deity-kind registry. One kind today: `deity`, unmanifest.
pub fn deity_registry() -> hornvale_kernel::ComponentStore<hornvale_kernel::KindId, DeityTraits> {
    [(
        hornvale_kernel::KindId("deity"),
        DeityTraits { manifest: false },
    )]
    .into_iter()
    .collect()
}

/// A summary of the flagship society's shape, mapped at the composition root
/// from its committed castes. Religion consumes this instead of importing
/// culture (the trace discipline).
/// type-audit: bare-ok(count: strata), bare-ok(flag: has_priesthood)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SocietySummary {
    /// Number of caste/role strata.
    pub strata: usize,
    /// Whether an organized priesthood (a shaman caste) exists.
    pub has_priesthood: bool,
}

/// A belief's felt relationship to the phenomenon it mythologizes —
/// watched (eternal), mourned-and-feasted (cyclic), or felt through the
/// ambient world rather than watched (ambient). Derived from the source
/// phenomenon's venue and periodicity (spec §6).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sentiment {
    /// An unchanging presence in the day or night sky: always watched.
    Eternal,
    /// A presence that departs and returns: mourned in absence, feasted on
    /// return.
    Cyclic,
    /// Felt through the ambient world (air, seasons) rather than watched.
    Ambient,
}

impl Sentiment {
    /// Derive a sentiment from a phenomenon's venue and periodicity:
    /// `Venue::Ambient` is always `Ambient`; otherwise an aperiodic
    /// phenomenon (`period_days: None`) is `Eternal` and a periodic one is
    /// `Cyclic`.
    pub fn of(phenomenon: &Phenomenon) -> Self {
        if phenomenon.venue == Venue::Ambient {
            Sentiment::Ambient
        } else if phenomenon.period_days.is_none() {
            Sentiment::Eternal
        } else {
            Sentiment::Cyclic
        }
    }

    /// The lowercase tag committed to the ledger's `sentiment` fact.
    /// type-audit: bare-ok(identifier-text)
    pub fn as_str(self) -> &'static str {
        match self {
            Sentiment::Eternal => "eternal",
            Sentiment::Cyclic => "cyclic",
            Sentiment::Ambient => "ambient",
        }
    }

    /// Parse the lowercase tag committed to the ledger's `sentiment` fact.
    /// `None` for anything else (a legacy save with no `sentiment` fact,
    /// for instance).
    fn parse(tag: &str) -> Option<Self> {
        match tag {
            "eternal" => Some(Sentiment::Eternal),
            "cyclic" => Some(Sentiment::Cyclic),
            "ambient" => Some(Sentiment::Ambient),
            _ => None,
        }
    }
}

/// Supplies a deity's name and epithet without religion knowing which
/// language — or whether any language at all — produced them (religion
/// stays language-agnostic, spec §6). Both methods return `(roman, ipa)`
/// pairs. `salt` is the minted belief entity's id: deterministic and
/// unique per deity, so an implementation can derive its own seed streams
/// from it without religion knowing anything about streams.
pub trait DeityNamer {
    /// A deity's name for the belief salted by `salt`.
    /// type-audit: pending(wave-3: salt), bare-ok(identifier-text: return)
    fn deity(&mut self, salt: u64) -> (String, String);
    /// An epithet for the belief salted by `salt`, fitting `sentiment`.
    /// type-audit: pending(wave-3: salt), bare-ok(identifier-text: return)
    fn epithet(&mut self, salt: u64, sentiment: Sentiment) -> (String, String);
}

/// A belief as this domain knows it.
/// type-audit: bare-ok(identifier-text: deity), bare-ok(identifier-text: epithet), bare-ok(identifier-text: source_kind), bare-ok(flag: high_god)
#[derive(Debug, Clone, PartialEq)]
pub struct Belief {
    /// The belief's entity id.
    pub id: EntityId,
    /// The deity's name (roman transcription).
    pub deity: String,
    /// The deity's epithet (roman transcription).
    pub epithet: String,
    /// The phenomenon kind it mythologizes.
    pub source_kind: String,
    /// The belief's sentiment (eternal, cyclic, or ambient).
    pub sentiment: Sentiment,
    /// Whether this belief is the pantheon's presiding high god.
    pub high_god: bool,
}

/// Tier-1 genesis: derive a pantheon from the salient phenomena the community
/// observes, structured by its society. One deity per phenomenon at or above
/// `PANTHEON_FLOOR` (or the single most salient if none clear it); the most
/// salient deity presides (`high-god`) in a stratified society; the cult form
/// follows the priesthood. Phenomena arrive salience-descending. Returns the
/// pantheon in that order (element 0 is the head).
///
/// Emits structured content, not prose (spec §6): each member phenomenon's
/// `Sentiment` is derived, then `names` is asked for the deity's name and an
/// epithet fitting that sentiment (salted by the freshly minted belief's
/// entity id — deterministic, unique per deity). The rendered tenet is a
/// downstream concern (Task 8); this crate commits meaning, never prose.
pub fn genesis(
    world: &mut World,
    community: EntityId,
    phenomena: &[Phenomenon],
    society: &SocietySummary,
    names: &mut dyn DeityNamer,
) -> Result<Vec<EntityId>, LedgerError> {
    // Members: everything above the floor; else the single most salient; else none.
    let above = phenomena
        .iter()
        .filter(|p| p.salience >= PANTHEON_FLOOR)
        .count();
    let take = if above > 0 {
        above
    } else {
        phenomena.len().min(1)
    };
    let members = &phenomena[..take];
    if members.is_empty() {
        return Ok(Vec::new());
    }

    let ranked = society.strata >= RANKED_STRATA;
    let cult_form = if society.has_priesthood {
        "organized"
    } else {
        "folk"
    };

    let mut ids = Vec::with_capacity(members.len());
    for (i, p) in members.iter().enumerate() {
        let sentiment = Sentiment::of(p);

        let belief = world.ledger.mint_entity();
        let salt = belief.get();
        let (deity_name, deity_ipa) = names.deity(salt);
        let (epithet, epithet_ipa) = names.epithet(salt, sentiment);

        let fact = |predicate: &str, object: Value| Fact {
            subject: belief,
            predicate: predicate.to_string(),
            object,
            place: None,
            day: Some(0.0),
            provenance: "religion".to_string(),
        };
        world
            .ledger
            .commit(fact(IS_BELIEF, Value::Flag(true)), &world.registry)?;
        world
            .ledger
            .commit(fact(DEITY_NAME, Value::Text(deity_name)), &world.registry)?;
        world.ledger.commit(
            fact(DEITY_NAME_IPA, Value::Text(deity_ipa)),
            &world.registry,
        )?;
        world
            .ledger
            .commit(fact(DEITY_EPITHET, Value::Text(epithet)), &world.registry)?;
        world.ledger.commit(
            fact(DEITY_EPITHET_IPA, Value::Text(epithet_ipa)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(SENTIMENT, Value::Text(sentiment.as_str().to_string())),
            &world.registry,
        )?;
        world
            .ledger
            .commit(fact(HELD_BY, Value::Entity(community)), &world.registry)?;
        world.ledger.commit(
            fact(DERIVED_FROM_PHENOMENON, Value::Text(p.kind.clone())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(CULT_FORM, Value::Text(cult_form.to_string())),
            &world.registry,
        )?;
        if ranked && i == 0 {
            world
                .ledger
                .commit(fact(HIGH_GOD, Value::Flag(true)), &world.registry)?;
        }
        ids.push(belief);
    }
    Ok(ids)
}

/// Every belief in the world, in commit order. A belief with no `sentiment`
/// fact (a legacy, pre-Tongues save) reads as `Sentiment::Ambient` here —
/// its raw `tenet` fact is still available and still recounted by
/// historiography, which reads facts directly rather than through this
/// structured view.
pub fn beliefs_of(world: &World) -> Vec<Belief> {
    world
        .ledger
        .find(IS_BELIEF)
        .map(|f| f.subject)
        .map(|id| Belief {
            id,
            deity: world
                .ledger
                .text_of(id, DEITY_NAME)
                .map(str::to_string)
                .unwrap_or_default(),
            epithet: world
                .ledger
                .text_of(id, DEITY_EPITHET)
                .map(str::to_string)
                .unwrap_or_default(),
            source_kind: world
                .ledger
                .text_of(id, DERIVED_FROM_PHENOMENON)
                .map(str::to_string)
                .unwrap_or_default(),
            sentiment: world
                .ledger
                .text_of(id, SENTIMENT)
                .and_then(Sentiment::parse)
                .unwrap_or(Sentiment::Ambient),
            high_god: matches!(world.ledger.value_of(id, HIGH_GOD), Some(Value::Flag(true))),
        })
        .collect()
}

/// The cult form shared by the world's pantheon (`organized`/`folk`), read
/// from the first belief; `None` if there are no beliefs.
/// type-audit: bare-ok(identifier-text)
pub fn cult_form_of(world: &World) -> Option<String> {
    let first = world.ledger.find(IS_BELIEF).next()?.subject;
    match world.ledger.value_of(first, CULT_FORM) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

/// The beliefs held by one community, in commit order (element 0 is the
/// pantheon's most salient deity — its head where one presides).
pub fn beliefs_held_by(world: &World, community: EntityId) -> Vec<Belief> {
    beliefs_of(world)
        .into_iter()
        .filter(|b| {
            matches!(
                world.ledger.value_of(b.id, HELD_BY),
                Some(Value::Entity(c)) if *c == community
            )
        })
        .collect()
}

/// The cult form of one community's pantheon, from its first belief.
/// type-audit: bare-ok(identifier-text)
pub fn cult_form_held_by(world: &World, community: EntityId) -> Option<String> {
    let first = beliefs_held_by(world, community).into_iter().next()?;
    match world.ledger.value_of(first.id, CULT_FORM) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    /// A deterministic test `DeityNamer`: `("XarN", "xarN")` for deities,
    /// `("EpithetN(sentiment)", "epithetN")` for epithets, where `N` is the
    /// salt. Distinct salts (minted belief ids) always yield distinct pairs.
    struct StubNamer;

    impl DeityNamer for StubNamer {
        fn deity(&mut self, salt: u64) -> (String, String) {
            (format!("Xar{salt}"), format!("xar{salt}"))
        }

        fn epithet(&mut self, salt: u64, sentiment: Sentiment) -> (String, String) {
            (
                format!("the {:?} Epithet{salt}", sentiment),
                format!("epithet{salt}"),
            )
        }
    }

    fn world(seed: u64) -> (World, EntityId) {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let community = w.ledger.mint_entity();
        (w, community)
    }

    fn ph(kind: &str, desc: &str, period: Option<f64>, salience: f64, venue: Venue) -> Phenomenon {
        Phenomenon {
            kind: kind.to_string(),
            description: desc.to_string(),
            period_days: period,
            salience,
            venue,
        }
    }

    // Pre-sorted salience-descending, as kernel::observe delivers.
    fn sky() -> Vec<Phenomenon> {
        vec![
            ph("celestial-body", "the sun", None, 1.0, Venue::DaySky), // eternal
            ph(
                "seasonal-cycle",
                "the seasons",
                Some(365.0),
                0.5,
                Venue::Ambient,
            ), // ambient venue wins regardless of period
            ph("celestial-body", "a moon", Some(29.0), 0.4, Venue::NightSky), // cyclic
            ph("ambient", "still air", None, 0.15, Venue::Ambient),    // below floor
        ]
    }

    fn society() -> SocietySummary {
        SocietySummary {
            strata: 5,
            has_priesthood: true,
        }
    }

    #[test]
    fn concepts_registered() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        for name in ["god", "spirit"] {
            let c = r
                .concept(name)
                .unwrap_or_else(|| panic!("missing concept {name}"));
            assert_eq!(c.domain, "religion");
            assert_eq!(c.kind, ConceptKind::Social);
        }
    }

    #[test]
    fn pantheon_takes_every_phenomenon_above_the_floor() {
        let (mut w, c) = world(42);
        let ids = genesis(&mut w, c, &sky(), &society(), &mut StubNamer).unwrap();
        assert_eq!(
            ids.len(),
            3,
            "sun+seasons+moon are above 0.25; ambient (0.15) is not"
        );
        // No cap, but the sub-floor ambient is excluded.
        assert!(beliefs_of(&w).iter().all(|b| b.source_kind != "ambient"));
    }

    #[test]
    fn a_ranked_society_crowns_the_most_salient_deity() {
        let (mut w, c) = world(42);
        let ids = genesis(&mut w, c, &sky(), &society(), &mut StubNamer).unwrap();
        let beliefs = beliefs_of(&w);
        assert!(
            beliefs[0].high_god,
            "top-salience deity presides in a stratified society"
        );
        assert_eq!(
            beliefs.iter().filter(|b| b.high_god).count(),
            1,
            "exactly one high god"
        );
        assert_eq!(beliefs[0].id, ids[0]);
    }

    #[test]
    fn a_flat_society_has_no_high_god() {
        let (mut w, c) = world(42);
        let flat = SocietySummary {
            strata: 2,
            has_priesthood: false,
        };
        let ids = genesis(&mut w, c, &sky(), &flat, &mut StubNamer).unwrap();
        let _ = ids;
        assert!(
            beliefs_of(&w).iter().all(|b| !b.high_god),
            "egalitarian society: flat pantheon"
        );
    }

    #[test]
    fn priesthood_sets_the_cult_form() {
        let (mut w, c) = world(42);
        genesis(&mut w, c, &sky(), &society(), &mut StubNamer).unwrap();
        assert_eq!(cult_form_of(&w).as_deref(), Some("organized"));
        let (mut w2, c2) = world(42);
        let flat = SocietySummary {
            strata: 2,
            has_priesthood: false,
        };
        genesis(&mut w2, c2, &sky(), &flat, &mut StubNamer).unwrap();
        assert_eq!(cult_form_of(&w2).as_deref(), Some("folk"));
    }

    #[test]
    fn beliefs_carry_deity_epithet_and_sentiment_no_tenet() {
        let (mut w, c) = world(42);
        // Locked sky: eternal sun (day sky, aperiodic), cyclic moon (night
        // sky, periodic).
        let locked = vec![
            ph("celestial-body", "a fixed sun", None, 1.0, Venue::DaySky),
            ph("celestial-body", "a moon", Some(29.0), 0.4, Venue::NightSky),
        ];
        let ids = genesis(&mut w, c, &locked, &society(), &mut StubNamer).unwrap();
        let beliefs = beliefs_of(&w);
        let head = &beliefs[0];
        assert!(!head.deity.is_empty(), "deity name committed");
        assert!(!head.epithet.is_empty(), "epithet committed");
        assert_eq!(
            head.sentiment,
            Sentiment::Eternal,
            "an aperiodic day-sky sun is eternal"
        );
        assert_eq!(
            beliefs[1].sentiment,
            Sentiment::Cyclic,
            "a periodic moon is cyclic"
        );

        // No tenet fact is committed for any minted belief.
        for id in &ids {
            assert!(
                w.ledger.text_of(*id, TENET).is_none(),
                "genesis must not commit a tenet fact"
            );
        }
    }

    #[test]
    fn ambient_venue_phenomena_yield_ambient_sentiment_regardless_of_period() {
        let (mut w, c) = world(42);
        let seasons = vec![ph(
            "seasonal-cycle",
            "the seasons",
            Some(365.0),
            1.0,
            Venue::Ambient,
        )];
        genesis(&mut w, c, &seasons, &society(), &mut StubNamer).unwrap();
        assert_eq!(beliefs_of(&w)[0].sentiment, Sentiment::Ambient);
    }

    #[test]
    fn empty_phenomena_means_no_pantheon() {
        let (mut w, c) = world(42);
        assert!(
            genesis(&mut w, c, &[], &society(), &mut StubNamer)
                .unwrap()
                .is_empty()
        );
        assert!(beliefs_of(&w).is_empty());
    }

    #[test]
    fn below_floor_only_still_yields_the_single_most_salient() {
        let (mut w, c) = world(42);
        let faint = vec![
            ph("ambient", "a whisper of air", None, 0.15, Venue::Ambient),
            ph("celestial-body", "a dim star", None, 0.1, Venue::DaySky),
        ];
        let flat = SocietySummary {
            strata: 2,
            has_priesthood: false,
        };
        let ids = genesis(&mut w, c, &faint, &flat, &mut StubNamer).unwrap();
        assert_eq!(ids.len(), 1, "never godless while something is observed");
        assert_eq!(beliefs_of(&w)[0].source_kind, "ambient");
    }

    #[test]
    fn genesis_is_deterministic() {
        let run = || {
            let (mut w, c) = world(7);
            genesis(&mut w, c, &sky(), &society(), &mut StubNamer).unwrap();
            beliefs_of(&w)
                .iter()
                .map(|b| (b.deity.clone(), b.epithet.clone(), b.sentiment))
                .collect::<Vec<_>>()
        };
        assert_eq!(run(), run());
    }

    #[test]
    fn each_deity_is_salted_by_its_own_belief_id() {
        let (mut w, c) = world(42);
        let c2 = w.ledger.mint_entity();
        genesis(&mut w, c, &sky(), &society(), &mut StubNamer).unwrap();
        genesis(&mut w, c2, &sky(), &society(), &mut StubNamer).unwrap();
        let all = beliefs_of(&w);
        let held_c = beliefs_held_by(&w, c);
        let held_c2 = beliefs_held_by(&w, c2);
        assert_eq!(held_c.len() + held_c2.len(), all.len());
        assert_eq!(held_c.len(), 3);
        assert_eq!(held_c2.len(), 3);
        // Different belief entities → different salts → different names,
        // even though both communities see the identical phenomena.
        let deities = |bs: &[Belief]| bs.iter().map(|b| b.deity.clone()).collect::<Vec<_>>();
        assert_ne!(
            deities(&held_c),
            deities(&held_c2),
            "distinct belief ids must salt distinct deity names"
        );
    }

    #[test]
    fn cult_form_is_read_per_community() {
        let (mut w, c) = world(42);
        let c2 = w.ledger.mint_entity();
        genesis(&mut w, c, &sky(), &society(), &mut StubNamer).unwrap();
        let flat = SocietySummary {
            strata: 2,
            has_priesthood: false,
        };
        genesis(&mut w, c2, &sky(), &flat, &mut StubNamer).unwrap();
        assert_eq!(cult_form_held_by(&w, c).as_deref(), Some("organized"));
        assert_eq!(cult_form_held_by(&w, c2).as_deref(), Some("folk"));
    }
}

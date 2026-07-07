//! Religion, tier 1: derive a pantheon from the salient phenomena a
//! community observes, structured by its society. This crate never learns
//! what produced a phenomenon — that ignorance is the trace protocol
//! working (spec §3.1.6).
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, Phenomenon, RegistryError, Value, World,
};

/// Predicate marking an entity as a belief.
pub const IS_BELIEF: &str = "is-belief";
/// Predicate relating a belief to a community that holds it.
pub const HELD_BY: &str = "held-by";
/// Predicate giving a belief's tenet text.
pub const TENET: &str = "tenet";
/// Predicate recording which phenomenon kind a belief mythologizes.
pub const DERIVED_FROM_PHENOMENON: &str = "derived-from-phenomenon";
/// Predicate: the presiding deity of a ranked pantheon (functional Flag).
pub const HIGH_GOD: &str = "high-god";
/// Predicate: the cult form of a belief — `organized` or `folk` (functional Text).
pub const CULT_FORM: &str = "cult-form";

/// Seed-derivation labels used by this crate (permanent contracts).
mod streams {
    /// Root stream label for religion.
    pub const ROOT: &str = "religion";
    /// Epithet-pick stream.
    pub const EPITHET: &str = "epithet";
}

/// Salience a phenomenon must reach to seat a deity in the pantheon.
const PANTHEON_FLOOR: f64 = 0.25;
/// Social strata at or above which the pantheon is ranked (a high god presides).
const RANKED_STRATA: usize = 4;

// Extended append-only: the first three of each pool are unchanged from tier 0.
const ETERNAL_EPITHETS: [&str; 6] = [
    "the Unblinking Eye",
    "the Ever-Flame",
    "the Gold Warden",
    "the Still Crown",
    "the Deathless Watch",
    "the Fixed Star",
];
const CYCLIC_EPITHETS: [&str; 6] = [
    "the Returning One",
    "the Tidewalker",
    "the Promised Lamp",
    "the Wheel-Turner",
    "the Waning Herald",
    "the Patient Pilgrim",
];

/// Every seed-derivation label this crate uses, with docs.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("religion", "root stream for religion generation"),
        ("religion/epithet", "deity epithet pick"),
    ]
}

/// Register religion's contribution to the concept registry.
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
    )
}

/// A summary of the flagship society's shape, mapped at the composition root
/// from its committed castes. Religion consumes this instead of importing
/// culture (the trace discipline).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SocietySummary {
    /// Number of caste/role strata.
    pub strata: usize,
    /// Whether an organized priesthood (a shaman caste) exists.
    pub has_priesthood: bool,
}

/// A belief as this domain knows it.
#[derive(Debug, Clone, PartialEq)]
pub struct Belief {
    /// The belief's entity id.
    pub id: EntityId,
    /// The belief's tenet text.
    pub tenet: String,
    /// The phenomenon kind it mythologizes.
    pub source_kind: String,
    /// Whether this belief is the pantheon's presiding high god.
    pub high_god: bool,
}

/// Tier-1 genesis: derive a pantheon from the salient phenomena the community
/// observes, structured by its society. One deity per phenomenon at or above
/// `PANTHEON_FLOOR` (or the single most salient if none clear it); the most
/// salient deity presides (`high-god`) in a stratified society; the cult form
/// follows the priesthood. Phenomena arrive salience-descending. Returns the
/// pantheon in that order (element 0 is the head).
pub fn genesis(
    world: &mut World,
    community: EntityId,
    phenomena: &[Phenomenon],
    society: &SocietySummary,
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
    let mut stream = world
        .seed
        .derive(streams::ROOT)
        .derive(streams::EPITHET)
        .stream();

    // Distinct epithets within a pantheon: draw an index, advance past any
    // already used (deterministic; pools of 6 cover the realistic pantheon
    // size — if ever exhausted, repeats are allowed rather than failing).
    let mut used: Vec<&'static str> = Vec::new();
    let mut ids = Vec::with_capacity(members.len());
    for (i, p) in members.iter().enumerate() {
        let pool: &[&'static str] = match p.period_days {
            None => &ETERNAL_EPITHETS,
            Some(_) => &CYCLIC_EPITHETS,
        };
        let start = stream.range_u32(0, pool.len() as u32 - 1) as usize;
        let mut epithet = pool[start];
        for step in 0..pool.len() {
            let candidate = pool[(start + step) % pool.len()];
            if !used.contains(&candidate) {
                epithet = candidate;
                break;
            }
        }
        used.push(epithet);

        let tenet = match p.period_days {
            None => format!(
                "{epithet} is {}; it has never departed and will never blink.",
                p.description
            ),
            Some(period) => format!(
                "{epithet} departs and returns every {period} days; its absences are mourned \
                 and its returns feasted."
            ),
        };

        let belief = world.ledger.mint_entity();
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
            .commit(fact(TENET, Value::Text(tenet)), &world.registry)?;
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

/// Every belief in the world, in commit order.
pub fn beliefs_of(world: &World) -> Vec<Belief> {
    world
        .ledger
        .find(IS_BELIEF)
        .map(|f| f.subject)
        .map(|id| Belief {
            id,
            tenet: world
                .ledger
                .text_of(id, TENET)
                .map(str::to_string)
                .unwrap_or_default(),
            source_kind: world
                .ledger
                .text_of(id, DERIVED_FROM_PHENOMENON)
                .map(str::to_string)
                .unwrap_or_default(),
            high_god: matches!(world.ledger.value_of(id, HIGH_GOD), Some(Value::Flag(true))),
        })
        .collect()
}

/// The cult form shared by the world's pantheon (`organized`/`folk`), read
/// from the first belief; `None` if there are no beliefs.
pub fn cult_form_of(world: &World) -> Option<String> {
    let first = world.ledger.find(IS_BELIEF).next()?.subject;
    match world.ledger.value_of(first, CULT_FORM) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn world(seed: u64) -> (World, EntityId) {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let community = w.ledger.mint_entity();
        (w, community)
    }

    fn ph(kind: &str, desc: &str, period: Option<f64>, salience: f64) -> Phenomenon {
        Phenomenon {
            kind: kind.to_string(),
            description: desc.to_string(),
            period_days: period,
            salience,
        }
    }

    // Pre-sorted salience-descending, as kernel::observe delivers.
    fn sky() -> Vec<Phenomenon> {
        vec![
            ph("celestial-body", "the sun", None, 1.0), // eternal
            ph("seasonal-cycle", "the seasons", Some(365.0), 0.5),
            ph("celestial-body", "a moon", Some(29.0), 0.4),
            ph("ambient", "still air", None, 0.15), // below floor
        ]
    }

    #[test]
    fn pantheon_takes_every_phenomenon_above_the_floor() {
        let (mut w, c) = world(42);
        let society = SocietySummary {
            strata: 5,
            has_priesthood: true,
        };
        let ids = genesis(&mut w, c, &sky(), &society).unwrap();
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
        let ids = genesis(
            &mut w,
            c,
            &sky(),
            &SocietySummary {
                strata: 5,
                has_priesthood: true,
            },
        )
        .unwrap();
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
        let ids = genesis(
            &mut w,
            c,
            &sky(),
            &SocietySummary {
                strata: 2,
                has_priesthood: false,
            },
        )
        .unwrap();
        let _ = ids;
        assert!(
            beliefs_of(&w).iter().all(|b| !b.high_god),
            "egalitarian society: flat pantheon"
        );
    }

    #[test]
    fn priesthood_sets_the_cult_form() {
        let (mut w, c) = world(42);
        genesis(
            &mut w,
            c,
            &sky(),
            &SocietySummary {
                strata: 5,
                has_priesthood: true,
            },
        )
        .unwrap();
        assert_eq!(cult_form_of(&w).as_deref(), Some("organized"));
        let (mut w2, c2) = world(42);
        genesis(
            &mut w2,
            c2,
            &sky(),
            &SocietySummary {
                strata: 2,
                has_priesthood: false,
            },
        )
        .unwrap();
        assert_eq!(cult_form_of(&w2).as_deref(), Some("folk"));
    }

    #[test]
    fn tenets_track_periodicity_and_a_locked_sun_is_eternal() {
        let (mut w, c) = world(42);
        // Locked sky: eternal sun, no seasons.
        let locked = vec![
            ph("celestial-body", "a fixed sun", None, 1.0),
            ph("celestial-body", "a moon", Some(29.0), 0.4),
        ];
        genesis(
            &mut w,
            c,
            &locked,
            &SocietySummary {
                strata: 5,
                has_priesthood: true,
            },
        )
        .unwrap();
        let head = &beliefs_of(&w)[0];
        assert!(
            head.tenet.contains("never"),
            "eternal sun yields an eternal tenet"
        );
    }

    #[test]
    fn empty_phenomena_means_no_pantheon() {
        let (mut w, c) = world(42);
        assert!(
            genesis(
                &mut w,
                c,
                &[],
                &SocietySummary {
                    strata: 5,
                    has_priesthood: true,
                }
            )
            .unwrap()
            .is_empty()
        );
        assert!(beliefs_of(&w).is_empty());
    }

    #[test]
    fn below_floor_only_still_yields_the_single_most_salient() {
        let (mut w, c) = world(42);
        let faint = vec![
            ph("ambient", "a whisper of air", None, 0.15),
            ph("celestial-body", "a dim star", None, 0.1),
        ];
        let ids = genesis(
            &mut w,
            c,
            &faint,
            &SocietySummary {
                strata: 2,
                has_priesthood: false,
            },
        )
        .unwrap();
        assert_eq!(ids.len(), 1, "never godless while something is observed");
        assert_eq!(beliefs_of(&w)[0].source_kind, "ambient");
    }

    #[test]
    fn genesis_is_deterministic() {
        let run = || {
            let (mut w, c) = world(7);
            genesis(
                &mut w,
                c,
                &sky(),
                &SocietySummary {
                    strata: 5,
                    has_priesthood: true,
                },
            )
            .unwrap();
            beliefs_of(&w)
                .iter()
                .map(|b| b.tenet.clone())
                .collect::<Vec<_>>()
        };
        assert_eq!(run(), run());
    }
}

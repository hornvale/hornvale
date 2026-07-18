//! The Chorus (C4): derive every placed culture's [`AccountParams`] from
//! existing authored/committed state — the `voice_params` twin — plus the
//! authored observability table (spec §3.2) and the ground fact list, then
//! run each culture's account through `hornvale_language::account_of`.
//!
//! Nothing here is new draws or new save state: [`account_params_of`] reads
//! the already-built [`WorldComponents`] and the already-committed ledger;
//! [`chorus_ground`] mirrors `windows/book`'s `render_volume` fact
//! selection exactly (layering forbids importing it — `windows/book`
//! depends on `windows/worldgen`, not the reverse). See each function's own
//! doc comment for its derivation rule.

use crate::{BuildError, WorldComponents};
use hornvale_kernel::{World, world::IS_A};
use hornvale_language::{
    Account, AccountParams, GroundFact, NeededConcept, Observability, OrderPolicy, Requirement,
    Stance, account_of,
};
use hornvale_species::{ActivityCycle, PerceptionVector};
use std::collections::{BTreeMap, BTreeSet};

/// The authored observability table (build-state; spec §3.2): what it
/// takes for an observing culture to retain each ground predicate's facts.
/// Keyed by predicate text — `hornvale_language::account_of`'s table lookup
/// is a plain string key, matched against [`chorus_ground`]'s
/// `GroundFact::predicate` values with no registry round-trip. The three
/// astronomy predicates use their domain's own constants rather than string
/// literals; `is-a` and `instance-of` use the kernel's world-fact
/// constants.
pub fn observability_table() -> BTreeMap<String, Observability> {
    let mut table = BTreeMap::new();
    table.insert(
        IS_A.to_string(),
        Observability {
            requirement: Requirement::Taxonomic,
            domain: "sky",
            concept: NeededConcept::Object,
        },
    );
    table.insert(
        hornvale_astronomy::facts::MOON_COUNT.to_string(),
        Observability {
            requirement: Requirement::SkyGraded { threshold: 0.6 },
            domain: "sky",
            concept: NeededConcept::Fixed("moon"),
        },
    );
    table.insert(
        hornvale_astronomy::facts::STAR_CLASS.to_string(),
        Observability {
            requirement: Requirement::Instrumental,
            domain: "sky",
            concept: NeededConcept::Fixed("star"),
        },
    );
    table.insert(
        hornvale_astronomy::facts::DAY_LENGTH_STD.to_string(),
        Observability {
            requirement: Requirement::CrossReferential,
            domain: "sky",
            concept: NeededConcept::Fixed("sun"),
        },
    );
    table.insert(
        hornvale_kernel::INSTANCE_OF.to_string(),
        Observability {
            requirement: Requirement::Manifest,
            domain: "peoples",
            concept: NeededConcept::ObjectKind,
        },
    );
    table
}

/// A culture's sky-observation capability, in `[0, 1]`: the mean of night
/// vision and sky attention, plus an activity-cycle bonus (`Nocturnal`
/// 0.15, `Crepuscular` 0.08, `Diurnal` 0.0), clamped to 1.0. Gates
/// [`hornvale_language::Requirement::SkyGraded`] facts in the observability
/// table above.
///
/// **TECH-1 seam**: the capability derivation is a seam — a future tech
/// ladder (instruments, recorded observation) replaces this function
/// without touching the filter contract (`Requirement`/[`Observability`]
/// stay exactly as authored; only how a culture's `sky_capability` scalar
/// is computed would change).
pub fn sky_capability(p: &PerceptionVector) -> f64 {
    let bonus = match p.activity {
        ActivityCycle::Nocturnal => 0.15,
        ActivityCycle::Crepuscular => 0.08,
        ActivityCycle::Diurnal => 0.0,
    };
    ((p.night_vision + p.sky_attention) / 2.0 + bonus).min(1.0)
}

/// Derive `species`'s [`AccountParams`] from existing authored/committed
/// state only: no new draws, no new ledger writes, no new save-format
/// state.
///
/// **Derived, never stored** (LANG-36): re-running this over the same
/// world and species always reconstructs the same `AccountParams`
/// byte-for-byte; a caller must never cache or serialize the result — the
/// world's registry/psyche/perception components and the ledger's
/// committed facts are the only durable state.
///
/// - `holdings`: every concept [`crate::exposure_of`] classes `Steeped` or
///   `KnowsOf` (a `KnowsOf` compound is still a held word).
/// - `sky_capability`: [`sky_capability`] over the species' perception
///   vector.
/// - `order`: `Salience { sky_first: sky_attention >= 0.6 }`.
/// - `stances`: for each placed people (`k`), `Ourselves` for `species`
///   itself; otherwise, read from `species`'s OWN psychology (not `k`'s) —
///   `in_group_radius >= 0.5` → `Neighbors`, else `threat_response >= 0.5` →
///   `Rivals`, else `Strangers`.
/// - `world_carving`: `Some("earth")` iff holdings contain `"earth"` (read
///   from holdings, never hardcoded — the universal stratum sits at the
///   floor for every culture today, but this must track holdings, not
///   assume them).
/// - `hold_all: false` always; the real [`observability_table`].
pub fn account_params_of(world: &World, species: &str) -> Result<AccountParams, BuildError> {
    let wc = WorldComponents::assemble()?;
    let perception = wc.perception.get_by_label(species).ok_or_else(|| {
        BuildError::MalformedKind(format!(
            "'{species}' carries no perception component (not a peopled kind)"
        ))
    })?;
    let psyche = wc.psyche.get_by_label(species).ok_or_else(|| {
        BuildError::MalformedKind(format!(
            "'{species}' carries no psyche component (not a peopled kind)"
        ))
    })?;

    let exposure = crate::exposure_of(world, species)?;
    let holdings: BTreeSet<String> = exposure
        .iter()
        .filter(|(_, class)| {
            matches!(
                class,
                hornvale_language::ExposureClass::Steeped
                    | hornvale_language::ExposureClass::KnowsOf
            )
        })
        .map(|(concept, _)| concept.clone())
        .collect();

    let mut stances = BTreeMap::new();
    for (kind, _village) in crate::placed_peoples(world) {
        let stance = if kind == species {
            Stance::Ourselves
        } else if psyche.in_group_radius >= 0.5 {
            Stance::Neighbors
        } else if psyche.threat_response >= 0.5 {
            Stance::Rivals
        } else {
            Stance::Strangers
        };
        stances.insert(kind.to_string(), stance);
    }

    let world_carving = holdings.contains("earth").then(|| "earth".to_string());

    Ok(AccountParams {
        hold_all: false,
        holdings,
        observability: observability_table(),
        sky_capability: sky_capability(perception),
        order: OrderPolicy::Salience {
            sky_first: perception.sky_attention >= 0.6,
        },
        stances,
        world_carving,
    })
}

/// The gibberish pole (the dial's pathological end, deterministic
/// build-state, independent of any specific world): `hold_all: false`,
/// empty holdings, the real table with every requirement rewritten —
/// `Taxonomic` for the three text-valued predicates (`is-a`, `star-class`,
/// `instance-of`), `Instrumental` for the two numeric ones (`moon-count`,
/// `day-length-std`) — `sky_capability: 0.0`, `order: Salience { sky_first:
/// true }`, every peopled kind's stance `Rivals`, `world_carving:
/// Some("earth")`. Every text-valued fact substitutes to the SAME target
/// under `Taxonomic` + `world_carving: Some("earth")` — an injectivity
/// collision that drives [`hornvale_language::recoverability`] to 0.0 —
/// while both numeric facts are simply beyond capability.
pub fn pathological_params() -> AccountParams {
    let mut observability = observability_table();
    for (predicate, obs) in observability.iter_mut() {
        let text_valued = predicate == IS_A
            || predicate == hornvale_astronomy::facts::STAR_CLASS
            || predicate == hornvale_kernel::INSTANCE_OF;
        obs.requirement = if text_valued {
            Requirement::Taxonomic
        } else {
            Requirement::Instrumental
        };
    }

    let stances = hornvale_species::psyche_registry()
        .ids()
        .map(|k| (k.0.to_string(), Stance::Rivals))
        .collect();

    AccountParams {
        hold_all: false,
        holdings: BTreeSet::new(),
        observability,
        sky_capability: 0.0,
        order: OrderPolicy::Salience { sky_first: true },
        stances,
        world_carving: Some("earth".to_string()),
    }
}

/// Resolve `entity`'s ground-fact subject text: its resolved `name`, or the
/// `Entity {id}` fallback — mirrors `windows/book::render_volume`'s subject
/// resolution exactly (see the module doc's layering note on why this
/// can't just call that function).
fn subject_name(world: &World, entity: hornvale_kernel::EntityId) -> String {
    world
        .ledger
        .text_of(entity, hornvale_kernel::NAME)
        .map(str::to_string)
        .unwrap_or_else(|| format!("Entity {}", entity.0))
}

/// The world's ground truth, flattened to [`GroundFact`]s for [`account_of`]
/// — mirrors `windows/book::render_volume`'s fact selection exactly (same
/// facts, same order; Task 4's null-filter-law test pins the two against
/// each other): one `is-a` `GroundFact` per classified subject, then one
/// `GroundFact` per [`hornvale_astronomy::facts`] construction predicate
/// with a committed value on that subject (moon-count, star-class,
/// day-length-std — `render_volume`'s `CONSTRUCTION_ORDER`, copied here
/// since `windows/book` cannot be imported from `windows/worldgen`,
/// layering runs the other way), then one `GroundFact` per `instance-of`
/// fact (subject = the collective's resolved name, no "The " prefix; object
/// = the kind text).
pub fn chorus_ground(world: &World) -> Vec<GroundFact> {
    // The mirror obligation: this order must match
    // `windows/book::render_volume`'s `CONSTRUCTION_ORDER` exactly.
    const CONSTRUCTION_ORDER: [&str; 3] = [
        hornvale_astronomy::facts::MOON_COUNT,
        hornvale_astronomy::facts::STAR_CLASS,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
    ];

    let mut ground = Vec::new();
    for fact in world.ledger.find(IS_A) {
        let hornvale_kernel::Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject_entity = fact.subject;
        let name = subject_name(world, subject_entity);
        ground.push(GroundFact {
            subject: name.clone(),
            predicate: IS_A.to_string(),
            object: hornvale_kernel::Value::Text(kind.clone()),
        });
        for predicate in CONSTRUCTION_ORDER {
            if let Some(value) = world.ledger.value_of(subject_entity, predicate) {
                ground.push(GroundFact {
                    subject: name.clone(),
                    predicate: predicate.to_string(),
                    object: value.clone(),
                });
            }
        }
    }

    for fact in world.ledger.find(hornvale_kernel::INSTANCE_OF) {
        let hornvale_kernel::Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject_entity = fact.subject;
        let name = subject_name(world, subject_entity);
        ground.push(GroundFact {
            subject: name,
            predicate: hornvale_kernel::INSTANCE_OF.to_string(),
            object: hornvale_kernel::Value::Text(kind.clone()),
        });
    }

    ground
}

/// One placed people's rendered account: its kind label, the derived
/// [`AccountParams`] that produced it, and the resulting [`Account`].
#[derive(Debug)]
pub struct ChorusVoice {
    /// The people's kind label (e.g. `"goblin"`).
    pub kind: String,
    /// The params [`account_params_of`] derived for this kind.
    pub params: AccountParams,
    /// The account [`account_of`] produced from those params over
    /// [`chorus_ground`].
    pub account: Account,
}

/// Every placed people's account over the world's ground truth, in
/// [`crate::placed_peoples`] order. Mirrors `render_volume`'s posture on a
/// param-derivation failure: skip that kind rather than fail the whole
/// pass (`else { continue }`).
pub fn accounts_of(world: &World) -> Vec<ChorusVoice> {
    let ground = chorus_ground(world);
    crate::placed_peoples(world)
        .into_iter()
        .filter_map(|(kind, _village)| {
            let params = account_params_of(world, kind).ok()?;
            let account = account_of(&ground, &params);
            Some(ChorusVoice {
                kind: kind.to_string(),
                params,
                account,
            })
        })
        .collect()
}

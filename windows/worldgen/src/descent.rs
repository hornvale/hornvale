//! The person-descent graph (The Namesake, spec §3.1): a pure reprojection
//! of the committed community tree into relations between individuals.
//!
//! This lives at the composition root and nowhere else, for the
//! constitutional reason the history bake does: it must read
//! `hornvale-history` (the occupation facts) and `hornvale-species` (the
//! allometric generation length) together, and a domain may depend on
//! neither sibling.
//!
//! **Nothing here is committed.** No fact is added, no entity is minted; a
//! founder's handle is derived from the occupation's founding coordinates
//! (where, when, and from whom it was founded) and the world seed, and the
//! chain between two founders is derived from the gap between their
//! foundings. That is what keeps this campaign free of an epoch — see
//! spec §4, and note that the freedom ends the moment a *committed* value
//! (an eponymous toponym) cites one of these names.
//!
//! **The Salt (spec D2/C2):** `founder_of` used to mix the occupation's
//! `EntityId` into the seed, so a founder's name moved whenever mint order
//! moved — 18 of 20 world-rows on The Namesake's name-prefix metrics. It now
//! keys on the founding itself: the occupation's own `(people, site,
//! founded)` triple plus one ancestry hop through its parent occupation's
//! same triple. `EntityId` is still the argument; it is used only as a
//! lookup key to read the founding facts back off the ledger.

use hornvale_history::descent::{Kinship, kinship};
use hornvale_history::flesh::RoleHandle;
use hornvale_kernel::{EntityId, Value, World};

/// The handle of the figure who founded `occupation`.
///
/// Derived from the occupation's founding (where, when, from whom) and the
/// world seed, so it is stable across rebuilds and independent of mint order
/// among *other* occupations. `occupation` is a lookup key only — it is
/// never read for its value (The Salt, spec D2/D7) — used to read the
/// founding facts back off the ledger. Carries no ledger write.
pub fn founder_of(world: &World, occupation: EntityId) -> RoleHandle {
    // The Salt: keyed on the FOUNDING (where, when, from whom), never on the
    // occupation's entity id. Excluding everything after the founding is
    // deliberate — a founder's name must not be a function of how their
    // community later died — and the ancestry hop is what recovers the
    // discrimination that exclusion costs (measured 8.4% / 3.3% / 3.6% stem
    // collisions at seeds 42 / 7 / 1000; spec D2).
    let own = founding_coords_of(world, occupation);
    let parent = mother_of(world, occupation).and_then(|m| founding_coords_of(world, m));
    let key = match own {
        Some(c) => hornvale_history::record::founding_key_from(c, parent),
        None => 0,
    };
    RoleHandle(key ^ world.seed.0.rotate_left(17))
}

/// The canonical component roster, assembled once per process rather than
/// once per [`founding_coords_of`] call. The roster is world-independent —
/// the same observation `generation_length_of`'s doc makes — but
/// `WorldComponents::assemble()` itself costs ~0.032 s measured, and
/// `founder_of` is called once per occupation: roughly 700 times for a
/// single seed-42 world. Assembling per call would add on the order of 22 s
/// to a single build; this cache keeps the path a lookup instead.
fn canonical_components() -> Option<&'static crate::WorldComponents> {
    static CACHE: std::sync::OnceLock<Option<crate::WorldComponents>> = std::sync::OnceLock::new();
    CACHE
        .get_or_init(|| crate::WorldComponents::assemble().ok())
        .as_ref()
}

/// The founding coordinates of `occupation`, read off its committed facts.
///
/// `None` when the occupation is not reconstructable (no `occ-people`,
/// `occ-site` or `occ-founded`), or when its people's label is not in the
/// canonical roster — a malformed ledger degrades rather than panicking, the
/// same posture [`clan_root_of`]'s bounded walk takes.
fn founding_coords_of(
    world: &World,
    occupation: EntityId,
) -> Option<hornvale_history::record::FoundingCoords> {
    let people = people_of(world, occupation)?;
    let founded = founded_year(world, occupation)?;
    let site = match world
        .ledger
        .value_of(occupation, hornvale_history::OCC_SITE)?
    {
        Value::Number(n) => hornvale_kernel::CellId(*n as u32),
        _ => return None,
    };
    // `KindId` wraps a `&'static str`, and `people` here is borrowed from
    // ledger text (a runtime `String`), so it cannot be used to construct a
    // `KindId` directly — the same constraint `generation_length_of` states
    // at a different call site. Resolved instead against the canonical
    // roster, which hands back the roster's own `'static` label.
    let wc = canonical_components()?;
    let label = wc
        .biosphere
        .iter()
        .find(|(k, _)| k.0 == people)
        .map(|(k, _)| *k)?;
    Some(hornvale_history::record::FoundingCoords {
        people: label,
        site,
        founded,
    })
}

/// The people occupying `occupation`, as a `KindId` label.
fn people_of(world: &World, occupation: EntityId) -> Option<String> {
    match world
        .ledger
        .value_of(occupation, hornvale_history::OCC_PEOPLE)?
    {
        Value::Text(t) => Some(t.clone()),
        _ => None,
    }
}

/// The standard year `occupation` was founded.
///
/// Note the unit: the bake writes `BakeConfig::start_year`/`end_year`
/// straight through, so this fact is in **years**, notwithstanding the
/// "standard day" wording on `OCC_FOUNDED`'s own doc comment. The
/// inconsistency is recorded as a followup in the spec (§7.2); the arithmetic
/// throughout the history subsystem is self-consistent in years.
fn founded_year(world: &World, occupation: EntityId) -> Option<f64> {
    match world
        .ledger
        .value_of(occupation, hornvale_history::OCC_FOUNDED)?
    {
        Value::Number(n) => Some(*n),
        _ => None,
    }
}

/// The occupation `occupation` was settled from, if it was settled from one.
fn mother_of(world: &World, occupation: EntityId) -> Option<EntityId> {
    match world
        .ledger
        .value_of(occupation, hornvale_history::OCC_FOUNDED_FROM)?
    {
        Value::Entity(e) => Some(*e),
        // A `Number` value is `Founding::Genesis(CellId)` — a root, not a
        // parent. See `windows/almanac::history`'s decoder, which this
        // mirrors.
        _ => None,
    }
}

/// A people's generation length in years, from the shipped allometry.
///
/// `None` for an `Ametabolic` kind (a construct has no mass-derived life
/// history) or a species absent from this world's roster.
///
/// `world` is unused today — the roster `WorldComponents::assemble()`
/// returns is world-independent — but the parameter keeps the signature
/// stable against a future where it is not.
/// type-audit: bare-ok(identifier-text: species), bare-ok(count: return)
pub fn generation_length_of(world: &World, species: &str) -> Option<f64> {
    let wc = crate::WorldComponents::assemble().ok()?;
    let _ = world;
    // `KindId` wraps `&'static str`; `species` here is borrowed from ledger
    // text (a runtime `String`) and is never `'static`, so a `KindId`
    // literal cannot be built from it. `get_by_label` exists for exactly
    // this: a label-content comparison instead of a key construction.
    let bio = wc.biosphere.get_by_label(species)?;
    hornvale_species::life_history(bio.mass, bio.metabolic_class)
        .generation_length
        .map(|y| y.get())
}

/// The figure `occupation`'s founder descends from — the founder of the
/// community it was settled from — together with how they are related.
///
/// `None` in two cases: `occupation` is a genesis occupation with no mother
/// community, or its people's generation length cannot be derived (an
/// `Ametabolic` kind, or a species absent from this world's roster). The
/// latter is deliberate: with no generation length, `remove` has nothing to
/// divide the founding gap by, so there is no basis to call the pair
/// `Sibling`, `Ancestor`, or anything else — reporting "no forebear
/// derivable" is honest about what is unknown, where guessing a `Kinship`
/// would not be.
///
/// **The returned [`Kinship`] is reserved and currently unconsumed.** The
/// only non-test caller (the lab's name renderer) binds it as `_kinship` and
/// uses the handle alone; nothing yet walks the remove back through
/// [`hornvale_history::descent::ancestor`], which is likewise unwired. So the
/// element this feeds is a *mother-community founder* citation, not a
/// patronymic — see the chronicle. Widening that is a design change, not a
/// fix.
pub fn forebear_of(world: &World, occupation: EntityId) -> Option<(RoleHandle, Kinship)> {
    let mother = mother_of(world, occupation)?;
    let child_year = founded_year(world, occupation)?;
    let mother_year = founded_year(world, mother)?;
    let species = people_of(world, occupation)?;
    let gl = generation_length_of(world, &species)?;
    Some((
        founder_of(world, mother),
        kinship(child_year - mother_year, gl),
    ))
}

/// The genesis occupation at the root of `occupation`'s descent chain — the
/// clan.
///
/// Walks `occ-founded-from` to its root. The committed tree is acyclic, but
/// this function does not assume it: the walk is bounded by the number of
/// occupations in the world and returns the last node reached rather than
/// looping, so a malformed ledger degrades instead of hanging.
pub fn clan_root_of(world: &World, occupation: EntityId) -> EntityId {
    let bound = world.ledger.find(hornvale_history::IS_OCCUPATION).count() + 1;
    let mut here = occupation;
    for _ in 0..bound {
        match mother_of(world, here) {
            Some(up) => here = up,
            None => return here,
        }
    }
    here
}

/// How wide a people draws "us", as the three-way decision the naming
/// pattern actually makes on `in_group_radius`.
///
/// Private and deliberately not a public axis: it exists to make the
/// midpoint decision exhaustive at the one place that reads it, so the
/// insular and expansive cases cannot both fire and neither can be widened
/// into the other by a one-character edit.
enum Breadth {
    /// Below the midpoint — everyone already knows everyone, so the
    /// outermost citation is dropped.
    Insular,
    /// Exactly the midpoint, where `SocietyVector::baseline` sits.
    Neutral,
    /// Above the midpoint — a wide "us" needs an extra gloss to disambiguate.
    Expansive,
}

/// The naming pattern a culture uses, derived from its society vector.
///
/// **Derived, never authored** (spec §3.3). A per-culture naming table would
/// be exactly the lookup table decision 0021 forecloses; the same discipline
/// already produces `morph_options`' honorific flag from
/// `StatusBasis::Rank`, and The Bane's whole threat niche from what the
/// creature already is.
///
/// The mapping:
///
/// - `Hierarchic` cites **descent** — who you came from legitimates you.
/// - `Communal` cites the **community or the deed** — what you did does.
/// - `Rank` adds a descent citation and (through `morph_options`) an
///   honorific; `Knowledge` cites the **mentor**, because where craft earns
///   standing the transmission lineage *is* the lineage; `Generosity` cites
///   the deed.
/// - `in_group_radius` sets how many elements the pattern carries: an
///   insular people needs fewer to pick someone out.
///
/// `mind` is unused today — the pattern reads only the society vector — but
/// the parameter keeps the signature stable against a future where a
/// mind-level trait (deliberation, time horizon) shapes the pattern too, the
/// same rationale [`generation_length_of`]'s unused `world` carries.
pub fn name_pattern(
    mind: &hornvale_species::MindVector,
    society: &hornvale_species::SocietyVector,
) -> hornvale_language::anthroponym::NamePattern {
    use hornvale_language::anthroponym::{Author, Cite, ElementSource, GlossBasis, NamePattern};
    let _ = mind;

    // Every culture gives a given name. It is the only universal element.
    let mut elements = vec![(ElementSource::Stem, Author::Kin)];

    // What legitimates a person here.
    match society.status_basis {
        hornvale_species::StatusBasis::Rank => {
            elements.push((ElementSource::Relation(Cite::Parent), Author::Kin));
        }
        hornvale_species::StatusBasis::Knowledge => {
            elements.push((ElementSource::Relation(Cite::Mentor), Author::Institution));
        }
        hornvale_species::StatusBasis::Generosity => {
            elements.push((ElementSource::Deed, Author::Witnesses));
        }
    }

    // How authority is shaped. Held rather than pushed: whether this
    // citation appears at all is the in-group-radius decision below.
    let sociality_citation = match society.sociality {
        hornvale_species::Sociality::Hierarchic => {
            (ElementSource::Relation(Cite::Clan), Author::Kin)
        }
        hornvale_species::Sociality::Communal => {
            (ElementSource::Relation(Cite::Community), Author::Community)
        }
    };

    // How wide "us" is drawn decides how much disambiguation a name must
    // carry on its face. The threshold is the midpoint of the [0,1] axis,
    // the same place `SocietyVector::baseline` sits — and the roster's most
    // common value, so the boundary is load-bearing rather than theoretical
    // (goblin sits exactly on it).
    //
    // This is one three-way decision that *builds* the tail of the list,
    // not a push followed by a positional `pop()`. The earlier shape was
    // correct only by arithmetic accident: two guards comparing the same
    // literal with strict operators, with `pop()` removing whichever element
    // happened to be last. Widening either guard by one character, or
    // inserting any element above this block, would have silently rewritten
    // a published pattern with no compiler signal. Here the three outcomes
    // are exhaustive `match` arms and the citation is named, so neither
    // mistake is expressible.
    let radius = society.in_group_radius;
    let breadth = if radius < 0.5 {
        Breadth::Insular
    } else if radius > 0.5 {
        Breadth::Expansive
    } else {
        Breadth::Neutral
    };
    match breadth {
        // An insular people drops the outermost citation: everyone already
        // knows which clan or community you belong to.
        Breadth::Insular => {}
        Breadth::Neutral => elements.push(sociality_citation),
        Breadth::Expansive => {
            elements.push(sociality_citation);
            elements.push((ElementSource::Gloss(GlossBasis::Bearing), Author::Outsiders));
        }
    }

    NamePattern { elements }
}

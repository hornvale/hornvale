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
//! founder's handle is derived from the occupation's entity id and the world
//! seed, and the chain between two founders is derived from the gap between
//! their foundings. That is what keeps this campaign free of an epoch — see
//! spec §4, and note that the freedom ends the moment a *committed* value
//! (an eponymous toponym) cites one of these names.

use hornvale_history::descent::{Kinship, kinship};
use hornvale_history::flesh::RoleHandle;
use hornvale_kernel::{EntityId, Value, World};

/// The handle of the figure who founded `occupation`.
///
/// Derived from the occupation's own entity id and the world seed, so it is
/// stable across rebuilds and independent of mint order among *other*
/// occupations. Carries no ledger write.
pub fn founder_of(world: &World, occupation: EntityId) -> RoleHandle {
    // Mix the entity id into the seed the same way `persona_of` mixes a
    // handle, so founder handles are drawn from the same space as the
    // ancestors `descent::ancestor` walks to.
    let mut x = u64::from(occupation.0) ^ world.seed.0.rotate_left(17);
    x = x.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    x ^= x >> 29;
    x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    RoleHandle(x ^ (x >> 32))
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

    // How authority is shaped.
    match society.sociality {
        hornvale_species::Sociality::Hierarchic => {
            elements.push((ElementSource::Relation(Cite::Clan), Author::Kin));
        }
        hornvale_species::Sociality::Communal => {
            elements.push((ElementSource::Relation(Cite::Community), Author::Community));
        }
    }

    // How wide "us" is drawn decides how much disambiguation a name must
    // carry on its face. The threshold is the midpoint of the [0,1] axis,
    // the same place `SocietyVector::baseline` sits.
    //
    // These two arms are mutually exclusive by construction (`> 0.5` and
    // `< 0.5` never both hold), so the `pop()` below never removes a `Gloss`
    // element this call just pushed — it always removes whatever was last
    // on the list *before* this block ran, which is the sociality citation
    // pushed immediately above (`Clan` or `Community`). That is exactly what
    // the comment on the `pop()` describes.
    if society.in_group_radius > 0.5 {
        elements.push((ElementSource::Gloss(GlossBasis::Bearing), Author::Outsiders));
    }
    if society.in_group_radius < 0.5 {
        // An insular people drops the outermost citation: everyone already
        // knows which clan or community you belong to.
        elements.pop();
    }

    NamePattern { elements }
}

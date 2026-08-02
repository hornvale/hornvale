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

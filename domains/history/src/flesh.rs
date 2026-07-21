//! The flesh derivations: pure, local, deterministic functions that expand
//! a committed [`crate::record::OccupationRecord`] into texture — a role
//! handle's persona, the physical residue a dead occupation leaves behind,
//! and the structures an occupation was built from. Every function here is
//! a *total function of its arguments*: no world, no global state, no
//! replay. The deep-history bake (Task 3, run at the composition root)
//! derives the `seed` these functions receive once per occupation, via
//! `world.seed.derive("history/flesh").derive(&occ.community.0.to_string())`;
//! these functions never derive that top-level label themselves — they
//! only derive their own sub-labels from whatever seed they're handed.

use crate::record::{CauseOfEnd, Function, Notability, OccupationRecord, TechHorizon};
use crate::streams;
use hornvale_kernel::Seed;

/// A lazily-expandable handle to the individual a role in an occupation's
/// history implies (a founder, the chieftain who led a flight, ...). The
/// handle itself carries no meaning; [`persona_of`] expands it into flesh
/// on demand, so a record can reference many unnamed roles without ever
/// materializing them until something actually observes one.
/// type-audit: bare-ok(identifier-text: 0)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RoleHandle(pub u64);

/// The hashed flesh a [`RoleHandle`] expands to: two independent seeds a
/// presenting window (never this crate) turns into a name and a set of
/// traits. Kept as raw seeds here so the expansion stays pure and total —
/// prose generation is a window's job, not a domain's.
/// type-audit: bare-ok(identifier-text: name_seed), bare-ok(identifier-text: trait_seed)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Persona {
    /// Seed a presenting window expands into a name.
    pub name_seed: u64,
    /// Seed a presenting window expands into a trait set.
    pub trait_seed: u64,
}

/// Expand a [`RoleHandle`] into its [`Persona`]: a pure splitmix-style hash
/// over `(handle, seed)`, mirroring `windows/chronicle`'s `expand_handle`
/// (The Sounding spike). Two independent scrambles of the same mixed state
/// give `name_seed` and `trait_seed`. No `Stream` is drawn — this is bit
/// arithmetic over the arguments alone, so it is trivially total and
/// deterministic: the same `(handle, seed)` always returns the same
/// `Persona`.
pub fn persona_of(handle: RoleHandle, seed: Seed) -> Persona {
    let mut x = handle.0 ^ seed.0;
    x = x.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    x ^= x >> 29;
    x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    let name_seed = x ^ (x >> 32);

    let mut y = name_seed.wrapping_add(0x9E37_79B9_7F4A_7C15);
    y = y.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    y ^= y >> 27;
    y = y.wrapping_mul(0x94D0_49BB_1331_11EB);
    let trait_seed = y ^ (y >> 31);

    Persona {
        name_seed,
        trait_seed,
    }
}

/// One physical remnant a dead occupation may leave behind, for the
/// present-day frame to observe.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ResidueItem {
    /// A child's toy — the signature find of a young, violently-ended
    /// family hamlet.
    Doll,
    /// A small personal ornament.
    Bauble,
    /// A sacred vessel or container — the signature find of a regional
    /// seat.
    Reliquary,
    /// A worked implement (agricultural, craft, or otherwise mundane).
    Tool,
    /// An edged or blunt arm.
    Weapon,
    /// Unburied skeletal remains.
    Bones,
    /// Carved or incised text — the most durable find, outlasting
    /// everything else this engine models.
    Inscription,
}

/// The physical remnants of one occupation, in no particular canonical
/// order (a presenting window sorts or filters as it needs).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Residue {
    /// The remnant items found.
    pub items: Vec<ResidueItem>,
}

/// The small, deterministic set of physical remnants an occupation leaves
/// behind, as of `now`. Keyed by `(people, cause, tenure-age, notability)`:
/// a violent end leaves personal effects while they're fresh (a doll for a
/// family hamlet, a weapon and bones for a fort); a regional seat leaves
/// durable sacred items that outlast the rest; and age weathers everything
/// else away. `seed` is the occupation-scoped seed the caller already
/// derived (see the module doc); this function derives its own
/// [`streams::RESIDUE`] sub-label from it.
/// type-audit: bare-ok(count: now)
pub fn residue_of(occ: &OccupationRecord, now: f64, seed: Seed) -> Residue {
    const YOUNG_RUIN_AGE: f64 = 250.0;
    const WORN_RUIN_AGE: f64 = 1_000.0;
    const HAMLET_POPULATION_CEILING: u32 = 150;

    let mut items = Vec::new();
    let age = occ.ended.map_or(0.0, |end| (now - end).max(0.0));
    let hamlet_scale = occ.peak_population <= HAMLET_POPULATION_CEILING;

    if let Some(cause) = occ.cause {
        match cause {
            CauseOfEnd::Burned => {
                if age < YOUNG_RUIN_AGE {
                    if occ.function == Function::Fort {
                        items.push(ResidueItem::Weapon);
                        items.push(ResidueItem::Bones);
                    } else if hamlet_scale {
                        items.push(ResidueItem::Doll);
                    } else {
                        items.push(ResidueItem::Tool);
                    }
                } else if age < WORN_RUIN_AGE {
                    if occ.function == Function::Fort {
                        items.push(ResidueItem::Bones);
                    } else {
                        items.push(ResidueItem::Tool);
                    }
                }
                // Beyond WORN_RUIN_AGE, fire leaves nothing personal behind.
            }
            CauseOfEnd::Plague => {
                if age < WORN_RUIN_AGE {
                    items.push(ResidueItem::Bones);
                }
            }
            CauseOfEnd::Famine | CauseOfEnd::Fled => {
                if age < YOUNG_RUIN_AGE {
                    items.push(ResidueItem::Tool);
                }
            }
            CauseOfEnd::Migrated => {
                // Climate abandonment is the real world's dominant end
                // (a cell the paleoclimate turned hostile, walked away from
                // over a generation). Real archaeology is abandonment-
                // driven: people who leave slowly do not take everything.
                // A young, hamlet-scale departure leaves modest personal
                // residue — a child's doll left in the grass, a worked pot
                // otherwise; older migrated ruins weather to nothing.
                // (Nathan's call, 2026-07-21, archaeological-realism.)
                if age < YOUNG_RUIN_AGE {
                    if hamlet_scale {
                        items.push(ResidueItem::Doll);
                    } else {
                        items.push(ResidueItem::Tool);
                    }
                }
                // Beyond YOUNG_RUIN_AGE, an orderly departure's remnants
                // have long since weathered away — nothing personal remains.
            }
        }
    }

    if occ.notability == Notability::Seat {
        items.push(ResidueItem::Reliquary);
        if age < WORN_RUIN_AGE {
            items.push(ResidueItem::Bauble);
        }
        if occ.function == Function::Cult {
            items.push(ResidueItem::Inscription);
        }
    }

    // A deterministic flavor draw, keyed by `people` as well as the rest of
    // the record: a well-populated occupation has enough churn to leave one
    // extra tool behind, on a coin-flip rooted in this occupation's own
    // seed (never global state).
    if !hamlet_scale && age < WORN_RUIN_AGE {
        let mut stream = seed.derive(streams::RESIDUE).derive(occ.people.0).stream();
        if stream.range_u32(0, 1) == 1 {
            items.push(ResidueItem::Tool);
        }
    }

    Residue { items }
}

/// One building an occupation raised, gated by its function, technological
/// horizon, and peak population.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Structure {
    /// A single-family dwelling.
    Hut,
    /// A multi-family communal dwelling (a populous, Iron-horizon-or-later
    /// occupation's residential core).
    Longhouse,
    /// Grain storage — an agrarian occupation's surplus.
    Granary,
    /// A small devotional site.
    Shrine,
    /// A monumental devotional site (a Classical-horizon cult occupation).
    Temple,
    /// A defensive perimeter.
    Wall,
    /// An extraction shaft.
    Mineshaft,
    /// A trade waypoint's exchange ground.
    Market,
}

/// The structures one occupation was built from: a residential core scaled
/// by `peak_population`, plus whatever its `function` × `tech` combination
/// implies (a granary for an agrarian community, a shrine that becomes a
/// temple once a cult reaches the Classical horizon, ...). `seed` is the
/// occupation-scoped seed the caller already derived (see the module doc);
/// this function derives its own [`streams::STRUCTURES`] sub-label from it
/// for the dwelling-count variance.
pub fn structures_of(occ: &OccupationRecord, seed: Seed) -> Vec<Structure> {
    const LONGHOUSE_POPULATION_FLOOR: u32 = 200;
    const DWELLING_POPULATION_STEP: u32 = 50;

    let mut structures = Vec::new();

    let dwelling =
        if occ.peak_population >= LONGHOUSE_POPULATION_FLOOR && occ.tech >= TechHorizon::Iron {
            Structure::Longhouse
        } else {
            Structure::Hut
        };
    let mut stream = seed.derive(streams::STRUCTURES).stream();
    let dwelling_count =
        1 + occ.peak_population / DWELLING_POPULATION_STEP + stream.range_u32(0, 1);
    for _ in 0..dwelling_count {
        structures.push(dwelling);
    }

    match occ.function {
        Function::Agrarian => structures.push(Structure::Granary),
        Function::Mine => structures.push(Structure::Mineshaft),
        Function::Trade => structures.push(Structure::Market),
        Function::Cult => {
            structures.push(Structure::Shrine);
            if occ.tech == TechHorizon::Classical {
                structures.push(Structure::Temple);
            }
        }
        Function::Fort => structures.push(Structure::Wall),
    }

    structures
}

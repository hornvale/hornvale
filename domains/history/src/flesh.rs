//! The flesh derivations: pure, local, deterministic functions that expand
//! a committed [`crate::record::OccupationRecord`] into texture — a role
//! handle's persona, the physical residue a dead occupation leaves behind,
//! and the structures an occupation was built from. Every function here is
//! a *total function of its arguments*: no world, no global state, no
//! replay. The deep-history bake (Task 3, run at the composition root)
//! derives the `seed` these functions receive once per occupation, via
//! `world.seed.derive(streams::FLESH).derive(StreamLabel::dynamic(&id))`;
//! these functions never derive that top-level label themselves — they
//! only derive their own sub-labels from whatever seed they're handed.

use crate::record::{CauseOfEnd, Function, Notability, OccupationRecord, TechHorizon};
use crate::streams;
use hornvale_kernel::Seed;
use hornvale_kernel::seed::StreamLabel;

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

/// The age (standard years) past which a *perishable* find — cloth, wood,
/// food, ash, a child's doll — has rotted back into the soil. Only a young
/// ruin still holds them; this is the same threshold the engine has always
/// called the "young ruin" age.
const PERISHABLE_MAX_AGE: f64 = 250.0;

/// The age (standard years) past which even a *durable* find — fired clay,
/// bone, dressed stone, worked flint — has finally weathered away. Set at
/// millennia scale on purpose: real durable archaeology (Chaco Canyon, Mesa
/// Verde, Jericho) endures far longer than the mere centuries this engine's
/// ruins actually reach, so *every* ruin the deep-history bake produces —
/// including the real world's ancient climate-abandoned hamlets — still
/// yields a legible archaeological impression rather than bare ground.
const DURABLE_TRACE_AGE: f64 = 12_000.0;

/// How long a physical remnant survives in the ground before it weathers
/// away — the material-durability axis that decides whether an *ancient* ruin
/// still leaves a findable trace. This is the keystone of Task 8b: perishable
/// goods rot within a few lifetimes, but durable goods (fired clay, bone,
/// dressed stone) last millennia and a few finds are effectively eternal, so
/// a hamlet abandoned a thousand years ago is still an archaeological site.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Durability {
    /// Organic or worked-soft material — cloth, wood, food, ash. Gone within
    /// a few human lifetimes (survives only a young ruin).
    Perishable,
    /// Fired clay, bone, dressed stone, worked flint — the ordinary
    /// archaeological record. Survives for millennia.
    Durable,
    /// Deliberately-buried sacred goods and incised stone — effectively
    /// permanent on the timescale this engine models.
    Eternal,
}

impl Durability {
    /// The age (standard years) past which a find of this durability has
    /// weathered away. `Eternal` never does (`f64::INFINITY`).
    /// type-audit: bare-ok(count: return)
    pub fn max_age(self) -> f64 {
        match self {
            Durability::Perishable => PERISHABLE_MAX_AGE,
            Durability::Durable => DURABLE_TRACE_AGE,
            Durability::Eternal => f64::INFINITY,
        }
    }
}

/// One physical remnant a dead occupation may leave behind, for the
/// present-day frame to observe. Each variant carries a material
/// [`Durability`] (see [`ResidueItem::durability`]) that decides how long it
/// survives in the ground: a perishable doll rots within a few lifetimes,
/// while durable domestic debris (potsherds, foundations, worked stone, bone)
/// outlasts the centuries and makes an ancient ruin legible.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ResidueItem {
    /// A child's toy — the signature find of a young family hamlet, and the
    /// campaign's whole promise. Perishable: gone once the ruin is old.
    Doll,
    /// A small personal ornament (beads, shell). Durable.
    Bauble,
    /// A sacred vessel or container — the signature find of a regional
    /// seat. Eternal (buried deliberate and deep).
    Reliquary,
    /// A worked implement (agricultural, craft, or otherwise mundane) —
    /// here, a fired-clay pot. Durable.
    Tool,
    /// An edged or blunt arm. Durable (a corroded metal head).
    Weapon,
    /// Unburied skeletal remains. Durable (bone lasts millennia).
    Bones,
    /// Fragments of fired-clay vessels — the single most common find of any
    /// abandoned settlement, scattered where the huts stood. Durable.
    Potsherd,
    /// The structure's own remains: postholes, collapsed daub walls, and the
    /// low turf-lines of the dwellings and granary still ridging the grass.
    /// Durable.
    Foundation,
    /// A scatter of knapped or dressed stone — worked flint, a lost
    /// arrowhead, a grinding-stone. Durable.
    WorkedStone,
    /// Carved or incised text — the most durable find, outlasting everything
    /// else this engine models. Eternal.
    Inscription,
}

impl ResidueItem {
    /// The material [`Durability`] of this find — how long it survives in the
    /// ground before weathering away. The single source of truth a presenting
    /// window reads to decide whether an ancient ruin still shows this trace.
    pub fn durability(self) -> Durability {
        match self {
            ResidueItem::Doll => Durability::Perishable,
            ResidueItem::Bauble => Durability::Durable,
            ResidueItem::Reliquary => Durability::Eternal,
            ResidueItem::Tool => Durability::Durable,
            ResidueItem::Weapon => Durability::Durable,
            ResidueItem::Bones => Durability::Durable,
            ResidueItem::Potsherd => Durability::Durable,
            ResidueItem::Foundation => Durability::Durable,
            ResidueItem::WorkedStone => Durability::Durable,
            ResidueItem::Inscription => Durability::Eternal,
        }
    }
}

/// The physical remnants of one occupation, in no particular canonical
/// order (a presenting window sorts or filters as it needs).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Residue {
    /// The remnant items found.
    pub items: Vec<ResidueItem>,
}

/// The small, deterministic set of physical remnants an occupation leaves
/// behind, as of `now`. Keyed by `(people, cause, tenure-age, notability)`.
///
/// The model is **material-durability first** (Task 8b): every ruin — no
/// matter how ancient — leaves the durable domestic debris archaeology
/// actually recovers (potsherds where the huts stood, the turf-lines of the
/// dwellings, scattered bone and worked stone), while *perishable* personal
/// effects (a child's doll) survive only while the ruin is young. Each cause
/// keeps its own character (a fort adds arms and the fallen; a plague, the
/// unburied dead; a regional seat, its buried sacred goods), but the durable
/// floor means an abandoned hamlet a thousand years gone is still a legible
/// archaeological site, not bare ground. A single durability filter at the
/// end weathers away whatever the ruin's age has outlived (see
/// [`Durability::max_age`]).
///
/// `seed` is the occupation-scoped seed the caller already derived (see the
/// module doc); this function derives its own [`streams::RESIDUE`] sub-label
/// from it.
/// type-audit: bare-ok(count: now)
pub fn residue_of(occ: &OccupationRecord, now: f64, seed: Seed) -> Residue {
    const HAMLET_POPULATION_CEILING: u32 = 150;

    let mut items = Vec::new();
    let age = occ.ended.map_or(0.0, |end| (now - end).max(0.0));
    let hamlet_scale = occ.peak_population <= HAMLET_POPULATION_CEILING;

    if let Some(cause) = occ.cause {
        match cause {
            CauseOfEnd::Burned => {
                // Fire leaves the fort's arms and its fallen, or a family
                // hamlet's doll while it is young; either way the burnt daub
                // and potsherds and the foundation lines of what stood endure.
                if occ.function == Function::Fort {
                    items.push(ResidueItem::Weapon);
                    items.push(ResidueItem::Bones);
                } else if hamlet_scale {
                    items.push(ResidueItem::Doll);
                }
                items.push(ResidueItem::Potsherd);
                items.push(ResidueItem::Foundation);
            }
            CauseOfEnd::Plague => {
                // Disease leaves the unburied dead, and the outlines of the
                // dwellings emptied around them.
                items.push(ResidueItem::Bones);
                items.push(ResidueItem::Foundation);
            }
            CauseOfEnd::Famine | CauseOfEnd::Fled => {
                // A hurried departure: a tool dropped, the pots left behind,
                // the dwelling lines abandoned to the grass.
                items.push(ResidueItem::Tool);
                items.push(ResidueItem::Potsherd);
                items.push(ResidueItem::Foundation);
            }
            CauseOfEnd::Migrated => {
                // Climate abandonment is the real world's dominant end (a cell
                // the paleoclimate turned hostile, walked away from over a
                // generation) — and the one that leaves the classic
                // archaeological hamlet. A young departure leaves a child's
                // doll in the grass; but for centuries and millennia after,
                // the durable domestic debris — potsherds where the huts
                // stood, the turf-lines of a granary, a scatter of worked
                // stone — is what a searcher still finds.
                // (Nathan's call, 2026-07-21, archaeological-realism / 8b.)
                if hamlet_scale {
                    items.push(ResidueItem::Doll);
                }
                items.push(ResidueItem::Potsherd);
                items.push(ResidueItem::Foundation);
                items.push(ResidueItem::WorkedStone);
            }
        }
    }

    if occ.notability == Notability::Seat {
        // A regional seat's sacred goods: a buried reliquary (eternal) and its
        // durable bead ornaments; a cult seat also incises its stone.
        items.push(ResidueItem::Reliquary);
        items.push(ResidueItem::Bauble);
        if occ.function == Function::Cult {
            items.push(ResidueItem::Inscription);
        }
    }

    // A deterministic flavor draw, keyed by `people` as well as the rest of
    // the record: a well-populated occupation has enough churn to leave an
    // extra scatter of worked stone behind, on a coin-flip rooted in this
    // occupation's own seed (never global state).
    if !hamlet_scale {
        let mut stream = seed
            .derive(streams::RESIDUE)
            .derive(StreamLabel::dynamic(occ.people.0))
            .stream();
        if stream.range_u32(0, 1) == 1 {
            items.push(ResidueItem::WorkedStone);
        }
    }

    // Weather away everything whose material cannot survive the ruin's age:
    // perishable finds vanish past a few lifetimes, durable ones past
    // millennia, and eternal finds never. This single filter is what makes an
    // ancient ruin legible — its durable traces endure while the doll is gone.
    items.retain(|item| age < item.durability().max_age());

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

//! The flesh derivations: pure, local, deterministic functions that expand
//! a committed [`crate::record::OccupationRecord`] into texture — a role
//! handle's persona, the physical residue a dead occupation leaves behind,
//! and the structures an occupation was built from. Every function here is
//! a *total function of its arguments*: no world, no global state, no
//! replay. The deep-history bake (Task 3, run at the composition root)
//! derives the `seed` these functions receive once per occupation, via
//! `world.seed.derive(streams::FLESH).derive(StreamLabel::dynamic(&material_key))`
//! — keyed on the occupation's material core (`record::material_key`), never
//! its entity id, so derived prose does not move when an id moves (The
//! Salt). These functions never derive that top-level label themselves —
//! they only derive their own sub-labels from whatever seed they're handed.

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

/// A discriminant mixed into every founder handle, so that a future second
/// role at the same occupation cannot collide with the founder.
const FOUNDER_ROLE: u64 = 0x466F_756E_6465_7200;

/// Derive a founder's stable identity from **their founding, one hop of
/// ancestry, and their community's whole span** — where, when, by whom, out of
/// which community, and how far it got.
///
/// Never from its `EntityId` and never from its position in a collection.
/// Decision 0051 forbids salting a procedural name from mint order, and a
/// handle is exactly such a salt — `persona_of` turns it into a name. Keying on
/// the occupation entity would rename every founder in the world the first time
/// an unrelated domain minted earlier in genesis.
///
/// The rest of this doc is the archaeology of how the key got here, kept
/// deliberately: every paragraph below describes the **superseded**
/// `(people, site, founded, ended, peak_population)` key, up to the one marked
/// "The widening landed", which describes the code.
///
/// ~~`(people, site, founded, ended, peak_population)` is unique across the
/// selected cast on every measured seed (90/90, 82/82, 100/100 for seeds 42, 7
/// and 1000) and collides on 3 colliding pairs in seed 42, 2 in seed 7, and 0
/// in seed 1000 — records that are indistinguishable in every *non-entity*
/// field (they differ only in `ended_by` and `founded_from`, both
/// `EntityId`-valued fields decision 0051 already forbids keying on).
/// `select_founders` asserts cast-uniqueness rather than trusting it.~~
///
/// **CORRECTED 2026-08-10 (The Radiation).** Struck through rather than
/// deleted: the wrong sentence above is the one that closed off the repair for
/// a campaign and a half, and it is worth more visible than tidy.
///
/// **The reasoning was wrong in kind, not merely out of date.** "…`ended_by`
/// and `founded_from`, both `EntityId`-valued fields decision 0051 already
/// forbids keying on" reads *`EntityId`-valued field* as a synonym for
/// *unkeyable*. That is not what 0051 says. 0051 forbids keying on an id **as a
/// value** — a mint counter whose number shifts when some unrelated domain
/// mints earlier in genesis. It says nothing against keying on the **referent's
/// own material facts**, and this crate had already written exactly that
/// resolution before the struck paragraph was: [`crate::record::FoundingCoords`],
/// [`crate::record::founding_key_from`] and [`crate::record::layer_key`]'s
/// ancestry tail each fold a *referenced* occupation by its own
/// `(people, site, founded)` and never by its id — which `record.rs` calls "a
/// material fact, not a compromise". The door reported shut has been open since
/// The Salt.
///
/// **The counts were stale on both trees.** Re-measured on this tree
/// (2026-08-10, `BuildDepth::Settlements`, default pins, seeds 42 / 7 / 1000):
/// the promoted cast is **148 / 122 / 174** founders, all handles distinct at
/// those three seeds; handle-sharing pairs across the *whole* record set are
/// **2 / 1 / 1**, not 3 / 2 / 0. Seed 1000 is recorded above as having none and
/// has one. These figures had not merely been overtaken by a widened species
/// roster — they had stopped describing `main` as well.
///
/// **Cast-uniqueness is not structural, and is no longer asserted.** Two
/// occupations agreeing on all five fields derive one handle *by construction*,
/// and a sweep of seeds 0–2999 on this tree (`BuildDepth::Settlements`, default
/// pins, 2026-08-10) finds 1904 such pairs across 958 worlds, of which **five
/// reach the promoted cast: seeds 283, 705, 2403, 2634 and 2898** — the first
/// two inside the census range 0–999. `select_founders` used to `assert!`
/// cast-uniqueness and take the whole world down with it, which is why the
/// once-per-campaign census could not run. Since The Radiation it **drops** the
/// later member of a handle-equal pair and reports it, so roughly two worlds in
/// a thousand remember one founder fewer. That is an authorized fidelity cut
/// (Nathan's ruling), not a repair of this key.
///
/// **The widening landed in The Ell (2026-08-11), riding that campaign's
/// epoch rather than paying for one of its own.** The key is now
/// [`crate::record::founding_key_from`] — which is the founding triple **plus
/// one hop of ancestry**, the fold this crate had already written and was
/// already using elsewhere — with `ended` and `peak_population` folded on top
/// of it and [`FOUNDER_ROLE`] mixed in last. It is a *widening*, not a
/// replacement: nothing the old key read has left it, and the parent's own
/// `(people, site, founded)` has joined it.
///
/// The parent is **resolved by the caller and passed in**, exactly as
/// [`crate::record::layer_key`]'s ancestry tail takes it, because a domain
/// holds no `World` and cannot follow an `EntityId` to its referent.
///
/// **What did NOT change, and is the thing to read the strike above for.**
/// Decision 0051 forbids keying on an id **as a value** — a mint counter that
/// shifts when an unrelated domain mints earlier in genesis. Folding a
/// *referent's own material facts* is a different act, and this crate performs
/// it in three places already ([`crate::record::material_key`],
/// [`crate::record::founding_key`], [`crate::record::layer_key`]). This key
/// still reads no id, not the occupation's own and not its parent's; the
/// parent arrives as coordinates. (The Signet has since made ids
/// lineage-derived rather than mint-ordered, so 0051's specific hazard no
/// longer exists at all — but this design would not need that reprieve.)
///
/// # Why the ancestry hop was ADDED and the post-founding fields were KEPT
///
/// The Ell's plan called for the narrower, more principled key — the founding
/// and its ancestry **alone**, dropping `ended` and `peak_population`, on the
/// ground that a founder is named at their founding and what their community
/// later grew to and how it died must not rename them. That is
/// [`crate::record::founding_key`]'s stated rationale and it is a good one.
/// **It was measured and it is unaffordable here.** Three arms, seeds 0–999,
/// `BuildDepth::Settlements`, default pins, 2026-08-11, counting worlds whose
/// *promoted cast* carries a handle collision and the founders that costs:
///
/// ```text
///   key                                    colliding worlds   founders lost
///   (people, site, founded, ended, peak)             2 / 1000             2
///   founding + parent hop ALONE                    732 / 1000          1582
///   founding + parent hop + ended + peak  <- this    0 / 1000             0
/// ```
///
/// The narrow key is 790× *worse* at the thing this repair exists to fix, and
/// the reason is legible rather than statistical. Dumping the pairs it newly
/// collides shows one shape every time: a people founds at a site in some
/// year, the attempt is raided and closed in that same year (`founded ==
/// ended`, `peak_population == 8`, `cause == Fled`, `ended_by == By(..)`), and
/// a second record — same people, same site, same year, same parent — carries
/// the community that took. Those two records are **identical in every
/// founding-side field there is**, so no amount of ancestry can separate them:
/// a second or third hop reaches the same parent. Only a post-founding fact
/// can, which is exactly the fact the narrow key discards.
///
/// That leaves a real world-model question — whether a failed attempt and its
/// same-year successor are *one* founding with *one* founder — and it is
/// Nathan's to answer, not this function's. What is settled is that answering
/// it "yes" by narrowing this key would silently turn
/// `windows/worldgen::person_promote`'s rare authorized fidelity cut into the
/// normal path in 73% of worlds, which is a change to what a world remembers
/// and not a change to a hash.
///
/// # The known residual
///
/// This key is not *total*, and the campaign's original scoring said so: two
/// worlds in 0–2999 (**2634 and 2898**) hold a pair whose two parents are
/// themselves twins, so the ancestry hop folds identically and the pair
/// collides on everything else too. Re-measured on this tree, both still do,
/// at one founder each — and **both sit outside the census range 0–999, which
/// is now clean where it previously was not** (283 and 705 were the two, and
/// are the two the fix removes). `windows/worldgen`'s
/// `person_promote::select_founders` is what happens when it fires, and
/// `windows/worldgen/tests/founder_collision.rs` carries the per-seed truth.
///
/// `parent` is the founding coordinates of the occupation `founded_from`
/// names, or `None` — for a `Genesis` founding, and for a caller that holds a
/// record whose predecessor it cannot resolve. Those two cases fold
/// identically, which is [`crate::record::founding_key_from`]'s existing
/// contract and the one `windows/worldgen::descent::founder_of` reads off the
/// ledger; deliberately **not** [`crate::record::layer_key`]'s three-way rank,
/// so that the founding-side half of this handle stays bit-for-bit the key a
/// ledger-side caller derives for the same founding.
pub fn founder_handle(
    occ: &OccupationRecord,
    parent: Option<crate::record::FoundingCoords<'_>>,
) -> RoleHandle {
    use crate::record::{day_key, founding_coords, founding_key_from, mix};
    let mut h = founding_key_from(founding_coords(&occ.core), parent);
    // `ended` folds with a presence tag and through `day_key`, matching
    // `material_key` rather than the raw `to_bits` the superseded key used:
    // `to_bits` disagrees with float order on negatives and `-0.0`, and a
    // sentinel `u64::MAX` for `None` is a value a real day could in principle
    // reach. Neither is reachable from today's bake, and neither should have
    // to be argued about again.
    h = match occ.core.ended {
        Some(d) => mix(mix(h, 1), day_key(d)),
        None => mix(h, 0),
    };
    h = mix(h, u64::from(occ.core.peak_population));
    RoleHandle(mix(h, FOUNDER_ROLE))
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

/// Why an occupation whose cause is [`CauseOfEnd::Migrated`] left its ground.
///
/// A **derived** distinction, never serialized and never committed. Predation
/// (The Tumult) gave `Migrated` two producers: the paleoclimate evicting a
/// people onto vacant refuge ground, and a *conqueror* abandoning its own
/// poorer seat to move onto the neighbour's ground it has just taken (an
/// orderly, self-directed move under [`crate::record::Ended::Nature`]). The
/// committed cause alone therefore no longer says which one a record is — and
/// it cannot be recovered from the record either, because the evidence is the
/// *contemporaneous victim*, a fact about some **other** record. So the
/// caller that holds the whole record set decides and passes the verdict in;
/// `windows/worldgen::migration_events` performs the identical fold for the
/// ledger-side count. Ignored for every cause but `Migrated`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Departure {
    /// The paleoclimate turned the cell hostile: the people walked away over a
    /// generation, onto vacant ground.
    Climate,
    /// The people left of its own accord, in the single season it took a
    /// neighbour's ground by force, and carried the settlement onto the prize.
    Conquest,
}

/// The peak population at or below which a settlement is HAMLET-SCALE — a family
/// place rather than a community with public business.
///
/// Hoisted from inside [`residue_of`] (where it was a function-local `const`)
/// when The Blocking's `store` chamber role became its second reader: a hamlet's
/// storeroom has nothing worth locking up, which is the same threshold read for a
/// different consequence. One number, one meaning — re-typing `150` in the vessel
/// would have let the two drift.
/// type-audit: bare-ok(count)
pub const HAMLET_POPULATION_CEILING: u32 = 150;

/// The small, deterministic set of physical remnants an occupation leaves
/// behind, as of `now`. Keyed by `(people, cause, tenure-age, notability)`,
/// plus — for a `Migrated` record only — the caller's [`Departure`] verdict.
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
/// from it. `departure` disambiguates the two producers of
/// [`CauseOfEnd::Migrated`] (see [`Departure`]) and is ignored for every other
/// cause; it consumes no draw either way, so the stream a caller hands in is
/// spent identically whichever verdict it passes.
/// type-audit: bare-ok(count: now)
pub fn residue_of(occ: &OccupationRecord, now: f64, seed: Seed, departure: Departure) -> Residue {
    let mut items = Vec::new();
    let age = occ.core.ended.map_or(0.0, |end| (now - end).max(0.0));
    let hamlet_scale = occ.core.peak_population <= HAMLET_POPULATION_CEILING;

    if let Some(cause) = occ.core.cause {
        match cause {
            CauseOfEnd::Burned => {
                // Fire leaves the fort's arms and its fallen, or a family
                // hamlet's doll while it is young; either way the burnt daub
                // and potsherds and the foundation lines of what stood endure.
                if occ.core.function == Function::Fort {
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
            CauseOfEnd::Migrated => match departure {
                Departure::Climate => {
                    // Climate abandonment is the real world's dominant end (a
                    // cell the paleoclimate turned hostile, walked away from
                    // over a generation) — and the one that leaves the classic
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
                Departure::Conquest => {
                    // A conqueror's abandoned seat, emptied in the one season
                    // it took the neighbour's ground: not an abandonment at
                    // all, a move. Both of the climate assemblage's
                    // slow-departure marks are therefore absent — nothing was
                    // left forgotten in the grass (no doll), and no generation
                    // of winnowing scattered worked stone across the site.
                    // What stays is what a people carrying its goods to a
                    // seized seat does not carry: the broken pots, and the
                    // turf-lines of the dwellings it walked out of.
                    items.push(ResidueItem::Potsherd);
                    items.push(ResidueItem::Foundation);
                }
            },
        }
    }

    if occ.core.notability == Notability::Seat {
        // A regional seat's sacred goods: a buried reliquary (eternal) and its
        // durable bead ornaments; a cult seat also incises its stone.
        items.push(ResidueItem::Reliquary);
        items.push(ResidueItem::Bauble);
        if occ.core.function == Function::Cult {
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
            .derive(StreamLabel::dynamic(occ.core.people.0))
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

    let dwelling = if occ.core.peak_population >= LONGHOUSE_POPULATION_FLOOR
        && occ.core.tech >= TechHorizon::Iron
    {
        Structure::Longhouse
    } else {
        Structure::Hut
    };
    let mut stream = seed.derive(streams::STRUCTURES).stream();
    let dwelling_count =
        1 + occ.core.peak_population / DWELLING_POPULATION_STEP + stream.range_u32(0, 1);
    for _ in 0..dwelling_count {
        structures.push(dwelling);
    }

    match occ.core.function {
        Function::Agrarian => structures.push(Structure::Granary),
        Function::Mine => structures.push(Structure::Mineshaft),
        Function::Trade => structures.push(Structure::Market),
        Function::Cult => {
            structures.push(Structure::Shrine);
            if occ.core.tech == TechHorizon::Classical {
                structures.push(Structure::Temple);
            }
        }
        Function::Fort => structures.push(Structure::Wall),
    }

    structures
}

//! The flesh derivations: pure, local, deterministic functions that expand
//! a committed [`crate::record::OccupationRecord`] into texture — a role
//! handle's persona, the physical residue a dead occupation leaves behind,
//! and the structures an occupation was built from. Every function here is
//! a *total function of its arguments*: no world, no global state, no
//! replay.
//!
//! [`flesh_seed_for`] and [`flesh_seed_of_key`] are the one place the
//! occupation-scoped seed itself is derived —
//! `world_seed.derive(streams::FLESH).derive(StreamLabel::dynamic(&material_key))`
//! — keyed on the occupation's material core (`record::material_key`), never
//! its entity id, so derived prose does not move when an id moves (The
//! Salt). Every other function in this module (`persona_of`, `residue_of`,
//! `structures_of`, …) never derives that top-level label itself — it only
//! derives its own sub-labels from whatever seed it's handed, which a
//! caller gets by calling `flesh_seed_for` first.

use crate::record::{
    CauseOfEnd, Function, Notability, Occupation, OccupationRecord, TechHorizon, material_key,
};
use crate::streams;
use hornvale_kernel::Seed;
use hornvale_kernel::seed::StreamLabel;

/// The occupation-scoped seed the flesh derivations expand from.
///
/// **THE one place `history/flesh/v2` is spelled out** (The Salt, moved here
/// from `windows/almanac` at the campaign ledger's finding #16: the stream
/// belongs to the domain that owns it, not to a presenting window). Keyed on
/// the occupation's **material core** (`material_key`), never its entity id,
/// so derived prose does not move when an id moves.
///
/// Takes `world_seed: &Seed` rather than `&World` — this stays a total
/// function of its own arguments, the same discipline every other function
/// in this module holds (see the module doc comment). `windows/almanac`'s
/// `flesh_seed`/`flesh_seed_for` and `windows/lot`'s `dwelling` slot call
/// this rather than re-deriving the stream themselves.
pub fn flesh_seed_for(world_seed: &Seed, core: &Occupation) -> Seed {
    flesh_seed_of_key(world_seed, material_key(core))
}

/// The shared tail of [`flesh_seed_for`] and any caller keyed by a raw
/// material key rather than a reconstructed [`Occupation`] — the one place
/// the `history/flesh/v2` derivation is spelled out, so entry points cannot
/// drift apart from each other.
/// type-audit: bare-ok(identifier-text: key)
pub fn flesh_seed_of_key(world_seed: &Seed, key: u64) -> Seed {
    world_seed
        .derive(streams::FLESH)
        .derive(StreamLabel::dynamic(&key.to_string()))
}

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
/// plumb: pending(wave-1)
const FOUNDER_ROLE: u64 = 0x466F_756E_6465_7200;

/// A founder's **discrimination handle**: the identity of their founding
/// ([`crate::record::founding_key`] — where, when, by whom, out of which
/// community) plus the tail that makes it unique across the cast a world
/// promotes.
///
/// The two halves are named, separate, and answer different questions; the
/// section below on identity-versus-discrimination is the load-bearing part of
/// this doc, and Nathan's ruling of 2026-08-11.
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
/// epoch rather than paying for one of its own.** It is a *widening*, not a
/// replacement: nothing the old key read has left it, and the founding's own
/// one hop of ancestry has joined it. The parent is **resolved by the caller
/// and passed in**, exactly as [`crate::record::layer_key`]'s ancestry tail
/// takes it, because a domain holds no `World` and cannot follow an
/// `EntityId` to its referent.
///
/// # An IDENTITY key and a DISCRIMINATION key are different things
///
/// This is the distinction `domains/history` had been making in code for two
/// campaigns without ever naming, and naming it is most of what The Ell
/// changed here. The body below is two steps and they answer two questions:
///
/// - **Identity** — *is this the same founding?* [`crate::record::founding_key`]
///   answers it, and it must be **founding-side only**: an identity that moves
///   when later events move is not an identity. That is why `ended`,
///   `peak_population`, `cause` and `notability` are excluded *there*, and the
///   exclusion is right *there*.
/// - **Discrimination** — *is this one unique across the population that will
///   be promoted?* That is a different requirement with a different
///   entitlement: a discrimination key must separate every member of a cast,
///   and it may legitimately read anything the record carries. `ended` and
///   `peak_population` are folded on for exactly this and nothing else.
///
/// `founder_handle` was previously being asked to do the second job while
/// wearing the first one's name and inheriting its rationale — one name, two
/// meanings, no marker, which is this campaign's own defect one level up. So
/// **"a founder's name must not be a function of how their community later
/// died" was not abandoned, it was mis-assigned.** It is
/// [`crate::record::founding_key`]'s rule, it is true there, and it was never
/// this function's to hold. What *is* this function's rule is that the
/// identity half must remain bit-for-bit the key a ledger-side caller derives
/// for the same founding, which is why step one is a call and not a copy.
///
/// # What the split is worth: the measurement that forced it
///
/// The Ell's plan called for the identity key **alone** as the handle. Three
/// arms, seeds 0–999, `BuildDepth::Settlements`, default pins, 2026-08-11,
/// counting worlds whose *promoted cast* carries a handle collision and the
/// founders that costs:
///
/// ```text
///   key                                    colliding worlds   founders lost
///   (people, site, founded, ended, peak)             2 / 1000             2
///   identity key ALONE                             732 / 1000          1582
///   identity + discrimination tail  <- this          0 / 1000             0
/// ```
///
/// The identity key alone is **366× worse by colliding worlds and 791× worse
/// by founders lost** (both axes named, because they differ). The reason is
/// legible rather than statistical. Dumping the pairs it newly collides shows
/// one shape every time: a people founds at a site in some year, the attempt
/// is raided and closed in that same year (`founded == ended`,
/// `peak_population == 8`, `cause == Fled`, `ended_by == By(..)`), and a
/// second record — same people, same site, same year, **same parent** —
/// carries the community that took. Those two records are **identical in every
/// founding-side field there is**, so no depth of ancestry separates them: a
/// two-hop key produces bit-identical keys for these pairs because the
/// grandparent is identical too (measured, not assumed). Only a post-founding
/// fact can separate them, which is exactly what a discrimination key is
/// entitled to read and an identity key is not.
///
/// **The mechanism the plan gave for the hop was wrong, and the hop works
/// anyway.** The spec said the pairs are separated because "one record's ender
/// is the other's parent, so the parent's own material facts separate them."
/// Measured: the two parents genuinely differ at 283, 705 and 2403 — which is
/// *why* those three clear — but they are identical at 2634 and 2898 and in
/// all 732 worlds the identity-only key collides. The hop earns its place; the
/// story attached to it did not survive measurement.
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
/// **Every field in the discrimination tail is a field a future campaign can
/// recompute, and each one is a forced epoch when it does — so the tail was
/// measured for trimmability and it does not trim.** Seeds 0–999, tail cut to
/// one field at a time:
///
/// ```text
///   discrimination tail        colliding worlds   founders lost
///   ended + peak  <- this               0 / 1000             0
///   ended alone                         5 / 1000             5   (92, 305, 365, 447, 535)
///   peak alone                          5 / 1000             5   (148, 301, 447, 517, 594)
/// ```
///
/// Neither field alone reaches zero, and the two failure sets are nearly
/// disjoint — each field catches pairs the other misses. Seed 447 is in both,
/// which means it holds two separate pairs: one the records separate only by
/// `peak_population`, one only by `ended`. Both fields carry their weight;
/// there is no cheaper tail to fall back to.
///
/// `parent` is the founding coordinates of the occupation `founded_from`
/// names, or `None` — for a `Genesis` founding, and for a caller that holds a
/// record whose predecessor it cannot resolve. Those two cases fold
/// identically, which is [`crate::record::founding_key_from`]'s existing
/// contract and the one `windows/worldgen::descent::founder_of` reads off the
/// ledger; deliberately **not** [`crate::record::layer_key`]'s three-way rank,
/// so that the identity half of this handle stays bit-for-bit the key a
/// ledger-side caller derives for the same founding.
pub fn founder_handle(
    occ: &OccupationRecord,
    parent: Option<crate::record::FoundingCoords<'_>>,
) -> RoleHandle {
    use crate::record::{day_key, founding_key, mix};

    // STEP 1 — IDENTITY. Is this the same founding? Delegated, never copied:
    // a ledger-side caller derives this same value from committed facts alone
    // (`windows/worldgen::descent::founder_of`), and two derivations of one
    // founding's identity must not be two pieces of arithmetic that agree by
    // inspection.
    let identity = founding_key(&occ.core, parent);

    // STEP 2 — DISCRIMINATION. Is this one unique across the cast? The
    // identity key is not, and cannot be made so: see this function's doc.
    // Nothing below may ever be read as part of the founding's identity —
    // it exists solely to separate two records of one founding.
    //
    // `ended` folds with a presence tag and through `day_key`, matching
    // `material_key` rather than the raw `to_bits` the superseded key used:
    // `to_bits` disagrees with float order on negatives and `-0.0`, and a
    // sentinel `u64::MAX` for `None` is a value a real day could in principle
    // reach. Neither is reachable from today's bake, and neither should have
    // to be argued about again.
    let mut h = match occ.core.ended {
        Some(d) => mix(mix(identity, 1), day_key(d)),
        None => mix(identity, 0),
    };
    h = mix(h, u64::from(occ.core.peak_population));

    // The role discriminant last, so a future second role at the same
    // occupation cannot collide with the founder.
    RoleHandle(mix(h, FOUNDER_ROLE))
}

/// The age (standard years) past which a *perishable* find — cloth, wood,
/// food, ash, a child's doll — has rotted back into the soil. Only a young
/// ruin still holds them; this is the same threshold the engine has always
/// called the "young ruin" age.
/// plumb: pending(wave-1)
const PERISHABLE_MAX_AGE: f64 = 250.0;

/// The age (standard years) past which even a *durable* find — fired clay,
/// bone, dressed stone, worked flint — has finally weathered away. Set at
/// millennia scale on purpose: real durable archaeology (Chaco Canyon, Mesa
/// Verde, Jericho) endures far longer than the mere centuries this engine's
/// ruins actually reach, so *every* ruin the deep-history bake produces —
/// including the real world's ancient climate-abandoned hamlets — still
/// yields a legible archaeological impression rather than bare ground.
/// plumb: pending(wave-1)
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
    /// The paleoclimate turned the vertex hostile: the people walked away over a
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
/// plumb: pending(wave-1)
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
            CauseOfEnd::Famine | CauseOfEnd::Fled | CauseOfEnd::Breached => {
                // A hurried departure: a tool dropped, the pots left behind,
                // the dwelling lines abandoned to the grass.
                //
                // **`Breached` shares this arm deliberately** (The Winze, spec
                // §4.3/§4.6). A working that breaks through ends where it
                // stands, and what a searcher finds is exactly this: the tools
                // still at the face, the pots in the huts, the turf-lines of a
                // camp nobody packed up. Giving the breach its own assemblage
                // was considered and refused twice over. `ResidueItem::Bones`
                // would assert the delvers were *destroyed*, and spec §4.4
                // leaves that open on purpose — a breach "flees or is
                // destroyed", and the model does not know which. Anything
                // more specific would be a description of what came through,
                // which §4.6 forbids to every surface, this one included: the
                // residue is the strongest temptation in the file to name it,
                // because a find is exactly where an author would put the
                // claw-mark.
                items.push(ResidueItem::Tool);
                items.push(ResidueItem::Potsherd);
                items.push(ResidueItem::Foundation);
            }
            CauseOfEnd::Migrated => match departure {
                Departure::Climate => {
                    // Climate abandonment is the real world's dominant end (a
                    // vertex the paleoclimate turned hostile, walked away from
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
    /// plumb: pending(wave-1)
    const LONGHOUSE_POPULATION_FLOOR: u32 = 200;
    /// plumb: pending(wave-1)
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

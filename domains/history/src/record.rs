//! The occupation record: one span of a people occupying a site, from
//! founding to (optionally) ending. This is the substrate every later
//! living-community task derives flesh from — a settlement's whole history
//! is a sequence of these, not a single snapshot.

use hornvale_kernel::{CellId, EntityId, KindId};

/// Why an occupation ended (drawn cause; `None` means still alive).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CauseOfEnd {
    /// The community starved out.
    Famine,
    /// The site was put to the torch.
    Burned,
    /// Disease emptied the site.
    Plague,
    /// The people fled without a conquering hand behind it.
    Fled,
    /// The people migrated onward, in an orderly fashion.
    Migrated,
}

/// What a community at this site was for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Function {
    /// Farming and herding.
    Agrarian,
    /// Extraction — ore, stone, salt.
    Mine,
    /// A waypoint or market on a trade route.
    Trade,
    /// A shrine or temple seat.
    Cult,
    /// A garrisoned defensive point.
    Fort,
}

/// A community's technological horizon, ordinal (neolithic is the floor,
/// classical the ceiling this engine models).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TechHorizon {
    /// Stone-tool, pre-metal.
    Neolithic,
    /// Bronze-working.
    Bronze,
    /// Iron-working.
    Iron,
    /// Classical-era statecraft and engineering.
    Classical,
}

/// How an occupation ended: on its own terms, or at another entity's hand
/// (the ★ global thread — every "ended by" reference in the engine resolves
/// through this same shape).
///
/// Generic over the handle type so the bake can reference its own private
/// handles and the ledger side can reference committed entities, without the
/// two being interchangeable.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ended<I> {
    /// No antagonist entity — famine, plague, or an orderly departure.
    Nature,
    /// Ended at the hand of another entity (a raiding people, a rival
    /// community, ...).
    By(I),
}

/// How an occupation began: raised from nothing at a site, or founded by
/// settlers from another community (the ★ global thread — every "founded
/// from" reference in the engine resolves through this same shape).
///
/// Generic over the handle type, for the same reason as [`Ended`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Founding<I> {
    /// The first occupation at a site — no predecessor community.
    Genesis(CellId),
    /// Founded by settlers dispatched from an existing community.
    From(I),
}

/// How notable an occupation was in its region.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Notability {
    /// Unremarkable, peripheral to regional affairs.
    Backwater,
    /// An ordinary community, neither notable nor obscure.
    Common,
    /// A regional seat of power or pilgrimage.
    Seat,
}

/// What both sides of the emit boundary agree an occupation is: a people, a
/// place, a span, and how it fared. Everything here is a committed fact or
/// derivable from one.
///
/// The handle-bearing fields — which community, which lineage, who founded it,
/// who ended it — live on the bake-side and ledger-side types instead, because
/// they mean different things there.
/// type-audit: bare-ok(count: founded), bare-ok(count: ended), bare-ok(count: peak_population)
#[derive(Clone, Debug, PartialEq)]
pub struct Occupation {
    /// The people occupying the site.
    pub people: KindId,
    /// The Geosphere cell the occupation sits on.
    pub site: CellId,
    /// The standard day the occupation began.
    pub founded: f64,
    /// The standard day the occupation ended, `None` if still alive.
    pub ended: Option<f64>,
    /// The highest population this occupation ever reached.
    pub peak_population: u32,
    /// The technological horizon of this occupation.
    pub tech: TechHorizon,
    /// What this occupation was for.
    pub function: Function,
    /// The deity this occupation's people held foremost, if any.
    pub deity: Option<KindId>,
    /// The tongue this occupation's people spoke, if any.
    pub tongue: Option<KindId>,
    /// Why the occupation ended, if it has.
    pub cause: Option<CauseOfEnd>,
    /// How notable the occupation was.
    pub notability: Notability,
}

impl Occupation {
    /// How long the occupation has lasted (or lasted), in standard days, as
    /// of `now`. Ended occupations ignore `now` entirely.
    /// type-audit: bare-ok(count: now), bare-ok(count: return)
    pub fn tenure(&self, now: f64) -> f64 {
        self.ended.unwrap_or(now) - self.founded
    }

    /// Whether the occupation is still ongoing.
    /// type-audit: bare-ok(flag: return)
    pub fn is_alive(&self) -> bool {
        self.ended.is_none()
    }
}

/// One span of a people occupying a site, as **reconstructed from committed
/// facts**. The ledger-side half of the pair.
///
/// It carries no `community` and no `lineage`: neither is ever emitted as a
/// fact, so a reconstructed record genuinely does not know them. What it does
/// know is its own identity, which earlier versions of this type smuggled into
/// the `community` field and called a placeholder.
#[derive(Clone, Debug, PartialEq)]
pub struct OccupationRecord {
    /// The facts both sides agree on.
    pub core: Occupation,
    /// This occupation's own entity — the subject of every fact above.
    pub id: EntityId,
    /// How the occupation began.
    pub founded_from: Founding<EntityId>,
    /// How the occupation ended.
    pub ended_by: Ended<EntityId>,
}

impl OccupationRecord {
    /// How long the occupation lasted, as of `now`. Delegates to [`Occupation`].
    /// type-audit: bare-ok(count: now), bare-ok(count: return)
    pub fn tenure(&self, now: f64) -> f64 {
        self.core.tenure(now)
    }

    /// Whether the occupation is still ongoing. Delegates to [`Occupation`].
    /// type-audit: bare-ok(flag: return)
    pub fn is_alive(&self) -> bool {
        self.core.is_alive()
    }
}

/// A `u64` whose unsigned order matches `f64::total_cmp` for every input,
/// including negatives, `-0.0`, and NaN. Lets [`layer_key`] be a plain `Ord`
/// tuple instead of a hand-written comparator, without inheriting `to_bits`'s
/// positives-only precondition: `to_bits` alone agrees with float order only
/// for non-negative, non-NaN inputs (`(-0.0).to_bits() == 1 << 63`, which
/// would sort after every positive day), and nothing pins the day fields this
/// key reads to that range — `founded`/`ended` come back from an already-
/// quantized, non-negative ledger today, but `BakeConfig::start_year` is a
/// bare `pub f64` with no such validation, so the guarantee belongs in the
/// key, not in a comment about its callers.
fn day_key(x: f64) -> u64 {
    let b = x.to_bits();
    if b >> 63 == 1 { !b } else { b | 1 << 63 }
}

/// The order a site's layers stack in: material facts only, oldest-founded
/// first.
///
/// Lives here rather than beside either caller because `windows/worldgen` and
/// `windows/almanac` both need it and neither depends on the other — the same
/// reason their decoders are duplicated. The decoders still are; this is one
/// less thing that has to be kept in lockstep by hand.
///
/// A layer that closed earlier lies deeper, which is what a stratigraphy is; a
/// still-living occupation is the top layer, so `None` sorts LAST; peak breaks
/// the remainder. `founded_from` closes the final ties — ancestry is genuinely
/// what distinguishes two occupations sharing a site, an epoch, a fate and a
/// size (measured: 6 such records in seed 42, 4 in seed 7, 0 in seed 1000,
/// separable by nothing else).
///
/// **Total given one invariant this crate does not own**: two layers both
/// `Founding::Genesis` at the same site carry an *identical* fourth key
/// (`Genesis` encodes only the site's own cell), so if the (founded, ended,
/// peak) prefix also ties, the key ties too. That never happens today only
/// because the bake opens at most one `Genesis` occupation per site
/// (`windows/worldgen`'s `history_bake.rs`) — a `domains/history` doc leaning
/// on a `windows/worldgen` invariant. A future re-genesis path (a site razed
/// and refounded from nothing a second time) must revisit this before
/// claiming the key is total again.
///
/// The fourth key itself is a compromise, not a material fact: `Founding::From`
/// orders two descended layers by the *predecessor's* `EntityId` — an identity
/// handle, not anything the world states about either occupation, and itself a
/// mint-order artifact. It is specified behavior (there is no other handle in
/// the ledger to break this tie with) and, per the invariant above, it is in
/// practice the discriminator actually doing the work whenever the first three
/// keys tie — Genesis-vs-Genesis never reaches it. A future encoding that gave
/// a founding its own material identity (a "signet") would close this gap;
/// until then, this is documented as the resort it is, not sold as more.
/// type-audit: bare-ok(count: return)
pub fn layer_key(r: &OccupationRecord) -> (u64, u8, u64, std::cmp::Reverse<u32>, u8, u64) {
    let founded = day_key(r.core.founded);
    let (ended_rank, ended) = match r.core.ended {
        Some(d) => (0u8, day_key(d)),
        None => (1u8, 0),
    };
    let (from_rank, from) = match r.founded_from {
        Founding::Genesis(c) => (0u8, u64::from(c.0)),
        Founding::From(e) => (1u8, e.get()),
    };
    (
        founded,
        ended_rank,
        ended,
        std::cmp::Reverse(r.core.peak_population),
        from_rank,
        from,
    )
}

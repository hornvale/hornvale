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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ended {
    /// No antagonist entity — famine, plague, or an orderly departure.
    Nature,
    /// Ended at the hand of another entity (a raiding people, a rival
    /// community, ...).
    By(EntityId),
}

/// How an occupation began: raised from nothing at a site, or founded by
/// settlers from another community (the ★ global thread — every "founded
/// from" reference in the engine resolves through this same shape).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Founding {
    /// The first occupation at a site — no predecessor community.
    Genesis(CellId),
    /// Founded by settlers dispatched from an existing community.
    From(EntityId),
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

/// One span of a people occupying a site: the atom of Hornvale's history
/// domain. A community's full history is a sequence of these, chained by
/// `founded_from`/`ended_by`.
/// type-audit: bare-ok(count: founded), bare-ok(count: ended), bare-ok(count: peak_population)
#[derive(Clone, Debug, PartialEq)]
pub struct OccupationRecord {
    /// The people occupying the site.
    pub people: KindId,
    /// The community entity this occupation belongs to.
    pub community: EntityId,
    /// The lineage entity this occupation continues (may equal `community`
    /// for a community's first occupation).
    pub lineage: EntityId,
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
    /// How the occupation ended (nature, or another entity's hand).
    pub ended_by: Ended,
    /// How the occupation began (genesis, or founded from another
    /// community).
    pub founded_from: Founding,
    /// How notable the occupation was.
    pub notability: Notability,
}

impl OccupationRecord {
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

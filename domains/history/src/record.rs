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
/// `pub` since The Salt: `windows/almanac`'s `conquest_victim` needs the same
/// float-ordering key to break its candidate set materially rather than by
/// entity id, and a second hand-written copy of a bit-twiddling comparator is
/// how two decoders drift apart in the first place.
/// type-audit: bare-ok(count: x), bare-ok(identifier-text: return)
pub fn day_key(x: f64) -> u64 {
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
/// The fourth key is now a material fact, not a compromise (The Salt, C3):
/// `Founding::From` orders two descended layers by the *predecessor's own
/// founding coordinates* — [`FoundingCoords`], the same `(people, site,
/// founded)` triple [`founding_key`] folds for the founder handle — folded
/// with the same [`mix`]/[`mix_str`] arithmetic under its own tag
/// (`0x5361_6C74_0000_0003`, distinct from `material_key`'s and
/// `founding_key`'s tags, so the three derivations never collide with each
/// other by construction). This closes the gap an earlier version of this
/// doc left open: it ordered by the predecessor's `EntityId` — "a compromise,
/// not a material fact … itself a mint-order artifact" — and pointed at a
/// future encoding that gave a founding its own material identity (a
/// "signet") to close it. `FoundingCoords` is that signet. When the caller
/// cannot resolve the predecessor at all (it holds a record for another
/// site, or the predecessor is missing), the tail ranks last (`from_rank ==
/// 2`) and ties with every other unresolved predecessor rather than reading
/// anything id-shaped. Measured: this changes the rendered layer order at 0
/// sites (seed 42), 1 (seed 7), 0 (seed 1000) — see
/// `windows/worldgen/tests/history_emit.rs`'s
/// `the_material_fourth_key_barely_moves_the_stratigraphy`.
///
/// **Sort with a STABLE sort.** Where the key ties — the re-genesis case
/// above, or the unresolved-predecessor case just described — the three call
/// sites agree only because `sort_by_key` is stable and all three sort the
/// same ledger iteration order, so all three fall back to the same order.
/// This matters *more* than it did under the id-valued tail: that one tied
/// only in the narrow re-genesis case, while the material tail ties whenever
/// a caller does not hold the predecessor's record, which is routine for a
/// single-site view. Ties fall through to ledger iteration order, which is
/// commit order — invariant under any change of id derivation, because
/// minting order and commit order are the same fact read two ways, and a
/// future re-derivation of ids (The Signet) moves ids, not the order facts
/// were committed in. Switching any of the three call sites to
/// `sort_unstable_by_key` would diverge them precisely where this key is not
/// total, and their agreement is a contract those decoders declare about
/// themselves.
/// type-audit: bare-ok(count: return)
pub fn layer_key(
    r: &OccupationRecord,
    parent: Option<FoundingCoords<'_>>,
) -> (u64, u8, u64, std::cmp::Reverse<u32>, u8, u64, u64, u64) {
    let founded = day_key(r.core.founded);
    let (ended_rank, ended) = match r.core.ended {
        Some(d) => (0u8, day_key(d)),
        None => (1u8, 0),
    };
    // The fourth key: ancestry, stated materially. `Genesis` keeps encoding
    // the site's own cell, exactly as before; `From` now orders by the
    // PREDECESSOR'S FOUNDING COORDINATES rather than its `EntityId`.
    let (from_rank, from_a, from_b, from_c) = match (r.founded_from, parent) {
        (Founding::Genesis(c), _) => (0u8, u64::from(c.0), 0, 0),
        (Founding::From(_), Some(p)) => (
            1u8,
            u64::from(p.site.0),
            day_key(p.founded),
            mix_str(0x5361_6C74_0000_0003, p.people),
        ),
        // Descended from an occupation this caller could not resolve. Ranks
        // after every resolvable predecessor and ties with its fellows,
        // which the stable sort then leaves in commit order.
        (Founding::From(_), None) => (2u8, 0, 0, 0),
    };
    (
        founded,
        ended_rank,
        ended,
        std::cmp::Reverse(r.core.peak_population),
        from_rank,
        from_a,
        from_b,
        from_c,
    )
}

/// The material coordinates of a *founding* — where, when, and by whom a
/// community was raised. Id-free by construction: every field is a fact the
/// world states about the occupation, never a handle.
///
/// Used two ways, both keyed to the same causal horizon: as the ancestry hop
/// in [`founding_key`], and as [`layer_key`]'s predecessor tie-break.
/// `Copy` is available because `KindId` and `CellId` both are; `Occupation`
/// itself is `Clone` only, which is why the tests below clone rather than move.
/// The people label is a bare `&str`, not a `KindId`, on purpose: the key
/// folds it by content, and requiring a `KindId` would force every caller to
/// resolve the label against the canonical roster first. A Lab synthetic
/// roster's species (`goblin-twin`) is absent from that roster, so such a
/// caller would resolve nothing and collapse every founder in
/// `census-of-the-meeting` onto one handle.
/// type-audit: bare-ok(count: founded), bare-ok(identifier-text: people)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FoundingCoords<'a> {
    /// The people who founded, by label.
    pub people: &'a str,
    /// The cell founded on.
    pub site: CellId,
    /// The standard day founded.
    pub founded: f64,
}

/// This occupation's own founding coordinates.
pub fn founding_coords(c: &Occupation) -> FoundingCoords<'static> {
    FoundingCoords {
        people: c.people.0,
        site: c.site,
        founded: c.founded,
    }
}

/// A splitmix-style mix step. Mirrors [`crate::flesh::persona_of`]'s
/// arithmetic so every derived handle in this crate is drawn from one space.
/// Pure bit arithmetic — no transcendental, no `libm`, no platform dependence.
fn mix(state: u64, x: u64) -> u64 {
    let mut z = state ^ x;
    z = z.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    z ^= z >> 29;
    z = z.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z ^ (z >> 32)
}

/// Fold a label's bytes in, so `KindId`s hash by content rather than by
/// pointer.
fn mix_str(state: u64, s: &str) -> u64 {
    let mut h = state;
    for b in s.as_bytes() {
        h = mix(h, u64::from(*b));
    }
    mix(h, s.len() as u64)
}

/// A derived handle for an occupation's whole material life — every fact the
/// world states about it, and no id.
///
/// Feeds the flesh derivations (residue, structures), where using the *whole*
/// core is correct: a ruin reflects the size a place reached and the way it
/// ended, not only its founding.
///
/// Two occupations identical in every material fact share this key, and
/// therefore leave identical remains. That is the intended output, not a
/// collision to be broken — under the entity id this replaced, two identical
/// occupations got *different* potsherds, which was entropy fabricated from
/// mint order. Measured rate: 1.0% / 0.2% / 0.3% of occupations at seeds
/// 42 / 7 / 1000, and **0.0% of the layers that actually render flesh** at
/// all three.
/// type-audit: bare-ok(identifier-text: return)
pub fn material_key(c: &Occupation) -> u64 {
    let mut h = mix_str(0x5361_6C74_0000_0001, c.people.0);
    h = mix(h, u64::from(c.site.0));
    h = mix(h, day_key(c.founded));
    h = match c.ended {
        Some(d) => mix(mix(h, 1), day_key(d)),
        None => mix(h, 0),
    };
    h = mix(h, u64::from(c.peak_population));
    h = mix(h, c.tech as u64);
    h = mix(h, c.function as u64);
    h = match c.deity {
        Some(k) => mix_str(mix(h, 1), k.0),
        None => mix(h, 0),
    };
    h = match c.tongue {
        Some(k) => mix_str(mix(h, 1), k.0),
        None => mix(h, 0),
    };
    h = match c.cause {
        Some(x) => mix(mix(h, 1), x as u64),
        None => mix(h, 0),
    };
    mix(h, c.notability as u64)
}

/// [`founding_key`] from already-resolved coordinates, for a caller that
/// read the founding off the ledger rather than holding a whole core.
///
/// This is the entry point Task 3's `founder_of` needs: it resolves founding
/// coordinates straight off the ledger and never holds a whole `Occupation`,
/// so it cannot call [`founding_key`] without fabricating one. A fabricated
/// `Occupation` with placeholder `ended`/`cause` fields is exactly the kind
/// of inert-placeholder construction this repo has been bitten by before, so
/// this sibling exists instead of a `synthetic_core` helper.
/// type-audit: bare-ok(identifier-text: return)
pub fn founding_key_from(own: FoundingCoords<'_>, parent: Option<FoundingCoords<'_>>) -> u64 {
    let mut h = mix_str(0x5361_6C74_0000_0002, own.people);
    h = mix(h, u64::from(own.site.0));
    h = mix(h, day_key(own.founded));
    match parent {
        Some(p) => {
            h = mix(h, 1);
            h = mix_str(h, p.people);
            h = mix(h, u64::from(p.site.0));
            mix(h, day_key(p.founded))
        }
        None => mix(h, 0),
    }
}

/// A derived handle for the *founding* of an occupation, plus one hop of
/// ancestry — where, when and from whom.
///
/// Feeds the founder role handle behind every person name. Deliberately
/// **excludes** everything after the founding (`ended`, `peak_population`,
/// `cause`, `notability`): a founder's name must not be a function of how
/// their community later died. The ancestry hop is what recovers the
/// discrimination that exclusion costs — measured stem-collision rate
/// 8.4% / 3.3% / 3.6% at seeds 42 / 7 / 1000, against 27.7% / 14.8% / 16.2%
/// for the founding triple alone (spec D2, Nathan's ruling).
/// type-audit: bare-ok(identifier-text: return)
pub fn founding_key(c: &Occupation, parent: Option<FoundingCoords<'_>>) -> u64 {
    founding_key_from(founding_coords(c), parent)
}

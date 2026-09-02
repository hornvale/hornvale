//! The roll (The Roll, spec §3.2): which of a session's bodies a tick
//! advances.
//!
//! A roll is two things at once — the register of who belongs to a place, and
//! the call that establishes who is present. [`crate::residents`] builds the
//! register; this module makes the call. It is a **pure function** of (each
//! body's home, the observer's room), so the same world observed from the
//! same room always yields the same roll, whatever route the observer took to
//! get there and whatever a body's own history has been.
//!
//! **Home, not current position, decides membership.** A resident who
//! wandered a room out is still ticked while its settlement is within call,
//! which keeps the roll a function of world state (the settlement's site, the
//! herd's attractor) rather than of the body's own walk. Spec §3.2 says this
//! in as many words; it is the reason [`roll_of`] reads `body.home` and never
//! `agent_position`.
//!
//! **Both constants here are budgets, not world facts** (spec §3.2): the
//! first setting of a knob the campaign's M2 measures, not a claim about how
//! far a voice carries.

use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::{EntityId, Facet, FacetId, RoomMeshMemo};

use crate::body::Body;

/// How many walk-band hops out "within call" reaches. A BUDGET, not a world
/// fact (spec §3.2).
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const ROLL_HOPS: u32 = 2;

/// The most bodies a tick advances. A BUDGET, not a world fact (spec §3.2).
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const ROLL_BUDGET: usize = 128;

/// Every room within `hops` of `centre`, with its BFS depth — `centre` itself
/// at depth `0`. The depth is what [`roll_of`] orders by, so this is the
/// primitive and [`rooms_within`] is the projection of it that asks only
/// membership.
///
/// A room whose [`Facet::pack`] fails (a path past the kernel's depth cap,
/// never reached at a session's own walk depth) is silently dropped — the
/// same "coarse constrains fine, never blocks" posture
/// `liveness::built_rooms` takes toward world-derived geometry.
/// type-audit: bare-ok(count: hops), bare-ok(count: return)
pub fn hops_within(centre: &Facet, hops: u32, memo: &mut RoomMeshMemo) -> BTreeMap<FacetId, u32> {
    let mut seen: BTreeMap<FacetId, u32> = BTreeMap::new();
    let mut frontier: Vec<Facet> = vec![centre.clone()];
    if let Ok(id) = centre.pack() {
        seen.insert(id, 0);
    }
    for depth in 1..=hops {
        let mut next = Vec::new();
        for room in &frontier {
            for n in room.neighbors_memo(memo) {
                let Ok(id) = n.pack() else {
                    continue;
                };
                // A room already seen was reached at a shallower depth (this
                // is a breadth-first walk), so its recorded depth is already
                // the right one and it is not expanded a second time.
                if let Entry::Vacant(slot) = seen.entry(id) {
                    slot.insert(depth);
                    next.push(n);
                }
            }
        }
        frontier = next;
    }
    seen
}

/// Every room within `hops` of `centre` (BFS over `neighbors_memo`), `centre`
/// included.
/// type-audit: bare-ok(count: hops)
pub fn rooms_within(centre: &Facet, hops: u32, memo: &mut RoomMeshMemo) -> BTreeSet<FacetId> {
    hops_within(centre, hops, memo).into_keys().collect()
}

/// One body's place in the roll's order: nearest first, residents before
/// wild at equal distance, then by whose it is, then by which of them.
///
/// The field ORDER is the sort order — this type's derived [`Ord`] is the
/// comparison [`roll_of`] uses, so there is no second place the ordering
/// could be stated differently.
///
/// **`parent` and `species` together are "whose it is", and they are read
/// through `wild`.** A resident's parent is its settlement's [`EntityId`]
/// and its species is the empty string; a wild body's parent is its herd's
/// ATTRACTOR VERTEX and its species is the herd's own label. Two herds at one
/// vertex are told apart by species and two herds of one species by vertex,
/// which is exactly the (species, vertex) pair `derive_wild_herds` keys a
/// herd's identity on.
///
/// **Why the wild half is not simply `0`, which is what the first cut of this
/// type did.** With every wild body carrying the same parent, two herds at
/// equal hop distance tied on the whole key and fell through to the body's
/// roster INDEX — derivation order, which is the order the settlements and
/// herds entered the observer's window, which depends on the route walked.
/// Under a binding budget that made the MASK route-dependent: approach a pair
/// of herds from the north and one is ticked, from the south and the other
/// is. Spec §3.2 says a wild body's identity is keyed by (species, attractor
/// vertex, member index) and "never by its position in a list", and the
/// ORDER has to obey that as much as the identity does.
///
/// **`parent` is a bare `u64` rather than the `Option<EntityId>` this
/// carried before.** It now holds one of two things — a settlement's entity
/// id or an attractor's vertex — discriminated by `wild`, and those are
/// different namespaces. A typed `EntityId` would be a lie for half its
/// inhabitants; the honest reading is "an order key, meaningless except
/// against another key with the same `wild`".
/// type-audit: bare-ok(count: hops), bare-ok(flag: wild), bare-ok(index: parent), bare-ok(identifier-text: species), bare-ok(index: ordinal)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RollKey {
    /// How many walk-band hops the body's home is from the observer's room.
    pub hops: u32,
    /// `false` for a resident, `true` for a wild body — so residents sort
    /// before wild at equal distance (`false < true`).
    pub wild: bool,
    /// A resident's settlement id, or a wild body's attractor vertex.
    pub parent: u64,
    /// A wild body's herd species; empty for a resident.
    pub species: String,
    /// The body's index within its own derivation — a resident's lineage
    /// ordinal, a herd member's member index.
    pub ordinal: u16,
}

/// The half of a [`RollKey`] that a [`Body`] does not carry, recorded when
/// the body is appended to a session's roster.
///
/// **Why a parallel record rather than more [`Body`] fields.** `Body` has
/// 28+ construction sites across `windows/vessel` and `windows/lab`, and
/// neither an ordinal nor an attractor vertex is meaningful except relative
/// to the derivation that produced the body — a lab fixture that fabricates
/// one has neither to give. So the deriving caller records the key beside the
/// body it appends (`Session::roll_keys`), and [`roll_of`] takes the two
/// slices together.
/// type-audit: bare-ok(flag: wild), bare-ok(index: parent), bare-ok(identifier-text: species), bare-ok(index: ordinal)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RollKeyStatic {
    /// `false` for a resident, `true` for a wild body.
    pub wild: bool,
    /// A resident's settlement id, or a wild body's attractor vertex.
    pub parent: u64,
    /// A wild body's herd species; empty for a resident.
    pub species: String,
    /// The body's index within its own derivation.
    pub ordinal: u16,
}

impl RollKeyStatic {
    /// The key for resident `ordinal` of the settlement `village`.
    /// type-audit: bare-ok(index: ordinal)
    pub fn resident(village: EntityId, ordinal: u16) -> RollKeyStatic {
        RollKeyStatic {
            wild: false,
            parent: village.0.get(),
            species: String::new(),
            ordinal,
        }
    }

    /// The key for member `member` of the herd of `species` at attractor
    /// `vertex` — the same (species, vertex, member) triple
    /// `liveness::derive_wild_herds` mints that member's entity from, so the
    /// order key and the identity cannot drift apart.
    /// type-audit: bare-ok(index: vertex), bare-ok(identifier-text: species), bare-ok(index: member)
    pub fn herd_member(vertex: u32, species: &str, member: u16) -> RollKeyStatic {
        RollKeyStatic {
            wild: true,
            parent: u64::from(vertex),
            species: species.to_string(),
            ordinal: member,
        }
    }

    /// The best key derivable from a [`Body`] ALONE, at `ordinal`.
    ///
    /// **Incomplete for a wild body, deliberately and only as a fallback.** A
    /// `Body` records its species but not the attractor it was drawn at
    /// (`liveness::wild_body` keeps no vertex), so a wild body's `parent`
    /// here is `0` and only its species tells it from another herd. Every
    /// caller that HAS the herd must use [`Self::herd_member`] instead; this
    /// exists for the two cases with nothing better to read — a staged
    /// tableau's village-less cast, which shares one room and never grows,
    /// and [`roll_of`]'s guard against a `keys` slice shorter than `bodies`.
    /// type-audit: bare-ok(index: ordinal)
    pub fn of(body: &Body, ordinal: u16) -> RollKeyStatic {
        match body.village.as_ref() {
            Some(village) => RollKeyStatic::resident(village.id, ordinal),
            None => RollKeyStatic {
                wild: true,
                parent: 0,
                species: body.species.clone(),
                ordinal,
            },
        }
    }
}

/// The roll: which of `bodies` a tick advances — a pure function of (the
/// bodies' homes, the observer's room). Returns a mask the same length as
/// `bodies`.
///
/// A body whose home is outside the `hops` window is off the roll. The
/// in-window ones are ordered by [`RollKey`] and the first `budget` kept; the
/// rest are dormant this tick (spec §3.7).
///
/// `keys` is the static half of each body's key, index-aligned with `bodies`
/// (see [`RollKeyStatic`]). A body with no key falls back to
/// [`RollKeyStatic::of`] at ordinal `0`, and the body's own INDEX is the
/// final tie-break in every case — so a short, absent or duplicated `keys`
/// can only ever change the order within a tie, never make the function
/// non-deterministic.
/// type-audit: bare-ok(count: hops), bare-ok(count: budget), bare-ok(flag: return)
pub fn roll_of(
    bodies: &[Body],
    keys: &[RollKeyStatic],
    observer: &Facet,
    hops: u32,
    budget: usize,
    memo: &mut RoomMeshMemo,
) -> Vec<bool> {
    let depths = hops_within(observer, hops, memo);
    let mut ranked: Vec<(RollKey, usize)> = Vec::new();
    for (i, body) in bodies.iter().enumerate() {
        let Ok(home) = body.home.pack() else {
            continue;
        };
        let Some(&depth) = depths.get(&home) else {
            continue;
        };
        let statics = keys
            .get(i)
            .cloned()
            .unwrap_or_else(|| RollKeyStatic::of(body, 0));
        ranked.push((
            RollKey {
                hops: depth,
                wild: statics.wild,
                parent: statics.parent,
                species: statics.species,
                ordinal: statics.ordinal,
            },
            i,
        ));
    }
    ranked.sort();
    let mut mask = vec![false; bodies.len()];
    for (_, i) in ranked.into_iter().take(budget) {
        mask[i] = true;
    }
    mask
}

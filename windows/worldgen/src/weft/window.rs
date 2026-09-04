//! The residency window (The Weft, Task 6; spec §5.4): a caller-owned cache
//! over [`super::prevalence`]/[`super::occurs`], so a walk-band read
//! (`brief_of`, on every `look` and every `enter`) does not re-derive the
//! whole surface from scratch on every call.
//!
//! **A window is a cache, and a cache must not be observable.** Every entry
//! this store holds is [`hornvale_kernel::derived::Validity::Pure`] — a pure
//! function of [`WeftKey`], never invalidated — which is what the module doc
//! on [`hornvale_kernel::derived`] calls "`Pure` with the world's identity …
//! folded into the key". [`WeftKey`] carries every input the derivation
//! reads: the seed (world identity), the globe level (which grid
//! [`Facet::corner_weights`] blends against — the same value
//! `hornvale_kernel::room::RoomMeshMemo::corner_weights_geo_level` keys on,
//! for the same reason), the facet, and the kind (`prevalence`/`occurs` both
//! branch their stream derivation and their macro-state recipe on it). A key
//! that omitted any one of these would let two different worlds, grids or
//! kinds collide on the same entry — the key-completeness bug
//! [`hornvale_kernel::derived::Derived::evict_all`]'s chaos-eviction rung
//! exists to pressure, and this module's own tests mirror that battery.
//!
//! **Caller-owned, `&mut`-threaded, no hidden cache.** `Derived::get` counts
//! hits and misses, so it takes `&mut self` — [`WeftWindow`] is threaded the
//! same way, never behind a `&self` and never wrapped in a `RefCell` (lexicon: the std interior-mutability type, not a place — `ground.rs` carries the same waiver on the same type), per the plan's Global Constraints.
//!
//! # Why "residency window" and not "memo"
//!
//! `GroundHazards` (`windows/vessel/src/ground.rs`) and `RoomMeshMemo`
//! (`kernel/src/room.rs`) are memos: they grow without bound and never evict
//! on their own, because their key spaces (a session's own rooms, a mesh
//! traversal) are naturally small. A weft window's key space is every walk
//! facet in the world, all four kinds deep, which is not small — so this
//! type is a genuine WINDOW: [`WeftWindow::advance_to`] keeps only the
//! facets currently within `radius` of a moving centre resident, evicting
//! the trailing edge as it fills the leading one.

use std::collections::BTreeSet;

use hornvale_kernel::derived::Derived;
use hornvale_kernel::{Facet, FacetId, Geosphere, NearestVertexIndex, Seed};

use crate::FieldPack;
use crate::weft::{WeftKind, occurs, prevalence};

/// Every input a weft feature's derivation reads, folded into one key: the
/// world identity (`Seed`), which grid the facet blends against (the globe
/// level, `u32` — [`Geosphere::depth`]), the facet itself
/// ([`FacetId`], not the unpacked [`Facet`], so the key stays `Copy` and
/// cheap to hold in a `BTreeMap`), and the kind. See the module doc for why
/// completeness here is what makes every entry [`hornvale_kernel::derived::
/// Validity::Pure`].
/// type-audit: bare-ok(count: WeftKey)
pub type WeftKey = (Seed, u32, FacetId, WeftKind);

/// One derived feature realized at a facet: which kind, and the prevalence
/// it was drawn against. The prevalence is kept rather than re-derived,
/// since a downstream reader (a legibility metric, a walk-band line with a
/// magnitude-sensitive phrasing) may want it without a second call into
/// [`prevalence`].
/// type-audit: bare-ok(ratio: prevalence)
#[derive(Clone, Debug, PartialEq)]
pub struct WeftFeature {
    /// Which kind this realization is.
    pub kind: WeftKind,
    /// The prevalence [`occurs`] was drawn against, `[0,1]`.
    pub prevalence: f64,
}

/// The residency window over the derived weft surface. See the module doc.
#[derive(Debug, Default)]
pub struct WeftWindow {
    /// Every currently-resident `(facet, kind)`'s derived features.
    store: Derived<WeftKey, Vec<WeftFeature>>,
    /// The facets [`Self::advance_to`] last filled — bookkeeping only, so a
    /// later call can diff old against new and touch just the trailing and
    /// leading edges. Purely advisory: [`Self::features_at`] never consults
    /// it, so a stale or empty `resident` set can only cost extra derivation
    /// work, never a wrong answer (the store's own `Validity::Pure` entries
    /// are correct regardless of window position — see the module doc).
    resident: BTreeSet<Facet>,
}

impl WeftWindow {
    /// An empty window, holding nothing.
    pub fn new() -> Self {
        Self::default()
    }

    /// `kind`'s derived features at `facet`: the cached entry on a hit, else
    /// [`prevalence`] + [`occurs`], held from then on. Empty where `facet` is
    /// shallower than `geo`'s own level ([`prevalence`] returns `None`
    /// there) or where `occurs` did not fire — never `None` itself, since
    /// "no feature here" and "not yet derived" are the same answer to a
    /// caller.
    pub fn features_at(
        &mut self,
        kind: WeftKind,
        facet: &Facet,
        geo: &Geosphere,
        index: &NearestVertexIndex,
        pack: &FieldPack,
        seed: Seed,
    ) -> &[WeftFeature] {
        let key = weft_key(facet, kind, geo, seed);
        if self.store.get(&key).is_none() {
            let derived = derive_features(kind, facet, geo, index, pack, seed);
            self.store.insert(key, derived);
        }
        self.store
            .get(&key)
            .expect("just inserted above")
            .as_slice()
    }

    /// Move the window's centre to `centre`, holding every facet within
    /// `radius` hops of it (8-connected — [`Facet::neighbors`]'s own metric,
    /// the same Chebyshev disc `windows/vessel/src/lattice/sight.rs`'s
    /// `shadowcast` walks over a different lattice type) resident, for every
    /// kind in [`WeftKind::ALL`].
    ///
    /// **Perimeter work per step, not area — where it matters.** Computing
    /// which facets are now IN the window costs `O((2·radius+1)²)`: cheap
    /// integer lattice arithmetic ([`Facet::neighbors`], no noise sample).
    /// What actually costs (a `SphereFbm` sample, twice, per kind) is bounded
    /// to the TRAILING and LEADING edges only — the facets that left or
    /// entered since the last call — because every facet that stays resident
    /// is a `Derived::get` hit here, not a re-derivation.
    ///
    /// **Measured, not the plan's own arithmetic (Task 6, seed 42, cold start
    /// excluded).** A single straight-line edge step (any of
    /// [`Facet::neighbor_steps`]'s first four, "the four edge-adjacent
    /// rooms") moves the square disc by one unit along one axis, so exactly
    /// one edge strip of width `2·radius+1` enters and one leaves —
    /// **21 derivations/step at radius 10** (measured `misses()` delta,
    /// constant across 19 consecutive steps and across three direction
    /// changes among the four edge steps), not the plan's arithmetic
    /// estimate of ~84 (a quarter of it: `84 / 21 = 4.0` exactly — that
    /// estimate assumed a full-perimeter turnover every step, which only a
    /// DIAGONAL step actually produces: measured **41/step** — `2·(2·radius+1)
    /// − 1`, an L-shaped two-edge turnover minus the one corner both edges
    /// share). At radius 5, the straight-line figure is 11/step
    /// (`2·5+1`); at radius 20, 41/step (`2·20+1`) — both exactly
    /// `2·radius+1`, confirming the formula rather than the radius-10 case
    /// alone. Cold start (an empty window's first call) always derives the
    /// whole disc, `(2·radius+1)²` — 441 at radius 10. See
    /// `docs/superpowers/ledgers/2026-09-03-the-weft.md` for the full
    /// measurement (commands, radii, and the direction-change detail).
    /// type-audit: bare-ok(count: radius)
    pub fn advance_to(
        &mut self,
        centre: &Facet,
        radius: u32,
        geo: &Geosphere,
        index: &NearestVertexIndex,
        pack: &FieldPack,
        seed: Seed,
    ) {
        let new_resident = disc(centre, radius);

        // Collected to OWNED `Vec`s before any mutation: `self.resident` is
        // borrowed by both `difference` calls, and `Self::features_at` below
        // needs `&mut self` (all of it), which cannot coexist with a live
        // borrow of one of its fields.
        let trailing: Vec<Facet> = self.resident.difference(&new_resident).cloned().collect();
        let leading: Vec<Facet> = new_resident.difference(&self.resident).cloned().collect();

        for facet in &trailing {
            let id = facet.pack().expect("a resident facet packs");
            for kind in WeftKind::ALL {
                self.store.evict(&(seed, geo.depth(), id, kind));
            }
        }
        for facet in &leading {
            for kind in WeftKind::ALL {
                self.features_at(kind, facet, geo, index, pack, seed);
            }
        }

        self.resident = new_resident;
    }

    /// How many `(facet, kind)` pairs are currently resident — the window's
    /// own size, for the movement-tax measurement.
    /// type-audit: bare-ok(count: return)
    pub fn resident_len(&self) -> usize {
        self.resident.len() * WeftKind::ALL.len()
    }

    /// Reads served without deriving, ever. Delegates to [`Derived::hits`] —
    /// the plan's own instruction: measure with the store's counters, not a
    /// second one.
    /// type-audit: bare-ok(count: return)
    pub fn hits(&self) -> u64 {
        self.store.hits()
    }

    /// Reads that derived, ever — the noise samples actually taken.
    /// type-audit: bare-ok(count: return)
    pub fn misses(&self) -> u64 {
        self.store.misses()
    }

    /// Drop every held entry (chaos eviction). `resident` is untouched —
    /// see its own doc for why that can only cost extra derivation, never
    /// change an answer.
    pub fn evict_all(&mut self) {
        self.store.evict_all();
    }
}

/// `facet`'s key at `kind`, under `geo`'s own level and `seed` — the single
/// place [`WeftKey`]'s four fields are assembled, so every caller in this
/// module builds the identical key for the identical inputs.
fn weft_key(facet: &Facet, kind: WeftKind, geo: &Geosphere, seed: Seed) -> WeftKey {
    (
        seed,
        geo.depth(),
        facet.pack().expect("a walk-band facet packs"),
        kind,
    )
}

/// The actual derivation `WeftKey` names: [`prevalence`] then, on `Some`,
/// [`occurs`] — realized as a `Vec` of zero or one [`WeftFeature`]. The only
/// place either function is called from this module, so `features_at`'s
/// cache-hit path and its cache-miss path can never compute this two
/// different ways.
fn derive_features(
    kind: WeftKind,
    facet: &Facet,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    pack: &FieldPack,
    seed: Seed,
) -> Vec<WeftFeature> {
    match prevalence(kind, facet, geo, index, pack, seed) {
        Some(p) if occurs(kind, facet, seed, p) => vec![WeftFeature {
            kind,
            prevalence: p,
        }],
        _ => Vec::new(),
    }
}

/// Every facet at 8-connected hop-distance `<= radius` from `centre`, via
/// repeated [`Facet::neighbors`] expansion. At `radius` r, away from any cube
/// corner or seam, this is the Chebyshev square of side `2r + 1`
/// (`(2r+1)²` facets) — where the plan's "~441 held" at radius 10 comes
/// from (`21² = 441`).
fn disc(centre: &Facet, radius: u32) -> BTreeSet<Facet> {
    let mut seen: BTreeSet<Facet> = BTreeSet::new();
    seen.insert(centre.clone());
    let mut frontier = vec![centre.clone()];
    for _ in 0..radius {
        let mut next = Vec::new();
        for f in &frontier {
            for n in f.neighbors() {
                if seen.insert(n.clone()) {
                    next.push(n);
                }
            }
        }
        frontier = next;
    }
    seen
}

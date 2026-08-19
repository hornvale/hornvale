//! Landscape features: individuated regions of the world with draw-free
//! identities. See `docs/superpowers/specs/2026-08-18-the-gazetteer-design.md`.

use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// Connected components of the cells satisfying `member`, under
/// [`Geosphere::neighbors`].
///
/// Returned in ascending order of each component's lowest cell id — the
/// identity the caller assigns — so no call site has to sort. Traversal only:
/// no draws, no transcendentals, integer comparisons, so cross-platform
/// byte-identical.
///
/// The components **partition** the members: every satisfying cell appears in
/// exactly one. That is what makes "lowest cell id" a valid identity, and it
/// is asserted rather than assumed.
/// type-audit: bare-ok(count: return)
pub fn components(geo: &Geosphere, member: impl Fn(CellId) -> bool) -> Vec<BTreeSet<CellId>> {
    let mut visited = vec![false; geo.cell_count()];
    let mut out = Vec::new();
    for start in geo.cells() {
        if visited[start.0 as usize] || !member(start) {
            continue;
        }
        visited[start.0 as usize] = true;
        let mut queue = VecDeque::from([start]);
        let mut set = BTreeSet::new();
        while let Some(cell) = queue.pop_front() {
            set.insert(cell);
            for &nb in geo.neighbors(cell) {
                if !visited[nb.0 as usize] && member(nb) {
                    visited[nb.0 as usize] = true;
                    queue.push_back(nb);
                }
            }
        }
        out.push(set);
    }
    out
}

/// What kind of thing a feature is.
///
/// **The discriminants are load-bearing and must not be reordered.** They form
/// the high half of the naming salt (`windows/worldgen`), so changing one
/// renames every feature of that class in every world. `Volcano = 0`
/// deliberately: it makes a volcano's salt equal its bare cell id, which is
/// exactly what `volcano_name` already draws with, so adopting the scheme
/// moves no volcano name.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FeatureClass {
    /// A volcanic edifice, individuated by `volcano_at` rather than here.
    Volcano = 0,
    /// A connected component of land.
    Landmass = 1,
    /// A connected component of ocean.
    Sea = 2,
    /// A connected component of endorheic salt-sink cells.
    SaltLake = 3,
    /// A maximal subtree of the flow forest.
    River = 4,
}

/// A feature's stable identity: its class and its canonical cell.
/// type-audit: bare-ok(index: cell)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FeatureId {
    /// Which kind of feature.
    pub class: FeatureClass,
    /// The canonical cell — a component's lowest, or a river's terminal.
    pub cell: CellId,
}

/// An individuated region of the world.
/// type-audit: bare-ok(count: magnitude)
#[derive(Clone, Debug)]
pub struct Feature {
    /// The stable identity.
    pub id: FeatureId,
    /// Every cell the feature occupies.
    pub extent: BTreeSet<CellId>,
    /// The cell a label is drawn at.
    pub anchor: CellId,
    /// The integer scalar ranking this feature within its class.
    pub magnitude: u32,
}

/// Fraction of a world's land (or ocean) cell count below which a connected
/// component is a quantization artifact rather than a feature, not "how
/// much land/ocean must exist for a floor to apply". Shared by the
/// `GeneratedTerrain` provider's Landmass/Sea floors (`provider.rs`) and the
/// lab's `continent-count` metric (`windows/lab/src/metrics.rs`) so the two
/// definitions of "is this a continent" cannot drift apart — a single
/// constant, not two copies of the same literal. Earth-calibrated:
/// Greenland is ~1.4% of Earth's land and qualifies, Iceland ~0.07% does
/// not.
/// type-audit: bare-ok(ratio)
pub const PROPORTIONAL_SIZE_FLOOR_FRACTION: f64 = 0.005;

/// Minimum a proportional size floor (`PROPORTIONAL_SIZE_FLOOR_FRACTION` ×
/// a world's land/ocean extent) may fall to, regardless of how small that
/// extent is. The fraction alone is unbounded below: on a world with under
/// ~200 land cells it floors to less than 1, which admits every single-cell
/// component — exactly the quantization-artifact case a floor exists to
/// exclude in the first place (land is a threshold on a continuous field,
/// so a lone cell is noise, not a landmass). 2 is the smallest value that
/// rules that case out: a component must span at least two cells to be
/// more than a single point.
/// type-audit: bare-ok(count)
pub const PROPORTIONAL_SIZE_FLOOR_MIN: usize = 2;

/// Every component of `member` at or above `floor` cells, as features of
/// `class`. Below the floor a component is not a small feature — it is not a
/// feature, and stays anonymous.
/// type-audit: bare-ok(count: floor)
pub fn classify(
    geo: &Geosphere,
    class: FeatureClass,
    member: impl Fn(CellId) -> bool,
    floor: usize,
) -> Vec<Feature> {
    components(geo, member)
        .into_iter()
        .filter(|extent| extent.len() >= floor)
        .map(|extent| {
            let cell = *extent.first().expect("components are nonempty");
            Feature {
                id: FeatureId { class, cell },
                anchor: cell,
                magnitude: extent.len() as u32,
                extent,
            }
        })
        .collect()
}

/// Every river catchment at or above `floor` cells.
///
/// A river is a maximal subtree of the flow forest [`crate::drainage::downhill_targets`]
/// gives, and its identity is that subtree's **terminal** — the ocean cell it
/// empties into, or the interior minimum it dies in. Magnitude is catchment
/// size, so the naming tier is how much land a river drains rather than how
/// much water crosses its mouth.
///
/// The extent holds the **land** that drains to the terminal; when the
/// terminal is an ocean cell it is not itself a member. The partition property
/// in this module's tests pins that.
///
/// The walk is bounded by `cell_count` because the flow forest is acyclic by
/// construction — every hop strictly decreases elevation. The bound is
/// belt-and-braces, not a real termination condition.
/// type-audit: bare-ok(count: floor)
pub fn rivers(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    floor: usize,
) -> Vec<Feature> {
    let downhill = crate::drainage::downhill_targets(geo, elevation, sea_level);
    let mut catchments: BTreeMap<CellId, BTreeSet<CellId>> = BTreeMap::new();
    for c in geo.cells() {
        if *elevation.get(c) < sea_level {
            continue;
        }
        let mut at = c;
        for _ in 0..geo.cell_count() {
            match downhill[at.0 as usize] {
                Some(next) => at = next,
                None => break,
            }
        }
        catchments.entry(at).or_default().insert(c);
    }
    catchments
        .into_iter()
        .filter(|(_, extent)| extent.len() >= floor)
        .map(|(terminal, extent)| Feature {
            id: FeatureId {
                class: FeatureClass::River,
                cell: terminal,
            },
            anchor: terminal,
            magnitude: extent.len() as u32,
            extent,
        })
        .collect()
}

/// The genesis-time collection of every individuated feature, one ordered
/// bucket per [`FeatureClass`].
///
/// **This ordering is the placement channel** — the same question
/// `surrounds_ascii::box_rank` (`clients/game/core/src/chart.rs`) answers one
/// rung down, for ASCII chart cells rather than world features. It is not
/// one of decision 0142's three measurement axes: ordering a feature set is
/// a claim about what fits on the page, not about the world. This campaign
/// ships the ordering and deliberately ships no salience banding — the map
/// campaign sets bands against a rendered picture rather than inventing a
/// number here.
///
/// Within a class, features order by magnitude descending, ties broken by
/// identity (`FeatureId::cell`) ascending — total, deterministic, and
/// integer-only, so no `total_cmp` appears anywhere in this module.
#[derive(Clone, Debug, Default)]
pub struct FeatureIndex {
    by_class: BTreeMap<FeatureClass, Vec<Feature>>,
}

impl FeatureIndex {
    /// Assemble an index from per-class feature lists, sorting each into the
    /// magnitude-descending, identity-ascending total order. Shared by the
    /// genesis-time builder ([`crate::provider::GeneratedTerrain::new`]) and
    /// this module's own tests.
    pub(crate) fn from_parts(parts: Vec<(FeatureClass, Vec<Feature>)>) -> FeatureIndex {
        let mut by_class = BTreeMap::new();
        for (class, mut feats) in parts {
            feats.sort_unstable_by_key(|f| (std::cmp::Reverse(f.magnitude), f.id.cell));
            by_class.insert(class, feats);
        }
        FeatureIndex { by_class }
    }

    /// Every feature of one class, ordered magnitude descending, ties
    /// broken by identity ascending. A class with no features (including
    /// one this index never builds at all, such as `Volcano` here — see
    /// this crate's `CLAUDE.md` on the layering that keeps it out) returns
    /// an empty slice rather than panicking.
    pub fn of(&self, class: FeatureClass) -> &[Feature] {
        self.by_class.get(&class).map(Vec::as_slice).unwrap_or(&[])
    }

    /// Every feature across every populated class, class by class in
    /// ascending [`FeatureClass`] order, each class's own run already
    /// ordered by [`Self::of`].
    pub fn all(&self) -> impl Iterator<Item = &Feature> {
        self.by_class.values().flat_map(|feats| feats.iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    /// On a geosphere every cell neighbours another, so an all-true predicate
    /// must yield exactly one component holding every cell.
    #[test]
    fn an_all_true_predicate_yields_one_component_of_every_cell() {
        let geo = Geosphere::new(3);
        let comps = components(&geo, |_| true);
        assert_eq!(comps.len(), 1, "the whole sphere is connected");
        assert_eq!(comps[0].len(), geo.cell_count());
    }

    /// The empty case is a legitimate answer, not a panic.
    #[test]
    fn an_all_false_predicate_yields_no_components() {
        let geo = Geosphere::new(3);
        assert!(components(&geo, |_| false).is_empty());
    }

    /// Every member appears in exactly one component. THIS is what makes
    /// "lowest cell id" a valid identity — if a cell could appear twice, two
    /// features would claim it.
    ///
    /// The predicate (`c.0 % 13 < 2`) is chosen to fragment the sphere into
    /// many small components (56 on `Geosphere::new(3)`, confirmed below) —
    /// a predicate yielding only one or two components could never exercise
    /// cross-component contamination, which is the failure mode this test
    /// exists to catch.
    #[test]
    fn components_partition_their_members() {
        let geo = Geosphere::new(3);
        let member = |c: CellId| c.0 % 13 < 2;
        let comps = components(&geo, member);
        assert!(
            comps.len() >= 30,
            "predicate must yield many components to exercise cross-component \
             contamination, got {} (expected 56 on Geosphere::new(3))",
            comps.len()
        );
        let mut seen = BTreeSet::new();
        for comp in &comps {
            for cell in comp {
                assert!(
                    seen.insert(*cell),
                    "cell {cell:?} appeared in two components"
                );
            }
        }
        let expected: BTreeSet<CellId> = geo.cells().filter(|c| member(*c)).collect();
        assert_eq!(seen, expected, "every member is in exactly one component");
    }

    /// Components arrive in identity order, so no call site has to sort.
    ///
    /// The predicate (`c.0 % 11 < 3`) is chosen to yield many components (49
    /// on `Geosphere::new(3)`, confirmed below) whose sizes are **not**
    /// monotonically related to their identity order — sizes run
    /// `[1, 1, 3, 1, 8, 13, 1, 9, 3, 2, 4, 7, ...]`, repeatedly rising and
    /// falling. The failure mode this test guards against is "components
    /// come back in size order, or insertion order, rather than identity
    /// order"; a fixture whose sizes happen to already be monotonic (or a
    /// two-component fixture, where insertion order and size order and
    /// identity order all trivially agree half the time) cannot distinguish
    /// identity order from those other orders.
    #[test]
    fn components_are_ordered_by_their_lowest_cell_id() {
        let geo = Geosphere::new(3);
        let comps = components(&geo, |c| c.0 % 11 < 3);
        assert!(
            comps.len() >= 30,
            "predicate must yield many components, got {} (expected 49 on Geosphere::new(3))",
            comps.len()
        );
        let sizes: Vec<usize> = comps.iter().map(BTreeSet::len).collect();
        let mut ascending = sizes.clone();
        ascending.sort_unstable();
        let mut descending = sizes.clone();
        descending.sort_unstable_by(|a, b| b.cmp(a));
        assert_ne!(
            sizes, ascending,
            "fixture sizes must not already be ascending, or this test cannot \
             distinguish identity order from size order"
        );
        assert_ne!(
            sizes, descending,
            "fixture sizes must not already be descending, or this test cannot \
             distinguish identity order from size order"
        );

        let firsts: Vec<u32> = comps
            .iter()
            .map(|c| c.first().expect("nonempty").0)
            .collect();
        let mut sorted = firsts.clone();
        sorted.sort_unstable();
        assert_eq!(firsts, sorted, "components must arrive in identity order");
    }

    /// A feature's identity is the lowest cell in its extent — canonical,
    /// integer, needing no tie-break.
    ///
    /// The brief's original predicate (`c.0 % 7 < 4`) measures thin on
    /// `Geosphere::new(3)`: only 5 components, one a 360-cell blob swallowing
    /// almost the whole sphere and the rest debris of size <= 4. `c.0 % 6 < 2`
    /// measures far richer (46 components, confirmed below) with no single
    /// component dominating, so a bug that only manifests away from the
    /// giant component has somewhere to show up.
    #[test]
    fn a_features_identity_is_the_lowest_cell_of_its_extent() {
        let geo = Geosphere::new(3);
        let feats = classify(&geo, FeatureClass::Landmass, |c| c.0 % 6 < 2, 1);
        assert!(
            feats.len() >= 30,
            "predicate must yield many features, got {} (expected 46 on Geosphere::new(3))",
            feats.len()
        );
        for f in &feats {
            assert_eq!(f.id.cell, *f.extent.first().expect("nonempty extent"));
        }
    }

    /// The floor EXCLUDES small components rather than shrinking them: a rock
    /// is not a small continent, it is not a continent.
    ///
    /// The brief's original predicate (`c.0 % 7 < 4`) measures thin here too
    /// (5 components total). `c.0 % 9 < 3` measures 38 components on
    /// `Geosphere::new(3)` (confirmed below), split 28 at-or-above a floor of
    /// 3 and 10 below it — components on BOTH sides of the floor, which is
    /// what this test needs: without both sides, `floored.len() < all.len()`
    /// would pass vacuously (nothing to exclude) or trivially (everything
    /// excluded).
    #[test]
    fn the_floor_excludes_components_below_it() {
        let geo = Geosphere::new(3);
        let member = |c: CellId| c.0 % 9 < 3;
        let all = classify(&geo, FeatureClass::Landmass, member, 1);
        let floored = classify(&geo, FeatureClass::Landmass, member, 3);
        assert!(
            all.len() >= 20,
            "predicate must yield many features, got {} (expected 38 on Geosphere::new(3))",
            all.len()
        );
        assert!(
            !floored.is_empty(),
            "some component must be at or above the floor, or this test cannot \
             distinguish exclusion from wiping everything out"
        );
        assert!(
            floored.len() < all.len(),
            "a floor of 3 must exclude something"
        );
        assert!(floored.iter().all(|f| f.magnitude >= 3));
        for f in &floored {
            assert!(
                all.iter().any(|a| a.id == f.id),
                "a floor must not invent a feature"
            );
        }
    }

    /// Magnitude is extent size, so the Task 5 ordering is over an integer.
    ///
    /// `c.0 % 4 == 0` measures rich on `Geosphere::new(3)` (72 components,
    /// confirmed below), so it is kept from the brief unchanged.
    #[test]
    fn magnitude_is_the_extent_size() {
        let geo = Geosphere::new(3);
        let feats = classify(&geo, FeatureClass::Sea, |c| c.0 % 4 == 0, 1);
        assert!(
            feats.len() >= 50,
            "predicate must yield many features, got {} (expected 72 on Geosphere::new(3))",
            feats.len()
        );
        for f in &feats {
            assert_eq!(f.magnitude as usize, f.extent.len());
        }
    }

    /// A sloped test globe: a base field where every cell's elevation is its
    /// distance to the nearer pole, so downhill flow converges on whichever
    /// pole is closer (two large polar catchments), plus five one-cell
    /// islands dropped onto otherwise-land cells with every one of their
    /// neighbours forced below sea level. Each island is surrounded entirely
    /// by the ocean pocket it carves, and the coastal cells that used to
    /// flow past that pocket toward a pole now terminate at it instead —
    /// fragmenting what would otherwise be two dominant catchments into many
    /// small ones scattered around each island.
    ///
    /// Measured on `Geosphere::new(3)` (642 cells, 613 land): `rivers(..., 1)`
    /// yields **30 terminals** with extent sizes
    /// `[1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,4,4,8,8,8,8,9,14,19,19,20,29,186,248]`
    /// — 16 below a floor of 4 (island debris) and 14 at or above it
    /// (including the two polar basins, 186 and 248 cells). Both facts are
    /// asserted below, generously under the measured counts, so a future
    /// edit that collapses this back into one dominant catchment (making the
    /// partition test vacuous) or loses the small fragments (making the
    /// floor test's `big.len() <= all.len()` hold only by equality) fails
    /// loudly here instead of silently downgrading what those tests can
    /// catch.
    fn sloped_test_globe() -> (Geosphere, CellMap<ReferenceElevation>, ReferenceElevation) {
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let north = [0.0, 0.0, 1.0];
        let south = [0.0, 0.0, -1.0];
        let dist2 = |p: [f64; 3], a: [f64; 3]| {
            let dx = p[0] - a[0];
            let dy = p[1] - a[1];
            let dz = p[2] - a[2];
            dx * dx + dy * dy + dz * dz
        };
        let island_cells: Vec<CellId> = [40, 140, 240, 340, 440].into_iter().map(CellId).collect();
        let mut island_neighbors: BTreeSet<CellId> = BTreeSet::new();
        for &island in &island_cells {
            for &nb in geo.neighbors(island) {
                island_neighbors.insert(nb);
            }
        }
        let elevation = CellMap::from_fn(&geo, |c| {
            if island_cells.contains(&c) {
                return ReferenceElevation::new(50.0).unwrap();
            }
            if island_neighbors.contains(&c) {
                return ReferenceElevation::new(-50.0).unwrap();
            }
            let p = geo.position(c);
            let d = dist2(p, north).min(dist2(p, south));
            ReferenceElevation::new(100.0 * d).unwrap()
        });

        let terminals = rivers(&geo, &elevation, sea, 1);
        assert!(
            terminals.len() >= 2,
            "fixture is vacuous: it must produce more than one catchment, got {}",
            terminals.len()
        );
        let below_floor = terminals.iter().filter(|r| r.extent.len() < 4).count();
        let at_or_above_floor = terminals.len() - below_floor;
        assert!(
            below_floor > 0 && at_or_above_floor > 0,
            "fixture must have catchments on both sides of a floor of 4 (below={below_floor}, \
             at_or_above={at_or_above_floor}, expected 16/14 on Geosphere::new(3)), or \
             the_floor_cuts_on_catchment_size cannot distinguish exclusion from equality"
        );

        (geo, elevation, sea)
    }

    /// Every land cell belongs to exactly one catchment — the river analogue
    /// of `components_partition_their_members`, and what makes the terminal a
    /// valid identity.
    #[test]
    fn every_land_cell_belongs_to_exactly_one_catchment() {
        let (geo, elev, sea) = sloped_test_globe();
        let mut seen = BTreeSet::new();
        for r in rivers(&geo, &elev, sea, 1) {
            for cell in &r.extent {
                assert!(seen.insert(*cell), "cell {cell:?} is in two catchments");
            }
        }
        let land: BTreeSet<CellId> = geo.cells().filter(|c| *elev.get(*c) >= sea).collect();
        assert_eq!(seen, land, "every land cell drains somewhere");
    }

    /// A river's identity is a fixed point of the downhill map.
    #[test]
    fn a_rivers_identity_is_a_terminal_of_the_flow_forest() {
        let (geo, elev, sea) = sloped_test_globe();
        let downhill = crate::drainage::downhill_targets(&geo, &elev, sea);
        for r in rivers(&geo, &elev, sea, 1) {
            assert!(
                downhill[r.id.cell.0 as usize].is_none(),
                "terminal {:?} still points downhill",
                r.id.cell
            );
        }
    }

    /// The floor cuts on CATCHMENT, not mouth drainage (spec 3.1).
    #[test]
    fn the_floor_cuts_on_catchment_size() {
        let (geo, elev, sea) = sloped_test_globe();
        let all = rivers(&geo, &elev, sea, 1);
        let big = rivers(&geo, &elev, sea, 4);
        assert!(big.iter().all(|r| r.magnitude >= 4));
        assert!(big.iter().all(|r| r.extent.len() == r.magnitude as usize));
        assert!(big.len() <= all.len());
        assert!(
            big.len() < all.len(),
            "a floor of 4 must exclude something, or this test cannot distinguish \
             exclusion from equality"
        );
    }

    /// Within a class, features order by magnitude descending, ties broken by
    /// identity ascending. Total, deterministic, integer-only.
    ///
    /// The brief's own predicate (`c.0 % 7 < 4`) measures thin here (5
    /// components, one a 360-cell blob, already flagged unusable for
    /// exactly this reason earlier in this module — see
    /// `a_features_identity_is_the_lowest_cell_of_its_extent`'s comment).
    /// Reused instead: `c.0 % 11 < 3`, already measured non-degenerate above
    /// (`components_are_ordered_by_their_lowest_cell_id`, 49 components,
    /// sizes `[1, 1, 3, 1, 8, 13, 1, 9, 3, 2, 4, 7, ...]`, repeatedly rising
    /// and falling). With floor 1 every component becomes a feature, so the
    /// same 49-count, non-monotonic guarantee carries over unchanged —
    /// confirmed below rather than assumed twice.
    #[test]
    fn features_order_by_magnitude_then_identity() {
        let geo = Geosphere::new(3);
        let feats = classify(&geo, FeatureClass::Landmass, |c| c.0 % 11 < 3, 1);
        assert!(
            feats.len() >= 30,
            "predicate must yield many features, got {} (expected 49 on Geosphere::new(3))",
            feats.len()
        );
        let sizes: Vec<u32> = feats.iter().map(|f| f.magnitude).collect();
        let mut ascending = sizes.clone();
        ascending.sort_unstable();
        let mut descending = sizes.clone();
        descending.sort_unstable_by(|a, b| b.cmp(a));
        assert_ne!(
            sizes, ascending,
            "fixture sizes must not already be ascending in identity order, or this \
             test cannot distinguish magnitude order from identity order"
        );
        assert_ne!(
            sizes, descending,
            "fixture sizes must not already be descending in identity order either, or \
             the input order alone would already satisfy the assertion below"
        );
        let index = FeatureIndex::from_parts(vec![(FeatureClass::Landmass, feats)]);
        for pair in index.of(FeatureClass::Landmass).windows(2) {
            let (a, b) = (&pair[0], &pair[1]);
            assert!(
                a.magnitude > b.magnitude || (a.magnitude == b.magnitude && a.id.cell < b.id.cell),
                "ordering is not total: {:?} before {:?}",
                a.id,
                b.id
            );
        }
    }

    /// The ordering does not depend on input order.
    #[test]
    fn ordering_is_independent_of_input_order() {
        let geo = Geosphere::new(3);
        let feats = classify(&geo, FeatureClass::Landmass, |c| c.0 % 11 < 3, 1);
        assert!(
            feats.len() >= 30,
            "predicate must yield many features, got {} (expected 49 on Geosphere::new(3))",
            feats.len()
        );
        let mut reversed = feats.clone();
        reversed.reverse();
        let a = FeatureIndex::from_parts(vec![(FeatureClass::Landmass, feats)]);
        let b = FeatureIndex::from_parts(vec![(FeatureClass::Landmass, reversed)]);
        let ids = |i: &FeatureIndex| -> Vec<FeatureId> {
            i.of(FeatureClass::Landmass).iter().map(|f| f.id).collect()
        };
        assert_eq!(ids(&a), ids(&b));
    }

    /// A class with no features returns an empty slice, not a panic — the
    /// shape `Volcano` will be in at the worldgen layer (Task 6) before this
    /// crate ever populates it.
    #[test]
    fn of_an_absent_class_is_an_empty_slice() {
        let index = FeatureIndex::from_parts(vec![(FeatureClass::Landmass, Vec::new())]);
        assert!(index.of(FeatureClass::Volcano).is_empty());
        assert!(index.of(FeatureClass::Landmass).is_empty());
    }

    /// `all()` yields every feature across every populated class.
    #[test]
    fn all_yields_every_feature_across_classes() {
        let geo = Geosphere::new(3);
        let land = classify(&geo, FeatureClass::Landmass, |c| c.0 % 6 < 2, 1);
        let sea = classify(&geo, FeatureClass::Sea, |c| c.0 % 4 == 0, 1);
        let land_count = land.len();
        let sea_count = sea.len();
        let index = FeatureIndex::from_parts(vec![
            (FeatureClass::Landmass, land),
            (FeatureClass::Sea, sea),
        ]);
        assert_eq!(index.all().count(), land_count + sea_count);
    }
}

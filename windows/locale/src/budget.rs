//! The rarity budget (P7): a bounded genesis pass placing rung-30 exotic
//! regimes sparsely and spaced, with a founder floor so a strongly-warranted
//! exotic is never missed by an unlucky draw. All accept/reject comparisons
//! are on quantized values (decision 0041). Placement is *repulsive*; the
//! future negative wing will be *excitatory* — do not bake repulsion deeper.

use crate::regime::{EnergySource, Kingdom, Negations, Substrate};
use crate::streams::LOCALE_PLACE;
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{CellId, Seed, quantize};
use hornvale_terrain::GeneratedTerrain;
use serde::Serialize;
use std::collections::BTreeMap;

/// Target strangeness mass: at most this fraction of land cells host an exotic.
const BUDGET_FRACTION: f64 = 0.01;
/// Minimum spacing between placed sites, in integer cell-graph hops.
const REPULSION_HOPS: u32 = 3;

/// A placed exotic site — a derived, findable record (never stored in the save).
/// type-audit: bare-ok(index: cell), bare-ok(flag: endemic)
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StrangeSite {
    /// Canonical-grid cell index.
    pub cell: u32,
    /// The energy negation, if any.
    pub energy: EnergySource,
    /// The kingdom negation, if any.
    pub kingdom: Kingdom,
    /// Whether the site's biota is endemic.
    pub endemic: bool,
}

/// The placed-regime map for one world.
pub(crate) struct StrangenessBudget {
    sites: BTreeMap<u32, StrangeSite>,
}

/// One candidate exotic regime and its per-cell eligibility score.
struct Candidate {
    energy: EnergySource,
    kingdom: Kingdom,
    score: fn(&GeneratedTerrain, CellId) -> f64,
}

/// The fixed candidate table (natural-tier exotics only).
fn candidates() -> [Candidate; 3] {
    [
        // Chemo/geothermal vents: warranted by tectonic unrest.
        Candidate {
            energy: EnergySource::Geothermal,
            kingdom: Kingdom::PlantAnimal,
            score: |t, c| quantize(*t.globe().unrest.get(c)),
        },
        Candidate {
            energy: EnergySource::Chemosynthetic,
            kingdom: Kingdom::Microbial,
            score: |t, c| quantize(*t.globe().unrest.get(c) * 0.8),
        },
        // Fungal kingdoms: warranted by damp, low-relief, non-volcanic ground.
        Candidate {
            energy: EnergySource::Sunlit,
            kingdom: Kingdom::Fungal,
            score: |t, c| {
                let g = t.globe();
                let relief = quantize(*g.elevation.get(c) - g.sea_level);
                if relief > 0.0 && quantize(*g.unrest.get(c)) < 0.4 {
                    quantize(1.0 - *g.unrest.get(c))
                } else {
                    0.0
                }
            },
        },
    ]
}

impl StrangenessBudget {
    /// Build the placement for a world (bounded O(cells)).
    pub(crate) fn build(
        seed: Seed,
        climate: &GeneratedClimate,
        terrain: &GeneratedTerrain,
    ) -> StrangenessBudget {
        let geo = climate.geosphere();
        let globe = terrain.globe();
        // Land cells only, in CellId order (deterministic).
        let land: Vec<CellId> = geo
            .cells()
            .filter(|&c| quantize(*globe.elevation.get(c)) > quantize(globe.sea_level))
            .collect();
        let cands = candidates();
        let mut sites: BTreeMap<u32, StrangeSite> = BTreeMap::new();
        let mut accepted: Vec<CellId> = Vec::new();

        // (2) Founder floor: reserve each candidate's single most-eligible cell.
        for cand in &cands {
            if let Some(best) = land
                .iter()
                .copied()
                .filter(|&c| (cand.score)(terrain, c) > 0.0 && !sites.contains_key(&c.0))
                .max_by(|&a, &b| {
                    (cand.score)(terrain, a)
                        .total_cmp(&(cand.score)(terrain, b))
                        .then(b.0.cmp(&a.0))
                })
            {
                insert_site(
                    &mut sites,
                    &mut accepted,
                    best,
                    cand,
                    *globe.endorheic.get(best),
                );
            }
        }

        // (3) Field-weighted blue-noise over a seeded permutation of land cells.
        // Exempt from the quantize rule: an exact IEEE-754 multiply of an
        // integer by a compile-time constant (no libm involved), so it is
        // platform-identical without quantization.
        let budget = ((land.len() as f64) * BUDGET_FRACTION) as usize;
        let mut stream = seed.derive(LOCALE_PLACE).stream();
        let order = permute(&land, &mut stream);
        for c in order {
            if sites.len() >= budget {
                break;
            }
            if sites.contains_key(&c.0) || within_repulsion(&accepted, c, geo) {
                continue;
            }
            let weights: Vec<f64> = cands.iter().map(|k| (k.score)(terrain, c)).collect();
            if let Some(i) = stream.weighted_index(&weights) {
                insert_site(
                    &mut sites,
                    &mut accepted,
                    c,
                    &cands[i],
                    *globe.endorheic.get(c),
                );
            }
        }
        StrangenessBudget { sites }
    }

    /// The negation vector a placed cell carries, if any.
    pub(crate) fn regime_at(&self, cell: CellId) -> Option<Negations> {
        self.sites.get(&cell.0).map(|s| Negations {
            substrate: Substrate::Ordinary, // substrate comes from the derived proxy
            energy: s.energy,
            kingdom: s.kingdom,
            endemic: s.endemic,
        })
    }

    /// The placed sites, for findability (derived, not stored).
    pub fn sites(&self) -> Vec<StrangeSite> {
        self.sites.values().cloned().collect()
    }
}

fn insert_site(
    sites: &mut BTreeMap<u32, StrangeSite>,
    accepted: &mut Vec<CellId>,
    cell: CellId,
    cand: &Candidate,
    endemic: bool,
) {
    sites.insert(
        cell.0,
        StrangeSite {
            cell: cell.0,
            energy: cand.energy,
            kingdom: cand.kingdom,
            endemic,
        },
    );
    accepted.push(cell);
}

/// A Fisher–Yates permutation off the given stream (deterministic).
fn permute(cells: &[CellId], stream: &mut hornvale_kernel::Stream) -> Vec<CellId> {
    let mut v = cells.to_vec();
    for i in (1..v.len()).rev() {
        let j = (stream.next_u64() % (i as u64 + 1)) as usize;
        v.swap(i, j);
    }
    v
}

/// True if `c` is within REPULSION_HOPS graph-hops of any accepted cell.
/// Uses the geosphere's integer neighbour graph (no transcendentals).
/// `hops_between(a, b, max)` returns `Some(hops)` iff `b` is within `max`
/// hops of `a` (a bounded BFS), else `None`.
fn within_repulsion(accepted: &[CellId], c: CellId, geo: &hornvale_kernel::Geosphere) -> bool {
    accepted
        .iter()
        .any(|&a| geo.hops_between(a, c, REPULSION_HOPS).is_some())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Seed, World};
    use hornvale_worldgen::{climate_of, terrain_of};

    fn budget_for(
        seed: u64,
    ) -> (
        StrangenessBudget,
        hornvale_climate::GeneratedClimate,
        GeneratedTerrain,
    ) {
        let w = World::new(Seed(seed));
        let climate = climate_of(&w).unwrap();
        let terrain = terrain_of(&w).unwrap();
        let b = StrangenessBudget::build(Seed(seed), &climate, &terrain);
        (b, climate, terrain)
    }

    #[test]
    fn budget_is_deterministic() {
        let a = budget_for(42).0.sites();
        let b = budget_for(42).0.sites();
        assert_eq!(a, b);
    }

    #[test]
    fn placed_sites_are_a_small_minority() {
        let (b, climate, _terrain) = budget_for(42);
        let land = climate.geosphere().cells().count(); // upper bound
        assert!(b.sites().len() * 20 < land, "strange sites must stay rare");
    }

    #[test]
    #[ignore = "heavy: live worldgen census over many seeds"]
    fn census_budget_and_spacing_hold_across_seeds() {
        // Founder-floor cells intentionally bypass the repulsion radius (each
        // candidate reserves its single best-unclaimed cell before the
        // blue-noise pass), so a global pairwise-spacing invariant is false
        // by design and is not asserted here.
        for seed in 0..25 {
            let (b, climate, terrain) = budget_for(seed);
            let globe = terrain.globe();
            let land_count = climate
                .geosphere()
                .cells()
                .filter(|&c| quantize(*globe.elevation.get(c)) > quantize(globe.sea_level))
                .count();
            let sites = b.sites();

            // Budget minority: placed sites are a small minority of land.
            assert!(
                sites.len() * 20 < land_count,
                "seed {seed}: strange sites must stay a rare minority of land"
            );

            // Determinism: rebuilding for the same seed is byte-identical.
            let rebuilt = StrangenessBudget::build(Seed(seed), &climate, &terrain);
            assert_eq!(
                serde_json::to_string(&b.sites()).unwrap(),
                serde_json::to_string(&rebuilt.sites()).unwrap(),
                "seed {seed}: rebuilding the budget must be byte-identical"
            );

            // Distinct cells: no silent overwrite in the sites map.
            let cells: Vec<u32> = sites.iter().map(|s| s.cell).collect();
            let distinct: std::collections::BTreeSet<u32> = cells.iter().copied().collect();
            assert_eq!(
                cells.len(),
                distinct.len(),
                "seed {seed}: placed sites must occupy distinct cells"
            );
        }
    }
}

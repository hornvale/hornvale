//! The hazard field (The Repose, spec §3.1): how often a cell's ground acts.
//!
//! **It does not accumulate.** No stress builds toward a threshold and no
//! state carries between calls: `hazard_at` is a pure read over terrain's
//! committed fields, and asking twice can only ever give the same answer.
//! This is constitutional rather than a simplification — `BIO-36` fixes
//! tier-0 as a *drawn stationary regime*, "invented on demand and narrated
//! backwards, never forward-simulated", and the Lorenz guard-rail forbids
//! the forward-integrator alternative outright. The timeline of a Hornvale
//! catastrophe has no left half, and that is the design.
//!
//! **Every constant here is authored, not fitted** ("models author, dice
//! roll"). Each carries the reasoning for its value in its own doc comment.
//! They are the analytically-known regime `TOOL-analytic-limiting-case`
//! asks for: we put the law in by hand, so recovering it downstream proves
//! the implementation and never the world (spec §3.3).
//!
//! **Why no second `BoundaryKind` match lives here.** Boundary kind is a
//! stated input of the field, and it reaches this module through the two
//! readings that already carry it, not through a match of its own:
//! `unrest_at` is *`intensity(kind)` × closing speed × youth × proximity*
//! (`terrain::elevation::generate_unrest`), and edifice presence is
//! island-arc-only by construction. A per-kind factor applied on top of
//! unrest would count the kind twice — an unauthored fudge with no
//! physical claim behind it. What kind *should* select is the shape of the
//! magnitude law, and that is the event draw's business (spec §3.3), not
//! the mean interval's.

use hornvale_kernel::{CellId, Years, math};
use hornvale_terrain::GeneratedTerrain;

/// How often a cell's ground acts, as mean intervals between events.
///
/// A STEADY rate, never an accumulating stress (spec §3.1): nothing here
/// carries state between events, and the timeline of a Hornvale catastrophe
/// has no left half. That is the design, forced by `BIO-36`'s tier-0 rule
/// and the Lorenz guard-rail, not a shortcut.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Recurrence {
    /// Mean interval between seismic events at or above the catalogue's
    /// lower magnitude cutoff.
    pub seismic: Years,
    /// Mean interval between eruptions, or `None` where there is no edifice
    /// to erupt from.
    pub volcanic: Option<Years>,
}

/// Mean interval between seismic events in the quietest ground there is —
/// an old plate interior, far from any boundary (`unrest` = 0).
///
/// AUTHORED, from the terrestrial analogue: stable cratonic interiors do
/// produce damaging earthquakes (New Madrid, Charleston), but a *given*
/// patch of one waits tens of millennia between them. 20,000 years puts the
/// quiet end firmly outside any culture's living memory, which is the
/// property Task 7's knownness half-life will lean on: the field's quiet
/// end must be forgettable, or there is nothing to forget.
const SEISMIC_QUIET_YEARS: f64 = 20_000.0;

/// Mean interval between seismic events on the most active ground there is
/// (`unrest` = 1: a young plate closing fast, on the boundary itself).
///
/// AUTHORED, from the terrestrial analogue: the most active plate-boundary
/// segments deliver a cutoff-grade shock on a human generational scale —
/// once or twice a lifetime, felt by everyone who lives there. 30 years is
/// the short end of that, and deliberately *not* shorter: an interval below
/// a human generation would make the hazard ordinary weather rather than
/// the thing a people remembers and mis-remembers.
const SEISMIC_ACTIVE_YEARS: f64 = 30.0;

/// Mean interval between eruptions at a quiet edifice (`unrest` = 0).
///
/// AUTHORED: an arc cone on a slow margin is not extinct, it is *dormant* —
/// five millennia between eruptions, long enough that the mountain is
/// remembered as a mountain and not as a volcano. This is the case spec §1
/// is named for: Vesuvius in AD 79 "was not a known hazard; it was a
/// fertile hill with towns on its flanks".
const VOLCANIC_QUIET_YEARS: f64 = 5_000.0;

/// Mean interval between eruptions at a vigorously active edifice
/// (`unrest` = 1).
///
/// AUTHORED: the busiest arc volcanoes erupt on a scale of years, but the
/// eruption worth narrating — the one that ends a settlement rather than
/// dusting it — is the rarer, larger event. Two centuries is the interval
/// at which a people can plausibly hold the memory of the last one and
/// still be living on the flank.
const VOLCANIC_ACTIVE_YEARS: f64 = 200.0;

/// Interpolate a recurrence interval between its quiet and active ends,
/// geometrically in `unrest`.
///
/// Geometric, not linear, because recurrence spans orders of magnitude and
/// the physically meaningful step is a *factor*: halving the interval is
/// the same event whether it happens at 20,000 years or at 200. Linear
/// interpolation would spend almost the whole of `unrest`'s range inside
/// the quiet decade and compress every active regime into its last few
/// percent. Strictly decreasing in `unrest` for any `active < quiet`.
fn geometric_years(quiet_years: f64, active_years: f64, unrest: f64) -> Years {
    let years = quiet_years * math::powf(active_years / quiet_years, unrest);
    Years::new(years).expect("authored recurrence bounds are finite and positive")
}

/// Whether a cell carries a volcanic edifice — the gated island-arc cone
/// the elevation raised there.
///
/// A pure delegation to terrain's own derived read, kept here so the hazard
/// surface reads as one thing to Task 5. The read lives in `domains/terrain`
/// rather than being re-derived at this layer deliberately: an edifice is
/// terrain's concept, and a copy of the gate here could drift from the
/// elevation it is supposed to describe.
/// type-audit: bare-ok(flag: return)
pub fn has_edifice(terrain: &GeneratedTerrain, cell: CellId) -> bool {
    terrain.has_edifice(cell)
}

/// The hazard field at a cell: mean intervals between seismic events, and
/// between eruptions where there is an edifice to erupt from.
///
/// Pure and stateless (spec §3.1). Composes exactly three shipped readings —
/// `unrest_at`, and the boundary kind and edifice presence that
/// [`has_edifice`] carries — and authors the mapping from them to intervals.
/// `unrest` is clamped defensively: terrain documents and clamps it to
/// `[0,1]`, and this module's monotonicity is stated over that range.
pub fn hazard_at(terrain: &GeneratedTerrain, cell: CellId) -> Recurrence {
    let unrest = terrain.unrest_at(cell).clamp(0.0, 1.0);
    let seismic = geometric_years(SEISMIC_QUIET_YEARS, SEISMIC_ACTIVE_YEARS, unrest);
    let volcanic = has_edifice(terrain, cell)
        .then(|| geometric_years(VOLCANIC_QUIET_YEARS, VOLCANIC_ACTIVE_YEARS, unrest));
    Recurrence { seismic, volcanic }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Geosphere, Seed};
    use hornvale_terrain::{BoundaryKind, GeneratedTerrain, TerrainPins};

    /// A globe small enough to build in a unit test and large enough to
    /// carry every boundary kind.
    fn globe() -> (Geosphere, GeneratedTerrain) {
        let geo = Geosphere::new(5);
        let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        (geo, terrain)
    }

    /// A transform boundary is documented as "unrest, little relief" —
    /// seismic, not volcanic. Direction: pins the KIND separation, not the
    /// magnitudes.
    #[test]
    fn a_transform_boundary_is_seismic_and_never_volcanic() {
        let (geo, terrain) = globe();
        let mut transforms = 0_u32;
        for cell in geo.cells() {
            if terrain.boundary_at(cell).map(|b| b.kind) != Some(BoundaryKind::Transform) {
                continue;
            }
            transforms += 1;
            let hazard = hazard_at(&terrain, cell);
            assert_eq!(
                hazard.volcanic, None,
                "{cell:?} is a transform boundary with a volcanic recurrence"
            );
            assert!(
                hazard.seismic.get() < SEISMIC_QUIET_YEARS,
                "{cell:?} is a transform boundary yet no more seismic than a dead interior"
            );
        }
        assert!(transforms > 0, "no transform boundary on the test globe");
    }

    /// Every cell without an edifice is amagmatic, and every cell with one
    /// erupts. The two halves of the volcanic option, over a whole globe.
    #[test]
    fn volcanic_recurrence_exists_exactly_where_an_edifice_does() {
        let (geo, terrain) = globe();
        let mut edifices = 0_u32;
        for cell in geo.cells() {
            let hazard = hazard_at(&terrain, cell);
            if has_edifice(&terrain, cell) {
                edifices += 1;
                assert!(
                    hazard.volcanic.is_some(),
                    "{cell:?} carries an edifice and no eruption interval"
                );
            } else {
                assert_eq!(
                    hazard.volcanic, None,
                    "{cell:?} erupts from an edifice it does not have"
                );
            }
        }
        assert!(edifices > 0, "no edifice on the test globe");
    }

    /// Recurrence is a pure function of the cell's fields: same inputs,
    /// same answer, every call, with no memory between calls.
    #[test]
    fn hazard_is_pure_and_carries_no_state() {
        let (geo, terrain) = globe();
        let cell = geo
            .cells()
            .max_by(|a, b| terrain.unrest_at(*a).total_cmp(&terrain.unrest_at(*b)))
            .expect("a non-empty globe");
        assert!(terrain.unrest_at(cell) > 0.0, "the scan found dead ground");
        let first = hazard_at(&terrain, cell);
        for _ in 0..100 {
            assert_eq!(hazard_at(&terrain, cell), first, "hazard accumulated state");
        }
    }

    /// Higher unrest means a SHORTER interval, monotonically. Direction:
    /// catches an inverted sign, which is the defect that would make the
    /// whole campaign measure backwards.
    #[test]
    fn higher_unrest_shortens_the_seismic_interval() {
        let (geo, terrain) = globe();
        let mut rows: Vec<(f64, f64)> = geo
            .cells()
            .map(|c| (terrain.unrest_at(c), hazard_at(&terrain, c).seismic.get()))
            .collect();
        rows.sort_by(|a, b| a.0.total_cmp(&b.0));
        let distinct = rows.windows(2).filter(|w| w[0].0 != w[1].0).count();
        assert!(
            distinct > 100,
            "only {distinct} distinct unrest values — too flat to test monotonicity"
        );
        for pair in rows.windows(2) {
            let (lo, hi) = (pair[0], pair[1]);
            if lo.0 < hi.0 {
                assert!(
                    hi.1 < lo.1,
                    "unrest {} -> {} did not shorten the interval: {} -> {}",
                    lo.0,
                    hi.0,
                    lo.1,
                    hi.1
                );
            } else {
                assert_eq!(hi.1, lo.1, "equal unrest gave unequal intervals");
            }
        }
        // The span is real, not a rounding artefact: the quietest and the
        // most active ground on one globe differ by orders of magnitude.
        let (quietest, busiest) = (rows[0].1, rows[rows.len() - 1].1);
        assert!(
            quietest / busiest > 10.0,
            "the field is nearly flat: {quietest} vs {busiest}"
        );
    }

    /// The authored ends are the ends: no cell can be quieter than the dead
    /// interior or busier than the most active belt.
    #[test]
    fn every_interval_lies_inside_its_authored_bracket() {
        let (geo, terrain) = globe();
        for cell in geo.cells() {
            let hazard = hazard_at(&terrain, cell);
            assert!(
                (SEISMIC_ACTIVE_YEARS..=SEISMIC_QUIET_YEARS).contains(&hazard.seismic.get()),
                "{cell:?} seismic {} escaped the authored bracket",
                hazard.seismic.get()
            );
            if let Some(volcanic) = hazard.volcanic {
                assert!(
                    (VOLCANIC_ACTIVE_YEARS..=VOLCANIC_QUIET_YEARS).contains(&volcanic.get()),
                    "{cell:?} volcanic {} escaped the authored bracket",
                    volcanic.get()
                );
            }
        }
    }
}

//! The water table — where a rock column stops being walkable (spec §4.2).
//!
//! **Why this module exists.** Before The Underworld,
//! `grep -rn "water_table\|phreatic\|vadose" --include=*.rs domains/ windows/`
//! returned nothing (spec §3.7): every column in every world was implicitly
//! dry at every depth, so the habitable window of a wet karst cell and of a dry
//! craton cell were the same window. This module splits a column at a derived
//! depth — **vadose** (air-filled, walkable) above it, **phreatic** (flooded)
//! below — and the depth varies by cell, which is the whole point.
//!
//! It exists to produce two things. A *habitable window* that differs by place,
//! and **sumps**: a chamber below the table, which the passage graph later
//! represents as a missing edge rather than as a new kind of place.
//!
//! Pure arithmetic over three fields terrain already owns — overhead drainage,
//! `MaterialBuffer::porosity`, and height above sea level. No draws, no
//! committed facts, so it cannot perturb stream consumption order (a
//! save-format contract) and adds nothing to the ledger. It takes no
//! [`crate::DelveRung`]: the table is a property of the column, and the ladder
//! reads it rather than the other way round.
//!
//! ## The derivation
//!
//! One dimensionless competition, scaled once into metres:
//!
//! ```text
//! depth = TABLE_SCALE_M * max(0, transmissivity(porosity) * relief(height) - wetness(drainage))
//! ```
//!
//! The physical statement is the classical topography-driven view of regional
//! groundwater (Tóth 1963): the table is a *subdued replica* of the ground
//! surface, held down by the rock's ability to shed water toward the regional
//! base level and pushed up by the water arriving from upslope. Two terms,
//! opposed, in the same dimensionless currency:
//!
//! - **Drawdown**, `transmissivity(porosity) * relief(height_asl_m)`. A column
//!   can only drain if it stands above the base level (`relief`) *and* the rock
//!   can move water (`transmissivity`). Either being zero means nowhere to
//!   drain to, or no way to get there.
//! - **Recharge**, `wetness(drainage)`. Flow accumulation is the count of land
//!   cells upstream, so it is the model's own measure of how much water passes
//!   through this column.
//!
//! ### The one calibration in the whole derivation
//!
//! Both terms run over `[0, 1)`-ish ranges and neither carries a scale of its
//! own, so the model's entire calibration is the statement that they are
//! **comparable at par**: recharge's maximum is worth exactly as much as
//! drawdown's unit. That is not arbitrary, and it is what the constants'
//! half-points are chosen to express. Take the reference column — mean land
//! elevation ([`RELIEF_HALF_M`], relief = 0.5), median rock (transmissivity =
//! 1), carrying exactly [`crate::RIVER_MIN_DRAINAGE`] (wetness = 0.5). The two
//! terms cancel and the table sits at the ground.
//!
//! **Which is the definition of a river.** `crate::water::classify` already
//! declares that accumulation to be where a perennial watercourse appears, and
//! a perennial watercourse *is* the water table intersecting the surface. So
//! the par is pinned to a claim the model had already committed to, in a
//! constant this module reads rather than restates, rather than to a knob
//! turned until the output looked right.
//!
//! ## Why the floor at zero is physics and not a rail
//!
//! Where recharge beats drawdown the expression goes negative, which says the
//! table would stand *above* the ground. It cannot: the water comes out, as a
//! spring, a marsh, a lake, a river. So the surface is a real physical floor
//! and the value `0.0` is a real physical state — "this column is drowned from
//! the top, and every depth in it is phreatic".
//!
//! **This puts a genuine ATOM at exactly 0.0, and it is declared rather than
//! discovered later.** This campaign has twice been bitten by clamps quietly
//! creating spikes ([`crate::CAVE_REACH_CEILING_M`] and `LAVATUBE_CEILING_M`
//! put 23.1% of seed 42's caves on one reach value, and the delve ladder's
//! `Deeps` boundary had to move off the mode it landed on). The difference
//! here is that this atom is the *quantity being measured*: its mass is
//! exactly the wholly-phreatic fraction spec §5's H3 preregisters bounds on,
//! not a side effect of a rail. `windows/worldgen/tests/underworld_water_table_probe.rs`
//! measures it, along with the rest of the distribution's shape, and records
//! the reading in its own module doc.
//!
//! **Measured, seeds 42 / 7 / 1234.** H3 holds: 29.9% / 45.5% / 42.5% of
//! cave-bearing columns are wholly phreatic, against bounds of 5% and 95%.
//! Above the floor the distribution is genuinely spread rather than lumpy —
//! 491–586 distinct values at 0.1 m resolution over 874–1681 columns, and the
//! fattest *non-zero* value holds 3.7% of a seed at worst, against the 23.1%
//! one reach value holds in [`crate::cave_depth_reach_m`]'s output.
//!
//! **The finding that is not H3, and matters more downstream:** the walkable
//! share of a cave column has a median of 0.2–4.9%, and the table never
//! exceeds ~500 m while `DelveRung::Underdeep` begins near 1 km. Every
//! `Underdeep` and `Sunless` chamber in all three worlds is therefore below
//! the table. Spec §4.2 makes that a *missing edge* rather than an
//! uninhabitable place, so it is a statement about reaching the deep rather
//! than about living there — but it is a whole-population statement and the
//! campaign should decide about it deliberately. See [`TABLE_SCALE_M`].
//!
//! ## What each constant does and does not control
//!
//! [`TABLE_SCALE_M`] is the only constant carrying metres, so it cannot move
//! **H3's statistic**: the sign of `drawdown - recharge` is independent of it,
//! so the set of drowned columns is the same at any value. Only the three
//! dimensionless shapes can move that fraction, and each of their scales is
//! pinned to a datum outside this campaign: an existing published constant
//! ([`crate::RIVER_MIN_DRAINAGE`]), Earth's mean land elevation, and the
//! log-spacing of hydraulic conductivity across rock types.
//!
//! **That is not the same as "it changes nothing else."** It sets how deep the
//! table gets, and therefore which delve rungs can ever be dry — see its own
//! doc, which carries the measured consequence. And [`RELIEF_SOFT_M`], which
//! looks like a pure smoothing detail, moves the drowned share by 10–20 points
//! across a 20× sweep, because a sixth of all cave columns sit at *exactly*
//! sea level; its doc carries that sweep. Both of those were claims this
//! module made loosely in an earlier draft and now states from measurement.
//!
//! ## Monotonicity, stated exactly
//!
//! Non-increasing in `drainage`, non-decreasing in `porosity` and in
//! `height_asl_m`, **strictly** so wherever the result is above zero, and
//! constant at zero where the column is drowned. That flat region is the floor
//! doing its job, not a loss of resolution: a drowned column has no water-table
//! depth to resolve.

use crate::water::RIVER_MIN_DRAINAGE;
use hornvale_kernel::math;

/// The one constant in metres: the depth to water beneath a column that stands
/// at [`RELIEF_HALF_M`] above sea level, in rock of median transmissivity, with
/// no upslope inflow at all, is half of this.
///
/// **The magnitude is authored, and it is the only authored magnitude here.**
/// Its anchor is the global water-table survey of Fan, Li & Miguez-Macho
/// (2013): humid lowlands sit within a few metres of the surface, while arid
/// uplands and continental interiors run from tens of metres to a few hundred.
/// 200 m puts the dry mid-elevation reference column at 100 m and leaves the
/// upper tail — high, porous, unwatered rock — in the low hundreds, which is
/// the band that survey reports and the band the delve ladder's habitable rungs
/// occupy.
///
/// **It cannot move H3**, and there is a test for that
/// (`the_metre_scale_never_decides_whether_a_column_is_drowned`). Every term it
/// multiplies is dimensionless and the zero-crossing is a comparison between
/// two of them, so this constant scales the distribution without changing which
/// columns are drowned.
///
/// **What it DOES decide, so that "magnitude only" is not read as
/// "harmless":** the deepest the table ever gets, and therefore which rungs of
/// the delve ladder can ever be dry. At this value the probe measures a maximum
/// table depth of **422–501 m** across the three seeds, while
/// `DelveRung::Underdeep` begins near 1 km — so **no `Underdeep` or `Sunless`
/// chamber is vadose in any measured world** (0.0% on every seed). That is a
/// real consequence of an Earth-calibrated water table meeting a
/// kilometre-scale habitation ladder, not an accident of arithmetic, and it is
/// recorded here rather than left for a downstream campaign to discover.
const TABLE_SCALE_M: f64 = 200.0;

/// Ratio between the transmissivity of the most and least porous rock the
/// `porosity` axis describes — the span of `100 ^ (porosity - 0.5)`, so the
/// term runs 0.1× at `porosity = 0` to 10× at `porosity = 1`, crossing 1 at the
/// midpoint.
///
/// **Logarithmic, for the same reason [`crate::cave_depth`]'s strength scale
/// is:** hydraulic conductivity is a log-distributed rock property, not a
/// linear one. Freeze & Cherry's standard table spans roughly 10⁻¹³ m/s
/// (unfractured igneous rock) to 10⁰ m/s (clean gravel) — thirteen orders —
/// with each lithological class occupying about the same *ratio* band as the
/// next. A linear map from `porosity` would put nearly the whole range on the
/// permeable side and lose the aquitards entirely.
///
/// **The span is authored at two orders, not thirteen**, because `porosity` is
/// a cell-scale matrix property rather than a conductivity: it does not carry
/// fracture permeability, which is most of that thirteen-order spread. Two
/// orders across the full `[0, 1]` axis works out to about **35×** across the
/// porosity the model actually produces on cave-bearing land — measured at
/// `min 0.051 … max 0.819` over the three preregistered seeds by
/// `underworld_water_table_probe`, not taken from
/// `crate::lithology::hydrogeology`'s older class figures, which describe all
/// land rather than the caved subset and stop at 0.65. That is a real
/// aquitard-to-aquifer contrast without pretending to a precision the input
/// does not have.
const TRANSMISSIVITY_SPAN: f64 = 100.0;

/// Height above sea level (m) at which the relief term reaches half its
/// maximum. **Earth's mean land elevation, ~840 m, rounded to 800** — a datum,
/// not a fit. It is the elevation at which "high ground" stops being a useful
/// description, and using it means the term is near its midpoint over the bulk
/// of any world's land rather than saturated at one end.
const RELIEF_HALF_M: f64 = 800.0;

/// The scale (m) over which the relief term is smoothed through sea level.
///
/// **Authored, and it exists to avoid an atom.** The physically obvious form is
/// `max(height_asl_m, 0.0)`, which is exact but puts every land cell at or
/// below sea level on one identical value. A softplus of the same shape is
/// monotone, smooth, strictly positive, and agrees with the clamp to within a
/// metre by ~200 m of elevation, so it buys atom-freedom for nothing.
///
/// **It is NOT a negligible knob, and an earlier draft of this comment claimed
/// it was.** The carve's marine trim pins a large population of land cells to
/// *exactly* sea level — the probe measures **16.4–18.6% of cave-bearing
/// columns at `height_asl_m == 0.0`** — and for those columns the drawdown term
/// is directly proportional to this constant. Swept over a 20× range with
/// everything else fixed, the wholly-phreatic share moves by 10–20 points:
///
/// ```text
/// RELIEF_SOFT_M     seed 42   seed 7   seed 1234
///        10 m        39.7%    54.3%     49.1%
///        50 m        29.9%    45.5%     42.5%
///       200 m        19.6%    36.5%     32.9%
/// ```
///
/// H3 holds across that whole span, which is the claim worth making; the
/// *value* of the statistic is not robust to it, which is the claim that would
/// have been wrong. 50 m is kept — it is 1/16 of [`RELIEF_HALF_M`], so it
/// perturbs the term only near sea level — and the sweep is recorded rather
/// than used to choose.
const RELIEF_SOFT_M: f64 = 50.0;

/// Flow accumulation at which the recharge term reaches half its maximum.
///
/// **Not authored — it is [`crate::RIVER_MIN_DRAINAGE`]**, the accumulation at
/// which `crate::water::classify` already declares a cell to carry a river. A
/// perennial river *is* the water table intersecting the ground, so the one
/// place this model already commits to saying "surface water lives here" is
/// the right half-saturation point for "the table is close to the surface".
/// Reading the existing constant rather than restating 15.0 means the two
/// cannot drift apart.
const DRAINAGE_HALF: f64 = RIVER_MIN_DRAINAGE;

/// `ln(1 + e^x)`, evaluated in the stable branch for each sign so a large
/// positive argument cannot overflow the exponential. Monotone, strictly
/// positive, and asymptotic to `x` above and to `0` below.
fn softplus(x: f64) -> f64 {
    if x > 0.0 {
        x + math::ln(1.0 + math::exp(-x))
    } else {
        math::ln(1.0 + math::exp(x))
    }
}

/// How much head this column has to drain with, in `(0, 1)`: a smoothed
/// height above sea level, divided by that height plus [`RELIEF_HALF_M`].
/// Sea level is the regional base level at a ~110 km cell, so a column at it
/// has almost nowhere to shed water to and a column a kilometre above it has
/// most of the head it will ever get.
fn relief(height_asl_m: f64) -> f64 {
    let head = RELIEF_SOFT_M * softplus(height_asl_m / RELIEF_SOFT_M);
    head / (head + RELIEF_HALF_M)
}

/// How freely the rock moves water, relative to median rock: a log-linear
/// interpolation across [`TRANSMISSIVITY_SPAN`], centred so `porosity = 0.5`
/// gives exactly 1.
fn transmissivity(porosity: f64) -> f64 {
    math::powf(TRANSMISSIVITY_SPAN, porosity.clamp(0.0, 1.0) - 0.5)
}

/// How much water arrives from upslope, in `[0, 1)`, half-saturating at
/// [`DRAINAGE_HALF`]. Negative accumulations are not a state the field can
/// reach (it counts cells); the guard is there so the function is total, not
/// because a caller is expected to use it.
fn wetness(drainage: f64) -> f64 {
    let flow = drainage.max(0.0);
    flow / (flow + DRAINAGE_HALF)
}

/// Depth below the ground surface (m) at which this column becomes saturated:
/// vadose above, phreatic below.
///
/// `drainage` is a flow accumulation — the upstream land-cell count
/// `crate::drainage::drainage_field` produces, not a normalised ratio;
/// `porosity` is the `[0, 1]` matrix porosity from
/// [`crate::lithology::MaterialBuffer`]; `height_asl_m` is metres above sea
/// level, which may be negative.
///
/// Total over the whole input domain and never negative — see the module doc
/// for why the floor at zero is a physical state (a drowned column) rather
/// than a rail, and for the atom it puts at exactly `0.0`.
///
/// type-audit: bare-ok(count: drainage), bare-ok(ratio: porosity), bare-ok(diagnostic-value: height_asl_m), bare-ok(diagnostic-value: return)
pub fn water_table_depth_m(drainage: f64, porosity: f64, height_asl_m: f64) -> f64 {
    let drawdown = transmissivity(porosity) * relief(height_asl_m);
    let net = drawdown - wetness(drainage);
    TABLE_SCALE_M * net.max(0.0)
}

/// Whether a point at `depth_m` below the surface lies in the flooded
/// (phreatic) zone of a column whose table sits at `water_table_m`.
///
/// The table itself is the *top* of saturation, so a point exactly on it reads
/// vadose. That asymmetry is deliberate and load-bearing at one place: a
/// drowned column has `water_table_m == 0.0`, and every depth strictly below
/// the ground is then phreatic — which is the definition the wholly-phreatic
/// statistic in spec §5's H3 counts.
///
/// Compared with `total_cmp` rather than `>`, matching
/// `crate::water::classify`: the ordering is then total and deterministic for
/// every pair of `f64`, including ones no caller should produce.
///
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(diagnostic-value: water_table_m), bare-ok(flag: return)
pub fn is_phreatic(depth_m: f64, water_table_m: f64) -> bool {
    depth_m.total_cmp(&water_table_m).is_gt()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn more_drainage_raises_the_table() {
        let dry = water_table_depth_m(0.0, 0.3, 500.0);
        let wet = water_table_depth_m(1.0, 0.3, 500.0);
        assert!(wet < dry, "wet={wet} should be shallower than dry={dry}");
    }

    #[test]
    fn more_porous_rock_drains_deeper() {
        let tight = water_table_depth_m(0.5, 0.05, 500.0);
        let porous = water_table_depth_m(0.5, 0.95, 500.0);
        assert!(
            porous > tight,
            "porous={porous} should be deeper than tight={tight}"
        );
    }

    #[test]
    fn the_table_is_never_above_the_surface_and_always_finite() {
        for drainage in [0.0, 0.5, 1.0] {
            for porosity in [0.0, 0.5, 1.0] {
                for h in [-500.0, 0.0, 3000.0] {
                    let d = water_table_depth_m(drainage, porosity, h);
                    assert!(d.is_finite(), "non-finite at {drainage}/{porosity}/{h}");
                    assert!(
                        d >= 0.0,
                        "table above the surface at {drainage}/{porosity}/{h}"
                    );
                }
            }
        }
    }

    #[test]
    fn phreatic_is_below_the_table_and_vadose_above() {
        assert!(is_phreatic(100.0, 50.0), "100 m is below a 50 m table");
        assert!(!is_phreatic(10.0, 50.0), "10 m is above a 50 m table");
    }

    /// The three arguments the brief's monotonicity tests probe at one point
    /// each, swept across the ranges a world actually produces. A single
    /// two-point check cannot tell a monotone function from one that happens to
    /// order two samples correctly.
    #[test]
    fn each_argument_is_monotone_across_its_whole_range() {
        let drainages = [0.0, 1.0, 5.0, 15.0, 60.0, 400.0, 5000.0];
        let porosities = [0.0, 0.025, 0.15, 0.325, 0.46, 0.65, 1.0];
        let heights = [-4000.0, -500.0, -50.0, 0.0, 50.0, 800.0, 3000.0, 8000.0];

        for &p in &porosities {
            for &h in &heights {
                for pair in drainages.windows(2) {
                    let (lo, hi) = (
                        water_table_depth_m(pair[0], p, h),
                        water_table_depth_m(pair[1], p, h),
                    );
                    assert!(hi <= lo, "drainage {}->{}: {lo} -> {hi}", pair[0], pair[1]);
                }
            }
        }
        for &q in &drainages {
            for &h in &heights {
                for pair in porosities.windows(2) {
                    let (lo, hi) = (
                        water_table_depth_m(q, pair[0], h),
                        water_table_depth_m(q, pair[1], h),
                    );
                    assert!(hi >= lo, "porosity {}->{}: {lo} -> {hi}", pair[0], pair[1]);
                }
            }
            for &p in &porosities {
                for pair in heights.windows(2) {
                    let (lo, hi) = (
                        water_table_depth_m(q, p, pair[0]),
                        water_table_depth_m(q, p, pair[1]),
                    );
                    assert!(hi >= lo, "height {}->{}: {lo} -> {hi}", pair[0], pair[1]);
                }
            }
        }
    }

    /// Totality at the edges of the domain, not only inside it. `-4000.0` is
    /// below any land elevation the model produces and `1e12` is an absurdly
    /// high argument to the softplus — the one place an exponential could
    /// overflow to a non-finite value and take the whole column with it.
    #[test]
    fn the_extremes_of_the_domain_stay_finite_and_non_negative() {
        for &q in &[0.0, -1.0, 1e9] {
            for &p in &[-1.0, 0.0, 1.0, 2.0] {
                for &h in &[-1e12, -4000.0, 0.0, 8848.0, 1e12] {
                    let d = water_table_depth_m(q, p, h);
                    assert!(d.is_finite(), "non-finite at {q}/{p}/{h}: {d}");
                    assert!(d >= 0.0, "negative at {q}/{p}/{h}: {d}");
                }
            }
        }
    }

    /// A drowned column is the state the floor exists to express, and the one
    /// H3 counts: nothing below the ground is walkable.
    #[test]
    fn a_river_cell_at_sea_level_is_drowned_from_the_surface() {
        let d = water_table_depth_m(RIVER_MIN_DRAINAGE * 10.0, 0.46, 0.0);
        assert_eq!(d, 0.0, "a well-watered sea-level column should be drowned");
        assert!(
            is_phreatic(1.0, d),
            "every depth below a drowned column floods"
        );
    }

    /// The complement: high, porous, unwatered rock keeps a walkable window
    /// hundreds of metres deep. Asserted as a band, with an absurd-high bound
    /// as well as a low one, so a runaway is a failure rather than a pass.
    #[test]
    fn a_dry_porous_upland_keeps_a_deep_walkable_window() {
        let d = water_table_depth_m(1.0, 0.46, 2000.0);
        assert!(
            (100.0..=600.0).contains(&d),
            "a dry porous upland gave {d} m"
        );
        assert!(!is_phreatic(d / 2.0, d), "half the window should be vadose");
    }

    /// The module doc's one calibration claim, pinned rather than asserted in
    /// prose: the reference column — mean land elevation, median rock, carrying
    /// exactly a river's worth of flow — has its table at the ground. Held to a
    /// millimetre rather than to `0.0` because the softplus makes `relief`
    /// approach one half from above rather than reach it.
    #[test]
    fn the_reference_river_column_sits_exactly_at_the_ground() {
        let at_par = water_table_depth_m(RIVER_MIN_DRAINAGE, 0.5, RELIEF_HALF_M);
        assert!(at_par < 0.001, "reference column gave {at_par} m");
        assert!(
            water_table_depth_m(RIVER_MIN_DRAINAGE * 1.01, 0.5, RELIEF_HALF_M) == 0.0,
            "a hair more flow should drown the reference column outright"
        );
        assert!(
            water_table_depth_m(RIVER_MIN_DRAINAGE * 0.99, 0.5, RELIEF_HALF_M) > 0.001,
            "a hair less flow should leave a walkable window"
        );
    }

    /// The scale constant sets the magnitude and cannot move which columns are
    /// drowned — the claim the module doc makes about H3's independence from
    /// [`TABLE_SCALE_M`]. Checked by re-deriving the sign of the competition
    /// without the constant and requiring it to agree everywhere.
    #[test]
    fn the_metre_scale_never_decides_whether_a_column_is_drowned() {
        for &q in &[0.0, 1.0, 15.0, 200.0] {
            for &p in &[0.0, 0.3, 0.46, 0.8] {
                for &h in &[-100.0, 0.0, 300.0, 2000.0] {
                    let drowned = water_table_depth_m(q, p, h) == 0.0;
                    let dimensionless = transmissivity(p) * relief(h) - wetness(q) <= 0.0;
                    assert_eq!(drowned, dimensionless, "disagreed at {q}/{p}/{h}");
                }
            }
        }
    }
}

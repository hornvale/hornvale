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
//! Two opposed terms, each with its own metre scale:
//!
//! ```text
//! depth = max(0, DRAWDOWN_SCALE_M * transmissivity(porosity) * relief(height)
//!                - RECHARGE_RISE_M * wetness(drainage))
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
//! ### Two datums fix the two scales, and neither is a knob
//!
//! The dimensionless terms carry no metres, so exactly two numbers have to come
//! from outside. Each is solved from one observation, and each solve is a test
//! rather than a comment — move a shape constant and the test names the new
//! root.
//!
//! **The par datum → [`RECHARGE_RISE_M`].** A column at Earth's mean land
//! elevation, in median rock, carrying exactly [`crate::RIVER_MIN_DRAINAGE`],
//! has its table at the ground. `crate::water::classify` already declares that
//! accumulation to be where a perennial watercourse appears, and a perennial
//! watercourse *is* the water table intersecting the surface — so the par is
//! pinned to a claim the model had already committed to.
//!
//! **The vadose datum → [`DRAWDOWN_SCALE_M`].** An Arabika-like column — 2300 m
//! above base level, high-permeability karst, on a recharge plateau — has a
//! ~2.2 km vadose zone. Krubera-Voronja (2197 m) and Veryovkina (2212 m) are
//! air-filled almost to the bottom, with sumps only at depth.
//!
//! Spec §4.2.1 added the second one. The first landing had only the par datum
//! and inferred the depth scale from a global *median* water-table survey,
//! which is a statement about the ordinary column and says nothing about the
//! extraordinary one; a karst massif is exactly the extraordinary column, and
//! it is the only kind a cave campaign cares about. See [`RELIEF_HALF_M`] for
//! the shape defect that went with it.
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
//! **Measured** — the reading lives in the probe's own module doc rather than
//! here, because it moves whenever the calibration does and a second copy would
//! go stale. What is stable enough to state: H3 holds on all three seeds, and
//! above the floor the distribution is a spread rather than a row of spikes.
//!
//! ## What each constant does and does not control
//!
//! **Neither metre MAGNITUDE decides H3 — their ratio does**, and
//! `the_par_ratio_and_not_the_magnitudes_decides_who_drowns` holds that.
//!
//! **H3 IS NOT STRUCTURALLY PROTECTED, and this paragraph exists so nobody
//! reads the one above as saying it is.** The drowned set is
//! `T(p)·R(h) ≤ par·W(q)` with `par = RECHARGE_RISE_M / DRAWDOWN_SCALE_M`, and
//! `par` is *itself* a function of [`RELIEF_HALF_M`], because the par datum
//! solves it as `relief(mean land elevation) / 0.5`. The §4.2.1 correction moved
//! `par` from 1.0 to 0.1658 — a factor of six — and reshaped `R(h)` at the same
//! time. **H3 was fully exposed to that change and survived it EMPIRICALLY, not
//! by construction** (31.9/43.6/41.5% against 29.9/45.5/42.5% before). A future
//! campaign moving any shape constant must re-measure H3; only a change to the
//! two magnitudes *in proportion* is free, and only
//! [`UNDERWORLD_DRYNESS_GAIN`] is that by construction.
//!
//! [`DRAWDOWN_SCALE_M`] sets the absolute depth of every table, and therefore
//! which delve rungs can ever be dry. [`RELIEF_SOFT_M`], which looks like a
//! pure smoothing detail, moves the drowned share by ~15 points across a 20×
//! sweep, because a sixth of all cave columns sit at *exactly* sea level; its
//! doc carries that sweep. Both were claims this module made loosely in an
//! earlier draft and now states from measurement.
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

// ---------------------------------------------------------------------------
// The calibration DATUMS. These five are observations, not shipped parameters:
// the numbers this module actually applies are the two metre scales below, and
// each was solved from these. They are `#[cfg(test)]` because the solve is a
// test — see `the_vadose_datum_is_reproduced` and
// `the_reference_river_column_sits_exactly_at_the_ground` — and they live here,
// beside the values they produced, rather than inside the test block, so a
// reader meets the provenance before the result.
// ---------------------------------------------------------------------------

/// Earth's mean land elevation, ~840 m rounded to 800 — a hypsometric datum,
/// and it is used **only** as the elevation coordinate of the river par datum
/// (see the module doc). It is *not* the relief term's half-saturation point;
/// conflating those two was the defect [`RELIEF_HALF_M`] records.
#[cfg(test)]
const MEAN_LAND_ELEVATION_M: f64 = 800.0;

/// The vadose datum's height above base level: the Arabika Massif stands
/// ~2300 m above the base level its karst drains to.
#[cfg(test)]
const ARABIKA_RELIEF_M: f64 = 2300.0;

/// The vadose datum's measured thickness. Krubera-Voronja (2197 m) and
/// Veryovkina (2212 m) are air-filled almost to the bottom, with sumps only at
/// depth, so the Arabika vadose zone is ~2.2 km thick. **An observation, not a
/// target chosen to make anything downstream come out.**
#[cfg(test)]
const ARABIKA_VADOSE_M: f64 = 2200.0;

/// The porosity coordinate the vadose datum is instantiated at. **The datum is
/// Earth's; this number is the model's**, and the distinction is deliberate:
/// Arabika is high-permeability karst limestone, and "high-permeability karst"
/// in this model means the top of the porosity axis, which
/// `underworld_water_table_probe` measures at 0.819 over the three seeds. Using
/// a literal Earth porosity would calibrate against a rock the model cannot
/// produce.
///
/// **`pub`, and not `#[cfg(test)]` like the other four, because it is the one
/// datum coordinate read off the MODEL rather than off the world.** The other
/// four are facts about Earth and cannot go stale; this one is a frozen
/// measurement of a population, and `crate::lithology`'s induration/porosity
/// coupling is exactly the kind of thing a later campaign moves. If the ceiling
/// drifts, [`DRAWDOWN_SCALE_M`] is silently calibrated against a rock the model
/// no longer makes — a decalibration with no symptom. So the probe pins it:
/// `the_calibration_coordinate_still_matches_the_model` fails if the measured
/// ceiling has moved away from this value, and says to re-solve.
/// type-audit: bare-ok(ratio)
pub const ARABIKA_POROSITY: f64 = 0.819;

/// The drainage coordinate the vadose datum is instantiated at. A massif's
/// entrance plateau is a *recharge* area by definition — water enters there and
/// leaves underground — so it carries essentially no upslope contributing area.
/// `1.0` is the smallest accumulation a land cell can have (itself).
#[cfg(test)]
const ARABIKA_DRAINAGE: f64 = 1.0;

/// Metres of drawdown at unit dimensionless drawdown — **solved from the
/// Arabika datum**, not authored, and pinned by
/// `the_vadose_datum_is_reproduced`.
///
/// The solve: require `water_table_depth_m(ARABIKA_DRAINAGE, ARABIKA_POROSITY,
/// ARABIKA_RELIEF_M) == ARABIKA_VADOSE_M`. Everything else in that equation is
/// already fixed by the shape constants, so it has exactly one root, and this
/// is it. Change any shape constant and the test that pins this will tell you
/// the new root rather than letting it drift.
///
/// **What it decides, since the previous single scale's doc had to be corrected
/// on exactly this point:** the absolute depth of every table, and therefore
/// which delve rungs can ever be dry. It cannot decide H3 — that is
/// [`RECHARGE_RISE_M`]'s *ratio* to this, not either magnitude — and
/// `the_par_ratio_and_not_the_magnitudes_decides_who_drowns` holds that.
const DRAWDOWN_SCALE_M: f64 = 2482.9;

/// Metres the table rises at full upslope inflow — **solved from the river par
/// datum**, given [`DRAWDOWN_SCALE_M`], and pinned by
/// `the_reference_river_column_sits_exactly_at_the_ground`.
///
/// The solve: require a column at `MEAN_LAND_ELEVATION_M`, in median rock,
/// carrying exactly [`crate::RIVER_MIN_DRAINAGE`], to have its table at the
/// ground — which is what a perennial river *is*. That fixes
/// `RECHARGE_RISE_M = DRAWDOWN_SCALE_M * relief(MEAN_LAND_ELEVATION_M) / 0.5`.
///
/// **Why this is a second constant now, when the first landing had one.** The
/// two terms were collapsed onto a single scale because `RELIEF_HALF_M` was
/// simultaneously the relief term's half-point *and* the par datum's elevation,
/// which made `relief(par) = 0.5` exactly and the two scales equal. Correcting
/// the half-point on karst grounds separates those roles, and the scales come
/// apart with them. That is a consequence of the correction rather than a knob
/// added for room: how deep a massif can drain and how far inflow can lift a
/// table are different physical quantities, and the collapse only ever looked
/// elegant because one constant was doing two jobs.
const RECHARGE_RISE_M: f64 = 411.7;

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

/// Height above base level (m) at which base-level control on vadose thickness
/// half-saturates. **Earth's highest land, 8848 m** — the point past which no
/// terrestrial column exists to be described, so the term is effectively
/// proportional across all real relief and saturates only at the top of the
/// world.
///
/// **Which half is datum and which is choice** (spec §4.2.1 clause 3 asks for
/// exactly this split, and it applies here as much as to the gain). The
/// *requirement* is the datum: karst says the term must stay near-proportional
/// across the range deep vadose caves occupy, and the test below turns that
/// into a floor. The *value* is a choice within the range that requirement
/// admits — the floor of 1.75 is met by **any half-point at or above ~6900 m**,
/// so 8848 is not uniquely determined by the physics. It is chosen as the
/// smallest principled stopping point above that floor: the top of the
/// terrestrial domain, which is a scale rather than a coincidence, and picking
/// the domain's edge avoids fitting a number inside an admissible interval.
/// Anything from ~6900 m up would satisfy the same argument.
///
/// **This was 800 m, and 800 m was a category error** (spec §4.2.1, clause 1).
/// That figure is Earth's *mean land elevation* — a hypsometric datum, a
/// statement about where land sits — and it was used as the half-point of a
/// *vadose-thickness* term, which is a statement about how thick an unsaturated
/// zone gets. Nothing licensed the substitution, and karst directly contradicts
/// it: vadose thickness tracks height above local base level with no observed
/// plateau up to ~2.2 km. Krubera-Voronja (2197 m) and Veryovkina (2212 m) are
/// air-filled almost to the bottom because the Arabika Massif stands ~2300 m
/// above its base level. A half-point at 800 m asserts that base-level control
/// has already half-expired at the height of an ordinary hill, so a 2300 m
/// massif drains only 1.48× as deep as an 800 m upland where the observations
/// say ~2.8×. The term saturated exactly across the range where deep vadose
/// caves live, which is the one range it had to resolve.
///
/// **The test the value is chosen against, stated so it can be re-run:** across
/// the karst range a doubling of height above base level must still roughly
/// double the vadose thickness. At `h = 1150 → 2300` this value gives a ratio
/// of **1.7937** against the proportional ideal of 2.0; at 800 m it gave
/// **1.2581**. `the_relief_term_stays_proportional_across_the_karst_range`
/// holds it.
///
/// (An earlier draft of this line said 1.32 for the retired value. That was an
/// estimate presented as a measurement — 1.32 corresponds to a half-point near
/// 1082 m — and it is corrected here rather than quietly overwritten, because
/// it is the same defect class this module has now produced three times. The
/// conclusion is untouched: 1.2581 fails the floor just as 1.32 would.
/// `c9dfee34`'s commit message carries the wrong figure and stays as history.)
const RELIEF_HALF_M: f64 = 8848.0;

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
///        10 m        38.6%    49.9%     46.4%
///        50 m        31.9%    43.6%     41.5%   <- shipped
///       200 m        23.8%    35.0%     34.6%
/// ```
///
/// H3 holds across that whole span, which is the claim worth making; the
/// *value* of the statistic is not robust to it, which is the claim that would
/// have been wrong. 50 m is kept — it is small against [`RELIEF_HALF_M`], so it
/// perturbs the term only near sea level — and the sweep is recorded rather
/// than used to choose.
///
/// **Re-run after the §4.2.1 recalibration, and it had to be.** The correction
/// changed `relief(0)` by roughly 10× (0.0415 → 0.0039), and cells at exactly
/// sea level are this constant's entire population, so the earlier sweep was
/// measuring a term that no longer exists. Its rows were 39.7/54.3/49.1,
/// 29.9/45.5/42.5 and 19.6/36.5/32.9 — close enough to the current ones that
/// leaving them would have looked harmless, which is the reason to re-measure
/// rather than eyeball. The sensitivity is unchanged in kind: ~15 points across
/// a 20× sweep.
///
/// Unlike [`UNDERWORLD_DRYNESS_GAIN`]'s table, this one is **not regenerable
/// from the tree** — it is produced by editing this constant three times and
/// re-running `the_water_table_is_not_degenerate`, because the smoothing scale
/// sits inside a private helper and exposing it would widen the API for a
/// sweep. Stated so no reader mistakes it for a probe output.
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

/// How much drier this world's underworld is than Earth's. **Shipped at 1.0 —
/// Earth — and the measurement that settled that is below.**
///
/// **This constant is the labelled home for an AUTHORED, PLAYABILITY choice**,
/// which is why it has its own name instead of being folded into
/// [`DRAWDOWN_SCALE_M`], where it would read as part of the calibration. Spec
/// §4.2.1 clause 3 is the authority: Earth calibration is a *floor on
/// plausibility, not a ceiling on scale*, and it licenses a drier underworld
/// than Earth's if the genre wants one.
///
/// **The physics is preserved, not overwritten.** [`earth_table_depth_m`] is
/// the gain-free Earth model and is what both datums are checked against, so
/// the calibration stays inspectable at any gain and a later campaign can move
/// the genre without touching the physics or vice versa.
///
/// **It cannot move H3, and that is proved twice.** Multiplying a floored
/// quantity by a positive constant preserves its zero set, so the
/// wholly-phreatic population is identical at every gain:
/// `the_dryness_gain_cannot_move_who_drowns` checks the code has that shape,
/// and the live sweep below confirms it on real worlds — H3 reads 31.9 / 43.6 /
/// 41.5% at **every** value tried, from 1 to 16.
///
/// ## Why it ships at 1.0: the lever does not reach
///
/// It was raised to open `Underdeep` and `Sunless`, which clause 1's physical
/// correction left dry on 0.0% of the columns reaching them. Swept on the three
/// preregistered seeds — share of columns reaching each rung that are naturally
/// dry there, seed 42 / 7 / 1234:
///
/// ```text
/// gain    Deeps              Underdeep         Sunless          H3
///  1.0    29.9/ 1.7/14.4     0.0/0.0/0.0       0.0/0.0/0.0      31.9/43.6/41.5
///  2.0    38.4/ 6.3/21.4     0.0/0.0/0.0       0.0/0.0/0.0      31.9/43.6/41.5
///  3.0    40.1/ 9.0/24.2     0.4/0.0/0.3       0.0/0.0/0.0      31.9/43.6/41.5
///  4.0    41.4/11.9/25.7     1.1/0.1/1.5       0.0/0.0/0.0      31.9/43.6/41.5
///  8.0    47.9/22.9/37.8     2.6/1.9/4.4       0.0/0.0/0.0      31.9/43.6/41.5
/// 16.0    54.2/36.1/45.2     8.2/8.2/10.2      0.5/0.0/0.7      31.9/43.6/41.5
/// ```
///
/// **A sixteenfold departure from Earth still leaves `Sunless` at 0.0–0.7% and
/// `Underdeep` under 11%.** The lever saturates, so the deep rungs are not
/// dry-inaccessible because the table is calibrated too shallow. Shipping a
/// large authored departure that fails at the one thing it was authored for
/// would be a cost with no purchase, so the gain stays at Earth and the finding
/// is recorded instead.
///
/// ## What the real obstacle is, measured rather than reasoned
///
/// **Deep-reaching caves sit in rock that cannot shed water, by construction.**
/// The columns whose caves reach `Sunless` have a median `porosity` of **0.056
/// on all three seeds**, against population medians of 0.781 / 0.374 / 0.379 —
/// essentially the aquitard floor (`crate::lithology`'s
/// `AQUITARD_MAX_POROSITY` is 0.15). So the term this constant multiplies is
/// already ~8× below median at exactly the columns that need it.
///
/// The mechanism is two shipped lines pulling opposite ways, and neither is
/// wrong on its own: [`crate::cave_depth_reach_m`]'s reach rises with
/// `induration` (competent rock holds a void open deeper), while
/// `crate::lithology`'s `assemble_material` builds porosity with a
/// `(1 - induration)` term (cemented rock has less pore space). Depth of reach
/// and capacity to drain are therefore anti-correlated in this model. That is
/// physically defensible and it is nobody's defect, but it means **no value of
/// this constant can open the deep rungs**, and a campaign that wants them
/// opened by hydrology would have to revisit that coupling — Task 1b's
/// territory, not this module's.
///
/// **What does open them is spec §4.2.1's clause 2**, the drainage rule: a
/// `ChamberOrigin::Made` chamber is dry regardless of the table, and the probe
/// measures that it recovers **100% of every reached `Underdeep` and `Sunless`
/// column** (267/877/665 and 214/727/536). The deep is reached by making, not
/// by finding — which is the reading clause 2 states, arrived at here from the
/// other direction.
const UNDERWORLD_DRYNESS_GAIN: f64 = 1.0;

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
    UNDERWORLD_DRYNESS_GAIN * earth_table_depth_m(drainage, porosity, height_asl_m)
}

/// The **gain-free Earth model** — the same derivation with
/// [`UNDERWORLD_DRYNESS_GAIN`] divided out.
///
/// It exists so the physical calibration stays separately inspectable from the
/// genre choice layered on it: the Arabika datum is checked against *this*, not
/// against the shipped function, so a later campaign can move the gain without
/// silently invalidating the physics, or retune the physics without having to
/// disentangle it from the gain first.
///
/// **`pub` so the gain sweep is reproducible from the tree.** With this exposed,
/// any caller can evaluate the model at an arbitrary gain as
/// `gain * earth_table_depth_m(..)` — which is the shipped function's definition
/// — so `underworld_water_table_probe`'s `how_far_does_the_dryness_gain_reach`
/// regenerates [`UNDERWORLD_DRYNESS_GAIN`]'s six-row table in one run instead of
/// requiring six hand-edits of a private constant and six transcriptions. That
/// table is the whole evidence for shipping at Earth, and a record that cannot
/// tell a real row from a typo is not evidence — this module has already
/// produced one sweep drafted from estimate and one probe edit silently
/// defeated by a formatter.
///
/// type-audit: bare-ok(count: drainage), bare-ok(ratio: porosity), bare-ok(diagnostic-value: height_asl_m), bare-ok(diagnostic-value: return)
pub fn earth_table_depth_m(drainage: f64, porosity: f64, height_asl_m: f64) -> f64 {
    let drawdown = DRAWDOWN_SCALE_M * transmissivity(porosity) * relief(height_asl_m);
    let recharge = RECHARGE_RISE_M * wetness(drainage);
    (drawdown - recharge).max(0.0)
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
    /// deep enough to hold habitation rungs. Asserted as a band, with an
    /// absurd-high bound as well as a low one, so a runaway is a failure rather
    /// than a pass.
    ///
    /// The band is stated against the SHIPPED function, gain included, because
    /// what it guards is the playable outcome; the gain-free physics has its own
    /// pin in `the_vadose_datum_is_reproduced`. Both bounds moved with §4.2.1:
    /// this column read ~288 m before the correction and reads ~355 m after, so
    /// the band is set wide enough to be a sanity rail rather than a second,
    /// unstated calibration — and it would catch a runaway from
    /// [`UNDERWORLD_DRYNESS_GAIN`] being raised without review.
    #[test]
    fn a_dry_porous_upland_keeps_a_deep_walkable_window() {
        let d = water_table_depth_m(1.0, 0.46, 2000.0);
        assert!(
            (150.0..=900.0).contains(&d),
            "a dry porous upland gave {d} m"
        );
        assert!(!is_phreatic(d / 2.0, d), "half the window should be vadose");
    }

    /// [`UNDERWORLD_DRYNESS_GAIN`] scales every table and moves no column into
    /// or out of the drowned set, so H3's statistic is bit-identical at every
    /// gain. Multiplying a `max(0, ·)` by a positive constant preserves its zero
    /// set; this checks that the code actually has that shape rather than
    /// trusting the algebra, which is the difference between this and a comment.
    ///
    /// It is what licenses shipping a genre choice on top of a calibrated model
    /// without re-opening a preregistered criterion.
    #[test]
    fn the_dryness_gain_cannot_move_who_drowns() {
        for &q in &[0.0, 1.0, 2.0, 15.0, 200.0, 5000.0] {
            for &p in &[0.0, 0.05, 0.3, 0.46, 0.78, 0.82, 1.0] {
                for &h in &[-500.0, 0.0, 27.0, 48.0, 300.0, 2000.0, 8000.0] {
                    assert_eq!(
                        water_table_depth_m(q, p, h) == 0.0,
                        earth_table_depth_m(q, p, h) == 0.0,
                        "the gain changed the drowned verdict at {q}/{p}/{h}"
                    );
                }
            }
        }
    }

    /// **The vadose datum** (spec §4.2.1, clause 1). An Arabika-like column —
    /// 2300 m above base level, high-permeability karst, on a recharge plateau
    /// — must have a ~2.2 km vadose zone, because Krubera-Voronja and
    /// Veryovkina are air-filled almost to the bottom.
    ///
    /// This test *is* the solve for [`DRAWDOWN_SCALE_M`]: the equation has one
    /// root and the failure message names it, so a change to any shape constant
    /// reports the new root rather than silently drifting off the datum.
    #[test]
    fn the_vadose_datum_is_reproduced() {
        // Against the GAIN-FREE model: the datum is Earth's, and
        // UNDERWORLD_DRYNESS_GAIN is a genre choice that must not be able to
        // make the physics look calibrated when it is not.
        let got = earth_table_depth_m(ARABIKA_DRAINAGE, ARABIKA_POROSITY, ARABIKA_RELIEF_M);
        let error = (got - ARABIKA_VADOSE_M).abs() / ARABIKA_VADOSE_M;
        // Re-solve for the scale that would land exactly on the datum, so a
        // reader who moves a shape constant is told the answer.
        let unit = transmissivity(ARABIKA_POROSITY) * relief(ARABIKA_RELIEF_M);
        let recharge = RECHARGE_RISE_M * wetness(ARABIKA_DRAINAGE);
        let solved = (ARABIKA_VADOSE_M + recharge) / unit;
        assert!(
            error < 0.005,
            "Arabika datum: wanted {ARABIKA_VADOSE_M} m, got {got} m \
             ({:.2}% off). DRAWDOWN_SCALE_M should be {solved:.1}",
            100.0 * error
        );
    }

    /// The shape requirement [`RELIEF_HALF_M`] is chosen against, argued from
    /// karst alone: vadose thickness tracks height above base level with no
    /// observed plateau below ~2.2 km, so across the karst range a doubling of
    /// relief must still roughly double the term.
    ///
    /// The floor is 1.75 against a proportional ideal of 2.0. The retired 800 m
    /// half-point scores **1.2581** and fails it.
    #[test]
    fn the_relief_term_stays_proportional_across_the_karst_range() {
        let ratio = relief(ARABIKA_RELIEF_M) / relief(ARABIKA_RELIEF_M / 2.0);
        assert!(
            (1.75..=2.0).contains(&ratio),
            "doubling relief multiplied the term by {ratio}, not ~2"
        );
    }

    /// **The par datum**, preserved through the recalibration: the reference
    /// column — mean land elevation, median rock, carrying exactly a river's
    /// worth of flow — has its table at the ground, which is what a perennial
    /// river is. This test is the solve for [`RECHARGE_RISE_M`] the same way
    /// the one above solves for [`DRAWDOWN_SCALE_M`].
    #[test]
    fn the_reference_river_column_sits_exactly_at_the_ground() {
        // Gain-free, for the same reason the vadose datum is.
        let at_par = earth_table_depth_m(RIVER_MIN_DRAINAGE, 0.5, MEAN_LAND_ELEVATION_M);
        let solved = DRAWDOWN_SCALE_M * relief(MEAN_LAND_ELEVATION_M) / wetness(RIVER_MIN_DRAINAGE);
        assert!(
            at_par < 1.0,
            "reference column gave {at_par} m; RECHARGE_RISE_M should be {solved:.1}"
        );
        assert!(
            water_table_depth_m(RIVER_MIN_DRAINAGE * 1.05, 0.5, MEAN_LAND_ELEVATION_M) == 0.0,
            "a little more flow should drown the reference column outright"
        );
        assert!(
            water_table_depth_m(RIVER_MIN_DRAINAGE * 0.9, 0.5, MEAN_LAND_ELEVATION_M) > 1.0,
            "a little less flow should leave a walkable window"
        );
    }

    /// Neither magnitude decides who drowns — only their **ratio** does, and
    /// that ratio is fixed by the par datum rather than chosen. Checked by
    /// re-deriving the sign of the competition with both scales divided out and
    /// requiring it to agree everywhere.
    ///
    /// This replaces the first landing's stronger claim that the single scale
    /// could not move H3 at all. With two scales that claim is no longer
    /// available, and pretending otherwise would be the kind of stale doc this
    /// module has already had to correct twice.
    #[test]
    fn the_par_ratio_and_not_the_magnitudes_decides_who_drowns() {
        let par = RECHARGE_RISE_M / DRAWDOWN_SCALE_M;
        for &q in &[0.0, 1.0, 15.0, 200.0] {
            for &p in &[0.0, 0.3, 0.46, 0.8] {
                for &h in &[-100.0, 0.0, 300.0, 2000.0] {
                    let drowned = water_table_depth_m(q, p, h) == 0.0;
                    let dimensionless = transmissivity(p) * relief(h) - par * wetness(q) <= 0.0;
                    assert_eq!(drowned, dimensionless, "disagreed at {q}/{p}/{h}");
                }
            }
        }
    }
}

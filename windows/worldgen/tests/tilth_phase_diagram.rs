//! THE TILTH — the synthetic phase diagram. Measurement only.
//!
//! Nathan's proposal: instead of probing a handful of *generated* worlds, sweep a
//! grid of hand-specified situations and see who settles each one. The reason it is
//! a better instrument is a sampling argument — a generated world only offers the
//! **narrow joint distribution** that terrain and climate happen to produce
//! together, so a five-seed probe can never visit hot-and-wet, cold-and-wet, or
//! hot-and-arid-with-food at all. This sweep visits every combination on purpose,
//! including corners no generated world contains.
//!
//! It needs **no terrain sculpting and no climate generation**: `carrying_capacity`
//! takes bare per-cell inputs, and the per-species term needs only four substrate
//! readings. So a full sweep costs milliseconds instead of the ~2 s per world a real
//! build costs, and it is a *controlled experiment* rather than a sample.
//!
//! What it answers that the probe cannot: **for each region of climate space, is it
//! unsettled because nothing tolerates it, or because nothing can eat there?** That
//! is the differential diagnosis, and the two causes want completely different
//! fixes — tolerance is a niche-authoring problem, supply is a food-web problem.

use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{SETTLERS_PER_CAPACITY, axis_supply};

/// The settling roster.
const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// Stage 5's derived constants (spec §5a, re-derived on stage-1+4 physics).
const V_MAX: f64 = 140.2;
const K_M: f64 = 0.03004;

/// Mirrored from `worldgen` (private): the trophic cascade's transfer fractions
/// and the ambient detritus floor.
const FORAGE_FRACTION: f64 = 0.5;
const PREY_FRACTION: f64 = 0.1;
const DETRITUS_AMBIENT: f64 = 0.2;

/// `carrying_capacity`'s own terms, mirrored so the sweep needs no `Geosphere`.
///
/// Routed through `hornvale_kernel::math` rather than the inherent `f64` methods —
/// not merely to satisfy the lint (decision 0041) but because a mirror using the
/// platform libm could disagree with production in the last ULP, which would make
/// this sweep's numbers subtly non-comparable to the probe's.
fn npp_temperature(t_c: f64) -> f64 {
    1.0 / (1.0 + hornvale_kernel::math::exp(1.315 - 0.119 * t_c))
}
fn npp_precipitation(p_mm: f64) -> f64 {
    1.0 - hornvale_kernel::math::exp(-0.000664 * p_mm.max(0.0))
}
/// `climate::precip_mm_yr`, mirrored.
fn precip_mm_yr(moisture: f64) -> f64 {
    2000.0 * hornvale_kernel::math::powf(moisture.clamp(0.0, 1.0), 1.5)
}

/// One synthetic situation.
#[derive(Clone, Copy)]
struct Situation {
    t_c: f64,
    moisture: f64,
    elevation_m: f64,
    insolation: f64,
}

/// Base productivity, as `carrying_capacity` computes it on land with no
/// freshwater bonus, no coast and no tectonic unrest — the neutral case, so the
/// sweep isolates climate.
fn base_capacity(s: Situation) -> f64 {
    let npp = npp_temperature(s.t_c).min(npp_precipitation(precip_mm_yr(s.moisture)));
    npp * SETTLERS_PER_CAPACITY
}

#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn tilth_phase_diagram() {
    let wc = WorldComponents::assemble().unwrap();

    // 8 temperatures x 7 moistures x 3 elevations = 168 situations, which is the
    // 150-200 Nathan asked for and spans well past what any generated world offers.
    let temps = [-20.0, -10.0, 0.0, 10.0, 18.0, 25.0, 32.0, 40.0];
    let moists = [0.02, 0.08, 0.15, 0.30, 0.50, 0.70, 0.95];
    let elevs = [100.0, 1500.0, 3500.0];

    println!("\n########## THE TILTH — synthetic phase diagram ##########");
    println!("168 situations (8 temp x 7 moisture x 3 elevation), no terrain, no climate gen.");
    println!(
        "\nDIAGNOSIS KEY  '.' nobody viable   'T' tolerance-limited (food exists, nobody fits)"
    );
    println!("               'F' food-limited (someone fits, nothing to eat)   letter = winner\n");

    let mut unsettled_tolerance = 0usize;
    let mut unsettled_food = 0usize;
    let mut settled = 0usize;

    for &elev in &elevs {
        println!("--- elevation {elev:.0} m ---");
        print!("{:>7}", "T\\moist");
        for m in moists {
            print!("{m:>7.2}");
        }
        println!();
        for &t in &temps {
            print!("{t:>7.0}");
            for &m in &moists {
                let s = Situation {
                    t_c: t,
                    moisture: m,
                    elevation_m: elev,
                    insolation: 0.5,
                };
                let base = base_capacity(s);
                let forage = base * FORAGE_FRACTION;
                let prey = forage * PREY_FRACTION;

                let mut best = (0.0_f64, ' ');
                let mut any_tolerant = false;
                let mut any_supply = false;
                for name in SETTLERS {
                    let bio = wc.biosphere.iter().find(|(id, _)| id.0 == name).unwrap().1;
                    let fl = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
                    let cn = &bio.condition_niche;
                    use hornvale_kernel::{
                        ANIMAL_PREY, DETRITUS, MARINE_FORAGE, MINERAL, PHOTOSYNTHATE, PLANT_FORAGE,
                    };
                    let per_axis = [
                        (PHOTOSYNTHATE, base / SETTLERS_PER_CAPACITY),
                        (PLANT_FORAGE, forage / SETTLERS_PER_CAPACITY),
                        (MINERAL, 0.0),
                        (DETRITUS, DETRITUS_AMBIENT),
                        (ANIMAL_PREY, prey / SETTLERS_PER_CAPACITY),
                        (MARINE_FORAGE, 0.0),
                    ];
                    let sup = axis_supply(&bio.niche, &per_axis);
                    // Liebig over the four tolerance axes (stage 5).
                    let tol = cn
                        .temperature
                        .eval(s.t_c, fl)
                        .min(cn.moisture.eval(s.moisture, fl))
                        .min(cn.insolation.eval(s.insolation, fl))
                        .min(cn.elevation.eval(s.elevation_m, 0.0));
                    if tol > 0.05 {
                        any_tolerant = true;
                    }
                    if sup > 0.0 {
                        any_supply = true;
                    }
                    let cap = (V_MAX * sup / (K_M + sup)) * tol;
                    // Viable iff a minimal community clears the famine bar
                    // (pop 2 x NEED / COLLAPSE_PRESSURE = 1.0).
                    if cap > 1.0 && cap > best.0 {
                        best = (cap, name.chars().next().unwrap());
                    }
                }
                let mark = if best.1 != ' ' {
                    settled += 1;
                    best.1
                } else if !any_tolerant {
                    unsettled_tolerance += 1;
                    'T'
                } else if !any_supply {
                    unsettled_food += 1;
                    'F'
                } else {
                    // someone tolerates it and food exists, but not enough of it
                    unsettled_food += 1;
                    'f'
                };
                print!("{mark:>7}");
            }
            println!();
        }
        println!();
    }

    println!("=== DIFFERENTIAL DIAGNOSIS over 168 situations ===");
    println!("  settled                         {settled:3}");
    println!(
        "  unsettled, TOLERANCE-limited    {unsettled_tolerance:3}  (nobody in the roster fits)"
    );
    println!(
        "  unsettled, FOOD-limited         {unsettled_food:3}  (someone fits; too little to eat)"
    );
    println!(
        "\nA food-limited cell is a FOOD-WEB problem (a niche that eats something\n\
         which survives there); a tolerance-limited cell is a ROSTER problem (author\n\
         a species that fits). They are not interchangeable, and the counts say which\n\
         work actually buys settlements."
    );
}

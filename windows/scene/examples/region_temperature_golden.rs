//! Throwaway: regenerate the orrery's `region-temperature-golden.csv` from the
//! current producer. Emits full-precision `temperature_grid_region` values for
//! seed 42 at the campaign golden address (face 0, level 3, tile 4,4, samples
//! 16), sampled at node indices {0,8,144,200,288} x days {0,91.3,200,366.5}.
//! Mirrors the provenance recorded in the golden's header. Run:
//!   cargo run -p hornvale-scene --example region_temperature_golden
use hornvale_kernel::Seed;
use hornvale_scene::temperature_grid_region;
use hornvale_worldgen::{SkyChoice, build_world};

const NODES: &[usize] = &[0, 8, 144, 200, 288];
const DAYS: &[f64] = &[0.0, 91.3, 200.0, 366.5];

fn main() {
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    println!("# Producer-sourced golden for the scene/tiles-region/v1 evaluator-equivalence test.");
    println!("# Provenance: hornvale `temperature_grid_region` (interp of `temperature_at`) —");
    println!("#   world: seed 42, generated sky (`hornvale new --seed 42`);");
    println!("#   address: face=0 level=3 ix=4 iy=4 samples=16 (the campaign golden address).");
    println!("# These are the sim's ground-truth temperatures (full precision); the orrery's");
    println!("# TS evaluator reconstructs them from the QUANTIZED regional layers and must");
    println!("# agree to ~1e-6 relative (quantization precision, decision 0033). This golden");
    println!("# is producer-sourced, NEVER a client reconstruction of itself (The Isotherm rule).");
    println!("# Regenerate: cargo run -p hornvale-scene --example region_temperature_golden");
    println!("# columns: node_index,day,temperature_c");
    for &day in DAYS {
        let grid =
            temperature_grid_region(&world, 0, 3, 4, 4, 16, day).expect("region grid builds");
        for &node in NODES {
            println!("{},{},{}", node, day, grid[node]);
        }
    }
}

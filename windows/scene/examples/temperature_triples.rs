//! Emit the cross-repo temperature contract golden for seed 42: quantized
//! per-tile temperatures from the Rust producer's actual `temperature_at`
//! (via [`hornvale_scene::temperature_grid`]), at a fixed set of
//! (tile-index, day) rows. The orrery client reconstructs temperature from
//! the `t_mean_c`/`t_swing_c` scene layers; this golden is the independent
//! ground truth its equivalence test pins against — not a JS reconstruction
//! of the formula, the sim's own answer.
use hornvale_kernel::Seed;
use hornvale_kernel::quantize::quantize;
use hornvale_scene::temperature_grid;
use hornvale_worldgen::{SkyChoice, build_world};

/// Lattice width; height is width / 2 (matches `scene/tiles/v1`).
const WIDTH: u32 = 64;

/// (tile index, day) rows to sample, fixed by the campaign brief.
const ROWS: &[(usize, f64)] = &[
    (0, 0.0),
    (100, 0.0),
    (100, 91.3),
    (100, 182.6),
    (2000, 300.0),
];

fn main() {
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");

    let rows: Vec<String> = ROWS
        .iter()
        .map(|&(i, day)| {
            let grid = temperature_grid(&world, WIDTH, day).expect("grid builds");
            let t = quantize(grid[i]);
            format!("{{\"i\":{i},\"day\":{day},\"t\":{t}}}")
        })
        .collect();

    println!(
        "{{\"provenance\":\"seed 42, width 64; Rust temperature_grid (windows/scene) quantized; regenerate: cargo run -p hornvale-scene --example temperature_triples\",\"width\":{WIDTH},\"rows\":[{}]}}",
        rows.join(",")
    );
}

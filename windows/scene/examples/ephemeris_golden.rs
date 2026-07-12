//! Emit the ephemeris golden contract for seed 42: the authoritative orbital
//! phase, moon synodic phases, and world rotation phase at sample days, which
//! the orrery client's `ephemeris.ts` must reproduce from the emitted elements.
use hornvale_astronomy::units::StdDays;
use hornvale_kernel::Seed;
use hornvale_worldgen::{SkyChoice, build_world, sky_of};

fn main() {
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let sky = sky_of(&world).expect("generated sky");
    let system = sky.system().expect("system");
    let cal = sky.calendar().expect("calendar");
    let day_length = match &system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => day.get(),
        hornvale_astronomy::Rotation::Locked => f64::INFINITY,
    };
    let mut rows = Vec::new();
    let mut d = 0.0_f64;
    while d < 365.0 {
        // Quantize every emitted phase to the platform-stable canonical form
        // (kernel `quantize`): these are transcendental-derived, and the
        // ephemeris JSON is a committed, cross-platform drift-checked artifact.
        let t = StdDays::new(d).unwrap();
        let world_phase = hornvale_kernel::quantize(cal.year_phase(t));
        let rotation_phase = hornvale_kernel::quantize(if day_length.is_finite() {
            (d / day_length).rem_euclid(1.0)
        } else {
            0.0
        });
        let moons: Vec<f64> = (0..system.moons.len())
            .map(|i| hornvale_kernel::quantize(cal.moon_phase(t, i).unwrap_or(0.0)))
            .collect();
        let moons_json = moons
            .iter()
            .map(|p| format!("{p}"))
            .collect::<Vec<_>>()
            .join(",");
        rows.push(format!(
            "{{\"t\":{d},\"world_phase\":{world_phase},\"rotation_phase\":{rotation_phase},\"moons\":[{moons_json}]}}"
        ));
        d += 20.0;
    }
    println!("{{\"seed\":42,\"samples\":[{}]}}", rows.join(","));
}

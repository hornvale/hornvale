//! Actual native source round-trip diagnostics; no GPU, wall time is not simulation time.
#![allow(
    clippy::disallowed_types,
    reason = "Instant measures observation latency only"
)]
use hornvale_bevy_view::ObservationMirror;
use planetarium::bridge::Bridge;
use std::{path::PathBuf, time::Instant};
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();
    let path = PathBuf::from(args.get(1).ok_or("usage: bridge_cost WORLD REVISION")?);
    let revision = args.get(2).ok_or("missing revision")?.clone();
    let start = Instant::now();
    let (mut bridge, initial) = Bridge::open(path, revision)?;
    let opened = start.elapsed();
    let mut mirror = ObservationMirror::new(&initial)?;
    let mut elapsed = Vec::new();
    for index in 0..1000 {
        let request = mirror.request(index * 10000 - 5000000)?;
        let start = Instant::now();
        let reply = bridge.observe(request)?;
        elapsed.push(start.elapsed().as_secs_f64() * 1e6);
        assert!(mirror.accept(&reply)?);
    }
    elapsed.sort_by(f64::total_cmp);
    println!(
        "binding={} initial_tile_width={} history_reset_policy={}",
        serde_json::to_string(&mirror.initial().binding)?,
        mirror.initial().tiles.width,
        hornvale_bevy_view::HISTORY_RESET_POLICY
    );
    println!(
        "open_including_initial_ms={:.3} exact_queries=1000 roundtrip_p50_us={:.3} p95_us={:.3} max_us={:.3} diagnostics={:?}",
        opened.as_secs_f64() * 1000.0,
        elapsed[500],
        elapsed[950],
        elapsed[999],
        bridge.diagnostics()
    );
    Ok(())
}

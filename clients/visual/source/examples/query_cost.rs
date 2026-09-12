//! Explicit source-only cost witness; no elapsed-time assertions or renderer.
// This excluded-client benchmark measures work duration; it never supplies simulation time.
#![allow(clippy::disallowed_types)]
use hornvale_visual_source::Source;
use std::{path::Path, time::Instant};
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();
    let path = args.get(1).ok_or("usage: query_cost WORLD FULL_REVISION")?;
    let revision = args.get(2).ok_or("usage: query_cost WORLD FULL_REVISION")?;
    let started = Instant::now();
    let mut source = Source::open(Path::new(path), revision, "query-cost")?;
    let open = started.elapsed();
    let started = Instant::now();
    let initial = source.initial_document(64)?;
    let initial_cost = started.elapsed();
    let doc: serde_json::Value = serde_json::from_str(&initial)?;
    let mut costs = Vec::new();
    for index in 0..1000 {
        let request = serde_json::json!({"schema":"visual/request/v1", "binding":doc["binding"], "request_id":index,"ticks":index*10000-5000000}).to_string();
        let started = Instant::now();
        std::hint::black_box(source.observe(&request)?);
        costs.push(started.elapsed().as_secs_f64() * 1e6);
    }
    costs.sort_by(f64::total_cmp);
    println!(
        "open_ms={:.3} initial_width64_ms={:.3} queries=1000 mean_us={:.3} p50_us={:.3} p95_us={:.3} max_us={:.3}",
        open.as_secs_f64() * 1000.0,
        initial_cost.as_secs_f64() * 1000.0,
        costs.iter().sum::<f64>() / costs.len() as f64,
        costs[500],
        costs[950],
        costs[999]
    );
    Ok(())
}

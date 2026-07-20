//! The parameter sweep, the log-log exponent fit, and the report renderer.

use crate::config::{NodeId, SoundingConfig};
use crate::measure::{bake, read_nanos_per_op, replay_nanos};
use hornvale_kernel::{Seed, math::ln, quantize};
use std::fmt::Write as _;

/// One measured point of the sweep.
#[derive(Clone, Copy, Debug)]
pub struct SweepRow {
    /// Which axis was varied ("communities" | "species" | "epochs" | "avg_degree" | "long_range").
    pub axis: &'static str,
    /// The axis value at this point.
    pub value: u64,
    /// Genesis-bake wall-time (ns).
    pub bake_nanos: u128,
    /// Read cost (ns/op).
    pub read_ns_per_op: f64,
    /// Present-replay wall-time (ns).
    pub replay_nanos: u128,
    /// Peak-memory proxy (bytes).
    pub peak_bytes: usize,
    /// Events produced.
    pub events: usize,
}

/// Vary one axis, holding the base config fixed, and measure each point.
pub fn sweep_axis(axis: &'static str, base: &SoundingConfig, values: &[u64]) -> Vec<SweepRow> {
    values
        .iter()
        .map(|&v| {
            let mut c = base.clone();
            match axis {
                "communities" => c.communities = v as u32,
                "species" => c.species = v as u32,
                "epochs" => c.epochs = v as u32,
                "avg_degree" => c.avg_degree = v as f64,
                "long_range" => c.long_range_fraction = v as f64 / 100.0,
                _ => {}
            }
            let (world, m) = bake(&c);
            SweepRow {
                axis,
                value: v,
                bake_nanos: m.nanos,
                read_ns_per_op: read_nanos_per_op(&world, 100_000, Seed(c.seed.0 ^ 0x1EAD)),
                replay_nanos: replay_nanos(&world, NodeId(0), 20, &c),
                peak_bytes: m.peak_bytes,
                events: m.events,
            }
        })
        .collect()
}

/// Least-squares slope of log(y) vs log(x) — the scaling exponent.
pub fn fit_exponent(rows: &[(f64, f64)]) -> f64 {
    let pts: Vec<(f64, f64)> = rows
        .iter()
        .filter(|(x, y)| *x > 0.0 && *y > 0.0)
        .map(|(x, y)| (ln(*x), ln(*y)))
        .collect();
    let n = pts.len() as f64;
    if n < 2.0 {
        return 0.0;
    }
    let (sx, sy) = pts
        .iter()
        .fold((0.0, 0.0), |(ax, ay), (x, y)| (ax + x, ay + y));
    let (mx, my) = (sx / n, sy / n);
    let (mut num, mut den) = (0.0, 0.0);
    for (x, y) in &pts {
        num += (x - mx) * (y - my);
        den += (x - mx) * (x - mx);
    }
    if den == 0.0 { 0.0 } else { num / den }
}

/// Render the report: a markdown summary and the raw CSV.
pub fn render_report(rows: &[SweepRow], sample: &str) -> (String, String) {
    let mut csv = String::from("axis,value,bake_ns,read_ns_per_op,replay_ns,peak_bytes,events\n");
    for r in rows {
        let _ = writeln!(
            csv,
            "{},{},{},{},{},{},{}",
            r.axis,
            r.value,
            r.bake_nanos,
            quantize(r.read_ns_per_op),
            r.replay_nanos,
            r.peak_bytes,
            r.events
        );
    }

    let mut md = String::from("# The Sounding — feasibility frontier\n\n");
    md.push_str(
        "Measured on ONE machine; timings are wall-clock and machine-dependent (the sample biographies below are byte-deterministic). Budgets are lines, not gates.\n\n",
    );
    for axis in [
        "communities",
        "species",
        "epochs",
        "avg_degree",
        "long_range",
    ] {
        let pts: Vec<(f64, f64)> = rows
            .iter()
            .filter(|r| r.axis == axis)
            .map(|r| (r.value as f64, r.bake_nanos as f64))
            .collect();
        if pts.len() >= 2 {
            let _ = writeln!(
                md,
                "- **bake vs {axis}**: scaling exponent ≈ {:.2}",
                fit_exponent(&pts)
            );
        }
    }
    md.push_str("\n## Sample biographies (deterministic)\n\n```\n");
    md.push_str(sample);
    md.push_str("```\n");
    (md, csv)
}

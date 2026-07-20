//! The parameter sweep, the log-log exponent fit, and the report renderer.

use crate::config::{DeliveryMode, NodeId, SoundingConfig};
use crate::measure::{bake, bake_with, read_nanos_per_op, replay_nanos};
use crate::world::Census;
use hornvale_kernel::{Seed, math::ln, quantize};
use std::fmt::Write as _;

/// One measured point of the sweep.
/// type-audit: bare-ok(identifier-text: axis), bare-ok(count: value), bare-ok(diagnostic-value: bake_nanos), bare-ok(ratio: read_ns_per_op), bare-ok(diagnostic-value: replay_nanos), bare-ok(count: peak_bytes), bare-ok(count: events)
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
/// type-audit: bare-ok(identifier-text: axis), bare-ok(count: values)
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

/// One point of the scan-vs-index comparison: the same config baked under
/// both delivery modes (byte-identical worlds; only the times differ).
/// type-audit: bare-ok(count: value), bare-ok(diagnostic-value: scan_nanos), bare-ok(diagnostic-value: index_nanos)
#[derive(Clone, Copy, Debug)]
pub struct ScanIndexRow {
    /// The community count at this point.
    pub value: u64,
    /// Genesis-bake time with the naive linear-scan delivery (ns).
    pub scan_nanos: u128,
    /// Genesis-bake time with the node-index delivery (ns).
    pub index_nanos: u128,
}

/// Bake each community count under BOTH delivery modes — the pair whose
/// exponents exhibit the O(Z²)-scan / O(Z·log Z)-index crossover. Keep the
/// `values` small: the scan is quadratic and will dominate wall-time.
/// type-audit: bare-ok(count: values)
pub fn sweep_scan_vs_index(base: &SoundingConfig, values: &[u64]) -> Vec<ScanIndexRow> {
    values
        .iter()
        .map(|&v| {
            let mut c = base.clone();
            c.communities = v as u32;
            let (_, scan) = bake_with(&c, DeliveryMode::Scan);
            let (_, index) = bake_with(&c, DeliveryMode::Index);
            ScanIndexRow {
                value: v,
                scan_nanos: scan.nanos,
                index_nanos: index.nanos,
            }
        })
        .collect()
}

/// Least-squares slope of log(y) vs log(x) — the scaling exponent.
/// type-audit: bare-ok(ratio: rows), bare-ok(ratio: return)
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
/// type-audit: bare-ok(prose: sample), bare-ok(prose: return)
pub fn render_report(
    rows: &[SweepRow],
    scan_index: &[ScanIndexRow],
    base_census: &Census,
    sample: &str,
) -> (String, String) {
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
    for r in scan_index {
        let _ = writeln!(
            csv,
            "scan_vs_index,{},{},{},0,0",
            r.value, r.scan_nanos, r.index_nanos
        );
    }

    let mut md = String::from("# The Sounding — feasibility frontier\n\n");
    md.push_str(
        "Measured on ONE machine; timings are wall-clock and machine-dependent (the sample biographies below are byte-deterministic). Budgets are lines, not gates.\n\n",
    );

    // Workload census — the transparency surface: the measured phenomena
    // actually fired at a meaningful sample size (see the floor guard in the
    // sweep test).
    md.push_str("## Workload census (base config)\n\n");
    let _ = writeln!(
        md,
        "The measurements below are trustworthy only because the phenomena fired at volume: Grew {}, Founded {}, **Raided {}** (the inter-community coupling), Fled {} (deliveries landing), Collapsed {} — {} communities ever, {} alive at the end.\n",
        base_census.grew,
        base_census.founded,
        base_census.raided,
        base_census.fled,
        base_census.collapsed,
        base_census.communities_total,
        base_census.communities_alive,
    );

    // The coupling, SHOWN: scan vs index crossover.
    let scan_pts: Vec<(f64, f64)> = scan_index
        .iter()
        .map(|r| (r.value as f64, r.scan_nanos as f64))
        .collect();
    let index_pts: Vec<(f64, f64)> = scan_index
        .iter()
        .map(|r| (r.value as f64, r.index_nanos as f64))
        .collect();
    if scan_pts.len() >= 2 {
        md.push_str("## The coupling, shown — naive scan vs node index\n\n");
        let _ = writeln!(
            md,
            "- **bake vs communities, SCAN delivery**: exponent ≈ {:.2} (quadratic ⇒ the architectural dead end the benchmark exists to catch)",
            fit_exponent(&scan_pts)
        );
        let _ = writeln!(
            md,
            "- **bake vs communities, INDEX delivery**: exponent ≈ {:.2} (near-linear ⇒ the fix, and what the shipping path uses)",
            fit_exponent(&index_pts)
        );
        if let Some(top) = scan_index.last() {
            let _ = writeln!(
                md,
                "- at {} communities the scan is {:.0}× the index and diverging.\n",
                top.value,
                top.scan_nanos as f64 / top.index_nanos.max(1) as f64
            );
        }
    }

    // Bake scaling by axis (index delivery — the shipping path).
    md.push_str("## Bake scaling by axis (index delivery)\n\n");
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

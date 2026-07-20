//! The heavy-tier sweep. The exponent fit itself is a fast, checkable unit.

use hornvale_chronicle::sweep::fit_exponent;

#[test]
fn exponent_fit_recovers_a_known_power_law() {
    // y = x^2: log-log slope must be ~2.
    let pts: Vec<(f64, f64)> = (1..=10).map(|x| (x as f64, (x * x) as f64)).collect();
    let slope = fit_exponent(&pts);
    assert!((slope - 2.0).abs() < 1e-6, "expected slope ~2, got {slope}");
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn run_the_sounding_and_write_the_report() {
    use hornvale_chronicle::sweep::{SweepRow, render_report, sweep_axis};
    use hornvale_chronicle::{SoundingConfig, biography_digest, run};
    use hornvale_kernel::Seed;

    let base = SoundingConfig {
        seed: Seed(1),
        communities: 1_000,
        species: 16,
        epochs: 500,
        avg_degree: 4.0,
        long_range_fraction: 0.05,
    };

    // PREREGISTERED HYPOTHESES (frozen here BEFORE reading the fitted values):
    //   bake vs communities ~ 1.0 (linear)   bake vs epochs ~ 1.0 (linear)
    //   bake vs avg_degree  ~ 1.0 (coupling bounded-local, NOT quadratic)
    //   read ~ 0.0 (flat, O(1))               replay ~ flat in world size
    // The headline finding is the avg_degree exponent: > 1.5 would falsify
    // "bounded-local coupling" and is the architectural red flag.

    let mut rows: Vec<SweepRow> = Vec::new();
    rows.extend(sweep_axis(
        "communities",
        &base,
        &[100, 300, 1_000, 3_000, 10_000, 30_000],
    ));
    rows.extend(sweep_axis("species", &base, &[4, 16, 64, 256]));
    rows.extend(sweep_axis("epochs", &base, &[100, 300, 1_000, 3_000]));
    // Adversarial density axes (stress the sparsity assumption to its edge):
    // push degree toward Z and long-range edges to 100%. NOTE: this spike's
    // coupling is EVENT-based (a raid picks ONE neighbour, O(1) per event), so
    // it is degree-independent by construction — these axes exercise graph
    // CREATION/TRAVERSAL cost at density, not the *diffuse* (all-edges
    // fixed-point) coupling, which is not in this spike and is the next
    // sounding's target (see the retrospective).
    rows.extend(sweep_axis(
        "avg_degree",
        &base,
        &[2, 4, 8, 16, 32, 64, 128, 256],
    ));
    rows.extend(sweep_axis("long_range", &base, &[0, 5, 20, 50, 100]));

    let sample = biography_digest(&run(&base));
    let sample_head: String = sample.lines().take(12).collect::<Vec<_>>().join("\n") + "\n";
    let (md, csv) = render_report(&rows, &sample_head);

    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../book/src/laboratory/generated/the-sounding");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("summary.md"), md).unwrap();
    std::fs::write(dir.join("rows.csv"), csv).unwrap();
    std::fs::write(dir.join("sample-biographies.txt"), sample_head).unwrap();
}

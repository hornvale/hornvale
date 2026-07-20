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
    use hornvale_chronicle::sweep::{SweepRow, render_report, sweep_axis, sweep_scan_vs_index};
    use hornvale_chronicle::{SoundingConfig, biography_digest, census, run};
    use hornvale_kernel::Seed;

    let base = SoundingConfig {
        seed: Seed(1),
        communities: 1_000,
        species: 16,
        epochs: 500,
        avg_degree: 4.0,
        long_range_fraction: 0.05,
    };

    // PREREGISTERED HYPOTHESES (frozen BEFORE reading the fitted values):
    //   coupling under SCAN delivery ~ 2.0 (quadratic — the naive dead end);
    //   coupling under INDEX delivery ~ 1.0 (near-linear — the fix);
    //   bake vs communities (index) ~ 1.0; vs epochs ~ 1.0; read ~ flat;
    //   density axes (degree, long_range) ~ flat (event-coupling is O(1)/event).

    // SAMPLE-SIZE FLOOR (the workload census guard): the coupling the sweep
    // measures MUST fire at volume, or the exponents measure noise. Abort with
    // a specific message rather than silently reporting a degenerate run.
    let base_census = census(&run(&base));
    const RAID_FLOOR: u64 = 100_000;
    assert!(
        base_census.raided >= RAID_FLOOR,
        "The Sounding measures the coupling's scaling, but raids fired only {} times at the base config (floor {}). The dynamics do not exercise the coupling — re-tune before trusting any exponent. Full census: {:?}",
        base_census.raided,
        RAID_FLOOR,
        base_census,
    );

    // The headline: scan vs index, on a small community range (the scan is
    // quadratic and will dominate wall-time, so keep Z modest and epochs low).
    let crossover_base = SoundingConfig {
        epochs: 200,
        ..base.clone()
    };
    let scan_index = sweep_scan_vs_index(&crossover_base, &[250, 500, 1_000, 2_000]);

    let mut rows: Vec<SweepRow> = Vec::new();
    rows.extend(sweep_axis(
        "communities",
        &base,
        &[100, 300, 1_000, 3_000, 10_000, 30_000],
    ));
    rows.extend(sweep_axis("species", &base, &[4, 16, 64, 256]));
    rows.extend(sweep_axis("epochs", &base, &[100, 300, 1_000, 3_000]));
    // Adversarial density axes (stress the sparsity assumption to its edge):
    // degree toward Z, long-range to 100%. The event-coupling picks ONE
    // neighbour per raid (O(1)/event), so these exercise graph CREATION/
    // TRAVERSAL at density, not the *diffuse* (all-edges) coupling — which is
    // absent from this spike and is the next sounding's target (see the retro).
    rows.extend(sweep_axis(
        "avg_degree",
        &base,
        &[2, 4, 8, 16, 32, 64, 128, 256],
    ));
    rows.extend(sweep_axis("long_range", &base, &[0, 5, 20, 50, 100]));

    let sample = biography_digest(&run(&base));
    let sample_head: String = sample.lines().take(12).collect::<Vec<_>>().join("\n") + "\n";
    let (md, csv) = render_report(&rows, &scan_index, &base_census, &sample_head);

    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../book/src/laboratory/generated/the-sounding");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("summary.md"), md).unwrap();
    std::fs::write(dir.join("rows.csv"), csv).unwrap();
    std::fs::write(dir.join("sample-biographies.txt"), sample_head).unwrap();
}

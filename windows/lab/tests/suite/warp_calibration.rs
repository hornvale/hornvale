//! The Warp, Task 6: the seed-42 calibration readout. Prints every spec §7
//! quantity for the one seed a constant may be set against, and asserts only
//! the fixture (the seed) and the shape (a number came back).
//!
//! **This file is a PRINTER, not a gate.** Spec §7's bands are claims about
//! the four READOUT seeds, and Task 7's `warp_readout.rs` owns them. Asserting
//! a band here would gate the calibration on the seed the calibration was
//! fitted to, which is the circularity decision 0016 exists to forbid. What
//! this file does assert is the fixture: that seed 42 built, that the
//! registered metrics returned numbers rather than `Absent`, and — in the
//! second test — that no readout seed is named anywhere in its own source.
//!
//! The verdict block the readout prints beside the table applies §7's bars to
//! seed 42 anyway, because that is what a calibrator has to read to know
//! whether to turn a dial. It is printed, never asserted. A second block
//! below it prints the between-kind ordering H2 used to gate — withdrawn
//! 2026-09-05, before any readout seed was built — raw and normalised by each
//! kind's own `H(Y)`; see the comment at that block for why an ordering in
//! bits cannot be a bar.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{BuiltView, ClimateView, MetricValue, registry};

/// The one seed this file may build. Task 7's `warp_readout.rs` owns the four
/// readout seeds, and this file must never name one — see
/// [`this_file_builds_only_the_calibration_seed`].
const CALIBRATION_SEED: Seed = Seed(42);

/// The four `hornvale_worldgen::WeftKind::ALL` suffixes, in slot order.
const KINDS: [&str; 4] = ["spring", "overhang", "thicket", "erratic"];

/// This file's own path, for the decision-0016 guard below.
/// `env!("CARGO_MANIFEST_DIR")` rather than `file!()`: `file!()`'s value is
/// the path rustc was handed, which is relative to the package directory and
/// therefore depends on the working directory the harness happens to use.
const OWN_SOURCE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/suite/warp_calibration.rs"
);

fn read(built: &BuiltView, name: &str) -> MetricValue {
    registry()
        .into_iter()
        .find(|m| m.name == name)
        .unwrap_or_else(|| panic!("{name} registered"))
        .extract
        .apply(built)
}

/// The metric's reading, or `NaN` when it is `Absent` — the table prints
/// `Absent` readings as blanks rather than fabricating a zero, so the two
/// stay distinguishable in the ledger.
fn number(built: &BuiltView, name: &str) -> f64 {
    match read(built, name) {
        MetricValue::Number(n) => n,
        MetricValue::Absent => f64::NAN,
        other => panic!("{name}: {other:?}"),
    }
}

fn entry(v: f64) -> String {
    if v.is_nan() {
        "  Absent".to_string()
    } else {
        format!("{v:8.5}")
    }
}

/// claim: readout(seed: 42, preregistered, 0016) — one seed, reported and
/// never gated; the bands it prints are claims about Task 7's readout seeds.
#[test]
#[ignore = "probe: builds seed 42 once (~2s) and prints every spec section 7 quantity for the calibration decision 0016 requires be made on one seed; asserts only the seed and that numbers came back"]
fn print_the_seed_42_calibration_table() {
    let built = BuiltView::Climate(
        ClimateView::build(CALIBRATION_SEED, &SkyPins::default()).expect("seed 42 builds"),
    );

    println!("=== The Warp, Task 6 — seed 42 calibration readout ===");
    println!(
        "{:<10} {:>8} {:>8} {:>8} {:>8} {:>7} {:>8} {:>8} {:>8} {:>9} {:>8}",
        "kind",
        "found",
        "ch-mi",
        "ch-null",
        "ch-net",
        "lift",
        "maxrate",
        "learner",
        "oracle",
        "false-net",
        "density",
    );
    let mut found = [f64::NAN; 4];
    let mut net = [f64::NAN; 4];
    let mut lift = [f64::NAN; 4];
    let mut maxrate = [f64::NAN; 4];
    let mut learner = [f64::NAN; 4];
    let mut false_net = [f64::NAN; 4];
    let mut density = [f64::NAN; 4];
    for (i, kind) in KINDS.iter().enumerate() {
        let g = |family: &str| number(&built, &format!("warp-{family}-{kind}"));
        let mi = g("channel-mi");
        let null = g("channel-null");
        found[i] = g("found-fraction");
        net[i] = mi - null;
        lift[i] = g("best-lift");
        maxrate[i] = g("max-class-rate");
        learner[i] = g("learner-gain");
        false_net[i] = g("false-sign-net");
        density[i] = number(&built, &format!("weft-existence-density-{kind}"));
        println!(
            "{kind:<10} {} {} {} {} {:>7} {} {} {} {:>9} {}",
            entry(found[i]),
            entry(mi),
            entry(null),
            entry(net[i]),
            if lift[i].is_nan() {
                " Absent".to_string()
            } else {
                format!("{:7.3}", lift[i])
            },
            entry(maxrate[i]),
            entry(learner[i]),
            entry(g("oracle-gain")),
            if false_net[i].is_nan() {
                "  Absent".to_string()
            } else {
                format!("{:9.5}", false_net[i])
            },
            entry(density[i]),
        );
    }

    // The verdict block: spec §7's bars applied to seed 42. PRINTED, NEVER
    // ASSERTED — see this file's module doc.
    println!("\n--- spec section 7 bars, applied to seed 42 (printed, not gated) ---");
    for (i, kind) in ["spring", "overhang"].iter().enumerate() {
        println!(
            "H1 {kind:<9} found {:.4} >= 0.60 : {}",
            found[i],
            found[i] >= 0.60
        );
    }
    println!(
        "H1 thicket   found {:.4} in [0.30, 0.55] : {}",
        found[2],
        (0.30..=0.55).contains(&found[2])
    );
    for (i, kind) in ["spring", "overhang"].iter().enumerate() {
        println!(
            "H2 {kind:<9} lift {:.3} >= 2 x erratic {:.3} = {:.3} : {}",
            lift[i],
            lift[3],
            2.0 * lift[3],
            lift[i] >= 2.0 * lift[3]
        );
    }
    for (i, kind) in ["spring", "overhang", "thicket"].iter().enumerate() {
        println!(
            "H3 {kind:<9} learner {:.5} > 0 : {}   learner/(channel net) = {:.4}",
            learner[i],
            learner[i] > 0.0,
            learner[i] / net[i]
        );
    }
    println!(
        "H3 erratic   learner {:.5} <= 0.001 : {}",
        learner[3],
        learner[3] <= 0.001
    );
    println!(
        "H4 erratic   channel net {:.5} <= 0.008 : {}",
        net[3],
        net[3] <= 0.008
    );
    for (i, kind) in KINDS.iter().enumerate() {
        println!(
            "H4 {kind:<9} false-sign net {:.5} within +/-0.002 : {}",
            false_net[i],
            false_net[i].abs() <= 0.002
        );
    }
    for (i, kind) in KINDS.iter().enumerate() {
        println!(
            "H5 {kind:<9} max class rate {:.5} <= 0.75 : {}",
            maxrate[i],
            maxrate[i] <= 0.75
        );
    }
    // THE BETWEEN-KIND ORDERING IS REPORTED, NEVER GATED — withdrawn from
    // H2's gate on 2026-09-05, before any readout seed was built (controller
    // ruling on Task 6's fix round 1; spec §7 amended, ledger #11).
    //
    // WHY IT CANNOT BE A BAR. Mutual information is in bits and scales with
    // the event's own entropy `H(Y)`, so a between-kind ordering
    // "spring's net ≥ overhang's" can always be satisfied by making OVERHANG
    // RARER — nothing about spring need improve at all. This task's round 3
    // did exactly that, cutting overhang's reliability to 0.16 and its
    // frequency twelvefold to clear a clause about a different kind. A bar
    // that can be met by removing a kind from the world is measuring the wrong
    // thing. Each kind is gated on its OWN legibility (found fraction, lift
    // against the erratic, learner gain, max class rate); the comparison
    // survives here as a reading, in both forms — raw, and divided by each
    // kind's own `H(Y)` so the base-rate term is taken out of it.
    println!("\n--- reported, never gated: the between-kind ordering, raw and normalised ---");
    let h = |p: f64| -> f64 {
        if p <= 0.0 || p >= 1.0 {
            f64::NAN
        } else {
            // `hornvale_kernel::math::log2`, never `f64::log2` — the platform
            // libm diverges in the last ULP and the workspace bans it.
            -(p * hornvale_kernel::math::log2(p) + (1.0 - p) * hornvale_kernel::math::log2(1.0 - p))
        }
    };
    for (i, kind) in KINDS.iter().enumerate() {
        println!(
            "  {kind:<9} net {:>9.5}   H(Y) {:>8.5}   net/H(Y) {:>7.4}",
            net[i],
            h(density[i]),
            net[i] / h(density[i])
        );
    }
    println!(
        "  spring net >= overhang net: {} (raw)   {} (share of own ceiling)",
        net[0] >= net[1],
        net[0] / h(density[0]) >= net[1] / h(density[1])
    );

    // Spec §6.3's consequence, reported per seed. THE POPULATIONS DIFFER AND
    // THE FIGURES ARE RESCALED SO THEY CAN BE COMPARED AT ALL: `the_weft.rs`'s
    // committed 0.984% / 2.058% / 3.703% / 1.045% are over the WHOLE grid
    // (40,962 geosphere vertices), while `weft-existence-density-*` is over
    // the LAND-ELIGIBLE subset (11,218) — a factor of 3.6515. Printing the two
    // side by side unrescaled reads as a 3.65x change in every kind, including
    // the two controls this campaign never touches.
    const WHOLE_GRID_OVER_LAND: f64 = 40_962.0 / 11_218.0;
    println!("\nP(Y) over the land-eligible pool, and the Weft's committed figure rescaled to it:");
    for (i, kind) in KINDS.iter().enumerate() {
        let weft = [0.00984, 0.02058, 0.03703, 0.01045][i] * WHOLE_GRID_OVER_LAND;
        println!("  {kind:<9} {:.5}   (the Weft: {weft:.5})", density[i]);
    }

    // The only assertions: the fixture built and the instrument answered.
    assert!(
        number(&built, "warp-channel-mi-spring").is_finite(),
        "the instrument returned no channel reading for spring on seed 42"
    );
    for (i, kind) in KINDS.iter().enumerate() {
        assert!(
            density[i].is_finite(),
            "{kind}: no existence density on seed 42"
        );
    }
}

/// The land-eligible pool's `macro_state` for one sign kind, rebuilt from
/// `hornvale-worldgen`'s own public surface exactly as `windows/lab`'s grid
/// pool builds it (one representative facet per geosphere vertex, seven
/// levels below the grid, land-eligible at a blended `land` of 0.5).
#[allow(clippy::disallowed_methods)]
fn land_causes(kind: hornvale_worldgen::WeftKind) -> Vec<f64> {
    use hornvale_kernel::{Facet, Vertex};
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate");
    let ctx = hornvale_locale::LocaleContext::build_from(&world, &terrain, &climate);
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let geo = climate.geosphere();
    let depth = geo.depth() + 7;
    let mut causes = Vec::new();
    for v in 0..geo.vertex_count() {
        let facet = Facet::containing(geo.position(Vertex(v as u32)), depth);
        let Some(weights) = facet.corner_weights(geo, ctx.nearest_index()) else {
            continue;
        };
        if hornvale_kernel::blend_corner_weights(weights, &pack.land) < 0.5 {
            continue;
        }
        causes.push(kind.macro_state(weights, &pack));
    }
    causes
}

/// The Hermite step `windows/worldgen`'s recipe uses, restated here so the
/// sweep predicts the recipe rather than an approximation of it.
fn smoothstep(x: f64, lo: f64, hi: f64) -> f64 {
    let t = ((x - lo) / (hi - lo)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

/// The calibration's own instrument: what a sign kind's cause distribution
/// on seed 42 does to the two H1 quantities as the step edges move.
///
/// The occurrence draw is a comparison of a decorrelated uniform field
/// against `rate · smoothstep(cause)`, so the EXPECTED occurrence count is
/// `rate · Σ smoothstep(cause_i)` and the EXPECTED found fraction is
/// `Σ_{cause ≥ 0.5} step / Σ step` — a quantity in which the rate cancels
/// entirely. That is the whole reason this table exists: it says which edge
/// pair can reach H1's 0.60 at all, before any world is rebuilt, and leaves
/// the rate free to set the frequency afterwards. `mean-step` is the density
/// per unit rate, so a candidate's predicted `P(Y)` is `rate × mean-step`.
///
/// It is a PREDICTION and the readout above is the measurement; the two
/// differ by the occurrence draw's own sampling noise over a few hundred
/// facets. The positive control is printed with the table: the prediction
/// evaluated at the constants that are actually compiled in, beside what
/// `print_the_seed_42_calibration_table` measured for them.
#[test]
#[ignore = "probe: builds seed 42 once (~2s) and sweeps the soft-step edges analytically over its cause distribution, the instrument decision 0016 calibration used; asserts only the population"]
fn print_the_seed_42_cause_distribution_and_step_sweep() {
    for kind in [
        hornvale_worldgen::WeftKind::Spring,
        hornvale_worldgen::WeftKind::Overhang,
    ] {
        let causes = land_causes(kind);
        assert_eq!(
            causes.len(),
            11_218,
            "the land-eligible population moved — the Weft's own figure is 11,218"
        );
        let mut sorted = causes.clone();
        sorted.sort_by(f64::total_cmp);
        println!(
            "\n=== {kind:?}: cause distribution over {} land facets ===",
            causes.len()
        );
        print!("quantiles ");
        for q in [0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 1.0] {
            let i = (((sorted.len() - 1) as f64) * q).round() as usize;
            print!(" p{:<5.1}={:.4}", q * 100.0, sorted[i]);
        }
        println!();
        println!(
            "facets with cause >= 0.5: {}",
            causes.iter().filter(|c| **c >= 0.5).count()
        );

        let (lo_now, hi_now) = kind.step_edges();
        println!(
            "\n{kind:?} edge sweep — found = predicted H1 found fraction, mean-step = P(Y) per unit rate"
        );
        println!("{:>6} {:>6} {:>8} {:>10}", "lo", "hi", "found", "mean-step");
        let mut edges: Vec<(f64, f64)> = Vec::new();
        for lo10 in 20u32..=60 {
            for hi10 in (lo10 + 5)..=95 {
                if lo10 % 5 == 0 && hi10 % 5 == 0 {
                    edges.push((f64::from(lo10) / 100.0, f64::from(hi10) / 100.0));
                }
            }
        }
        edges.push((lo_now, hi_now));
        for (lo, hi) in edges {
            let mut total = 0.0;
            let mut on_cause = 0.0;
            for &c in &causes {
                let s = smoothstep(c, lo, hi);
                total += s;
                if c >= 0.5 {
                    on_cause += s;
                }
            }
            if total <= 0.0 {
                continue;
            }
            let mark = if (lo, hi) == (lo_now, hi_now) {
                "  <- compiled in"
            } else {
                ""
            };
            println!(
                "{lo:>6.2} {hi:>6.2} {:>8.4} {:>10.6}{mark}",
                on_cause / total,
                total / causes.len() as f64
            );
        }
    }
}

/// Decision 0016's guard: this file names exactly one seed, and it is the
/// calibration seed. The readout seeds are Task 7's, and a constant fitted
/// against one of them would make the readout a measurement of its own
/// training set.
///
/// The forbidden literals are BUILT rather than written, so this test's own
/// source does not contain the strings it forbids — spelling them inline
/// would make the guard fail on itself, which reads exactly like a violation.
/// claim: structural(seed: 42) — a source scan over this file's own text,
/// not a sweep: the seed numbers it iterates are the ones it forbids, and it
/// builds no world at all.
#[test]
fn this_file_builds_only_the_calibration_seed() {
    let src = std::fs::read_to_string(OWN_SOURCE).expect("own source");
    // A positive control on the read: if the path ever stops resolving to
    // this file, the scan below would pass over the wrong text (or empty
    // text) and report a clean bill.
    assert!(
        src.contains("CALIBRATION_SEED") && src.contains(&format!("Seed({})", 42)),
        "the guard is not reading this file: {OWN_SOURCE}"
    );
    for n in [13u32, 7, 1, 100] {
        let forbidden = format!("Seed({n})");
        assert!(
            !src.contains(&forbidden),
            "a readout seed appears in the calibration file: {forbidden}"
        );
    }
}

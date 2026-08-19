//! THE FALLOW — feasibility probe for §3.1, run BEFORE any implementation.
//!
//! The Fallow's `H1` predicts that a land-capital account plus year-to-year
//! variance restores deep-history columns — at least four stacked layers on some
//! cell, against the one that The Tilth's stages 1+4 leave. That prediction rests
//! on a *mechanism*: a community draws its ground down, a bad stretch arrives, the
//! ground cannot carry it, the community leaves, the ground recovers, someone
//! returns. Nothing about that requires a `Geosphere`, a world, or the bake — so it
//! can be tested as a **pure time-stepped model of one cell in milliseconds**,
//! before committing to the determinism-sensitive bake rewire it would otherwise
//! need.
//!
//! This is the same discipline that has paid repeatedly in this arc: The Keeping's
//! Task 0 killed a spec's design for the price of one test, and The Tilth's `H1`
//! was answered as a pure measurement because the species-blind capacity cancels
//! from `argmax`. Measure the mechanism, then build it.
//!
//! **Nothing here is tuned to produce the answer.** The probe *sweeps* extraction
//! rate against variance amplitude and reports the resulting column count as a
//! surface, so the question is "is there a regime where columns emerge, and how
//! wide is it?" rather than "can I find numbers that work?". A narrow regime is a
//! finding against §3.1; a wide one is support for it.

/// Epochs and their length, matching `BakeConfig::default_millennia` — two
/// millennia in 25-year steps, so a "layer" here means what it means in the bake.
const EPOCHS: usize = 80;

/// Population below which an occupation is over (`history_bake::VIABLE_MIN`).
const VIABLE_MIN: f64 = 2.0;
/// Pressure at or above which a community collapses (`history_bake::COLLAPSE_PRESSURE`).
const COLLAPSE_PRESSURE: f64 = 2.0;
/// Per-epoch logistic growth rate (`history_bake::GROWTH_RATE`).
const GROWTH_RATE: f64 = 0.2;
/// Starting population of a fresh occupation (`history_bake::DAUGHTER_POP`).
const FOUND_POP: f64 = 8.0;

/// Deterministic stand-in for the paleoclimate era series' good and bad stretches.
/// NOT a seeded draw: §5 forbids adding one, since new draws are the epoch-
/// triggering additions. A fixed multi-period beat gives runs of good and bad
/// epochs without randomness, which is what the era series supplies in production.
fn climate_multiplier(epoch: usize, amplitude: f64) -> f64 {
    let t = epoch as f64;
    // Two incommensurate periods so good and bad stretches cluster rather than
    // alternating — clustering is what evicts, a single bad epoch rarely does.
    let beat = hornvale_kernel::math::sin(t / 7.0) + hornvale_kernel::math::sin(t / 23.0);
    (1.0 + amplitude * beat / 2.0).max(0.0)
}

/// One cell's two-millennia history under a land-capital model.
/// Returns the number of distinct occupations (the column depth).
fn run_cell(base_capacity: f64, extraction_rate: f64, variance: f64, regen_rate: f64) -> usize {
    // The capital account, normalised: 1.0 is pristine ground, 0.0 exhausted.
    let mut tilth = 1.0_f64;
    let mut pop = 0.0_f64;
    let mut layers = 0usize;
    let mut occupied = false;

    for epoch in 0..EPOCHS {
        // Effective capacity: productivity this epoch, scaled by the state of the
        // ground. This is §3.1's `productivity x h(tilth)` with `h` linear.
        let eff = (base_capacity * climate_multiplier(epoch, variance) * tilth).max(0.0);

        if !occupied {
            // Someone returns if the ground could now carry a founding party.
            if eff > FOUND_POP / COLLAPSE_PRESSURE {
                occupied = true;
                pop = FOUND_POP;
                layers += 1;
            }
        } else {
            let pressure = if eff > 0.0 { pop / eff } else { f64::INFINITY };
            if pressure >= COLLAPSE_PRESSURE || pop < VIABLE_MIN {
                occupied = false;
                pop = 0.0;
            } else {
                pop *= 1.0 + GROWTH_RATE * (1.0 - pressure);
            }
        }

        // The capital account moves: worked ground draws down, idle ground heals.
        // Slow in, fast out -- regeneration is a small fraction of the deficit,
        // extraction scales with how hard the ground is being worked.
        let extraction = if occupied {
            extraction_rate * (pop / base_capacity.max(1e-9))
        } else {
            0.0
        };
        tilth = (tilth + regen_rate * (1.0 - tilth) - extraction).clamp(0.0, 1.0);
    }
    layers
}

#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn fallow_feasibility_sweep() {
    println!("\n######## THE FALLOW — §3.1 feasibility sweep ########");
    println!("One cell, 80 epochs x 25 years, no world. Column depth as a surface.");
    println!("Target: H1 wants >= 4 layers. Stages 1+4 currently deliver 1.\n");

    // Marginal-but-real ground: the median capacity on good ground post-stage-1+4
    // was ~58, so take a fraction of it -- columns should form on MARGINAL land,
    // which is where the sixteen steadings actually were.
    let base = 20.0_f64;
    let regen = 0.02_f64; // ~50 epochs (1250 yr) to heal from bare -- centuries, per §6 q2

    let extractions = [0.00, 0.01, 0.02, 0.04, 0.08, 0.16];
    let variances = [0.0, 0.2, 0.4, 0.6, 0.8];

    println!("  rows = extraction rate, cols = climate variance amplitude");
    print!("{:>10}", "extr\\var");
    for v in variances {
        print!("{v:>7.1}");
    }
    println!();
    let mut in_target = 0usize;
    let mut total = 0usize;
    for e in extractions {
        print!("{e:>10.2}");
        for v in variances {
            let n = run_cell(base, e, v, regen);
            total += 1;
            if n >= 4 {
                in_target += 1;
            }
            print!("{n:>7}");
        }
        println!();
    }
    println!(
        "\n  cells of the sweep reaching H1's >= 4 layers: {in_target}/{total}\n\
         \x20 A WIDE regime supports §3.1 (the mechanism works without tuning).\n\
         \x20 A NARROW regime is evidence AGAINST it -- columns would then be an\n\
         \x20 artifact of particular constants, which is the trap the old hard zero\n\
         \x20 already set once."
    );

    // The two controls that say WHICH term is doing the work -- H4's null, tested
    // here rather than after implementation.
    println!("\n  CONTROLS (which term earns its keep?)");
    println!(
        "    variance only, no extraction   (e=0.00, v=0.6): {} layers",
        run_cell(base, 0.0, 0.6, regen)
    );
    println!(
        "    extraction only, no variance   (e=0.04, v=0.0): {} layers",
        run_cell(base, 0.04, 0.0, regen)
    );
    println!(
        "    both                           (e=0.04, v=0.6): {} layers",
        run_cell(base, 0.04, 0.6, regen)
    );
    println!(
        "    neither                        (e=0.00, v=0.0): {} layers",
        run_cell(base, 0.0, 0.0, regen)
    );
    println!(
        "\n  If 'variance only' already reaches >= 4, the capital account is\n\
         \x20 DECORATION and §3.1 should be cut -- exactly H4's first null."
    );
}

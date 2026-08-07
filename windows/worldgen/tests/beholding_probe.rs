//! SPEC-TIME PROBE, not a shipped test. Measures whether a candidate
//! per-species observer derivation actually DIFFERENTIATES the axis it
//! claims to — before the claim goes into a spec.
//!
//! Run: cargo test -p hornvale-worldgen --test beholding_probe -- --nocapture

use hornvale_kernel::color::{BANDS, Illuminant, Observer, Reflectance, Signal, Spectrum};
use hornvale_language::exemplars::{HUE_CONCEPTS, hue_exemplar};

/// The standard observer's four authored curves, copied from
/// `kernel/src/color.rs::standard_observer` (which does not expose them).
fn standard_curves() -> [[f64; BANDS]; 4] {
    [
        [0.00, 0.25, 1.00, 0.62, 0.10, 0.01, 0.00, 0.00, 0.00, 0.00], // short
        [0.00, 0.01, 0.10, 0.45, 0.90, 1.00, 0.72, 0.28, 0.05, 0.00], // medium
        [0.00, 0.01, 0.06, 0.25, 0.60, 0.92, 1.00, 0.75, 0.30, 0.06], // long
        [0.00, 0.15, 0.55, 0.95, 1.00, 0.68, 0.25, 0.05, 0.00, 0.00], // scotopic
    ]
}

fn pack_hue_depth(night_vision: f64) -> u8 {
    2 + ((1.0 - night_vision) * 3.0).round() as u8
}

/// The CANDIDATE derivation under test. Channel count is read off
/// `pack_depths`' own hue ladder so the two cannot disagree by construction.
fn candidate_observer(night_vision: f64) -> (Observer, Vec<usize>, &'static str) {
    let [short, medium, long, scotopic] = standard_curves();
    let hue = pack_hue_depth(night_vision);
    match hue {
        // Full trichromat: the standard observer, unchanged.
        5 => (
            Observer::new(vec![
                Spectrum::new(short).unwrap(),
                Spectrum::new(medium).unwrap(),
                Spectrum::new(long).unwrap(),
                Spectrum::new(scotopic).unwrap(),
            ])
            .unwrap(),
            vec![2, 1, 0],
            "trichromat",
        ),
        // Anomalous trichromat: medium and long pulled halfway together, so
        // red-green separation narrows without vanishing.
        4 => {
            let mut m2 = [0.0; BANDS];
            let mut l2 = [0.0; BANDS];
            for b in 0..BANDS {
                m2[b] = 0.5 * medium[b] + 0.5 * ((medium[b] + long[b]) * 0.5);
                l2[b] = 0.5 * long[b] + 0.5 * ((medium[b] + long[b]) * 0.5);
            }
            (
                Observer::new(vec![
                    Spectrum::new(short).unwrap(),
                    Spectrum::new(m2).unwrap(),
                    Spectrum::new(l2).unwrap(),
                    Spectrum::new(scotopic).unwrap(),
                ])
                .unwrap(),
                vec![2, 1, 0],
                "anomalous trichromat",
            )
        }
        // Dichromat: medium and long merged into one channel. The merge is
        // CONTINUOUS in night_vision rather than keyed on the hue tier, so
        // two species that share a tier do not share an eye.
        _ => {
            // 0.5 at the tier boundary, 1.0 at fully rod-dominant sight.
            let t = ((night_vision - 0.5) / 0.5).clamp(0.0, 1.0);
            let mut merged = [0.0; BANDS];
            for b in 0..BANDS {
                let full = (medium[b] + long[b]) * 0.5;
                merged[b] = (1.0 - t) * long[b] + t * full;
            }
            (
                Observer::new(vec![
                    Spectrum::new(short).unwrap(),
                    Spectrum::new(merged).unwrap(),
                    Spectrum::new(scotopic).unwrap(),
                ])
                .unwrap(),
                // R and G both driven by the merged channel; B by short.
                vec![1, 1, 0],
                "dichromat",
            )
        }
    }
}

fn flat_light() -> Illuminant {
    Illuminant::new([1.0; BANDS]).unwrap()
}

/// Project a signal with an explicitly declared channel assignment,
/// normalizing each output by that channel's own unit-surface response.
fn project(obs: &Observer, sig: &Signal, rgb: &[usize], norms: &[f64]) -> [u8; 3] {
    let mut out = [0u8; 3];
    let _ = obs;
    for (i, slot) in out.iter_mut().enumerate() {
        let linear = (sig.get()[rgb[i]] / norms[rgb[i]]).clamp(0.0, 1.0);
        let encoded = if linear <= 0.003_130_8 {
            12.92 * linear
        } else {
            1.055 * hornvale_kernel::math::powf(linear, 1.0 / 2.4) - 0.055
        };
        *slot = (encoded.clamp(0.0, 1.0) * 255.0).round() as u8;
    }
    out
}

fn norms_of(obs: &Observer) -> Vec<f64> {
    let white = Reflectance::new([1.0; BANDS]).unwrap();
    obs.sense(&white, &flat_light()).get().to_vec()
}

#[test]
fn probe_does_the_observer_model_differentiate_the_axis() {
    let roster: [(&str, f64); 5] = [
        ("human", 0.15),
        ("goblin", 0.50),
        ("hobgoblin", 0.60),
        ("bugbear", 0.70),
        ("kobold", 0.90),
    ];
    let light = flat_light();

    for (species, nv) in roster {
        let (obs, rgb, shape) = candidate_observer(nv);
        let norms = norms_of(&obs);
        println!(
            "\n=== {species} (night_vision {nv}, pack hue depth {}) -> {shape}, {} channels",
            pack_hue_depth(nv),
            obs.channels()
        );

        // Swatches, as truecolor.
        let mut sigs: Vec<(&str, Signal, [u8; 3])> = Vec::new();
        for c in HUE_CONCEPTS {
            let r = hue_exemplar(c).unwrap();
            let s = obs.sense(&r, &light);
            let px = project(&obs, &s, &rgb, &norms);
            sigs.push((c, s, px));
        }
        let mut line = String::new();
        for (c, _, px) in &sigs {
            line.push_str(&format!(
                "\u{1b}[48;2;{};{};{}m  \u{1b}[0m {c:<7}",
                px[0], px[1], px[2]
            ));
        }
        println!("{line}");

        // Pairwise separation, each observer normalized by its OWN maximum
        // so spaces of different dimension are comparable.
        let mut max = 0.0f64;
        let mut pairs: Vec<(&str, &str, f64)> = Vec::new();
        for (i, a) in sigs.iter().enumerate() {
            for b in sigs.iter().skip(i + 1) {
                let d = a.1.distance_to(&b.1);
                if d > max {
                    max = d;
                }
                pairs.push((a.0, b.0, d));
            }
        }
        let mut rel: Vec<(String, f64)> = pairs
            .iter()
            .map(|(a, b, d)| (format!("{a}/{b}"), d / max))
            .collect();
        rel.sort_by(|x, y| x.1.total_cmp(&y.1));
        let closest: Vec<String> = rel
            .iter()
            .take(4)
            .map(|(n, v)| format!("{n} {v:.3}"))
            .collect();
        println!("  closest pairs (relative): {}", closest.join("  "));
    }
}

#[test]
fn probe_which_pairs_collapse_for_a_bugbear_that_a_human_separates() {
    let light = flat_light();
    let (human, _, _) = candidate_observer(0.15);
    let (bugbear, _, _) = candidate_observer(0.70);

    let sig = |o: &Observer, c: &str| o.sense(&hue_exemplar(c).unwrap(), &light);

    // CHROMATICITY, not raw signal. The first run of this probe showed raw
    // `Signal::distance_to` is dominated by BRIGHTNESS: red/green came out
    // at 0.025 relative separation for a full trichromat, which is absurd.
    // Dividing each channel by the signal's own total removes the luminance
    // axis and leaves the colour axis, which is the thing an observer swap
    // is claimed to move.
    let chroma = |o: &Observer, c: &str| -> Vec<f64> {
        let s = sig(o, c);
        let total: f64 = s.get().iter().sum();
        s.get().iter().map(|v| v / total).collect()
    };
    let cdist = |a: &[f64], b: &[f64]| -> f64 {
        a.iter().zip(b).map(|(x, y)| (x - y) * (x - y)).sum::<f64>()
    };
    let norm = |o: &Observer| -> f64 {
        let mut max = 0.0f64;
        for (i, a) in HUE_CONCEPTS.iter().enumerate() {
            for b in HUE_CONCEPTS.iter().skip(i + 1) {
                let d = cdist(&chroma(o, a), &chroma(o, b));
                if d > max {
                    max = d;
                }
            }
        }
        max
    };
    let hn = norm(&human);
    let bn = norm(&bugbear);

    println!("\n=== CHROMATICITY separation: pairs a human separates that a bugbear does not");
    let mut rows: Vec<(f64, String)> = Vec::new();
    for (i, a) in HUE_CONCEPTS.iter().enumerate() {
        for b in HUE_CONCEPTS.iter().skip(i + 1) {
            let (a, b) = (*a, *b);
            let h = cdist(&chroma(&human, a), &chroma(&human, b)) / hn;
            let g = cdist(&chroma(&bugbear, a), &chroma(&bugbear, b)) / bn;
            rows.push((h - g, format!("{a:<7}/{b:<7} human {h:.3}  bugbear {g:.3}")));
        }
    }
    rows.sort_by(|x, y| y.0.total_cmp(&x.0));
    for (delta, row) in rows.iter().take(8) {
        println!("  {row}   (lost {delta:.3})");
    }
    println!("\n=== and the reverse: pairs a bugbear separates MORE than a human");
    for (delta, row) in rows.iter().rev().take(3) {
        println!("  {row}   (gained {:.3})", -delta);
    }
}

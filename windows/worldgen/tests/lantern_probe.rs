//! SPEC-TIME PROBE for The Lantern, not a shipped test. Two questions,
//! answered before the spec is written:
//!   1. Do a torch, a hearth, lava and fungi actually give a stone wall
//!      visibly different colours?
//!   2. In near-darkness, does a rod-dominant eye (kobold, night vision 0.9)
//!      SEE anything a human does not? The Beholding built the achromatic
//!      channel and never cashed it.
//!
//! Run: cargo test -p hornvale-worldgen --test lantern_probe -- --nocapture

use hornvale_kernel::color::{BAND_CENTERS_NM, BANDS, Illuminant, Observer, Reflectance};
use hornvale_species::perception_registry;
use hornvale_worldgen::observer::observer_for;

/// Planck's law up to a constant — reimplemented here because
/// `astronomy::illuminant::planck_relative` is PRIVATE. Exposing it is one
/// of the campaign's cheapest steps; this probe stands in for it.
fn planck_relative(wavelength_nm: f64, t_kelvin: f64) -> f64 {
    const C2_NM_K: f64 = 1.438_776_877e7;
    let l5 = wavelength_nm.powi(5);
    let x = C2_NM_K / (wavelength_nm * t_kelvin);
    1.0 / (l5 * (hornvale_kernel::math::exp(x) - 1.0))
}

/// A blackbody emitter normalized so its brightest band is `peak`.
fn blackbody(t_kelvin: f64, peak: f64) -> Illuminant {
    let mut b = [0.0f64; BANDS];
    let mut max = 0.0f64;
    for (i, c) in BAND_CENTERS_NM.iter().enumerate() {
        b[i] = planck_relative(*c, t_kelvin);
        if b[i] > max {
            max = b[i];
        }
    }
    for v in b.iter_mut() {
        *v = *v / max * peak;
    }
    Illuminant::new(b).unwrap()
}

/// Bioluminescence: a narrow emission near 490 nm. NOT a blackbody — this is
/// why `Illuminant` being a full spectrum rather than a temperature matters.
fn fungal(peak: f64) -> Illuminant {
    // Bands are 360,400,440,480,520,560,600,640,680,720.
    let shape = [0.0, 0.05, 0.45, 1.00, 0.60, 0.12, 0.02, 0.0, 0.0, 0.0];
    let mut b = [0.0f64; BANDS];
    for (i, s) in shape.iter().enumerate() {
        b[i] = s * peak;
    }
    Illuminant::new(b).unwrap()
}

/// A pale limestone wall — the canonical cave surface.
const LIMESTONE: [f64; BANDS] = [0.55, 0.68, 0.76, 0.80, 0.82, 0.83, 0.84, 0.84, 0.85, 0.85];

fn eye(species: &'static str) -> Observer {
    let reg = perception_registry();
    observer_for(reg.get(&hornvale_species::KindId(species)).unwrap())
}

fn swatch(o: &Observer, r: &Reflectance, l: &Illuminant) -> [u8; 3] {
    o.to_srgb(&o.sense(r, l)).expect("a derived eye projects")
}

#[test]
fn probe_do_the_lights_differ() {
    let human = eye("human");
    let wall = Reflectance::new(LIMESTONE).unwrap();

    println!("\n=== a limestone wall under five lights, human eye (peak 1.0)");
    for (name, light) in [
        ("daylight  5800K", blackbody(5800.0, 1.0)),
        ("torch     1900K", blackbody(1900.0, 1.0)),
        ("hearth    1200K", blackbody(1200.0, 1.0)),
        ("lava      1100K", blackbody(1100.0, 1.0)),
        ("fungi   ~490nm ", fungal(1.0)),
    ] {
        let px = swatch(&human, &wall, &light);
        println!(
            "  \u{1b}[48;2;{};{};{}m      \u{1b}[0m  {name}  {px:?}",
            px[0], px[1], px[2]
        );
    }
}

#[test]
fn probe_does_night_vision_cash_out_in_the_dark() {
    let human = eye("human");
    let kobold = eye("kobold");
    let wall = Reflectance::new(LIMESTONE).unwrap();

    println!("\n=== THE CRITICAL QUESTION: does a rod-dominant eye see in the dark?");
    println!(
        "  Channel counts: human {} ch, kobold {} ch",
        human.channels(),
        kobold.channels()
    );
    println!(
        "  Achromatic channels: human {}, kobold {}",
        human.channels() - human.chromatic_channels(),
        kobold.channels() - kobold.chromatic_channels()
    );
    println!("\n  light      human sRGB        kobold sRGB      human rod   kobold rod");
    for level in [1.0, 0.1, 0.01, 0.003, 0.001] {
        let l = blackbody(1900.0, level);
        let (ph, pk) = (swatch(&human, &wall, &l), swatch(&kobold, &wall, &l));
        // The rod is the LAST channel in both eyes, by construction.
        let sh = human.sense(&wall, &l);
        let sk = kobold.sense(&wall, &l);
        let rh = sh.get()[sh.get().len() - 1];
        let rk = sk.get()[sk.get().len() - 1];
        println!("  {level:>6.3}    {ph:>15?}   {pk:>15?}    {rh:>8.4}   {rk:>8.4}");
    }
    println!(
        "\n  If the two sRGB columns go black together while the rod columns\n  \
         differ, then the rod is NOT reaching the projection and 'night\n  \
         vision matters' needs a design addition, not just a dark scene."
    );
}

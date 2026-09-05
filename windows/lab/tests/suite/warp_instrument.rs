//! The Warp, Task 5: the instrument's own properties, on seed 42 — every
//! claim about the instrument, none about the world (those are Task 7's).
//!
//! The instrument is the legibility readout of spec §5: for each derived
//! kind, how much the words a walker is TOLD at a facet say about whether
//! that kind occurs there, every reading paired with a five-shift
//! permutation null so a rich sign tuple's finite-sample bias is subtracted
//! rather than reported as legibility.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{BuiltView, ClimateView, MetricValue, registry};

/// The eight registered readout families (spec §5.2, as amended at G4).
const FAMILIES: [&str; 8] = [
    "channel-mi",
    "channel-null",
    "found-fraction",
    "best-lift",
    "learner-gain",
    "false-sign-net",
    "oracle-gain",
    "max-class-rate",
];

/// The four `hornvale_worldgen::WeftKind::ALL` suffixes, in slot order.
const KINDS: [&str; 4] = ["spring", "overhang", "thicket", "erratic"];

// THE TWO H4 BARS BELOW ARE FOUR NULL STANDARD DEVIATIONS EACH, AND BOTH ARE
// DERIVED RATHER THAN CHOSEN. The plug-in mutual-information estimator's
// value under independence is `chi2 / (2 n ln 2)` with `(K - 1)` degrees of
// freedom, so at seed 42's `n = 11,218` land facets it has
//
//   mean = (K - 1) / (2 n ln 2)        sd = sqrt(2 (K - 1)) / (2 n ln 2)
//
//   sign tuple `(biome, rock, steep, wet)`, K = 469 occupied classes:
//       mean 0.030091 bits, sd 0.001967 bits
//   false-sign tuple `(relief, aspect, openness)`, K = 27:
//       mean 0.001672 bits, sd 0.000464 bits
//
// The predicted mean for the sign tuple is, to six decimal places, exactly
// what `warp-channel-null-erratic` measures (0.03009126) — which is the
// positive control on the whole null construction: the five-shift
// permutation is measuring the estimator's finite-sample bias and nothing
// else. It also means a NET reading has a noise floor of about +/- 0.002
// bits at the sign tuple and +/- 0.0005 at the false-sign tuple BY
// CONSTRUCTION, so the +/- 0.001 H4 was originally preregistered at — a
// figure taken from the pre-spec probe's PER-AXIS false-sign readings
// (<= 0.0002 each), never from the joints measured here — sits below what
// the instrument can resolve. Spec §7's H4 has been amended to the numbers
// below (controller ruling, ledger #10, dated before any readout seed was
// built); these constants are the spec, not a deviation from it.

/// Spec §7's H4 bar on the erratic's channel reading net of its null, in
/// bits: four standard deviations of the sign tuple's own null estimator.
/// ONE-SIDED, as H4 writes it — the failure guarded is the instrument
/// CREDITING its negative control, and a reading below its own null credits
/// nothing.
const WARP_CREDIT_CEILING_BITS: f64 = 0.008;

/// The other side of the same reading, and NOT a second copy of the bar
/// above: a net far below the null would mean the shift construction had
/// stopped being a permutation of the same marginals, which is a defect in
/// the instrument rather than a legible world. Four standard deviations
/// again, so the two sides are symmetric in sd even though H4 gates only
/// one of them.
const WARP_SIGN_TUPLE_NOISE_FLOOR_BITS: f64 = 0.008;

/// Spec §7's H4 bar on every kind's false-sign net, in bits, TWO-SIDED as H4
/// writes it: four standard deviations of the false-sign tuple's own null
/// estimator (sd 0.000464).
const WARP_FALSE_SIGN_NOISE_FLOOR_BITS: f64 = 0.002;

fn read(built: &BuiltView, name: &str) -> MetricValue {
    registry()
        .into_iter()
        .find(|m| m.name == name)
        .unwrap_or_else(|| panic!("{name} registered"))
        .extract
        .apply(built)
}

fn number(built: &BuiltView, name: &str) -> f64 {
    match read(built, name) {
        MetricValue::Number(n) => n,
        other => panic!("{name}: {other:?}"),
    }
}

#[test]
fn all_thirty_two_warp_metrics_are_registered() {
    let names: std::collections::BTreeSet<&str> = registry().into_iter().map(|m| m.name).collect();
    let mut n = 0;
    for family in FAMILIES {
        for kind in KINDS {
            let name = format!("warp-{family}-{kind}");
            assert!(names.contains(name.as_str()), "{name} missing");
            n += 1;
        }
    }
    assert_eq!(n, 32, "eight families x four kinds");
}

/// claim: invariant(instrument) — the erratic's cause is a constant, so its
/// channel reading net of null must be ~0 and its found fraction Absent;
/// the false signs read at their nulls for every kind. These hold on ANY
/// world by construction, so seed 42 is a representative, not a sample.
#[test]
#[ignore = "probe: builds one seed-42 Climate-rung world and reads the 32 warp-* metrics (one grid sweep, ~1 s release)"]
fn the_instrument_credits_nothing_to_noise() {
    let view = ClimateView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    let built = BuiltView::Climate(view);

    // The whole readout, printed as a table so the campaign's report can
    // quote seed 42's values without a second run.
    println!(
        "{:<16} {:>14} {:>14} {:>14} {:>14}",
        "family", "spring", "overhang", "thicket", "erratic"
    );
    for family in FAMILIES {
        let row: Vec<String> = KINDS
            .iter()
            .map(
                |kind| match read(&built, &format!("warp-{family}-{kind}")) {
                    MetricValue::Number(n) => format!("{n:.8}"),
                    MetricValue::Absent => "Absent".to_string(),
                    other => format!("{other:?}"),
                },
            )
            .collect();
        println!(
            "{:<16} {:>14} {:>14} {:>14} {:>14}",
            family, row[0], row[1], row[2], row[3]
        );
    }

    let erratic_net =
        number(&built, "warp-channel-mi-erratic") - number(&built, "warp-channel-null-erratic");
    // ONE-SIDED, AND THE ASYMMETRY IS THE SPEC'S OWN (§7, H4): the erratic's
    // channel net is bounded above, the false-sign nets are bounded on both
    // sides. The failure H4 guards is the instrument CREDITING noise, and a
    // reading below its own null credits nothing. Seed 42 reads -0.00170943,
    // which is 0.87 of the null estimator's own standard deviation (see the
    // derivation at the constants above).
    assert!(
        erratic_net <= WARP_CREDIT_CEILING_BITS,
        "erratic channel net {erratic_net}: the instrument credits its own negative control"
    );
    // The other side catches the failure H4 does not gate but the instrument
    // must still refuse: a null far above the real pairing, which a
    // permutation holding both marginals cannot produce by chance.
    assert!(
        erratic_net >= -WARP_SIGN_TUPLE_NOISE_FLOOR_BITS,
        "erratic channel net {erratic_net}: the null is far above the real pairing, which a \
         permutation of the same marginals cannot be by chance — check the shift construction"
    );
    assert!(matches!(
        read(&built, "warp-found-fraction-erratic"),
        MetricValue::Absent
    ));
    for kind in KINDS {
        let fs = number(&built, &format!("warp-false-sign-net-{kind}"));
        // Four sd of the false-sign tuple's own estimator (sd 0.000464), per
        // the derivation at the constants above. Seed 42's erratic reads
        // -0.00100025, which is 2.16 sd — conservative, and 0.25 parts per
        // thousand outside the +/- 0.001 the spec carried before this
        // campaign measured the joint.
        assert!(
            fs.abs() <= WARP_FALSE_SIGN_NOISE_FLOOR_BITS,
            "{kind}: false signs read {fs} bits net of null"
        );
    }
    // The null is a permutation: it must preserve H(Y), so MI <= H(Y) for
    // every shift.
    for kind in ["spring", "overhang", "thicket"] {
        let mi = number(&built, &format!("warp-channel-mi-{kind}"));
        let null = number(&built, &format!("warp-channel-null-{kind}"));
        assert!(
            null >= 0.0 && null < mi + 0.05,
            "{kind}: null {null} vs mi {mi}"
        );
    }
}

/// The sign tuple the instrument tabulates is the tuple the prose renders.
#[test]
#[ignore = "probe: describes every 79th seed-42 geosphere vertex (519 rooms, ~140 of them land-eligible) and compares each rendered word with the instrument's sign (one world build)"]
#[allow(clippy::disallowed_methods)]
fn the_instruments_signs_are_the_prose_words() {
    use hornvale_kernel::{Facet, Vertex, WorldTime};
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate");
    let ctx = hornvale_locale::LocaleContext::build_from(&world, &terrain, &climate);
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let geo = climate.geosphere();
    let depth = geo.depth() + 7;
    let mut n = 0;
    let mut wet_seen = 0;
    // Every 79th vertex, not the Weft's own 137: only ~27% of the globe is
    // land-eligible, so a 299-vertex sample yields 86 rooms and cannot meet
    // the 100-room fixture check below. 519 candidates yield ~140.
    for v in (0..geo.vertex_count()).step_by(79) {
        let facet = Facet::containing(geo.position(Vertex(v as u32)), depth);
        let Some(weights) = facet.corner_weights(geo, ctx.nearest_index()) else {
            continue;
        };
        if hornvale_kernel::blend_corner_weights(weights, &pack.land) < 0.5 {
            continue;
        }
        let loc = ctx.describe(&facet, WorldTime::GENESIS).expect("describe");
        let tuple = hornvale_lab::warp_sign_tuple_for_test(
            &terrain,
            &climate,
            ctx.nearest_index(),
            world.seed,
            &facet,
            &weights,
            &pack,
        );
        assert_eq!(
            loc.biome,
            hornvale_locale::biome_prose_name(tuple.biome),
            "biome word"
        );
        // THE SIGN IS ONE CUT; THE WORD IS FOUR VOCABULARIES. Task 5's brief
        // asserted `descriptor.contains("damp"|"dry")`, which is the
        // OVERWORLD's clause only — `windows/locale/src/grammar.rs` switches
        // on the room's medium and formation, so an ice sheet reads "drifted
        // deep"/"scoured bare", a rock passage "weeping with
        // seep-water"/"bone dry", and water "swept by a current"/"in slack
        // water". Seed 42 has ice rooms in this sample and the brief's form
        // reddened on one ("wind-carved sastrugi, in blue shadow, scoured
        // bare, in a hollow"). What Task 1 made single is the CUT — every one
        // of those four arms calls the same threshold — so the claim this
        // test can make is that the sign agrees with whichever of the four
        // vocabularies the room renders in.
        let wet_words: &[&str] = match tuple.wet {
            hornvale_worldgen::Wetness::Damp => &[
                "damp",
                "drifted deep",
                "weeping with seep-water",
                "swept by a current",
            ],
            hornvale_worldgen::Wetness::Dry => {
                &["dry", "scoured bare", "bone dry", "in slack water"]
            }
            hornvale_worldgen::Wetness::Mid => &[],
        };
        if !wet_words.is_empty() {
            assert!(
                wet_words.iter().any(|w| loc.regime.descriptor.contains(w)),
                "{:?} carries none of {wet_words:?} for {:?}",
                loc.regime.descriptor,
                tuple.wet
            );
            wet_seen += 1;
        }
        n += 1;
    }
    assert!(n >= 100, "fixture check: {n} rooms");
    // Without this the wetness half is vacuous on a world whose sampled rooms
    // all read `Mid`, and a vacuous half reads exactly like a passing one.
    assert!(
        wet_seen > 0,
        "fixture check: no room rendered a wetness word"
    );
}

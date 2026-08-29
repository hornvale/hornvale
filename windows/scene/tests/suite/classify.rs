// FLAT import, not `hornvale_scene::classify::...` — the crate glob-
// re-exports its private modules, and its existing tests import this way.
use hornvale_scene::{ELEVATION_LEGEND, elevation_band};

#[test]
fn the_elevation_band_is_monotone_in_elevation() {
    // The ORDINAL property, which is the whole reason a ladder reads without
    // a legend (decision NNNN). If this is not monotone, ink density stops
    // meaning "higher" and the ladder becomes a nominal set — the thing
    // CLIENT-glyphs-22-rejected rejected.
    let sea = 0.0;
    let mut last = elevation_band(-5000.0, sea);
    for m in (-5000..=9000).step_by(50) {
        let b = elevation_band(f64::from(m), sea);
        assert!(
            b >= last,
            "band fell from {last} to {b} at {m} m — the ladder is not ordinal"
        );
        last = b;
    }
}

#[test]
fn the_elevation_band_spans_its_whole_legend() {
    // A ladder whose top rung is unreachable has fewer rungs than it claims,
    // and the specimen sheet would be measuring a fiction.
    let sea = 0.0;
    let seen: std::collections::BTreeSet<u8> = (-5000..=9000)
        .step_by(10)
        .map(|m| elevation_band(f64::from(m), sea))
        .collect();
    assert_eq!(
        seen.len(),
        ELEVATION_LEGEND.len(),
        "bands actually reached: {seen:?} against {} legend entries",
        ELEVATION_LEGEND.len()
    );
}

#[test]
fn sea_level_is_the_datum_not_the_number_zero() {
    // A world whose sea level is not 0 m must band identically to one whose
    // is, for the same HEIGHT ABOVE SEA. Hard-coding 0.0 passes every seed-42
    // test and silently mis-bands every other world.
    for offset in [-800.0, 0.0, 1200.0] {
        assert_eq!(
            elevation_band(offset + 500.0, offset),
            elevation_band(500.0, 0.0),
            "banding moved when sea level moved to {offset}"
        );
    }
}

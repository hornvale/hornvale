//! The documented prevailing-wind derivation (scene/tiles/v1 schema page
//! §3.2) restated and pinned against domains/climate's circulation model.
//! If the wind model changes, this fails and names the contract break.

use hornvale_climate::circulation::{band_index, is_rising_band};

/// The schema page's documented band function, restated verbatim.
fn documented_band(latitude_deg: f64, bands: u32) -> u32 {
    let width = 90.0 / f64::from(bands);
    ((latitude_deg.abs() / width) as u32).min(bands - 1)
}

#[test]
fn documented_band_matches_circulation() {
    for &bands in &[1_u32, 3, 5, 7] {
        for lat in [-89.0, -60.0, -30.0, -1.0, 0.0, 1.0, 30.0, 60.0, 89.0] {
            assert_eq!(
                documented_band(lat, bands),
                band_index(lat, bands),
                "band mismatch at lat {lat}, bands {bands}"
            );
        }
    }
    // Parity → direction: even bands easterly (rising/wet), odd westerly.
    assert!(is_rising_band(0));
    assert!(!is_rising_band(1));
}

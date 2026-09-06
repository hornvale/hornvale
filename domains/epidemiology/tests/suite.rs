use hornvale_epidemiology::{
    CCS_TROUGH, OutbreakOutcome, critical_community_size, outbreak, persists, wave_reach,
};

#[test]
fn critical_size_is_anchor_derived_and_in_expected_bands() {
    assert!((165.0..=175.0).contains(&CCS_TROUGH));
    let measles = critical_community_size(15.0, 8.0 / 365.25, 1.0 / 30.0);
    assert!((249_999.0..=250_001.0).contains(&measles));
    assert!((3_000.0..=5_000.0).contains(&critical_community_size(3.0, 2.0, 1.0 / 30.0)));
    assert!((200_000.0..=400_000.0).contains(&critical_community_size(3.0, 0.027, 1.0 / 30.0)));
    assert!((120_000.0..=200_000.0).contains(&critical_community_size(6.0, 0.038, 1.0 / 30.0)));
}

#[test]
fn critical_size_decreases_with_each_transmission_parameter() {
    let base = critical_community_size(3.0, 2.0, 1.0 / 30.0);
    assert!(critical_community_size(4.0, 2.0, 1.0 / 30.0) < base);
    assert!(critical_community_size(3.0, 3.0, 1.0 / 30.0) < base);
    assert!(critical_community_size(3.0, 2.0, 1.0 / 20.0) < base);
}

#[test]
fn persistence_has_a_strict_threshold() {
    let ccs = critical_community_size(3.0, 2.0, 1.0 / 30.0);
    assert!(persists(2.0 * ccs, ccs));
    assert!(!persists(0.5 * ccs, ccs));
    assert!(!persists(ccs, ccs));
}

#[test]
fn wave_reach_is_bounded_and_deterministic() {
    let occupied = [
        (10, &[11, 12][..]),
        (11, &[10, 13][..]),
        (12, &[10][..]),
        (13, &[11, 14][..]),
        (14, &[13, 99][..]),
        (99, &[14][..]),
    ];
    assert_eq!(wave_reach(10, &occupied, 0), vec![10]);
    assert_eq!(wave_reach(10, &occupied, 2), vec![10, 11, 12, 13]);
}

#[test]
fn outbreak_arithmetic_and_plague_threshold_are_exact() {
    let result: OutbreakOutcome = outbreak(100.0, 0.5, 0.8, 0.75, 0.30);
    assert!((result.deaths - 30.0).abs() < 1e-12);
    assert!((result.population_after - 70.0).abs() < 1e-12);
    assert!(result.ends_as_plague);

    let below = outbreak(100.0, 0.5, 0.8, 0.749, 0.30);
    assert!(!below.ends_as_plague);
}

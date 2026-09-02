//! Integration: same tree -> byte-identical roster; no timestamps. Mirrors
//! `tools/type-audit/tests/report_determinism.rs`.

use placement_audit::detect::twins;
use placement_audit::report::render_report;
use placement_audit::walk::scan;
use std::path::PathBuf;

fn fixture_src(side: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/verdicts/domains")
        .join(side)
        .join("src/lib.rs")
}

#[test]
fn roster_is_deterministic_and_carries_no_timestamps() {
    let render = || {
        let crates = scan(&[fixture_src("a"), fixture_src("b")]).unwrap();
        render_report(&twins(&crates))
    };
    let a = render();
    let b = render();
    assert_eq!(a, b, "roster must be byte-identical across runs");
    assert!(!a.contains("2026"));
    assert!(a.contains("# Placement Audit Roster"));
    assert!(a.contains("deliberate(kept apart on purpose, for the fixture)"));
}

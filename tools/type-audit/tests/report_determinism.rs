//! Integration: same tree → byte-identical report; content sanity.

use std::path::PathBuf;
use type_audit::report::render_report;
use type_audit::walk::scan;

fn mini() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/mini/good.rs")
}

#[test]
fn report_is_deterministic_and_counts_the_good_fixture() {
    let a = render_report(&scan(&[mini()]).unwrap());
    let b = render_report(&scan(&[mini()]).unwrap());
    assert_eq!(a, b, "report must be byte-identical across runs");
    assert!(a.contains("bare-ok(count)"));
    assert!(!a.contains("2026")); // no timestamps
}

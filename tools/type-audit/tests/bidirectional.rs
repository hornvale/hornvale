//! Integration: on a fixture tree, `check` passes iff tags exactly cover
//! audited positions.

use std::path::PathBuf;
use type_audit::audit::audit_items;
use type_audit::walk::scan;

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/mini")
        .join(name)
}

#[test]
fn good_file_has_no_findings_bad_file_does() {
    let good = scan(&[fixture("good.rs")]).unwrap();
    let good_findings: Vec<_> = good.iter().flat_map(|c| audit_items(&c.items)).collect();
    assert!(good_findings.is_empty(), "unexpected: {good_findings:?}");

    let bad = scan(&[fixture("bad.rs")]).unwrap();
    let bad_findings: Vec<_> = bad.iter().flat_map(|c| audit_items(&c.items)).collect();
    assert_eq!(bad_findings.len(), 2); // mass + return, both untagged
}

//! Integration: the detector finds an exact member-set twin across two
//! crates and refuses the near-miss.

use placement_audit::{detect, walk};
use std::path::PathBuf;

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/twins/domains")
        .join(name)
        .join("src/lib.rs")
}

#[test]
fn exact_member_sets_twin_and_near_misses_do_not() {
    let crates = walk::scan(&[fixture("a"), fixture("b"), fixture("c")]).unwrap();
    let twins = detect::twins(&crates);
    assert_eq!(twins.len(), 1, "exactly one twin group: {twins:?}");
    let names: Vec<&str> = twins[0].members.iter().map(|t| t.name.as_str()).collect();
    assert_eq!(names, ["Mood", "Temper"]);
}

//! Integration: `StreamLabel::from_static(<literal>)` outside `streams.rs`
//! is flagged; the same call inside `streams.rs`, a `dynamic(...)` call, and
//! a `from_static(NAMED_CONST)` call are all left alone.

use std::path::PathBuf;
use type_audit::walk::scan;

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stream_label")
        .join(name)
}

fn violation_count(name: &str) -> usize {
    scan(&[fixture(name)])
        .unwrap()
        .iter()
        .map(|c| c.stream_label_violations.len())
        .sum()
}

#[test]
fn inline_literal_outside_streams_rs_is_flagged() {
    assert_eq!(violation_count("inline_literal.rs"), 1);
}

#[test]
fn the_identical_call_inside_streams_rs_is_not_flagged() {
    assert_eq!(violation_count("streams.rs"), 0);
}

#[test]
fn a_dynamic_call_is_never_flagged() {
    assert_eq!(violation_count("dynamic_call.rs"), 0);
}

#[test]
fn a_named_constant_argument_is_not_flagged() {
    assert_eq!(violation_count("const_argument.rs"), 0);
}

#[test]
fn a_literal_inside_a_cfg_test_module_is_not_flagged() {
    assert_eq!(violation_count("test_module_literal.rs"), 0);
}

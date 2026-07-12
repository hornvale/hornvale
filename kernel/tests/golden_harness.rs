//! Integration tests for the golden-master accept harness (TOOL-20).
//! These exercise `check_golden` (the env-free core) directly so no test
//! ever mutates process-global environment variables.

use hornvale_kernel::golden::{GoldenOutcome, check_golden};
use std::path::PathBuf;

/// A scratch path under cargo's per-target test tmpdir.
fn scratch(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(name)
}

#[test]
fn matching_bytes_pass() {
    let path = scratch("match.txt");
    std::fs::write(&path, "alpha\n").unwrap();
    assert_eq!(
        check_golden(&path, "alpha\n", false),
        Ok(GoldenOutcome::Match)
    );
}

#[test]
fn a_mismatch_reports_the_first_divergence_and_the_accept_instruction() {
    let path = scratch("mismatch.txt");
    std::fs::write(&path, "alpha\nbeta\n").unwrap();
    let err = check_golden(&path, "alpha\ngamma\n", false)
        .expect_err("differing bytes must be a mismatch");
    assert!(err.message.contains("line 2"), "got: {}", err.message);
    assert!(err.message.contains("beta"), "got: {}", err.message);
    assert!(err.message.contains("gamma"), "got: {}", err.message);
    assert!(
        err.message.contains("REBASELINE=1"),
        "the accept instruction must be in the message: {}",
        err.message
    );
    assert!(
        err.message.contains("mismatch.txt"),
        "the fixture path must be in the message: {}",
        err.message
    );
    // A rejected mismatch must never modify the fixture.
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "alpha\nbeta\n");
}

#[test]
fn rebaseline_rewrites_a_drifted_fixture() {
    let path = scratch("rewrite.txt");
    std::fs::write(&path, "old\n").unwrap();
    assert_eq!(
        check_golden(&path, "new\n", true),
        Ok(GoldenOutcome::Rewritten)
    );
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "new\n");
}

#[test]
fn rebaseline_on_matching_bytes_is_a_no_op_match() {
    let path = scratch("noop.txt");
    std::fs::write(&path, "same\n").unwrap();
    assert_eq!(
        check_golden(&path, "same\n", true),
        Ok(GoldenOutcome::Match)
    );
}

#[test]
fn rebaseline_creates_a_missing_fixture_and_its_parents() {
    let path = scratch("created/nested/fixture.txt");
    let _ = std::fs::remove_file(&path);
    assert_eq!(
        check_golden(&path, "fresh\n", true),
        Ok(GoldenOutcome::Created)
    );
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "fresh\n");
}

#[test]
fn a_missing_fixture_without_rebaseline_says_how_to_create_it() {
    let path = scratch("never-written.txt");
    let _ = std::fs::remove_file(&path);
    let err = check_golden(&path, "x", false).expect_err("missing fixture must fail");
    assert!(err.message.contains("REBASELINE=1"), "got: {}", err.message);
}

#[test]
fn a_trailing_length_difference_is_still_a_mismatch() {
    let path = scratch("prefix.txt");
    std::fs::write(&path, "alpha\n").unwrap();
    let err = check_golden(&path, "alpha\nextra\n", false)
        .expect_err("extra trailing content must be a mismatch");
    assert!(err.message.contains("line 2"), "got: {}", err.message);
}

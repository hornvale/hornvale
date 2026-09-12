#[test]
fn astronomy_at_accepts_exact_negative_ticks() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args([
            "scene",
            "astronomy-at",
            "--world",
            "tests/fixtures/world-seed-42.json",
            "--ticks",
            "-1",
        ])
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let doc: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(doc["ticks"].as_i64(), Some(-1));
}
#[test]
fn astronomy_at_rejects_missing_fractional_and_overflow_ticks() {
    for ticks in [None, Some("1.5"), Some("9223372036854775808"), Some("nope")] {
        let mut command = std::process::Command::new(env!("CARGO_BIN_EXE_hornvale"));
        command.args(["scene", "astronomy-at"]);
        if let Some(ticks) = ticks {
            command.args(["--ticks", ticks]);
        }
        let output = command.output().unwrap();
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains("--ticks"));
    }
}

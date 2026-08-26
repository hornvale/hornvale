//! The Reticence, Task 6: a per-people readout of which improvised name each
//! culture reaches for, and what it costs a rider.

#[test]
fn the_report_covers_every_people_and_names_no_doctrine_arm() {
    let report = hornvale_lab::render_reticence_report().expect("seed 42 builds");
    let rows = report.lines().filter(|l| l.starts_with("| ")).count();
    assert!(
        rows >= 15,
        "one row per people (15 on seed 42), plus header rows; got {rows}"
    );

    // No rider concept is registered, so every people must land on one of the
    // three IMPROVISING arms. Asserting on the arm fields rather than on the
    // absence of the word "doctrine" anywhere in the file: a prose sentence
    // legitimately mentioning doctrine would fail a substring check while
    // saying nothing about the data, and that is a test that breaks for the
    // wrong reason.
    for line in report.lines().filter(|l| l.starts_with("| ")) {
        let fields: Vec<_> = line.split('|').map(str::trim).collect();
        if fields
            .iter()
            .any(|c| *c == "God" || *c == "Spirit" || c.starts_with("Wordless"))
        {
            continue;
        }
        assert!(
            fields.iter().all(|c| !c.contains("Doctrine")),
            "a people resolved to a doctrine arm, which is unreachable: {line}\n\
             If this fires, spec section 10.4 was reopened without updating H1."
        );
    }
}

/// Same standing as `render_confidant_report_is_deterministic`
/// (`windows/lab/src/metrics.rs`): this artifact is a pure function of
/// `Seed(42)`, so two calls must be byte-identical — the guarantee the drift
/// check over `docs/generated-paths.txt` relies on to make a stale committed
/// copy detectable at all.
#[test]
fn the_report_is_deterministic() {
    assert_eq!(
        hornvale_lab::render_reticence_report().unwrap(),
        hornvale_lab::render_reticence_report().unwrap()
    );
}

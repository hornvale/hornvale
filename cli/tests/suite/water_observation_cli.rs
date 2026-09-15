//! **The Waterworld observation has a caller** — The Tidemark, Task 5.
//!
//! The Living Vent authored the ordinary/diagnostic split and
//! `windows/worldgen/tests/suite/waterworld.rs` has pinned its CONTENT ever
//! since: ordinary output reports present consequences and claims no cause,
//! diagnostic output names the inferred source phase and marks it uncertain.
//!
//! What no test could pin was that anything **reached** it. `observe_waterworld`
//! and `observe_waterworld_snapshot` had no production caller at all — The
//! Tidemark's own design spec records the state of the whole overlay as "no
//! `BuildDepth` rung constructs [it], no CLI command reaches it, no window
//! renders it" — so a split nobody can reach was a discipline nobody was held
//! to. This file pins the caller, through the real binary, because that is the
//! only instrument that can see the flag actually being threaded: a `cmd_water`
//! that hardcoded `diagnostic: false` would leave every library-level test in
//! the workspace green.

use std::process::Command;

fn water(args: &[&str]) -> String {
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .arg("water")
        .args(args)
        .output()
        .expect("the hornvale binary runs");
    assert!(
        out.status.success(),
        "hornvale water {args:?} failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8(out.stdout).expect("the readout is utf-8")
}

/// The marker strings are The Living Vent's own, quoted rather than
/// paraphrased, so this test moves only when that campaign's contract does.
const INFERENCE_MARKER: &str = "phase and source are an inferred cause; uncertain at observation \
                                scale";
const PHASE_MARKER: &str = "source phase (derived, not directly observed)";
const PROVENANCE_MARKER: &str = "provenance:";

#[test]
fn ordinary_output_reports_what_the_water_is_doing_and_claims_no_cause() {
    let text = water(&["--seed", "42"]);
    assert!(
        text.contains("marine substrate:") && text.contains("present stocks:"),
        "the ordinary readout must report present consequences: {text}"
    );
    assert!(
        text.contains("vent consequence:"),
        "a vent's present CONSEQUENCE is an ordinary observation — what is absent below is its \
         inferred cause, not its effect: {text}"
    );
    for marker in [INFERENCE_MARKER, PHASE_MARKER, PROVENANCE_MARKER] {
        assert!(
            !text.contains(marker),
            "ordinary output must not name an inferred cause, but it carries {marker:?}"
        );
    }
}

#[test]
fn diagnostic_output_names_the_inferred_vent_phase_and_marks_it_uncertain() {
    // Day 40 rather than genesis: 40 days into the 100-day succession cycle
    // is inside the ACTIVE interval for an unshifted source, so the phase
    // census below has something to say. At genesis it would still print,
    // and a reader could not tell a populated census from an empty one.
    let text = water(&["--seed", "42", "--day", "40", "--diagnostic"]);
    for marker in [PHASE_MARKER, PROVENANCE_MARKER, INFERENCE_MARKER] {
        assert!(
            text.contains(marker),
            "diagnostic output must carry {marker:?}: {text}"
        );
    }
    // The five phases are named, not merely counted in aggregate — the
    // campaign's claim is that a habitat EXPIRES, and `failed` is the word
    // that says so.
    for phase in ["absent", "nascent", "active", "weakening", "failed"] {
        assert!(
            text.contains(phase),
            "the diagnostic phase census must name {phase}: {text}"
        );
    }
}

/// The instant is the caller's, and it reaches the succession. Two days far
/// enough apart to sit in different phases must not print the same report —
/// otherwise `--day` is decoration and the observation is frozen at genesis
/// whatever it says.
#[test]
fn the_named_day_reaches_the_succession() {
    let early = water(&["--seed", "42", "--day", "40", "--diagnostic"]);
    let late = water(&["--seed", "42", "--day", "90", "--diagnostic"]);
    assert_ne!(
        early, late,
        "the same overlay read at two instants must differ, or --day is not reaching \
         `WaterWorld::at`"
    );
    let again = water(&["--seed", "42", "--day", "40", "--diagnostic"]);
    assert_eq!(
        early, again,
        "and the same instant must read back byte-identically — the observation is pure"
    );
}

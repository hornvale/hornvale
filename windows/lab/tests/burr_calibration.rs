//! The Burr's preregistered calibration (spec §6), pinned per decision 0016 —
//! the instrument and its direction are frozen here BEFORE any generation
//! code moves, and the exact value is pinned from the first measured run,
//! never tuned to pass.
//!
//! **What this measures and what it does not.** Assignment accuracy is
//! *distinguishability*: can a word be traced back to its tongue. The
//! campaign's goal is aesthetic, and a tongue family can become highly
//! distinguishable and no lovelier. A green reading here is therefore never
//! on its own a claim that the campaign succeeded — see the spec's §6
//! cautions, which this file deliberately restates rather than links.
use hornvale_lab::tongue_distance::{assignment_accuracy, wordlists_from_dictionary};

/// The committed reference corpus.
const DICTIONARY: &str = "../../book/src/reference/dictionary-generated.md";

/// The pre-campaign baseline, measured 2026-08-16 on the committed
/// dictionary at `0e7d5757`. Pinned from the first run per decision 0016;
/// never tuned.
const BASELINE: f64 = 0.7201897018970189;

/// The chance floor for an 18-way assignment. Reported alongside the baseline
/// because an accuracy figure without its denominator is not interpretable.
const CHANCE_FLOOR: f64 = 1.0 / 18.0;

fn load() -> Vec<(String, Vec<String>)> {
    let md = std::fs::read_to_string(DICTIONARY).expect("read the committed dictionary");
    wordlists_from_dictionary(&md).expect("parse wordlists")
}

/// P1 (descriptive): the pre-campaign baseline, pinned exactly.
#[test]
fn the_baseline_assignment_accuracy_is_pinned() {
    let acc = assignment_accuracy(&load());
    assert!(
        (acc - BASELINE).abs() < 1e-9,
        "baseline moved: expected {BASELINE}, measured {acc}. \
         If generation code changed, this is the campaign's readout, not a \
         failure — update the pin in the SAME commit as the change that moved \
         it, and say so in the chronicle."
    );
}

/// The instrument reports its own denominator. A baseline at or below chance
/// would mean the tongues are wholly interchangeable; one near 1.0 would mean
/// the premise of this campaign is wrong and it should stop.
#[test]
fn the_baseline_sits_between_chance_and_certainty() {
    let acc = assignment_accuracy(&load());
    assert!(
        acc > CHANCE_FLOOR,
        "accuracy {acc} at or below the {CHANCE_FLOOR} chance floor — the \
         classifier is not separating anything; fix the instrument before \
         trusting any later reading"
    );
    assert!(
        acc < 0.99,
        "accuracy {acc} is near-perfect BEFORE any change — the tongues are \
         already maximally distinguishable and The Burr's premise is refuted. \
         Stop and report this as the finding."
    );
}

/// The roster the baseline was taken over, pinned so a later reading cannot
/// silently compare against a different set of tongues.
#[test]
fn the_baseline_roster_is_eighteen_tongues() {
    let lists = load();
    assert_eq!(
        lists.len(),
        18,
        "expected 18 tongues, got {}: {:?}",
        lists.len(),
        lists.iter().map(|(n, _)| n).collect::<Vec<_>>()
    );
}

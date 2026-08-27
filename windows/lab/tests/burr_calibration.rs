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

/// Stage 1 baseline: 0.7201897018970189 (18 tongues, pre-epoch), measured
/// 2026-08-16 on the committed dictionary at `0e7d5757`.
///
/// Stage 2 (trill ungated + sonorant floor, ROOT_EPOCH v4): 0.6795392953929539.
/// Accuracy **fell**, opposite the direction P2 predicts — see spec §6. The
/// floor is a *global* rule, so it pushes every quiet tongue toward holding
/// the same segment, which makes inventories more alike, not less; §3.7
/// documents why no word actually gained a liquid despite the inventory
/// change.
///
/// Stage 3 (per-bundle phonotactic law, the elf bundle): 0.7899728997289973.
/// Accuracy **rose** +0.110 above Stage 2 and +0.070 above the Stage 1
/// baseline — P2 CONFIRMED. The contrast is the campaign's thesis: a *global*
/// change (Stage 2) homogenises a diverse population, while a *per-family*
/// rule (Stage 3) differentiates it. Half of each elf lexicon now carries a
/// liquid where all four had none.
///
/// Stage 4 (root-and-pattern morphology, the dwarf family): 0.7791327913279132.
/// Accuracy **fell** −0.0108. Not a regression: the three dwarf sub-tongues
/// (desert-, gully-, hill-dwarf) all adopt the SAME templatic structure with
/// the SAME uniform citation melody (Singular, an all-`a` C-a-C-a-C shape), so
/// they become measurably more similar *to each other* even as the family as a
/// whole becomes structurally distinct from the concatenative tongues. The
/// metric rewards inter-tongue distance; a single shared template trades
/// within-family distinguishability for a distinctive family character a reader
/// registers instantly but a trigram classifier does not (the FINDING-1
/// character-vs-capacity distinction, measured again). A richer paradigm
/// (varying the melody per daughter) would differentiate them; this campaign
/// ships the citation form uniformly and reports its cost. Pinned from the
/// measured run per decision 0016; never tuned.
///
/// Stage 5 (per-bundle orthography, Task 15): 0.7913279132791328. Accuracy
/// **rose** +0.0122 back above Stage 4. The classifier reads character
/// trigrams, and orthography changes exactly those characters for the elf
/// (Diacritic: `ŋ`→`ṅ`, `ʃ`→`š`, `ʒ`→`ž`, the ejective `kʼ`→`ḳ`, the click
/// `ǃ`→`ṭ`) and dwarf (Apostrophe: a `'` before a consonant following a
/// digraph) bundles — a rendering view, not a phonological change, so this
/// move is orthogonal to Stage 4's structural one: the digraph-heavy elf
/// words the Stage 3 finding turned on (`sh`/`zh`/`ng`) become
/// single-diacritic trigrams, which is more informative per character than
/// the two-letter digraph every OTHER bundle still shares.
///
/// Absorb of main (Task 16, The Underworld and peers merged in): 0.7994579945799458.
/// Accuracy **rose** +0.0081. The Burr never changed a word after Stage 5; this
/// move is The Underworld's placement re-key (decision 0102, keying the bake to a
/// PLACE not a vertex), which shifts which concepts each tongue is exposed to and so
/// which words populate the dictionary the classifier reads — a different corpus of
/// the SAME per-family character, measured once more on the merged product. The
/// campaign's thesis is unaffected: every typological stage still raised
/// distinguishability, and the merged readout is the highest of the run. Pinned from
/// the merged run per decision 0016; never tuned.
///
/// The Confidant (Arc III of The Bridle): 0.7993421052631579. Accuracy **fell**
/// 0.0116 percentage points — the smallest move this pin has recorded. Same
/// mechanism the entry above names, from the other direction: that campaign
/// registered six felt-state concepts and gave each people a derived exposure to
/// some of them (`MindVector`-governed, at most one pole of each valence-opposed
/// pair), so six new words enter every tongue's dictionary and the classifier
/// reads a slightly larger corpus. Not a typological change and not a claim about
/// distinguishability: The Burr's thesis is untouched, and a readout that moves by
/// one part in ten thousand when the corpus grows is the pin behaving as designed.
/// Re-pinned deliberately, per this test's own instruction, and recorded in the
/// chronicle. Never tuned.
///
/// The Granary: sub-year raid timing moved settlement survival/naming; the
/// canonical census re-pin (lefford, goldens c54fb62c9) reads
/// 0.7980645161290323. Accuracy **fell** 0.0013 — the second-smallest move
/// this pin has recorded. The Granary touches no language code; the mover is
/// the same corpus effect the two entries above name — which settlements
/// survive to be named changes which words populate the dictionary the
/// classifier reads. The Burr's thesis (typological stages raise
/// distinguishability) is untouched. Re-pinned deliberately, per this test's
/// own instruction. Never tuned.
///
/// The Inquest (Task 6): 0.7978316326530612. Accuracy **fell** 0.00023 — the
/// smallest move this pin has ever recorded, and the first one whose cause is
/// legible as an exact fraction rather than inferred. The campaign registered
/// `kill` (one concept, universal stratum), so every one of the 18 daughters
/// gains one word: the readout goes from **1237/1550 to 1251/1568**. The
/// denominator grew by exactly 18 and the numerator by 14 — `kill` classified
/// correctly in 14 of the 18 tongues, marginally below the running rate, which
/// is the whole of the move. **1237/1550 is a subset of the new reading**: not
/// one pre-existing word changed corpus or classification, which is the same
/// thing the campaign's three byte-goldens show as zero-deletion diffs. Same
/// mechanism as The Confidant's entry above, one concept instead of six. The
/// Burr's thesis (typological stages raise distinguishability) is untouched.
/// Re-pinned deliberately, per this test's own instruction. Never tuned.
const BASELINE: f64 = 0.7978316326530612;

/// The chance floor for an 18-way assignment. Reported alongside the baseline
/// because an accuracy figure without its denominator is not interpretable.
const CHANCE_FLOOR: f64 = 1.0 / 18.0;

fn load() -> Vec<(String, Vec<String>)> {
    let md = std::fs::read_to_string(DICTIONARY).expect("read the committed dictionary");
    wordlists_from_dictionary(&md).expect("parse wordlists")
}

/// P1 (descriptive): the campaign's current assignment-accuracy readout,
/// pinned exactly (Stage 3 — see [`BASELINE`] for the per-stage history).
///
/// Exact equality, not a tolerance, is safe here: `assignment_accuracy`
/// computes a single IEEE-754 division of two small integers (correct
/// assignments over total words), single-threaded, with no ties
/// contributing fractional credit and no summation-order sensitivity — so
/// the result is bit-reproducible, not merely numerically close. Do not
/// widen this back into a tolerance without re-establishing that.
#[test]
fn the_baseline_assignment_accuracy_is_pinned() {
    let acc = assignment_accuracy(&load());
    assert_eq!(
        acc, BASELINE,
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

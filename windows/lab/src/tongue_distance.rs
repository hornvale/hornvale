//! The tongue classifier: how reliably can a word be assigned back to the
//! tongue that produced it? The Burr's preregistered instrument (spec §6).
//!
//! Pure functions by design — the contract is unit-tested directly, not only
//! through a calibration run, following `timings.rs`'s precedent in this
//! crate.

use std::collections::BTreeMap;

/// The character n-gram width the profile uses. Three is the standard choice
/// for language identification and is fixed rather than drawn: this is a
/// measuring instrument, and a tunable width would let a disappointing
/// baseline be rescued by retuning it.
const NGRAM: usize = 3;

/// The relative frequency of each character trigram across `words`, with
/// word boundaries marked by `^` and `$` so onset and coda shape are visible
/// to the profile. Returns an empty map for an empty input.
/// type-audit: bare-ok(identifier-text: words), bare-ok(ratio: return)
pub fn trigram_profile(words: &[String]) -> BTreeMap<String, f64> {
    let mut counts: BTreeMap<String, u64> = BTreeMap::new();
    let mut total: u64 = 0;
    for w in words {
        let padded: Vec<char> = format!("^{}$", w.to_lowercase()).chars().collect();
        if padded.len() < NGRAM {
            continue;
        }
        for window in padded.windows(NGRAM) {
            *counts.entry(window.iter().collect()).or_insert(0) += 1;
            total += 1;
        }
    }
    if total == 0 {
        return BTreeMap::new();
    }
    counts
        .into_iter()
        .map(|(k, v)| (k, v as f64 / total as f64))
        .collect()
}

/// Cosine similarity between two trigram profiles, in `[0, 1]`. Zero for a
/// pair where either side is empty.
/// type-audit: bare-ok(ratio)
fn similarity(a: &BTreeMap<String, f64>, b: &BTreeMap<String, f64>) -> f64 {
    let dot: f64 = a.iter().map(|(k, v)| b.get(k).unwrap_or(&0.0) * v).sum();
    let na: f64 = a.values().map(|v| v * v).sum::<f64>().sqrt();
    let nb: f64 = b.values().map(|v| v * v).sum::<f64>().sqrt();
    if na == 0.0 || nb == 0.0 {
        return 0.0;
    }
    dot / (na * nb)
}

/// Held-out assignment accuracy: for each word of each tongue, rebuild every
/// tongue's profile with that word withheld, then ask which profile(s) the
/// word's own trigrams sit closest to. The reported value is the mean, over
/// every word, of the fraction of the top-scoring tongues that are the
/// word's true tongue.
///
/// **Withholding is symmetric across tongues, not just the word's own.** Any
/// *other* tongue whose word list contains an exact copy of the tested word
/// has every matching entry stripped from it too (all occurrences, not just
/// one) before its profile is built. Withholding only from the word's own
/// tongue would leave a sibling tongue that happens to share the word with
/// an unwithheld literal copy — cosine similarity favors that copy
/// unconditionally, which biases assignment *away* from the true tongue
/// rather than leaving the two indistinguishable. That asymmetry is an
/// artifact of the withholding, not a fact about the languages, which is why
/// it is removed on both sides.
///
/// **A tie for the top score is scored as ambiguity, never resolved by an
/// arbitrary tie-break.** A probe tying across `k` tongues' profiles
/// contributes `1/k` credit if its true tongue is among those `k` leaders,
/// `0` otherwise. This is why two tongues built from the same word list
/// score at exactly the `k`-way chance floor (`1/k`) — every shared word
/// ties across all `k` — rather than at a number that depends on which
/// index the input happened to place a tongue at. An index-based tie-break
/// would report a *different* accuracy for the same two tongues passed in
/// the opposite order, which is not a property of the tongues.
/// type-audit: bare-ok(identifier-text: tongues), bare-ok(ratio: return)
pub fn assignment_accuracy(tongues: &[(String, Vec<String>)]) -> f64 {
    let mut correct: f64 = 0.0;
    let mut total: u64 = 0;
    for (i, (_, words)) in tongues.iter().enumerate() {
        for (w_idx, word) in words.iter().enumerate() {
            let profiles: Vec<BTreeMap<String, f64>> = tongues
                .iter()
                .enumerate()
                .map(|(j, (_, ws))| {
                    if i == j {
                        let held: Vec<String> = ws
                            .iter()
                            .enumerate()
                            .filter(|(k, _)| *k != w_idx)
                            .map(|(_, s)| s.clone())
                            .collect();
                        trigram_profile(&held)
                    } else {
                        // Strip every occurrence of the same word from a
                        // sibling tongue too — see the symmetric-withholding
                        // paragraph above.
                        let held: Vec<String> = ws.iter().filter(|s| *s != word).cloned().collect();
                        trigram_profile(&held)
                    }
                })
                .collect();
            let probe = trigram_profile(std::slice::from_ref(word));
            let scores: Vec<f64> = profiles.iter().map(|p| similarity(&probe, p)).collect();
            let best_score = scores.iter().copied().fold(f64::NEG_INFINITY, f64::max);
            // Exact equality is deliberate, not fuzzed: every score being
            // compared here came from the identical code path (`similarity`)
            // run over identical-in-content inputs, so a genuine tie is
            // bit-identical, and an `f64::EPSILON` fuzz would blur ties into
            // near-ties that are not actually the same computation.
            let tied: Vec<usize> = scores
                .iter()
                .enumerate()
                .filter(|(_, s)| **s == best_score)
                .map(|(j, _)| j)
                .collect();
            if tied.contains(&i) {
                correct += 1.0 / tied.len() as f64;
            }
            total += 1;
        }
    }
    if total == 0 {
        0.0
    } else {
        correct / total as f64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Two tongues with disjoint segment sets must be perfectly separable:
    /// this is the instrument's positive control, and it fails loudly if the
    /// profile ever stops distinguishing anything.
    #[test]
    fn disjoint_tongues_are_perfectly_assignable() {
        let tongues = vec![
            (
                "alpha".to_string(),
                vec!["baba".to_string(), "bab".to_string(), "abab".to_string()],
            ),
            (
                "beta".to_string(),
                vec!["kiki".to_string(), "kik".to_string(), "ikik".to_string()],
            ),
        ];
        assert_eq!(assignment_accuracy(&tongues), 1.0);
    }

    /// The negative control, and the more important half: two tongues drawn
    /// from the SAME word list cannot be told apart, so accuracy must sit at
    /// the chance floor (1/2 here), never above it. A classifier that scores
    /// identical inputs above chance is measuring its own leakage.
    #[test]
    fn identical_tongues_score_at_the_chance_floor() {
        let shared = vec!["tata".to_string(), "tat".to_string(), "atat".to_string()];
        let tongues = vec![
            ("alpha".to_string(), shared.clone()),
            ("beta".to_string(), shared),
        ];
        assert_eq!(assignment_accuracy(&tongues), 0.5);
    }

    /// The tie-break must not be an index accident: two tongues that share
    /// exactly one word tie on that word (and on nothing else), and the
    /// reported accuracy must come out identical whichever order the two
    /// tongues are passed in. An index-based tie-break ("later tongue wins")
    /// fails this — it silently favors whichever tongue lands at the higher
    /// slice index, so the same two tongues would score differently
    /// depending on argument order alone. Fractional credit does not have an
    /// order to depend on.
    #[test]
    fn shared_word_accuracy_is_order_independent() {
        let alpha = (
            "alpha".to_string(),
            vec!["cat".to_string(), "xqzv".to_string(), "xqzw".to_string()],
        );
        let beta = (
            "beta".to_string(),
            vec!["cat".to_string(), "wvtp".to_string(), "wvtq".to_string()],
        );
        let forward = vec![alpha.clone(), beta.clone()];
        let reversed = vec![beta, alpha];
        assert_eq!(
            assignment_accuracy(&forward),
            assignment_accuracy(&reversed)
        );
    }
}

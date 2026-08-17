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
/// tongue's profile **with that word withheld from its own tongue**, then ask
/// which profile the word's own trigrams sit closest to. The reported value is
/// the fraction assigned back correctly.
///
/// Leave-one-out is load-bearing rather than fastidious: without it a word
/// contributes to the profile it is then scored against, so a tongue with few
/// words scores near-perfectly by memorising itself, and the instrument would
/// report a high number for exactly the impoverished tongues this campaign
/// exists to fix.
///
/// Ties go to the **later** tongue in slice order, deterministically, so an
/// instrument reading identical tongues cannot drift with input ordering.
/// type-audit: bare-ok(identifier-text: tongues), bare-ok(ratio: return)
pub fn assignment_accuracy(tongues: &[(String, Vec<String>)]) -> f64 {
    let mut correct: u64 = 0;
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
                        // A different tongue that happens to carry the exact
                        // same word (the negative control constructs this
                        // deliberately) must not keep an unwithheld literal
                        // copy: doing so hands it a leaked exact match while
                        // the word's own tongue only sees the word withheld,
                        // which biases assignment *away* from the true
                        // tongue instead of leaving the two indistinguishable.
                        let held: Vec<String> = ws.iter().filter(|s| *s != word).cloned().collect();
                        trigram_profile(&held)
                    }
                })
                .collect();
            let probe = trigram_profile(std::slice::from_ref(word));
            let mut best = 0usize;
            let mut best_score = f64::NEG_INFINITY;
            for (j, p) in profiles.iter().enumerate() {
                let s = similarity(&probe, p);
                if s >= best_score {
                    best_score = s;
                    best = j;
                }
            }
            if best == i {
                correct += 1;
            }
            total += 1;
        }
    }
    if total == 0 {
        0.0
    } else {
        correct as f64 / total as f64
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
}

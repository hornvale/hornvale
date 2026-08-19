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

/// The em-dash the dictionary renders for a lexical gap. A gap is not a word.
const GAP: &str = "—";

/// Per-tongue wordlists parsed from the committed dictionary artifact
/// (`book/src/reference/dictionary-generated.md`). A `## Heading` opens a
/// tongue; a table row's **third** cell is its surface word.
///
/// Two rows are excluded and both exclusions are load-bearing. The table's own
/// `| Concept | Gloss | Word | ... |` **header** parses as a row whose third
/// cell is the literal `Word` — counting it is the defect that nearly put a
/// fabricated figure into this campaign's spec. And a `—` cell is a lexical
/// gap, not a word. A tongue left with no words is omitted entirely rather
/// than returned empty, so it cannot enter the accuracy denominator.
///
/// The `Cognates` section is skipped: its rows carry one column per daughter,
/// so its third cell is a proto form rather than one tongue's word.
///
/// **Two independent shape checks, each catching a different way the
/// artifact can break, each with its own message.** Neither is "the parse
/// produced nothing" — that conflated two failure modes in an earlier draft
/// (see the fix commit) and broke the legitimate all-gap case below.
/// - **No `## ` heading was ever seen.** The heading syntax itself changed;
///   nothing downstream can be trusted, so this errors even before any table
///   is examined.
/// - **At least one heading was seen, but no table row (inside a non-
///   `Cognates` section) ever reached the point of being cell-indexed** — a
///   row counts here the moment it starts with `|` and splits into at least
///   five `|`-delimited cells, *regardless* of whether cell three then turns
///   out to be the header text, the `---` separator, a gap, or a real word.
///   A row missing its leading `|` (or otherwise short) never reaches that
///   point and does not count, so a document whose headings survived but
///   whose table syntax broke is caught here rather than silently returning
///   `Ok(vec![])`.
///
/// **What this does NOT catch, and what does instead.** A row that keeps the
/// `| ... | ... |` shape but carries wrong *content* — a shifted column, a
/// corrupted word — still counts as a seen row and parses without error.
/// This function has no way to know a cell's content is wrong, only that its
/// shape is intact. That is Task 3's job: `the_baseline_roster_is_eighteen_
/// tongues` catches a roster that gained or lost a tongue, and
/// `the_baseline_sits_between_chance_and_certainty` catches an accuracy
/// figure a content-level corruption would produce.
///
/// A heading that *is* recognized but whose tongue turns out all-gap hits
/// neither `Err` path: its rows were seen (so `saw_row` is true), it is
/// simply omitted from the returned list (see above), and an otherwise-valid
/// document is still `Ok`, possibly with an empty list.
/// type-audit: bare-ok(prose: return), bare-ok(identifier-text: md)
pub fn wordlists_from_dictionary(md: &str) -> Result<Vec<(String, Vec<String>)>, String> {
    let mut out: Vec<(String, Vec<String>)> = Vec::new();
    let mut current: Option<(String, Vec<String>)> = None;
    let mut saw_heading = false;
    let mut saw_row = false;
    for line in md.lines() {
        if let Some(rest) = line.strip_prefix("## ") {
            saw_heading = true;
            if let Some(t) = current.take()
                && !t.1.is_empty()
            {
                out.push(t);
            }
            current = Some((rest.trim().to_string(), Vec::new()));
            continue;
        }
        let Some((name, words)) = current.as_mut() else {
            continue;
        };
        if name == "Cognates" || !line.starts_with('|') {
            continue;
        }
        let cells: Vec<&str> = line.split('|').collect();
        if cells.len() < 5 {
            continue;
        }
        saw_row = true;
        let word = cells[3].trim();
        if word.is_empty() || word == GAP || word == "Word" || word.starts_with("---") {
            continue;
        }
        words.push(word.to_string());
    }
    if let Some(t) = current
        && !t.1.is_empty()
    {
        out.push(t);
    }
    if !saw_heading {
        return Err("no `## ` heading found — the dictionary's heading shape changed".to_string());
    }
    if !saw_row {
        return Err(
            "headings found but no table rows — the dictionary's table shape changed".to_string(),
        );
    }
    Ok(out)
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

    /// The parser must key on the Word column and must NOT count the table
    /// header. This is the exact defect that nearly put a fictitious "1 liquid
    /// word per tongue" figure into the spec: `| Word |` matches a naive
    /// row filter, and a header counted as a word is indistinguishable from a
    /// real one in the output.
    #[test]
    fn the_table_header_is_not_a_word() {
        let md = "\
## Alpha

| Concept | Gloss | Word | IPA | Proto | Derivation |
|---|---|---|---|---|---|
| `fire` | flame | Baba | /baba/ | Baba | no change |
| `water` | drink | — | — | — | gap (experiential): none |
";
        let lists = wordlists_from_dictionary(md).expect("parse");
        assert_eq!(lists.len(), 1);
        assert_eq!(lists[0].0, "Alpha");
        assert_eq!(lists[0].1, vec!["Baba".to_string()]);
    }

    /// A gap renders as an em-dash and is not a word; a tongue that is all
    /// gaps contributes no wordlist at all rather than an empty one, so it
    /// cannot silently drag the accuracy denominator.
    #[test]
    fn an_all_gap_tongue_is_omitted() {
        let md = "\
## Beta

| Concept | Gloss | Word | IPA | Proto | Derivation |
|---|---|---|---|---|---|
| `fire` | flame | — | — | — | gap (experiential): none |
";
        let lists = wordlists_from_dictionary(md).expect("parse");
        assert!(
            lists.is_empty(),
            "all-gap tongue must be omitted, got {lists:?}"
        );
    }

    /// A heading that survives alongside table rows that do not (every row
    /// here is missing its leading `|`, so none ever reaches the point of
    /// being cell-indexed) must error, and the error must name the table —
    /// not the heading — shape as what broke, distinguishing it from the
    /// "no heading at all" failure mode. Asserting only `is_err()` would pass
    /// even if the two `Err` paths were swapped.
    #[test]
    fn malformed_table_rows_are_a_distinct_error() {
        let md = "\
## Gamma

Concept | Gloss | Word | IPA | Proto | Derivation |
---|---|---|---|---|---|
`fire` | flame | Baba | /baba/ | Baba | no change |
";
        let err = wordlists_from_dictionary(md).expect_err("malformed rows must error");
        assert!(
            err.contains("row"),
            "error must name the table/row shape as what broke, got: {err}"
        );
    }
}

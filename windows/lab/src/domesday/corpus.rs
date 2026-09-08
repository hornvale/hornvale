//! The Domesday's minimal view of a frozen regularity corpus.
//!
//! **A second reader of the same files, kept on purpose (decision 0261).**
//! `cli/src/regularities.rs` owns the resolver: the full seven-verdict
//! schema, the anchor grammar, the audit and its two-way regression guard.
//! This module reads the same JSON for one job — putting a criterion, a
//! measurement and a verdict under a metric on a generated Book page — and
//! so keeps only the fields a page shows. `windows/lab` may not depend on
//! `cli` (the layering is `kernel/` -> `domains/*` -> `windows/*` ->
//! `cli/`), and a corpus schema has no business in the kernel, which is the
//! determinism substrate. A duplicated reader is therefore the design, not
//! an accident, and 0261's condition applies in full: each reader names the
//! other, and the two are held together by an agreement test that fails if
//! they diverge.
//!
//! The agreement test lives in `cli/tests/suite/regularity_coverage.rs`
//! (`the_two_readers_of_the_corpus_agree`), and it CANNOT live here: the
//! layering guard refuses the dev-dependency it would need. Putting it in
//! `windows/lab/tests/` means `hornvale-lab` declaring a dev-dependency on
//! `hornvale`, and `cli/tests/suite/architecture.rs` reads
//! `all_deps` — every dependency kind, unfiltered — for the window layer
//! check, so it sees a dev-dependency exactly as it sees a normal one.
//! Verified by adding the edge and running the check:
//! `architecture.rs:146` panicked with *"window hornvale-lab depends on
//! hornvale, which sits above the window layer"*. The edge was then
//! removed.
//!
//! **An earlier draft of this paragraph said the opposite** — that
//! `architecture.rs` "does not examine dev-dependencies at all", so the
//! edge would *escape* the guard rather than be refused by it. That claim
//! came from grepping for the literal string `dev-dependencies` and finding
//! none; the behaviour lives in an unfiltered `deps.iter().map(dep_name)`
//! that never spells the word. Only `normal_deps` filters on
//! `kind.is_null()`. The correction is recorded rather than quietly edited
//! because the wrong version told a reader that dev-dependencies are an
//! unpoliced hole in the layering, which would invite exactly the edge the
//! guard refuses.
//!
//! **Nothing here evaluates a criterion.** The verdict this module reads is
//! the one the corpus RECORDS; whether the census still agrees with it is
//! `cli`'s audit to decide, and it emits a `Regressed` finding in either
//! direction when it does not. What this module does compute is the single
//! number the criterion is decided on ([`Criterion::measured`]), because a
//! page that stated a verdict without the number behind it would be
//! unfalsifiable in exactly the way the stats table already is not.

use crate::domesday::census::Census;
use serde::Deserialize;
use std::path::Path;

/// A measured verdict: the criterion was evaluated against the population.
///
/// Two values, not the resolver's seven. `refused`, `deferred`, `absent`,
/// `inapplicable` and `unmeasured` all mean *no measurement happened*, and
/// an item carrying one of them makes no claim a page could print — so
/// [`load`] drops those items rather than representing them here. Widening
/// this enum is how a future measured verdict would reach a page.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
    /// Measured; the frozen criterion is met.
    Grown,
    /// Measured; the frozen criterion is not met.
    Flat,
}

impl Verdict {
    /// The verdict as the claim line shouts it.
    ///
    /// Upper-case deliberately: the verdict is the half of the sentence a
    /// reader is meant to be able to disagree with, and burying it in
    /// lower-case beside six other lower-case words is how a claim becomes
    /// decoration.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn shouted(self) -> &'static str {
        match self {
            Verdict::Grown => "GROWN",
            Verdict::Flat => "FLAT",
        }
    }
}

/// The frozen, falsifiable claim an item makes about the population.
///
/// The same five kinds `cli/src/regularities.rs` defines, with the same
/// serde tag and the same field names, because both readers parse the same
/// bytes. The compiler makes divergence loud on this side: every `match`
/// below is exhaustive, so adding a kind to the corpus schema without
/// adding it here fails to build rather than rendering a page that quietly
/// omits a claim.
/// type-audit: bare-ok(ratio: MedianInBand.lo), bare-ok(ratio: MedianInBand.hi), bare-ok(ratio: FractionInBandAtLeast.lo), bare-ok(ratio: FractionInBandAtLeast.hi), bare-ok(ratio: FractionInBandAtLeast.min_fraction), bare-ok(ratio: MedianAtLeast.bound), bare-ok(ratio: MedianAtMost.bound), bare-ok(ratio: PresentOnFraction.min_fraction)
#[derive(Debug, Clone, Copy, PartialEq, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum Criterion {
    /// The statistic's median over the population lies in `[lo, hi]`.
    MedianInBand {
        /// Inclusive lower edge.
        lo: f64,
        /// Inclusive upper edge.
        hi: f64,
    },
    /// At least `min_fraction` of worlds lie in `[lo, hi]`.
    FractionInBandAtLeast {
        /// Inclusive lower edge.
        lo: f64,
        /// Inclusive upper edge.
        hi: f64,
        /// Minimum share of worlds, in `[0, 1]`.
        min_fraction: f64,
    },
    /// The median is at least `bound`.
    MedianAtLeast {
        /// Inclusive lower bound.
        bound: f64,
    },
    /// The median is at most `bound`.
    MedianAtMost {
        /// Inclusive upper bound.
        bound: f64,
    },
    /// The statistic is non-absent on at least `min_fraction` of worlds.
    PresentOnFraction {
        /// Minimum share of worlds, in `[0, 1]`.
        min_fraction: f64,
    },
}

impl Criterion {
    /// The criterion as a reader can check it, assembled from the frozen
    /// parameters rather than from a second authored string.
    ///
    /// Derived, not transcribed: tightening a band in the corpus moves this
    /// sentence, and there is no way to leave the two disagreeing.
    /// type-audit: bare-ok(prose: return)
    pub fn prose(&self) -> String {
        match self {
            Criterion::MedianInBand { lo, hi } => format!("median in [{lo}, {hi}]"),
            Criterion::FractionInBandAtLeast {
                lo,
                hi,
                min_fraction,
            } => format!("at least {min_fraction} of worlds in [{lo}, {hi}]"),
            Criterion::MedianAtLeast { bound } => format!("median at least {bound}"),
            Criterion::MedianAtMost { bound } => format!("median at most {bound}"),
            Criterion::PresentOnFraction { min_fraction } => {
                format!("present on at least {min_fraction} of worlds")
            }
        }
    }

    /// The one number this criterion is decided on, given the statistic's
    /// present values and the population size.
    ///
    /// `None` only when the quantity is undefined — a median with nothing
    /// present, or a share of an empty population. A world *present with
    /// value 0.0* is not absent, so this never reads absence off
    /// `present.len()` for the median kinds.
    ///
    /// **Both fraction kinds divide by `worlds`, never by `present.len()`**,
    /// matching `hornvale::regularities::meets`: a world where the statistic
    /// is absent counts against the claim rather than being excluded from
    /// the vote. Dividing by the present count would let a statistic that is
    /// absent almost everywhere score highly on the handful of worlds where
    /// it happens to appear.
    /// type-audit: bare-ok(ratio: present), bare-ok(count: worlds), bare-ok(ratio: return)
    pub fn measured(&self, present: &[f64], worlds: usize) -> Option<f64> {
        match self {
            Criterion::MedianInBand { .. }
            | Criterion::MedianAtLeast { .. }
            | Criterion::MedianAtMost { .. } => median(present),
            Criterion::FractionInBandAtLeast { lo, hi, .. } => {
                if worlds == 0 {
                    return None;
                }
                let inside = present.iter().filter(|v| **v >= *lo && **v <= *hi).count();
                Some(inside as f64 / worlds as f64)
            }
            Criterion::PresentOnFraction { .. } => {
                if worlds == 0 {
                    return None;
                }
                Some(present.len() as f64 / worlds as f64)
            }
        }
    }
}

/// One scored item, as the Domesday needs to render it.
///
/// The criterion is kept STRUCTURED rather than as pre-rendered prose (the
/// brief's first sketch): the page must state both the criterion and the
/// number it is decided on, and only the parameters can produce the second.
/// Storing prose beside them would be a second authored copy of the frozen
/// claim, free to drift from the numbers the verdict was actually taken on
/// — the transcription failure this whole line exists to avoid.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(identifier-text: id), bare-ok(prose: title), bare-ok(prose: source), bare-ok(identifier-text: statistic)
#[derive(Debug, Clone, PartialEq)]
pub struct ScoredItem {
    /// Which corpus scored it, e.g. `sugarscape-1996`.
    pub corpus: String,
    /// The item's corpus-local id, e.g. `sug-wealth-skew`.
    pub id: String,
    /// The regularity itself, stated as the source states it.
    ///
    /// Without this a claim line is two unglossed slugs and a number: a
    /// reader can check the arithmetic but cannot learn WHAT was predicted,
    /// which is half of what makes a claim falsifiable by someone outside
    /// this repository.
    pub title: String,
    /// Where in the source the regularity appears, e.g.
    /// `Ch. II, 'Emergence'; Animation II-3`.
    ///
    /// **Never passed through
    /// [`crate::domesday::render::redact_registry_citations`]**, unlike a
    /// metric's `doc`. That redactor matches on SHAPE — two to eight
    /// upper-case letters, a hyphen, digits — with no prefix allowlist, so
    /// it would silently delete `II-3` from this very string. The Book's own
    /// guard (`docs_consistency::the_book_carries_no_registry_ids_or_
    /// process_vocabulary`) filters by ACTUAL registry prefixes and is
    /// therefore untroubled by a chapter-and-plate citation.
    pub source: String,
    /// The census column it is measured through.
    pub statistic: String,
    /// The frozen criterion, as parameters.
    pub criterion: Criterion,
    /// What measuring found, as the corpus records it.
    pub verdict: Verdict,
}

impl ScoredItem {
    /// The number this item's criterion is decided on, read off `census`.
    ///
    /// `None` when the census has no such column at all, or when the
    /// quantity is undefined — the two are distinguished by the caller,
    /// which renders nothing for the first and says `absent` for the
    /// second.
    ///
    /// Present values are parsed the way `hornvale::regularities::values_of`
    /// parses them (`parse::<f64>().ok()`, unparseable readings skipped),
    /// deliberately and not by coincidence: the number this page prints must
    /// be the number the verdict was taken on, and two parsers can agree for
    /// a thousand worlds and disagree on the one that matters.
    /// type-audit: bare-ok(ratio: return)
    pub fn measured(&self, census: &Census) -> Option<f64> {
        if !census.has(&self.statistic) {
            return None;
        }
        let present: Vec<f64> = census
            .values(&self.statistic)
            .into_iter()
            .filter_map(|reading| reading.parse::<f64>().ok())
            .collect();
        self.criterion.measured(&present, census.rows.len())
    }
}

/// The median of the present values, or `None` when there are none.
///
/// The average of the two middle values on an even-length sample, matching
/// both `hornvale::regularities::median` and
/// `crate::domesday::stats`'s own median — never a nearest-rank
/// `percentile(0.5)`, which picks the lower of the two and silently differs
/// whenever the pair disagrees. Sorted with `total_cmp`, the workspace's
/// only sanctioned float ordering.
fn median(present: &[f64]) -> Option<f64> {
    if present.is_empty() {
        return None;
    }
    let mut sorted = present.to_vec();
    sorted.sort_by(f64::total_cmp);
    let mid = sorted.len() / 2;
    Some(if sorted.len().is_multiple_of(2) {
        (sorted[mid - 1] + sorted[mid]) / 2.0
    } else {
        sorted[mid]
    })
}

/// A corpus file, as much of it as this module reads.
#[derive(Deserialize)]
struct RawCorpus {
    /// The corpus identifier.
    corpus: String,
    /// Where this catalogue comes from and what bias it carries.
    provenance: String,
    /// Every item, measured or not.
    items: Vec<RawItem>,
}

/// One frozen corpus, reduced to what a survey page shows: where it came
/// from, and the items it actually scores.
///
/// The provenance is carried because a claim line's `source` names a
/// chapter but not a BOOK. A reader outside this repository needs the
/// bibliographic origin to reach Epstein & Axtell at all — and the same
/// paragraph carries the corpus's own statement of its bias ("an instrument
/// with known bias, never a standard"), which is what stops a `FLAT` being
/// read as a verdict on Hornvale rather than on the analogy the item drew.
/// It is reproduced VERBATIM rather than summarised or sliced: any rule for
/// extracting "just the citation" is a heuristic over authored prose, and
/// the obvious one — take the first sentence — cuts this corpus's
/// provenance at `Joshua M.` and would degrade silently on the next.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(artifact: path), bare-ok(prose: provenance)
#[derive(Debug, Clone, PartialEq)]
pub struct ScoredCorpus {
    /// The corpus identifier, e.g. `sugarscape-1996`.
    pub corpus: String,
    /// Where the corpus file lives, as the caller named it.
    ///
    /// **Must be repository-relative.** It is printed onto a committed,
    /// drift-checked Book page, so an absolute path would make the artifact
    /// machine-dependent. `cli`'s `lab domesday` passes the entries of
    /// `hornvale::regularities::CORPORA`, which are relative, and
    /// `regularity_coverage::the_rendered_corpus_pointer_is_the_relative_path`
    /// holds the rendered page to exactly that string.
    pub path: String,
    /// The corpus's own provenance statement, verbatim.
    pub provenance: String,
    /// The items this corpus scores, in corpus order.
    pub items: Vec<ScoredItem>,
}

/// One item, as much of it as this module reads.
#[derive(Deserialize)]
struct RawItem {
    /// Corpus-local identifier.
    id: String,
    /// The regularity as the source states it.
    title: String,
    /// Where in the source it appears.
    source: String,
    /// The census column, empty for a non-measured verdict.
    #[serde(default)]
    statistic: String,
    /// The frozen claim, absent for a non-measured verdict.
    #[serde(default)]
    criterion: Option<Criterion>,
    /// The recorded verdict, as the corpus spells it.
    verdict: String,
}

/// Parse a corpus and keep the items it actually scores, in corpus order.
///
/// An item is scored when its verdict is `grown` or `flat` AND it carries a
/// criterion. The two conditions are checked together rather than either
/// alone: a measured verdict with no criterion is a malformed corpus, which
/// `cli`'s audit reports as a finding, and this reader must not turn that
/// into a page that states a verdict with nothing behind it.
///
/// A parse failure is an `Err`, never an empty reading — a reader that
/// failed open to the empty set would strip every claim line from the
/// survey and leave a healthy-looking page behind.
/// `path` is recorded on the result as [`ScoredCorpus::path`] and reaches a
/// committed page, so it must be repository-relative.
/// type-audit: bare-ok(artifact: json), bare-ok(artifact: path), bare-ok(prose: return)
pub fn load(json: &str, path: &str) -> Result<ScoredCorpus, String> {
    let raw: RawCorpus =
        serde_json::from_str(json).map_err(|e| format!("regularity corpus parse: {e}"))?;
    let corpus = raw.corpus;
    let items = raw
        .items
        .into_iter()
        .filter_map(|item| {
            let verdict = match item.verdict.as_str() {
                "grown" => Verdict::Grown,
                "flat" => Verdict::Flat,
                _ => return None,
            };
            let criterion = item.criterion?;
            Some(ScoredItem {
                corpus: corpus.clone(),
                id: item.id,
                title: item.title,
                source: item.source,
                statistic: item.statistic,
                criterion,
                verdict,
            })
        })
        .collect();
    Ok(ScoredCorpus {
        corpus,
        path: path.to_string(),
        provenance: raw.provenance,
        items,
    })
}

/// Read a corpus from disk and keep the items it scores.
///
/// The path is used both to open the file and, verbatim, as the pointer the
/// survey prints — so pass a repository-relative one.
/// type-audit: bare-ok(artifact: path), bare-ok(prose: return)
pub fn read(path: &str) -> Result<ScoredCorpus, String> {
    let json =
        std::fs::read_to_string(Path::new(path)).map_err(|e| format!("reading {path}: {e}"))?;
    load(&json, path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_median_kinds_average_the_two_middle_values() {
        let c = Criterion::MedianInBand { lo: 0.0, hi: 1.0 };
        assert_eq!(c.measured(&[1.0, 2.0, 3.0, 6.0], 4), Some(2.5));
        assert_eq!(c.measured(&[], 4), None);
    }

    #[test]
    fn the_fraction_kinds_divide_by_the_population_not_the_present_count() {
        let c = Criterion::FractionInBandAtLeast {
            lo: 0.0,
            hi: 1.0,
            min_fraction: 0.5,
        };
        // Two present values, both in band, over a population of ten: the
        // share is 0.2, not 1.0.
        assert_eq!(c.measured(&[0.5, 0.5], 10), Some(0.2));
        assert_eq!(c.measured(&[0.5, 0.5], 0), None);
        let p = Criterion::PresentOnFraction { min_fraction: 0.5 };
        assert_eq!(p.measured(&[0.5, 0.5], 10), Some(0.2));
    }

    #[test]
    fn a_measured_verdict_without_a_criterion_is_not_scored() {
        let json = r#"{"corpus":"c","provenance":"p","items":[
          {"id":"a","title":"t","source":"s","verdict":"flat","statistic":"x"},
          {"id":"b","title":"t","source":"s","verdict":"grown","statistic":"x",
           "criterion":{"kind":"median-at-most","bound":1.0}}
        ]}"#;
        let scored = load(json, "regularities/c.regularity.json").expect("parses");
        assert_eq!(scored.items.len(), 1);
        assert_eq!(scored.items[0].id, "b");
        assert_eq!(scored.path, "regularities/c.regularity.json");
        assert_eq!(scored.provenance, "p");
    }
}

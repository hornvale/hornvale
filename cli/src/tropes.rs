//! The Repertoire: score a frozen corpus of dramatic situations against the
//! concept registry. Build-state only — no seed, no world save, no census.
//!
//! Wired into the CLI as `hornvale tropes report|check`; `cmd_tropes` in
//! `main.rs` builds a real world and calls `load`/`resolve`/`render` on it,
//! and Task 4 pins `render`'s output as a ratchet.

use hornvale_kernel::ConceptRegistry;
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};

/// One situation as authored in the corpus.
/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: name), bare-ok(prose: actants), bare-ok(identifier-text: requires), bare-ok(prose: excluded_by)
#[derive(Debug, Deserialize)]
pub struct Situation {
    /// Stable corpus-local identifier.
    pub id: String,
    /// Human-readable situation name.
    pub name: String,
    /// Greimas actant role → the role's description in this situation.
    pub actants: BTreeMap<String, String>,
    /// Namespaced tokens and `bundle:` references this situation needs.
    pub requires: Vec<String>,
    /// Preconditions whose absence makes this situation inapplicable.
    pub excluded_by: Vec<String>,
}

/// A frozen, provenance-stamped corpus.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(prose: provenance), bare-ok(prose: frozen), bare-ok(identifier-text: bundles)
#[derive(Debug, Deserialize)]
pub struct Corpus {
    /// Corpus identifier, e.g. `polti-1895`.
    pub corpus: String,
    /// Where this catalogue comes from and what bias it carries.
    pub provenance: String,
    /// Note recording that the freeze preceded measurement.
    pub frozen: String,
    /// Bundle name → the tokens it expands to.
    pub bundles: BTreeMap<String, Vec<String>>,
    /// The situations themselves.
    pub situations: Vec<Situation>,
}

/// How one situation resolved.
/// type-audit: bare-ok(identifier-text: Blocked.0), bare-ok(prose: Inapplicable.0)
#[derive(Debug, PartialEq, Eq)]
pub enum Outcome {
    /// Every requirement satisfied.
    Stageable,
    /// One or more tokens absent, listed in corpus order.
    Blocked(Vec<String>),
    /// The world deliberately lacks a precondition; different, not deficient.
    Inapplicable(String),
}

/// Parse a corpus from JSON.
/// type-audit: bare-ok(artifact: json), bare-ok(prose: return)
pub fn load(json: &str) -> Result<Corpus, String> {
    serde_json::from_str(json).map_err(|e| format!("corpus parse: {e}"))
}

/// Every token the registry holds, namespaced.
fn registry_tokens(r: &ConceptRegistry) -> BTreeSet<String> {
    let mut t = BTreeSet::new();
    for p in r.predicates() {
        t.insert(format!("predicate:{}", p.name));
    }
    for (kind, _doc) in r.phenomenon_kinds() {
        t.insert(format!("phenomenon:{kind}"));
    }
    for c in r.concepts() {
        t.insert(format!("concept:{}", c.name));
    }
    t
}

/// Expand a requirement into concrete tokens, following one `bundle:` level.
///
/// A dangling `bundle:` reference expands to the reference itself, which no
/// registry token can ever match, so a typo blocks its situation. Returning
/// an empty list instead would make a situation whose requirements are all
/// dangling resolve `Stageable` — the exact inversion of spec D4's
/// default-deny posture.
fn expand(corpus: &Corpus, req: &str) -> Vec<String> {
    match req.strip_prefix("bundle:") {
        Some(b) => match corpus.bundles.get(b) {
            Some(tokens) => tokens.clone(),
            None => vec![req.to_string()],
        },
        None => vec![req.to_string()],
    }
}

/// Resolve every situation against a registry. Keyed by situation `id`.
/// type-audit: bare-ok(identifier-text: return)
pub fn resolve(corpus: &Corpus, registry: &ConceptRegistry) -> BTreeMap<String, Outcome> {
    let held = registry_tokens(registry);
    let mut out = BTreeMap::new();
    for s in &corpus.situations {
        if let Some(reason) = s.excluded_by.first() {
            out.insert(s.id.clone(), Outcome::Inapplicable(reason.clone()));
            continue;
        }
        // Deduplicated, in corpus order: two bundles may name the same
        // token, and `Blocked` is rendered verbatim into the committed
        // artifact, where a repeat would misstate the count.
        let mut missing: Vec<String> = Vec::new();
        for t in s.requires.iter().flat_map(|r| expand(corpus, r)) {
            if !held.contains(&t) && !missing.contains(&t) {
                missing.push(t);
            }
        }
        out.insert(
            s.id.clone(),
            if missing.is_empty() {
                Outcome::Stageable
            } else {
                Outcome::Blocked(missing)
            },
        );
    }
    out
}

/// Hard-wrap a prose paragraph at 76 columns on word boundaries.
///
/// The report is a byte-ratcheted artifact. An unwrapped paragraph makes every
/// future word change a whole-line diff in the review that has to approve it,
/// so prose is wrapped and tables are not.
/// type-audit: bare-ok(prose: text), bare-ok(prose: return)
fn wrap(text: &str) -> String {
    let mut out = String::new();
    let mut col = 0;
    for word in text.split_whitespace() {
        let w = word.chars().count();
        if col > 0 && col + 1 + w > 76 {
            out.push('\n');
            col = 0;
        } else if col > 0 {
            out.push(' ');
            col += 1;
        }
        out.push_str(word);
        col += w;
    }
    out
}

/// Render the coverage report. Four sections, provenance first (spec §4 L2).
/// type-audit: bare-ok(identifier-text: out), bare-ok(prose: return)
pub fn render(
    corpus: &Corpus,
    out: &BTreeMap<String, Outcome>,
    registry: &ConceptRegistry,
) -> String {
    let mut s = String::new();
    s.push_str(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale tropes report`. -->\n\n",
    );
    s.push_str("# Trope coverage\n\n## Provenance\n\n");
    s.push_str(&format!("- **Corpus:** `{}`\n", corpus.corpus));
    s.push_str(&format!("- **Source:** {}\n", corpus.provenance));
    s.push_str(&format!("- **Frozen:** {}\n", corpus.frozen));
    s.push_str(
        "\nThis measures reach against *that* catalogue. It is not a verdict on the\nworld, and it scores **representability only** — whether an agent could plan\nor recognise a situation is not measured here.\n\nA low score is the expected reading at this stage: the report is a baseline\ntaken before the machinery it measures exists. What carries information is\nmovement between runs, not the absolute number.\n\n",
    );

    let stageable = out.values().filter(|o| **o == Outcome::Stageable).count();
    let inapplicable = out
        .values()
        .filter(|o| matches!(o, Outcome::Inapplicable(_)))
        .count();
    s.push_str("## Demand\n\n");
    s.push_str(&format!(
        "Stageable {stageable} of {} ({inapplicable} inapplicable).\n\n| Situation | Actants | Outcome |\n|---|---|---|\n",
        out.len()
    ));
    let names: BTreeMap<&str, &str> = corpus
        .situations
        .iter()
        .map(|x| (x.id.as_str(), x.name.as_str()))
        .collect();
    let roles: BTreeMap<&str, String> = corpus
        .situations
        .iter()
        .map(|x| {
            (
                x.id.as_str(),
                x.actants.keys().cloned().collect::<Vec<_>>().join(", "),
            )
        })
        .collect();
    for (id, o) in out {
        let cell = match o {
            Outcome::Stageable => "stageable".to_string(),
            Outcome::Inapplicable(r) => format!("inapplicable — {r}"),
            Outcome::Blocked(m) => format!("blocked — missing `{}`", m.join("`, `")),
        };
        s.push_str(&format!(
            "| {} ({id}) | {} | {cell} |\n",
            names.get(id.as_str()).copied().unwrap_or("?"),
            roles.get(id.as_str()).map(String::as_str).unwrap_or("")
        ));
    }

    // Rank only bundles that are actually MISSING. A blocked situation also
    // requires bundles the world already holds; counting those put seven
    // satisfied bundles in a table headed "missing", and the backlog
    // ordering IS the deliverable here (spec D3, P1).
    let held = registry_tokens(registry);
    let mut fan: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for st in &corpus.situations {
        if let Some(Outcome::Blocked(_)) = out.get(&st.id) {
            for r in st.requires.iter().filter(|r| r.starts_with("bundle:")) {
                if expand(corpus, r).iter().any(|t| !held.contains(t)) {
                    fan.entry(r.clone()).or_default().push(st.id.clone());
                }
            }
        }
    }
    let mut ranked: Vec<_> = fan.into_iter().collect();
    ranked.sort_by(|a, b| b.1.len().cmp(&a.1.len()).then(a.0.cmp(&b.0)));

    // How close is the closest blocked situation? If this is ever 1, a single
    // bundle really would unlock something and the caveat below should change.
    let closest = corpus
        .situations
        .iter()
        .filter(|st| matches!(out.get(&st.id), Some(Outcome::Blocked(_))))
        .map(|st| {
            st.requires
                .iter()
                .filter(|r| r.starts_with("bundle:"))
                .filter(|r| expand(corpus, r).iter().any(|t| !held.contains(t)))
                .count()
        })
        .min()
        .unwrap_or(0);
    let blocked = out
        .values()
        .filter(|o| matches!(o, Outcome::Blocked(_)))
        .count();
    let inapplicable_noun = if inapplicable == 1 {
        "situation is"
    } else {
        "situations are"
    };
    s.push_str("\n## Leverage\n\n");
    s.push_str(&wrap(&format!(
        "Missing bundles ranked by fan-in over the {blocked} **blocked** situations. The \
         {inapplicable} inapplicable {inapplicable_noun} excluded from this ranking, but not \
         from the report: the Supply section below still counts its requirements as demand, \
         which keeps those tokens off the orphan list. The **corpus** column counts all {} \
         situations.",
        out.len()
    )));
    s.push_str("\n\n");
    s.push_str(&wrap(&format!(
        "Fan-in is **not** an unlock count: the closest blocked situation is still missing \
         {closest} bundles, so no single row makes anything stageable on its own."
    )));
    // A bundle required ONLY by inapplicable situations never enters the fan
    // map above, so a genuinely missing capability can vanish from the
    // ranking. This is the mirror image of the defect that put seven
    // SATISFIED bundles under a heading reading "missing": the preamble
    // licenses the mechanism (inapplicable situations are excluded) but
    // nothing lets a reader get from the row count to the true total. The
    // ranked misses are this report's deliverable, so disclose it.
    let shown: BTreeSet<&str> = ranked
        .iter()
        .map(|(b, _)| b.trim_start_matches("bundle:"))
        .collect();
    let hidden: Vec<&String> = corpus
        .bundles
        .iter()
        .filter(|(_, toks)| toks.iter().any(|t| !held.contains(t)))
        .map(|(b, _)| b)
        .filter(|b| !shown.contains(b.as_str()))
        .collect();
    if !hidden.is_empty() {
        // Total is ranked + hidden rather than an independent count over
        // `corpus.bundles`, so the two numbers in the sentence always
        // reconcile to the length of the list beside them. A dangling
        // `bundle:` reference ranks (default-deny) without being a corpus
        // key, and an independent count would silently break that arithmetic.
        let total_missing = ranked.len() + hidden.len();
        let (lead, verb) = if hidden.len() == 1 {
            ("1 missing bundle".to_string(), "is")
        } else {
            (format!("{} missing bundles", hidden.len()), "are")
        };
        let list = hidden
            .iter()
            .map(|b| format!("`bundle:{b}`"))
            .collect::<Vec<_>>()
            .join(", ");
        s.push_str("\n\n");
        s.push_str(&wrap(&format!(
            "**{lead} {verb} not ranked below.** {list} — required only by situations that \
             resolve inapplicable, so they contribute no fan-in and no row. The corpus holds \
             {total_missing} missing bundles against the {} ranked here; that is the difference.",
            ranked.len()
        )));
    }
    s.push_str("\n\n| Bundle | Fan-in (blocked) | Corpus | Situations |\n|---|---|---|---|\n");
    for (bundle, sits) in &ranked {
        let corpus_wide = corpus
            .situations
            .iter()
            .filter(|st| st.requires.contains(bundle))
            .count();
        s.push_str(&format!(
            "| `{bundle}` | {} | {corpus_wide} | {} |\n",
            sits.len(),
            sits.join(", ")
        ));
    }

    let required: BTreeSet<String> = corpus
        .situations
        .iter()
        .flat_map(|st| st.requires.iter().flat_map(|r| expand(corpus, r)))
        .collect();
    let orphans: Vec<String> = held
        .iter()
        .filter(|t| !required.contains(*t))
        .cloned()
        .collect();
    s.push_str(&format!(
        "\n## Supply\n\n{} registered tokens no situation in this corpus requires.\n\n",
        orphans.len()
    ));
    s.push_str(&wrap(
        "**Demand-side only.** Spec §4 L2.4 asks for tokens no situation requires *and no \
         readout consumes*; the second half is not implemented. So this list includes \
         tokens that readouts do consume — `predicate:is-a` carries the Book, and the \
         `moon-*` family carries the almanac. Read it as *unrequired by this catalogue*, \
         not *unused*. Spec D5's Goodhart guard — a rising demand score beside a rising \
         count of genuinely unconsumed tokens — needs the missing half before this list \
         can serve it.",
    ));
    s.push_str("\n\n");
    // Annotate `concept:` orphans with their owning domain. Many come from
    // the language lexicon and are WORDS, not modelled capabilities; an
    // unannotated list invites reading every orphan as a registered-but-
    // unused mechanism, which is the opposite of what the Supply count is
    // for (spec D5).
    let domains: BTreeMap<String, String> = registry
        .concepts()
        .map(|c| (format!("concept:{}", c.name), c.domain.clone()))
        .collect();
    for t in &orphans {
        match domains.get(t) {
            Some(d) => s.push_str(&format!("- `{t}` ({d})\n")),
            None => s.push_str(&format!("- `{t}`\n")),
        }
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A requirement naming a token the registry does not hold resolves
    /// `Blocked`, never silently satisfied — the default-deny posture.
    #[test]
    fn an_unknown_token_blocks_its_situation() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{"b":["predicate:no-such-predicate"]},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["bundle:b"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        match out.get("s1").expect("s1 resolved") {
            Outcome::Blocked(missing) => {
                assert_eq!(missing, &vec!["predicate:no-such-predicate".to_string()]);
            }
            other => panic!("expected Blocked, got {other:?}"),
        }
    }

    /// A registered token satisfies, so the situation is stageable.
    #[test]
    fn a_registered_token_makes_a_situation_stageable() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t","bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:known"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let mut registry = hornvale_kernel::ConceptRegistry::default();
        registry
            .register_predicate("known", false, "a predicate for the test")
            .expect("registers");
        assert_eq!(
            resolve(&corpus, &registry).get("s1"),
            Some(&Outcome::Stageable)
        );
    }

    /// `excluded_by` short-circuits: the world lacks the precondition, so the
    /// situation is inapplicable rather than blocked.
    #[test]
    fn an_excluded_situation_is_inapplicable_not_blocked() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t","bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:absent"],
                         "excluded_by":["this world has no marriage"]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        assert_eq!(
            resolve(&corpus, &registry).get("s1"),
            Some(&Outcome::Inapplicable(
                "this world has no marriage".to_string()
            ))
        );
    }

    /// Resolution is deterministic: same inputs, identical output ordering.
    #[test]
    fn resolution_is_order_stable() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{"b":["predicate:x","predicate:y"]},
          "situations":[{"id":"s2","name":"B","actants":{},"requires":["bundle:b"],"excluded_by":[]},
                        {"id":"s1","name":"A","actants":{},"requires":["bundle:b"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let a = resolve(&corpus, &registry);
        let b = resolve(&corpus, &registry);
        assert_eq!(format!("{a:?}"), format!("{b:?}"));
        assert_eq!(a.keys().collect::<Vec<_>>(), vec!["s1", "s2"]);
    }

    /// A dangling `bundle:` reference must block, not silently vanish into
    /// an empty expansion — the exact inversion of default-deny that an
    /// `unwrap_or_default()` would produce.
    #[test]
    fn a_dangling_bundle_reference_blocks() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t","bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["bundle:does-not-exist"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        match resolve(&corpus, &registry).get("s1") {
            Some(Outcome::Blocked(missing)) => {
                assert_eq!(missing, &vec!["bundle:does-not-exist".to_string()]);
            }
            other => panic!("expected Blocked, got {other:?}"),
        }
    }

    /// Two bundles that share a token must not double-count it in `Blocked`,
    /// and the survivors keep first-appearance-in-corpus order — not sorted,
    /// which is the assertion `resolution_is_order_stable` cannot make on
    /// its own since this repo bans `HashMap` outright.
    #[test]
    fn blocked_tokens_are_deduplicated_in_corpus_order() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{
            "first":["predicate:shared","predicate:only-in-first"],
            "second":["predicate:shared","predicate:only-in-second"]
          },
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["bundle:first","bundle:second"],
                         "excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        match resolve(&corpus, &registry).get("s1") {
            Some(Outcome::Blocked(missing)) => {
                assert_eq!(
                    missing,
                    &vec![
                        "predicate:shared".to_string(),
                        "predicate:only-in-first".to_string(),
                        "predicate:only-in-second".to_string(),
                    ]
                );
            }
            other => panic!("expected Blocked, got {other:?}"),
        }
    }

    /// The report leads with provenance and carries all four sections, so a
    /// reader cannot mistake the number for a verdict on the world.
    #[test]
    fn the_report_states_provenance_and_all_four_sections() {
        let json = r#"{
          "corpus":"t","provenance":"a catalogue with known bias","frozen":"t",
          "bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:absent"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        let text = render(&corpus, &out, &registry);
        assert!(text.contains("a catalogue with known bias"));
        for section in ["## Provenance", "## Demand", "## Leverage", "## Supply"] {
            assert!(text.contains(section), "missing {section}");
        }
        assert!(text.contains("GENERATED FILE"));
    }

    /// A bundle required ONLY by inapplicable situations never enters the fan
    /// map, so it silently drops out of the ranked misses. The report must
    /// disclose it in prose — and must not smuggle it back in as a row, since
    /// the ranking is by fan-in and this bundle has none.
    #[test]
    fn a_bundle_masked_by_an_exclusion_is_disclosed_but_not_ranked() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{
            "masked":["predicate:never-registered"],
            "ranked-bundle":["predicate:also-absent"]
          },
          "situations":[
            {"id":"s1","name":"Blocked","actants":{"subject":"a"},
             "requires":["bundle:ranked-bundle"],"excluded_by":[]},
            {"id":"s2","name":"Excluded","actants":{"subject":"a"},
             "requires":["bundle:masked"],
             "excluded_by":["this world has no such thing"]}
          ]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        let text = render(&corpus, &out, &registry);

        // Disclosed in prose, with the arithmetic that gets a reader from the
        // row count to the true total.
        assert!(
            text.contains("1 missing bundle is not ranked below"),
            "no disclosure sentence:\n{text}"
        );
        assert!(
            text.contains("`bundle:masked`"),
            "the masked bundle is not named:\n{text}"
        );
        assert!(
            text.contains("2 missing bundles against the 1 ranked here"),
            "the disclosure does not reconcile 2 - 1 = 1:\n{text}"
        );

        // Not a row. A disclosure that becomes a 32nd entry in a fan-in
        // ranking is the defect it was written to fix, one direction over.
        assert!(
            !text.contains("| `bundle:masked` |"),
            "the masked bundle leaked into the ranking table:\n{text}"
        );
        assert!(
            text.contains("| `bundle:ranked-bundle` | 1 |"),
            "the genuinely ranked bundle lost its row:\n{text}"
        );
    }

    /// The corpus's actant roles stay inside Greimas' six. A seventh role, or
    /// a situation declaring none, means the hand-authored decomposition
    /// vocabulary drifted — and nothing else in the workspace checks it.
    #[test]
    fn every_situation_declares_only_greimas_actants() {
        const GREIMAS: [&str; 6] = [
            "helper", "object", "opponent", "receiver", "sender", "subject",
        ];
        let corpus =
            load(include_str!("../../tropes/polti.trope.json")).expect("the live corpus parses");
        for st in &corpus.situations {
            assert!(!st.actants.is_empty(), "{} declares no actants", st.id);
            for role in st.actants.keys() {
                assert!(
                    GREIMAS.contains(&role.as_str()),
                    "{}: actant role `{role}` is not one of Greimas' six",
                    st.id
                );
            }
        }
    }
}

//! The repo self-map.

use crate::scan::repo_root;
use crate::store::ProjectLedger;
use crate::vocabulary::project_registry;
use hornvale_kernel::ledger::Value;

/// Render the layering and dependency-allowlist section of the self-map.
///
/// Every value here is DERIVED. Nothing in this function may name an
/// allowlist member literally — that is what drifted before (spec §1.1).
///
/// The determinism-contracts, committed-artifacts, and documentation-map
/// sections that follow are ASSERTED prose (spec §6 S6): each line is a
/// `self-map-line` fact in `docs/digest/facts.jsonl`, read here rather than
/// hand-restated, which is what let `scripts/doctor.sh` drift (it once
/// omitted `libm` long after decision 0041 admitted it). The one exception
/// is the ratified-decision count, which stays DERIVED (PROC-11 forbids
/// storing a fast-drifting fact) — its line carries a `{decision_count}`
/// placeholder substituted here, not a frozen number.
pub fn self_map(allowed: &[String], layers: &[String]) -> String {
    let layering = format!(
        "== Layering (enforced: cli/tests/suite/architecture.rs; picture: book/src/reference/layering.md)\n  \
         {}\n  \
         a domain depends on the kernel and NOTHING else; windows/worldgen is the\n  \
         composition root; external deps allowlist: {}\n",
        layers.join(" -> "),
        allowed.join(", ")
    );

    let decision_count = count_decisions();
    let (determinism, artifacts, docmap) = self_map_lines(decision_count);

    format!(
        "{layering}\n\
         == Determinism contracts (CLAUDE.md Determinism section is authoritative)\n\
         {determinism}\n\n\
         == Committed generated artifacts\n\
         {artifacts}\n\n\
         == Documentation map\n\
         {docmap}\n",
        determinism = determinism.join("\n"),
        artifacts = artifacts.join("\n"),
        docmap = docmap.join("\n"),
    )
}

/// The number of ratified decision records — derived by counting
/// `docs/decisions/*.md`, excluding `README.md`. Kept derived, never
/// stored: this is the fast-drifting count PROC-11's rule exists for.
fn count_decisions() -> usize {
    std::fs::read_dir(repo_root().join("docs/decisions"))
        .expect("decisions dir")
        .filter_map(Result::ok)
        .filter(|e| {
            let name = e.file_name().to_string_lossy().to_string();
            name.ends_with(".md") && name != "README.md"
        })
        .count()
}

/// Read the asserted `self-map-line` facts from `docs/digest/facts.jsonl`
/// and split them into the three sections by their authored subject-id
/// range (100-103 determinism, 200-204 artifacts, 300-303 documentation
/// map). JSONL's `(subject, predicate)` sort reproduces authored line
/// order; each subject holds exactly one line (a Task 2 review finding:
/// tie order between facts sharing a `(subject, predicate)` key is
/// unspecified, so grouping multiple lines under one subject would scramble
/// them).
fn self_map_lines(decision_count: usize) -> (Vec<String>, Vec<String>, Vec<String>) {
    let text = std::fs::read_to_string(repo_root().join("docs/digest/facts.jsonl"))
        .expect("docs/digest/facts.jsonl is readable");
    let led = ProjectLedger::from_jsonl(&text, project_registry()).expect("facts.jsonl parses");

    let mut lines: Vec<(u64, String)> = led
        .facts()
        .iter()
        .filter(|f| f.predicate == "self-map-line")
        .filter_map(|f| match &f.object {
            Value::Text(t) => Some((f.subject.get(), t.clone())),
            _ => None,
        })
        .collect();
    lines.sort_by_key(|(subject, _)| *subject);

    let mut determinism = Vec::new();
    let mut artifacts = Vec::new();
    let mut docmap = Vec::new();
    for (subject, line) in lines {
        let line = line.replace("{decision_count}", &decision_count.to_string());
        match subject {
            100..=199 => determinism.push(line),
            200..=299 => artifacts.push(line),
            300..=399 => docmap.push(line),
            _ => {}
        }
    }
    (determinism, artifacts, docmap)
}

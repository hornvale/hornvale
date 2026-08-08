//! The Digest CLI.

use digest::render;
use digest::scan;
use digest::store::ProjectLedger;
use digest::vocabulary::project_registry;
use hornvale_kernel::ledger::Value;
use std::collections::BTreeMap;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.get(1).map(String::as_str) {
        Some("render") => match args.get(2).map(String::as_str) {
            Some("doctor") => print!(
                "{}",
                render::doctor::self_map(
                    &scan::capability::allowed_external(),
                    &scan::capability::layers()
                )
            ),
            // `index` takes the decisions and their asserted supersession
            // scopes (spec §4.4: the trailing parenthetical in a status
            // line is not reliably scannable, so scope is asserted, never
            // parsed — see docs/digest/facts.jsonl).
            Some("decisions") => print!(
                "{}",
                render::decisions::index(&all_decisions(), &decision_scopes())
            ),
            Some("delta") => print!(
                "{}",
                render::delta::report(&all_decisions(), &decision_scopes())
            ),
            _ => {
                eprintln!("usage: digest render <doctor|decisions|delta>");
                std::process::exit(2);
            }
        },
        _ => {
            eprintln!("usage: digest render <doctor|decisions|delta>");
            std::process::exit(2);
        }
    }
}

fn all_decisions() -> Vec<scan::decisions::DecisionRecord> {
    let dir = scan::repo_root().join("docs/decisions");
    let mut out = Vec::new();
    let mut entries: Vec<_> = std::fs::read_dir(&dir)
        .expect("decisions dir")
        .filter_map(Result::ok)
        .map(|e| e.path())
        .collect();
    entries.sort();
    for path in entries {
        let name = path
            .file_name()
            .expect("name")
            .to_string_lossy()
            .to_string();
        if !name.ends_with(".md") || name == "README.md" {
            continue;
        }
        let id = name.split('-').next().expect("id prefix").to_string();
        let text = std::fs::read_to_string(&path).expect("read");
        out.push(scan::decisions::parse(&id, &text));
    }
    out
}

/// Decision ids mapped to their surviving-provisions text, sourced from
/// ASSERTED `supersession-scope` facts in `docs/digest/facts.jsonl` — never
/// scanned (spec §4.4).
fn decision_scopes() -> BTreeMap<String, String> {
    let text = std::fs::read_to_string(scan::repo_root().join("docs/digest/facts.jsonl"))
        .expect("docs/digest/facts.jsonl is readable");
    let led = ProjectLedger::from_jsonl(&text, project_registry()).expect("facts.jsonl parses");
    led.facts()
        .iter()
        .filter(|f| f.predicate == "supersession-scope")
        .filter_map(|f| match &f.object {
            Value::Text(t) => Some((format!("{:04}", f.subject.get()), t.clone())),
            _ => None,
        })
        .collect()
}

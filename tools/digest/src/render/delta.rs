//! The intent ≠ reality report.
//!
//! Generated documentation CANNOT disagree with the code, and disagreement is
//! diagnostic (spec §4.9). So intent is asserted, reality is scanned, and the
//! gap is computed rather than silently collapsed.

use crate::scan::decisions::{DecisionRecord, Status};
use crate::scan::repo_root;
use std::collections::{BTreeMap, BTreeSet};

/// The two paths the idea registry has lived at. It moved on 2026-07-10.
const REGISTRY_PATHS: &[&str] = &[
    "book/src/frontier/idea-registry.md",
    "docs/vision/idea-registry.md",
];

/// Run a git command in the repo and return stdout, or `None` on failure.
fn git(args: &[&str]) -> Option<String> {
    let out = std::process::Command::new("git")
        .current_dir(repo_root())
        .args(args)
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    String::from_utf8(out.stdout).ok()
}

/// Distinct row-leading numbered ids (`MAP-9`, `MAP-9a`) in registry text.
/// Row-leading only — a mention in prose is a reference, not a minting.
fn numbered_ids_in(text: &str) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for line in text.lines() {
        let Some(rest) = line.strip_prefix('|') else {
            continue;
        };
        let cell = rest.trim().trim_start_matches('`');
        let id: String = cell
            .chars()
            .take_while(|c| {
                c.is_ascii_uppercase() || *c == '-' || c.is_ascii_digit() || c.is_ascii_lowercase()
            })
            .collect();
        let Some((prefix, suffix)) = id.split_once('-') else {
            continue;
        };
        let digits = suffix.trim_end_matches(|c: char| c.is_ascii_lowercase());
        if !prefix.is_empty()
            && prefix.chars().all(|c| c.is_ascii_uppercase())
            && !digits.is_empty()
            && digits.chars().all(|c| c.is_ascii_digit())
        {
            out.insert(id);
        }
    }
    out
}

/// Is this checkout missing history? A shallow clone answers `true`.
///
/// `git clone --depth 1` is `actions/checkout@v4`'s DEFAULT, and under it the
/// graft boundary makes every reachable file look newly added: the archaeology
/// below then names the graft root as the commit that "first added" a decision.
/// No `Option` on a single lookup can catch that, because each individual git
/// call succeeds — it is the *history* that is a lie. So this view refuses to
/// compute an across-time gap here at all.
fn repo_is_shallow() -> bool {
    git(&["rev-parse", "--is-shallow-repository"]).is_some_and(|s| s.trim() == "true")
}

/// How many numbered ids the registry held at `rev`, trying both paths.
///
/// `None` means git could not answer: the rev does not resolve in this
/// checkout. That is emphatically NOT the same fact as `Some(0)` — "the rev
/// resolved, and the registry held no numbered identifiers there" — and
/// collapsing the two is how this report once emitted a confident, plausible,
/// wrong baseline. A rev that resolves but carries no registry file at either
/// path genuinely held zero, and is reported as `Some(0)`.
fn numbered_ids_at(rev: &str) -> Option<usize> {
    // Resolve first. A rev git cannot name is a rev this view cannot count.
    git(&["rev-parse", "--verify", "--quiet", rev])?;
    for path in REGISTRY_PATHS {
        if let Some(text) = git(&["show", &format!("{rev}:{path}")]) {
            let n = numbered_ids_in(&text).len();
            if n > 0 {
                return Some(n);
            }
        }
    }
    Some(0)
}

/// The commit that first added a decision file — when the rule took effect.
///
/// BOTH the outer `log` and the inner `show` must be scoped to `--diff-filter=A`:
/// a later commit can go on to MODIFY the same decision file (0043's renumbering
/// commit, `c8c494d`, touches `docs/decisions/0026-*` alongside adding new
/// decisions elsewhere under `docs/decisions/`). `git log` iterates newest-first,
/// so an unfiltered `git show --name-only` on that later commit would match the
/// id prefix on its MODIFIED line and shadow the true add commit (`dc4a406e`).
fn decision_effective_commit(id: &str) -> Option<String> {
    let listing = git(&[
        "log",
        "--diff-filter=A",
        "--format=%H",
        "--",
        "docs/decisions/",
    ])?;
    for line in listing.lines() {
        let files = git(&["show", "--name-only", "--diff-filter=A", "--format=", line])?;
        if files
            .lines()
            .any(|f| f.starts_with(&format!("docs/decisions/{id}-")))
        {
            return Some(line.to_string());
        }
    }
    None
}

/// Report every place authored intent and scanned reality disagree.
///
/// `scopes` is the same asserted map [`crate::render::decisions::index`] takes:
/// decision id to surviving-provisions text. A decision only counts as
/// partially superseded if a scope was ASSERTED for it.
pub fn report(records: &[DecisionRecord], scopes: &BTreeMap<String, String>) -> String {
    let mut out = String::from(
        "# Intent vs reality\n\n\
         GENERATED by `digest render delta` — do not edit. Each row is a rule \
         the project says it holds, next to what the repo actually does.\n\n",
    );
    // Counts LINES EMITTED, not gaps proven: an "undetermined" row is still an
    // answer, and must not fall through to the "no gaps found" reassurance.
    let mut rows = 0usize;
    let shallow = repo_is_shallow();

    for r in records {
        let Some(scope) = scopes.get(&r.id) else {
            continue;
        };
        if r.status != Status::Superseded {
            continue;
        }
        if !r.title.to_lowercase().contains("slug") {
            continue;
        }
        // Say "I do not know" out loud. The one artifact in this repo whose
        // whole purpose is to not say false things about the project must not
        // emit a number it cannot stand behind.
        let undetermined = |out: &mut String| {
            out.push_str(&format!(
                "- **{} {}** still governs registry rows ({}). **The across-time gap \
                 cannot be determined in this checkout** — git could not resolve the \
                 commit before the rule took effect. That is what a shallow clone \
                 looks like (`git clone --depth 1`, which is `actions/checkout@v4`'s \
                 default); re-run with full history (`fetch-depth: 0`).\n",
                r.id, r.title, scope
            ));
        };
        if shallow {
            rows += 1;
            undetermined(&mut out);
            continue;
        }
        // The gap is a difference across TIME, not a property of the working
        // tree: every id today is on the frozen list and the guard is green.
        // Compare the registry at the commit BEFORE the rule took effect
        // against the registry now.
        let Some(effective) = decision_effective_commit(&r.id) else {
            continue;
        };
        let (Some(before), Some(now)) = (
            numbered_ids_at(&format!("{effective}^")),
            numbered_ids_at("HEAD"),
        ) else {
            rows += 1;
            undetermined(&mut out);
            continue;
        };
        if now > before {
            rows += 1;
            out.push_str(&format!(
                "- **{} {}** still governs registry rows ({}). It took effect at \
                 `{}`, when the registry held {} numbered identifiers. It now holds \
                 {} — **{} were minted after the rule forbade them**, and the frozen \
                 fixture grandfathers all of them.\n",
                r.id,
                r.title,
                scope,
                &effective[..7.min(effective.len())],
                before,
                now,
                now - before
            ));
        }
    }

    if rows == 0 {
        out.push_str("- No gaps found. Verify the view still works before believing this.\n");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scan::decisions::{DecisionRecord, Status};

    fn scopes_with_0026() -> BTreeMap<String, String> {
        let mut m = BTreeMap::new();
        m.insert(
            "0026".to_string(),
            "for decision records; the study/chronicle/registry-row provisions stand".to_string(),
        );
        m
    }

    #[test]
    fn reports_the_registry_id_gap() {
        let recs = vec![DecisionRecord {
            id: "0026".into(),
            title: "Slugs, not numbers".into(),
            status: Status::Superseded,
            superseded_by: Some("0043".into()),
        }];
        let out = report(&recs, &scopes_with_0026());
        assert!(
            out.contains("0026"),
            "the partially-surviving decision must be named"
        );
        assert!(
            out.contains("minted after the rule forbade them"),
            "the across-time gap must be reported, not a working-tree count"
        );
        // The measured truth at 07d0afe1: 171 before, 403 now, 232 minted after.
        assert!(
            out.contains("232"),
            "the count of post-rule mintings must appear"
        );
    }

    #[test]
    fn numbered_ids_are_counted_row_leading_not_in_prose() {
        // A mention in prose is a REFERENCE, not a minting. Only a row-leading
        // id counts — this is what separates 403 real ids from 1,402 mentions,
        // and getting it wrong is what produced this campaign's retracted
        // falsification.
        let text = "| MAP-9 | an idea that mentions SKY-15 and BIO-2 in prose | raw | high | x |\n\
                    | SKY-eclipse-seasons | a slug row | raw | high | y |\n";
        let ids = numbered_ids_in(text);
        assert_eq!(ids.len(), 1, "only the row-leading numbered id counts");
        assert!(ids.contains("MAP-9"));
    }

    #[test]
    fn an_unresolvable_rev_is_none_not_a_fabricated_zero() {
        // The defect this pins: `numbered_ids_at` used to return `0` both when
        // the registry genuinely held no numbered ids AND when git could not
        // resolve the rev at all. Under a shallow checkout the second case fires
        // for `<effective>^`, and `now > before` then reports every id in the
        // registry as "minted after the rule forbade them".
        let bogus = "0000000000000000000000000000000000000000^";
        assert_eq!(
            numbered_ids_at(bogus),
            None,
            "an unresolvable rev must be an explicit non-answer, never a count"
        );
        assert!(
            numbered_ids_at("HEAD").is_some_and(|n| n > 0),
            "a resolvable rev with a live registry must still answer"
        );
    }

    #[test]
    fn the_report_is_not_vacuous_on_real_repo_state() {
        // S5: if this ever returns 'no gaps' on the live repo, either the
        // repo became perfect or the view stopped working. Assume the latter.
        let out = report(&crate::scan::decisions::all_for_test(), &scopes_with_0026());
        assert!(out.contains("0026"), "S5: the known live gap must be found");
    }
}

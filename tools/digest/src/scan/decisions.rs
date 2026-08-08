//! Parse `docs/decisions/*.md` into decision records.

pub use super::repo_root;

/// A decision's lifecycle state.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Status {
    /// In force.
    Accepted,
    /// Not yet ratified.
    Proposed,
    /// Replaced, wholly or in part.
    Superseded,
}

/// One parsed decision record.
#[derive(Debug, PartialEq, Clone)]
pub struct DecisionRecord {
    /// Zero-padded numeric id, e.g. `"0026"`.
    pub id: String,
    /// Title text from the `# NNNN. Title` line.
    pub title: String,
    /// Lifecycle state.
    pub status: Status,
    /// The superseding decision's id, if any.
    pub superseded_by: Option<String>,
}

/// Parse one decision file.
pub fn parse(id: &str, text: &str) -> DecisionRecord {
    let title = text
        .lines()
        .find(|l| l.starts_with("# "))
        .and_then(|l| l.split_once(". "))
        .map(|(_, t)| t.trim().to_string())
        .unwrap_or_default();

    // The status line may wrap, so join until the first `·` or blank line.
    let start = text.find("**Status:**").map(|i| i + "**Status:**".len());
    let raw = start
        .map(|i| {
            let rest = &text[i..];
            let end = rest.find("\n\n").unwrap_or(rest.len());
            rest[..end].replace('\n', " ")
        })
        .unwrap_or_default();
    let status_field = raw.split('·').next().unwrap_or("").trim().to_string();

    let (status, superseded_by) = if status_field.starts_with("Superseded") {
        // Take the FIRST run of ascii digits after "by", wherever it sits:
        // bare (`by 0063`), bracketed (`by [0099](0099-...md)`), or on the
        // next line (0006). Everything after it — rationale, scope, or date —
        // is deliberately ignored: those three are not distinguishable
        // syntactically, and scope is asserted rather than scanned (spec §4.4).
        let after = status_field
            .split_once("by")
            .map(|(_, a)| a.trim())
            .unwrap_or("");
        let mut sup = None;
        let mut run = String::new();
        for c in after.chars() {
            if c.is_ascii_digit() {
                run.push(c);
            } else if !run.is_empty() {
                sup = Some(run.clone());
                break;
            }
        }
        if sup.is_none() && !run.is_empty() {
            sup = Some(run);
        }
        (Status::Superseded, sup)
    } else if status_field.starts_with("Proposed") {
        (Status::Proposed, None)
    } else {
        (Status::Accepted, None)
    };

    DecisionRecord {
        id: id.to_string(),
        title,
        status,
        superseded_by,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_an_accepted_decision() {
        let text = "# 0110. The census is the suite's shared world-building pass\n\n\
                    **Status:** Accepted (2026-08-07, G3) · **Decider:** Nathan\n";
        let d = parse("0110", text);
        assert_eq!(
            d.title,
            "The census is the suite's shared world-building pass"
        );
        assert_eq!(d.status, Status::Accepted);
        assert_eq!(d.superseded_by, None);
    }

    #[test]
    fn parses_a_plain_supersession() {
        let text = "# 0029. CI checks 500-seed censuses\n\n\
                    **Status:** Superseded by 0063 · **Decider:** Nathan\n";
        let d = parse("0029", text);
        assert_eq!(d.status, Status::Superseded);
        assert_eq!(d.superseded_by.as_deref(), Some("0063"));
    }

    #[test]
    fn parses_a_bracketed_supersession() {
        let text = "# 0098. Hornvale is single-player\n\n\
                    **Status:** Superseded by [0099](0099-worlds-are-version-locked.md)\n";
        let d = parse("0098", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0099"));
    }

    #[test]
    fn a_trailing_parenthetical_is_never_read_as_scope() {
        // THE case this campaign exists for: 0026 reads as superseded to any
        // grepping reader, but its registry-row provision still stands, and
        // the registry violates it 1,402 times. The SCANNER must not try to
        // infer that — the same slot holds a rationale in 0063 and a date in
        // 0099. Scope is asserted, not scanned (spec §4.4).
        let text = "# 0026. Slugs, not numbers\n\n\
                    **Status:** Superseded by 0043 (for decision records; the \
                    study/chronicle/registry-row provisions stand) · **Decider:** Nathan\n";
        let d = parse("0026", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0043"));
    }

    #[test]
    fn a_date_parenthetical_does_not_corrupt_the_superseder_id() {
        // 0099's form: a bracket link followed by a DATE parenthetical.
        let text = "# 0082. A thing\n\n\
                    **Status:** Superseded by [0099](0099-worlds-are-version-locked.md) (2026-08-04) ·\n";
        let d = parse("0082", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0099"));
    }

    #[test]
    fn a_rationale_parenthetical_does_not_corrupt_the_superseder_id() {
        // 0063's form: a prose rationale, not a scope.
        let text = "# 0029. CI checks 500-seed censuses\n\n\
                    **Status:** Superseded by 0063 (The Local Census made the full census a ~7-min\n\
                    local run) · **Decider:** Nathan\n";
        let d = parse("0029", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0063"));
    }

    #[test]
    fn every_committed_decision_parses() {
        let dir = repo_root().join("docs/decisions");
        let mut n = 0;
        for entry in std::fs::read_dir(&dir).expect("decisions dir") {
            let path = entry.expect("entry").path();
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
            let d = parse(&id, &text);
            assert!(!d.title.is_empty(), "{name} produced an empty title");
            n += 1;
        }
        assert!(n >= 112, "expected at least 112 decisions, found {n}");
    }
}

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
    /// Which provisions the supersession covers. `None` means all of them —
    /// so `Some(..)` is a PARTIAL supersession and the rest still governs.
    pub scope: Option<String>,
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

    let (status, superseded_by, scope) = if status_field.starts_with("Superseded") {
        let after = status_field
            .split_once("by")
            .map(|(_, a)| a.trim())
            .unwrap_or("");

        // The id token is either a bare number (`0063`) or a markdown link
        // (`[0099](0099-worlds-are-version-locked.md)`). A link's own
        // parenthesised target must not be mistaken for a trailing scope
        // parenthetical, so the id is consumed first and the scope search
        // starts only after it.
        let (sup, after_id) = if let Some(bracket_rest) = after.strip_prefix('[') {
            let close_bracket = bracket_rest.find(']').unwrap_or(bracket_rest.len());
            let id_str = bracket_rest[..close_bracket].trim().to_string();
            let after_bracket = &bracket_rest[close_bracket.min(bracket_rest.len())..];
            let after_bracket = after_bracket.strip_prefix(']').unwrap_or(after_bracket);
            // Skip the link's own `(target.md)` portion, if present.
            let trimmed = after_bracket.trim_start();
            let after_link = if let Some(link_rest) = trimmed.strip_prefix('(') {
                match link_rest.find(')') {
                    Some(close_paren) => &link_rest[close_paren + 1..],
                    None => link_rest,
                }
            } else {
                trimmed
            };
            let sup = if id_str.is_empty() {
                None
            } else {
                Some(id_str)
            };
            (sup, after_link)
        } else {
            let digit_end = after
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(after.len());
            let id_str = after[..digit_end].to_string();
            let sup = if id_str.is_empty() {
                None
            } else {
                Some(id_str)
            };
            (sup, &after[digit_end..])
        };

        // Whatever parenthetical remains after the id (and, for a link, past
        // its own target parens) is the scope note — the text describing
        // which provisions the supersession covers.
        let scope = {
            let trimmed = after_id.trim_start();
            trimmed.strip_prefix('(').and_then(|rest| {
                rest.rfind(')')
                    .map(|end| rest[..end].trim().to_string())
                    .filter(|s| !s.is_empty())
            })
        };

        (Status::Superseded, sup, scope)
    } else if status_field.starts_with("Proposed") {
        (Status::Proposed, None, None)
    } else {
        (Status::Accepted, None, None)
    };

    DecisionRecord {
        id: id.to_string(),
        title,
        status,
        superseded_by,
        scope,
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
        assert_eq!(d.scope, None, "an unqualified supersession has no scope");
    }

    #[test]
    fn parses_a_bracketed_supersession() {
        let text = "# 0098. Hornvale is single-player\n\n\
                    **Status:** Superseded by [0099](0099-worlds-are-version-locked.md)\n";
        let d = parse("0098", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0099"));
    }

    #[test]
    fn a_partial_supersession_keeps_its_surviving_scope() {
        // THE case this campaign exists for: 0026 reads as superseded to any
        // grepping reader, but its registry-row provision still stands, and
        // the registry violates it 1,402 times.
        let text = "# 0026. Slugs, not numbers\n\n\
                    **Status:** Superseded by 0043 (for decision records; the \
                    study/chronicle/registry-row provisions stand) · **Decider:** Nathan\n";
        let d = parse("0026", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0043"));
        assert_eq!(
            d.scope.as_deref(),
            Some("for decision records; the study/chronicle/registry-row provisions stand"),
            "a partial supersession must retain what survives"
        );
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

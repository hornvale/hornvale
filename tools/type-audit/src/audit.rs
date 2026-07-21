//! Bidirectional coverage: audited positions and tag verdicts must match
//! exactly — no untagged primitive, no stale tag.

use crate::extract::AuditItem;
use crate::tag::{Verdict, parse_tag};
use crate::walk::StreamLabelViolation;

/// One audit failure, anchored to an item and line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Finding {
    /// Item the finding is about.
    pub item: String,
    /// 1-based source line.
    pub line: usize,
    /// Human-readable failure.
    pub message: String,
}

/// Audit one item against its own tag line.
pub fn audit_item(item: &AuditItem) -> Vec<Finding> {
    let verdicts = match parse_tag(&item.doc) {
        Ok(v) => v,
        Err(e) => {
            return vec![Finding {
                item: item.name.clone(),
                line: item.line,
                message: e.message,
            }];
        }
    };

    let mut findings = Vec::new();
    let position_names: Vec<&str> = item.positions.iter().map(|p| p.name.as_str()).collect();

    // Rule 3: a position qualifier that names no audited position is stale.
    for v in &verdicts {
        if let Some(pos) = verdict_position(v)
            && !position_names.contains(&pos)
        {
            findings.push(Finding {
                item: item.name.clone(),
                line: item.line,
                message: format!("stale tag position {pos}"),
            });
        }
    }

    // Rule 2: every audited position needs a covering verdict.
    let has_blanket = verdicts.iter().any(|v| verdict_position(v).is_none());
    for p in &item.positions {
        let covered = has_blanket
            || verdicts
                .iter()
                .any(|v| verdict_position(v) == Some(p.name.as_str()));
        if !covered {
            findings.push(Finding {
                item: item.name.clone(),
                line: p.line,
                message: format!("untagged primitive at {}", p.name),
            });
        }
    }

    findings
}

/// Audit every item, flattening the findings.
pub fn audit_items(items: &[AuditItem]) -> Vec<Finding> {
    items.iter().flat_map(audit_item).collect()
}

/// Turn inline `StreamLabel::from_static(<literal>)` violations into findings
/// (the same shape `audit_items` produces, so `check`/`report` handle both
/// checks identically).
pub fn stream_label_findings(violations: &[StreamLabelViolation]) -> Vec<Finding> {
    violations
        .iter()
        .map(|v| Finding {
            item: v.path.display().to_string(),
            line: v.line,
            message: "inline StreamLabel::from_static(<literal>) outside streams.rs".to_string(),
        })
        .collect()
}

/// The position qualifier of a verdict, if any (uniform across variants).
fn verdict_position(v: &Verdict) -> Option<&str> {
    match v {
        Verdict::BareOk { position, .. }
        | Verdict::Waiver { position, .. }
        | Verdict::Pending { position, .. } => position.as_deref(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extract::Position;

    fn item(doc: &str, positions: &[&str]) -> AuditItem {
        AuditItem {
            name: "thing".into(),
            doc: doc.into(),
            line: 1,
            positions: positions
                .iter()
                .map(|n| Position {
                    name: n.to_string(),
                    line: 2,
                })
                .collect(),
        }
    }

    #[test]
    fn position_less_verdict_covers_all() {
        let i = item("type-audit: bare-ok(count)", &["a", "b"]);
        assert!(audit_item(&i).is_empty());
    }

    #[test]
    fn uncovered_position_is_a_finding() {
        let i = item("type-audit: bare-ok(count: a)", &["a", "b"]);
        let f = audit_item(&i);
        assert_eq!(f.len(), 1);
        assert!(f[0].message.contains("untagged primitive at b"));
    }

    #[test]
    fn stale_position_qualifier_is_a_finding() {
        let i = item("type-audit: bare-ok(count: ghost)", &["a"]);
        let f = audit_item(&i);
        // "a" uncovered AND "ghost" stale → two findings.
        assert!(
            f.iter()
                .any(|x| x.message.contains("stale tag position ghost"))
        );
        assert!(
            f.iter()
                .any(|x| x.message.contains("untagged primitive at a"))
        );
    }

    #[test]
    fn malformed_tag_is_a_finding() {
        let i = item("type-audit: bare-ok(nonsense)", &["a"]);
        assert_eq!(audit_item(&i).len(), 1);
    }

    #[test]
    fn missing_tag_is_a_finding() {
        let i = item("just prose", &["a"]);
        let f = audit_item(&i);
        assert_eq!(f.len(), 1);
        assert!(f[0].message.contains("untagged primitive at a"));
    }
}

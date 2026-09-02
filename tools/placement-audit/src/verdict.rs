//! The three-valued placement verdict: for each member of a shape twin,
//! whether it is UNTAGGED (no `placement:` tag at all), STALE (a tag exists
//! but its fingerprint is missing or has moved), or current (tagged and
//! matching — no finding at all). A malformed tag is reported separately, as
//! a parse error, so a broken tag is never silently read as either.

use crate::detect::TwinGroup;
use crate::extract::TypeShape;
use crate::fingerprint;
use crate::tag;
use std::path::PathBuf;

/// What kind of defect a [`Finding`] reports.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FindingKind {
    /// The type carries no `placement:` tag.
    Untagged,
    /// A tag exists but its `shape(...)` is missing or no longer matches
    /// the type's current fingerprint.
    Stale,
    /// The tag's own grammar failed to parse.
    ParseError,
}

/// One actionable verdict-engine finding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Finding {
    /// The kind of defect.
    pub kind: FindingKind,
    /// Crate the offending type lives in.
    pub crate_name: String,
    /// The offending type's identifier.
    pub type_name: String,
    /// Source file the type was found in.
    pub file: PathBuf,
    /// 1-based source line of the type's identifier.
    pub line: usize,
    /// A human-actionable description of the defect and how to fix it.
    pub message: String,
}

fn sort_key(f: &Finding) -> (&str, &std::path::Path, usize) {
    (f.crate_name.as_str(), f.file.as_path(), f.line)
}

/// Format one twin member as `crate:Type@file:line`, for naming a twin's
/// other members in an `Untagged` finding's message.
fn member_ref(m: &TypeShape) -> String {
    format!(
        "{}:{}@{}:{}",
        m.crate_name,
        m.name,
        m.file.display(),
        m.line
    )
}

/// Judge every member of every twin group, producing one [`Finding`] per
/// member that is untagged, stale, or carries a malformed tag. A member
/// whose tag is present and current produces no finding at all.
///
/// Findings are sorted deterministically by `(crate_name, file, line)`.
pub fn judge(groups: &[TwinGroup]) -> Vec<Finding> {
    let mut findings = Vec::new();
    for group in groups {
        for member in &group.members {
            let others: Vec<String> = group
                .members
                .iter()
                .filter(|m| *m != member)
                .map(member_ref)
                .collect();
            match tag::parse(&member.doc) {
                Ok(None) => findings.push(Finding {
                    kind: FindingKind::Untagged,
                    crate_name: member.crate_name.clone(),
                    type_name: member.name.clone(),
                    file: member.file.clone(),
                    line: member.line,
                    message: format!("no placement: tag — twin of {}", others.join(", ")),
                }),
                Err(e) => findings.push(Finding {
                    kind: FindingKind::ParseError,
                    crate_name: member.crate_name.clone(),
                    type_name: member.name.clone(),
                    file: member.file.clone(),
                    line: member.line,
                    message: format!("placement: tag error: {e}"),
                }),
                Ok(Some(parsed)) => {
                    let current = fingerprint::of(&member.members);
                    match parsed.shape {
                        None => findings.push(Finding {
                            kind: FindingKind::Stale,
                            crate_name: member.crate_name.clone(),
                            type_name: member.name.clone(),
                            file: member.file.clone(),
                            line: member.line,
                            message: format!("tag has no shape(…) — add shape({current})"),
                        }),
                        Some(tagged) if tagged != current => findings.push(Finding {
                            kind: FindingKind::Stale,
                            crate_name: member.crate_name.clone(),
                            type_name: member.name.clone(),
                            file: member.file.clone(),
                            line: member.line,
                            message: format!(
                                "shape moved: tag says {tagged}, type is {current} — \
                                 re-adjudicate and re-fingerprint"
                            ),
                        }),
                        Some(_) => {
                            // Tagged and current: no finding.
                        }
                    }
                }
            }
        }
    }
    findings.sort_by(|a, b| sort_key(a).cmp(&sort_key(b)));
    findings
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extract::ShapeKind;
    use std::path::PathBuf;

    fn shape(crate_name: &str, name: &str, doc: &str, members: &[&str], line: usize) -> TypeShape {
        TypeShape {
            crate_name: crate_name.to_string(),
            name: name.to_string(),
            kind: ShapeKind::Enum,
            members: members.iter().map(|m| m.to_string()).collect(),
            doc: doc.to_string(),
            file: PathBuf::from(format!("domains/{crate_name}/src/lib.rs")),
            line,
        }
    }

    #[test]
    fn an_untagged_member_is_a_finding_naming_the_twins_other_members() {
        let group = TwinGroup {
            members: vec![
                shape("a", "Mood", "", &["Bright", "Dim"], 3),
                shape("b", "Temper", "", &["Bright", "Dim"], 9),
            ],
        };
        let findings = judge(&[group]);
        assert_eq!(findings.len(), 2);
        assert!(findings.iter().all(|f| f.kind == FindingKind::Untagged));
        assert!(
            findings[0]
                .message
                .contains("b:Temper@domains/b/src/lib.rs:9")
        );
        assert!(
            findings[1]
                .message
                .contains("a:Mood@domains/a/src/lib.rs:3")
        );
    }

    #[test]
    fn a_tag_with_no_shape_is_stale_and_prints_the_computed_fingerprint() {
        let members = ["Bright", "Dim"];
        let fp = fingerprint::of(&members.iter().map(|m| m.to_string()).collect::<Vec<_>>());
        let group = TwinGroup {
            members: vec![
                shape(
                    "a",
                    "Mood",
                    "placement: deliberate(kept apart)",
                    &members,
                    3,
                ),
                shape(
                    "b",
                    "Temper",
                    "placement: deliberate(kept apart)",
                    &members,
                    9,
                ),
            ],
        };
        let findings = judge(&[group]);
        assert_eq!(findings.len(), 2);
        for f in &findings {
            assert_eq!(f.kind, FindingKind::Stale);
            assert!(
                f.message.contains(&fp),
                "message should print {fp}: {}",
                f.message
            );
        }
    }

    #[test]
    fn a_mismatched_shape_is_stale_and_prints_both_values() {
        let members = ["Bright", "Dim"];
        let doc = "placement: deliberate(kept apart) shape(000000)";
        let group = TwinGroup {
            members: vec![
                shape("a", "Mood", doc, &members, 3),
                shape("b", "Temper", doc, &members, 9),
            ],
        };
        let findings = judge(&[group]);
        assert_eq!(findings.len(), 2);
        let fp = fingerprint::of(&members.iter().map(|m| m.to_string()).collect::<Vec<_>>());
        for f in &findings {
            assert_eq!(f.kind, FindingKind::Stale);
            assert!(f.message.contains("000000"));
            assert!(f.message.contains(&fp));
        }
    }

    #[test]
    fn a_current_tag_produces_no_finding() {
        let members = ["Bright", "Dim"];
        let fp = fingerprint::of(&members.iter().map(|m| m.to_string()).collect::<Vec<_>>());
        let doc = format!("placement: deliberate(kept apart) shape({fp})");
        let group = TwinGroup {
            members: vec![
                shape("a", "Mood", &doc, &members, 3),
                shape("b", "Temper", &doc, &members, 9),
            ],
        };
        assert!(judge(&[group]).is_empty());
    }

    #[test]
    fn a_malformed_tag_is_a_parse_error_finding() {
        let group = TwinGroup {
            members: vec![
                shape(
                    "a",
                    "Mood",
                    "placement: deliberate()",
                    &["Bright", "Dim"],
                    3,
                ),
                shape(
                    "b",
                    "Temper",
                    "placement: deliberate()",
                    &["Bright", "Dim"],
                    9,
                ),
            ],
        };
        let findings = judge(&[group]);
        assert_eq!(findings.len(), 2);
        assert!(findings.iter().all(|f| f.kind == FindingKind::ParseError));
    }

    #[test]
    fn findings_sort_by_crate_then_file_then_line() {
        let group_one = TwinGroup {
            members: vec![
                shape("z", "Late", "", &["x", "y"], 1),
                shape("m", "Mid", "", &["x", "y"], 1),
            ],
        };
        let group_two = TwinGroup {
            members: vec![
                shape("m", "Other", "", &["p", "q"], 50),
                shape("a", "Early", "", &["p", "q"], 1),
            ],
        };
        let findings = judge(&[group_one, group_two]);
        let crates: Vec<&str> = findings.iter().map(|f| f.crate_name.as_str()).collect();
        let mut sorted = crates.clone();
        sorted.sort();
        assert_eq!(crates, sorted);
    }
}

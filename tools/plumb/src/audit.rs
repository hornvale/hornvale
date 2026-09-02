//! Turning a scanned constant and its tag into a verdict.
//!
//! The audit distinguishes two very different states that both mean "this
//! constant has no rung":
//!
//! - **undeclared** — nobody has judged it yet. That is *backlog*, and it is
//!   the state 610-odd constants are in on the day this tool ships.
//! - **malformed** — somebody wrote a tag and wrote it wrong. That is a
//!   *defect*, and it is true of zero constants today.
//!
//! Task 4's ratchet needs the two apart, because it may close on the second
//! long before it closes on the first.

use crate::tag::{Rung, TagErrorKind, parse_tag_full};
use crate::walk::{AuthoredConst, Scan};

/// What the audit decided about one constant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Judgement {
    /// A well-formed tag naming a rung.
    Declared(Rung),
    /// No `plumb:` line at all — backlog.
    Undeclared,
    /// A tag that could not be parsed — a defect, with its reason.
    Malformed(String),
}

/// One line of `check` output, anchored to a file and line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Finding {
    /// Path of the file, as scanned.
    pub path: String,
    /// 1-based source line of the constant.
    pub line: usize,
    /// The constant's identifier.
    pub name: String,
    /// Human-readable statement of what is missing or wrong.
    pub message: String,
    /// True when this is a malformed tag rather than a merely absent one.
    pub malformed: bool,
}

impl Finding {
    /// The single diagnostic line, in the `path:line:` shape every other tool
    /// in this repo prints.
    pub fn render(&self) -> String {
        format!(
            "{}:{}: {} ({})",
            self.path, self.line, self.message, self.name
        )
    }
}

/// Judge one constant against its own doc comment.
pub fn judge(c: &AuthoredConst) -> Judgement {
    match parse_tag_full(&c.doc) {
        Ok(tag) => Judgement::Declared(tag.rung),
        Err(e) if e.kind == TagErrorKind::Missing => Judgement::Undeclared,
        Err(e) => Judgement::Malformed(e.message),
    }
}

/// Every constant that carries no usable rung, sorted for a stable diff.
pub fn findings(scan: &Scan) -> Vec<Finding> {
    let mut out: Vec<Finding> = scan
        .consts
        .iter()
        .filter_map(|c| match judge(c) {
            Judgement::Declared(_) => None,
            Judgement::Undeclared => Some(Finding {
                path: c.path.display().to_string(),
                line: c.line,
                name: c.name.clone(),
                message: format!("undeclared numeric const ({})", c.ty),
                malformed: false,
            }),
            Judgement::Malformed(why) => Some(Finding {
                path: c.path.display().to_string(),
                line: c.line,
                name: c.name.clone(),
                message: format!("malformed plumb tag: {why}"),
                malformed: true,
            }),
        })
        .collect();
    out.sort_by(|a, b| (&a.path, a.line, &a.name).cmp(&(&b.path, b.line, &b.name)));
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::walk::Site;
    use std::path::PathBuf;

    fn c(doc: &str) -> AuthoredConst {
        AuthoredConst {
            crate_name: "probe".into(),
            path: PathBuf::from("domains/probe/src/lib.rs"),
            line: 7,
            name: "THING".into(),
            ty: "f64".into(),
            doc: doc.into(),
            site: Site::File,
            kind_adjacent: false,
        }
    }

    #[test]
    fn a_well_formed_tag_is_declared() {
        assert_eq!(
            judge(&c("plumb: per-species(sleep debt is a body property)")),
            Judgement::Declared(Rung::PerSpecies)
        );
        assert_eq!(
            judge(&c("plumb: pending(wave-1)")),
            Judgement::Declared(Rung::Pending("wave-1".into()))
        );
    }

    /// "Nobody judged this yet" and "somebody wrote a broken tag" are different
    /// facts, and collapsing them would let a typo hide inside a backlog of
    /// hundreds.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): collapse the two
    /// arms — replace both `Err(...)` arms of `judge` with
    /// `Err(_) => Judgement::Undeclared,`.
    ///
    /// ```text
    /// assertion failed: matches!(judge(&c("plumb: universal")), Judgement::Malformed(_))
    /// ```
    #[test]
    fn an_absent_tag_and_a_broken_tag_are_different_judgements() {
        assert_eq!(judge(&c("Just prose.")), Judgement::Undeclared);
        assert!(matches!(
            judge(&c("plumb: universal")),
            Judgement::Malformed(_)
        ));
        assert!(matches!(
            judge(&c("plumb: frobnicate(x)")),
            Judgement::Malformed(_)
        ));
    }

    #[test]
    fn findings_carry_the_distinction_and_sort_stably() {
        let scan = Scan {
            consts: vec![
                AuthoredConst {
                    line: 20,
                    name: "B".into(),
                    doc: "plumb: universal".into(),
                    ..c("")
                },
                AuthoredConst {
                    line: 10,
                    name: "A".into(),
                    ..c("")
                },
                AuthoredConst {
                    name: "OK".into(),
                    doc: "plumb: universal(a lattice constant)".into(),
                    ..c("")
                },
            ],
            ..Scan::default()
        };
        let f = findings(&scan);
        assert_eq!(f.len(), 2, "the declared const produces no finding");
        assert_eq!(f[0].name, "A");
        assert!(!f[0].malformed);
        assert_eq!(f[1].name, "B");
        assert!(f[1].malformed);
        assert!(f[1].render().starts_with("domains/probe/src/lib.rs:20:"));
    }
}

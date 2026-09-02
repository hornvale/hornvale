//! Parsing the `placement:` adjudication tag inside a shape twin's doc
//! comment.
//!
//! A twin's placement is adjudicated once, where the type is defined, so the
//! declaration cannot drift away from the thing it describes — the same
//! discipline `type-audit:` and `seam-guard:` tags use. The tag states the
//! two things the tool cannot infer: which of the three adjudications
//! applies (promote to one shared type / stay duplicated on purpose / not
//! decided yet), and the shape fingerprint the adjudication was made
//! against, so a later signature change is caught as stale rather than
//! silently trusted forever.

/// Which adjudication a `placement:` tag records for a shape twin.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TagVerdict {
    /// The twin should be promoted to a single shared type. Carries the
    /// decision or spec anchor that tracks the promotion.
    Promote(String),
    /// The duplication is a deliberate, reasoned choice. Carries the reason
    /// it stays duplicated.
    Deliberate(String),
    /// Adjudication is deferred. Carries the reason it is not decided yet.
    Deferred(String),
}

/// A parsed `placement:` tag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlacementTag {
    /// The recorded adjudication.
    pub verdict: TagVerdict,
    /// The shape fingerprint the adjudication was made against, if given.
    /// Absent means the tag has never been fingerprinted at all — a `Stale`
    /// finding distinct from a fingerprint that has since moved.
    pub shape: Option<String>,
}

/// Errors a malformed `placement:` tag can produce. A tag is a claim that a
/// twin has been looked at; a silently-ignored malformed one would let a
/// broken claim stand in for a real adjudication, so every parse failure is
/// loud.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TagError {
    /// None of `promote`/`deliberate`/`deferred` was given.
    MissingVerdict,
    /// The same clause name appeared twice in one tag — including two
    /// *different* verdict clauses (`promote(...) deliberate(...)`), since a
    /// tag records exactly one adjudication and the tool would otherwise
    /// silently honour whichever came last.
    DuplicateClause(String),
    /// A `name(` that never closed.
    UnbalancedParens(String),
    /// A verdict clause carrying no reason/anchor.
    ///
    /// A reasonless acknowledgement is the exact failure this whole tool
    /// exists to catch, one level up: it silences the finding while
    /// recording nothing about why, so nobody can ever tell whether the
    /// adjudication is still sound.
    EmptyReason,
    /// `shape(...)` whose argument was not exactly 6 lowercase hex digits.
    BadShape(String),
}

impl std::fmt::Display for TagError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingVerdict => write!(
                f,
                "no verdict: expected promote(...), deliberate(...), or deferred(...)"
            ),
            Self::DuplicateClause(s) => {
                write!(f, "clause {s:?} given more than once in one tag")
            }
            Self::UnbalancedParens(s) => write!(f, "unbalanced parentheses in {s:?}"),
            Self::EmptyReason => write!(
                f,
                "the verdict needs a reason or anchor — an unexplained acknowledgement \
                 silences the finding while recording nothing about why"
            ),
            Self::BadShape(s) => write!(f, "shape(...) is not 6 lowercase hex digits: {s:?}"),
        }
    }
}

/// The marker that opens a placement tag.
pub const MARKER: &str = "placement:";

/// Extract `name(...)` clauses from a tag body, respecting nesting so a
/// parenthesised anchor or reason survives intact.
fn clauses(body: &str) -> Result<Vec<(String, String)>, TagError> {
    let chars: Vec<char> = body.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        if !(chars[i].is_ascii_alphanumeric() || chars[i] == '-' || chars[i] == '_') {
            i += 1;
            continue;
        }
        let name_start = i;
        while i < chars.len()
            && (chars[i].is_ascii_alphanumeric() || chars[i] == '-' || chars[i] == '_')
        {
            i += 1;
        }
        let name: String = chars[name_start..i].iter().collect();
        if i >= chars.len() || chars[i] != '(' {
            continue;
        }
        i += 1;
        let arg_start = i;
        let mut depth = 1usize;
        while i < chars.len() && depth > 0 {
            match chars[i] {
                '(' => depth += 1,
                ')' => depth -= 1,
                _ => {}
            }
            if depth > 0 {
                i += 1;
            }
        }
        if depth != 0 {
            return Err(TagError::UnbalancedParens(name));
        }
        let arg: String = chars[arg_start..i].iter().collect();
        i += 1; // consume the closing paren
        out.push((name, arg.trim().to_string()));
    }
    Ok(out)
}

/// True for exactly 6 lowercase hex digits.
fn is_shape_hex(s: &str) -> bool {
    s.len() == 6
        && s.chars()
            .all(|c| c.is_ascii_digit() || ('a'..='f').contains(&c))
}

/// Collapse a doc comment's own line-wrapping inside a reason/anchor: a
/// value spanning several `///` lines otherwise carries their indentation
/// into a rendered finding, where it reads as ragged noise.
fn collapse_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Parse the portion of a doc comment after [`MARKER`].
///
/// Returns `Ok(None)` when the text carries no marker at all — that is an
/// ordinary doc comment, not an error.
pub fn parse(doc: &str) -> Result<Option<PlacementTag>, TagError> {
    let Some(idx) = doc.find(MARKER) else {
        return Ok(None);
    };
    // The tag is ONE PARAGRAPH. Stop at the first blank doc line so ordinary
    // prose below it cannot be read as clauses (seam-guard's rule, adapted:
    // `doc_text` joins doc lines with '\n', so a blank `///` line is '\n\n').
    let rest = &doc[idx + MARKER.len()..];
    let body = match rest.find("\n\n") {
        Some(end) => &rest[..end],
        None => rest,
    };
    let found = clauses(body)?;

    let mut verdict: Option<TagVerdict> = None;
    let mut shape: Option<String> = None;
    let mut seen_verdict = false;
    let mut seen_shape = false;
    for (name, arg) in found {
        match name.as_str() {
            "promote" | "deliberate" | "deferred" => {
                if seen_verdict {
                    return Err(TagError::DuplicateClause(name));
                }
                seen_verdict = true;
                let reason = collapse_whitespace(arg.trim());
                if reason.is_empty() {
                    return Err(TagError::EmptyReason);
                }
                verdict = Some(match name.as_str() {
                    "promote" => TagVerdict::Promote(reason),
                    "deliberate" => TagVerdict::Deliberate(reason),
                    _ => TagVerdict::Deferred(reason),
                });
            }
            "shape" => {
                if seen_shape {
                    return Err(TagError::DuplicateClause(name));
                }
                seen_shape = true;
                let s = arg.trim().to_string();
                if !is_shape_hex(&s) {
                    return Err(TagError::BadShape(s));
                }
                shape = Some(s);
            }
            _ => {}
        }
    }

    let verdict = verdict.ok_or(TagError::MissingVerdict)?;
    Ok(Some(PlacementTag { verdict, shape }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_ordinary_doc_comment_is_not_a_tag() {
        assert_eq!(parse("Just prose about the type.").unwrap(), None);
    }

    #[test]
    fn promote_parses_with_shape() {
        let p = parse("placement: promote(spec anchor S-9) shape(a1b2c3)")
            .unwrap()
            .unwrap();
        assert_eq!(
            p.verdict,
            TagVerdict::Promote("spec anchor S-9".to_string())
        );
        assert_eq!(p.shape.as_deref(), Some("a1b2c3"));
    }

    #[test]
    fn deliberate_parses_without_shape() {
        let p = parse("placement: deliberate(kept apart on purpose)")
            .unwrap()
            .unwrap();
        assert_eq!(
            p.verdict,
            TagVerdict::Deliberate("kept apart on purpose".to_string())
        );
        assert_eq!(p.shape, None);
    }

    #[test]
    fn deferred_parses() {
        let p = parse("placement: deferred(not looked at yet) shape(000000)")
            .unwrap()
            .unwrap();
        assert_eq!(
            p.verdict,
            TagVerdict::Deferred("not looked at yet".to_string())
        );
        assert_eq!(p.shape.as_deref(), Some("000000"));
    }

    #[test]
    fn a_tag_without_a_verdict_is_an_error() {
        assert_eq!(
            parse("placement: shape(a1b2c3)"),
            Err(TagError::MissingVerdict)
        );
    }

    #[test]
    fn a_reasonless_verdict_is_an_error() {
        // The whole point of the tag is the reason/anchor. Without one it is
        // a silencer, which is the defect this tool exists to find.
        assert_eq!(
            parse("placement: deliberate() shape(a1b2c3)"),
            Err(TagError::EmptyReason)
        );
    }

    #[test]
    fn two_different_verdict_clauses_is_a_duplicate_error() {
        // A tag records exactly one adjudication, so two verdict clauses —
        // even two different ones — are the same defect as repeating one:
        // the tool would otherwise silently honour whichever came last.
        assert_eq!(
            parse("placement: promote(x) deliberate(y) shape(a1b2c3)"),
            Err(TagError::DuplicateClause("deliberate".to_string()))
        );
    }

    #[test]
    fn a_duplicate_shape_clause_is_an_error() {
        assert_eq!(
            parse("placement: promote(x) shape(a1b2c3) shape(d4e5f6)"),
            Err(TagError::DuplicateClause("shape".to_string()))
        );
    }

    #[test]
    fn a_malformed_shape_is_an_error() {
        assert_eq!(
            parse("placement: promote(x) shape(ZZZZZZ)"),
            Err(TagError::BadShape("ZZZZZZ".to_string()))
        );
        assert_eq!(
            parse("placement: promote(x) shape(abc)"),
            Err(TagError::BadShape("abc".to_string()))
        );
    }

    #[test]
    fn an_unclosed_clause_is_an_error() {
        assert!(matches!(
            parse("placement: promote(x shape(a1b2c3)"),
            Err(TagError::UnbalancedParens(_))
        ));
    }

    #[test]
    fn prose_after_a_blank_line_cannot_overwrite_the_tag() {
        // The exact defect seam-guard's own rule was added for: explanatory
        // prose below the tag naming a clause it was telling you to change
        // must not be read back into the parsed tag.
        let p = parse(
            "placement: promote(spec anchor S-9) shape(a1b2c3)\n\
             \n\
             Once promoted, delete the shape(...) clause.",
        )
        .unwrap()
        .unwrap();
        assert_eq!(
            p.verdict,
            TagVerdict::Promote("spec anchor S-9".to_string())
        );
        assert_eq!(p.shape.as_deref(), Some("a1b2c3"));
    }

    #[test]
    fn a_tag_paragraph_may_still_wrap_across_lines() {
        // Wrapping is normal in a doc comment; only a BLANK line ends the tag.
        let p = parse("placement: deliberate(kept apart)\n            shape(a1b2c3)")
            .unwrap()
            .unwrap();
        assert_eq!(p.shape.as_deref(), Some("a1b2c3"));
    }

    #[test]
    fn a_wrapped_reason_loses_the_doc_comments_indentation() {
        let p = parse("placement: deliberate(one\n     two\n     three) shape(a1b2c3)")
            .unwrap()
            .unwrap();
        assert_eq!(
            p.verdict,
            TagVerdict::Deliberate("one two three".to_string())
        );
    }

    #[test]
    fn returns_ok_none_when_no_marker_is_present_even_amid_other_text() {
        assert_eq!(
            parse("type-audit: bare-ok(count: x)\nplacement mentions nothing here.").unwrap(),
            None
        );
    }
}

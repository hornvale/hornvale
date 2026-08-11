//! Parsing the `seam-guard:` registration line inside a function's doc comment.
//!
//! A seam is registered where it is defined, so the declaration cannot drift
//! away from the thing it describes — the same discipline `type-audit:` tags
//! use. The tag states two things the tool cannot infer: how to neutralise
//! the function in a way that still type-checks, and which tests are supposed
//! to notice.

/// How to neutralise a seam at one of its call sites.
///
/// Both operators are chosen to keep the tree compiling. A mutation that
/// fails to build proves nothing about whether a test would have caught the
/// behaviour, so the tool reports that case separately rather than counting
/// it as a kill.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    /// Replace the whole call with its Nth argument (0-based).
    ///
    /// The operator for unit conversions, clamps, normalisations and
    /// wrappers — anything whose return type matches an argument's. Dropping
    /// it is exactly the defect class that renders wrong without failing.
    Identity(usize),
    /// Replace the whole call with a literal expression (`None`, `0.0`,
    /// `Departure::Climate`).
    ///
    /// The operator for a function whose result is a choice rather than a
    /// transformation of its input.
    Returns(String),
}

/// One registered seam: a function, how to neutralise it, and the test scope
/// that is meant to object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Seam {
    /// Bare function name, as it appears at a call site.
    pub name: String,
    /// The mutation to apply at each call site.
    pub op: Op,
    /// Cargo package whose tests are run to decide KILLED vs SURVIVED.
    pub scope: String,
    /// A declared, reasoned expectation that this seam currently survives.
    ///
    /// Present means "we know nothing guards this, and here is why it is not
    /// being fixed right now". It makes a survivor KNOWN rather than red —
    /// **and it makes an unexpected kill red instead**, which is the half
    /// that stops the declaration rotting. A one-directional acknowledgement
    /// can only ever be satisfied, so it decays into permanent background
    /// noise; this one fails the moment someone adds the missing assertion,
    /// demanding the declaration be deleted in the same change.
    pub expect_survives: Option<String>,
    /// Source file the registration was read from.
    pub file: String,
    /// 1-based line of the `fn` item the tag sits on.
    pub line: usize,
}

/// Errors a malformed tag can produce. A tag is a claim about how to break
/// the code; a silently-ignored one would make the whole tool vacuous, so
/// every parse failure is loud.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TagError {
    /// No recognised operator (`identity(N)` / `returns(EXPR)`).
    MissingOp,
    /// No `scope(<cargo-package>)`.
    MissingScope,
    /// `identity(N)` whose N did not parse as an index.
    BadIdentityIndex(String),
    /// A `name(` that never closed.
    UnbalancedParens(String),
    /// An `expect(...)` whose content was not `survives: <reason>`.
    BadExpect(String),
    /// The same clause given twice in one tag.
    ///
    /// Defence in depth beside the one-paragraph rule: within the paragraph,
    /// a repeated clause means two different claims are being made and the
    /// tool would silently honour whichever came last.
    DuplicateClause(String),
    /// An `expect(survives:)` carrying no reason.
    ///
    /// A reasonless acknowledgement is the exact failure this whole tool
    /// exists to catch, one level up: it silences a finding while recording
    /// nothing about why, so nobody can ever tell whether it is still true.
    EmptyExpectReason,
}

impl std::fmt::Display for TagError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingOp => write!(f, "no operator: expected identity(N) or returns(EXPR)"),
            Self::MissingScope => write!(f, "no scope(<cargo-package>)"),
            Self::BadIdentityIndex(s) => write!(f, "identity index is not a number: {s:?}"),
            Self::UnbalancedParens(s) => write!(f, "unbalanced parentheses in {s:?}"),
            Self::DuplicateClause(s) => {
                write!(f, "clause {s:?} given more than once in one tag")
            }
            Self::BadExpect(s) => {
                write!(f, "expected expect(survives: <reason>), got expect({s:?})")
            }
            Self::EmptyExpectReason => write!(
                f,
                "expect(survives:) needs a reason — an unexplained acknowledgement \
                 silences the finding while recording nothing about why"
            ),
        }
    }
}

/// A parsed registration line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parsed {
    /// The mutation operator.
    pub op: Op,
    /// The cargo package whose tests decide the outcome.
    pub scope: String,
    /// A declared, reasoned expectation that the seam survives.
    pub expect_survives: Option<String>,
}

/// The marker that opens a registration line.
pub const MARKER: &str = "seam-guard:";

/// Extract `name(...)` clauses from a tag body, respecting nesting so
/// `returns(Some(Value::Entity(id)))` survives intact.
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

/// Parse the portion of a doc comment after [`MARKER`].
///
/// Returns `Ok(None)` when the text carries no marker at all — that is an
/// ordinary doc comment, not an error.
pub fn parse(doc: &str) -> Result<Option<Parsed>, TagError> {
    let Some(idx) = doc.find(MARKER) else {
        return Ok(None);
    };
    // The tag is ONE PARAGRAPH. Stop at the first blank doc line so ordinary
    // prose below it cannot be read as clauses.
    //
    // This is not hypothetical tidiness: the first real registration explained
    // itself with the sentence "delete the `expect(survives: …)` clause", and
    // that placeholder — being the LAST `expect(` in the comment — silently
    // replaced the genuine reason with a literal ellipsis. The declaration
    // still parsed, still looked plausible in the roster, and recorded
    // nothing. Caught by reading the generated artifact, not the code.
    let rest = &doc[idx + MARKER.len()..];
    let body = match rest.find("\n\n") {
        Some(end) => &rest[..end],
        None => rest,
    };
    let found = clauses(body)?;

    let mut op = None;
    let mut scope = None;
    let mut expect_survives = None;
    let mut seen: Vec<String> = Vec::new();
    for (name, arg) in found {
        if matches!(name.as_str(), "identity" | "returns" | "scope" | "expect") {
            if seen.contains(&name) {
                return Err(TagError::DuplicateClause(name));
            }
            seen.push(name.clone());
        }
        match name.as_str() {
            "identity" => {
                let n = arg
                    .trim()
                    .parse::<usize>()
                    .map_err(|_| TagError::BadIdentityIndex(arg.clone()))?;
                op = Some(Op::Identity(n));
            }
            "returns" => op = Some(Op::Returns(arg)),
            "scope" => scope = Some(arg),
            "expect" => {
                let rest = arg
                    .trim()
                    .strip_prefix("survives")
                    .and_then(|r| r.trim_start().strip_prefix(':'))
                    .ok_or_else(|| TagError::BadExpect(arg.clone()))?;
                // Collapse the doc comment's own wrapping. A reason spanning
                // several `///` lines otherwise carries their indentation into
                // the committed roster, where it reads as ragged noise.
                let reason = rest.split_whitespace().collect::<Vec<_>>().join(" ");
                if reason.is_empty() {
                    return Err(TagError::EmptyExpectReason);
                }
                expect_survives = Some(reason.to_string());
            }
            _ => {}
        }
    }

    let op = op.ok_or(TagError::MissingOp)?;
    let scope = scope.ok_or(TagError::MissingScope)?;
    Ok(Some(Parsed {
        op,
        scope,
        expect_survives,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_ordinary_doc_comment_is_not_a_tag() {
        assert_eq!(parse("Just prose about the function.").unwrap(), None);
    }

    #[test]
    fn identity_and_scope_parse() {
        let p = parse("seam-guard: identity(0) scope(hornvale-kernel)")
            .unwrap()
            .unwrap();
        assert_eq!(p.op, Op::Identity(0));
        assert_eq!(p.scope, "hornvale-kernel");
    }

    #[test]
    fn returns_keeps_nested_parens() {
        let p = parse("seam-guard: returns(Some(Value::Entity(id))) scope(x)")
            .unwrap()
            .unwrap();
        assert_eq!(p.op, Op::Returns("Some(Value::Entity(id))".to_string()));
    }

    #[test]
    fn a_seam_with_no_expect_clause_declares_nothing() {
        let p = parse("seam-guard: returns(None) scope(x)")
            .unwrap()
            .unwrap();
        assert_eq!(p.expect_survives, None);
    }

    #[test]
    fn expect_survives_captures_its_reason() {
        let p =
            parse("seam-guard: returns(None) scope(x) expect(survives: drift-only, being-fixed)")
                .unwrap()
                .unwrap();
        assert_eq!(
            p.expect_survives.as_deref(),
            Some("drift-only, being-fixed")
        );
    }

    #[test]
    fn an_expect_without_a_reason_is_an_error() {
        // The whole point of the declaration is the reason. Without one it is
        // a silencer, which is the defect this tool exists to find.
        assert_eq!(
            parse("seam-guard: returns(None) scope(x) expect(survives:)"),
            Err(TagError::EmptyExpectReason)
        );
    }

    #[test]
    fn prose_after_a_blank_line_cannot_overwrite_the_declaration() {
        // The exact defect this rule was added for: explanatory prose below
        // the tag named the clause it was telling you to delete, and that
        // placeholder — the last `expect(` in the comment — replaced the real
        // reason with a literal ellipsis.
        let p = parse(
            "seam-guard: returns(None) scope(x) expect(survives: the real reason)\n\
             \n\
             When the guard lands, delete the expect(survives: …) clause.",
        )
        .unwrap()
        .unwrap();
        assert_eq!(p.expect_survives.as_deref(), Some("the real reason"));
    }

    #[test]
    fn a_tag_paragraph_may_still_wrap_across_lines() {
        // Wrapping is normal in a doc comment; only a BLANK line ends the tag.
        let p = parse(
            "seam-guard: returns(None) scope(x)\n            expect(survives: a wrapped reason)",
        )
        .unwrap()
        .unwrap();
        assert_eq!(p.expect_survives.as_deref(), Some("a wrapped reason"));
    }

    #[test]
    fn a_wrapped_reason_loses_the_doc_comments_indentation() {
        let p =
            parse("seam-guard: returns(None) scope(x) expect(survives: one\n     two\n     three)")
                .unwrap()
                .unwrap();
        assert_eq!(p.expect_survives.as_deref(), Some("one two three"));
    }

    #[test]
    fn a_duplicate_clause_is_an_error() {
        assert_eq!(
            parse("seam-guard: returns(None) returns(Some(x)) scope(y)"),
            Err(TagError::DuplicateClause("returns".into()))
        );
    }

    #[test]
    fn an_expect_of_something_other_than_survives_is_an_error() {
        assert!(matches!(
            parse("seam-guard: returns(None) scope(x) expect(killed: whatever)"),
            Err(TagError::BadExpect(_))
        ));
    }

    #[test]
    fn a_tag_without_a_scope_is_an_error() {
        assert_eq!(
            parse("seam-guard: returns(None)"),
            Err(TagError::MissingScope)
        );
    }

    #[test]
    fn a_tag_without_an_operator_is_an_error() {
        assert_eq!(
            parse("seam-guard: scope(hornvale-almanac)"),
            Err(TagError::MissingOp)
        );
    }

    #[test]
    fn a_non_numeric_identity_index_is_an_error() {
        assert!(matches!(
            parse("seam-guard: identity(first) scope(x)"),
            Err(TagError::BadIdentityIndex(_))
        ));
    }

    #[test]
    fn an_unclosed_clause_is_an_error() {
        assert!(matches!(
            parse("seam-guard: returns(None scope(x)"),
            Err(TagError::UnbalancedParens(_))
        ));
    }
}

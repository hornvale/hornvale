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
}

impl std::fmt::Display for TagError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingOp => write!(f, "no operator: expected identity(N) or returns(EXPR)"),
            Self::MissingScope => write!(f, "no scope(<cargo-package>)"),
            Self::BadIdentityIndex(s) => write!(f, "identity index is not a number: {s:?}"),
            Self::UnbalancedParens(s) => write!(f, "unbalanced parentheses in {s:?}"),
        }
    }
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
pub fn parse(doc: &str) -> Result<Option<(Op, String)>, TagError> {
    let Some(idx) = doc.find(MARKER) else {
        return Ok(None);
    };
    let body = &doc[idx + MARKER.len()..];
    let found = clauses(body)?;

    let mut op = None;
    let mut scope = None;
    for (name, arg) in found {
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
            _ => {}
        }
    }

    let op = op.ok_or(TagError::MissingOp)?;
    let scope = scope.ok_or(TagError::MissingScope)?;
    Ok(Some((op, scope)))
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
        let (op, scope) = parse("seam-guard: identity(0) scope(hornvale-kernel)")
            .unwrap()
            .unwrap();
        assert_eq!(op, Op::Identity(0));
        assert_eq!(scope, "hornvale-kernel");
    }

    #[test]
    fn returns_keeps_nested_parens() {
        let (op, _) = parse("seam-guard: returns(Some(Value::Entity(id))) scope(x)")
            .unwrap()
            .unwrap();
        assert_eq!(op, Op::Returns("Some(Value::Entity(id))".to_string()));
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

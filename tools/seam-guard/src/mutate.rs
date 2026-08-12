//! Rewriting one call site in a source file.

use crate::scan::{CallSite, Span2};
use crate::tag::Op;

/// Why a call site could not be mutated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MutateError {
    /// `identity(N)` where the call has no Nth argument.
    NoSuchArgument {
        /// The index the tag asked for.
        wanted: usize,
        /// How many arguments the call actually has.
        found: usize,
    },
    /// A span did not resolve to a byte offset in this file.
    SpanOutOfRange,
}

impl std::fmt::Display for MutateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoSuchArgument { wanted, found } => {
                write!(f, "identity({wanted}) but the call has {found} argument(s)")
            }
            Self::SpanOutOfRange => write!(f, "span did not resolve to a byte offset"),
        }
    }
}

/// Convert a (1-based line, 0-based column) position to a byte offset.
///
/// Columns count *characters*, not bytes — this tree's source is full of
/// em-dashes and typographic quotes in comments, and a byte-indexed column
/// would land mid-codepoint and panic on the slice.
fn offset_of(src: &str, (line, col): (usize, usize)) -> Option<usize> {
    let mut offset = 0usize;
    for (i, l) in src.split_inclusive('\n').enumerate() {
        if i + 1 == line {
            let mut chars = 0usize;
            for (byte_idx, _) in l.char_indices() {
                if chars == col {
                    return Some(offset + byte_idx);
                }
                chars += 1;
            }
            return if chars == col {
                Some(offset + l.len())
            } else {
                None
            };
        }
        offset += l.len();
    }
    None
}

/// Byte range of a span within `src`.
fn range_of(src: &str, span: Span2) -> Option<(usize, usize)> {
    let start = offset_of(src, span.start)?;
    let end = offset_of(src, span.end)?;
    if end < start {
        None
    } else {
        Some((start, end))
    }
}

/// Produce the mutated text of `src` with one call site neutralised.
///
/// The replacement is wrapped in parentheses so it cannot re-associate with
/// surrounding operators: substituting `a + b` for `f(x)` inside `f(x) * 2`
/// would otherwise silently change the arithmetic and make the result a test
/// of the tool's own bug rather than of the seam.
pub fn apply(src: &str, site: &CallSite, op: &Op) -> Result<String, MutateError> {
    let (call_start, call_end) = range_of(src, site.call).ok_or(MutateError::SpanOutOfRange)?;

    let replacement = match op {
        Op::Returns(expr) => format!("({expr})"),
        Op::Identity(n) => {
            let span = site.args.get(*n).ok_or(MutateError::NoSuchArgument {
                wanted: *n,
                found: site.args.len(),
            })?;
            let (s, e) = range_of(src, *span).ok_or(MutateError::SpanOutOfRange)?;
            format!("({})", &src[s..e])
        }
    };

    let mut out = String::with_capacity(src.len() + replacement.len());
    out.push_str(&src[..call_start]);
    out.push_str(&replacement);
    out.push_str(&src[call_end..]);
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scan::calls_in;
    use std::path::Path;

    fn site_of(src: &str, name: &str) -> CallSite {
        let sites = calls_in(Path::new("t.rs"), src, name);
        assert_eq!(sites.len(), 1, "fixture must have exactly one call site");
        sites.into_iter().next().unwrap()
    }

    #[test]
    fn identity_replaces_the_call_with_its_argument() {
        let src = "fn m() -> f64 { to_days(years) }\n";
        let site = site_of(src, "to_days");
        let out = apply(src, &site, &Op::Identity(0)).unwrap();
        assert_eq!(out, "fn m() -> f64 { (years) }\n");
    }

    #[test]
    fn identity_can_select_a_later_argument() {
        let src = "fn m() -> f64 { clamp(lo, value, hi) }\n";
        let site = site_of(src, "clamp");
        let out = apply(src, &site, &Op::Identity(1)).unwrap();
        assert_eq!(out, "fn m() -> f64 { (value) }\n");
    }

    #[test]
    fn returns_replaces_the_whole_call() {
        let src = "fn m() { let v = victim(world, r); }\n";
        let site = site_of(src, "victim");
        let out = apply(src, &site, &Op::Returns("None".into())).unwrap();
        assert_eq!(out, "fn m() { let v = (None); }\n");
    }

    #[test]
    fn the_replacement_is_parenthesised_so_precedence_cannot_shift() {
        let src = "fn m() -> f64 { scale(a + b) * 2.0 }\n";
        let site = site_of(src, "scale");
        let out = apply(src, &site, &Op::Identity(0)).unwrap();
        // Without the parens this would be `a + b * 2.0` — a different sum,
        // and the tool would be measuring its own defect.
        assert_eq!(out, "fn m() -> f64 { (a + b) * 2.0 }\n");
    }

    #[test]
    fn a_multiline_call_is_replaced_whole() {
        let src = "fn m() -> f64 {\n    to_days(\n        years,\n    )\n}\n";
        let site = site_of(src, "to_days");
        let out = apply(src, &site, &Op::Identity(0)).unwrap();
        assert_eq!(out, "fn m() -> f64 {\n    (years)\n}\n");
    }

    #[test]
    fn non_ascii_earlier_in_the_line_does_not_misalign_the_rewrite() {
        // Columns are character counts; a byte-indexed implementation slices
        // mid-codepoint here and panics or corrupts the file.
        let src = "fn m() -> f64 { /* — a dash — */ to_days(years) }\n";
        let site = site_of(src, "to_days");
        let out = apply(src, &site, &Op::Identity(0)).unwrap();
        assert_eq!(out, "fn m() -> f64 { /* — a dash — */ (years) }\n");
    }

    #[test]
    fn an_out_of_range_identity_index_is_an_error_not_a_panic() {
        let src = "fn m() -> f64 { to_days(years) }\n";
        let site = site_of(src, "to_days");
        assert_eq!(
            apply(src, &site, &Op::Identity(3)),
            Err(MutateError::NoSuchArgument {
                wanted: 3,
                found: 1
            })
        );
    }

    #[test]
    fn a_call_inside_a_test_fn_is_not_a_site() {
        let src = "#[test]\nfn t() { to_days(years); }\n";
        assert!(calls_in(Path::new("t.rs"), src, "to_days").is_empty());
    }

    #[test]
    fn a_call_inside_a_cfg_test_module_is_not_a_site() {
        let src = "#[cfg(test)]\nmod tests {\n    fn helper() { to_days(years); }\n}\n";
        assert!(calls_in(Path::new("t.rs"), src, "to_days").is_empty());
    }
}

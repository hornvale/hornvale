//! Scanning for inline `StreamLabel::from_static(<literal>)` call sites.
//!
//! The campaign convention (proc-17) is that every real stream label is a
//! named `pub const` declared in a crate's own `streams.rs`, constructed via
//! `StreamLabel::from_static`; other code references the constant. A literal
//! string argument to `from_static` found anywhere else has bypassed that
//! discipline — this module finds those call sites.
//!
//! This scans the raw token stream rather than a parsed [`syn::File`]: a
//! `syn` AST leaves the inside of any macro invocation (e.g. the `vec![...]`
//! a `streams.rs`-style helper commonly builds) as an opaque, unparsed
//! `TokenStream`, so an AST-only walk would miss a literal call nested
//! inside one. Token trees don't have that blind spot — a macro's body is
//! just more tokens — so a literal `from_static` call is found wherever it
//! textually appears, matched purely as the token sequence
//! `StreamLabel :: from_static ( <one literal token> )`. `StreamLabel::dynamic`
//! calls and `from_static` calls passed a named constant (an identifier
//! token, not a literal) are never flagged.
//!
//! `#[cfg(test)] mod { … }` blocks are exempt too, matching the precedent
//! [`crate::extract`] already sets for the tag-audit check: a test
//! comparing `derive(StreamLabel::from_static("x"))` against
//! `derive(StreamLabel::dynamic("x"))` for parity, or a `_TEST` const kept
//! as a deliberately independent literal copy to catch drift, is testing
//! *behavior*, not declaring a save-format-relevant label — it has no
//! "real declared constant" to point at instead.

use crate::extract::has_cfg_test;
use proc_macro2::{TokenStream, TokenTree};

/// One `StreamLabel::from_static(<string-literal>)` call site, by line.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiteralCall {
    /// 1-based source line of the call.
    pub line: usize,
}

/// Find every `StreamLabel::from_static(...)` call in `src` whose sole
/// argument is a string-literal token (not an identifier or const path).
/// Errors only if `src` fails to tokenize as Rust (should not happen for a
/// file that already parsed with `syn::parse_file`).
pub fn from_static_literal_calls(src: &str) -> Result<Vec<LiteralCall>, String> {
    let tokens: TokenStream = src.parse().map_err(|e| format!("{e}"))?;
    let mut found = Vec::new();
    scan(tokens, &mut found);
    Ok(found)
}

/// [`from_static_literal_calls`], minus any call whose line falls inside a
/// `#[cfg(test)] mod { … }` block in the already-parsed `file` (the same
/// source `src` was parsed from). This is the check `walk::scan` actually
/// wires up — production code only.
pub fn from_static_literal_calls_outside_tests(
    src: &str,
    file: &syn::File,
) -> Result<Vec<LiteralCall>, String> {
    let test_ranges = cfg_test_line_ranges(file);
    let calls = from_static_literal_calls(src)?;
    Ok(calls
        .into_iter()
        .filter(|c| {
            !test_ranges
                .iter()
                .any(|&(start, end)| (start..=end).contains(&c.line))
        })
        .collect())
}

/// Line ranges `[start, end]` (inclusive, 1-based) covered by every
/// `#[cfg(test)] mod { … }` block in `file`, at any nesting depth.
fn cfg_test_line_ranges(file: &syn::File) -> Vec<(usize, usize)> {
    let mut ranges = Vec::new();
    collect_test_mod_ranges(&file.items, &mut ranges);
    ranges
}

fn collect_test_mod_ranges(items: &[syn::Item], out: &mut Vec<(usize, usize)>) {
    use syn::spanned::Spanned;
    for item in items {
        let syn::Item::Mod(m) = item else { continue };
        let Some((brace, inner)) = &m.content else {
            continue; // `mod foo;` (external file) — nothing to scan here
        };
        if has_cfg_test(&m.attrs) {
            let start = m.mod_token.span().start().line;
            let end = brace.span.close().start().line;
            out.push((start, end));
        } else {
            collect_test_mod_ranges(inner, out);
        }
    }
}

/// Recursively scan `tokens`, descending into every group (block, macro
/// body, parenthesized/bracketed expression — all of them, uniformly).
fn scan(tokens: TokenStream, found: &mut Vec<LiteralCall>) {
    let trees: Vec<TokenTree> = tokens.into_iter().collect();
    for i in 0..trees.len() {
        if let Some(line) = from_static_call_at(&trees, i) {
            found.push(LiteralCall { line });
        }
        if let TokenTree::Group(g) = &trees[i] {
            scan(g.stream(), found);
        }
    }
}

/// If a `StreamLabel :: from_static ( <literal> )` sequence starts at index
/// `i`, return its source line.
fn from_static_call_at(trees: &[TokenTree], i: usize) -> Option<usize> {
    let type_name = as_ident(trees.get(i)?, "StreamLabel")?;
    is_colon(trees.get(i + 1)?)?;
    is_colon(trees.get(i + 2)?)?;
    as_ident(trees.get(i + 3)?, "from_static")?;
    let TokenTree::Group(args) = trees.get(i + 4)? else {
        return None;
    };
    if args.delimiter() != proc_macro2::Delimiter::Parenthesis {
        return None;
    }
    let arg_trees: Vec<TokenTree> = args.stream().into_iter().collect();
    let [TokenTree::Literal(lit)] = arg_trees.as_slice() else {
        return None; // zero args, multiple args, or a non-literal argument
    };
    if matches!(syn::Lit::new(lit.clone()), syn::Lit::Str(_)) {
        Some(type_name.start().line)
    } else {
        None
    }
}

fn as_ident(tt: &TokenTree, expected: &str) -> Option<proc_macro2::Span> {
    match tt {
        TokenTree::Ident(id) if id == expected => Some(id.span()),
        _ => None,
    }
}

fn is_colon(tt: &TokenTree) -> Option<()> {
    match tt {
        TokenTree::Punct(p) if p.as_char() == ':' => Some(()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flags_a_string_literal_argument() {
        let src = r#"
            fn make() -> StreamLabel<'static> {
                StreamLabel::from_static("room/face")
            }
        "#;
        let found = from_static_literal_calls(src).unwrap();
        assert_eq!(found.len(), 1);
    }

    #[test]
    fn does_not_flag_a_named_constant_argument() {
        let src = r#"
            const LABEL_TEXT: &str = "room/face";
            fn make() -> StreamLabel<'static> {
                StreamLabel::from_static(LABEL_TEXT)
            }
        "#;
        assert!(from_static_literal_calls(src).unwrap().is_empty());
    }

    #[test]
    fn does_not_flag_dynamic_calls() {
        let src = r#"
            fn make(name: &str) -> StreamLabel<'_> {
                StreamLabel::dynamic(name)
            }
        "#;
        assert!(from_static_literal_calls(src).unwrap().is_empty());
    }

    #[test]
    fn finds_calls_nested_inside_a_macro_invocation() {
        // `vec![...]` is exactly the shape a real `streams.rs`'s
        // `stream_labels()` helper uses — its body must not be a blind spot.
        let src = r#"
            fn make() {
                let pairs = vec![("a", StreamLabel::from_static("nested/literal"))];
            }
        "#;
        assert_eq!(from_static_literal_calls(src).unwrap().len(), 1);
    }

    #[test]
    fn reports_the_correct_line() {
        let src = "fn make() {\n    let x = 1;\n    StreamLabel::from_static(\"line-three\");\n}\n";
        let found = from_static_literal_calls(src).unwrap();
        assert_eq!(found, vec![LiteralCall { line: 3 }]);
    }

    #[test]
    fn does_not_flag_a_qualified_path_to_a_different_method() {
        let src = r#"
            fn make() {
                StreamLabel::not_from_static("x");
            }
        "#;
        assert!(from_static_literal_calls(src).unwrap().is_empty());
    }

    #[test]
    fn raw_scan_sees_a_literal_inside_cfg_test_but_the_outside_tests_scan_does_not() {
        let src = r#"
            fn real() -> StreamLabel<'static> {
                StreamLabel::from_static(LABEL)
            }

            #[cfg(test)]
            mod tests {
                #[test]
                fn round_trips() {
                    let x = StreamLabel::from_static("astronomy");
                }
            }
        "#;
        let file: syn::File = syn::parse_file(src).unwrap();

        // The raw scan is intentionally blind to test-vs-production —
        // that's the whole reason `from_static_literal_calls_outside_tests`
        // exists as a separate, filtered function.
        assert_eq!(from_static_literal_calls(src).unwrap().len(), 1);

        assert!(
            from_static_literal_calls_outside_tests(src, &file)
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn a_literal_in_production_code_is_still_flagged_alongside_an_exempt_test_module() {
        let src = r#"
            fn real() -> StreamLabel<'static> {
                StreamLabel::from_static("production/literal")
            }

            #[cfg(test)]
            mod tests {
                #[test]
                fn round_trips() {
                    let x = StreamLabel::from_static("astronomy");
                }
            }
        "#;
        let file: syn::File = syn::parse_file(src).unwrap();
        let found = from_static_literal_calls_outside_tests(src, &file).unwrap();
        assert_eq!(found.len(), 1);
    }
}

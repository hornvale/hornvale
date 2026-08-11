//! Finding registered seams and their call sites with `syn`.

use crate::tag::{self, Op, Seam};
use proc_macro2::LineColumn;
use std::path::{Path, PathBuf};
use syn::spanned::Spanned;
use syn::visit::Visit;

/// The workspace roots scanned when no explicit paths are given. Mirrors
/// `type-audit`'s list — `tools/` and `clients/` are outside the workspace
/// and outside determinism, so they are outside this tool too.
pub const WORKSPACE_ROOTS: &[&str] = &["kernel", "domains", "windows", "cli"];

/// A source range, as a pair of 1-based-line / 0-based-column positions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span2 {
    /// Inclusive start position.
    pub start: (usize, usize),
    /// Exclusive end position.
    pub end: (usize, usize),
}

impl Span2 {
    fn of(span: proc_macro2::Span) -> Self {
        let s: LineColumn = span.start();
        let e: LineColumn = span.end();
        Self {
            start: (s.line, s.column),
            end: (e.line, e.column),
        }
    }
}

/// One call of a registered seam.
#[derive(Debug, Clone)]
pub struct CallSite {
    /// File the call appears in.
    pub file: PathBuf,
    /// Span of the entire call expression, `f(a, b)`.
    pub call: Span2,
    /// Spans of each argument, in order.
    pub args: Vec<Span2>,
}

impl CallSite {
    /// 1-based line the call starts on, for reporting.
    pub fn line(&self) -> usize {
        self.call.start.0
    }
}

/// True iff any attribute is `#[cfg(test)]`.
fn is_cfg_test(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        if !a.path().is_ident("cfg") {
            return false;
        }
        let mut found = false;
        let _ = a.parse_nested_meta(|m| {
            if m.path.is_ident("test") {
                found = true;
            }
            Ok(())
        });
        found
    })
}

/// True iff any attribute marks this as a test function.
fn is_test_fn(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| a.path().is_ident("test")) || is_cfg_test(attrs)
}

/// Concatenate an item's `///` doc comments into one string.
fn doc_of(attrs: &[syn::Attribute]) -> String {
    let mut out = String::new();
    for a in attrs {
        if !a.path().is_ident("doc") {
            continue;
        }
        if let syn::Meta::NameValue(nv) = &a.meta
            && let syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(s),
                ..
            }) = &nv.value
        {
            out.push_str(&s.value());
            out.push(' ');
        }
    }
    out
}

/// Collects `seam-guard:` registrations from one parsed file.
struct SeamVisitor<'a> {
    path: &'a Path,
    seams: Vec<Seam>,
    errors: Vec<(usize, tag::TagError)>,
}

impl<'ast> Visit<'ast> for SeamVisitor<'_> {
    fn visit_item_mod(&mut self, node: &'ast syn::ItemMod) {
        if is_cfg_test(&node.attrs) {
            return;
        }
        syn::visit::visit_item_mod(self, node);
    }

    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        let line = node.sig.ident.span().start().line;
        match tag::parse(&doc_of(&node.attrs)) {
            Ok(Some((op, scope))) => self.seams.push(Seam {
                name: node.sig.ident.to_string(),
                op,
                scope,
                file: self.path.display().to_string(),
                line,
            }),
            Ok(None) => {}
            Err(e) => self.errors.push((line, e)),
        }
        syn::visit::visit_item_fn(self, node);
    }
}

/// Collects call sites of one named function from a parsed file.
struct CallVisitor<'a> {
    path: &'a Path,
    name: &'a str,
    sites: Vec<CallSite>,
}

impl<'ast> Visit<'ast> for CallVisitor<'_> {
    fn visit_item_mod(&mut self, node: &'ast syn::ItemMod) {
        if is_cfg_test(&node.attrs) {
            return;
        }
        syn::visit::visit_item_mod(self, node);
    }

    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        // A seam mutated inside its own test proves nothing — the test would
        // be asserting on the mutation, not on the behaviour under it.
        if is_test_fn(&node.attrs) {
            return;
        }
        syn::visit::visit_item_fn(self, node);
    }

    fn visit_expr_call(&mut self, node: &'ast syn::ExprCall) {
        if let syn::Expr::Path(p) = &*node.func
            && p.path.segments.last().is_some_and(|s| s.ident == self.name)
        {
            self.sites.push(CallSite {
                file: self.path.to_path_buf(),
                call: Span2::of(node.span()),
                args: node.args.iter().map(|a| Span2::of(a.span())).collect(),
            });
        }
        syn::visit::visit_expr_call(self, node);
    }
}

/// Parse one file and return the seams it registers.
pub fn seams_in(path: &Path, src: &str) -> (Vec<Seam>, Vec<(usize, tag::TagError)>) {
    let Ok(file) = syn::parse_file(src) else {
        return (Vec::new(), Vec::new());
    };
    let mut v = SeamVisitor {
        path,
        seams: Vec::new(),
        errors: Vec::new(),
    };
    v.visit_file(&file);
    (v.seams, v.errors)
}

/// Parse one file and return every non-test call site of `name`.
pub fn calls_in(path: &Path, src: &str, name: &str) -> Vec<CallSite> {
    let Ok(file) = syn::parse_file(src) else {
        return Vec::new();
    };
    let mut v = CallVisitor {
        path,
        name,
        sites: Vec::new(),
    };
    v.visit_file(&file);
    v.sites
}

/// Every `.rs` file under the given roots, sorted for determinism.
pub fn rust_files(roots: &[&str]) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for root in roots {
        collect(Path::new(root), &mut out);
    }
    out.sort();
    out
}

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
    paths.sort();
    for p in paths {
        if p.is_dir() {
            if p.file_name().is_some_and(|n| n == "target") {
                continue;
            }
            collect(&p, out);
        } else if p.extension().is_some_and(|e| e == "rs") {
            out.push(p);
        }
    }
}

/// Describe an operator for the report.
pub fn describe(op: &Op) -> String {
    match op {
        Op::Identity(n) => format!("identity({n})"),
        Op::Returns(e) => format!("returns({e})"),
    }
}

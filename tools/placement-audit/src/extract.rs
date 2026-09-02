//! Extracting pub enum/struct "shapes" (a type's kind plus its sorted
//! member-name set) from a parsed source file.

use std::path::{Path, PathBuf};

/// Whether a [`TypeShape`] came from a `pub enum` or a `pub struct` with
/// named fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ShapeKind {
    /// A `pub enum`; members are its variant identifiers.
    Enum,
    /// A `pub struct` with named fields; members are its field identifiers.
    Struct,
}

/// One extracted bare-`pub` enum or named-field struct, keyed for
/// cross-crate twin comparison by its `(kind, members)` pair.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeShape {
    /// The crate this shape was found in (`kernel`, `<domain-name>`, `other`).
    pub crate_name: String,
    /// Type identifier — informative only; twin matching never keys on it.
    pub name: String,
    /// Enum or (named-field) struct.
    pub kind: ShapeKind,
    /// Sorted variant/field identifier strings — the twin-matching key.
    pub members: Vec<String>,
    /// Concatenated doc-comment text.
    pub doc: String,
    /// Source file the shape was found in.
    pub file: PathBuf,
    /// 1-based source line of the type's identifier.
    pub line: usize,
}

/// True only for a bare `pub` (not `pub(crate)`/`pub(super)`/`pub(in …)`).
pub fn is_bare_pub(vis: &syn::Visibility) -> bool {
    matches!(vis, syn::Visibility::Public(_))
}

/// Concatenate the text of `#[doc = "…"]` attributes into one string.
pub fn doc_text(attrs: &[syn::Attribute]) -> String {
    let mut out = String::new();
    for attr in attrs {
        if attr.path().is_ident("doc")
            && let syn::Meta::NameValue(nv) = &attr.meta
            && let syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(s),
                ..
            }) = &nv.value
        {
            out.push_str(&s.value());
            out.push('\n');
        }
    }
    out
}

/// True for a `#[cfg(test)]` attribute — inline test modules are never
/// recursed into.
pub(crate) fn has_cfg_test(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        a.path().is_ident("cfg")
            && a.parse_args::<syn::Meta>()
                .map(|m| matches!(m, syn::Meta::Path(p) if p.is_ident("test")))
                .unwrap_or(false)
    })
}

/// Collect every bare-`pub` enum and named-field struct in `file`.
/// `crate_name` and `file_path` are stamped onto every [`TypeShape`]
/// produced — extraction over a bare [`syn::File`] has no notion of its own
/// location, so the caller (the walker) supplies both.
pub fn extract_types(file: &syn::File, crate_name: &str, file_path: &Path) -> Vec<TypeShape> {
    let mut shapes = Vec::new();
    collect_items(&file.items, crate_name, file_path, &mut shapes);
    shapes
}

fn collect_items(
    items: &[syn::Item],
    crate_name: &str,
    file_path: &Path,
    out: &mut Vec<TypeShape>,
) {
    for item in items {
        match item {
            syn::Item::Enum(e) if is_bare_pub(&e.vis) => push_enum(e, crate_name, file_path, out),
            syn::Item::Struct(s) if is_bare_pub(&s.vis) => {
                push_struct(s, crate_name, file_path, out)
            }
            syn::Item::Mod(m) if !has_cfg_test(&m.attrs) => {
                if let Some((_, items)) = &m.content {
                    collect_items(items, crate_name, file_path, out);
                }
            }
            _ => {}
        }
    }
}

fn push_enum(e: &syn::ItemEnum, crate_name: &str, file_path: &Path, out: &mut Vec<TypeShape>) {
    let mut members: Vec<String> = e.variants.iter().map(|v| v.ident.to_string()).collect();
    members.sort();
    out.push(TypeShape {
        crate_name: crate_name.to_string(),
        name: e.ident.to_string(),
        kind: ShapeKind::Enum,
        members,
        doc: doc_text(&e.attrs),
        file: file_path.to_path_buf(),
        line: e.ident.span().start().line,
    });
}

fn push_struct(s: &syn::ItemStruct, crate_name: &str, file_path: &Path, out: &mut Vec<TypeShape>) {
    // Only named-field structs carry a member set worth comparing — a tuple
    // or unit struct has no names to match on, so a "twin" there would be
    // near-meaningless (brief step 4).
    let syn::Fields::Named(fields) = &s.fields else {
        return;
    };
    let mut members: Vec<String> = fields
        .named
        .iter()
        .filter_map(|f| f.ident.as_ref().map(|i| i.to_string()))
        .collect();
    if members.is_empty() {
        return;
    }
    members.sort();
    out.push(TypeShape {
        crate_name: crate_name.to_string(),
        name: s.ident.to_string(),
        kind: ShapeKind::Struct,
        members,
        doc: doc_text(&s.attrs),
        file: file_path.to_path_buf(),
        line: s.ident.span().start().line,
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    fn file(src: &str) -> syn::File {
        syn::parse_file(src).unwrap()
    }

    #[test]
    fn extracts_bare_pub_enum_with_sorted_variants() {
        let f = file(
            r#"
            /// A mood.
            pub enum Mood {
                /// Up.
                Bright,
                /// Down.
                Dim,
                /// Flat.
                Level,
            }
            enum Private { A, B }
            pub(crate) enum Restricted { A, B }
            "#,
        );
        let shapes = extract_types(&f, "a", &PathBuf::from("a/src/lib.rs"));
        assert_eq!(shapes.len(), 1);
        assert_eq!(shapes[0].name, "Mood");
        assert_eq!(shapes[0].kind, ShapeKind::Enum);
        assert_eq!(shapes[0].members, vec!["Bright", "Dim", "Level"]);
        assert_eq!(shapes[0].crate_name, "a");
        assert!(shapes[0].doc.contains("A mood."));
    }

    #[test]
    fn extracts_named_field_struct_and_skips_tuple_and_unit_structs() {
        let f = file(
            r#"
            /// A point.
            pub struct Point {
                /// X.
                pub x: f64,
                /// Y.
                pub y: f64,
            }
            /// A wrapper — no names to match on.
            pub struct Wrapper(pub u32);
            /// A marker — no members at all.
            pub struct Marker;
            "#,
        );
        let shapes = extract_types(&f, "a", &PathBuf::from("a/src/lib.rs"));
        let names: Vec<&str> = shapes.iter().map(|shape| shape.name.as_str()).collect();
        assert_eq!(names, vec!["Point"]);
        assert_eq!(shapes[0].members, vec!["x", "y"]);
    }

    #[test]
    fn skips_cfg_test_modules_but_recurses_into_plain_ones() {
        let f = file(
            r#"
            /// Outer.
            pub enum Outer { A, B }
            pub mod inner {
                /// Inner, reached via recursion.
                pub enum Inner { C, D }
            }
            #[cfg(test)]
            mod tests {
                pub enum Hidden { E, F }
            }
            "#,
        );
        let shapes = extract_types(&f, "a", &PathBuf::from("a/src/lib.rs"));
        let names: Vec<&str> = shapes.iter().map(|shape| shape.name.as_str()).collect();
        assert!(names.contains(&"Outer"));
        assert!(names.contains(&"Inner"));
        assert!(!names.contains(&"Hidden"));
    }

    #[test]
    fn member_name_equality_ignores_type_name() {
        let a = file(
            r#"
            /// A mood.
            pub enum Mood { Bright, Level, Dim }
            "#,
        );
        let b = file(
            r#"
            /// Same mood, different name.
            pub enum Temper { Bright, Level, Dim }
            "#,
        );
        let sa = extract_types(&a, "a", &PathBuf::from("a/src/lib.rs"));
        let sb = extract_types(&b, "b", &PathBuf::from("b/src/lib.rs"));
        assert_eq!(sa[0].members, sb[0].members);
        assert_ne!(sa[0].name, sb[0].name);
    }
}

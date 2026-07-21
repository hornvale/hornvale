//! Walking a parsed file for primitives at `pub` boundaries.

use crate::primitives::contains_tracked_primitive;
use syn::spanned::Spanned;

/// One audited primitive-bearing position on an item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    /// Parameter or field name; `"return"` for a return type.
    pub name: String,
    /// 1-based source line of the position, for diagnostics.
    pub line: usize,
}

/// One audited `pub` item and all its primitive-bearing positions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuditItem {
    /// Item name (function/struct/enum/const/type identifier).
    pub name: String,
    /// Concatenated doc-comment text (where a `type-audit:` line may live).
    pub doc: String,
    /// 1-based source line of the item, for diagnostics.
    pub line: usize,
    /// Every primitive-bearing audited position on the item.
    pub positions: Vec<Position>,
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

/// Collect every audited `pub` item with ≥1 primitive-bearing position.
pub fn positions_in_file(file: &syn::File) -> Vec<AuditItem> {
    let mut items = Vec::new();
    collect_items(&file.items, &mut items);
    items
}

fn collect_items(syn_items: &[syn::Item], out: &mut Vec<AuditItem>) {
    for item in syn_items {
        match item {
            syn::Item::Fn(f) if is_bare_pub(&f.vis) => push_fn(&f.sig, &f.attrs, out),
            syn::Item::Struct(s) if is_bare_pub(&s.vis) => push_struct(s, out),
            syn::Item::Enum(e) if is_bare_pub(&e.vis) => push_enum(e, out),
            syn::Item::Const(c) if is_bare_pub(&c.vis) => {
                push_type_item(&c.ident, &c.ty, &c.attrs, out)
            }
            syn::Item::Static(s) if is_bare_pub(&s.vis) => {
                push_type_item(&s.ident, &s.ty, &s.attrs, out)
            }
            syn::Item::Type(t) if is_bare_pub(&t.vis) => {
                push_type_item(&t.ident, &t.ty, &t.attrs, out)
            }
            syn::Item::Trait(t) if is_bare_pub(&t.vis) => {
                for ti in &t.items {
                    if let syn::TraitItem::Fn(m) = ti {
                        push_fn(&m.sig, &m.attrs, out);
                    }
                }
            }
            syn::Item::Impl(i) if i.trait_.is_none() => {
                for ii in &i.items {
                    if let syn::ImplItem::Fn(m) = ii
                        && is_bare_pub(&m.vis)
                    {
                        push_fn(&m.sig, &m.attrs, out);
                    }
                }
            }
            syn::Item::Mod(m) if !has_cfg_test(&m.attrs) => {
                if let Some((_, items)) = &m.content {
                    collect_items(items, out);
                }
            }
            _ => {}
        }
    }
}

fn push_fn(sig: &syn::Signature, attrs: &[syn::Attribute], out: &mut Vec<AuditItem>) {
    let mut positions = Vec::new();
    for input in &sig.inputs {
        if let syn::FnArg::Typed(pt) = input
            && contains_tracked_primitive(&pt.ty)
        {
            positions.push(Position {
                name: pat_name(&pt.pat),
                line: pt.span().start().line,
            });
        }
    }
    if let syn::ReturnType::Type(_, ty) = &sig.output
        && contains_tracked_primitive(ty)
    {
        positions.push(Position {
            name: "return".to_string(),
            line: ty.span().start().line,
        });
    }
    if !positions.is_empty() {
        out.push(AuditItem {
            name: sig.ident.to_string(),
            doc: doc_text(attrs),
            line: sig.ident.span().start().line,
            positions,
        });
    }
}

fn push_struct(s: &syn::ItemStruct, out: &mut Vec<AuditItem>) {
    let mut positions = Vec::new();
    for (idx, field) in s.fields.iter().enumerate() {
        if !is_bare_pub(&field.vis) {
            continue;
        }
        if contains_tracked_primitive(&field.ty) {
            let name = field
                .ident
                .as_ref()
                .map(|i| i.to_string())
                .unwrap_or_else(|| idx.to_string());
            positions.push(Position {
                name,
                line: field.ty.span().start().line,
            });
        }
    }
    if !positions.is_empty() {
        out.push(AuditItem {
            name: s.ident.to_string(),
            doc: doc_text(&s.attrs),
            line: s.ident.span().start().line,
            positions,
        });
    }
}

fn push_enum(e: &syn::ItemEnum, out: &mut Vec<AuditItem>) {
    let mut positions = Vec::new();
    for variant in &e.variants {
        for (idx, field) in variant.fields.iter().enumerate() {
            if contains_tracked_primitive(&field.ty) {
                let base = field
                    .ident
                    .as_ref()
                    .map(|i| i.to_string())
                    .unwrap_or_else(|| idx.to_string());
                positions.push(Position {
                    name: format!("{}.{}", variant.ident, base),
                    line: field.ty.span().start().line,
                });
            }
        }
    }
    if !positions.is_empty() {
        out.push(AuditItem {
            name: e.ident.to_string(),
            doc: doc_text(&e.attrs),
            line: e.ident.span().start().line,
            positions,
        });
    }
}

fn push_type_item(
    ident: &syn::Ident,
    ty: &syn::Type,
    attrs: &[syn::Attribute],
    out: &mut Vec<AuditItem>,
) {
    if contains_tracked_primitive(ty) {
        out.push(AuditItem {
            name: ident.to_string(),
            doc: doc_text(attrs),
            line: ident.span().start().line,
            positions: vec![Position {
                name: ident.to_string(),
                line: ty.span().start().line,
            }],
        });
    }
}

/// True for a `#[cfg(test)]` attribute — shared with [`crate::stream_label`],
/// so both checks agree on what counts as test-only code.
pub(crate) fn has_cfg_test(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        a.path().is_ident("cfg")
            && a.parse_args::<syn::Meta>()
                .map(|m| matches!(m, syn::Meta::Path(p) if p.is_ident("test")))
                .unwrap_or(false)
    })
}

fn pat_name(pat: &syn::Pat) -> String {
    match pat {
        syn::Pat::Ident(pi) => pi.ident.to_string(),
        _ => "_".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn file(src: &str) -> syn::File {
        syn::parse_file(src).unwrap()
    }

    #[test]
    fn extracts_pub_fn_params_and_returns() {
        let f = file(
            r#"
            /// A function.
            /// type-audit: bare-ok(count)
            pub fn seed_count(octaves: u32, label: &str) -> f64 { 0.0 }
            fn private(x: f64) {}
            pub fn opaque(cell: CellId) -> CellId { cell }
            "#,
        );
        let items = positions_in_file(&f);
        assert_eq!(items.len(), 1); // private + opaque excluded
        let item = &items[0];
        assert_eq!(item.name, "seed_count");
        assert!(item.doc.contains("type-audit: bare-ok(count)"));
        let names: Vec<_> = item.positions.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["octaves", "label", "return"]);
    }

    #[test]
    fn extracts_pub_fields_only_and_respects_restricted_vis() {
        let f = file(
            r#"
            /// A struct.
            pub struct Config {
                /// Ocean fraction.
                pub ocean_fraction: f64,
                pub(crate) hidden: f64,
                secret: u32,
            }
            "#,
        );
        let items = positions_in_file(&f);
        assert_eq!(items.len(), 1);
        let names: Vec<_> = items[0].positions.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["ocean_fraction"]);
    }

    #[test]
    fn extracts_tuple_struct_pub_fields_by_index() {
        let f = file(
            r#"
            /// A cell id.
            pub struct CellId(pub u32);
        "#,
        );
        let items = positions_in_file(&f);
        assert_eq!(items[0].positions[0].name, "0");
    }

    #[test]
    fn extracts_enum_variant_fields() {
        let f = file(
            r#"
            /// A rotation.
            pub enum Rotation {
                /// Spinning with a day length.
                Spinning { day: f64 },
                /// Tidally locked.
                Locked,
            }
        "#,
        );
        let items = positions_in_file(&f);
        assert_eq!(items.len(), 1);
        // Variant-scoped names ("Variant.field") disambiguate same-named
        // fields across variants — see push_enum and the task's naming rule.
        assert_eq!(items[0].positions[0].name, "Spinning.day");
    }

    #[test]
    fn extracts_const_static_and_type_alias() {
        let f = file(
            r#"
            /// A label.
            pub const SEED_LABEL: &str = "x";
            /// An alias that launders a primitive.
            pub type Meters = f64;
        "#,
        );
        let items = positions_in_file(&f);
        let names: Vec<_> = items.iter().map(|i| i.name.as_str()).collect();
        assert!(names.contains(&"SEED_LABEL"));
        assert!(names.contains(&"Meters"));
    }

    #[test]
    fn audits_trait_defs_and_inherent_impls_but_not_trait_impls() {
        let f = file(
            r#"
            /// A provider.
            pub trait Provider {
                /// Sample at a coordinate.
                fn sample(&self, x: f64) -> f64;
            }
            pub struct P;
            impl P {
                /// Inherent, audited.
                pub fn scale(&self, k: f64) -> f64 { k }
            }
            impl Provider for P {
                fn sample(&self, x: f64) -> f64 { x } // trait impl — NOT audited
            }
        "#,
        );
        let items = positions_in_file(&f);
        let names: Vec<_> = items.iter().map(|i| i.name.as_str()).collect();
        assert!(names.contains(&"sample")); // trait def
        assert!(names.contains(&"scale")); // inherent impl
        assert_eq!(names.iter().filter(|n| **n == "sample").count(), 1); // impl not double-counted
    }

    #[test]
    fn skips_cfg_test_modules() {
        let f = file(
            r#"
            /// Real.
            pub fn real(x: f64) -> f64 { x }
            #[cfg(test)]
            mod tests {
                pub fn helper(y: f64) -> f64 { y }
            }
        "#,
        );
        let items = positions_in_file(&f);
        let names: Vec<_> = items.iter().map(|i| i.name.as_str()).collect();
        assert_eq!(names, vec!["real"]);
    }

    #[test]
    fn recurses_into_plain_nested_modules() {
        let f = file(
            r#"
            /// Outer.
            pub fn outer(x: f64) -> f64 { x }
            pub mod inner {
                /// Inner, audited via recursion.
                pub fn deep(y: f64) -> f64 { y }
            }
        "#,
        );
        let items = positions_in_file(&f);
        let names: Vec<_> = items.iter().map(|i| i.name.as_str()).collect();
        assert!(names.contains(&"outer"));
        assert!(names.contains(&"deep"));
    }
}

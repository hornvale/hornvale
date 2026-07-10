//! Recognising the primitives the audit tracks, wherever they nest.

/// The primitive type names the audit tracks. `&str` is caught as its inner
/// path segment `str`; `String` is a path segment.
const TRACKED: &[&str] = &[
    "f64", "f32", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64", "i128", "usize",
    "isize", "bool", "char", "String", "str",
];

/// True iff `ty` contains a tracked primitive at any nesting depth.
pub fn contains_tracked_primitive(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(p) => {
            // A tracked leaf, or a generic wrapper whose args contain one.
            if let Some(seg) = p.path.segments.last() {
                if TRACKED.contains(&seg.ident.to_string().as_str()) {
                    return true;
                }
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    return args.args.iter().any(|a| match a {
                        syn::GenericArgument::Type(t) => contains_tracked_primitive(t),
                        _ => false,
                    });
                }
            }
            false
        }
        syn::Type::Reference(r) => contains_tracked_primitive(&r.elem),
        syn::Type::Slice(s) => contains_tracked_primitive(&s.elem),
        syn::Type::Array(a) => contains_tracked_primitive(&a.elem),
        syn::Type::Tuple(t) => t.elems.iter().any(contains_tracked_primitive),
        syn::Type::Paren(p) => contains_tracked_primitive(&p.elem),
        syn::Type::Group(g) => contains_tracked_primitive(&g.elem),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ty(src: &str) -> syn::Type {
        syn::parse_str(src).unwrap()
    }

    #[test]
    fn tracks_primitives_wherever_they_nest() {
        assert!(contains_tracked_primitive(&ty("f64")));
        assert!(contains_tracked_primitive(&ty("Option<f64>")));
        assert!(contains_tracked_primitive(&ty("Vec<(u32, String)>")));
        assert!(contains_tracked_primitive(&ty("&str")));
        assert!(contains_tracked_primitive(&ty("&[f64]")));
        assert!(contains_tracked_primitive(&ty("[u8; 4]")));
        // Non-tracked: opaque domain types and bare generics.
        assert!(!contains_tracked_primitive(&ty("CellId")));
        assert!(!contains_tracked_primitive(&ty("Vec<Cell>")));
        assert!(!contains_tracked_primitive(&ty("T")));
        assert!(!contains_tracked_primitive(&ty("Au")));
    }
}

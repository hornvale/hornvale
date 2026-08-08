//! The repo self-map.

/// Render the layering and dependency-allowlist section of the self-map.
///
/// Every value here is DERIVED. Nothing in this function may name an
/// allowlist member literally — that is what drifted before (spec §1.1).
pub fn self_map(allowed: &[String], layers: &[String]) -> String {
    format!(
        "== Layering (enforced: cli/tests/architecture.rs; picture: book/src/reference/layering.md)\n  \
         {}\n  \
         a domain depends on the kernel and NOTHING else; windows/worldgen is the\n  \
         composition root; external deps allowlist: {}\n",
        layers.join(" -> "),
        allowed.join(", ")
    )
}

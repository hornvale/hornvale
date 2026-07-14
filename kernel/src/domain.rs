//! The `Domain` trait: a generative domain's declarative registration surface.
//!
//! The composition root aggregates these members uniformly across every
//! domain. Genesis is NOT here — its inputs are domain-specific composition
//! work (Constitution §2.6). Metrics and reference metadata are not here
//! either; future per-domain behaviors get their own traits, never new
//! `Domain` members.

use crate::registry::{ConceptRegistry, RegistryError};

/// A generative domain's declarative registration surface.
pub trait Domain {
    /// This domain's crate name — the streams-manifest key
    /// (e.g. `"hornvale-astronomy"`). Implementations return
    /// `env!("CARGO_PKG_NAME")` so the key is compiled from the crate itself.
    /// type-audit: bare-ok(identifier-text)
    fn crate_name(&self) -> &'static str;

    /// Register this domain's predicates and concepts into the shared registry.
    fn register_concepts(&self, registry: &mut ConceptRegistry) -> Result<(), RegistryError>;

    /// This domain's seed-derivation stream labels (permanent save-format
    /// contracts). Empty for domains that draw no seed streams.
    /// type-audit: bare-ok(identifier-text)
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::ConceptRegistry;

    struct Dummy;
    impl Domain for Dummy {
        fn crate_name(&self) -> &'static str {
            "dummy"
        }
        fn register_concepts(&self, _r: &mut ConceptRegistry) -> Result<(), RegistryError> {
            Ok(())
        }
        // stream_labels intentionally omitted — exercises the default.
    }

    #[test]
    fn default_stream_labels_is_empty() {
        assert!(Dummy.stream_labels().is_empty());
    }

    #[test]
    fn is_object_safe() {
        let d: &dyn Domain = &Dummy;
        assert_eq!(d.crate_name(), "dummy");
    }
}

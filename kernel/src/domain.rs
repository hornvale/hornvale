//! The `Domain` trait: a generative domain's declarative registration surface.
//!
//! The composition root aggregates these members uniformly across every
//! domain. Genesis is NOT here — its inputs are domain-specific composition
//! work (Constitution §2.6). Metrics and reference metadata are not here
//! either; future per-domain behaviors get their own traits, never new
//! `Domain` members.

use crate::phenomena::PhenomenaSource;
use crate::registry::{ConceptRegistry, RegistryError};
use crate::world::World;
use std::collections::BTreeMap;

/// The composition root's context for a single phenomena observation, handed
/// to each domain's [`Domain::phenomena_source`]. It carries any live sources
/// the root pre-assembled from cross-domain inputs, keyed by the owning
/// crate's name.
///
/// A domain whose phenomena source needs data its own crate cannot see (a
/// sibling domain's output — e.g. climate's provider is built from terrain
/// and sky) does not construct that source itself: the composition root
/// builds it (the only layer where domains legally meet) and hands it over
/// here for the domain to reclaim by [`claim`](WorldContext::claim). This
/// keeps a domain crate free of sibling dependencies while still letting its
/// `phenomena_source` return a rich, composed provider. A domain whose source
/// needs no cross-domain inputs ignores this and constructs its own.
#[derive(Default)]
pub struct WorldContext {
    provisions: BTreeMap<&'static str, Box<dyn PhenomenaSource>>,
}

impl WorldContext {
    /// A fresh context with no pre-assembled sources.
    pub fn new() -> Self {
        WorldContext {
            provisions: BTreeMap::new(),
        }
    }

    /// Supply a composition-root-built source under its owning crate's name,
    /// for a domain whose provider is composed from cross-domain inputs.
    /// type-audit: bare-ok(identifier-text: crate_name)
    pub fn provide(&mut self, crate_name: &'static str, source: Box<dyn PhenomenaSource>) {
        self.provisions.insert(crate_name, source);
    }

    /// Reclaim (removing) the source the composition root supplied for
    /// `crate_name`, if any.
    /// type-audit: bare-ok(identifier-text: crate_name)
    pub fn claim(&mut self, crate_name: &str) -> Option<Box<dyn PhenomenaSource>> {
        self.provisions.remove(crate_name)
    }
}

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

    /// This domain's live phenomena source for the given world, or `None` if
    /// it contributes none (the default — most domains observe nothing). The
    /// composition root folds every domain's source into the roster that
    /// [`crate::phenomena::observe`] aggregates. `ctx` carries any source the
    /// root pre-built from cross-domain inputs (see [`WorldContext`]); a
    /// domain whose provider is composed elsewhere reclaims it from there,
    /// while a self-contained one builds and returns its own.
    fn phenomena_source(
        &self,
        world: &World,
        ctx: &mut WorldContext,
    ) -> Option<Box<dyn PhenomenaSource>> {
        let _ = (world, ctx);
        None
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

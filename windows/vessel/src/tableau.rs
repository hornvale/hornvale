//! A situation staged declaratively, rather than hunted for.
//!
//! A tableau is a **diff against a derived session**, never a world built from
//! nothing. It overrides named layers and inherits the rest, which is what
//! makes it cheap: every expensive layer — sculpted terrain, fitted climate,
//! the species roster, the demography fit — lives in the [`crate::
//! WorldContext`] a tableau rides, so a hundred tableaux over one context
//! each cost a session start rather than a genesis.
//!
//! **Unspecified means EMPTY for anything you could have written down.** A
//! tableau with no cast stages nobody; it does not fall back to the world's
//! own inhabitants. If it did, a tableau that stipulated a room but not a cast
//! would depend, silently, on whatever the seed happened to place — which is
//! the whole complaint this campaign answers, and is the *mystery guest* every
//! fixture library learns to fear. Layers that cannot be written down
//! (terrain, climate, the species roster, the sky) inherit, because you cannot
//! state a planet.
//!
//! A tableau proves the MACHINERY, never the world. A scene staged here is
//! evidence that the sim can carry a situation, and never evidence that any
//! world produces one.

/// One creature in a staged cast.
///
/// Species only, for now: everything else a [`crate::body::Body`] carries —
/// its activity cycle, temperature niche, psychology dials, mass — is derived
/// FROM the species, so stating those separately would let a tableau describe
/// a creature the world's own registries could not.
/// type-audit: bare-ok(identifier-text: species)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StagedBody {
    /// The creature's species, as the biosphere registry labels it.
    pub species: String,
}

/// A staged situation: what the caller has stipulated, and nothing else.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Tableau {
    /// Who is here. Empty stages nobody — see the module doc; this is never
    /// "inherit the world's inhabitants".
    pub cast: Vec<StagedBody>,
}

impl Tableau {
    /// An empty tableau: stages nobody, inherits the world beneath.
    pub fn new() -> Self {
        Self::default()
    }

    /// Stage a cast, by species, in the order given. They share a room —
    /// which is the point of staging one.
    pub fn with_cast<I, S>(mut self, species: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.cast = species
            .into_iter()
            .map(|s| StagedBody { species: s.into() })
            .collect();
        self
    }
}

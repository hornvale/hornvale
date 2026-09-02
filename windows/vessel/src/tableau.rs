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
//! **Absent and empty are deliberately the same thing here**, for every
//! layer a tableau can state. The design considered keeping them distinct so
//! that "this room, but explicitly nobody in it" could differ from "nobody
//! mentioned" — but under the rule above there is no third state for them to
//! differ INTO: both mean empty, and inventing a distinction with no
//! consequence would be a field nothing reads. If a future layer ever offers
//! *inherit* as a real third option, that is when the distinction earns its
//! keep.
//!
//! A tableau proves the MACHINERY, never the world. A scene staged here is
//! evidence that the sim can carry a situation, and never evidence that any
//! world produces one.

use serde::{Deserialize, Serialize};

/// One thing a tableau puts in a creature's hands.
///
/// `kind` is a thing-kind the world already knows (`"key"`, `"loaf"`), not a
/// free noun. A tableau may stage anything the world can REPRESENT, and a
/// staged prop is one of the world's own kinds.
///
/// **THE CLOSED-ENUM FENCE IS GONE (The Wicket, Task 2), and this sentence
/// said the opposite.** It read: *"the vocabulary of things is currently a
/// closed enum — see `MAP-one-kind-model`, which is where that fence gets
/// retired. Until then a staged prop is one of the world's own kinds."* True
/// when written and false now: an anchor carries a
/// [`hornvale_kernel::KindId`], the vocabulary is
/// `hornvale_thing::THING_KINDS`, and adding a kind is a row rather than a
/// variant. What bounds a staged prop today is that roster — open, but
/// ratcheted — not a type.
///
/// `MAP-one-kind-model` is NOT thereby discharged, and reading it that way is
/// the over-read this correction invites. Its first addition (kinds as data)
/// is what landed; the other two — kind-to-kind EDGES, and per-instance
/// components DERIVED from `Lineage` — are untouched, and the registry row is
/// still open.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(index: held_by)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StagedThing {
    /// The thing-kind, as `hornvale_thing::THING_KINDS` spells it.
    pub kind: String,
    /// Whose hands it is in, as an index into the cast.
    pub held_by: usize,
}

/// One creature in a staged cast.
///
/// Species only, for now: everything else a [`crate::body::Body`] carries —
/// its activity cycle, temperature niche, psychology dials, mass — is derived
/// FROM the species, so stating those separately would let a tableau describe
/// a creature the world's own registries could not.
/// type-audit: bare-ok(identifier-text: species)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StagedBody {
    /// The creature's species, as the biosphere registry labels it.
    pub species: String,
}

/// A staged situation: what the caller has stipulated, and nothing else.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct Tableau {
    /// Who is here. Empty stages nobody — see the module doc; this is never
    /// "inherit the world's inhabitants".
    pub cast: Vec<StagedBody>,
    /// What they are holding. Empty stages nothing, on the same rule.
    pub things: Vec<StagedThing>,
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

    /// Read a tableau from its serialized form.
    ///
    /// The file is a FRONT-END over this builder, never a second way to make
    /// one: `from_json` constructs a `Tableau` and every consumer goes through
    /// the same type. `deny_unknown_fields` is deliberate — a misspelt key in
    /// a hand-written tableau would otherwise stage something quietly
    /// different from what was written.
    /// type-audit: bare-ok(artifact: text)
    pub fn from_json(text: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(text)
    }

    /// Put a thing of `kind` into the hands of cast member `held_by`.
    /// type-audit: bare-ok(index: held_by)
    pub fn with_thing(mut self, kind: impl Into<String>, held_by: usize) -> Self {
        self.things.push(StagedThing {
            kind: kind.into(),
            held_by,
        });
        self
    }
}

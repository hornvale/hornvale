//! What a chamber is FOR — moved here from `interior::pattern` because the
//! structure is what now derives it (The Cruck), where the pattern layer only
//! consumes it.

/// What a chamber is FOR. A role admits a different pattern subset — the pattern
/// language one rung finer, where a role is a bundle of patterns that complete
/// each other. Same composer, a different declared vocabulary (spec §4.1).
///
/// DERIVED by the structure grammar ([`crate::structure::grammar`]) for a built
/// site, and read off the index for a wild one — never authored per place. A
/// role is not a room template: it names a vocabulary, and what a chamber ends
/// up holding is still whatever that vocabulary's `requires` clauses admit.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Role {
    /// The chamber `enter` lands in, and the only one whose doorway is screened.
    Threshold,
    /// The chamber built around a fire. The ONLY role that admits an alcove,
    /// which is what confines the fire to it (see [`INVENTORY`]).
    Hearthroom,
    /// A chamber for keeping things: the water jar's own room.
    Store,
    /// A regional seat's own chamber.
    Hall,
    /// A chamber given over to cloth.
    Loomroom,
    /// A chamber given over to metal.
    Smithy,
    /// A chamber given over to a rite.
    Shrine,
}

/// Every role, once. Written out rather than derived so that a pattern may
/// declare "any role draws me" without a magic empty slice, and so a new role
/// is a visible edit here rather than a silent widening.
pub const EVERY_ROLE: &[Role] = &[
    Role::Threshold,
    Role::Hearthroom,
    Role::Store,
    Role::Hall,
    Role::Loomroom,
    Role::Smithy,
    Role::Shrine,
];

impl Role {
    /// The one-word name a player types to walk through the aperture that
    /// leads to a chamber of this role (`enter the store`). Unique among the
    /// chambers of one structure because the grammar admits each role once.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn noun(self) -> &'static str {
        match self {
            Role::Threshold => "threshold",
            Role::Hearthroom => "hearth",
            Role::Store => "store",
            Role::Hall => "hall",
            Role::Loomroom => "loomroom",
            Role::Smithy => "smithy",
            Role::Shrine => "shrine",
        }
    }
}

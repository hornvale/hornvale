//! Channel ablation for The Repose's counterfactual arm (spec §6.6).
//!
//! Unrest reaches settlement siting through two OPPOSED channels and soil
//! fertility reaches it through none (see the campaign plan §0.1/§0.2). A
//! single-channel ablation therefore cannot separate "no effect exists" from
//! "the wire was never connected" — the two live channels are the positive
//! control that proves the harness can see movement at all.
//!
//! `ChannelMask::NONE` is the identity: every channel live, and an IEEE-754
//! no-op against the unmasked path (pinned bit-for-bit by
//! `repose_exposure.rs`). Nothing in a shipped world path ever passes
//! anything else.

/// Which contributions to per-species suitability are suppressed.
///
/// A `true` field means the channel is ABLATED (zeroed), not that it is on —
/// the field names read as "suppress this", and `NONE` is all-false.
/// type-audit: bare-ok(flag: hostility), bare-ok(flag: mineral_unrest), bare-ok(flag: andosol)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ChannelMask {
    /// Suppress the unrest hostility penalty in carrying capacity
    /// (`k *= 1.0 - hostility`), leaving the mineral reward intact.
    pub hostility: bool,
    /// Suppress the unrest term inside mineral prospectivity, leaving the
    /// boundary-setting and metamorphic-grade terms intact.
    pub mineral_unrest: bool,
    /// Suppress andosol's fertility advantage. Expected to be inert —
    /// measuring THAT is the point (plan §0.1): soil never reaches the siting
    /// path, so this flag has no application point and is deliberately read
    /// by nothing. It exists so the three-arm design is stated in the type,
    /// not only in a test's prose.
    pub andosol: bool,
}

impl ChannelMask {
    /// The identity: every channel live. The only value any shipped path uses.
    pub const NONE: ChannelMask = ChannelMask {
        hostility: false,
        mineral_unrest: false,
        andosol: false,
    };
}

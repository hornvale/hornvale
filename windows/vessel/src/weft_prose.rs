//! One clause: what a walker is told when standing on a facet carrying a
//! derived weft feature (The Weft, Task 8).
//!
//! **The kind-to-prose mapping lives HERE, not on `WeftKind` itself**,
//! mirroring `ruin_prose.rs`'s own division of labour (controller ruling,
//! Task 2, restated for the same reason at Task 8): `windows/worldgen`'s
//! layering doc is explicit that it composes domains, never renders prose,
//! so a `phrase()` on `WeftKind` would put the first rendering method in a
//! composition-root crate. The match below is exhaustive with **no `_`
//! arm** — the same discipline `cause_phrase` uses — so a fifth kind fails
//! to compile here until it is named.

use hornvale_worldgen::{WeftFeature, WeftKind};

/// The walk-band clause naming every derived feature at a facet, in the
/// order `features` names them. Empty when `features` is empty — a facet
/// with nothing derived stays silent, the same "most facets say nothing"
/// design `Session::site_clause`'s own doc states, and non-silence here is
/// the whole point of wiring this clause in at all.
/// type-audit: bare-ok(prose: return)
#[must_use]
pub fn weft_clause(features: &[WeftFeature]) -> String {
    features
        .iter()
        .map(|f| feature_line(f.kind))
        .collect::<Vec<_>>()
        .join("")
}

/// One kind's own sentence. Exhaustive, no `_` arm — see the module header.
fn feature_line(kind: WeftKind) -> &'static str {
    match kind {
        WeftKind::Spring => " A spring seeps up out of the ground here.",
        WeftKind::Overhang => {
            " A low overhang offers shelter from the weather, and a place to \
             build a fire."
        }
        WeftKind::Thicket => " A dense thicket presses close around you.",
        WeftKind::Erratic => " A lone boulder sits half-buried in the ground nearby.",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every kind renders through the exhaustive match without panicking and
    /// produces non-empty prose — mirrors `ruin_prose::every_cause_renders_
    /// nonempty_prose`, the same cheap witness that the match stays total as
    /// `WeftKind` grows.
    #[test]
    fn every_kind_renders_nonempty_prose() {
        for kind in WeftKind::ALL {
            assert!(!feature_line(kind).is_empty());
        }
    }

    /// No features, no clause — the silence [`weft_clause`]'s own doc
    /// promises for the common case (most facets carry nothing).
    #[test]
    fn no_features_is_silent() {
        assert_eq!(weft_clause(&[]), "");
    }

    /// Two features at one facet both render, in order — spec §5.2's "nothing
    /// normalises across kinds" means a facet CAN carry more than one, and
    /// the clause must not silently pick just one of them.
    #[test]
    fn two_features_both_render() {
        let features = [
            WeftFeature {
                kind: WeftKind::Spring,
                prevalence: 0.5,
            },
            WeftFeature {
                kind: WeftKind::Overhang,
                prevalence: 0.5,
            },
        ];
        let clause = weft_clause(&features);
        assert!(clause.contains("spring"));
        assert!(clause.contains("overhang"));
    }
}

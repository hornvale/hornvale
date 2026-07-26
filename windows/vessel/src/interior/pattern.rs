//! The pattern inventory — authored primitives, DERIVED selection, and a
//! validator. The architecture is `domains/language/src/phonology.rs`
//! transposed: one authored inventory shared by the world, a per-culture draw
//! from it conditioned on what that culture already is, and an admissibility
//! predicate that rejects ill-formed results.
//!
//! THE UNIT OF AUTHORSHIP IS A PATTERN, NOT A ROOM. A pattern is a relational
//! fragment; a room is a composition of them. Authoring whole rooms would make
//! this a catalogue of solutions rather than a generative language, which is
//! the failure mode that killed software's borrowing of Alexander.
//!
//! THE COMPOSITION RULES ARE THE SUBSTANCE, not the inventory's size. Two rules
//! carry the weight here: a pattern declares WHERE it attaches, and a pattern
//! may declare another it COMPLETES and without which it is inadmissible.
//! Together they give a room depth — a hub composition, where everything hangs
//! off the centre, is the degenerate case and is what the anti-hub test forbids.

use super::anchor::{AnchorId, AnchorKind, Interior};

/// Where a pattern's anchor attaches to what is already composed.
pub enum Attach {
    /// To the room's [`AnchorKind::Ground`] — the open middle.
    Hub,
    /// Adjacent to the first anchor of this kind (`Ec`).
    Beside(AnchorKind),
    /// Strictly inside the first anchor of this kind (`Ntpp`).
    Within(AnchorKind),
}

/// One authored pattern: a named relational fragment contributing one anchor,
/// where it attaches, and what it completes.
/// type-audit: bare-ok(identifier-text: name), bare-ok(flag: needs_cold), bare-ok(flag: built)
pub struct Pattern {
    /// The pattern's name — the selection key (never its index; see
    /// [`selection`]).
    pub name: &'static str,
    /// The anchor this pattern contributes.
    pub kind: AnchorKind,
    /// Where that anchor attaches.
    pub attach: Attach,
    /// A kind that must ALREADY be present for this pattern to be admissible —
    /// Alexander's "patterns complete other patterns", made checkable.
    pub requires: Option<AnchorKind>,
    /// Whether this pattern is drawn only where warmth matters.
    pub needs_cold: bool,
    /// Whether this pattern belongs to BUILT rooms (false = wilderness).
    pub built: bool,
}

/// The authored inventory. Sized near its intended scale deliberately: once
/// The Threshold makes furnishing live, `room/furnishing/v1` is a determinism
/// contract and growth costs an epoch.
pub const INVENTORY: [Pattern; 9] = [
    // --- built ---
    Pattern {
        name: "the-ground",
        kind: AnchorKind::Ground,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
    },
    Pattern {
        name: "the-threshold",
        kind: AnchorKind::Threshold,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
    },
    Pattern {
        name: "the-alcove",
        kind: AnchorKind::Alcove,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
    },
    Pattern {
        name: "the-fire",
        kind: AnchorKind::Hearth,
        attach: Attach::Within(AnchorKind::Alcove),
        requires: Some(AnchorKind::Alcove),
        needs_cold: true,
        built: true,
    },
    Pattern {
        name: "the-fireside-bed",
        kind: AnchorKind::Bed,
        attach: Attach::Beside(AnchorKind::Hearth),
        requires: Some(AnchorKind::Hearth),
        needs_cold: true,
        built: true,
    },
    Pattern {
        name: "the-water-jar",
        kind: AnchorKind::Vessel,
        attach: Attach::Beside(AnchorKind::Ground),
        requires: None,
        needs_cold: false,
        built: true,
    },
    Pattern {
        name: "the-screen",
        kind: AnchorKind::Screen,
        attach: Attach::Beside(AnchorKind::Threshold),
        requires: Some(AnchorKind::Threshold),
        needs_cold: false,
        built: true,
    },
    // --- wild ---
    Pattern {
        name: "the-clearing",
        kind: AnchorKind::Ground,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: false,
    },
    Pattern {
        name: "the-pool",
        kind: AnchorKind::Pool,
        attach: Attach::Beside(AnchorKind::Ground),
        requires: None,
        needs_cold: false,
        built: false,
    },
];

/// The patterns a room draws, DERIVED from what it already is — never authored
/// per culture. Admissibility is order-sensitive: a pattern that COMPLETES
/// another is admitted only once that other has been admitted, so the inventory
/// order encodes the grammar's dependency order.
///
/// **Keyed by NAME, never by position.** A future seeded draw must select on
/// `p.name`; keying on an index would silently re-roll every room the moment a
/// pattern is inserted (the same bug class as an id-as-offset, one scale up).
/// type-audit: bare-ok(flag: built), bare-ok(flag: cold)
pub fn selection(built: bool, cold: bool) -> Vec<&'static Pattern> {
    let mut out: Vec<&'static Pattern> = Vec::new();
    let mut present: std::collections::BTreeSet<AnchorKind> = std::collections::BTreeSet::new();
    for p in INVENTORY.iter() {
        if p.built != built {
            continue;
        }
        if p.needs_cold && !cold {
            continue;
        }
        if let Some(req) = p.requires
            && !present.contains(&req)
        {
            continue;
        }
        present.insert(p.kind);
        out.push(p);
    }
    out
}

/// Compose the selected patterns into one interior, honouring each pattern's
/// attachment. The first `Ground` anchor is the hub; everything else attaches to
/// the hub, beside a named kind, or within one. Depth comes from the chain
/// (`Within` then `Beside`), which is what keeps the result from collapsing to
/// a star.
pub fn compose(selected: &[&Pattern]) -> Interior {
    let mut interior = Interior::new();
    let mut hub: Option<AnchorId> = None;
    // First placed anchor of each kind — the attachment target.
    let mut first_of: std::collections::BTreeMap<AnchorKind, AnchorId> =
        std::collections::BTreeMap::new();

    for p in selected {
        let target = match p.attach {
            Attach::Hub => hub,
            Attach::Beside(k) | Attach::Within(k) => first_of.get(&k).copied().or(hub),
        };
        let within = match (&p.attach, target) {
            (Attach::Within(_), Some(t)) => Some(t),
            _ => None,
        };
        let id = interior.push(p.kind, within);
        // A contained anchor is already linked by containment; anything else
        // needs an explicit edge (unless it IS the hub).
        if within.is_none()
            && let Some(t) = target
        {
            interior.connect(t, id);
        }
        if hub.is_none() && p.kind == AnchorKind::Ground {
            hub = Some(id);
        }
        first_of.entry(p.kind).or_insert(id);
    }
    interior
}

/// Whether a composition is well-formed. The first rule: the anchor graph must
/// be CONNECTED, or part of the room is unreachable and a creature could be
/// asked to walk somewhere it cannot get to.
/// type-audit: bare-ok(flag: return)
pub fn permits(interior: &Interior) -> bool {
    interior.is_connected()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn selection_is_derived_from_conditions_not_authored_per_culture() {
        // The SAME inventory yields different sets under different climates —
        // the culture signal is derived, exactly as a phoneme inventory is.
        let cold = selection(true, true);
        let warm = selection(true, false);
        assert_ne!(
            cold.iter().map(|p| p.name).collect::<Vec<_>>(),
            warm.iter().map(|p| p.name).collect::<Vec<_>>(),
            "climate must change which patterns a people uses"
        );
        assert!(
            cold.iter().any(|p| p.kind == AnchorKind::Hearth),
            "a cold people builds around a fire"
        );
    }

    #[test]
    fn a_pattern_whose_requirement_is_absent_is_not_admissible() {
        // THE COMPOSITION RULE, and the whole test of whether this is a language
        // or a catalogue: `the-fireside-bed` completes `the-fire`, so it may not
        // be drawn where no fire was. This is Alexander's "patterns complete
        // other patterns" made checkable.
        let warm = selection(true, false);
        assert!(
            !warm.iter().any(|p| p.kind == AnchorKind::Hearth),
            "no fire in a warm room (fixture precondition)"
        );
        assert!(
            !warm.iter().any(|p| p.name == "the-fireside-bed"),
            "a bed BY THE FIRE cannot be drawn where there is no fire"
        );
        let cold = selection(true, true);
        assert!(
            cold.iter().any(|p| p.name == "the-fireside-bed"),
            "with a fire present, the pattern that completes it becomes admissible"
        );
    }

    #[test]
    fn wilderness_draws_natural_patterns_and_no_built_ones() {
        // The fine layer must exist where most agents live (spec §13 item 2).
        // A wilderness interior legitimately has NO threshold: seams belong to
        // room-graph edges, not to a room's interior (found by The Threshold).
        let wild = selection(false, false);
        assert!(!wild.is_empty(), "wilderness rooms get anchors too");
        assert!(
            wild.iter().all(|p| !p.built),
            "an unbuilt room contains no built patterns"
        );
        assert!(
            !wild.iter().any(|p| p.kind == AnchorKind::Threshold),
            "wilderness needs no doorway"
        );
    }

    #[test]
    fn composition_is_not_degenerate() {
        // THE ANTI-HUB TEST. A hub composition puts everything one hop from the
        // centre, so graph distance is 1-2 and field decay has nothing to decay
        // over. A real grammar produces DEPTH: some pair of anchors must be at
        // least three steps apart.
        let interior = compose(&selection(true, true));
        let ids = interior.ids();
        let mut deepest = 0usize;
        for a in &ids {
            for b in &ids {
                if let Some(path) = crate::interior::route_within(&interior, *a, *b, 256) {
                    deepest = deepest.max(path.len());
                }
            }
        }
        assert!(
            deepest >= 3,
            "the composed interior is degenerate (deepest route {deepest} hops); \
             a hub composition is a catalogue, not a language"
        );
    }

    #[test]
    fn a_composition_is_connected_and_the_validator_says_so() {
        for (built, cold) in [(true, true), (true, false), (false, false)] {
            let interior = compose(&selection(built, cold));
            assert!(
                interior.is_connected(),
                "composition (built={built}, cold={cold}) is walkable"
            );
            assert!(permits(&interior), "the validator accepts it");
        }
    }

    #[test]
    fn a_permitted_interior_is_routable_between_every_pair() {
        // CONNECTIVITY AND ROUTABILITY MUST AGREE. `permits` walks containment;
        // routing must too, or the validator green-lights a room a creature
        // cannot cross. This is the invariant a hearth-inside-an-alcove broke
        // silently — the anti-hub test still passed, via a different arm.
        for (built, cold) in [(true, true), (true, false), (false, false)] {
            let interior = compose(&selection(built, cold));
            assert!(permits(&interior));
            for a in interior.ids() {
                for b in interior.ids() {
                    assert!(
                        crate::interior::route_within(&interior, a, b, 256).is_some(),
                        "permitted interior (built={built}, cold={cold}) has no route \
                         {a:?} -> {b:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn the_intended_chain_is_the_deep_one() {
        // Not merely SOME 3-hop route: the route the grammar was designed to
        // produce. threshold -> ground -> alcove -> hearth -> bed.
        let interior = compose(&selection(true, true));
        let find = |k: AnchorKind| {
            interior
                .ids()
                .into_iter()
                .find(|id| interior.anchor(*id).kind == k)
                .unwrap_or_else(|| panic!("a cold built room has a {k:?}"))
        };
        let door = find(AnchorKind::Threshold);
        let bed = find(AnchorKind::Bed);
        let plan = crate::interior::route_within(&interior, door, bed, 256)
            .expect("the bed is reachable from the door");
        assert!(
            plan.len() >= 4,
            "the intended chain is at least four steps, got {}: {plan:?}",
            plan.len()
        );
    }

    #[test]
    fn the_validator_rejects_a_disconnected_composition() {
        // The first well-formedness rule (spec §6): an unreachable anchor means
        // part of the room cannot be used, so the composition is ill-formed.
        let mut broken = Interior::new();
        broken.push(AnchorKind::Hearth, None);
        broken.push(AnchorKind::Bed, None); // no edge — orphaned
        assert!(
            !permits(&broken),
            "the validator rejects an unreachable anchor"
        );
    }
}

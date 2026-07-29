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

/// What a chamber is FOR. A role admits a different pattern subset — the pattern
/// language one rung finer, where a role is a bundle of patterns that complete
/// each other. Same composer, a different declared vocabulary (spec §4.1).
///
/// DERIVED from the chamber's index and the brief ([`role_for`]), never authored
/// per place. A role is not a room template: it names a vocabulary, and what a
/// chamber ends up holding is still whatever that vocabulary's `requires` clauses
/// admit.
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

/// The roles a store's vocabulary is shared with: every role that keeps things,
/// which is every role except the two that furnish the front of a dwelling.
const STORING_ROLES: &[Role] = &[
    Role::Store,
    Role::Hall,
    Role::Loomroom,
    Role::Smithy,
    Role::Shrine,
];

/// One authored pattern: a named relational fragment contributing one anchor,
/// where it attaches, and what it completes.
/// type-audit: bare-ok(identifier-text: name), bare-ok(flag: needs_cold), bare-ok(flag: built), bare-ok(flag: at_locale), bare-ok(flag: needs_populous)
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
    /// Which chamber roles draw this pattern. [`EVERY_ROLE`] for a pattern no
    /// role withholds — including the two the GRAMMAR confines anyway
    /// (`the-fire`, `the-fireside-bed`), which is the point.
    pub roles: &'static [Role],
    /// Whether the LOCALE band draws this pattern — the epoch gate, and the one
    /// field in this struct that exists for a determinism reason rather than a
    /// world reason.
    ///
    /// [`selection`] admits only patterns with `at_locale: true`. A creature
    /// stands at a LOCALE, and its thermal drive reads the warmth of the
    /// interior composed there, which is committed history. So a pattern with
    /// `at_locale: false` **cannot** move a world: no live read can reach it.
    /// Setting one to `true` is what turns a latent pattern into an epoch, and
    /// `a_locale_composition_is_untouched_by_the_role_layer` is the check that
    /// makes that deliberate rather than accidental.
    pub at_locale: bool,
    /// Whether this pattern is drawn only where the place held more people than
    /// a hamlet ([`crate::brief::Brief::is_populous`]). The `needs_cold` of
    /// social scale: a hamlet has nothing worth locking up.
    pub needs_populous: bool,
}

/// The authored inventory.
///
/// **What costs an epoch, stated exactly.** This comment used to say flatly that
/// adding or reordering a pattern *is* an epoch. That was true while every
/// pattern reached every band, and The Blocking made it over-strict — an
/// over-strict warning is one that gets ignored, which is how an *undeclared*
/// epoch ships. The true condition has three parts:
///
/// 1. **Reordering or inserting is ALWAYS an epoch.** [`draw`] admits a pattern
///    only once its `requires` kind is present, so the order IS the grammar's
///    dependency order. Moving a pattern before its requirement silently drops
///    it; moving one after a pattern that requires it silently drops that one.
/// 2. **Appending a pattern with `at_locale: true` is an epoch.** A locale
///    composition feeds [`crate::interior::warmth_at`], which feeds a creature's
///    thermal drive, which is committed history.
/// 3. **Appending a pattern with `at_locale: false` is LATENT.** No live read can
///    reach it: [`selection`] filters it out, and the only other consumer is
///    [`selection_for`], whose output is read by the chamber renderer and by
///    nothing that commits. It becomes an epoch on the day something that
///    commits reads a chamber — the first in-chamber mark — and that is the day
///    the gate opens, not the day the pattern was written.
///
/// Sized near its intended scale deliberately, all the same: growth is cheap
/// today and will not stay cheap.
pub const INVENTORY: [Pattern; 14] = [
    // --- built, drawn at BOTH bands ---
    Pattern {
        name: "the-ground",
        kind: AnchorKind::Ground,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
    },
    Pattern {
        name: "the-threshold",
        kind: AnchorKind::Threshold,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
        // Every chamber has at least one link, so a doorway cannot be the
        // threshold role's private property (spec §4.1).
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
    },
    Pattern {
        name: "the-alcove",
        kind: AnchorKind::Alcove,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
        // THE ONE ROLE GATE THE WHOLE GRAMMAR HANGS OFF. See `the-fire`.
        roles: &[Role::Hearthroom],
        at_locale: true,
        needs_populous: false,
    },
    Pattern {
        name: "the-fire",
        kind: AnchorKind::Hearth,
        attach: Attach::Within(AnchorKind::Alcove),
        requires: Some(AnchorKind::Alcove),
        needs_cold: true,
        built: true,
        // NO ROLE WITHHOLDS THE FIRE, and it still burns in exactly one room.
        // It requires an alcove and only `Hearthroom` admits one, so the fire is
        // confined to the hearthroom by the GRAMMAR rather than by a rule anyone
        // wrote. And since a wall is a cell (Task 4b), a fire within an alcove is
        // a recess in a wall with a fire in it: a FIREPLACE.
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
    },
    Pattern {
        name: "the-fireside-bed",
        kind: AnchorKind::Bed,
        attach: Attach::Beside(AnchorKind::Hearth),
        requires: Some(AnchorKind::Hearth),
        needs_cold: true,
        built: true,
        // Confined the same way, one link further along the chain: a bed by the
        // fire needs a fire, which needs an alcove, which only one role admits.
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
    },
    Pattern {
        name: "the-water-jar",
        kind: AnchorKind::Vessel,
        attach: Attach::Beside(AnchorKind::Ground),
        requires: None,
        needs_cold: false,
        built: true,
        roles: STORING_ROLES,
        at_locale: true,
        needs_populous: false,
    },
    Pattern {
        name: "the-screen",
        kind: AnchorKind::Screen,
        attach: Attach::Beside(AnchorKind::Threshold),
        requires: Some(AnchorKind::Threshold),
        needs_cold: false,
        built: true,
        // A screen affords nothing and shapes sightlines, which is a thing worth
        // doing beside exactly one doorway: the one strangers come through.
        roles: &[Role::Threshold],
        at_locale: true,
        needs_populous: false,
    },
    // --- wild ---
    Pattern {
        name: "the-clearing",
        kind: AnchorKind::Ground,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: false,
        // A hollow has a floor and a pool whatever anyone would use it for; the
        // role layer has nothing to say about unbuilt ground.
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
    },
    Pattern {
        name: "the-pool",
        kind: AnchorKind::Pool,
        attach: Attach::Beside(AnchorKind::Ground),
        requires: None,
        needs_cold: false,
        built: false,
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
    },
    // --- built, CHAMBER BAND ONLY (`at_locale: false`) ---
    //
    // Appended, never inserted: each requires a kind an EARLIER pattern
    // contributes, so the append position is also the dependency-correct one.
    Pattern {
        name: "the-strongbox",
        kind: AnchorKind::Strongbox,
        attach: Attach::Beside(AnchorKind::Vessel),
        requires: Some(AnchorKind::Vessel),
        needs_cold: false,
        built: true,
        roles: &[Role::Store],
        at_locale: false,
        needs_populous: true,
    },
    Pattern {
        name: "the-high-seat",
        kind: AnchorKind::HighSeat,
        attach: Attach::Beside(AnchorKind::Threshold),
        requires: Some(AnchorKind::Threshold),
        needs_cold: false,
        built: true,
        // A high seat is set where whoever sits in it sees who comes in. That is
        // what the seat is FOR, so `Beside(Threshold)` is the pattern, not decor.
        roles: &[Role::Hall],
        at_locale: false,
        needs_populous: false,
    },
    Pattern {
        name: "the-loom",
        kind: AnchorKind::Loom,
        attach: Attach::Beside(AnchorKind::Threshold),
        requires: Some(AnchorKind::Threshold),
        needs_cold: false,
        built: true,
        // Weaving wants light, and in a building with no windows the doorway is
        // where the light is.
        roles: &[Role::Loomroom],
        at_locale: false,
        needs_populous: false,
    },
    Pattern {
        name: "the-anvil",
        kind: AnchorKind::Anvil,
        attach: Attach::Beside(AnchorKind::Vessel),
        requires: Some(AnchorKind::Vessel),
        needs_cold: false,
        built: true,
        // The quench. An anvil without water within arm's reach is a smithy
        // nobody could work in, so the water jar is the anvil's requirement and
        // not merely its neighbour.
        roles: &[Role::Smithy],
        at_locale: false,
        needs_populous: false,
    },
    Pattern {
        name: "the-altar",
        kind: AnchorKind::Altar,
        attach: Attach::Beside(AnchorKind::Vessel),
        requires: Some(AnchorKind::Vessel),
        needs_cold: false,
        built: true,
        // The washing the rite asks for before it begins.
        roles: &[Role::Shrine],
        at_locale: false,
        needs_populous: false,
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
    // The LOCALE band: no role (a creature stands in a place, not in a room with
    // a purpose), never populous (no locale-band pattern is population-gated),
    // and only patterns this band draws at all.
    draw(built, cold, false, |p| p.at_locale)
}

/// The patterns a CHAMBER of `role` draws. [`selection`]'s sibling, sharing its
/// composer and its admissibility walk and differing only in the declared
/// vocabulary — which is spec §4.1's claim ("the same composer, a different
/// declared vocabulary") reduced to one predicate argument.
///
/// `populous` is [`crate::brief::Brief::is_populous`]: whether the place ever
/// held more people than a hamlet.
///
/// It does NOT filter on `at_locale`: a chamber draws the shared vocabulary
/// *and* the chamber-only patterns. That asymmetry is the whole gate — see
/// [`Pattern::at_locale`].
/// type-audit: bare-ok(flag: built), bare-ok(flag: cold), bare-ok(flag: populous)
pub fn selection_for(role: Role, built: bool, cold: bool, populous: bool) -> Vec<&'static Pattern> {
    draw(built, cold, populous, |p| p.roles.contains(&role))
}

/// The one admissibility walk. `admits` is the band's declared vocabulary; every
/// other gate is a property of the place.
///
/// Order-sensitive by design, and shared so that the two bands cannot drift into
/// two different readings of what "completes" means.
fn draw(
    built: bool,
    cold: bool,
    populous: bool,
    admits: impl Fn(&'static Pattern) -> bool,
) -> Vec<&'static Pattern> {
    let mut out: Vec<&'static Pattern> = Vec::new();
    let mut present: std::collections::BTreeSet<AnchorKind> = std::collections::BTreeSet::new();
    for p in INVENTORY.iter() {
        if p.built != built {
            continue;
        }
        if p.needs_cold && !cold {
            continue;
        }
        if p.needs_populous && !populous {
            continue;
        }
        if !admits(p) {
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

/// The role of the chamber at `chamber_index` in a structure whose place is
/// described by `brief`.
///
/// Derived, never authored. Two facts about [`crate::structure::Structure`] make
/// the index a legitimate input rather than an arbitrary one: `chambers[0]` is
/// always the chamber `enter` arrives in, and `links` is a path graph in depth
/// order, so a HIGHER INDEX IS DEEPER. So this reads "the front room, the room
/// behind it, and then the rooms the place's own business fills".
///
/// The brief only reaches the chambers a structure has *room* for: index 2 is the
/// first that consults it, which is why a two-chamber dwelling differentiates on
/// nothing but depth. That is deliberate — a hamlet's hut is a front room and a
/// hearthroom, and claiming a shrine in it would be the catalogue §4.3 forbids.
/// type-audit: bare-ok(index: chamber_index)
pub fn role_for(chamber_index: usize, brief: &crate::brief::Brief) -> Role {
    use hornvale_history::record::{Function, Notability};
    match chamber_index {
        0 => Role::Threshold,
        1 => Role::Hearthroom,
        2 => match (brief.notability, brief.function) {
            (Some(Notability::Seat), _) => Role::Hall,
            (_, Some(Function::Agrarian)) => Role::Loomroom,
            // A garrison and a mine both work iron, and this inventory has one
            // anvil. Two functions sharing a role is honest; inventing a fourth
            // craft to keep them apart would be pattern count masquerading as
            // substance (spec §4.3).
            (_, Some(Function::Mine | Function::Fort)) => Role::Smithy,
            (_, Some(Function::Cult)) => Role::Shrine,
            // A waypoint's third room is what a waypoint is for: keeping goods.
            (_, Some(Function::Trade)) | (_, None) => Role::Store,
        },
        _ => Role::Store,
    }
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

    /// A brief with no alive occupation — a place whose deep chambers are stores.
    fn plain_brief() -> crate::brief::Brief {
        crate::brief::Brief::from_parts(None, None, None, None, 0, true, false)
    }

    #[test]
    fn the_locale_band_draws_exactly_what_it_drew() {
        // THE EPOCH GATE, asserted as a list rather than as a property. Task 6
        // appended five patterns; if any of them reaches this list, the warmth a
        // creature's committed thermal drive read has changed, and that is an
        // epoch. Written out so a future append cannot pass by being "the same
        // shape".
        let warm: Vec<&str> = selection(true, false).iter().map(|p| p.name).collect();
        assert_eq!(
            warm,
            [
                "the-ground",
                "the-threshold",
                "the-alcove",
                "the-water-jar",
                "the-screen"
            ],
            "the LOCALE band's warm built composition moved: this is an epoch"
        );
        let cold: Vec<&str> = selection(true, true).iter().map(|p| p.name).collect();
        assert_eq!(
            cold,
            [
                "the-ground",
                "the-threshold",
                "the-alcove",
                "the-fire",
                "the-fireside-bed",
                "the-water-jar",
                "the-screen"
            ],
            "the LOCALE band's cold built composition moved: this is an epoch"
        );
        let wild: Vec<&str> = selection(false, false).iter().map(|p| p.name).collect();
        assert_eq!(wild, ["the-clearing", "the-pool"]);
    }

    #[test]
    fn no_pattern_the_locale_band_declines_can_reach_it_by_any_route() {
        // The structural counterpart to the list above: `selection` filters on
        // `at_locale`, so a chamber-only pattern is unreachable from the locale
        // band whatever gates it also carries. This is what makes the LATENT
        // outcome auditable — disarm it by flipping one `at_locale` to `true` and
        // the test above fires too.
        let locale_names: Vec<&str> = [(true, true), (true, false), (false, true), (false, false)]
            .into_iter()
            .flat_map(|(b, c)| selection(b, c))
            .map(|p| p.name)
            .collect();
        for p in INVENTORY.iter().filter(|p| !p.at_locale) {
            assert!(
                !locale_names.contains(&p.name),
                "{} is chamber-only and reached a locale composition",
                p.name
            );
        }
        assert!(
            INVENTORY.iter().any(|p| !p.at_locale),
            "no pattern is chamber-only, so this test asserts nothing"
        );
    }

    #[test]
    fn only_the_hearthroom_can_hold_a_fire_and_no_rule_says_so() {
        // THE CLAIM THAT MAKES THIS A LANGUAGE RATHER THAN A CATALOGUE. Nothing
        // forbids the fire to any role — `the-fire` declares `EVERY_ROLE` — and
        // it still burns in exactly one, because it REQUIRES an alcove and only
        // the hearthroom admits one. The confinement is a consequence of the
        // grammar, not a rule anyone wrote, and this test asserts both halves so
        // that a future edit which "helpfully" adds a role gate to the fire is
        // recognized as a loss rather than a tidy-up.
        let fire = INVENTORY
            .iter()
            .find(|p| p.name == "the-fire")
            .expect("the-fire is authored");
        assert_eq!(
            fire.roles.len(),
            EVERY_ROLE.len(),
            "the fire must be withheld from NO role; its confinement is grammatical"
        );
        for &role in EVERY_ROLE {
            let has_fire = selection_for(role, true, true, true)
                .iter()
                .any(|p| p.kind == AnchorKind::Hearth);
            assert_eq!(
                has_fire,
                role == Role::Hearthroom,
                "{role:?} and the fire disagree"
            );
        }
    }

    #[test]
    fn every_role_composes_something_the_validator_accepts() {
        // Swept over roles AND over the place-gates, because
        // `chamber_interior_of` is the only composer the session calls and a
        // role whose composition is unwalkable would strand a possession.
        for &role in EVERY_ROLE {
            for (built, cold, populous) in [
                (true, true, true),
                (true, true, false),
                (true, false, true),
                (true, false, false),
                (false, false, false),
            ] {
                let interior = compose(&selection_for(role, built, cold, populous));
                assert!(
                    permits(&interior),
                    "{role:?} (built={built}, cold={cold}, populous={populous}) \
                     composes an interior the validator rejects"
                );
                for a in interior.ids() {
                    for b in interior.ids() {
                        assert!(
                            crate::interior::route_within(&interior, a, b, 256).is_some(),
                            "{role:?}: no route {a:?} -> {b:?}"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn the_role_is_derived_from_depth_and_then_from_the_brief() {
        use hornvale_history::record::{Function, Notability};
        let plain = plain_brief();
        assert_eq!(role_for(0, &plain), Role::Threshold);
        assert_eq!(role_for(1, &plain), Role::Hearthroom);
        assert_eq!(role_for(2, &plain), Role::Store);
        let fort = crate::brief::Brief::from_parts(
            Some(Function::Fort),
            None,
            Some(Notability::Common),
            None,
            0,
            true,
            false,
        );
        let farm = crate::brief::Brief::from_parts(
            Some(Function::Agrarian),
            None,
            Some(Notability::Common),
            None,
            0,
            true,
            false,
        );
        assert_ne!(
            role_for(2, &fort),
            role_for(2, &farm),
            "a fort and a farm must not furnish the same third room (spec §9)"
        );
        // Notability outranks function: a seat's own chamber is a hall whatever
        // the place's trade is.
        let seat = crate::brief::Brief::from_parts(
            Some(Function::Agrarian),
            None,
            Some(Notability::Seat),
            None,
            0,
            true,
            false,
        );
        assert_eq!(role_for(2, &seat), Role::Hall);
        // The front two rooms are the place's own regardless of its business.
        for b in [&fort, &farm, &seat] {
            assert_eq!(role_for(0, b), Role::Threshold);
            assert_eq!(role_for(1, b), Role::Hearthroom);
        }
    }

    #[test]
    fn a_fort_and_a_farm_draw_different_things_not_more_things() {
        use hornvale_history::record::{Function, Notability};
        let of = |f: Function| {
            let b = crate::brief::Brief::from_parts(
                Some(f),
                None,
                Some(Notability::Common),
                None,
                0,
                true,
                false,
            );
            selection_for(role_for(2, &b), true, false, false)
                .iter()
                .map(|p| p.name)
                .collect::<Vec<_>>()
        };
        let fort = of(Function::Fort);
        let farm = of(Function::Agrarian);
        assert!(
            fort.iter().any(|n| !farm.contains(n)) && farm.iter().any(|n| !fort.contains(n)),
            "one place's third room is a superset of the other's, which is a tier \
             list rather than a vocabulary: {fort:?} vs {farm:?}"
        );
    }

    #[test]
    fn the_strongbox_is_gated_by_scale_and_by_the_shared_ceiling() {
        // `peak_population`'s only reader. The threshold is HOISTED, not
        // re-typed, so this asserts against the same constant the ruin model
        // reads — one number, one meaning.
        let ceiling = hornvale_history::flesh::HAMLET_POPULATION_CEILING;
        let hamlet = crate::brief::Brief::from_parts(None, None, None, None, ceiling, true, false);
        let town =
            crate::brief::Brief::from_parts(None, None, None, None, ceiling + 1, true, false);
        assert!(!hamlet.is_populous(), "at the ceiling is still a hamlet");
        assert!(town.is_populous());
        let names = |b: &crate::brief::Brief| {
            selection_for(Role::Store, true, false, b.is_populous())
                .iter()
                .map(|p| p.name)
                .collect::<Vec<_>>()
        };
        assert!(!names(&hamlet).contains(&"the-strongbox"));
        assert!(names(&town).contains(&"the-strongbox"));
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

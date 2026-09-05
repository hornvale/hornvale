//! The structure-band grammar: how a built place's brief becomes a rooted
//! tree of chamber roles (The Cruck, spec §3). Form follows use here; a
//! wild site runs the other way and never reaches this module.
//!
//! Read the `admit` functions as a justified permeability graph (Hillier and
//! Hanson): a room's DEPTH from the entry is who controls access to it.
//! Cold nests everything on the hearth — one fire heats what opens off it —
//! and outranks the social rules. Authority sets depth: a `Command` people's
//! rooms hang off the hearthroom, a `Common` people's off the threshold.
//! Posture sets who reaches the workroom: `Outward` puts it at the door.
//! The store is the household's, never the guest's, so posture does not
//! move it. An ABSENT housemark takes the shallow reading (the threshold), the
//! same "universal only" answer The Housemark gives an absent mark.
//!
//! `RULES` is walked once, in order, and the order is the priority: the walk
//! stops admitting at `MAX_CHAMBERS`, so a Seat keeps its hall and drops the
//! store (spec §3.3). A rule whose parent role is absent is REFUSED, never
//! re-hung on the root — a room off the door for a reason nobody stated is
//! exactly what this module exists to prevent (ledger #2).

use super::{MAX_CHAMBERS, Role};
use crate::brief::Brief;
use crate::housemark::{AuthorityMark, ThresholdPosture};
use hornvale_history::record::{Function, Notability};

/// Where a chamber attaches at the structure band. No `Within`: a chamber
/// cannot contain a chamber, and "nested" IS `Beside(Hearthroom)` (ledger #5).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Attach {
    /// The threshold: the one chamber with no parent.
    Root,
    /// One aperture away from the first chamber of this role.
    Beside(Role),
}

/// One structure-band rule: a name, and what it admits for a brief —
/// `None` to refuse, else the role it contributes and where it hangs.
/// type-audit: bare-ok(identifier-text: name)
pub struct Rule {
    /// The rule's name, for diagnostics; never a selection key.
    pub name: &'static str,
    /// The admission and attachment, read from the brief.
    pub admit: fn(&Brief) -> Option<(Role, Attach)>,
}

/// A derived chamber tree: roles in walk order and each chamber's parent index.
/// `parents[0]` is `None`; every other entry is `Some(p)` with `p < i`.
/// type-audit: bare-ok(index: parents)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Frame {
    /// The role of each chamber, in admission order.
    pub roles: Vec<Role>,
    /// The index of each chamber's parent, `None` at the root.
    pub parents: Vec<Option<usize>>,
}

fn threshold(_: &Brief) -> Option<(Role, Attach)> {
    Some((Role::Threshold, Attach::Root))
}

fn hearthroom(_: &Brief) -> Option<(Role, Attach)> {
    Some((Role::Hearthroom, Attach::Beside(Role::Threshold)))
}

fn hall(b: &Brief) -> Option<(Role, Attach)> {
    (b.notability == Some(Notability::Seat))
        .then_some((Role::Hall, Attach::Beside(Role::Threshold)))
}

/// The place's business, and who reaches it (spec §3.1, workroom row).
fn workroom(b: &Brief) -> Option<(Role, Attach)> {
    let role = match b.function? {
        Function::Agrarian => Role::Loomroom,
        Function::Mine | Function::Fort => Role::Smithy,
        Function::Cult => Role::Shrine,
        // A waypoint's business IS keeping goods: the store rule covers it.
        Function::Trade => return None,
    };
    let beside = if b.cold {
        Role::Hearthroom
    } else {
        match b.housemark.map(|h| h.threshold) {
            Some(ThresholdPosture::Outward) => Role::Threshold,
            Some(ThresholdPosture::Inward) => Role::Hearthroom,
            Some(ThresholdPosture::Plain) | None => deep_or_shallow(b),
        }
    };
    Some((role, Attach::Beside(beside)))
}

/// Where the household keeps its goods (spec §3.1, store row).
fn store(b: &Brief) -> Option<(Role, Attach)> {
    b.function?;
    let beside = if b.cold {
        Role::Hearthroom
    } else {
        deep_or_shallow(b)
    };
    Some((Role::Store, Attach::Beside(beside)))
}

/// Authority's reading of depth: command hangs rooms off the hearth, common
/// (and an absent mark) off the door.
fn deep_or_shallow(b: &Brief) -> Role {
    match b.housemark.map(|h| h.authority) {
        Some(AuthorityMark::Command) => Role::Hearthroom,
        Some(AuthorityMark::Common) | None => Role::Threshold,
    }
}

/// The inventory, in priority order (spec §3.1's `order` column).
pub const RULES: [Rule; 5] = [
    Rule {
        name: "threshold",
        admit: threshold,
    },
    Rule {
        name: "hearthroom",
        admit: hearthroom,
    },
    Rule {
        name: "hall",
        admit: hall,
    },
    Rule {
        name: "workroom",
        admit: workroom,
    },
    Rule {
        name: "store",
        admit: store,
    },
];

/// The frame a built brief derives. Pure: no seed, no stream, no fact.
pub fn frame_for(brief: &Brief) -> Frame {
    walk(&RULES, brief)
}

/// The one admissibility walk, over any rule set — the seam that lets a test
/// prove the refusal arm with a rule no authored inventory carries.
pub(crate) fn walk(rules: &[Rule], brief: &Brief) -> Frame {
    let mut roles: Vec<Role> = Vec::new();
    let mut parents: Vec<Option<usize>> = Vec::new();
    for rule in rules {
        if roles.len() == MAX_CHAMBERS {
            break;
        }
        let Some((role, attach)) = (rule.admit)(brief) else {
            continue;
        };
        if roles.contains(&role) {
            continue;
        }
        let parent = match attach {
            Attach::Root => {
                if !roles.is_empty() {
                    continue;
                }
                None
            }
            Attach::Beside(p) => match roles.iter().position(|r| *r == p) {
                Some(i) => Some(i),
                None => continue,
            },
        };
        roles.push(role);
        parents.push(parent);
    }
    Frame { roles, parents }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::housemark::{AuthorityMark, Housemark, ThresholdPosture};
    use crate::site::{Site, SiteKind};
    use hornvale_history::record::{Function, Notability};

    fn dwelling(
        cold: bool,
        mark: Option<Housemark>,
        function: Option<Function>,
        notability: Notability,
    ) -> Brief {
        Brief::from_parts(
            function,
            None,
            Some(notability),
            None,
            mark,
            0,
            true,
            cold,
            Some(Site::placed(SiteKind::Settlement, None)),
            None,
        )
    }

    /// `(role, parent role)` per chamber, in walk order — the shape as the spec writes it.
    fn shape(brief: &Brief) -> Vec<(Role, Option<Role>)> {
        let f = frame_for(brief);
        f.roles
            .iter()
            .enumerate()
            .map(|(i, &r)| (r, f.parents[i].map(|p| f.roles[p])))
            .collect()
    }

    #[test]
    fn the_living_agrarian_dwelling_takes_exactly_the_preregistered_shape() {
        use AuthorityMark::*;
        use Role::*;
        use ThresholdPosture::*;
        let deep = vec![
            (Threshold, None),
            (Hearthroom, Some(Threshold)),
            (Loomroom, Some(Hearthroom)),
            (Store, Some(Hearthroom)),
        ];
        let bush = vec![
            (Threshold, None),
            (Hearthroom, Some(Threshold)),
            (Loomroom, Some(Threshold)),
            (Store, Some(Threshold)),
        ];
        let shopfront = vec![
            (Threshold, None),
            (Hearthroom, Some(Threshold)),
            (Loomroom, Some(Threshold)),
            (Store, Some(Hearthroom)),
        ];
        let backroom = vec![
            (Threshold, None),
            (Hearthroom, Some(Threshold)),
            (Loomroom, Some(Hearthroom)),
            (Store, Some(Threshold)),
        ];
        for cold in [true, false] {
            for authority in [Command, Common] {
                for posture in [Inward, Plain, Outward] {
                    let mark = Housemark {
                        authority,
                        threshold: posture,
                    };
                    let got = shape(&dwelling(
                        cold,
                        Some(mark),
                        Some(Function::Agrarian),
                        Notability::Common,
                    ));
                    let want = match (cold, authority, posture) {
                        (true, _, _) => &deep,
                        (false, Command, Outward) => &shopfront,
                        (false, Command, _) => &deep,
                        (false, Common, Inward) => &backroom,
                        (false, Common, _) => &bush,
                    };
                    assert_eq!(&got, want, "cold={cold} {authority:?} {posture:?}");
                }
            }
        }
    }

    #[test]
    fn a_place_with_no_business_is_a_threshold_and_a_hearth() {
        // function None: nobody's living occupation — a dead settlement's facet, or a synthetic brief.
        assert_eq!(
            shape(&dwelling(false, None, None, Notability::Common)),
            vec![
                (Role::Threshold, None),
                (Role::Hearthroom, Some(Role::Threshold))
            ]
        );
    }

    #[test]
    fn a_seat_keeps_its_hall_and_workroom_and_drops_the_store_at_the_bound() {
        let mark = Housemark {
            authority: AuthorityMark::Command,
            threshold: ThresholdPosture::Plain,
        };
        let got = frame_for(&dwelling(
            false,
            Some(mark),
            Some(Function::Agrarian),
            Notability::Seat,
        ));
        assert_eq!(
            got.roles,
            vec![
                Role::Threshold,
                Role::Hearthroom,
                Role::Hall,
                Role::Loomroom
            ]
        );
        assert_eq!(
            got.roles.len(),
            MAX_CHAMBERS,
            "the bound, not a fifth chamber"
        );
    }

    #[test]
    fn the_workroom_is_the_functions_own_and_trade_keeps_a_store_instead() {
        for (function, role) in [
            (Function::Mine, Role::Smithy),
            (Function::Fort, Role::Smithy),
            (Function::Cult, Role::Shrine),
            (Function::Agrarian, Role::Loomroom),
        ] {
            let f = frame_for(&dwelling(false, None, Some(function), Notability::Common));
            assert!(
                f.roles.contains(&role),
                "{function:?} → {role:?}: {:?}",
                f.roles
            );
        }
        let trade = frame_for(&dwelling(
            false,
            None,
            Some(Function::Trade),
            Notability::Common,
        ));
        assert_eq!(
            trade.roles,
            vec![Role::Threshold, Role::Hearthroom, Role::Store]
        );
    }

    #[test]
    fn every_frame_is_a_rooted_tree_with_no_role_twice_and_parent_before_child() {
        for brief in every_brief() {
            let f = frame_for(&brief);
            assert!(!f.roles.is_empty() && f.roles.len() <= MAX_CHAMBERS);
            assert_eq!(f.parents[0], None);
            for i in 1..f.roles.len() {
                assert!(f.parents[i].is_some_and(|p| p < i), "{:?}", f);
            }
            let distinct: std::collections::BTreeSet<Role> = f.roles.iter().copied().collect();
            assert_eq!(
                distinct.len(),
                f.roles.len(),
                "a role admitted twice: {:?}",
                f.roles
            );
        }
    }

    /// Every combination of the axes the grammar reads, generated.
    fn every_brief() -> Vec<Brief> {
        let mut out = Vec::new();
        let marks = [None].into_iter().chain(
            [AuthorityMark::Command, AuthorityMark::Common]
                .into_iter()
                .flat_map(|a| {
                    [
                        ThresholdPosture::Inward,
                        ThresholdPosture::Plain,
                        ThresholdPosture::Outward,
                    ]
                    .into_iter()
                    .map(move |t| {
                        Some(Housemark {
                            authority: a,
                            threshold: t,
                        })
                    })
                }),
        );
        for mark in marks {
            for cold in [true, false] {
                for function in [
                    None,
                    Some(Function::Agrarian),
                    Some(Function::Mine),
                    Some(Function::Fort),
                    Some(Function::Cult),
                    Some(Function::Trade),
                ] {
                    for notability in [Notability::Common, Notability::Seat] {
                        out.push(dwelling(cold, mark, function, notability));
                    }
                }
            }
        }
        out
    }

    #[test]
    fn a_rule_whose_parent_is_absent_is_refused_not_rehung_on_the_root() {
        // A synthetic inventory: a store beside a hall, and no hall rule at all.
        fn threshold(_: &Brief) -> Option<(Role, Attach)> {
            Some((Role::Threshold, Attach::Root))
        }
        fn orphan(_: &Brief) -> Option<(Role, Attach)> {
            Some((Role::Store, Attach::Beside(Role::Hall)))
        }
        let rules = [
            Rule {
                name: "threshold",
                admit: threshold,
            },
            Rule {
                name: "orphan",
                admit: orphan,
            },
        ];
        let f = walk(
            &rules,
            &dwelling(false, None, Some(Function::Agrarian), Notability::Common),
        );
        assert_eq!(
            f.roles,
            vec![Role::Threshold],
            "the orphan must be refused, not attached to the root"
        );
    }
}

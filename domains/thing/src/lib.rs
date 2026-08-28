//! The thing domain: what an object *kind* is, independent of any world.
//!
//! A thing-kind is a row in [`thing_registry`] — the same shape as
//! `hornvale_terrain::MaterialTraits` ("thin and honest": a kind with no
//! biosphere row carries only what a shipped verb reads). `ThingTraits`
//! carries just two facts today: whether a body may carry the kind, and the
//! label prose uses to name it. The *property* vocabulary (`Openable`,
//! `Lockable`, `AffordsPassage`, …) stays in `windows/vessel`'s
//! `ObjectTraits` until a later task in this campaign re-keys that table from
//! `AnchorKind` to [`KindId`] and merges it with this one — see spec §3.5.
//!
//! THIS DOMAIN DRAWS NOTHING, on the `hornvale_alchemy` model: there is no
//! `streams.rs`, no `StreamLabel`, and no `Seed` parameter anywhere in it.
//! What a kind *is* does not vary by world; which kinds a world places is
//! derived at the composition root (`windows/worldgen`) from state other
//! domains already drew.
//!
//! The roster is deliberately wider than the three kinds this campaign's
//! verbs need (`cave-mouth`, `strongbox`, `key`): a later task needs a total
//! mapping from every `AnchorKind` to a thing-kind, so every anchor kind gets
//! a row here too, carried at no cost to a world that never places one.

#![warn(missing_docs)]

use hornvale_kernel::{ComponentStore, KindId};

/// Object-kind traits: whether a body may carry the kind, and the label
/// prose uses to name it. Thin and honest (spec §3.5) — no property lives
/// here; see the module doc for where those stay until they join.
/// type-audit: bare-ok(flag: portable), bare-ok(identifier-text: display)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ThingTraits {
    /// Whether a body may take and carry the kind.
    pub portable: bool,
    /// The label prose uses to name the kind.
    pub display: &'static str,
}

/// The authored thing-kind labels. Every label here has a row in
/// [`thing_registry`], and every row in that registry has a label here —
/// see `roster_and_registry_agree_in_both_directions` below.
///
/// Three kinds are earned by a verb this campaign ships (spec §3.8):
/// `cave-mouth` and `strongbox` by `open`/`close`, `key` by `take`/`drop`.
/// The rest give every `AnchorKind` (`windows/vessel/src/interior/anchor.rs`)
/// a thing-kind counterpart, so a later task's `AnchorKind` → [`KindId`]
/// mapping can be total without adding a row here first.
/// type-audit: bare-ok(identifier-text)
pub const THING_KINDS: &[&str] = &[
    "alcove",
    "altar",
    "anvil",
    "bed",
    "cave-mouth",
    "ground",
    "hearth",
    "high-seat",
    "key",
    "log",
    "loom",
    "pool",
    "screen",
    "strongbox",
    "threshold",
    "vessel",
];

/// The canonical thing-kind registry. Only `key` is portable today; the rest
/// are fixtures a body may use in place but never carry — including
/// `cave-mouth` and `strongbox`, whose `Openable`/`Lockable` properties
/// arrive when `ObjectTraits` joins this table (spec §3.5, §3.8).
pub fn thing_registry() -> ComponentStore<KindId, ThingTraits> {
    [
        (
            KindId("alcove"),
            ThingTraits {
                portable: false,
                display: "alcove",
            },
        ),
        (
            KindId("altar"),
            ThingTraits {
                portable: false,
                display: "altar",
            },
        ),
        (
            KindId("anvil"),
            ThingTraits {
                portable: false,
                display: "anvil",
            },
        ),
        (
            KindId("bed"),
            ThingTraits {
                portable: false,
                display: "bed",
            },
        ),
        (
            KindId("cave-mouth"),
            ThingTraits {
                portable: false,
                display: "cave mouth",
            },
        ),
        (
            KindId("ground"),
            ThingTraits {
                portable: false,
                display: "ground",
            },
        ),
        (
            KindId("hearth"),
            ThingTraits {
                portable: false,
                display: "hearth",
            },
        ),
        (
            KindId("high-seat"),
            ThingTraits {
                portable: false,
                display: "high seat",
            },
        ),
        (
            KindId("key"),
            ThingTraits {
                portable: true,
                display: "key",
            },
        ),
        (
            KindId("log"),
            ThingTraits {
                portable: false,
                display: "log",
            },
        ),
        (
            KindId("loom"),
            ThingTraits {
                portable: false,
                display: "loom",
            },
        ),
        (
            KindId("pool"),
            ThingTraits {
                portable: false,
                display: "pool",
            },
        ),
        (
            KindId("screen"),
            ThingTraits {
                portable: false,
                display: "screen",
            },
        ),
        (
            KindId("strongbox"),
            ThingTraits {
                portable: false,
                display: "strongbox",
            },
        ),
        (
            KindId("threshold"),
            ThingTraits {
                portable: false,
                display: "threshold",
            },
        ),
        (
            KindId("vessel"),
            ThingTraits {
                portable: false,
                display: "vessel",
            },
        ),
    ]
    .into_iter()
    .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The roster is non-empty and every label is unique — a duplicate label
    /// would make two kinds share one `KindId`, and every fact about either
    /// would key to the same row.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: change `THING_KINDS`'s `"loom"` entry
    /// to `"log"` (a duplicate of the existing `"log"` row). Red observed:
    ///
    /// ```text
    /// thread 'tests::thing_kind_labels_are_unique' panicked at domains/thing/src/lib.rs:202:13:
    /// duplicate thing-kind label "log"
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 1 filtered out
    /// ```
    #[test]
    fn thing_kind_labels_are_unique() {
        let mut seen = std::collections::BTreeSet::new();
        for label in THING_KINDS {
            assert!(seen.insert(*label), "duplicate thing-kind label {label:?}");
        }
        assert!(!THING_KINDS.is_empty(), "the roster may not be empty");
    }

    /// Every label in the roster has a row in the registry, and the registry
    /// has no row the roster does not name — the two-way agreement decision
    /// 0261 requires of a rule written down twice.
    #[test]
    fn roster_and_registry_agree_in_both_directions() {
        let reg = thing_registry();
        for label in THING_KINDS {
            assert!(
                reg.get(&KindId(label)).is_some(),
                "roster names {label:?}, registry does not"
            );
        }
        for id in reg.ids() {
            assert!(
                THING_KINDS.contains(&id.0),
                "registry has {:?}, roster does not",
                id.0
            );
        }
    }
}

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

use hornvale_kernel::{
    ComponentStore, ConceptDef, ConceptKind, ConceptRegistry, Correspondent, KindId, Manifest,
    RegistryError, Void,
};

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

/// A short, honest gloss for each roster label — what a player would call
/// the kind, not a mechanic. Kept beside [`THING_KINDS`] rather than folded
/// into [`ThingTraits::display`], which names the kind in running prose, not
/// what it *is*.
fn concept_doc(label: &str) -> &'static str {
    match label {
        "alcove" => "a recessed space set into a wall",
        "altar" => "a raised surface where offerings are made",
        "anvil" => "a heavy iron block a smith hammers metal against",
        "bed" => "a place made for lying down and sleeping",
        "cave-mouth" => "the opening where a cave meets the outside",
        "ground" => "the bare earth underfoot",
        "hearth" => "the fire at the center of a home",
        "high-seat" => "the seat of a hall's presiding figure",
        "key" => "a small tool shaped to work one particular lock",
        "log" => "a length of felled, unworked timber",
        "loom" => "a frame for weaving thread into cloth",
        "pool" => "a small standing body of water",
        "screen" => "a partition set up to divide or shield a space",
        "strongbox" => "a locked chest built to keep valuables safe",
        "threshold" => "the sill marking where one place ends and another begins",
        "vessel" => "a container shaped to hold liquid or goods",
        other => unreachable!("concept_doc has no gloss for thing-kind {other:?}"),
    }
}

/// Register thing's contribution to the concept registry: every label in
/// [`THING_KINDS`] becomes a nameable concept.
///
/// No language pack names any thing-kind yet, so the lexeme edge is an
/// honest `Gap` rather than an over-optimistic `Expected` (the same choice
/// `settlement`'s home/hearth pair makes, and for the same reason).
///
/// **Check-then-map, never a homonym (decision 0025).** `hearth` is already
/// a nameable concept — `settlement` registers it (domain `settlement`,
/// [`ConceptKind::Social`], "the fire at the center of a home") as a social
/// space, and that registration is semantically broader than this domain's
/// reading of `hearth` as a fixture a body may stand at. Decision 0025 gives
/// the earlier, broader registrant the word; `thing` maps to the existing
/// concept instead of minting a conflicting redefinition under its own
/// domain, which `ConceptRegistry::register_manifest` would otherwise reject
/// as [`RegistryError::ConflictingDefinition`] the moment both domains sit on
/// one roster. Every other label here is thing's alone.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    for label in THING_KINDS {
        if registry.concept(label).is_some() {
            continue;
        }
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: label.to_string(),
                domain: "thing".to_string(),
                kind: ConceptKind::Object,
                doc: concept_doc(label).to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap("no language pack names it yet")),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }
    Ok(())
}

/// Thing as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Thing;

impl hornvale_kernel::Domain for Thing {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
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

    /// Every roster kind is a registered concept after `register_concepts`, and
    /// each carries this domain's own name — so the registry dump attributes
    /// them here and not to whichever crate happened to call the function.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: replace `register_concepts`'s body
    /// with `Ok(())` (registration becomes vacuous — nothing is registered,
    /// but the function still reports success). Red observed:
    ///
    /// ```text
    /// thread 'tests::every_roster_kind_registers_as_a_concept' panicked at domains/thing/src/lib.rs:323:36:
    /// "alcove" not registered
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 1 filtered out
    /// ```
    #[test]
    fn every_roster_kind_registers_as_a_concept() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        register_concepts(&mut reg).expect("registration is total and idempotent");
        for label in THING_KINDS {
            let c = reg
                .concept(label)
                .unwrap_or_else(|| panic!("{label:?} not registered"));
            assert_eq!(
                c.domain, "thing",
                "{label:?} attributed to the wrong domain"
            );
        }
    }

    /// Re-registering must succeed and change nothing — every domain's
    /// `register_concepts` relies on `ConceptRegistry` accepting a
    /// byte-identical redefinition (`domains/person/src/lib.rs:199-205` is
    /// the precedent this follows).
    #[test]
    fn registering_twice_is_idempotent() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        register_concepts(&mut reg).expect("first registration");
        register_concepts(&mut reg).expect("second registration is idempotent");
    }

    /// Decision 0025 (one concept name, one owner): `hearth` is already a
    /// concept when `settlement` registers it first, and `thing` must map to
    /// that existing definition rather than mint a conflicting redefinition
    /// under its own domain. `thing` cannot depend on `hornvale-settlement`
    /// (domains depend on the kernel only), so this hand-rolls settlement's
    /// exact registration to stand in for it.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: delete the `if
    /// registry.concept(label).is_some() { continue; }` guard in
    /// `register_concepts`. Red observed:
    ///
    /// ```text
    /// thread 'tests::hearth_maps_to_an_existing_owner_instead_of_conflicting' panicked at domains/thing/src/lib.rs:389:14:
    /// thing must map to the existing `hearth` concept, not conflict with it: ConflictingDefinition { name: "hearth" }
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 1 filtered out
    /// ```
    #[test]
    fn hearth_maps_to_an_existing_owner_instead_of_conflicting() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_manifest(Manifest {
            concept: ConceptDef {
                name: "hearth".to_string(),
                domain: "settlement".to_string(),
                kind: ConceptKind::Social,
                doc: "the fire at the center of a home".to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap("no language pack names it yet")),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })
        .expect("settlement's own registration");

        register_concepts(&mut reg)
            .expect("thing must map to the existing `hearth` concept, not conflict with it");
        assert_eq!(
            reg.concept("hearth").unwrap().domain,
            "settlement",
            "thing must not steal ownership of a word another domain already registered"
        );
    }
}

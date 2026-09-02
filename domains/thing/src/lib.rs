//! The thing domain: what an object *kind* is, independent of any world.
//!
//! A thing-kind is a row in [`thing_registry`] — the same shape as
//! `hornvale_terrain::MaterialTraits` ("thin and honest": a kind with no
//! biosphere row carries only what a shipped verb reads). `ThingTraits`
//! carries exactly one fact today: the label prose uses to name the kind.
//!
//! **The property vocabulary lives in `windows/vessel`'s `ObjectTraits`, and
//! this crate deliberately holds no second copy of any of it (Task 7, spec
//! §3.6).** Task 2 gave `ThingTraits` a `portable: bool` keyed on
//! [`KindId`]; Task 7 re-keyed `ObjectTraits` to [`KindId`] as well and added
//! `ObjectProperty::Portable`, at which point the two were two
//! [`KindId`]-keyed tables answering one question — the disagreement §3.6
//! re-keys to prevent, minted by the task meant to prevent it. So the field
//! was deleted, not kept: `ObjectProperty::Portable` is the single source of
//! truth for whether a body may carry a kind, sitting beside `Openable` and
//! `Lockable` where the verb dispatcher already reads. Nothing consumed the
//! field (no access to it existed outside this file), so the deletion moved
//! the fact without changing it.
//!
//! THIS DOMAIN DRAWS NOTHING, on the `hornvale_alchemy` model: there is no
//! `streams.rs`, no `StreamLabel`, and no `Seed` parameter anywhere in it.
//! What a kind *is* does not vary by world; which kinds a world places is
//! derived at the composition root (`windows/worldgen`) from state other
//! domains already drew.
//!
//! The roster is deliberately wider than the three kinds this campaign's
//! verbs need (`cave-mouth`, `strongbox`, `key`): every kind a room's grammar
//! can place carries a row here too, at no cost to a world that never places
//! one. It was written wide so that The Wicket's Task 2 could re-key
//! `windows/vessel`'s interior grammar onto [`KindId`] without adding a row
//! first; that task has landed, and the width is now simply what the grammar
//! needs.

#![warn(missing_docs)]

use hornvale_kernel::{
    ComponentStore, ConceptDef, ConceptKind, ConceptRegistry, Correspondent, KindId, Manifest,
    RegistryError, Void,
};

/// Object-kind traits: the label prose uses to name the kind. Thin and
/// honest (spec §3.5) — no property lives here; see the module doc for why
/// `portable` was deleted rather than kept beside `ObjectProperty::Portable`.
/// type-audit: bare-ok(identifier-text: display), bare-ok(prose: doc)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ThingTraits {
    /// The label prose uses to name the kind.
    pub display: &'static str,
    /// The gloss the concept registry publishes for this kind, or `None`
    /// where another domain owns the concept — see [`BORROWED`]. A gloss
    /// authored here for a borrowed kind would be a second, divergent
    /// definition of prose that domain already owns.
    pub doc: Option<&'static str>,
}

/// The authored thing-kind labels. Every label here has a row in
/// [`thing_registry`], and every row in that registry has a label here —
/// see `roster_and_registry_agree_in_both_directions` below.
///
/// Three kinds are earned by a verb this campaign ships (spec §3.8):
/// `cave-mouth` and `strongbox` by `open`/`close`, `key` by `take`/`drop`.
/// The rest are the kinds `windows/vessel`'s pattern grammar composes into a
/// room. They were written here first so The Wicket's Task 2 could delete the
/// closed anchor-kind enum and point the grammar straight at these labels
/// without adding a row in the same change.
/// type-audit: bare-ok(identifier-text)
pub const THING_KINDS: &[&str] = &[
    "alcove",
    "altar",
    "anvil",
    "bed",
    "brazier",
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

/// Named handles for the kinds that code names.
///
/// **A handle is a convenience; a variant was a requirement.** This is the
/// asymmetry the whole campaign turns on. The closed `AnchorKind` enum that
/// `windows/vessel` carried until The Wicket's Task 2 made a variant
/// mandatory: a kind with no variant could not be placed in a room, however
/// open the stores behind it were. A handle is the opposite — it exists so a
/// predicate can say `kind == kinds::HEARTH` instead of `kind ==
/// KindId("hearth")` and have the compiler catch the typo. A kind with **no**
/// handle is a first-class kind that simply has no predicate written against
/// it, and adding a kind never requires adding one here.
///
/// So: add a handle when you write code that names the kind. Do not add one
/// "for completeness" — an unused handle is a name with no reader.
pub mod kinds {
    use hornvale_kernel::KindId;

    /// A recess off the main space.
    pub const ALCOVE: KindId = KindId("alcove");
    /// An altar.
    pub const ALTAR: KindId = KindId("altar");
    /// A smith's anvil.
    pub const ANVIL: KindId = KindId("anvil");
    /// A place to sleep.
    pub const BED: KindId = KindId("bed");
    /// A vessel of fire, standing apart from a hearth.
    pub const BRAZIER: KindId = KindId("brazier");
    /// The mouth of a cave — a `Vertex`/`ChamberAddr`, never an anchor.
    pub const CAVE_MOUTH: KindId = KindId("cave-mouth");
    /// The room's open middle: the floor itself, not a thing standing on it.
    pub const GROUND: KindId = KindId("ground");
    /// A fire.
    pub const HEARTH: KindId = KindId("hearth");
    /// The seat that commands the entrance.
    pub const HIGH_SEAT: KindId = KindId("high-seat");
    /// A small key.
    pub const KEY: KindId = KindId("key");
    /// A fallen log.
    pub const LOG: KindId = KindId("log");
    /// An upright loom.
    pub const LOOM: KindId = KindId("loom");
    /// A natural pool.
    pub const POOL: KindId = KindId("pool");
    /// A screen or pillar: affords nothing, shapes sightlines.
    pub const SCREEN: KindId = KindId("screen");
    /// A locked chest.
    pub const STRONGBOX: KindId = KindId("strongbox");
    /// A doorway — an anchor that is also a room-graph edge.
    pub const THRESHOLD: KindId = KindId("threshold");
    /// A water vessel or basin.
    pub const VESSEL: KindId = KindId("vessel");

    /// Every handle with its own name, for the roster check.
    ///
    /// Hand-written, and that is a deliberate cost rather than an oversight:
    /// there is no macro here because a macro generating both the constants
    /// and this list would make the list unable to disagree with them. That
    /// is what made the deleted `anchor_kinds!` macro's own roster safe, and
    /// it is exactly what is NOT wanted here — this list is checked against
    /// the ROSTER, a third party, so it must be able to go wrong.
    /// type-audit: bare-ok(identifier-text)
    pub const EVERY_HANDLE: &[(&str, KindId)] = &[
        ("ALCOVE", ALCOVE),
        ("ALTAR", ALTAR),
        ("ANVIL", ANVIL),
        ("BED", BED),
        ("BRAZIER", BRAZIER),
        ("CAVE_MOUTH", CAVE_MOUTH),
        ("GROUND", GROUND),
        ("HEARTH", HEARTH),
        ("HIGH_SEAT", HIGH_SEAT),
        ("KEY", KEY),
        ("LOG", LOG),
        ("LOOM", LOOM),
        ("POOL", POOL),
        ("SCREEN", SCREEN),
        ("STRONGBOX", STRONGBOX),
        ("THRESHOLD", THRESHOLD),
        ("VESSEL", VESSEL),
    ];
}

/// The canonical thing-kind registry: one row per [`THING_KINDS`] label,
/// carrying the display name and nothing else. Which kinds are portable,
/// openable or lockable is `windows/vessel`'s `object_registry`
/// (`ObjectProperty::{Portable, Openable, Lockable}`, spec §3.6/§3.8) — one
/// table, not two.
pub fn thing_registry() -> ComponentStore<KindId, ThingTraits> {
    [
        (
            KindId("alcove"),
            ThingTraits {
                display: "alcove",
                doc: Some("a recessed space set into a wall"),
            },
        ),
        (
            KindId("altar"),
            ThingTraits {
                display: "altar",
                doc: Some("a raised surface where offerings are made"),
            },
        ),
        (
            KindId("anvil"),
            ThingTraits {
                display: "anvil",
                doc: Some("a heavy iron block a smith hammers metal against"),
            },
        ),
        (
            KindId("bed"),
            ThingTraits {
                display: "bed",
                doc: Some("a place made for lying down and sleeping"),
            },
        ),
        (
            KindId("brazier"),
            ThingTraits {
                display: "brazier",
                doc: Some("a metal basin that holds a fire apart from a hearth"),
            },
        ),
        (
            KindId("cave-mouth"),
            ThingTraits {
                display: "cave mouth",
                doc: Some("the opening where a cave meets the outside"),
            },
        ),
        (
            KindId("ground"),
            ThingTraits {
                display: "ground",
                doc: Some("the bare earth underfoot"),
            },
        ),
        (
            KindId("hearth"),
            ThingTraits {
                display: "hearth",
                // BORROWED (ceded to settlement, decision 0025) — a gloss
                // here would be a second, divergent definition of a concept
                // that domain already owns. See `ThingTraits::doc`.
                doc: None,
            },
        ),
        (
            KindId("high-seat"),
            ThingTraits {
                display: "high seat",
                doc: Some("the seat of a hall's presiding figure"),
            },
        ),
        (
            KindId("key"),
            ThingTraits {
                display: "key",
                doc: Some("a small tool shaped to work one particular lock"),
            },
        ),
        (
            KindId("log"),
            ThingTraits {
                display: "log",
                doc: Some("a length of felled, unworked timber"),
            },
        ),
        (
            KindId("loom"),
            ThingTraits {
                display: "loom",
                doc: Some("a frame for weaving thread into cloth"),
            },
        ),
        (
            KindId("pool"),
            ThingTraits {
                display: "pool",
                doc: Some("a small standing body of water"),
            },
        ),
        (
            KindId("screen"),
            ThingTraits {
                display: "screen",
                doc: Some("a partition set up to divide or shield a space"),
            },
        ),
        (
            KindId("strongbox"),
            ThingTraits {
                display: "strongbox",
                doc: Some("a locked chest built to keep valuables safe"),
            },
        ),
        (
            KindId("threshold"),
            ThingTraits {
                display: "threshold",
                doc: Some("the sill marking where one place ends and another begins"),
            },
        ),
        (
            KindId("vessel"),
            ThingTraits {
                display: "vessel",
                doc: Some("a container shaped to hold liquid or goods"),
            },
        ),
    ]
    .into_iter()
    .collect()
}

/// Labels this domain deliberately cedes to an earlier, broader registrant
/// (decision 0025), paired with the domain each is ceded to.
///
/// **Declared, not inferred, and checked in both directions by
/// [`register_concepts`]**: a bare `if registry.concept(label).is_some() {
/// continue; }` would cede silently and forever, in either direction — a
/// future domain claiming an undeclared `THING_KINDS` label (`pool` in
/// hydrology, say) would transfer ownership with nothing reddening, and a
/// stale entry here (its owner retiring the concept) would go unnoticed
/// just as quietly. This is the same shape a cross-crate exception gets
/// everywhere else in this repo — type-audit's `waiver(<reason>)`,
/// seam-guard's `expect(survives: …)` plus its STALE-DECL verdict,
/// `subfloor_roster_coverage`'s `DECLARED_ABSENT` — and it needs its own
/// table rather than `domains/climate`'s inline `if
/// registry.concept(name).is_some() { continue; }` (its `ice` substance/
/// biome collision) because that check is a same-crate, same-function,
/// fixed-order exception with the reasoning written into the one call site;
/// this one crosses crates, so nothing else pins that `thing` runs after
/// `settlement` except `windows/worldgen`'s roster-order test
/// (`domains_roster_registers_thing_after_its_borrowed_owners`), which reads
/// this table rather than duplicating it.
/// type-audit: bare-ok(identifier-text)
pub const BORROWED: &[(&str, &str)] = &[("hearth", "settlement")];

/// [`BORROWED`]'s owner for `label`, if any.
fn borrowed_owner(label: &str) -> Option<&'static str> {
    BORROWED
        .iter()
        .find(|(borrowed, _)| *borrowed == label)
        .map(|(_, owner)| *owner)
}

/// Register thing's contribution to the concept registry: every label in
/// [`THING_KINDS`] becomes a nameable concept, except a label in
/// [`BORROWED`], which maps to its declared owner's existing concept
/// instead (decision 0025, "one concept name, one owner").
///
/// No language pack names any thing-kind yet, so the lexeme edge is an
/// honest `Gap` rather than an over-optimistic `Expected` (the same choice
/// `settlement`'s home/hearth pair makes, and for the same reason).
///
/// Three-valued per label, matching what is registered against what
/// [`BORROWED`] declares — never a bare "does it already exist" skip (see
/// [`BORROWED`]'s doc for why that shape is unsafe):
/// - collides, and `BORROWED` names this owner: map to it, register nothing.
/// - collides, and `BORROWED` says nothing (or names a different owner):
///   panic — an unrecognized domain has claimed a `thing` label, or the
///   declared owner is wrong, and either needs a human decision, not a
///   silent transfer.
/// - `BORROWED` names an owner but nothing collides: panic — the
///   declaration is stale (its target concept is gone or renamed) and must
///   be deleted, the same STALE-DECL shape seam-guard's `expect(survives:
///   …)` uses.
/// - neither: `thing` owns the label outright and registers it.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    let traits = thing_registry();
    for label in THING_KINDS {
        match registry.concept(label) {
            // Already registered, and thing is the owner -- either an
            // earlier label in this same pass registered it, or this is a
            // second call to `register_concepts` (which must be idempotent,
            // the same convention every domain's `register_concepts`
            // relies on). Nothing further to do.
            Some(existing) if existing.domain == "thing" => {}
            // Already registered under a DIFFERENT domain.
            Some(existing) => match borrowed_owner(label) {
                Some(owner) if existing.domain == owner => {
                    // Declared and colliding with exactly the owner
                    // BORROWED names: cede the word, register nothing.
                }
                Some(owner) => panic!(
                    "BORROWED declares thing cedes {label:?} to {owner:?}, but the \
                     registry attributes it to {:?} instead -- the declaration is \
                     stale and must be corrected",
                    existing.domain
                ),
                None => panic!(
                    "thing-kind {label:?} collides with a concept already owned by \
                     {:?}, and BORROWED does not declare this cession -- add an \
                     entry to BORROWED (decision 0025) if that ownership is \
                     intended, or rename the collision away",
                    existing.domain
                ),
            },
            // Not registered at all.
            None => {
                if let Some(owner) = borrowed_owner(label) {
                    panic!(
                        "BORROWED declares {label:?} ceded to {owner:?}, but no such \
                         concept is registered -- delete the stale declaration"
                    );
                }
                let doc = traits
                    .get(&KindId(label))
                    .and_then(|t| t.doc)
                    .unwrap_or_else(|| {
                        unreachable!(
                            "{label:?} reached thing's own registration with no gloss -- \
                             it is either missing from THING_KINDS/thing_registry, or it \
                             is BORROWED and should never reach this branch"
                        )
                    });
                registry.register_manifest(Manifest {
                    concept: ConceptDef {
                        name: label.to_string(),
                        domain: "thing".to_string(),
                        kind: ConceptKind::Object,
                        doc: doc.to_string(),
                    },
                    lexeme: Correspondent::Absent(Void::Gap("no language pack names it yet")),
                    percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
                    cognition: Correspondent::Absent(Void::Uncognized {
                        pending_wave: "wave-cognition",
                    }),
                })?;
            }
        }
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
    /// thread 'tests::thing_kind_labels_are_unique' panicked at domains/thing/src/lib.rs:372:13:
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

    /// Every roster kind carries a non-empty gloss, and the gloss reaches the
    /// concept registry. This replaces `concept_doc`'s exhaustive match: the
    /// match could go short by one arm and panic at world genesis, where a
    /// missing struct field will not compile.
    ///
    /// **Direction: rostered ⊆ glossed.** Its converse — that no gloss exists
    /// for a kind outside the roster — is structural now rather than asserted,
    /// because a gloss can only exist as a field of a row.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: give `hearth` a gloss (it is BORROWED,
    /// so the fourth arm must catch it), and separately set `bed`'s to `Some("")`.
    #[test]
    fn every_roster_kind_carries_a_gloss() {
        let reg = thing_registry();
        for label in THING_KINDS {
            let traits = reg
                .get(&KindId(label))
                .unwrap_or_else(|| panic!("roster names {label:?}, registry does not"));
            match (traits.doc, borrowed_owner(label)) {
                (Some(doc), None) => assert!(
                    !doc.is_empty(),
                    "{label:?} has an empty gloss — a registered concept with no \
                     doc renders as a blank line in the reference page"
                ),
                (None, Some(_)) => {}
                (Some(_), Some(owner)) => panic!(
                    "{label:?} is BORROWED by {owner:?} and also carries a gloss \
                     here — that is a second definition of their concept"
                ),
                (None, None) => panic!(
                    "{label:?} is not borrowed and has no gloss — nothing will \
                     describe it in the concept registry"
                ),
            }
        }
    }

    /// Stands in for `settlement`'s own registration of `hearth`
    /// (`domains/settlement/src/lib.rs`'s home/hearth pair) — `thing` cannot
    /// depend on `hornvale-settlement` (domains depend on the kernel only),
    /// so every test exercising the BORROWED/hearth interaction hand-rolls
    /// this rather than importing it.
    fn settlement_hearth_manifest() -> Manifest {
        Manifest {
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
        }
    }

    /// Every roster kind is a registered concept after `register_concepts`,
    /// and every kind `thing` actually owns (everything but the BORROWED
    /// `hearth`) carries this domain's own name — so the registry dump
    /// attributes them here and not to whichever crate happened to call the
    /// function. `hearth` is pre-registered the way real genesis always
    /// orders it (`settlement` before `thing`,
    /// `windows/worldgen`'s `domains_roster_registers_thing_after_its_
    /// borrowed_owners` pins that order), since `register_concepts` now
    /// panics on a BORROWED label nothing has registered yet (see
    /// `stale_declaration_panics_if_nothing_collides`).
    ///
    /// MUTATION THIS MUST FAIL AGAINST: replace `register_concepts`'s body
    /// with `Ok(())` (registration becomes vacuous — nothing is registered,
    /// but the function still reports success). Red observed:
    ///
    /// ```text
    /// thread 'tests::every_roster_kind_registers_as_a_concept' panicked at domains/thing/src/lib.rs:397:36:
    /// "alcove" not registered
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 1 filtered out
    /// ```
    #[test]
    fn every_roster_kind_registers_as_a_concept() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_manifest(settlement_hearth_manifest())
            .expect("settlement's own registration");
        register_concepts(&mut reg).expect("registration is total and idempotent");
        for label in THING_KINDS {
            let c = reg
                .concept(label)
                .unwrap_or_else(|| panic!("{label:?} not registered"));
            let expected_domain = if *label == "hearth" {
                "settlement"
            } else {
                "thing"
            };
            assert_eq!(
                c.domain, expected_domain,
                "{label:?} attributed to the wrong domain"
            );
        }
    }

    /// Re-registering must succeed and change nothing — every domain's
    /// `register_concepts` relies on `ConceptRegistry` accepting a
    /// byte-identical redefinition (`domains/person/src/lib.rs:199-205` is
    /// the precedent this follows). `hearth` is pre-registered for the same
    /// reason `every_roster_kind_registers_as_a_concept` pre-registers it.
    #[test]
    fn registering_twice_is_idempotent() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_manifest(settlement_hearth_manifest())
            .expect("settlement's own registration");
        register_concepts(&mut reg).expect("first registration");
        register_concepts(&mut reg).expect("second registration is idempotent");
    }

    /// Decision 0025 (one concept name, one owner): `hearth` is already a
    /// concept when `settlement` registers it first, and `thing` must map to
    /// that existing definition — registering nothing more for it — rather
    /// than mint a conflicting redefinition under its own domain. Also
    /// asserts a non-colliding label (`key`) is genuinely registered under
    /// `thing` in the same pass, so this demonstrates mapping-plus-
    /// registration, not merely the absence of a conflict.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: revert `register_concepts` to
    /// unconditional registration (delete the whole `match
    /// registry.concept(label) { ... }` and go back to registering every
    /// `THING_KINDS` label under domain `thing` regardless of what is
    /// already there). Red observed (via `register_concepts`'s own doc-lookup
    /// `unreachable!`, since `hearth`'s row carries no gloss -- `hearth`
    /// never reaches it under the real guard, so restoring unconditional
    /// registration trips that safety net before it can even reach the
    /// `ConflictingDefinition` `register_manifest` would otherwise return).
    ///
    /// **Correction (Fix Round 1): this red used to quote `concept_doc`'s own
    /// `unreachable!` message verbatim. Task 1 deleted that function and
    /// moved its safety net into `register_concepts`'s doc-lookup -- the
    /// quoted text below is what the mutation actually produces now,
    /// re-run at HEAD:**
    ///
    /// ```text
    /// thread 'tests::hearth_maps_to_an_existing_owner_instead_of_conflicting' panicked at domains/thing/src/lib.rs:365:17:
    /// internal error: entered unreachable code: "hearth" reached thing's own registration with no gloss -- it is either missing from THING_KINDS/thing_registry, or it is BORROWED and should never reach this branch
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 10 filtered out
    /// ```
    #[test]
    fn hearth_maps_to_an_existing_owner_instead_of_conflicting() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        reg.register_manifest(settlement_hearth_manifest())
            .expect("settlement's own registration");

        register_concepts(&mut reg)
            .expect("thing must map to the existing `hearth` concept, not conflict with it");
        assert_eq!(
            reg.concept("hearth").unwrap().domain,
            "settlement",
            "thing must not steal ownership of a word another domain already registered"
        );
        assert_eq!(
            reg.concept("key").unwrap().domain,
            "thing",
            "a non-colliding label must still be registered under thing in the same pass"
        );
    }

    /// The undeclared-collision direction: a domain `BORROWED` does not
    /// mention has already claimed a `THING_KINDS` label. This must panic
    /// rather than silently cede (or silently steal back) the word — a bare
    /// `if registry.concept(label).is_some() { continue; }` would cede
    /// silently here, forever, which is exactly the shape this guard
    /// replaces (see `BORROWED`'s doc).
    ///
    /// MUTATION THIS MUST FAIL AGAINST: revert `register_concepts` to the
    /// bare `if registry.concept(label).is_some() { continue; }` skip
    /// (Fix Round 1's original shape). Under that mutation this test's
    /// `#[should_panic]` fails because nothing panics -- `register_concepts`
    /// returns `Ok(())` having silently ceded `pool` to `climate` instead.
    /// Red observed:
    ///
    /// ```text
    /// note: test did not panic as expected at domains/thing/src/lib.rs:501:8
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 1 filtered out
    /// ```
    #[test]
    #[should_panic(expected = "and BORROWED does not declare this cession")]
    fn undeclared_collision_panics_instead_of_silently_ceding() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        // `hearth` is BEFORE `pool` in THING_KINDS, so it must be
        // pre-registered here too -- otherwise the loop panics on
        // `hearth`'s stale-declaration check first and never reaches the
        // undeclared collision this test means to exercise.
        reg.register_manifest(settlement_hearth_manifest())
            .expect("settlement's own registration");
        reg.register_manifest(Manifest {
            concept: ConceptDef {
                name: "pool".to_string(),
                domain: "climate".to_string(),
                kind: ConceptKind::Terrain,
                doc: "a standing body of water".to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap("no language pack names it yet")),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })
        .expect("climate's stand-in registration");

        let _ = register_concepts(&mut reg);
    }

    /// The declared-owner-mismatch direction: `BORROWED` names an owner for
    /// `hearth` (`settlement`), but the registry attributes it to a
    /// DIFFERENT domain entirely. This is not the undeclared-collision case
    /// (`BORROWED` does have an opinion about `hearth`) and not the
    /// nothing-collides case (something is registered) — it is its own
    /// failure, the real scenario being: `settlement` drops `hearth`,
    /// `domains/climate` later registers it for hydrology, and `BORROWED`
    /// never got updated. That deserves its own diagnostic ("the
    /// declaration is stale and must be corrected") rather than being
    /// folded into the undeclared-collision message ("BORROWED does not
    /// declare this cession") — the two name different repairs (correct
    /// `BORROWED`'s owner vs. add a new entry to it), and collapsing them
    /// would leave a maintainer fixing the wrong thing.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: collapse `register_concepts`'s two
    /// `Some(existing) => match borrowed_owner(label) { ... }` panic arms
    /// (`Some(owner) => panic!(...stale...)` and `None =>
    /// panic!(...does not declare this cession...)`) into one `_ =>
    /// panic!(<undeclared-collision message>)`. Every other
    /// `hornvale-thing` test still passes under this mutation (the arm is
    /// unreachable from anywhere else in the suite), which is exactly the
    /// coverage hole this test closes. Red observed:
    ///
    /// ```text
    /// thread 'tests::declared_owner_mismatch_panics_with_its_own_message' panicked at domains/thing/src/lib.rs:295:22:
    /// thing-kind "hearth" collides with a concept already owned by "climate", and BORROWED does not declare this cession -- add an entry to BORROWED (decision 0025) if that ownership is intended, or rename the collision away
    /// note: panic did not contain expected string
    ///       panic message: "thing-kind \"hearth\" collides with a concept already owned by \"climate\", and BORROWED does not declare this cession -- add an entry to BORROWED (decision 0025) if that ownership is intended, or rename the collision away"
    ///  expected substring: "the declaration is stale and must be corrected"
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 1 filtered out
    /// ```
    #[test]
    #[should_panic(expected = "the declaration is stale and must be corrected")]
    fn declared_owner_mismatch_panics_with_its_own_message() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        // `hearth` IS declared in BORROWED (owner "settlement"), but this
        // registers it under "climate" instead -- the stale-declaration
        // scenario, not the undeclared-collision one.
        reg.register_manifest(Manifest {
            concept: ConceptDef {
                name: "hearth".to_string(),
                domain: "climate".to_string(),
                kind: ConceptKind::Substance,
                doc: "warmth radiating from a fire".to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap("no language pack names it yet")),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })
        .expect("climate's stand-in registration");

        let _ = register_concepts(&mut reg);
    }

    /// The stale-declaration direction: `BORROWED` names an owner for
    /// `hearth`, but nothing has registered it — the declaration's target
    /// is gone (or this call ran before its owner, which is exactly the
    /// ordering `windows/worldgen`'s
    /// `domains_roster_registers_thing_after_its_borrowed_owners` pins
    /// against). A silent skip would let `thing` invent its own `hearth`
    /// here instead of catching the broken assumption.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: revert `register_concepts` to the
    /// bare `if registry.concept(label).is_some() { continue; }` skip
    /// (an earlier fix round's original shape). Under that mutation this
    /// test's `#[should_panic]` fails: `hearth` is absent from a fresh
    /// registry, so the bare skip's condition is false and the code falls
    /// through to the doc-lookup for `hearth`'s row -- which panics too, but
    /// on the wrong thing (`register_concepts`'s own `unreachable!`, since
    /// `hearth`'s row carries no gloss), so `#[should_panic(expected =
    /// "delete the stale declaration")]` still fails to match.
    ///
    /// **Correction (Fix Round 1): this red used to quote `concept_doc`'s own
    /// `unreachable!` message verbatim. Task 1 deleted that function and
    /// moved its safety net into `register_concepts`'s doc-lookup -- the
    /// quoted text below is what the mutation actually produces now,
    /// re-run at HEAD:**
    ///
    /// ```text
    /// thread 'tests::stale_declaration_panics_if_nothing_collides' panicked at domains/thing/src/lib.rs:368:17:
    /// internal error: entered unreachable code: "hearth" reached thing's own registration with no gloss -- it is either missing from THING_KINDS/thing_registry, or it is BORROWED and should never reach this branch
    /// note: panic did not contain expected string
    ///       panic message: "internal error: entered unreachable code: \"hearth\" reached thing's own registration with no gloss -- it is either missing from THING_KINDS/thing_registry, or it is BORROWED and should never reach this branch"
    ///  expected substring: "delete the stale declaration"
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 10 filtered out
    /// ```
    #[test]
    #[should_panic(expected = "delete the stale declaration")]
    fn stale_declaration_panics_if_nothing_collides() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        let _ = register_concepts(&mut reg);
    }

    /// Every named handle resolves to a roster row (G-d, spec §5.1). The
    /// direction this enforces is **named ⊆ rostered**: it cannot see a
    /// rostered kind that has no handle, and deliberately so — a kind no code
    /// names needs no handle, which is the whole difference between a handle
    /// and a variant.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: change `kinds::HIGH_SEAT` to
    /// `KindId("high_seat")` (underscore for hyphen — the spelling a reader
    /// guesses). It compiles, and every consumer keeps compiling, which is
    /// exactly the failure a bare literal invites. Red observed:
    ///
    /// ```text
    /// thread 'tests::every_named_handle_is_a_roster_row' panicked at domains/thing/src/lib.rs:680:13:
    /// handle HIGH_SEAT is "high_seat", which the roster does not carry
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 9 filtered out
    /// ```
    #[test]
    fn every_named_handle_is_a_roster_row() {
        for (name, id) in kinds::EVERY_HANDLE {
            assert!(
                THING_KINDS.contains(&id.0),
                "handle {name} is {:?}, which the roster does not carry",
                id.0
            );
        }
    }

    /// The roster is frozen as an ORDERED SET, not a count (G-f, spec §5.1).
    ///
    /// A length assertion passes any compensating swap — drop one kind, add
    /// another, and a count-based ratchet reports nothing. Freezing the
    /// sequence makes every addition, removal and reordering a visible edit to
    /// this list. Update this list in the same commit that changes the roster,
    /// never afterwards.
    ///
    /// **WHY A ROSTER NEEDS A RATCHET AT ALL, measured rather than argued.**
    /// This paragraph is the substance of a rationale that lived on
    /// `windows/vessel`'s `anchor_kinds!` macro until The Wicket's Task 2
    /// deleted it with the enum, and it is the reason this test exists rather
    /// than a length check.
    ///
    /// Three separate hand-written `[AnchorKind; 14]` rosters had accumulated
    /// across the tree — in `windows/vessel/tests/suite/affordance.rs`,
    /// `cli/tests/suite/anchor_thing_correspondence.rs` and
    /// `windows/vessel/src/chamber_prose.rs` — and each carried a comment
    /// saying it was "kept in step by an exhaustive match". **That claim was
    /// false in the direction that actually happens.** The compiler forces an
    /// ARM per variant; it says nothing about a *list* sitting beside them. A
    /// reviewer added a fifteenth variant, wrote the arms the compiler
    /// demanded, pointed it at `KindId("cave-mouth")`, and 1209 tests passed
    /// — including a precondition whose own doc comment promised to stop
    /// being evidence in exactly that case. Dropping a variant reddened a
    /// test; adding one reddened nothing.
    ///
    /// No test can enumerate a member it has never heard of, so no test can
    /// close that gap from the inside. The macro closed it by GENERATING the
    /// roster from the same declaration that produced the enum. That route is
    /// gone — `THING_KINDS` is authored data, not a derivation — so the
    /// closure here is the opposite one: the roster is frozen against a
    /// committed copy, and every addition, removal and swap has to move two
    /// lists in one commit. `every_named_handle_is_a_roster_row` above is the
    /// other half, checking the handles against this same roster.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: swap the `"log"` and `"loom"` entries
    /// in `THING_KINDS`. The length is unchanged and the set is unchanged;
    /// only the order moves, and a count-based check would stay green. Red
    /// observed:
    ///
    /// ```text
    /// thread 'tests::the_roster_is_frozen_as_an_ordered_set' panicked at domains/thing/src/lib.rs:729:9:
    /// assertion `left == right` failed: the thing-kind roster moved; update FROZEN in the same commit
    ///   left: ["alcove", "altar", "anvil", "bed", "cave-mouth", "ground", "hearth", "high-seat", "key", "loom", "log", "pool", "screen", "strongbox", "threshold", "vessel"]
    ///  right: ["alcove", "altar", "anvil", "bed", "cave-mouth", "ground", "hearth", "high-seat", "key", "log", "loom", "pool", "screen", "strongbox", "threshold", "vessel"]
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 9 filtered out
    /// ```
    #[test]
    fn the_roster_is_frozen_as_an_ordered_set() {
        const FROZEN: &[&str] = &[
            "alcove",
            "altar",
            "anvil",
            "bed",
            "brazier",
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
        assert_eq!(
            THING_KINDS, FROZEN,
            "the thing-kind roster moved; update FROZEN in the same commit"
        );
    }
}

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
//! verbs need (`cave-mouth`, `strongbox`, `key`): a later task needs a total
//! mapping from every `AnchorKind` to a thing-kind, so every anchor kind gets
//! a row here too, carried at no cost to a world that never places one.

#![warn(missing_docs)]

use hornvale_kernel::{
    ComponentStore, ConceptDef, ConceptKind, ConceptRegistry, Correspondent, KindId, Manifest,
    RegistryError, Void,
};

/// Object-kind traits: the label prose uses to name the kind. Thin and
/// honest (spec §3.5) — no property lives here; see the module doc for why
/// `portable` was deleted rather than kept beside `ObjectProperty::Portable`.
/// type-audit: bare-ok(identifier-text: display)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ThingTraits {
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

/// The canonical thing-kind registry: one row per [`THING_KINDS`] label,
/// carrying the display name and nothing else. Which kinds are portable,
/// openable or lockable is `windows/vessel`'s `object_registry`
/// (`ObjectProperty::{Portable, Openable, Lockable}`, spec §3.6/§3.8) — one
/// table, not two.
pub fn thing_registry() -> ComponentStore<KindId, ThingTraits> {
    [
        (KindId("alcove"), ThingTraits { display: "alcove" }),
        (KindId("altar"), ThingTraits { display: "altar" }),
        (KindId("anvil"), ThingTraits { display: "anvil" }),
        (KindId("bed"), ThingTraits { display: "bed" }),
        (
            KindId("cave-mouth"),
            ThingTraits {
                display: "cave mouth",
            },
        ),
        (KindId("ground"), ThingTraits { display: "ground" }),
        (KindId("hearth"), ThingTraits { display: "hearth" }),
        (
            KindId("high-seat"),
            ThingTraits {
                display: "high seat",
            },
        ),
        (KindId("key"), ThingTraits { display: "key" }),
        (KindId("log"), ThingTraits { display: "log" }),
        (KindId("loom"), ThingTraits { display: "loom" }),
        (KindId("pool"), ThingTraits { display: "pool" }),
        (KindId("screen"), ThingTraits { display: "screen" }),
        (
            KindId("strongbox"),
            ThingTraits {
                display: "strongbox",
            },
        ),
        (
            KindId("threshold"),
            ThingTraits {
                display: "threshold",
            },
        ),
        (KindId("vessel"), ThingTraits { display: "vessel" }),
    ]
    .into_iter()
    .collect()
}

/// A short, honest gloss for each roster label thing itself owns — what a
/// player would call the kind, not a mechanic. Kept beside [`THING_KINDS`]
/// rather than folded into [`ThingTraits::display`], which names the kind in
/// running prose, not what it *is*. Carries no entry for a label in
/// [`BORROWED`] (`hearth`): that concept is never registered under `thing`,
/// so a gloss for it here would be dead code asserting thing's reading of a
/// word it does not own — see [`register_concepts`]'s doc for that reading.
fn concept_doc(label: &str) -> &'static str {
    match label {
        "alcove" => "a recessed space set into a wall",
        "altar" => "a raised surface where offerings are made",
        "anvil" => "a heavy iron block a smith hammers metal against",
        "bed" => "a place made for lying down and sleeping",
        "cave-mouth" => "the opening where a cave meets the outside",
        "ground" => "the bare earth underfoot",
        "high-seat" => "the seat of a hall's presiding figure",
        "key" => "a small tool shaped to work one particular lock",
        "log" => "a length of felled, unworked timber",
        "loom" => "a frame for weaving thread into cloth",
        "pool" => "a small standing body of water",
        "screen" => "a partition set up to divide or shield a space",
        "strongbox" => "a locked chest built to keep valuables safe",
        "threshold" => "the sill marking where one place ends and another begins",
        "vessel" => "a container shaped to hold liquid or goods",
        other => unreachable!(
            "concept_doc has no gloss for thing-kind {other:?} — it is either \
             missing from THING_KINDS/concept_doc, or it is BORROWED and should \
             never reach this function"
        ),
    }
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
    /// already there). Red observed (via `concept_doc`'s own
    /// `unreachable!`, since it has no gloss for `hearth` any more --
    /// `hearth` never reaches it under the real guard, so restoring
    /// unconditional registration trips that safety net before it can even
    /// reach the `ConflictingDefinition` `register_manifest` would
    /// otherwise return):
    ///
    /// ```text
    /// thread 'tests::hearth_maps_to_an_existing_owner_instead_of_conflicting' panicked at domains/thing/src/lib.rs:218:18:
    /// internal error: entered unreachable code: concept_doc has no gloss for thing-kind "hearth" -- it is either missing from THING_KINDS/concept_doc, or it is BORROWED and should never reach this function
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 1 filtered out
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
    /// (Fix Round 1's original shape). Under that mutation this test's
    /// `#[should_panic]` fails: `hearth` is absent from a fresh registry, so
    /// the bare skip's condition is false and the old code falls through to
    /// `concept_doc("hearth")` -- which panics too, but on the wrong thing
    /// (its own `unreachable!`, since `concept_doc` no longer glosses a
    /// BORROWED label), so `#[should_panic(expected = "delete the stale
    /// declaration")]` still fails to match. Red observed:
    ///
    /// ```text
    /// thread 'tests::stale_declaration_panics_if_nothing_collides' panicked at domains/thing/src/lib.rs:218:18:
    /// internal error: entered unreachable code: concept_doc has no gloss for thing-kind "hearth" -- it is either missing from THING_KINDS/concept_doc, or it is BORROWED and should never reach this function
    /// note: panic did not contain expected string
    /// test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 1 filtered out
    /// ```
    #[test]
    #[should_panic(expected = "delete the stale declaration")]
    fn stale_declaration_panics_if_nothing_collides() {
        let mut reg = hornvale_kernel::ConceptRegistry::default();
        let _ = register_concepts(&mut reg);
    }
}

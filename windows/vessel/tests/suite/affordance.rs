//! The offer query (The Offer, Task 2, spec §3.2/§6): the acceptance test
//! that proves authoring cost is M verbs + N objects, never M×N. Both
//! directions are asserted, deliberately — spec §6 is explicit that either
//! direction alone is satisfiable by a hardcoded verb×object table, so a
//! reviewer must see both held at once before believing the M+N claim.
//!
//! **Revised after review (fix round 1).** Two Criticals, both verified by
//! mutation rather than by reading:
//!
//! - `extra_properties_do_not_withdraw_an_offer` (this file's earlier
//!   version) asserted a static fact about `required_properties`'s output
//!   against a hand-built `ObjectTraits`; it never called `offered_by` at
//!   all, so flipping `offered_by`'s `is_subset` to `==` in
//!   `affordance.rs` left it green. Fixed by adding
//!   [`hornvale_vessel::affordance::offered`] — the real subset query,
//!   factored out of `offered_by` so a test can hand it a constructed
//!   `ObjectTraits` the registry itself could not produce at the time
//!   (every registered kind then carried exactly one property, which cannot
//!   distinguish subset from equality) — and rewriting the test to call it.
//!   **The parenthesis is history, not present tense, since The Chattel's
//!   Task 7**: `strongbox` now carries three properties, so the registry can
//!   discriminate the two on its own — see
//!   `a_registered_multi_property_kind_discriminates_subset_from_equality`,
//!   and `extra_properties_expand_the_offer_never_withdraw_it`'s doc for why
//!   the constructed traits stay anyway.
//! - `no_verb_by_object_table_exists`'s string scan looked for the literal
//!   substring `"=> OfferedVerb::"`, which a real hardcoded table would
//!   never contain: `offered_by` returns a `BTreeSet<OfferedVerb>`, so any
//!   per-kind arm has to build a set
//!   (`[OfferedVerb::Sleep, ...].into_iter().collect()`) or a block, never
//!   an arm whose own output type IS `OfferedVerb`. Replaced with a
//!   bracket-depth-aware scanner (below) that reads the whole arm body
//!   between a `AnchorKind::X =>` and the arm's end, catching both shapes.
//!   Its own positive/negative controls are below, so the scanner's claim
//!   to catch the forbidden shape is evidence, not assertion.

use std::collections::BTreeSet;

use hornvale_kernel::{ConditionResponse, EntityId, Facet, KindId, ResourceVector};
use hornvale_vessel::Knowledge;
use hornvale_vessel::affordance::{
    ObjectProperty, ObjectTraits, OfferedVerb, object_registry, offered, offered_by, offered_to,
    offered_to_observer, thing_kind_of,
};
use hornvale_vessel::body::Body;
use hornvale_vessel::clock::{REFERENCE_MASS_KG, mass_for_species};
use hornvale_vessel::interior::AnchorKind;
use hornvale_vessel::liveness::ThreatNiche;

/// Acceptance test (1): a new OBJECT kind ships with properties only — no
/// dispatcher change — and the right verbs appear on it.
///
/// **What it still catches after Task 7's re-key** (the write-up spec §7's
/// Task 3 demands, kept beside the test rather than in a report that dies
/// with the campaign): the query is now keyed on thing-kind, so this reads
/// `offered_by(thing_kind_of(AnchorKind::Pool))` and covers one more link
/// than it used to — a `pool` row that loses `HoldsLiquid`, a
/// `required_properties(Drink)` that stops requiring it, a subset filter
/// broken to return everything or nothing, AND a `thing_kind_of` arm that
/// carries `Pool` to a kind with no liquid. It does NOT catch a mapping that
/// sends `Pool` to `vessel`, because `vessel` also holds liquid; that is
/// `the_re_key_preserves_every_anchor_kinds_offer`'s job, and the reason
/// this file needs that test at all.
#[test]
fn a_kind_gains_every_verb_its_properties_satisfy_with_no_dispatcher_edit() {
    // Pool carries HoldsLiquid; nothing anywhere names "pool" and "drink"
    // together. The verb arrives because the property matches.
    assert!(offered_by(thing_kind_of(AnchorKind::Pool)).contains(&OfferedVerb::Drink));
    assert!(!offered_by(thing_kind_of(AnchorKind::Bed)).contains(&OfferedVerb::Drink));
}

/// Acceptance test (2), the multi-carrier half: `HoldsLiquid` is the one
/// property [`object_registry`] assigns to more than one kind (`Pool` and
/// `Vessel`), so this is the sharpest available proof — from the *registry*
/// alone — that the query generalizes rather than being hardcoded per kind.
///
/// **What this test does NOT show, corrected from an earlier, wrong doc
/// comment**: `Drink` is not "declared only in the test" — it is a
/// production `OfferedVerb` variant, shipped in the same commit as this
/// test, exactly like `Sleep`/`Enter`/`Examine`. The genuine "a new verb
/// ships declaring required properties only, no object change" witness is
/// `Warm` (spec §6 names it explicitly), asserted separately below in
/// `warm_appears_on_hearth_with_no_object_table_edit`. This test's own job
/// is narrower and still real: proving `offered_by` does not special-case
/// any one carrier of a shared property.
///
/// **What it still catches after Task 7's re-key**: everything it caught
/// before, over the same two carriers (`pool`/`vessel`), now discovered as
/// [`KindId`]s from the re-keyed registry rather than as `AnchorKind`s. The
/// discovery is still from the registry, never a hand-written pair, so a
/// carrier added or removed changes what this sweeps. The `>= 2` guard is
/// what keeps it from going vacuous if the shared property ever loses a
/// carrier.
#[test]
fn every_kind_carrying_a_shared_property_is_offered_the_verb_it_gates() {
    let want = BTreeSet::from([ObjectProperty::HoldsLiquid]);
    let reg = object_registry();
    let carriers: BTreeSet<KindId> = reg
        .iter()
        .filter(|(_, t)| want.is_subset(&t.properties))
        .map(|(k, _)| *k)
        .collect();

    assert!(
        carriers.len() >= 2,
        "a one-carrier property proves nothing about M+N"
    );
    for kind in carriers {
        assert!(
            offered_by(kind).contains(&OfferedVerb::Drink),
            "{kind:?} carries HoldsLiquid but is not offered drink"
        );
    }
}

/// The offer is a SUBSET relation, not an equality: an object carrying more
/// properties than a verb requires still affords that verb.
///
/// **Fix for C1.** The earlier version of this test asserted
/// `required_properties(OfferedVerb::Drink).is_subset(&traits.properties)`
/// directly — a true fact about `required_properties`'s *output*, checked
/// against a hand-built `BTreeSet`, that never called `offered_by` (or
/// anything else in `affordance.rs`) at all. A reviewer flipped
/// `offered_by`'s `is_subset` to `==` and this test stayed green, because
/// it was never exercising that code path. It now calls
/// [`offered`] — the real query `offered_by` wraps — against traits
/// carrying `HoldsLiquid` *and* `Encloses`. Under subset logic `Drink` is
/// still offered (`{HoldsLiquid} ⊆ {HoldsLiquid, Encloses}`); under
/// equality it would not be (`{HoldsLiquid} ≠ {HoldsLiquid, Encloses}`), so
/// this is now a datum the subset-vs-equality distinction actually moves.
///
/// **The sentence that used to close this doc is now FALSE, and is corrected
/// rather than deleted (Task 7).** It read: *"No registered kind could do
/// this job: every one of `object_registry`'s six carriers holds exactly one
/// property today, so subset and equality agree on all of them."* That was
/// true of The Offer's table and stopped being true the moment Task 7 gave
/// `strongbox` three properties (`Encloses`+`Openable`+`Lockable`) — a
/// registered kind can now discriminate subset from equality, and
/// `a_registered_multi_property_kind_discriminates_subset_from_equality`
/// below asserts exactly that against the live registry. This test keeps its
/// constructed traits anyway: it must stay falsifiable if some later
/// campaign flattens the registry back to one property per kind, and a test
/// whose discriminating power depends on an authored table is one authoring
/// decision away from proving nothing.
#[test]
fn extra_properties_expand_the_offer_never_withdraw_it() {
    let mut traits = ObjectTraits::default();
    traits.properties.insert(ObjectProperty::HoldsLiquid);
    traits.properties.insert(ObjectProperty::Encloses);
    assert!(
        offered(&traits).contains(&OfferedVerb::Drink),
        "a HoldsLiquid+Encloses object must still be offered Drink under a \
         subset relation; this fails if the implementation checks equality \
         instead"
    );
}

/// A positive-control companion to the test above: run the SAME mutation
/// (subset -> equality) by hand, in the test itself rather than in
/// `affordance.rs`, and confirm it disagrees with `offered`. This is not
/// redundant with `extra_properties_expand_the_offer_never_withdraw_it` — it
/// exists so a reader can see, in one place, exactly what "equality would
/// fail this" means without having to go mutate the source file themselves.
#[test]
fn subset_and_equality_genuinely_disagree_on_the_constructed_traits() {
    let mut traits = ObjectTraits::default();
    traits.properties.insert(ObjectProperty::HoldsLiquid);
    traits.properties.insert(ObjectProperty::Encloses);
    let required = BTreeSet::from([ObjectProperty::HoldsLiquid]);
    assert!(required.is_subset(&traits.properties), "subset: true");
    assert_ne!(
        required, traits.properties,
        "equality: false — they disagree"
    );
}

/// Acceptance test (2)'s real witness: `Warm` is the one verb spec §3.3
/// names as genuinely NEW (the other four retrofit verbs that already
/// ship), and it is spec §6's own named witness — "it is the one new verb,
/// it declares `radiates-heat`, and it must appear on the hearth without
/// the hearth being edited." `object_registry` is Task 1's committed
/// output; this test does not touch it, and `Warm`'s presence on `Hearth`
/// still falls out of the subset query.
#[test]
fn warm_appears_on_hearth_with_no_object_table_edit() {
    assert!(offered_by(thing_kind_of(AnchorKind::Hearth)).contains(&OfferedVerb::Warm));
    assert!(!offered_by(thing_kind_of(AnchorKind::Bed)).contains(&OfferedVerb::Warm));
}

/// `Examine` requires the empty property set (spec §3.3: universal), and the
/// empty set is a subset of every set — including the empty set itself. So
/// universality must hold even for an `AnchorKind` `object_registry` never
/// mentions at all (e.g. `Screen`, which Task 1 verified carries no property):
/// there is no per-kind registry entry to fall back on, so this is the
/// sharpest test that `offered_by` derives universality from the subset
/// relation rather than from iterating only over registered kinds.
#[test]
fn examine_is_universal_even_for_a_kind_with_no_registered_properties() {
    assert!(!object_registry().contains(&thing_kind_of(AnchorKind::Screen)));
    let offered = offered_by(thing_kind_of(AnchorKind::Screen));
    assert!(offered.contains(&OfferedVerb::Examine));
    assert_eq!(
        offered.len(),
        1,
        "a property-less kind should offer Examine and nothing else"
    );
}

/// A byte-level substring search. Used instead of `str::find` below because
/// the depth-tracking scan slices `src` at arbitrary byte offsets that are
/// not guaranteed to land on UTF-8 char boundaries (the file's doc comments
/// use non-ASCII characters like `⊆`/`×`/`—`/`§`); operating on `&[u8]`
/// throughout sidesteps that instead of trying to prove it can't happen.
fn find_bytes(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if needle.is_empty() || haystack.len() < needle.len() {
        return None;
    }
    (0..=haystack.len() - needle.len()).find(|&i| &haystack[i..i + needle.len()] == needle)
}

/// Whether `src` contains a match arm keyed on a specific `AnchorKind`
/// variant whose body mentions an `OfferedVerb` variant anywhere — the
/// verb x object table shape acceptance test (4) forbids.
///
/// **This replaces a scanner a review found could not fire.** The original
/// looked for the literal substring `"=> OfferedVerb::"`, but `offered_by`
/// returns `BTreeSet<OfferedVerb>`, so no real per-kind arm's output type is
/// ever `OfferedVerb` itself — a hardcoded table has to build a *set*:
/// `AnchorKind::Bed => [OfferedVerb::Sleep, OfferedVerb::Examine]
/// .into_iter().collect()`, or a block. A reviewer checked both spellings
/// against the old scanner and got 0 matches on each — the guard's blind
/// spot was the only shape anyone would ever write.
///
/// This version tracks bracket depth from the `=>` to the arm's end (a
/// top-level comma, or the closing brace of the enclosing `match`) and
/// checks the WHOLE arm body for `OfferedVerb::`, so it catches both the
/// set-builder and block forms. See the three control tests directly below
/// this function for evidence it actually does.
///
/// **The direction this still enforces, unchanged from before** (stated so
/// it cannot be mistaken for a broader guarantee): it scans exactly the one
/// file passed to it, for exactly the one syntactic shape "a match arm
/// pattern containing the literal text `AnchorKind::<ident>`, whose body
/// contains the literal text `OfferedVerb::`". A table in another file; one
/// reached through a re-exported alias, a fully-qualified path that never
/// spells `AnchorKind::`, or a helper function called from the arm instead
/// of inlined in it; or one keyed on something other than `AnchorKind`
/// entirely — none of those are seen. The concrete instance already in this
/// crate: `interior/field.rs`'s `warmth_at` contains `if
/// interior.anchor(id).kind != AnchorKind::Hearth { continue; }`, a
/// kind-to-behavior coupling this guard cannot see because it is not in
/// `affordance.rs` and never mentions `OfferedVerb`.
fn anchor_kind_arm_mentions_offered_verb(src: &str) -> bool {
    let bytes = src.as_bytes();
    let anchor_marker = b"AnchorKind::";
    let mut cursor = 0usize;
    while let Some(rel) = find_bytes(&bytes[cursor..], anchor_marker) {
        let variant_start = cursor + rel + anchor_marker.len();
        let mut j = variant_start;
        while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_') {
            j += 1;
        }
        let mut k = j;
        while k < bytes.len() && bytes[k].is_ascii_whitespace() {
            k += 1;
        }
        if bytes[k..].starts_with(b"=>") {
            let mut depth: i32 = 0;
            let mut m = k + 2;
            let mut arm_end = bytes.len();
            while m < bytes.len() {
                match bytes[m] {
                    b'{' | b'(' | b'[' => depth += 1,
                    b'}' | b')' | b']' => {
                        if depth == 0 {
                            arm_end = m;
                            break;
                        }
                        depth -= 1;
                    }
                    b',' if depth == 0 => {
                        arm_end = m;
                        break;
                    }
                    _ => {}
                }
                m += 1;
            }
            if find_bytes(&bytes[k + 2..arm_end], b"OfferedVerb::").is_some() {
                return true;
            }
        }
        // Advance past just the marker (not the whole arm) so an or-pattern
        // like `AnchorKind::Bed | AnchorKind::Pool => ...` still finds the
        // second mention even though the first wasn't followed by `=>`.
        cursor = variant_start;
    }
    false
}

/// Positive control: the exact single-expression (set-builder) form a
/// reviewer demonstrated defeats the old scanner. This must be caught, or
/// the scanner below is exactly as blind as the one it replaces.
#[test]
fn the_table_scanner_catches_a_set_builder_arm() {
    let table = "match kind {\n    \
                  AnchorKind::Bed => [OfferedVerb::Sleep, OfferedVerb::Examine].into_iter().collect(),\n    \
                  _ => BTreeSet::new(),\n\
                  }";
    assert!(
        anchor_kind_arm_mentions_offered_verb(table),
        "positive control: a set-builder verb x object arm must be caught"
    );
}

/// Positive control: the block form of the same forbidden shape.
#[test]
fn the_table_scanner_catches_a_block_arm() {
    let table = "match kind {\n    \
                  AnchorKind::Bed => {\n        \
                  let mut s = BTreeSet::new();\n        \
                  s.insert(OfferedVerb::Sleep);\n        \
                  s\n    \
                  }\n    \
                  _ => BTreeSet::new(),\n\
                  }";
    assert!(
        anchor_kind_arm_mentions_offered_verb(table),
        "positive control: a block-form verb x object arm must be caught"
    );
}

/// Negative control: an `AnchorKind`-keyed arm whose body mentions only
/// `ObjectProperty` (the permitted indirection this whole module is built
/// on) must NOT trip the scanner — otherwise it would also condemn
/// `object_registry` itself.
#[test]
fn the_table_scanner_does_not_false_positive_on_property_indirection() {
    let legitimate = "match kind {\n    \
                       AnchorKind::Bed => ObjectProperty::SupportsRest,\n    \
                       _ => ObjectProperty::HoldsLiquid,\n\
                       }";
    assert!(!anchor_kind_arm_mentions_offered_verb(legitimate));
}

/// Acceptance test (4): no verb x object table exists in `affordance.rs`. A
/// source scan, because the property this asserts is STRUCTURAL — it is
/// about what the code does not contain, which no runtime assertion can
/// witness. See `anchor_kind_arm_mentions_offered_verb`'s own doc comment
/// for exactly what this does and does not prove.
#[test]
fn no_verb_by_object_table_exists() {
    let src = include_str!("../../src/affordance.rs");
    assert!(
        !anchor_kind_arm_mentions_offered_verb(src),
        "affordance.rs maps an AnchorKind variant to an OfferedVerb through a \
         match arm: that is the verb x object table the acceptance test \
         forbids"
    );
}

// --- Task 3: body-relative offers (spec §3.4, Gibson) -------------------

/// A `Body` fixture varying only `species`/`mass_kg`. Built directly from
/// public constructors (`EntityId::new`, `Facet::containing`,
/// `ResourceVector::new`, `ThreatNiche`'s own pub fields) rather than
/// through any world or `Ledger` — the controller's resolution of ambiguity
/// is explicit that this task does not need a built world, and every field
/// here is authored/default the same way `liveness.rs`'s own test bodies
/// are, just assembled locally since those helpers are private to that
/// module.
fn body_with_mass(species: &str, mass_kg: f64) -> Body {
    let home = Facet::containing([0.0, 0.0, 0.0], 6);
    Body {
        entity: EntityId::new(1).expect("1 is a valid entity id"),
        home: home.clone(),
        resource: home,
        species: species.into(),
        activity: hornvale_species::ActivityCycle::Diurnal,
        temperature_niche: ConditionResponse {
            optimum: 15.0,
            width: 10.0,
            devotion: 0.5,
        },
        deliberation_latency: 0.5,
        time_horizon: 0.0,
        thermal_strategy: hornvale_species::ThermalStrategy::Endothermic,
        niche: ResourceVector::new(&[]).expect("the empty niche is valid"),
        boldness: 0.5,
        threat_niche: ThreatNiche {
            uncanny: 1.0,
            heat: 0.0,
            cold: 0.0,
            predator: 0.5,
        },
        mass_kg,
        label: "test-body".into(),
        perception: hornvale_species::PerceptionVector::MANIKIN,
        village: None,
    }
}

/// Gibson, via MAP-19: "a supporter to a sprite is not one to a giant." The
/// same bed offers rest to one body and not another.
///
/// The discriminating pair is `kobold` (13.6 kg) and `woolly-mammoth`
/// (6000.0 kg) from `hornvale_species::biosphere_registry()` — both real,
/// registered species, not a pair chosen from outside the code. The masses
/// are read from the registry rather than hardcoded here, and both are
/// asserted to differ from [`REFERENCE_MASS_KG`] first: `mass_for_species`
/// silently falls back to the reference mass for an unregistered species,
/// so a typo'd label on both sides would produce two identical `70.0`s and
/// a null result that *looks* like a pass.
#[test]
fn the_same_object_offers_differently_to_different_bodies() {
    let biosphere = hornvale_species::biosphere_registry();
    let small_mass = mass_for_species("kobold", Some(&biosphere));
    let large_mass = mass_for_species("woolly-mammoth", Some(&biosphere));

    assert_ne!(
        small_mass, REFERENCE_MASS_KG,
        "kobold must be a real biosphere entry, not a fallback to the \
         reference mass"
    );
    assert_ne!(
        large_mass, REFERENCE_MASS_KG,
        "woolly-mammoth must be a real biosphere entry, not a fallback to \
         the reference mass"
    );
    assert!(
        small_mass < large_mass,
        "the pair must discriminate: kobold ({small_mass} kg) is not \
         lighter than woolly-mammoth ({large_mass} kg)"
    );

    let small = body_with_mass("kobold", small_mass);
    let large = body_with_mass("woolly-mammoth", large_mass);

    assert_ne!(
        offered_to(thing_kind_of(AnchorKind::Bed), &small),
        offered_to(thing_kind_of(AnchorKind::Bed), &large),
        "supports-rest is not body-relative: the offer is identical for \
         bodies of very different mass, so §3.4 is unexercised"
    );
}

/// **Not a live guard on spec §3.4 — a structural sanity check, disclosed
/// here rather than only in `affordance.rs` (final review minor M-a).**
/// `offered_to(kind, body)` is *defined* as
/// `offered_by(kind).into_iter().filter(..)` (`affordance.rs`), and a
/// filter over a baseline is a subset of that baseline for ANY predicate —
/// including a restrictive one `body_can_use` could never legally return.
/// So this assertion holds unconditionally, by construction, regardless of
/// whether §3.4's additive-only rule is actually honoured; it cannot
/// distinguish a correct `body_can_use` from a broken one. Kept anyway as a
/// cheap regression check on the SHAPE of `offered_to` itself (that it
/// really is implemented as a filter over `offered_by`, across every
/// registered kind plus one unregistered one), not as evidence for §3.4.
///
/// **The real, falsifiable additive-rule guard is
/// `only_supports_rest_is_body_relative_in_iv_a`**, in `affordance.rs`'s
/// own `#[cfg(test)] mod tests` — it asserts against [`crate::affordance`]'s
/// private `body_can_use` directly, outside this filter chain, and is what
/// actually reddens under a mass-gated `AffordsPassage` (the exact mutation
/// §3.4 forbids by name). That test's own doc comment carries the full
/// history of why this one could not.
#[test]
fn body_relativity_never_withdraws_an_existing_capability() {
    let biosphere = hornvale_species::biosphere_registry();
    let bodies = [
        body_with_mass("kobold", mass_for_species("kobold", Some(&biosphere))),
        body_with_mass("human", mass_for_species("human", Some(&biosphere))),
        body_with_mass(
            "woolly-mammoth",
            mass_for_species("woolly-mammoth", Some(&biosphere)),
        ),
    ];

    let mut kinds: Vec<KindId> = object_registry().iter().map(|(k, _)| *k).collect();
    kinds.push(thing_kind_of(AnchorKind::Screen));

    for kind in kinds {
        let baseline = offered_by(kind);
        for body in &bodies {
            let narrowed = offered_to(kind, body);
            assert!(
                narrowed.is_subset(&baseline),
                "{:?} offered {:?} to a body of mass {} kg, which offered_by({:?}) \
                 does not grant at all: body-relativity may only narrow \
                 offered_by's set, never exceed it",
                kind,
                narrowed.difference(&baseline).collect::<Vec<_>>(),
                body.mass_kg,
                kind
            );
        }
    }
}

// --- Task 4: the offer is knowledge-gated (spec §3.5) -------------------

/// A body that has not encountered anything is offered nothing by an
/// object it would otherwise reach — [`Knowledge::default`] is the empty
/// store, and `offered_to_observer` must withdraw the whole offer for it.
///
/// **The mutation this test must catch, named per the task brief:** make
/// `offered_to_observer` ignore `known` entirely and delegate straight to
/// `offered_to`. Without this test that mutation is invisible — see
/// `an_encountered_object_offers_its_verbs` below for why this direction
/// alone is not enough either.
#[test]
fn an_unencountered_object_offers_nothing() {
    let body = body_with_mass("human", REFERENCE_MASS_KG);
    let empty = Knowledge::default();
    assert!(
        offered_to_observer(AnchorKind::Hearth, &body, &empty).is_empty(),
        "a body with no recorded knowledge must be offered nothing"
    );
}

/// ...and the same object offers its verbs once a room has been
/// encountered — asserted as the SPECIFIC set `offered_to` returns, not
/// mere non-emptiness. `!is_empty()` would be satisfiable by a knowledge
/// filter that drops verbs arbitrarily, or by one that ignores knowledge
/// entirely for any object carrying two or more verbs (`Hearth` offers
/// both `Warm` and `Examine`, so it exercises that risk directly). Without
/// this direction, `an_unencountered_object_offers_nothing` alone is
/// satisfiable by a function that always returns the empty set.
#[test]
fn an_encountered_object_offers_its_verbs() {
    let body = body_with_mass("human", REFERENCE_MASS_KG);
    // A Knowledge store that has recorded a room — the room-granularity
    // gate `offered_to_observer` actually checks (its own doc explains why
    // the interface cannot express anchor- or specific-room granularity).
    // The exact packed id is irrelevant to the gate, which only tests the
    // key's `room/` prefix; a real one from `IdentityProjection` would
    // differ only in that string, not in which branch this exercises.
    let known = Knowledge(std::collections::BTreeMap::from([(
        "room/1".to_string(),
        "recorded".to_string(),
    )]));
    assert_eq!(
        offered_to_observer(AnchorKind::Hearth, &body, &known),
        offered_to(thing_kind_of(AnchorKind::Hearth), &body),
        "knowledge of an encountered room must withdraw nothing"
    );
}

// --- Fix wave (final whole-branch review, I1): warm must not reintroduce
// the per-kind coupling this campaign abolishes --------------------------

/// `Warm` is offered to ANY object carrying [`ObjectProperty::RadiatesHeat`]
/// — never to `AnchorKind::Hearth` because it is named `Hearth`. This is
/// the M+N claim `a_kind_gains_every_verb_its_properties_satisfy_with_no_
/// dispatcher_edit` already proves for `Drink`/`HoldsLiquid`; restated here
/// for `Warm`/`RadiatesHeat` because I1 found the one place in this
/// codebase that did NOT go through this derivation:
/// `Session::warm` (`session.rs`) used to compare `interior.anchor(a).kind
/// == AnchorKind::Hearth` directly, a hardcoded per-kind check one file
/// over from this exact query. `warm_appears_on_hearth_with_no_object_
/// table_edit` already shows the QUERY is generic; `no_hardcoded_anchor_
/// kind_gates_warm` below is the companion proof that `Session::warm`
/// actually reaches it rather than short-circuiting past it — the two
/// together are the assertion that would have caught I1, since neither one
/// alone does (the query was always generic; the dispatcher was the bug).
#[test]
fn warm_is_offered_to_any_object_carrying_radiates_heat_not_only_hearth() {
    let mut traits = ObjectTraits::default();
    traits.properties.insert(ObjectProperty::RadiatesHeat);
    assert!(
        offered(&traits).contains(&OfferedVerb::Warm),
        "an object carrying RadiatesHeat must offer Warm regardless of \
         which AnchorKind (if any) produced those traits"
    );
    let bare = ObjectTraits::default();
    assert!(
        !offered(&bare).contains(&OfferedVerb::Warm),
        "an object carrying no properties must not offer Warm"
    );
}

/// Extracts the `{ ... }` block immediately following the first occurrence
/// of `needle` in `src`, tracking brace depth from the block's own opening
/// `{` to its matching close. Used below to isolate exactly `Session::
/// warm`'s body out of `session.rs`'s several thousand lines — scanning the
/// WHOLE file would also match `AnchorKind::Hearth` in `warm`'s own doc
/// comment and in unrelated methods (`chamber_sources`'s light check,
/// `interior/field.rs`'s `warmth_at`), none of which this test is about.
fn block_body_after<'a>(src: &'a [u8], needle: &[u8]) -> Option<&'a [u8]> {
    let start = find_bytes(src, needle)? + needle.len();
    let open = start + src[start..].iter().position(|&b| b == b'{')?;
    let mut depth: i32 = 0;
    for (i, &b) in src[open..].iter().enumerate() {
        match b {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(&src[open..=open + i]);
                }
            }
            _ => {}
        }
    }
    None
}

/// Positive control: `block_body_after` must actually isolate the block it
/// claims to, not the whole remainder of the source.
#[test]
fn block_body_after_isolates_exactly_the_matched_block() {
    let src = b"fn a() { one(); { nested(); } }\nfn b() { two(); }";
    let body = block_body_after(src, b"fn a()").expect("must find fn a's block");
    assert!(std::str::from_utf8(body).unwrap().contains("one()"));
    assert!(
        !std::str::from_utf8(body).unwrap().contains("two()"),
        "must not run past fn a's own closing brace into fn b: {:?}",
        std::str::from_utf8(body).unwrap()
    );
}

/// The Offer, fix wave (I1): `Session::warm`'s own body must contain no
/// `AnchorKind::` literal at all — the shape a per-kind dispatcher edit
/// would take (`interior.anchor(a).kind == AnchorKind::Hearth`, the exact
/// code this test's own mutation restores). A verb gated on the OFFER never
/// needs to name a specific kind; naming one is the M×N coupling spec §3.2
/// exists to abolish, reintroduced here even though every other surface
/// (`examine_chamber`) was fixed cleanly at Task 7.
///
/// **Would NOT have been caught by a plain "warm succeeds at a real
/// hearth" test alone.** `AnchorKind::Bed` always co-occurs with
/// `AnchorKind::Hearth` in every real chamber (`the-fireside-bed` requires
/// `Hearth` in the same chamber, `interior/pattern.rs`), so a session-level
/// success test cannot distinguish "gated on Hearth" from "gated on Bed" —
/// confirmed directly in `session.rs`'s own `warm_succeeds_at_a_real_
/// hearth_through_a_real_session` doc comment. A structural scan of the
/// DISPATCHER's own source, in the style of `no_verb_by_object_table_exists`
/// above, is what actually distinguishes "routes through the offer" from
/// "hardcodes a kind that happens to agree with the offer today".
///
/// Mutation this must fail against: revert `Session::warm` (session.rs) to
/// the pre-fix body, `interior.anchor(a).kind ==
/// crate::interior::AnchorKind::Hearth` — reddens (confirmed in the fix
/// wave's report) while every other `warm`/`examine` test stays green,
/// exactly the I1 finding.
///
/// **The direction this enforces, stated so it cannot be mistaken for a
/// broader guarantee** — the same disclosure
/// `anchor_kind_arm_mentions_offered_verb` above carries, for the same
/// reason. This scans exactly the block `block_body_after` isolates for
/// `fn warm(&self) -> Turn {`, for exactly the literal text
/// `AnchorKind::`. **Extracting the gate into a one-line private helper
/// defeats it while it stays green**: `fn hearth_here(i: &Interior) ->
/// bool { i.ids().iter().any(|&a| i.anchor(a).kind ==
/// AnchorKind::Hearth) }`, called from `warm`, moves the literal out of
/// the block this reads and reintroduces the coupling with no test
/// objecting. A re-reviewer BUILT that evasion and confirmed it. Nor does
/// this see a coupling reached through a re-exported alias, a
/// fully-qualified path that never spells `AnchorKind::`, or a gate keyed
/// on something other than an anchor kind. The concrete in-tree instance,
/// the same one the sibling guard names: `interior/field.rs`'s
/// `warmth_at` contains `if interior.anchor(id).kind !=
/// AnchorKind::Hearth { continue; }` — a live kind-to-behaviour coupling
/// neither guard can see, because it is in another file and mentions no
/// `OfferedVerb`. Acceptance clause (4) is worded against what these two
/// scans actually cover (spec §6, decision 0350), not against "anywhere".
#[test]
fn no_hardcoded_anchor_kind_gates_warm() {
    let src = include_str!("../../src/session.rs");
    let body = block_body_after(src.as_bytes(), b"fn warm(&self) -> Turn {")
        .expect("session.rs must define fn warm(&self) -> Turn");
    assert!(
        find_bytes(body, b"AnchorKind::").is_none(),
        "Session::warm's body names a specific AnchorKind literal directly, \
         reintroducing the per-kind coupling spec 3.2 forbids: {:?}",
        std::str::from_utf8(body).unwrap_or("<non-utf8>")
    );
}

// --- The Chattel, Task 7: the re-key from AnchorKind to KindId ----------

/// **The guard the re-key itself needed, and the one The Offer's suite could
/// not provide.** Every test above reads the table through
/// `thing_kind_of`, so a mapping arm that sends an anchor kind to the WRONG
/// thing-kind is invisible to all of them whenever the wrong kind happens to
/// carry the right property — `Pool -> KindId("vessel")` passes
/// `a_kind_gains_every_verb_its_properties_satisfy_with_no_dispatcher_edit`
/// unchanged, because a vessel holds liquid too. This freezes the whole
/// fourteen-row answer instead: what verb set each `AnchorKind` offers,
/// which is exactly what The Offer's table produced before the re-key and
/// what spec §3.6 requires the re-key to preserve.
///
/// It is a frozen expectation, not a derivation — deriving it from
/// `object_registry` would re-use the machinery under test and assert
/// nothing. Moving a row here is therefore a deliberate act, the same
/// discipline a byte-golden carries.
///
/// MUTATION THIS MUST FAIL AGAINST: point `thing_kind_of`'s `Pool` arm at
/// `KindId("bed")` — a mapping error that keeps every property real. Red
/// observed:
///
/// ```text
/// thread 'affordance::the_re_key_preserves_every_anchor_kinds_offer' panicked at
/// windows/vessel/tests/suite/affordance.rs:728:9:
/// assertion `left == right` failed: Pool (KindId("bed")) offers {Sleep, Examine},
/// but the pre-re-key table offered {Drink, Examine}
///   left: {Sleep, Examine}
///  right: {Drink, Examine}
/// ```
///
/// That run reddened THREE tests, not one — this,
/// `the_anchor_to_thing_kind_mapping_is_injective`, and
/// `a_kind_gains_every_verb_its_properties_satisfy_with_no_dispatcher_edit`
/// — which is stated rather than trimmed because it is the honest shape of
/// the evidence: this mutation is caught several ways over. The mutation
/// that isolates THIS test is `Log -> KindId("ground")`'s sibling in the
/// other direction — see the injectivity test's own doc for the pair that
/// separates the two.
#[test]
fn the_re_key_preserves_every_anchor_kinds_offer() {
    use OfferedVerb::{Drink, Enter, Examine, Sleep, Warm};
    let expected: [(AnchorKind, &[OfferedVerb]); 14] = [
        (AnchorKind::Hearth, &[Examine, Warm]),
        (AnchorKind::Threshold, &[Enter, Examine]),
        (AnchorKind::Bed, &[Sleep, Examine]),
        (AnchorKind::Vessel, &[Drink, Examine]),
        (AnchorKind::Screen, &[Examine]),
        (AnchorKind::Pool, &[Drink, Examine]),
        (AnchorKind::Log, &[Examine]),
        (AnchorKind::Ground, &[Examine]),
        // Encloses gates no OfferedVerb (it is read by `examine`'s prose,
        // not by the offer query), so an enclosing kind offers Examine and
        // nothing more — and so does a strongbox, whose Openable/Lockable
        // wait on the verbs Tasks 10-11 ship.
        (AnchorKind::Alcove, &[Examine]),
        (AnchorKind::Strongbox, &[Examine]),
        (AnchorKind::HighSeat, &[Examine]),
        (AnchorKind::Loom, &[Examine]),
        (AnchorKind::Anvil, &[Examine]),
        (AnchorKind::Altar, &[Examine]),
    ];
    for (kind, want) in expected {
        let want: BTreeSet<OfferedVerb> = want.iter().copied().collect();
        let got = offered_by(thing_kind_of(kind));
        assert_eq!(
            got,
            want,
            "{kind:?} ({:?}) offers {got:?}, but the pre-re-key table offered {want:?}",
            thing_kind_of(kind)
        );
    }
}

/// Every `AnchorKind` maps to a DISTINCT thing-kind. Not a law forever — two
/// anchor kinds could honestly be one thing some day — but it is the
/// property that makes the frozen table above a preservation claim rather
/// than a coincidence: if two anchor kinds collapsed onto one key they would
/// silently share one property set, which is the two-lifecycle-stage
/// disagreement spec §3.6 re-keys to prevent, arriving from the other
/// direction.
///
/// MUTATION THIS MUST FAIL AGAINST: point `thing_kind_of`'s `Log` arm at
/// `KindId("ground")` (both carry no property, so
/// `the_re_key_preserves_every_anchor_kinds_offer` stays green — the two
/// tests genuinely cover different failures, confirmed: the run below failed
/// exactly one test, `21 passed; 1 failed`). Red observed:
///
/// ```text
/// test affordance::the_anchor_to_thing_kind_mapping_is_injective ... FAILED
/// thread 'affordance::the_anchor_to_thing_kind_mapping_is_injective' panicked at
/// windows/vessel/tests/suite/affordance.rs:776:9:
/// two anchor kinds map to KindId("ground"): 7 kinds for 14 variants
/// ```
///
/// (`7`, not `13`: the count is how many DISTINCT keys had been accepted
/// when the collision was hit, and `Log` is the seventh variant swept — the
/// message reports progress, not a total.)
#[test]
fn the_anchor_to_thing_kind_mapping_is_injective() {
    let all = [
        AnchorKind::Hearth,
        AnchorKind::Threshold,
        AnchorKind::Bed,
        AnchorKind::Vessel,
        AnchorKind::Screen,
        AnchorKind::Pool,
        AnchorKind::Log,
        AnchorKind::Ground,
        AnchorKind::Alcove,
        AnchorKind::Strongbox,
        AnchorKind::HighSeat,
        AnchorKind::Loom,
        AnchorKind::Anvil,
        AnchorKind::Altar,
    ];
    let mut seen: BTreeSet<KindId> = BTreeSet::new();
    for kind in all {
        let id = thing_kind_of(kind);
        assert!(
            seen.insert(id),
            "two anchor kinds map to {id:?}: {} kinds for {} variants",
            seen.len(),
            all.len()
        );
    }
}

/// `thing_kind_of` must stay exhaustive with **no wildcard arm** — a `_ =>`
/// would compile, change no behaviour today, and silently absorb the next
/// appended `AnchorKind` variant into whatever default it named, inheriting
/// that thing-kind's properties. No behavioural test can see that: the
/// mapping is total for all fourteen variants either way, so totality is
/// only observable in the SOURCE until the day someone appends a variant,
/// which is precisely too late.
///
/// A structural scan, in the style of `no_verb_by_object_table_exists` and
/// `no_hardcoded_anchor_kind_gates_warm` above, with the same disclosure:
/// it reads exactly the block `block_body_after` isolates, for exactly the
/// literal text `_ =>`. A wildcard spelled `other =>` or `kind @ _ =>` is
/// not seen; neither is one in a different function. The synthetic positive
/// control below shows the scanner can fire at all.
///
/// MUTATION THIS MUST FAIL AGAINST — and this one is the real evidence,
/// because it is run against PRODUCTION source rather than a synthetic
/// string: replace `thing_kind_of`'s `AnchorKind::Altar => KindId("altar")`
/// arm with `_ => KindId("altar")`. It compiles, changes no behaviour, and
/// no other test in the file notices (`21 passed; 1 failed`). Red observed:
///
/// ```text
/// test affordance::thing_kind_of_has_no_wildcard_arm ... FAILED
/// thread 'affordance::thing_kind_of_has_no_wildcard_arm' panicked at
/// windows/vessel/tests/suite/affordance.rs:807:5:
/// thing_kind_of has a wildcard arm: an appended AnchorKind variant would fall
/// through it instead of failing to compile, and inherit a thing-kind's
/// properties silently: "{\n    match kind {\n        AnchorKind::Hearth => ...
/// ```
#[test]
fn thing_kind_of_has_no_wildcard_arm() {
    let src = include_str!("../../src/affordance.rs");
    let body = block_body_after(
        src.as_bytes(),
        b"pub fn thing_kind_of(kind: AnchorKind) -> KindId",
    )
    .expect("affordance.rs must define pub fn thing_kind_of(kind: AnchorKind) -> KindId");
    assert!(
        find_bytes(body, b"_ =>").is_none(),
        "thing_kind_of has a wildcard arm: an appended AnchorKind variant \
         would fall through it instead of failing to compile, and inherit a \
         thing-kind's properties silently: {:?}",
        std::str::from_utf8(body).unwrap_or("<non-utf8>")
    );
}

/// Positive control for the scan above: the forbidden shape must actually be
/// caught, or `thing_kind_of_has_no_wildcard_arm`'s green is worth nothing.
#[test]
fn the_wildcard_scan_catches_a_wildcard_arm() {
    let src = b"pub fn thing_kind_of(kind: AnchorKind) -> KindId {\n    \
                 match kind {\n        \
                 AnchorKind::Bed => KindId(\"bed\"),\n        \
                 _ => KindId(\"ground\"),\n    \
                 }\n\
                 }";
    let body = block_body_after(src, b"pub fn thing_kind_of(kind: AnchorKind) -> KindId")
        .expect("the control must parse");
    assert!(
        find_bytes(body, b"_ =>").is_some(),
        "positive control: a wildcard arm must be caught"
    );
}

/// The registry can now discriminate a subset filter from an equality check
/// on its OWN rows, which it could not before Task 7 — `strongbox` carries
/// three properties (`Encloses`, `Openable`, `Lockable`), so a `required`
/// set of one is a strict subset of it. This is the corrected half of
/// `extra_properties_expand_the_offer_never_withdraw_it`'s doc comment,
/// asserted rather than merely stated.
///
/// The multi-property carrier is discovered from the registry, not named:
/// the point is that SOME registered kind has this shape, and hard-coding
/// `strongbox` would turn a property of the table into a fact about one row.
#[test]
fn a_registered_multi_property_kind_discriminates_subset_from_equality() {
    let reg = object_registry();
    let multi: Vec<(KindId, BTreeSet<ObjectProperty>)> = reg
        .iter()
        .filter(|(_, t)| t.properties.len() >= 2)
        .map(|(k, t)| (*k, t.properties.clone()))
        .collect();
    assert!(
        !multi.is_empty(),
        "no registered kind carries two or more properties, so the registry \
         cannot tell a subset filter from an equality check — \
         extra_properties_expand_the_offer_never_withdraw_it's constructed \
         traits are the only discriminating datum left, and this test's own \
         doc comment must be corrected rather than the test deleted"
    );
    for (kind, properties) in multi {
        let one: BTreeSet<ObjectProperty> = properties.iter().copied().take(1).collect();
        assert!(
            one.is_subset(&properties),
            "{kind:?}: a single property must be a subset of its own row"
        );
        assert_ne!(
            one, properties,
            "{kind:?}: subset and equality must genuinely disagree on this row"
        );
    }
}

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
//!   `ObjectTraits` the registry itself cannot produce (every registered
//!   kind carries exactly one property, which cannot distinguish subset
//!   from equality) — and rewriting the test to call it.
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

use hornvale_vessel::affordance::{
    ObjectProperty, ObjectTraits, OfferedVerb, object_registry, offered, offered_by,
};
use hornvale_vessel::interior::AnchorKind;

/// Acceptance test (1): a new OBJECT kind ships with properties only — no
/// dispatcher change — and the right verbs appear on it.
#[test]
fn a_kind_gains_every_verb_its_properties_satisfy_with_no_dispatcher_edit() {
    // Pool carries HoldsLiquid; nothing anywhere names "pool" and "drink"
    // together. The verb arrives because the property matches.
    assert!(offered_by(AnchorKind::Pool).contains(&OfferedVerb::Drink));
    assert!(!offered_by(AnchorKind::Bed).contains(&OfferedVerb::Drink));
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
#[test]
fn every_kind_carrying_a_shared_property_is_offered_the_verb_it_gates() {
    let want = BTreeSet::from([ObjectProperty::HoldsLiquid]);
    let reg = object_registry();
    let carriers: BTreeSet<AnchorKind> = reg
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
/// No registered kind could do this job: every one of `object_registry`'s
/// six carriers holds exactly one property today, so subset and equality
/// agree on all of them — this test has to reach for constructed traits
/// precisely because the registry can't discriminate the two.
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
    assert!(offered_by(AnchorKind::Hearth).contains(&OfferedVerb::Warm));
    assert!(!offered_by(AnchorKind::Bed).contains(&OfferedVerb::Warm));
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
    assert!(!object_registry().contains(&AnchorKind::Screen));
    let offered = offered_by(AnchorKind::Screen);
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

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
//!   between a `kinds::X =>` and the arm's end, catching both shapes.
//!   Its own positive/negative controls are below, so the scanner's claim
//!   to catch the forbidden shape is evidence, not assertion.

use std::collections::BTreeSet;

use hornvale_kernel::{ConditionResponse, EntityId, Facet, KindId, ResourceVector};
use hornvale_thing::kinds;
use hornvale_vessel::Knowledge;
use hornvale_vessel::affordance::{
    ObjectProperty, ObjectTraits, OfferedVerb, Substrate, object_registry, offered, offered_by,
    offered_to, offered_to_observer,
};
use hornvale_vessel::body::Body;
use hornvale_vessel::clock::{REFERENCE_MASS_KG, mass_for_species};
use hornvale_vessel::liveness::ThreatNiche;
use hornvale_vessel::{PossessOpts, Session, Turn};

use crate::common;

/// Every kind the roster carries, once — `hornvale_thing::THING_KINDS`, read
/// as ids.
///
/// **It was `AnchorKind::ALL`, generated from the enum's own declaration, and
/// what replaces it had to keep that generated property (The Wicket, Task
/// 2).** The paragraph this replaces recorded why, and the reason outlives
/// the enum: Task 9 of The Chattel hoisted a hand-written `[AnchorKind; 14]`
/// into this file and answered "what stops THIS copy going short" with the
/// compiler plus a two-way agreement against the frozen verb table below.
/// Both halves were true and neither was anchored to CARDINALITY. The
/// compiler forces an ARM, never a LIST ENTRY; two hand-maintained lists go
/// short **together**, since the person appending a kind touches neither.
/// Fix round 1 measured it: a fifteenth variant, given the arms the compiler
/// demanded and pointed at `KindId("cave-mouth")`, left 1209 tests green —
/// including a precondition whose doc comment promised to stop being
/// evidence in exactly that case.
///
/// **Task 2 landed this reading `hornvale_thing::kinds::EVERY_HANDLE`, and
/// its own doc said that was one step weaker than what it replaced:**
/// `EVERY_HANDLE` is hand-written, so a handle quietly dropped from it would
/// narrow this sweep and nothing would object — `every_named_handle_is_a_
/// roster_row` (G-d, Task 1) only checks *named ⊆ rostered*, never the
/// converse. Task 3 (The Wicket, spec §5) points this at `THING_KINDS`
/// instead: that roster is frozen as an ordered SET by
/// `the_roster_is_frozen_as_an_ordered_set` (G-f), so it — unlike a
/// hand-written handle list — cannot go short without a visible edit to a
/// committed list. Handles exist for *code that names a kind*; a sweep
/// asserting totality is not that, so it wants the set that cannot go short,
/// not the set of names someone happened to write code against. The swap
/// needs no new mechanism, and it is strictly stronger: `EVERY_HANDLE` and
/// `THING_KINDS` name the same **22** kinds in the same order today (both
/// alphabetical by label), so this table was unchanged by the swap — see the
/// ledger entry ruling on this at plan time.
///
/// (**That count read 16 until The Wicket's close.** It was correct when Task
/// 3 wrote it; `brazier`, `door`, `bench`, and The Tenon's three rest surfaces
/// subsequently moved both lists together, which is why the sentence stayed
/// true-looking each time: the two lists agree, so the *claim* survives while
/// its *number* dies. Nothing reddens on a count written into prose — ledger
/// #58, #60, and the reason this file's own guarantees are asserted in code
/// rather than described here.)
fn every_rostered_kind() -> Vec<KindId> {
    hornvale_thing::THING_KINDS
        .iter()
        .map(|l| KindId(l))
        .collect()
}

/// Acceptance test (1): a new OBJECT kind ships with properties only — no
/// dispatcher change — and the right verbs appear on it.
///
/// **What it still catches after Task 7's re-key** (the write-up spec §7's
/// Task 3 demands, kept beside the test rather than in a report that dies
/// with the campaign): the query is now keyed on thing-kind, so this reads
/// `offered_by(kinds::POOL)` and covers one more link than it used to — a
/// `pool` row that loses `HoldsLiquid`, a `required_properties(Drink)` that
/// stops requiring it, and a subset filter broken to return everything or
/// nothing. **It used to cover one more than that**, an anchor-kind → label
/// mapping arm carrying `Pool` to a kind with no liquid; The Wicket deleted
/// that mapping, so the link is gone rather than unguarded.
/// `the_re_key_preserves_every_anchor_kinds_offer` is still what freezes the
/// whole per-kind answer, and is still the reason this file needs that test
/// at all.
#[test]
fn a_kind_gains_every_verb_its_properties_satisfy_with_no_dispatcher_edit() {
    // Pool carries HoldsLiquid; nothing anywhere names "pool" and "drink"
    // together. The verb arrives because the property matches.
    assert!(offered_by(kinds::POOL).contains(&OfferedVerb::Drink));
    assert!(!offered_by(kinds::BED).contains(&OfferedVerb::Drink));
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
/// [`KindId`]s read from the registry rather than as anchor-kind variants. The
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
    assert!(offered_by(kinds::HEARTH).contains(&OfferedVerb::Warm));
    assert!(!offered_by(kinds::BED).contains(&OfferedVerb::Warm));
}

/// A brazier offers `warm`, and it is the first carrier of `RadiatesHeat`
/// outside a hearthroom. Before The Wicket this kind could not exist: `warm`
/// reads whichever anchor is present through `offered_to_observer`, but the
/// only kinds an anchor could BE were the enum's fifteen.
///
/// MUTATION THIS MUST FAIL AGAINST: drop `ObjectProperty::RadiatesHeat` from
/// the `brazier` row in `object_registry()`. Applied with `scripts/
/// mutate.py`, run unfiltered over the crate (`--no-fail-fast`), restored
/// from a `cp` backup and re-run green afterwards. Red observed
/// 2026-09-01 — **two tests failed, not one**, the same shape
/// `an_encountered_passage_offers_its_verbs`'s own doc names above:
///
/// ```text
///     Summary [ 190.823s] 881 tests run: 879 passed, 2 failed, 3 skipped
///        FAIL [   0.016s] (594/881) hornvale-vessel::suite affordance::a_brazier_offers_warm_with_no_dispatcher_edit
///        FAIL [   0.012s] (621/881) hornvale-vessel::suite affordance::the_re_key_preserves_every_anchor_kinds_offer
///
/// thread 'affordance::a_brazier_offers_warm_with_no_dispatcher_edit' panicked at windows/vessel/tests/suite/affordance.rs:240:5:
/// assertion failed: offered_by(kinds::BRAZIER).contains(&OfferedVerb::Warm)
///
/// thread 'affordance::the_re_key_preserves_every_anchor_kinds_offer' panicked at windows/vessel/tests/suite/affordance.rs:1726:9:
/// assertion `left == right` failed: KindId("brazier") (KindId("brazier")) offers {Examine}, but the pre-re-key table offered {Examine, Warm}
///   left: {Examine}
///  right: {Examine, Warm}
/// ```
///
/// This test and the frozen table below it (`the_re_key_preserves_every_
/// anchor_kinds_offer`) catch the same mutation two ways, the same
/// over-determination `an_encountered_passage_offers_its_verbs`'s doc
/// already remarks on rather than trims.
#[test]
fn a_brazier_offers_warm_with_no_dispatcher_edit() {
    assert!(offered_by(kinds::BRAZIER).contains(&OfferedVerb::Warm));
    assert!(!offered_by(kinds::ALTAR).contains(&OfferedVerb::Warm));
}

/// The Tenon's three authored kinds are real sleep surfaces, not prose-only
/// roster rows: each advertises `Sleep` through `SupportsRest`, and the
/// payloads distinguish the yielding pair from the hard ledge. These literals
/// are the pre-implementation authorship decision — changing a row's offer or
/// hardness changes the edge the recovery fold and sleep-site chooser read.
///
/// MUTATIONS THIS MUST FAIL AGAINST: omit any one of the three rows; omit its
/// `SupportsRest`; swap the ledge's hard substrate with either yielding one;
/// or change any authored offer/hardness value.
#[test]
fn three_natural_surfaces_offer_sleep_with_their_authored_substrates() {
    let reg = object_registry();
    let expected = [
        (
            KindId("rushes"),
            hornvale_vessel::affordance::RestSurface {
                offer: 0.7,
                substrate: Substrate::Natural(0.1),
            },
        ),
        (
            KindId("ledge"),
            hornvale_vessel::affordance::RestSurface {
                offer: 0.7,
                substrate: Substrate::Natural(0.85),
            },
        ),
        (
            KindId("bracken"),
            hornvale_vessel::affordance::RestSurface {
                offer: 0.7,
                substrate: Substrate::Natural(0.1),
            },
        ),
    ];

    for (kind, surface) in expected {
        let traits = reg
            .get(&kind)
            .unwrap_or_else(|| panic!("{kind:?} has no ObjectTraits row"));
        assert!(
            traits.properties.contains(&ObjectProperty::SupportsRest),
            "{kind:?} does not advertise Sleep"
        );
        assert_eq!(traits.rest, Some(surface), "{kind:?} surface moved");
        assert!(
            offered_by(kind).contains(&OfferedVerb::Sleep),
            "{kind:?} carries a surface but the offer query cannot reach it"
        );
    }
}

/// `Examine` requires the empty property set (spec §3.3: universal), and the
/// empty set is a subset of every set — including the empty set itself. So
/// universality must hold even for a kind `object_registry` never
/// mentions at all (e.g. `Screen`, which Task 1 verified carries no property):
/// there is no per-kind registry entry to fall back on, so this is the
/// sharpest test that `offered_by` derives universality from the subset
/// relation rather than from iterating only over registered kinds.
#[test]
fn examine_is_universal_even_for_a_kind_with_no_registered_properties() {
    assert!(!object_registry().contains(&kinds::SCREEN));
    let offered = offered_by(kinds::SCREEN);
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

/// Whether `src` contains a match arm keyed on a specific thing-kind
/// variant whose body mentions an `OfferedVerb` variant anywhere — the
/// verb x object table shape acceptance test (4) forbids.
///
/// **This replaces a scanner a review found could not fire.** The original
/// looked for the literal substring `"=> OfferedVerb::"`, but `offered_by`
/// returns `BTreeSet<OfferedVerb>`, so no real per-kind arm's output type is
/// ever `OfferedVerb` itself — a hardcoded table has to build a *set*:
/// `kinds::BED => [OfferedVerb::Sleep, OfferedVerb::Examine]
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
/// pattern containing the literal text `<marker><key>`, whose body contains
/// the literal text `OfferedVerb::`". A table in another file; one reached
/// through a re-exported alias, a fully-qualified path that never spells the
/// marker, or a helper function called from the arm instead of inlined in
/// it; or one keyed on something the callers below do not pass — none of
/// those are seen. The concrete instance already in this crate:
/// `interior/field.rs`'s `warmth_at` contains `if interior.anchor(id).kind
/// != kinds::HEARTH { continue; }`, a kind-to-behavior coupling this
/// guard cannot see because it is not in `affordance.rs` and never mentions
/// `OfferedVerb`.
///
/// **`marker` is a parameter since The Chattel's Task 7, and that is a
/// coverage repair rather than a tidy-up.** The scan was hard-coded to
/// the anchor-kind enum's variant spelling, which was the property table's
/// key when The Offer wrote
/// it. Task 7 re-keyed the table to `KindId`, so the natural spelling of a
/// hardcoded verb table moved with it — `KindId("bed") => [OfferedVerb::
/// Sleep, ...]` is a legal match arm (a tuple-struct pattern over a string
/// literal) that the old scan could not see at all. Nothing would have gone
/// red; acceptance clause (4) would simply have stopped covering the shape
/// anyone would now write. Both markers are scanned by the two callers
/// below.
fn arm_mentions_offered_verb(src: &str, marker: &[u8]) -> bool {
    let bytes = src.as_bytes();
    let mut cursor = 0usize;
    while let Some(rel) = find_bytes(&bytes[cursor..], marker) {
        let variant_start = cursor + rel + marker.len();
        let mut j = variant_start;
        // The key text between the marker and the `=>`. The set is wide
        // enough for both a `kinds` handle name (`BED`) and a
        // `KindId` literal's remainder (`"cave-mouth")`), so one scan
        // serves both markers; it is a superset for the enum case and
        // changes nothing there.
        while j < bytes.len()
            && (bytes[j].is_ascii_alphanumeric()
                || bytes[j] == b'_'
                || bytes[j] == b'"'
                || bytes[j] == b'-'
                || bytes[j] == b')')
        {
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
        // like `kinds::BED | kinds::POOL => ...` still finds the
        // second mention even though the first wasn't followed by `=>`.
        cursor = variant_start;
    }
    false
}

/// The `kinds::`-keyed reading of [`arm_mentions_offered_verb`] — the shape
/// The Offer's acceptance clause (4) named, kept as its own function so the
/// three controls below read unchanged.
///
/// **The marker moved from `AnchorKind::` to `kinds::` (The Wicket, Task
/// 2), and moving it was the whole repair rather than a rename.** The Offer
/// wrote this against the enum's variant spelling; The Wicket deleted the
/// enum, so `AnchorKind::` can no longer appear in any file and a scan for
/// it would have gone permanently, invisibly vacuous — green because its
/// subject left the tree, not because the forbidden shape is absent. The
/// spelling a hardcoded anchor-side verb table would take today is
/// `kinds::HEARTH => [OfferedVerb::Warm, ...]`, and that is what this now
/// reads. This is the same repair Task 7's re-key forced on the sibling
/// `KindId(` reading, one campaign later and one key further along.
fn handle_arm_mentions_offered_verb(src: &str) -> bool {
    arm_mentions_offered_verb(src, b"kinds::")
}

/// The `KindId(`-keyed reading — the shape a hardcoded verb table would take
/// AFTER Task 7's re-key, and the one the scan was blind to until it was
/// parameterised.
fn thing_kind_arm_mentions_offered_verb(src: &str) -> bool {
    arm_mentions_offered_verb(src, b"KindId(")
}

/// Positive control: the exact single-expression (set-builder) form a
/// reviewer demonstrated defeats the old scanner. This must be caught, or
/// the scanner below is exactly as blind as the one it replaces.
#[test]
fn the_table_scanner_catches_a_set_builder_arm() {
    let table = "match kind {\n    \
                  kinds::BED => [OfferedVerb::Sleep, OfferedVerb::Examine].into_iter().collect(),\n    \
                  _ => BTreeSet::new(),\n\
                  }";
    assert!(
        handle_arm_mentions_offered_verb(table),
        "positive control: a set-builder verb x object arm must be caught"
    );
}

/// Positive control: the block form of the same forbidden shape.
#[test]
fn the_table_scanner_catches_a_block_arm() {
    let table = "match kind {\n    \
                  kinds::BED => {\n        \
                  let mut s = BTreeSet::new();\n        \
                  s.insert(OfferedVerb::Sleep);\n        \
                  s\n    \
                  }\n    \
                  _ => BTreeSet::new(),\n\
                  }";
    assert!(
        handle_arm_mentions_offered_verb(table),
        "positive control: a block-form verb x object arm must be caught"
    );
}

/// Negative control: a handle-keyed arm whose body mentions only
/// `ObjectProperty` (the permitted indirection this whole module is built
/// on) must NOT trip the scanner — otherwise it would also condemn
/// `object_registry` itself.
#[test]
fn the_table_scanner_does_not_false_positive_on_property_indirection() {
    let legitimate = "match kind {\n    \
                       kinds::BED => ObjectProperty::SupportsRest,\n    \
                       _ => ObjectProperty::HoldsLiquid,\n\
                       }";
    assert!(!handle_arm_mentions_offered_verb(legitimate));
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
        !handle_arm_mentions_offered_verb(src),
        "affordance.rs maps a kinds:: handle to an OfferedVerb through a \
         match arm: that is the verb x object table the acceptance test \
         forbids"
    );
}

/// Positive control for the thing-kind reading: the shape a hardcoded table
/// would take now that the property table is keyed on `KindId`. Without
/// this, `no_thing_kind_keyed_verb_table_exists` below could be green
/// because the scanner never fires rather than because the shape is absent.
#[test]
fn the_table_scanner_catches_a_thing_kind_keyed_arm() {
    let table = "match kind {\n    \
                  KindId(\"bed\") => [OfferedVerb::Sleep, OfferedVerb::Examine].into_iter().collect(),\n    \
                  _ => BTreeSet::new(),\n\
                  }";
    assert!(
        thing_kind_arm_mentions_offered_verb(table),
        "positive control: a KindId-keyed verb x object arm must be caught"
    );
}

/// Negative control: a `KindId`-keyed arm whose body mentions only
/// `ObjectProperty` — the permitted key-to-PROPERTY direction — must not
/// trip the scan, or acceptance clause (4) would condemn the indirection the
/// whole design is built on.
///
/// **This fixture is a shape, not a transcription of `object_registry`, and
/// the sentence it replaces got that wrong** (fix round 1, m6).
/// `object_registry` (`src/affordance.rs`) is an array of
/// `(KindId, ObjectTraits)` tuples fed to `.collect()`, with no `match` and
/// no arms at all — the scanner never reaches an arm for it, so calling this
/// "the legitimate indirection `object_registry` itself is written in" named
/// the wrong production shape. The control still discriminates and is worth
/// keeping: it is the arm-shaped form the same key-to-property mapping would
/// take if anyone wrote it as a `match`, which is precisely the case a
/// scanner keyed on `KindId(` could plausibly over-condemn.
#[test]
fn the_table_scanner_does_not_false_positive_on_thing_kind_property_rows() {
    let legitimate = "match kind {\n    \
                       KindId(\"bed\") => ObjectProperty::SupportsRest,\n    \
                       _ => ObjectProperty::HoldsLiquid,\n\
                       }";
    assert!(!thing_kind_arm_mentions_offered_verb(legitimate));
}

/// Acceptance test (4), the half Task 7's re-key made necessary: no
/// **thing-kind**-keyed verb table exists in `affordance.rs` either.
///
/// `no_verb_by_object_table_exists` above scans for `kinds::`-keyed
/// arms, which was the whole of the forbidden shape while the property table
/// was keyed on that enum. It no longer is, so this is the same structural
/// claim over the key the table actually uses. Stated so it cannot be
/// over-read: this shares every blind spot its sibling discloses (one file,
/// one syntactic shape, nothing reached through a helper or an alias), and
/// adds one of its own — **it sees only the spelling `KindId(`, not every
/// table keyed on a thing-kind.**
///
/// That sentence is a correction, and the wording it replaces is why the
/// correction is loud rather than a quiet edit. This doc used to name the
/// blind spot as "a table keyed on a `KindId` held in a variable rather than
/// spelled as a literal", which a reader takes as the boundary — and it is
/// far too narrow. `KindId` is `pub struct KindId(pub &'static str)`, so
/// `match kind.0 { "bed" => ... }` is a table keyed on a thing-kind, spelled
/// as a LITERAL, that this scan cannot see. A reviewer put exactly that in
/// production and it was reproduced here: `254 passed; 0 failed` for the
/// whole suite with this test reporting `ok`. What closes it is
/// `no_kind_keyed_dispatch_names_a_verb` below, which asks a question that
/// never mentions the key at all. This scan is kept alongside it because it
/// reads the WHOLE file rather than only the dispatch bodies — coverage the
/// key-agnostic guard does not have.
///
/// MUTATION THIS MUST FAIL AGAINST, and it is the evidence that the gap was
/// real rather than theoretical: replace `offered_by`'s body with the
/// forbidden table itself —
///
/// ```ignore
/// match kind {
///     KindId("bed") => [OfferedVerb::Sleep, OfferedVerb::Examine]
///         .into_iter()
///         .collect(),
///     _ => BTreeSet::new(),
/// }
/// ```
///
/// Observed: this test FAILED and **`no_verb_by_object_table_exists` passed**
/// — a live, hardcoded verb x object table sitting in production while the
/// guard decision 0350 names for it reported green. (Five behavioural tests
/// also reddened, because that mutation breaks the answers too; a subtler
/// table that agreed with the registry on every row would have left only
/// this one.)
///
/// ```text
/// test affordance::no_thing_kind_keyed_verb_table_exists ... FAILED
/// test affordance::no_verb_by_object_table_exists ... ok
/// thread 'affordance::no_thing_kind_keyed_verb_table_exists' panicked at
/// windows/vessel/tests/suite/affordance.rs:
/// affordance.rs maps a KindId literal to an OfferedVerb through a match arm ...
/// ```
#[test]
fn no_thing_kind_keyed_verb_table_exists() {
    let src = include_str!("../../src/affordance.rs");
    assert!(
        !thing_kind_arm_mentions_offered_verb(src),
        "affordance.rs maps a KindId literal to an OfferedVerb through a \
         match arm: that is the verb x object table the acceptance test \
         forbids, in the spelling Task 7's re-key made natural"
    );
}

// --- The key-agnostic guard (The Chattel, Task 7 fix round 1) -----------

/// A copy of `src` byte-for-byte the same length, with every line comment's
/// text and every string literal's CONTENTS replaced by spaces.
///
/// Both blanks are load-bearing rather than tidiness, and each was checked
/// against the real file before being written:
///
/// - **Comments**: `affordance.rs` spells `fn hearth_here(i: &Interior) ->
///   bool { ... }` inside a doc comment (see
///   `no_hardcoded_anchor_kind_gates_warm`'s own doc), so a signature walk
///   over the raw bytes would try to parse prose as a function. Blanking
///   the comment removes it from the walk without moving any other byte's
///   offset.
/// - **String contents**: `affordance.rs` contains `panic!("{kind:?} has no
///   ObjectTraits")` and three sibling format strings, each carrying an
///   UNBALANCED-looking `{`/`}` pair inside quotes. A brace balance that
///   counted those would end a function body in the wrong place. It would
///   also see the identifier `kind` inside `{kind:?}`, which is exactly the
///   token the dispatch selector below keys on.
///
/// Length is preserved (spaces in, spaces out) so the blanked copy and the
/// original agree on every byte offset — the messages below quote the
/// blanked text deliberately, since that is what the scan actually read.
///
/// **What this does NOT handle, stated so the guard is not over-read**:
/// block comments (`/* */`) and character literals (`'x'`). Neither appears
/// in `affordance.rs` today — verified by `grep -n '/\*'` and
/// `grep -nE "'[^ ]'"`, both empty — and a character literal cannot be
/// distinguished from a lifetime (`&'static str`, which the file does have)
/// without real lexing. `the_dispatch_scan_reports_the_functions_it_found`
/// below is the tripwire for that: if either construct arrives and breaks
/// the walk, the roster it prints stops containing `offered_by` and the
/// test fails loudly rather than quietly scanning nothing.
fn blank_comments_and_strings(src: &[u8]) -> Vec<u8> {
    let mut out = src.to_vec();
    let mut i = 0usize;
    while i < out.len() {
        if out[i] == b'/' && i + 1 < out.len() && out[i + 1] == b'/' {
            while i < out.len() && out[i] != b'\n' {
                out[i] = b' ';
                i += 1;
            }
        } else if out[i] == b'"' {
            i += 1;
            while i < out.len() && out[i] != b'"' {
                if out[i] == b'\\' && i + 1 < out.len() {
                    out[i] = b' ';
                    i += 1;
                }
                out[i] = b' ';
                i += 1;
            }
            i += 1;
        } else {
            i += 1;
        }
    }
    out
}

/// Whether `body` names a specific [`OfferedVerb`] VARIANT — the literal
/// text `OfferedVerb::` immediately followed by an uppercase ASCII letter.
///
/// The uppercase test is what separates naming a verb (`OfferedVerb::Sleep`)
/// from asking the enum for its own roster (`OfferedVerb::all()`), and the
/// distinction is the difference between the forbidden shape and the
/// permitted one: a dispatch that filters `OfferedVerb::all()` by a
/// property predicate is exactly the design decision 0350 mandates, while a
/// dispatch that spells `Sleep` has decided which verb a kind gets.
fn names_an_offered_verb_variant(body: &[u8]) -> bool {
    let mut cursor = 0usize;
    while let Some(rel) = find_bytes(&body[cursor..], b"OfferedVerb::") {
        let after = cursor + rel + b"OfferedVerb::".len();
        if body.get(after).is_some_and(u8::is_ascii_uppercase) {
            return true;
        }
        cursor = after;
    }
    false
}

/// Every kind → verbs dispatch function in `src`, as `(name, body)` pairs:
/// each `fn` whose return type is `BTreeSet<OfferedVerb>` AND whose
/// parameter list names an object kind (the identifier `kind`, or the type
/// `KindId`). `src` must already have been through
/// [`blank_comments_and_strings`].
///
/// **This selector is the whole point of the repair, so the reasoning for
/// its two halves is here rather than in the test.** Decision 0350 forbids a
/// verb × object table: a function that is handed an object's IDENTITY and
/// answers with VERBS must derive that answer from properties, never decide
/// it per kind. Both halves of the selector are that sentence:
///
/// - the return type `BTreeSet<OfferedVerb>` is "answers with verbs";
/// - a kind-shaped parameter is "handed an object's identity".
///
/// `offered(traits: &ObjectTraits) -> BTreeSet<OfferedVerb>` is deliberately
/// OUT of scope — it is handed properties, not an identity, so a verb it
/// names is keyed on the property vocabulary rather than on which object it
/// is. That is the bound the fix round required be expressible against the
/// real source, and it is: the selector never has to enumerate it, because
/// `&ObjectTraits` names no kind. Today the file's dispatch surface is
/// exactly `offered_by`, `offered_to`, `offered_to_observer`, and a fourth
/// one added tomorrow is covered without editing anything here.
fn kind_to_verb_dispatch_bodies(src: &[u8]) -> Vec<(String, Vec<u8>)> {
    let mut out = Vec::new();
    let mut cursor = 0usize;
    while let Some(rel) = find_bytes(&src[cursor..], b"fn ") {
        let at = cursor + rel;
        cursor = at + 3;
        if at > 0 && (src[at - 1].is_ascii_alphanumeric() || src[at - 1] == b'_') {
            continue;
        }
        let name_start = at + 3;
        let mut i = name_start;
        while i < src.len() && (src[i].is_ascii_alphanumeric() || src[i] == b'_') {
            i += 1;
        }
        if i == name_start || src.get(i) != Some(&b'(') {
            continue;
        }
        let name = String::from_utf8_lossy(&src[name_start..i]).into_owned();
        let mut depth: i32 = 0;
        let mut j = i;
        let mut params_end = None;
        while j < src.len() {
            match src[j] {
                b'(' => depth += 1,
                b')' => {
                    depth -= 1;
                    if depth == 0 {
                        params_end = Some(j);
                        break;
                    }
                }
                _ => {}
            }
            j += 1;
        }
        let Some(params_end) = params_end else {
            continue;
        };
        let Some(brace_rel) = src[params_end..].iter().position(|&b| b == b'{') else {
            continue;
        };
        let returns_verbs = find_bytes(
            &src[params_end + 1..params_end + brace_rel],
            b"BTreeSet<OfferedVerb>",
        )
        .is_some();
        let params = &src[i + 1..params_end];
        let names_a_kind =
            find_bytes(params, b"kind").is_some() || find_bytes(params, b"KindId").is_some();
        if !returns_verbs || !names_a_kind {
            continue;
        }
        if let Some(body) = balanced_block(src, params_end + brace_rel) {
            out.push((name, body.to_vec()));
        }
    }
    out
}

/// **Acceptance clause (4), key-agnostically: no kind → verbs dispatch
/// function in `affordance.rs` may name an `OfferedVerb` variant in its own
/// body.**
///
/// This is the repair fix round 1 asked for, and it replaces enumerating
/// spellings rather than adding a third one to the pile. The two scans above
/// it (`no_verb_by_object_table_exists`,
/// `no_thing_kind_keyed_verb_table_exists`) look for a match arm keyed on a
/// PARTICULAR spelling of the object key — `kinds::`, then `KindId(`.
/// A reviewer defeated both at once with a third spelling that is neither:
///
/// ```ignore
/// pub fn offered_by(kind: KindId) -> BTreeSet<OfferedVerb> {
///     match kind.0 {
///         "bed" => [OfferedVerb::Sleep, OfferedVerb::Examine]
///             .into_iter()
///             .collect(),
///         _ => { /* the legitimate registry path */ }
///     }
/// }
/// ```
///
/// `KindId` is `pub struct KindId(pub &'static str)`, so its inner `&str` is
/// a match scrutinee too, and neither marker appears anywhere in that arm.
/// **Independently reproduced before this guard was written**: that exact
/// body in production gave `254 passed; 0 failed` for the whole
/// `hornvale-vessel` suite, with `no_verb_by_object_table_exists ... ok` and
/// `no_thing_kind_keyed_verb_table_exists ... ok`. A fourth spelling
/// (`kind.0.as_bytes()`, a `matches!`, a `HashMap`-free `if` chain) would
/// have needed a fourth marker, and the marker list is unbounded.
///
/// So this asks a question that does not mention the key at all: the
/// selector is a TYPE shape (kind in, verbs out — see
/// [`kind_to_verb_dispatch_bodies`]) and the forbidden text is the VERB
/// side. Every key spelling, present and future, is covered by construction,
/// because none of them is looked at.
///
/// **Why no behavioural test can do this job, which is why a source scan is
/// the right instrument rather than a fallback.** The reviewer's table
/// AGREES with the registry — `bed`'s registry answer is precisely
/// `{Sleep, Examine}` — so it moves no output whatsoever. No behavioural
/// check, however total, can observe a function that returns the same values
/// by a worse route. Decision 0350's guard is necessarily structural.
///
/// **The honest boundary, stated so it cannot be over-read.** A scan over
/// source text is still a scan over source text, and this one is narrower
/// than "no verb × object table exists anywhere":
///
/// - It reads exactly `windows/vessel/src/affordance.rs`. A table in another
///   file is invisible — the live in-tree instance
///   `no_hardcoded_anchor_kind_gates_warm` names, `interior/field.rs`'s
///   `warmth_at`, still is.
/// - It reads exactly the dispatch functions' own bodies. **A per-kind
///   decision extracted into a helper called from the arm defeats it** —
///   `fn verbs_for(kind: KindId) -> Vec<OfferedVerb>` returns a `Vec`, not a
///   `BTreeSet`, so the selector does not reach it and the dispatch body
///   names no variant. This is the same evasion a re-reviewer built against
///   `no_hardcoded_anchor_kind_gates_warm`, and it is unfixed for the same
///   reason: following a call needs a compiler, not a scanner.
/// - It reads exactly the text `OfferedVerb::<uppercase>`. A dispatch that
///   selected verbs by comparing `v.word()` against a per-kind string, or
///   through a re-exported alias that never spells `OfferedVerb`, names no
///   variant and passes.
///
/// What it DOES buy over the two marker scans is that none of those three
/// residual holes is *key-shaped*: inventing a new way to spell the object
/// key — the thing Task 7's re-key did, and the thing that silently narrowed
/// this guard once already — can no longer open a hole at all.
///
/// MUTATIONS THIS MUST FAIL AGAINST — one per key spelling, each run against
/// PRODUCTION source, each restored and re-run afterwards. See the fix
/// round's report for the full transcripts.
///
/// ```text
/// (1) `match kind.0 { "bed" => [OfferedVerb::Sleep, OfferedVerb::Examine] ... }`
///     in `offered_by` — the spelling BOTH marker scans miss:
/// test affordance::no_kind_keyed_dispatch_names_a_verb ... FAILED
/// test affordance::no_thing_kind_keyed_verb_table_exists ... ok
/// test affordance::no_verb_by_object_table_exists ... ok
///
/// (2) `match kind { KindId("bed") => [OfferedVerb::Sleep, ...] ... }`
///     in `offered_by` — the Task 7 spelling:
/// test affordance::no_kind_keyed_dispatch_names_a_verb ... FAILED
/// test affordance::no_thing_kind_keyed_verb_table_exists ... FAILED
///
/// (3) `match kind { kinds::BED => [OfferedVerb::Sleep, ...] ... }`
///     in `offered_to_observer` — The Offer's original spelling:
/// test affordance::no_kind_keyed_dispatch_names_a_verb ... FAILED
/// test affordance::no_verb_by_object_table_exists ... FAILED
/// ```
#[test]
fn no_kind_keyed_dispatch_names_a_verb() {
    let src = include_str!("../../src/affordance.rs");
    let blanked = blank_comments_and_strings(src.as_bytes());
    for (name, body) in kind_to_verb_dispatch_bodies(&blanked) {
        assert!(
            !names_an_offered_verb_variant(&body),
            "affordance.rs's `{name}` is handed an object kind and answers \
             with OfferedVerbs, yet names a specific verb variant in its own \
             body: that is the verb x object table decision 0350 forbids, in \
             whatever spelling the key happens to take. Body read: {:?}",
            std::str::from_utf8(&body).unwrap_or("<non-utf8>")
        );
    }
}

/// **Vacuity guard for the test above, and the tripwire for
/// [`blank_comments_and_strings`]'s two disclosed gaps.** A signature walk
/// that finds nothing asserts nothing, and would go green forever — the
/// exact failure mode the whole fix round is about. So the roster the
/// selector discovers is asserted directly.
///
/// Frozen as an EXACT set rather than a floor. A dispatch function that
/// disappears is what makes the guard above vacuous; a dispatch function
/// that ARRIVES is a new place a table can live, and a human should be told
/// that it is now covered rather than have it happen silently. Either
/// direction reddens here.
///
/// MUTATION THIS MUST FAIL AGAINST, and it is the evidence that this test is
/// load-bearing rather than decorative: break the selector's return-type
/// needle (`b"BTreeSet<OfferedVerb>"` ->
/// `b"BTreeSet<OfferedVerbNeverMatches>"`) so it discovers nothing. The
/// guard above then passes **vacuously** — it iterates an empty roster —
/// while this one reds:
///
/// ```text
/// test affordance::no_kind_keyed_dispatch_names_a_verb ... ok
/// test affordance::the_dispatch_scan_reports_the_functions_it_found ... FAILED
/// assertion `left == right` failed: the kind -> verbs dispatch roster moved ...
///   left: []
///  right: ["offered_by", "offered_to", "offered_to_observer"]
/// ```
///
/// That `ok` on the line above the `FAILED` is the whole point: a scanner
/// that stops finding its subject reports success, and nothing but this
/// assertion distinguishes that from a clean tree.
#[test]
fn the_dispatch_scan_reports_the_functions_it_found() {
    let src = include_str!("../../src/affordance.rs");
    let blanked = blank_comments_and_strings(src.as_bytes());
    let found: Vec<String> = kind_to_verb_dispatch_bodies(&blanked)
        .into_iter()
        .map(|(name, _)| name)
        .collect();
    assert_eq!(
        found,
        vec![
            // `weft_offers` (The Weft, Task 8) scans FIRST: it is defined
            // earlier in `affordance.rs` than `offered_by`, right after
            // `object_registry` — the scan walks the file in source order,
            // not alphabetically. It carries the SAME "kind -> verbs"
            // shape (`weft_object_registry` then `offered`, no verb
            // variant named in its own body), which is exactly why it is
            // a legitimate arrival here rather than a defect: a second
            // key SPACE reusing the one query, never a second dispatch
            // table.
            "weft_offers".to_string(),
            "offered_by".to_string(),
            "offered_to".to_string(),
            "offered_to_observer".to_string(),
        ],
        "the kind -> verbs dispatch roster moved: no_kind_keyed_dispatch_\
         names_a_verb covers exactly these bodies, so a name leaving this \
         list is coverage lost and a name arriving is coverage gained"
    );
}

/// Positive control (1): the `kind.0` spelling, the one that defeated both
/// marker scans, on a synthetic dispatch function.
#[test]
fn the_dispatch_scan_catches_an_inner_str_keyed_table() {
    let src = b"pub fn offered_by(kind: KindId) -> BTreeSet<OfferedVerb> {\n    \
                 match kind.0 {\n        \
                 \"bed\" => [OfferedVerb::Sleep, OfferedVerb::Examine].into_iter().collect(),\n        \
                 _ => BTreeSet::new(),\n    \
                 }\n\
                 }";
    let blanked = blank_comments_and_strings(src);
    let found = kind_to_verb_dispatch_bodies(&blanked);
    assert_eq!(found.len(), 1, "the selector must reach offered_by");
    assert!(names_an_offered_verb_variant(&found[0].1));
}

/// Positive control (2): the `KindId(` spelling.
#[test]
fn the_dispatch_scan_catches_a_thing_kind_keyed_table() {
    let src = b"pub fn offered_by(kind: KindId) -> BTreeSet<OfferedVerb> {\n    \
                 match kind {\n        \
                 KindId(\"bed\") => [OfferedVerb::Sleep].into_iter().collect(),\n        \
                 _ => BTreeSet::new(),\n    \
                 }\n\
                 }";
    let blanked = blank_comments_and_strings(src);
    let found = kind_to_verb_dispatch_bodies(&blanked);
    assert_eq!(found.len(), 1);
    assert!(names_an_offered_verb_variant(&found[0].1));
}

/// Positive control (3): a MULTI-LINE signature.
///
/// **This doc has been corrected twice by campaigns deleting its subject,
/// and the second correction is the reason it is now about the signature
/// rather than about a spelling.** It read "on the one dispatch function
/// that still speaks that key (`offered_to_observer`)" until Task 9 of The
/// Chattel made that false; it then read that the fixture kept the scan's
/// coverage of the deleted enum's spelling alive after production stopped
/// containing one. The Wicket deleted the type outright, so a fixture
/// naming it would be a synthetic source sample of a construct that cannot
/// be written — coverage of nothing.
///
/// What the fixture proves, and what the sibling controls do not, is that
/// [`kind_to_verb_dispatch_bodies`] walks a signature broken across lines
/// before reaching the return type. That is a property of the scanner's
/// parser rather than of any key spelling, so it survives every future
/// re-key.
///
/// **Renamed at The Wicket's close, which is the third correction to this
/// same fixture and the one its own doc had already argued for.** It was
/// `the_dispatch_scan_catches_an_anchor_kind_keyed_table`: a name promising
/// coverage of a KEY SPELLING for a type this campaign deleted, on a fixture
/// whose body has never keyed on that type and whose doc, two paragraphs up,
/// says the property is the multi-line SIGNATURE. Task 2 deferred the rename
/// on the belief that touching a test name meant hand-editing
/// `docs/timings/subfloor-roster.tsv`; that file is an ordinary chamber
/// artifact the `gate` phase rewrites, and a name missing from it only means
/// `gate-commit` skips this one test until the next green run.
/// **`docs/decisions/0397` still names the old identifier** — decision
/// records are append-only, so that citation stays as the dated record it is.
#[test]
fn the_dispatch_scan_walks_a_multi_line_signature() {
    let src = b"pub fn offered_to_observer(\n    \
                 kind: KindId,\n    \
                 body: &Body,\n\
                 ) -> BTreeSet<OfferedVerb> {\n    \
                 match kind.0 {\n        \
                 \"bed\" => [OfferedVerb::Sleep].into_iter().collect(),\n        \
                 _ => BTreeSet::new(),\n    \
                 }\n\
                 }";
    let blanked = blank_comments_and_strings(src);
    let found = kind_to_verb_dispatch_bodies(&blanked);
    assert_eq!(
        found.len(),
        1,
        "a multi-line signature must still be walked"
    );
    assert!(names_an_offered_verb_variant(&found[0].1));
}

/// Negative control: the production shape itself — a dispatch that reads the
/// registry and hands the traits to `offered` — must not fire, or the guard
/// would condemn the design it exists to protect. Distinct from the live
/// test above in that it is a fixture: it discriminates the SHAPE, and would
/// still discriminate it if `affordance.rs` were empty.
#[test]
fn the_dispatch_scan_does_not_fire_on_the_registry_indirection() {
    let src = b"pub fn offered_by(kind: KindId) -> BTreeSet<OfferedVerb> {\n    \
                 let reg = object_registry();\n    \
                 let traits = reg.get(&kind).cloned().unwrap_or_default();\n    \
                 offered(&traits)\n\
                 }";
    let blanked = blank_comments_and_strings(src);
    let found = kind_to_verb_dispatch_bodies(&blanked);
    assert_eq!(found.len(), 1, "the selector must reach offered_by");
    assert!(!names_an_offered_verb_variant(&found[0].1));
}

/// Negative control: asking the enum for its own roster is the PERMITTED
/// shape and must not fire. This is the control that gives
/// [`names_an_offered_verb_variant`]'s uppercase test its meaning — without
/// it, "contains `OfferedVerb::`" would look like an equally good rule while
/// forbidding the property-filter design decision 0350 mandates.
#[test]
fn the_dispatch_scan_does_not_fire_on_the_verb_roster_call() {
    let src = b"pub fn offered_by(kind: KindId) -> BTreeSet<OfferedVerb> {\n    \
                 let traits = object_registry().get(&kind).cloned().unwrap_or_default();\n    \
                 OfferedVerb::all().into_iter().filter(|v| \
                 required_properties(*v).is_subset(&traits.properties)).collect()\n\
                 }";
    let blanked = blank_comments_and_strings(src);
    let found = kind_to_verb_dispatch_bodies(&blanked);
    assert_eq!(found.len(), 1);
    assert!(
        !names_an_offered_verb_variant(&found[0].1),
        "OfferedVerb::all() is the roster call, not a named variant"
    );
}

/// Scoping control: a PROPERTY-keyed query is out of the selector's scope,
/// which is what lets `offered` name verbs freely without this guard having
/// an opinion. Asserts the bound the fix round asked be made expressible —
/// and asserts it by construction rather than by an exclusion list, since
/// `&ObjectTraits` simply names no kind.
#[test]
fn the_dispatch_scan_does_not_select_a_property_keyed_query() {
    let src = b"pub fn offered(traits: &ObjectTraits) -> BTreeSet<OfferedVerb> {\n    \
                 [OfferedVerb::Sleep].into_iter().collect()\n\
                 }";
    let blanked = blank_comments_and_strings(src);
    assert!(
        kind_to_verb_dispatch_bodies(&blanked).is_empty(),
        "a query handed properties rather than an object identity is not a \
         verb x object table, however many verbs it names"
    );
}

/// Control for [`blank_comments_and_strings`]: a verb named only inside a
/// doc comment or a string literal must not fire, and — the half that
/// actually bites — an unbalanced brace inside a format string must not end
/// the body early. `affordance.rs` really does contain `panic!("{kind:?} has
/// no ObjectTraits")`, so this is the production hazard, not a hypothetical.
#[test]
fn blanking_survives_comments_and_format_strings() {
    let src = b"pub fn offered_by(kind: KindId) -> BTreeSet<OfferedVerb> {\n    \
                 // OfferedVerb::Sleep mentioned in a comment\n    \
                 let t = reg.get(&kind).unwrap_or_else(|| panic!(\"{kind:?} }} OfferedVerb::Drink\"));\n    \
                 offered(&t)\n\
                 }\nfn later() {}";
    let blanked = blank_comments_and_strings(src);
    let found = kind_to_verb_dispatch_bodies(&blanked);
    assert_eq!(found.len(), 1);
    assert!(
        !names_an_offered_verb_variant(&found[0].1),
        "a verb named in a comment or a string is not a dispatch decision"
    );
    assert!(
        find_bytes(&found[0].1, b"offered(&t)").is_some(),
        "the unbalanced brace inside the format string must not have ended \
         the body before its last real statement: {:?}",
        std::str::from_utf8(&found[0].1).unwrap_or("<non-utf8>")
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
        offered_to(kinds::BED, &small),
        offered_to(kinds::BED, &large),
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
    kinds.push(kinds::SCREEN);

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
        offered_to_observer(kinds::HEARTH, &body, &empty).is_empty(),
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
        offered_to_observer(kinds::HEARTH, &body, &known),
        offered_to(kinds::HEARTH, &body),
        "knowledge of an encountered room must withdraw nothing"
    );
}

// --- Task 9: the gate denies a PASSAGE (spec §3.6, decision 0397) --------

/// **The knowledge gate denies a passage — the thing decision 0369 recorded
/// as unreachable, and the reason acceptance criterion 4 is claimable.**
///
/// The gate has denied a *hearth* since The Offer
/// (`an_unencountered_object_offers_nothing`, above). What it could never
/// deny is a PASSAGE, and 0369 is precise about why: `offered_to_observer`
/// took an anchor-kind variant, that enum had no
/// cave-mouth variant, and a cave mouth is addressed by a
/// `Vertex`/`ChamberAddr`. **The obstacle was addressing, not durability**
/// — so the remedy is not more state, it is a different key, which is what
/// Task 9's re-key to [`KindId`] is.
///
/// **This test carried a precondition loop making that concrete, and The
/// Wicket's Task 2 retired it** — see the comment in the body. It swept the
/// anchor-kind enum asserting no variant mapped to `cave-mouth`, which was
/// the fact that made the pre-re-key call unwritable. With the enum deleted
/// there is no closed variant set left to be unreachable through, so the
/// loop could only ever have passed.
///
/// **MUTATION THIS MUST FAIL AGAINST**: make `offered_to_observer` ignore
/// its `known` argument and delegate straight to `offered_to` — the same
/// mutation `an_unencountered_object_offers_nothing` is proven against, run
/// again because a re-keyed function is a rewritten function and the old
/// red does not transfer. Applied with `scripts/mutate.py` to production
/// source, restored from a `cp` backup, and re-run green afterwards. Red
/// observed:
///
/// ```text
/// FAIL [   0.007s] (1/1) hornvale-vessel::suite affordance::an_unencountered_passage_offers_nothing
/// thread 'affordance::an_unencountered_passage_offers_nothing' panicked at
/// windows/vessel/tests/suite/affordance.rs:
/// a cave mouth offers {Enter, Examine} to a body that has encountered no
/// room: the knowledge gate did not deny a passage
/// ```
///
/// **That run reddens THREE tests, not one, and the count was wrong here
/// until fix round 1 because the run behind it was FILTERED.** The figure
/// this paragraph used to give — "TWO tests … `41 tests run: 39 passed, 2
/// failed`" — is a `-E 'test(affordance)'` selection, taken in the same
/// commit in which the sibling mutation below was self-corrected to an
/// unfiltered run, one doc comment away. Re-taken unfiltered over the whole
/// crate:
///
/// ```text
/// FAIL [   4.541s] (436/836) hornvale-vessel session::tests::examine_chamber_anchor_is_refused_when_the_observer_has_no_recorded_knowledge
/// FAIL [   0.025s] (568/836) hornvale-vessel::suite affordance::an_unencountered_object_offers_nothing
/// FAIL [   0.018s] (569/836) hornvale-vessel::suite affordance::an_unencountered_passage_offers_nothing
///      Summary [ 174.236s] 836 tests run: 833 passed, 3 failed, 3 skipped
/// ```
///
/// Stated rather than trimmed because it is the honest shape of the evidence:
/// the mutation kills the gate for every kind, the hearth test was already
/// holding it for anchors, and `session.rs`'s own test was already holding it
/// through a real `Session::examine_chamber`. What this test adds is the half
/// no existing test could reach — the same kill against a kind with no
/// anchor-kind variant behind it.
///
/// **The residual, stated here because this is the first file a successor
/// grepping for acceptance criterion 4 lands in** (fix round 1, m2; the same
/// sentence decision 0397 clause 3 carries). The addressing half is
/// discharged; the *live-reachability* half is not. `Session::new`'s
/// `absorb_here` is unconditional, so no session reaches an empty
/// `Knowledge` on its own, and no production caller passes
/// `KindId("cave-mouth")` at all — chamber entry gates on the cave mouth's
/// `openness` fold (0396), not on this query. The denial above is observed
/// with a synthetic `Knowledge::default()`: a real and reachable state of the
/// type, not one today's callers produce.
///
/// That `{Enter, Examine}` in the message is the whole point — it is a
/// non-empty offer, so this test cannot be satisfied by a `cave-mouth` row
/// that carries nothing. `an_encountered_passage_offers_its_verbs` below
/// pins the same set from the other direction, so the pair cannot both be
/// satisfied by a gate that always denies.
#[test]
fn an_unencountered_passage_offers_nothing() {
    let cave_mouth = KindId(hornvale_vessel::passage::CAVE_MOUTH);
    // THIS TEST CARRIED A PRECONDITION LOOP UNTIL THE WICKET, AND ITS
    // SUBJECT IS GONE RATHER THAN ITS CLAIM. It swept every anchor-kind
    // variant asserting that none mapped to `cave-mouth` — the fact that
    // made "a passage cannot be named in the query's currency" true before
    // Task 9 of The Chattel, and the reason THAT task's re-key was the
    // deliverable rather than a tidy-up. The Wicket deleted the enum, so
    // there is no closed variant set left to be unreachable through, and a
    // loop asserting one would be a check that can never fire. The claim
    // below is unchanged and is the one that was always being made: an
    // observer who has encountered no room is offered nothing, including
    // for a kind no room's grammar composes.
    let body = body_with_mass("human", REFERENCE_MASS_KG);
    let empty = Knowledge::default();
    let offered = offered_to_observer(cave_mouth, &body, &empty);
    assert!(
        offered.is_empty(),
        "a cave mouth offers {offered:?} to a body that has encountered no \
         room: the knowledge gate did not deny a passage"
    );
}

/// Vacuity control for the denial above, and the half that makes the pair a
/// claim about the GATE rather than about `cave-mouth` being empty: the same
/// passage offers its real verbs to a body that has encountered a room.
///
/// Asserted as the SPECIFIC set — `{Enter, Examine}`, which `AffordsPassage`
/// (spec §3.7) and `Examine`'s universality earn — rather than as
/// `!is_empty()`, for the reason `an_encountered_object_offers_its_verbs`
/// gives: non-emptiness is satisfiable by a filter that drops verbs
/// arbitrarily. Compared against `offered_to` rather than a hand-written
/// literal, so a registry change moves both sides together and this stays a
/// claim about the gate withdrawing nothing.
///
/// MUTATION THIS MUST FAIL AGAINST: drop `ObjectProperty::AffordsPassage`
/// from `cave-mouth`'s row in `object_registry`, which is what would make
/// the denial above vacuous. Red observed:
///
/// ```text
/// FAIL [   0.008s] (1/1) hornvale-vessel::suite affordance::an_encountered_passage_offers_its_verbs
/// thread 'affordance::an_encountered_passage_offers_its_verbs' panicked at
/// windows/vessel/tests/suite/affordance.rs:
/// a cave mouth must offer Enter to a body that has encountered a room, or
/// `an_unencountered_passage_offers_nothing` is satisfied by an empty row
/// rather than by the gate: {Examine}
/// ```
///
/// `{Examine}`, not `{}`, is why this is the right mutation: dropping
/// `AffordsPassage` leaves the row present and universal-`Examine` intact,
/// so the denial test above stays green while it has stopped being a claim
/// about a PASSAGE.
///
/// **Two tests failed on that run, not one, and the second is the reason
/// this doc says so instead of claiming isolation it does not have.** The
/// full `hornvale-vessel` suite reported `834 tests run: 832 passed, 2
/// failed`; the other was `affordance::tests::the_certain_carriers_named_
/// by_the_spec_carry_their_property`, the in-module assertion over the
/// carriers spec §3.7/§3.8 names outright. That is the correct shape — a
/// registry row the spec names is pinned in two places on purpose — and it
/// is stated because a first, narrower run of this same mutation filtered
/// to ONE test name and could not have seen it.
#[test]
fn an_encountered_passage_offers_its_verbs() {
    let cave_mouth = KindId(hornvale_vessel::passage::CAVE_MOUTH);
    let body = body_with_mass("human", REFERENCE_MASS_KG);
    let known = Knowledge(std::collections::BTreeMap::from([(
        "room/1".to_string(),
        "recorded".to_string(),
    )]));
    let offered = offered_to_observer(cave_mouth, &body, &known);
    assert!(
        offered.contains(&OfferedVerb::Enter),
        "a cave mouth must offer Enter to a body that has encountered a \
         room, or `an_unencountered_passage_offers_nothing` is satisfied by \
         an empty row rather than by the gate: {offered:?}"
    );
    assert_eq!(
        offered,
        offered_to(cave_mouth, &body),
        "knowledge of an encountered room must withdraw nothing from a \
         passage either"
    );
}

// --- Fix wave (final whole-branch review, I1): warm must not reintroduce
// the per-kind coupling this campaign abolishes --------------------------

/// `Warm` is offered to ANY object carrying [`ObjectProperty::RadiatesHeat`]
/// — never to `kinds::HEARTH` because it is named `Hearth`. This is
/// the M+N claim `a_kind_gains_every_verb_its_properties_satisfy_with_no_
/// dispatcher_edit` already proves for `Drink`/`HoldsLiquid`; restated here
/// for `Warm`/`RadiatesHeat` because I1 found the one place in this
/// codebase that did NOT go through this derivation:
/// `Session::warm` (`session.rs`) used to compare `interior.anchor(a).kind
/// == kinds::HEARTH` directly, a hardcoded per-kind check one file
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
         which kind (if any) produced those traits"
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
/// WHOLE file would also match `kinds::HEARTH` in `warm`'s own doc
/// comment and in unrelated methods (`chamber_sources`'s light check,
/// `interior/field.rs`'s `warmth_at`), none of which this test is about.
fn block_body_after<'a>(src: &'a [u8], needle: &[u8]) -> Option<&'a [u8]> {
    let start = find_bytes(src, needle)? + needle.len();
    let open = start + src[start..].iter().position(|&b| b == b'{')?;
    balanced_block(src, open)
}

/// The brace-balanced block of `src` beginning at the `{` at byte `open` —
/// the half of [`block_body_after`] that does not depend on how the block
/// was located, split out so [`kind_to_verb_dispatch_bodies`] (which finds
/// its blocks by walking signatures rather than by a literal needle) can
/// reuse the same balance rather than writing a second copy of it.
fn balanced_block(src: &[u8], open: usize) -> Option<&[u8]> {
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
/// kind literal at all — the shape a per-kind dispatcher edit would take
/// (`interior.anchor(a).kind == kinds::HEARTH`, the exact code this test's
/// own mutation restores).
///
/// **The needle moved from `AnchorKind::` to `kinds::` (The Wicket, Task
/// 2), for the reason `handle_arm_mentions_offered_verb` gives at length:**
/// the enum is deleted, so a scan for `AnchorKind::` would be green in every
/// possible tree — a check that can never fire, which is worse than an
/// absent one. `kinds::HEARTH` is the spelling the mutation would take
/// today, and it is what this reads. A verb gated on the OFFER never
/// needs to name a specific kind; naming one is the M×N coupling spec §3.2
/// exists to abolish, reintroduced here even though every other surface
/// (`examine_chamber`) was fixed cleanly at Task 7.
///
/// **Would NOT have been caught by a plain "warm succeeds at a real
/// hearth" test alone.** `kinds::BED` always co-occurs with
/// `kinds::HEARTH` in every real chamber (`the-fireside-bed` requires
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
/// kinds::HEARTH` — reddens (confirmed in the fix
/// wave's report) while every other `warm`/`examine` test stays green,
/// exactly the I1 finding.
///
/// **The direction this enforces, stated so it cannot be mistaken for a
/// broader guarantee** — the same disclosure
/// `handle_arm_mentions_offered_verb` above carries, for the same
/// reason. This scans exactly the block `block_body_after` isolates for
/// `fn warm(&self) -> Turn {`, for exactly the literal text
/// `kinds::`. **Extracting the gate into a one-line private helper
/// defeats it while it stays green**: `fn hearth_here(i: &Interior) ->
/// bool { i.ids().iter().any(|&a| i.anchor(a).kind ==
/// kinds::HEARTH) }`, called from `warm`, moves the literal out of
/// the block this reads and reintroduces the coupling with no test
/// objecting. A re-reviewer BUILT that evasion and confirmed it. Nor does
/// this see a coupling reached through a re-exported alias, a
/// fully-qualified path that never spells `kinds::` (a bare
/// `KindId("hearth")` is exactly that shape, and is newly writable now the
/// key is an open label), or a gate keyed on something other than a kind. The concrete in-tree instance,
/// the same one the sibling guard names: `interior/field.rs`'s
/// `warmth_at` contains `if interior.anchor(id).kind !=
/// kinds::HEARTH { continue; }` — a live kind-to-behaviour coupling
/// neither guard can see, because it is in another file and mentions no
/// `OfferedVerb`. Acceptance clause (4) is worded against what these two
/// scans actually cover (spec §6, decision 0350), not against "anywhere".
#[test]
fn no_hardcoded_anchor_kind_gates_warm() {
    let src = include_str!("../../src/session.rs");
    let body = block_body_after(src.as_bytes(), b"fn warm(&self) -> Turn {")
        .expect("session.rs must define fn warm(&self) -> Turn");
    assert!(
        find_bytes(body, b"kinds::").is_none(),
        "Session::warm's body names a specific thing-kind handle directly, \
         reintroducing the per-kind coupling spec 3.2 forbids: {:?}",
        std::str::from_utf8(body).unwrap_or("<non-utf8>")
    );
}

// --- The Chattel, Task 7: the re-key onto KindId -----------------------

/// **The guard the re-key itself needed, and the one The Offer's suite could
/// not provide.** Every test above read the table through the anchor-kind →
/// thing-kind mapping, so a mapping arm that sent an anchor kind to the WRONG
/// thing-kind was invisible to all of them whenever the wrong kind happened
/// to carry the right property — `Pool -> KindId("vessel")` passed
/// `a_kind_gains_every_verb_its_properties_satisfy_with_no_dispatcher_edit`
/// unchanged, because a vessel holds liquid too. This freezes the whole
/// per-kind answer instead: what verb set each kind offers, which is exactly
/// what The Offer's table produced before the re-key and what spec §3.6
/// requires the re-key to preserve.
///
/// **The mapping it was written against is gone (The Wicket, Task 2) and the
/// freeze is not.** An anchor carries its `KindId`, so the specific error
/// this guarded — a mis-pointed arm — cannot be made. What CAN be made is a
/// mis-spelled handle, a row silently losing a property, and a subset filter
/// that stops discriminating, and this table catches all three by naming the
/// answer rather than deriving it.
///
/// It is a frozen expectation, not a derivation — deriving it from
/// `object_registry` would re-use the machinery under test and assert
/// nothing. Moving a row here is therefore a deliberate act, the same
/// discipline a byte-golden carries.
///
/// MUTATION THIS WAS PROVEN AGAINST, on the mapping that then stood between
/// the roster and this table: point `thing_kind_of`'s `Pool` arm at
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
/// the evidence: this mutation was caught several ways over.
///
/// **Both the mutation and one of its three witnesses are historical now
/// (The Wicket, Task 2).** The mutation was applied to
/// `affordance::thing_kind_of`, the anchor-kind -> thing-kind match, which no
/// longer exists — an anchor CARRIES a `KindId`, so there is nothing between
/// the roster and this table to mis-point. `the_anchor_to_thing_kind_mapping_
/// is_injective` went with it. The red above is kept as the record of what
/// this table caught while a mapping stood in that position; the table's
/// live job is the agreement below.
///
/// **Also the agreement half for [`every_rostered_kind`] (Task 9), and the
/// agreement still has a side this table cannot follow on its own.** This
/// table is
/// now the ONLY hand-maintained enumeration of kinds in this file:
/// [`every_rostered_kind`] reads `hornvale_thing::THING_KINDS`, the roster
/// itself, rather than a hand-written handle list or an enum's generated
/// declaration. That asymmetry is what makes the first assertion below carry
/// the campaign's add-a-kind property. An added ROSTER ROW lengthens the
/// frozen side and not this one, so it reddens here; a row dropped from
/// this table shortens it against a side that cannot follow, so that reddens
/// here too. Neither direction can be satisfied by both lists going short
/// together, which is exactly how the pre-fix arrangement failed. **The side
/// that cannot follow is not the compiler any more**: `THING_KINDS` is a
/// hand-written slice, where `AnchorKind::ALL` was macro-generated, and what
/// holds it honest is `the_roster_is_frozen_as_an_ordered_set` (G-f) rather
/// than a generator. Say so rather than inheriting the old sentence's
/// confidence.
///
/// (**This paragraph said `EVERY_HANDLE` until the final review**, describing
/// the arrangement Task 2 shipped and Task 3 replaced one task later — the
/// same shape as the count correction on [`every_rostered_kind`]'s own doc,
/// and one screen from it. The helper's NAME carried the stale reading too,
/// and is renamed here: a roster is not a set of names, which is the
/// distinction decisions 0556 and 0557 make constitutional.)
///
/// MUTATION THAT AGREEMENT MUST FAIL AGAINST — run because a guard written
/// in the same commit as the thing it guards is unaudited text: drop
/// `(kinds::LOOM, &[Examine])` from the table below and its length
/// annotation from 14 to 13 (the hand-maintained side; the generated side
/// can no longer be shortened without deleting the variant). **Exactly one
/// test failed, this one.**
///
/// (Those two figures are the annotation as it stood when that run was
/// taken. It is 15 now — Task 11 appended `kinds::KEY` — so the same
/// mutation today is 15 to 14. The transcript below is left verbatim rather
/// than renumbered: it is a record of a run, and a run that was not re-taken
/// must not be made to look as though it had been. What the guard CAUGHT is
/// re-witnessed independently anyway — the append itself reddened this test
/// before the row was added, and that red is pasted in the Task 11 report.)
///
/// **The `41 tests run: 40 passed, 1 failed` figure this paragraph used to
/// cite was a `-E 'test(affordance)'` selection** — the claim it supported
/// was true and the evidence pasted under it could not establish it, since a
/// filtered run says nothing about the 795 tests it did not select (fix
/// round 1, m4). Re-taken unfiltered over the whole crate:
///
/// ```text
/// FAIL [   0.011s] (593/836) hornvale-vessel::suite affordance::the_re_key_preserves_every_anchor_kinds_offer
///      Summary [ 173.431s] 836 tests run: 835 passed, 1 failed, 3 skipped
///
/// assertion `left == right` failed: this table and ALL_ANCHOR_KINDS are the
/// file's only two hand-maintained kind enumerations, ...
///   left: [Hearth, Threshold, Bed, Vessel, Screen, Pool, Log, Ground, Alcove, Strongbox, HighSeat, Anvil, Altar]
///  right: [Hearth, Threshold, Bed, Vessel, Screen, Pool, Log, Ground, Alcove, Strongbox, HighSeat, Loom, Anvil, Altar]
/// ```
///
/// The `left`/`right` sides are the other way round from the citation this
/// replaces, and that is the mutation moving rather than a transcription
/// slip: the shortened list is now the TABLE, which is `left`.
#[test]
fn the_re_key_preserves_every_anchor_kinds_offer() {
    use OfferedVerb::{Close, Drink, Drop, Enter, Examine, Open, Put, Sleep, Take, Warm};
    let expected: [(KindId, &[OfferedVerb]); 22] = [
        // Encloses gates no OfferedVerb (it is read by `examine`'s prose,
        // not by the offer query), so an enclosing kind offers Examine and
        // nothing more.
        (kinds::ALCOVE, &[Examine]),
        (kinds::ALTAR, &[Examine]),
        (kinds::ANVIL, &[Examine]),
        (kinds::BED, &[Sleep, Examine]),
        (kinds::BENCH, &[Examine]),
        (kinds::BRACKEN, &[Sleep, Examine]),
        // THE BRAZIER'S ROW IS NEW (The Wicket, Task 5): the campaign's own
        // proof that a kind can arrive with data rows only. `RadiatesHeat`
        // gates `Warm` the same way `hearth`'s row does; `Examine` is
        // universal.
        (kinds::BRAZIER, &[Examine, Warm]),
        // THE CAVE MOUTH'S ROW IS NEW, AND ITS ARRIVAL IS THE WICKET'S OWN
        // DELIVERABLE SHOWING UP AT THE FROZEN TABLE. This table used to
        // enumerate anchor-kind variants, so `cave-mouth` — a `Vertex`/`ChamberAddr`
        // that no anchor variant could express — was structurally
        // unreachable from it, and its verbs were asserted only by the two
        // passage tests above. With the key an open label the row is simply
        // writable, which is the addressing half of decision 0397 arriving
        // at a place that could not previously hold it.
        (kinds::CAVE_MOUTH, &[Enter, Examine, Open, Close]),
        // THE DOOR'S ROW IS NEW (The Brattice, Task 5, spec §3.7), and it is
        // the cave mouth's row exactly — which is the point. `Lockable` gates
        // nothing in this query, for the reason the strongbox's row states: a
        // lock is a precondition on the ACT, read against the body's custody,
        // and this query holds one object and no body. So a door offers what
        // any openable passage offers, and the lock shows up only in
        // `Session::open_or_close`'s underground arm.
        (kinds::DOOR, &[Enter, Examine, Open, Close]),
        (kinds::GROUND, &[Examine]),
        (kinds::HEARTH, &[Examine, Warm]),
        (kinds::HIGH_SEAT, &[Examine]),
        // THE KEY'S ROW MOVED, AND THIS IS TASK 12 ARRIVING AT THE FROZEN
        // TABLE — the second half of the deliberate move the strongbox's own
        // row demanded. Task 11 wrote it as `&[Examine]` and said in this
        // very comment that `Portable` gated `take`/`drop`, "verbs Task 12
        // ships", so that "Task 12's addition has to move this line
        // deliberately". It did, and nothing in `object_registry` changed to
        // produce it: three new `OfferedVerb`s naming `Portable` in their
        // `required_properties` reach the one carrier through the subset
        // filter alone. That is acceptance clause 7's second direction
        // ("a new verb appears on every qualifying thing with no kind edit")
        // observed a second time, on a different property, by a table that
        // could not have followed on its own.
        //
        // `Put` is here on the same footing as `Take` and `Drop`: the
        // property belongs to the thing being MOVED. What the CONTAINER must
        // be is a precondition on the act read against a second object, the
        // way `Lockable` is for `Open`, and this query holds one object.
        (kinds::KEY, &[Examine, Take, Drop, Put]),
        (kinds::LEDGE, &[Sleep, Examine]),
        (kinds::LOG, &[Examine]),
        (kinds::LOOM, &[Examine]),
        (kinds::POOL, &[Drink, Examine]),
        (kinds::RUSHES, &[Sleep, Examine]),
        (kinds::SCREEN, &[Examine]),
        // THE STRONGBOX'S ROW MOVED, AND THAT IS THE TASK-11 DELIVERABLE
        // ARRIVING AT THE FROZEN TABLE. `Openable` gated no verb until Task
        // 11 shipped `open`/`close`; the property was granted in Task 7 and
        // the row's own comment said the verbs were still to come. They came.
        // Nothing in `object_registry` changed to produce this — a new
        // `OfferedVerb` with `Openable` in its `required_properties` reaches
        // every carrier through the subset filter alone, which is acceptance
        // clause 7's second direction ("a new verb appears on every
        // qualifying thing with no kind edit") observed rather than argued.
        // `Lockable` still gates nothing here and must not: a lock is a
        // precondition on the ACT, read against the body's custody, and this
        // query has no body's custody in it.
        (kinds::STRONGBOX, &[Examine, Open, Close]),
        (kinds::THRESHOLD, &[Enter, Examine]),
        (kinds::VESSEL, &[Drink, Examine]),
    ];
    // The two rosters agree, in both directions and in order. A kind dropped
    // from either enumeration reddens here rather than quietly shrinking
    // what some other test sweeps. The ORDER is `THING_KINDS`', which is
    // alphabetical by label; it was the enum's declaration order until The
    // Wicket deleted the enum, and the rows themselves are unchanged.
    let table_kinds: Vec<KindId> = expected.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        table_kinds,
        every_rostered_kind(),
        "this table and every_rostered_kind() are the file's only two kind \
         enumerations, and only the table is UNFROZEN: a kind added to \
         `hornvale_thing::THING_KINDS` lengthens every_rostered_kind() and \
         not this table, and a row dropped from this table shortens it \
         against a roster the_roster_is_frozen_as_an_ordered_set (G-f) will \
         not let follow"
    );
    for (kind, want) in expected {
        let want: BTreeSet<OfferedVerb> = want.iter().copied().collect();
        let got = offered_by(kind);
        assert_eq!(
            got, want,
            "{kind:?} ({:?}) offers {got:?}, but the pre-re-key table offered {want:?}",
            kind
        );
    }
}

// --- FIVE SOURCE-SCANNING TESTS RETIRED HERE (The Wicket, Task 2) -------
//
// Their SUBJECTS were deleted, not their claims relaxed, and each is named
// so a reader looking for the guarantee finds where it went:
//
// - `the_anchor_to_thing_kind_mapping_is_injective` guarded
//   `affordance::thing_kind_of`, the total anchor-kind -> thing-kind match.
//   An anchor now CARRIES a `KindId`, so there is no mapping to be injective
//   and the frozen verb table above indexes kinds directly.
// - `thing_kind_of_has_no_wildcard_arm` and its positive control
//   `the_wildcard_scan_catches_a_wildcard_arm` scanned that same function's
//   source for a `_ =>` arm.
// - `the_anchor_kind_roster_is_generated_from_the_enums_declaration` and its
//   positive control `the_roster_generation_scan_catches_a_hand_written_
//   roster` scanned `interior/anchor.rs` for the `anchor_kinds!` macro that
//   declared the enum and its roster together. Both are gone with the macro.
//
// WHAT REPLACES THE GUARANTEE, since "the subject left" is only half an
// answer. The enum's roster is now `hornvale_thing::THING_KINDS`, frozen as
// an ORDERED SET by `the_roster_is_frozen_as_an_ordered_set` (G-f) rather
// than generated; the handles this file sweeps are checked against it by
// `every_named_handle_is_a_roster_row` (G-d); and the exhaustiveness the
// wildcard scan protected becomes spec §5.1's G-a/G-b/G-c in Task 3 and
// Task 4. Until those land, this file's frozen table is the only two-way
// check over the kind set, which is why it gained a `cave-mouth` row above
// rather than staying at fifteen.

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

/// Sleeping is NOT gated on an object, and this test exists so it stays that
/// way (decision 0558). `Session::sleep` refuses a non-empty argument, charges
/// the clock, commits a `slept` fact carrying the bout's span, and sets
/// `wake_at` if the act renders the body unconscious; it asks nothing about
/// the room.
///
/// (This paragraph said "commits `rested`" until the Definition-of-Done
/// sweep. Task 8's act split moved the method onto `slept` and nothing about
/// a doc comment reddens when the predicate under it changes.)
///
/// The tripwire is for a specific future mistake: someone reads
/// `OfferedVerb::Sleep`'s doc, sees "gates on SupportsRest", and makes the
/// verb honour it. At that point a magically-slept target walks off to find a
/// bed, because the two routes into sleep — the voluntary act and an imposed
/// effect — would share a gate that only one of them chose.
///
/// The room this test sleeps in is the walk-band start position
/// `Session::start` opens in, so it has no made bed: `the-fireside-bed` still
/// requires a built, cold room. The Tenon's natural surfaces mean an outdoor
/// locale may now carry bracken, so the older claim that every walk-band room
/// had no rest-affording anchor at all is deliberately gone. The contract this
/// test pins is unchanged and narrower: the `sleep` command takes no object
/// argument and never requires a bed.
///
/// MUTATION THIS MUST FAIL AGAINST: add an early return to `Session::sleep`
/// refusing when no anchor in the current interior offers `OfferedVerb::Sleep`
/// (the "fix" this test exists to reject).
#[test]
fn sleeping_needs_no_bed() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).expect("possession starts");

    let reply = match s.handle("sleep") {
        Turn::Out(t) | Turn::Released(t) => t,
    };
    assert!(
        reply.contains("You lie down"),
        "sleep must succeed with no made bed or home in reach — sleeping is \
         gated on nothing: {reply}"
    );
}

/// The two-way agreement between [`ObjectProperty::SupportsRest`] and
/// `ObjectTraits::rest` (The Tenon, Task 2, spec §4.1).
///
/// **Direction: BOTH.** A marker without an offer is a kind the fold will
/// grade as afforded and then find nothing to grade it by; an offer without
/// a marker is data no verb can reach. A one-directional check here would be
/// blind to exactly the half that broke `RadiatesHeat` (two carriers in
/// `object_registry`, one in `warmth_at`) and would still read as total.
#[test]
fn supports_rest_and_a_rest_surface_imply_each_other() {
    fn assert_agreement<K: std::fmt::Debug>(kind: &K, traits: &ObjectTraits) {
        let marked = traits
            .properties
            .contains(&hornvale_vessel::affordance::ObjectProperty::SupportsRest);
        assert_eq!(
            marked,
            traits.rest.is_some(),
            "{kind:?} carries SupportsRest={marked} but rest={:?}; the two must \
             agree in both directions",
            traits.rest.is_some()
        );
    }

    let reg = hornvale_vessel::affordance::object_registry();
    for (kind, traits) in reg.iter() {
        assert_agreement(kind, traits);
    }

    let weft = hornvale_vessel::affordance::weft_object_registry();
    for (kind, traits) in weft.iter() {
        assert_agreement(kind, traits);
    }
}

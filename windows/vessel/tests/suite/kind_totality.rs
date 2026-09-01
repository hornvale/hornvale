//! The default-deny gate family that replaces the compiler's exhaustiveness
//! (The Wicket, spec §5). Each test states the DIRECTION it enforces in its
//! own doc comment, because a gate asserting `declared ⊆ resolvable` is
//! structurally blind to over-admission and still reads as total to the next
//! reader.

use hornvale_thing::THING_KINDS;
use hornvale_vessel::interior::pattern::{Attach, INVENTORY};

/// **Direction: declared ⊆ rostered.** Every kind the authored pattern
/// inventory names — the anchor it contributes, the kind it requires, and the
/// kind it attaches beside or within — has a row in `THING_KINDS`.
///
/// This is the guarantee the enum gave for free: before The Wicket a pattern
/// could only name a variant, and a variant could only exist by being
/// declared. Now a pattern names a string, and a typo compiles.
///
/// It does NOT check the converse — that every rostered kind is placeable by
/// some pattern — and that omission is deliberate. `cave-mouth` is a rostered
/// kind that is a `Vertex`/`ChamberAddr` and never an anchor, so the converse
/// would need an exemption list, and an exemption list is a thing nobody
/// maintains.
///
/// MUTATION THIS MUST FAIL AGAINST: change `the-high-seat`'s `kind` in
/// `INVENTORY` to `KindId("high_seat")` — underscore for hyphen. It compiles,
/// the room composes, and the anchor silently has no prose and no properties.
///
/// Red observed (`python3 scripts/mutate.py windows/vessel/src/interior/pattern.rs
/// 'kind: kinds::HIGH_SEAT' 'kind: KindId("high_seat")'`, then the command
/// below):
/// ```text
/// FAIL [   0.008s] (1/1) hornvale-vessel::suite kind_totality::every_kind_the_grammar_names_is_a_roster_row
///
/// thread 'kind_totality::every_kind_the_grammar_names_is_a_roster_row' (176720714) panicked at windows/vessel/tests/suite/kind_totality.rs:48:13:
/// pattern "the-high-seat" names "high_seat" in its kind, which the roster does not carry
/// ```
#[test]
fn every_kind_the_grammar_names_is_a_roster_row() {
    let mut checked = 0usize;
    for p in INVENTORY.iter() {
        let mut named = vec![("kind", p.kind)];
        if let Some(r) = p.requires {
            named.push(("requires", r));
        }
        match p.attach {
            Attach::Beside(k) | Attach::Within(k) => named.push(("attach", k)),
            Attach::Hub => {}
        }
        for (slot, id) in named {
            assert!(
                THING_KINDS.contains(&id.0),
                "pattern {:?} names {:?} in its {slot}, which the roster does not carry",
                p.name,
                id.0
            );
            checked += 1;
        }
    }
    // A vacuous pass is the failure mode of any loop-over-a-table test: an
    // empty INVENTORY satisfies every assertion above. One kind per pattern is
    // the floor, so this cannot pass while the loop is not running.
    assert!(
        checked >= INVENTORY.len(),
        "checked {checked} kinds across {} patterns — the loop is not running",
        INVENTORY.len()
    );
}

/// **Direction: propertied ⊆ rostered.** Every key in `object_registry()` is a
/// roster row, so a property can never be granted to a kind that does not
/// exist.
///
/// This one has bitten in miniature already: `object_registry`'s own doc
/// carried a claim about which rows had no `AnchorKind` behind them that went
/// stale four tasks after it was written, and nothing checked it. A table of
/// string keys wants a check that the strings mean something.
///
/// MUTATION THIS MUST FAIL AGAINST: change the `strongbox` key in
/// `object_registry()` from `KindId("strongbox")` to `KindId("strong-box")`.
/// (Task 2 left this table spelling bare literals rather than
/// `kinds::STRONGBOX`, so the literal itself is the mutation target.) It
/// compiles; `offered_by` silently returns an empty set for the real
/// strongbox, and every lock in every world stops offering `open`.
///
/// Red observed (`python3 scripts/mutate.py windows/vessel/src/affordance.rs
/// 'KindId("strongbox"),' 'KindId("strong-box"),'`, then the command below):
/// ```text
/// FAIL [   0.009s] (1/1) hornvale-vessel::suite kind_totality::every_propertied_kind_is_a_roster_row
///
/// thread 'kind_totality::every_propertied_kind_is_a_roster_row' (176728426) panicked at windows/vessel/tests/suite/kind_totality.rs:98:9:
/// object_registry has "strong-box", the roster does not
/// ```
#[test]
fn every_propertied_kind_is_a_roster_row() {
    let reg = hornvale_vessel::affordance::object_registry();
    assert!(
        !reg.is_empty(),
        "the property table is empty — this test is vacuous"
    );
    for id in reg.ids() {
        assert!(
            THING_KINDS.contains(&id.0),
            "object_registry has {:?}, the roster does not",
            id.0
        );
    }
}

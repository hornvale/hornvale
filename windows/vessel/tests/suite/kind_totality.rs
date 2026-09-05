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
    // Anti-vacuity, and an exact ratchet rather than a floor: `checked >=
    // INVENTORY.len()` reads as "the loop ran" but is satisfied by `0 >= 0`
    // when `INVENTORY` is empty, which is exactly the vacuous pass this
    // comment used to claim could not happen. Pin the real count instead —
    // the same precedent `chamber_prose::every_kind_the_grammar_places_has_a_
    // detail` set inside this campaign — so a future edit that dropped a slot
    // from the walk above would redden rather than silently measuring less.
    //
    // Task 5 appended `the-brazier`, which names all three slots (`kind`,
    // `requires: Some(kinds::LOOM)`, `attach: Attach::Beside(kinds::LOOM)`),
    // moving this from 38 to 41. The Housemark appended three threshold
    // relations, each naming all three slots, moving this to 50. The Tenon's
    // three natural surfaces each name two (`kind` and `attach`, with no
    // requirement), moving it to 56.
    assert_eq!(
        checked, 56,
        "the sweep no longer reads every kind INVENTORY names: {checked} \
         slots, not 56"
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
    // Anti-vacuity, exact rather than a floor: `!reg.is_empty()` passes with
    // one row where there are nine, which is the same relative-guard defect
    // the sibling test above carried. Pin the real count so a row dropped
    // from `object_registry` reddens here instead of narrowing the
    // population this loop measures.
    //
    // Task 5 added the `brazier` row (`RadiatesHeat`), moving this from 9 to
    // 10; The Brattice's Task 5 added the `door` row (`AffordsPassage`,
    // `Openable`, `Lockable`), moving it from 10 to 11. The Tenon's three
    // rest-surface rows move it to 14.
    assert_eq!(
        reg.len(),
        14,
        "object_registry has {} rows, not 14 — the population this sweep \
         measures has changed",
        reg.len()
    );
    for id in reg.ids() {
        assert!(
            THING_KINDS.contains(&id.0),
            "object_registry has {:?}, the roster does not",
            id.0
        );
    }
}

/// **Direction: rostered ⊆ prosed.** Every kind in `THING_KINDS` has a
/// chamber-prose row, so a kind the grammar may place always has something to
/// be called and something to say when examined.
///
/// This is one half of what the two exhaustive `match`es used to guarantee. It
/// is stated separately from its converse because the cheapest repair to a
/// one-way check is to delete the check, and a pair that must agree in both
/// directions cannot be repaired that way.
///
/// MUTATION THIS MUST FAIL AGAINST: delete the `"log"` row from
/// `chamber_prose_registry()`.
///
/// Red observed (Task 4; `cp` backup, mutate, run
/// `-E 'test(kind_totality) or test(chamber_prose)'`, `cp` restore, re-run
/// green 21/21):
/// ```text
/// FAIL [   0.012s] (12/21) hornvale-vessel chamber_prose::tests::no_detail_speaks_of_terrain
/// thread 'chamber_prose::tests::no_detail_speaks_of_terrain' panicked at windows/vessel/src/chamber_prose.rs:230:28:
/// no chamber prose for kind "log"
///
/// FAIL [   0.018s] (21/21) hornvale-vessel::suite kind_totality::every_roster_kind_has_chamber_prose
/// thread 'kind_totality::every_roster_kind_has_chamber_prose' panicked at windows/vessel/tests/suite/kind_totality.rs:136:9:
/// roster names "log", chamber prose does not
///
/// Summary [ 0.043s] 21 tests run: 18 passed, 3 failed
/// ```
/// The targeted test reddened as specified; two siblings
/// (`every_kind_has_a_detail`, `no_detail_speaks_of_terrain`) reddened too,
/// via `detail`'s own refusal, because both now sweep the full roster and
/// would have hit the deleted row as well. **Pasted here at the campaign's
/// close** — G-b and G-c were the only two of the six gates whose reds lived
/// solely in a task report, which dies with its scratch while decision 0556's
/// claim that both directions were reddened by hand persists.
#[test]
fn every_roster_kind_has_chamber_prose() {
    let prose = hornvale_vessel::chamber_prose::chamber_prose_registry();
    for label in THING_KINDS {
        assert!(
            prose.get(&hornvale_kernel::KindId(label)).is_some(),
            "roster names {label:?}, chamber prose does not"
        );
    }
}

/// **Direction: prosed ⊆ rostered.** No chamber-prose row names a kind the
/// roster does not carry — the converse of the check above, and the one that
/// catches a row added for a kind that was renamed or never existed.
///
/// MUTATION THIS MUST FAIL AGAINST: add a `KindId("brasier")` row to
/// `chamber_prose_registry()` (the plausible misspelling of the kind Task 5
/// adds).
///
/// Red observed (Task 4, same procedure as the check above):
/// ```text
/// FAIL [   0.008s] (18/21) hornvale-vessel::suite kind_totality::every_chamber_prose_row_is_a_roster_kind
/// thread 'kind_totality::every_chamber_prose_row_is_a_roster_kind' panicked at windows/vessel/tests/suite/kind_totality.rs:154:9:
/// chamber prose has "brasier", the roster does not
///
/// Summary [ 0.027s] 21 tests run: 20 passed, 1 failed
/// ```
/// Exactly the targeted test failed, alone — the sharper of the pair's two
/// mutations, which is what a converse check should look like.
#[test]
fn every_chamber_prose_row_is_a_roster_kind() {
    let prose = hornvale_vessel::chamber_prose::chamber_prose_registry();
    for id in prose.ids() {
        assert!(
            THING_KINDS.contains(&id.0),
            "chamber prose has {:?}, the roster does not",
            id.0
        );
    }
}

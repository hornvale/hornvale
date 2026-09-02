//! The Tableau: a situation staged declaratively, rather than hunted for.
//!
//! These tests assert on SPECIES, never on labels or entity ids. The whole
//! point of the campaign is that the caller chose who is here, so the thing
//! worth pinning is that the world handed back what was asked for.

use hornvale_vessel::{PossessOpts, Session, Tableau};

use crate::common;

/// A cast the caller named is the cast the session has — including a species
/// the local settlement does not have.
///
/// **The drow is the load-bearing half.** A goblin might coincide with what
/// `derive_npcs` would have produced anyway; a drow in a drow-less world can
/// only be there because it was staged. That is the permissive ratification
/// this campaign exists to deliver.
#[test]
fn a_staged_cast_is_the_cast_the_session_has() {
    let world = common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(Tableau::new().with_cast(["goblin", "drow"])),
        ..PossessOpts::default()
    };
    let (session, _) = Session::start(&world, &opts).expect("a staged session starts");

    let species: Vec<&str> = session
        .bodies()
        .iter()
        .map(|b| b.species.as_str())
        .collect();
    assert_eq!(
        species,
        vec!["goblin", "drow"],
        "a staged cast is exactly what was asked for, in order, and nothing \
         else: an inherited body here is the mystery guest the campaign \
         exists to kill"
    );
}

/// The staged cast shares a room, which is the entire point of staging one.
#[test]
fn a_staged_cast_stands_in_one_room() {
    let world = common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(Tableau::new().with_cast(["goblin", "drow"])),
        ..PossessOpts::default()
    };
    let (session, _) = Session::start(&world, &opts).expect("a staged session starts");
    let snap = session.snapshot().expect("a live session snapshots");
    assert_eq!(
        snap.sensed.present.len(),
        1,
        "the other staged creature is in this room; `sensed.present` is the \
         presence-gated channel and must say so"
    );
}

/// An unspecified cast is EMPTY, not inherited (spec section 5).
///
/// This is the negative direction and it is the one that rots: if a tableau
/// with no cast quietly fell back to `derive_npcs`, every tableau would
/// depend on whatever the seed happened to place, silently, which is the
/// complaint the campaign answers.
#[test]
fn a_tableau_with_no_cast_stages_nobody() {
    let world = common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(Tableau::new()),
        ..PossessOpts::default()
    };
    let started = Session::start(&world, &opts);
    match started {
        Ok((session, _)) => assert!(
            session.bodies().is_empty(),
            "an unspecified cast is EMPTY, never inherited: got {:?}",
            session
                .bodies()
                .iter()
                .map(|b| &b.species)
                .collect::<Vec<_>>()
        ),
        Err(_) => { /* refusing to possess an empty world is also correct */ }
    }
}

/// The file is a FRONT-END over the builder, not a second way to make one.
#[test]
fn a_tableau_read_from_json_equals_the_one_the_builder_makes() {
    let built = Tableau::new()
        .with_cast(["drow", "goblin"])
        .with_thing("key", 1);
    let read = Tableau::from_json(
        r#"{"cast":[{"species":"drow"},{"species":"goblin"}],
            "things":[{"kind":"key","held_by":1}]}"#,
    )
    .expect("a well-formed tableau parses");
    assert_eq!(built, read);
}

/// A misspelt key must not stage something quietly different from what was
/// written — the failure mode a hand-authored file is most prone to.
#[test]
fn a_tableau_with_an_unknown_field_is_refused() {
    assert!(
        Tableau::from_json(r#"{"casts":[{"species":"drow"}]}"#).is_err(),
        "`casts` is not `cast`; a tableau that silently staged an empty cast \
         here would be a scene its author never wrote"
    );
}

/// **The drama.** A goblin holding something, a drow who can see it, in one
/// room — staged, with no seed hunted for and no world that happened to
/// contain it.
#[test]
fn a_drow_can_see_what_the_goblin_is_holding() {
    let world = common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(
            Tableau::new()
                .with_cast(["drow", "goblin"])
                .with_thing("key", 1),
        ),
        ..PossessOpts::default()
    };
    let (session, _) = Session::start(&world, &opts).expect("the drama stages");
    let snap = session.snapshot().expect("snapshots");

    assert_eq!(session.bodies()[0].species, "drow", "we are the drow");
    let goblin = snap
        .sensed
        .present
        .first()
        .expect("the goblin shares the room");
    assert_eq!(goblin.label, "goblin");
    let held: Vec<&str> = goblin.carrying.iter().map(|c| c.noun.as_str()).collect();
    assert_eq!(
        held,
        vec!["a key"],
        "the drow can see what the goblin is holding — the assertion The \
         Company had to leave as `the channel exists`, because no world it \
         could find had a co-located creature holding anything"
    );
}

/// A tableau that puts a thing in nobody's hands is refused, not ignored.
#[test]
fn a_thing_held_by_a_cast_member_that_does_not_exist_is_refused() {
    let world = common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(Tableau::new().with_cast(["drow"]).with_thing("key", 7)),
        ..PossessOpts::default()
    };
    assert!(
        Session::start(&world, &opts).is_err(),
        "a tableau naming a holder it never staged has not described the \
         scene its author meant"
    );
}

/// An unspecified `relations` is EMPTY, not inherited — the same rule
/// `a_tableau_with_no_cast_stages_nobody` pins for the cast.
///
/// The world's own ledger already carries `instance-of` facts of its own
/// (settlement roster kinds, promoted props elsewhere) that have nothing to
/// do with this session's cast, so the check is scoped to facts naming a
/// freshly staged cast member as SUBJECT — none of those may carry the
/// predicate a relation would have staged, since none was.
#[test]
fn a_tableau_with_no_relations_stages_none() {
    let world = common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(Tableau::new().with_cast(["goblin", "drow"])),
        ..PossessOpts::default()
    };
    let (session, _) = Session::start(&world, &opts).expect("a staged session starts");
    let cast: Vec<u64> = session.bodies().iter().map(|b| b.entity.get()).collect();

    let ledger: serde_json::Value =
        serde_json::from_str(&session.session_ledger_json()).expect("the ledger is JSON");
    let staged_a_relation = ledger
        .get("facts")
        .and_then(|f| f.as_array())
        .expect("a ledger serializes its facts as an array")
        .iter()
        .any(|f| {
            f.get("predicate").and_then(|p| p.as_str()) == Some(hornvale_kernel::INSTANCE_OF)
                && f.get("subject")
                    .and_then(|s| s.as_u64())
                    .is_some_and(|s| cast.contains(&s))
        });
    assert!(
        !staged_a_relation,
        "an unspecified relations list is EMPTY, never inherited: no fact of \
         the predicate a relation would have staged may name a freshly \
         staged cast member as subject: {ledger}"
    );
}

/// A stated relation between two cast members is readable from the session
/// after staging.
///
/// `INSTANCE_OF` is used here only because it is a predicate the concept
/// registry always carries (kernel-core) and no other staging path commits
/// it for a cast member's own entity — the point under test is the
/// MACHINERY (a tableau's relation reaches the ledger, subject/predicate/
/// object exactly as named), not this predicate's usual meaning.
#[test]
fn a_staged_relation_is_readable_from_the_session() {
    let world = common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(Tableau::new().with_cast(["drow", "goblin"]).with_relation(
            hornvale_kernel::INSTANCE_OF,
            0,
            1,
        )),
        ..PossessOpts::default()
    };
    let (session, _) = Session::start(&world, &opts).expect("a staged relation stages");
    let subject = session.bodies()[0].entity.get();
    let object = session.bodies()[1].entity.get();

    let ledger: serde_json::Value =
        serde_json::from_str(&session.session_ledger_json()).expect("the ledger is JSON");
    let found = ledger
        .get("facts")
        .and_then(|f| f.as_array())
        .expect("a ledger serializes its facts as an array")
        .iter()
        .any(|f| {
            f.get("predicate").and_then(|p| p.as_str()) == Some(hornvale_kernel::INSTANCE_OF)
                && f.get("subject").and_then(|s| s.as_u64()) == Some(subject)
                && f.get("object")
                    .and_then(|o| o.get("Entity"))
                    .and_then(|e| e.as_u64())
                    == Some(object)
        });
    assert!(
        found,
        "the staged relation must be readable back out of the session's ledger: {ledger}"
    );
}

/// A relation naming a cast index that does not exist is refused, not
/// ignored — mirroring `a_thing_held_by_a_cast_member_that_does_not_exist_is_refused`.
/// The refusal names the offending index, the same as `held_by`'s does.
#[test]
fn a_relation_naming_a_cast_index_that_does_not_exist_is_refused() {
    let world = common::build(42).expect("seed 42 builds");
    let opts = PossessOpts {
        tableau: Some(Tableau::new().with_cast(["drow"]).with_relation(
            hornvale_kernel::INSTANCE_OF,
            0,
            7,
        )),
        ..PossessOpts::default()
    };
    let err = Session::start(&world, &opts)
        .err()
        .expect("a relation naming a cast member it never staged must be refused");
    let message = format!("{err}");
    assert!(
        message.contains('7'),
        "the refusal must name the offending index: {message}"
    );
}

/// The file is a FRONT-END over the builder for relations too, not a second
/// way to make one.
#[test]
fn a_tableau_read_from_json_round_trips_relations() {
    let built = Tableau::new()
        .with_cast(["drow", "goblin"])
        .with_relation("instance-of", 0, 1);
    let read = Tableau::from_json(
        r#"{"cast":[{"species":"drow"},{"species":"goblin"}],
            "relations":[{"predicate":"instance-of","subject":0,"object":1}]}"#,
    )
    .expect("a well-formed tableau parses");
    assert_eq!(built, read);
}

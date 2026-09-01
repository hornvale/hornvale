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

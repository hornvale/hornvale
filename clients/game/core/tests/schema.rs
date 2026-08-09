//! The mirror parses the real emitted document, and deliberately cannot see
//! the channel the schema itself warns is a cheat pane.

const FIXTURE: &str = include_str!("fixtures/session-seed-42-turn-0.json");

#[test]
fn the_fixture_parses() {
    let s = hornvale_game_core::Snapshot::parse(FIXTURE).expect("fixture must parse");
    assert_eq!(s.schema, "vessel/session/v1");
    assert_eq!(s.turn, 0);
    assert!(
        !s.narration.prose.is_empty(),
        "prose is the constitutional primary"
    );
}

/// The agent id crosses the wire as a decimal STRING, not a number — it is a
/// uniform 64-bit draw that exceeds the 2^53 a JSON number holds losslessly.
#[test]
fn the_agent_id_is_read_as_a_string() {
    let s = hornvale_game_core::Snapshot::parse(FIXTURE).unwrap();
    assert!(
        s.me.agent.parse::<u64>().is_ok(),
        "agent id must round-trip as u64"
    );
}

/// THE REDACTION. `social` is present in the emitted document and is world
/// truth, not knowledge-gated — the schema's own doc says rendering it
/// unfiltered ships a cheat pane. The mirror omits it, so no render path can
/// reach it. This test proves the field is really in the input (otherwise it
/// asserts nothing) and really absent from the parsed value.
#[test]
fn social_is_present_in_the_document_and_absent_from_the_mirror() {
    let raw: serde_json::Value = serde_json::from_str(FIXTURE).unwrap();
    assert!(
        raw.get("social")
            .is_some_and(|v| v.as_array().is_some_and(|a| !a.is_empty())),
        "VACUOUS TEST GUARD: the fixture must actually contain a non-empty \
         `social` array, or this test proves nothing"
    );

    let mirrored =
        serde_json::to_value(hornvale_game_core::Snapshot::parse(FIXTURE).unwrap()).unwrap();
    assert!(
        mirrored.get("social").is_none(),
        "the mirror must not carry `social` — see The Quire spec section 6"
    );
}

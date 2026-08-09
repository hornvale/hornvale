//! The mirror parses the real emitted document, and deliberately cannot see
//! the channel the schema itself warns is a cheat pane.

const FIXTURE: &str = include_str!("fixtures/session-seed-42-turn-0.json");

/// A chamber-band snapshot (one `enter` from the same seed-42 opening
/// `FIXTURE` observes at turn 0) — the turn-0 fixture always lands on
/// `spatial.band == "walk"`, so without this the `Spatial::Chamber` mirror
/// (`Plan`, `PlanExtent`, `PaletteEntry`, `PlanPoint`, `PlanMark`) had no
/// committed coverage at all: nothing here would catch a regression before
/// later tasks lean on those types.
const CHAMBER_FIXTURE: &str = include_str!("fixtures/session-seed-42-chamber.json");

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

/// The chamber branch, exercised for the first time. Asserts on SHAPE, not
/// on exact coordinates — a coordinate is a witness that will drift the
/// moment this fixture is regenerated, and this test's job is to catch a
/// broken invariant in the mirror, not to pin one world's particular floor
/// plan.
#[test]
fn the_chamber_band_parses_and_the_plan_is_internally_consistent() {
    let s = hornvale_game_core::Snapshot::parse(CHAMBER_FIXTURE).expect("fixture must parse");
    assert_eq!(s.schema, "vessel/session/v1");

    let plan = match s.spatial {
        hornvale_game_core::Spatial::Chamber { plan } => plan,
        hornvale_game_core::Spatial::Walk { .. } => {
            panic!(
                "CHAMBER_FIXTURE must be indoors — regenerate it with scripts/possession-chamber.txt"
            )
        }
    };

    assert!(
        !plan.palette.is_empty(),
        "a chamber plan must name at least one cell type"
    );
    assert_eq!(
        plan.cells.len(),
        (plan.extent.w * plan.extent.h) as usize,
        "the index grid must carry exactly one entry per cell of the extent"
    );
    for &ix in &plan.cells {
        assert!(
            (ix as usize) < plan.palette.len(),
            "cell indexes palette entry {ix}, but the palette holds {} entries",
            plan.palette.len()
        );
    }
    assert!(
        plan.you.x >= plan.extent.x
            && plan.you.x < plan.extent.x + plan.extent.w
            && plan.you.y >= plan.extent.y
            && plan.you.y < plan.extent.y + plan.extent.h,
        "the possession's own cell ({}, {}) must sit inside the plan's extent {:?}",
        plan.you.x,
        plan.you.y,
        plan.extent
    );
}

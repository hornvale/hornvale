//! THE FATHOM, Task 4, Step 1: the before-arm.
//!
//! Captures what UNMODIFIED code answers for `Vantage::submerged` — today's
//! inline `matches!(stratum, Some(st) if st != Stratum::Surface)` inside
//! `vantage::observable_at` (`windows/vessel/src/vantage.rs:64`) — for every
//! `Stratum` value in every realm plus the absent case. Committed alone,
//! before Step 4's fix touches `vantage.rs`, so a later comparison is against
//! evidence rather than against a description of the old code re-derived
//! after the fact ("a before-arm re-derived from the new code proves
//! nothing").
//!
//! **A rock stratum cannot be captured by executing the real
//! `observable_at` at all — not because of this task's bug, but because of a
//! DIFFERENT, unrelated one.** `observable_at` computes `locale` (via
//! `ctx.describe_at`) before it ever assigns `submerged`, and
//! `LocaleContext::expr_at_stratum`'s below-floor fallback
//! (`windows/locale/src/lib.rs:651-665`, "KNOWN WRONG... kept byte-for-byte
//! because The Fathom may not move behaviour") hands a rock stratum to
//! `BiomeExpr::biome()` paired with `Formation::OpenWater`, which panics
//! (`domains/climate/src/facets.rs:305`, `unreachable!("OpenWater never
//! pairs with a rock stratum...")`). So executing unmodified `observable_at`
//! with `Some(Stratum::Basement)` at the flagship's (surface) position does
//! not silently return the wrong `submerged` — it CRASHES, for a reason this
//! task does not touch (tracked as followup F-10). This is independent,
//! stronger evidence for the controller's Correction 2 (the defect is
//! LATENT, not live): no real call site can even hand a rock stratum through
//! this path today without the world already crashing somewhere else first.
//!
//! So this capture uses two channels and cross-checks them where both are
//! safe:
//! - **Live execution** of the real, unmodified `observable_at`, for every
//!   stratum where that succeeds (the absent case, `Stratum::Surface`, and
//!   every water stratum) — the strongest evidence, because it is the actual
//!   code running.
//! - **A literal transcription** of the current inline predicate
//!   (`old_predicate`, verified byte-for-byte against `vantage.rs`'s own
//!   source text below via `include_str!`, so a future edit to the formula
//!   fails this capture loudly instead of silently drifting), for the rock
//!   strata that live execution cannot reach.
//!
//! The two channels agree everywhere both apply (asserted below), which is
//! what makes the transcription trustworthy for the cases live execution
//! cannot cover.
//!
//! **Ignored in the ordinary suite.** This is a one-shot provenance capture,
//! not a standing regression test: running it again after Step 4 would
//! silently regenerate the fixture from the FIXED code and defeat the whole
//! point of freezing it first. It is kept (rather than deleted once run) so
//! the capture stays reproducible — `git stash` back to the pre-fix tree and
//!
//! ```text
//! cargo test -p hornvale-vessel --test suite -- submerged_before_arm --ignored
//! ```
//!
//! reproduces `tests/fixtures/submerged-before-arm.json` byte-for-byte.

use hornvale_climate::{Realm, Stratum};
use hornvale_kernel::{EntityId, Seed, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_vessel::observable_at;

/// A byte-for-byte transcription of `vantage.rs:64`'s current inline
/// predicate. Not itself "the code under test" — `capture_before_arm` checks
/// this stays textually in sync with the real source before trusting it, and
/// cross-checks it against live execution wherever live execution is safe.
fn old_predicate(stratum: Option<Stratum>) -> bool {
    matches!(stratum, Some(st) if st != Stratum::Surface)
}

/// The exact substring `old_predicate` transcribes, so a future edit to
/// `vantage.rs`'s formula fails this loudly rather than the transcription
/// silently going stale.
const CURRENT_FORMULA_SNIPPET: &str =
    "matches!(stratum, Some(st) if st != hornvale_climate::Stratum::Surface)";

/// One row of the before-arm table.
struct Row {
    realm: &'static str,
    stratum: Option<Stratum>,
    /// Whether this row's `submerged` came from running the real,
    /// unmodified `observable_at` (`true`) or only from `old_predicate`
    /// because live execution panics for this stratum (`false`).
    live: bool,
    submerged: bool,
}

#[test]
#[ignore = "one-shot before-arm capture (The Fathom, Task 4 Step 1); run by hand, not a standing regression test - see module doc"]
fn capture_before_arm() {
    let src = include_str!("../../src/vantage.rs");
    assert!(
        src.contains(CURRENT_FORMULA_SNIPPET),
        "vantage.rs's inline `submerged` predicate has moved — update \
         `old_predicate` (and CURRENT_FORMULA_SNIPPET) in this file to match \
         before trusting a capture from it"
    );

    let world = hornvale_worldgen::build_world(
        Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let ctx = LocaleContext::build(&world).expect("seed 42's locale context builds");
    // The walk-band flagship position — the same real, committed-fixture
    // position `windows/vessel/tests/fixtures/session-seed-42.json` was
    // taken from (The Quire). `submerged` does not vary with position, so one
    // real position stands in for "positions from the session fixtures".
    // Built via `body_at` rather than the pre-Hand `mint_flagship` (The
    // Hand, Task 3): `body_at` wants an already-minted entity, and this
    // harness never commits, so the placeholder is discarded exactly as
    // `agent::mint_at` used to discard it.
    let village = hornvale_settlement::village_info(&world).expect("seed 42 has a flagship");
    let placeholder = EntityId::new(1).expect("1 is a valid nonzero entity id");
    let npc = hornvale_vessel::liveness::body_at(&world, &ctx, &village, placeholder);
    let position = npc.home.clone();

    let mut rows: Vec<Row> = Vec::new();

    // The absent case and every stratum whose realm live execution can
    // actually reach without the unrelated locale-layer panic (see module
    // doc): OVERWORLD (Surface) and WATERWORLD (the water column). Each row
    // cross-checks live execution against the transcription.
    rows.push({
        let v = observable_at(&world, &ctx, &npc, &position, WorldTime::GENESIS, None)
            .expect("observable_at(None) succeeds");
        assert_eq!(
            v.submerged,
            old_predicate(None),
            "None: live vs transcribed disagree"
        );
        Row {
            realm: "none",
            stratum: None,
            live: true,
            submerged: v.submerged,
        }
    });
    for (realm, name) in [
        (Realm::OVERWORLD, "overworld"),
        (Realm::WATERWORLD, "waterworld"),
    ] {
        for st in realm.strata() {
            let v = observable_at(&world, &ctx, &npc, &position, WorldTime::GENESIS, Some(*st))
                .unwrap_or_else(|e| panic!("observable_at({st:?}) failed: {e}"));
            assert_eq!(
                v.submerged,
                old_predicate(Some(*st)),
                "{st:?}: live vs transcribed disagree"
            );
            rows.push(Row {
                realm: name,
                stratum: Some(*st),
                live: true,
                submerged: v.submerged,
            });
        }
    }
    // UNDERDARK (the rock column): live execution panics (module doc), so
    // these rows come from the verified transcription only.
    for st in Realm::UNDERDARK.strata() {
        rows.push(Row {
            realm: "underdark",
            stratum: Some(*st),
            live: false,
            submerged: old_predicate(Some(*st)),
        });
    }

    let mut body = String::from("[\n");
    for (i, row) in rows.iter().enumerate() {
        let stratum = match row.stratum {
            Some(st) => format!("\"{st:?}\""),
            None => "null".to_string(),
        };
        body.push_str(&format!(
            "  {{\"realm\": \"{}\", \"stratum\": {stratum}, \"live\": {}, \"submerged\": {}}}{}\n",
            row.realm,
            row.live,
            row.submerged,
            if i + 1 == rows.len() { "" } else { "," }
        ));
    }
    body.push_str("]\n");

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join("submerged-before-arm.json");
    std::fs::write(&path, &body).expect("the fixture directory exists");
}

//! `vessel/session/v1` — the per-turn structured session emit.
//!
//! One snapshot per committed turn, grouped by the epistemic channel each
//! datum belongs to rather than by data type: a pane reads one channel and
//! cannot see outside it, so the redaction discipline is structural rather
//! than conventional (The Snapshot spec §3).
//!
//! Save-format contract: additive changes are free, a meaning change mints
//! `vessel/session/v2`, and nothing is ever renamed.

use hornvale_locale::Locale;
use serde::{Serialize, Serializer};

/// The schema tag every snapshot carries.
/// type-audit: bare-ok(identifier-text)
pub const SESSION_SCHEMA: &str = "vessel/session/v1";

/// Serialize a `u64` as its decimal text rather than a JSON number. JSON has
/// no int64 type, and JavaScript's `number` is an IEEE-754 double: lossy
/// above 2^53. A uniform 64-bit draw like `AgentId` routinely exceeds that
/// (seed 42's `7225590595188407000` round-trips through `JSON.parse` as
/// `7225590595188407296`, and two ids within 2048 of each other collapse to
/// the same JS number). Emitting the exact decimal digits as a string side-
/// steps the lossy conversion; the Rust field stays `u64` in memory and this
/// only governs the emit boundary. Private: not a `pub` API boundary, so it
/// carries no `type-audit:` tag of its own.
fn u64_as_decimal_string<S: Serializer>(x: &u64, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str(&x.to_string())
}

/// One committed turn, as the client sees it.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(count: turn), waiver(decision-0014: day)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SessionSnapshot {
    /// Schema tag (`vessel/session/v1`).
    pub schema: String,
    /// Advances by one for every non-empty verb line since the possession
    /// began; 0 is the opening. Not a commit count — it also advances for
    /// verbs that commit nothing (`look`, `whoami`, `help`, an unknown
    /// verb).
    pub turn: u64,
    /// The frozen day this turn observes, in absolute standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub day: f64,
    /// Who the player is. Serializes as `self`, which is a Rust keyword.
    #[serde(rename = "self")]
    pub me: SelfChannel,
    /// What the agent senses here and now — evaporates when presence does.
    pub sensed: SensedChannel,
    /// What the agent has come to know, accumulated across the walk.
    pub known: KnownChannel,
    /// Committed, placeless, per-creature standing toward the player. **Not
    /// knowledge-gated**: this folds over every NPC the session derived, not
    /// only those the agent has encountered (see `SocialEntry`'s doc).
    pub social: Vec<SocialEntry>,
    /// The sim's own rendering. Carried verbatim: prose is the
    /// constitutional primary and the client never re-derives it.
    pub narration: Narration,
    /// Where the possession stands, as cells. Last in key order because it
    /// is the newest channel and key order is contract.
    pub spatial: SpatialChannel,
}

/// The possessed agent's own identity.
/// type-audit: bare-ok(index: agent), bare-ok(index: room), bare-ok(count: population), bare-ok(identifier-text: species), bare-ok(identifier-text: settlement)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SelfChannel {
    /// The agent's deterministic minted id. Serializes as a decimal
    /// **string**, not a JSON number: JSON has no int64 type, and this is a
    /// uniform 64-bit draw (`AgentId`) that routinely exceeds the 2^53 a JS
    /// `number` can hold losslessly (see `u64_as_decimal_string`). The Rust
    /// type stays `u64`; only the emitted JSON shape differs.
    #[serde(serialize_with = "u64_as_decimal_string")]
    pub agent: u64,
    /// The species whose perception this agent carries.
    pub species: String,
    /// The settlement the agent was minted from.
    pub settlement: String,
    /// How many live there.
    pub population: u32,
    /// The agent's room, as a packed `RoomId`.
    pub room: u64,
}

/// The presence-gated channel: true only while the agent stands here.
/// type-audit: bare-ok(prose: sky)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SensedChannel {
    /// The room, as `locale/room/v2`, embedded verbatim — one schema, one
    /// owner. Its `exits` are the authoritative ways on; the client filters
    /// `Edge` + `Compass` exactly as `Session::ways()` does.
    pub room: Locale,
    /// The sky over this day, as the sky provider rendered it.
    pub sky: String,
    /// Who else is in this room right now.
    pub present: Vec<PresentEntry>,
}

/// A co-located creature, as read from presence.
/// type-audit: bare-ok(index: entity), bare-ok(identifier-text: label), bare-ok(prose: felt)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct PresentEntry {
    /// The creature's ledger entity id.
    pub entity: u64,
    /// Its label, as the narration names it.
    pub label: String,
    /// Its felt state — a presence-gated read of another creature's
    /// interior, which is why it lives here and not in `social`.
    pub felt: String,
}

/// What the agent knows: the accumulated projection, in key order.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct KnownChannel {
    /// Entries in `BTreeMap` key order, so the bytes are deterministic.
    pub entries: Vec<KnownEntry>,
}

/// One knowledge entry.
/// type-audit: bare-ok(identifier-text: key), bare-ok(artifact: value)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct KnownEntry {
    /// The knowledge key (`room/<id>`, `settlement/<id>/<field>`, `a::b`).
    pub key: String,
    /// Its surface value.
    pub value: String,
}

/// A creature's committed standing toward the player. Placeless and
/// entity-keyed, so it survives leaving the room — the reason this is its
/// own channel rather than part of `sensed`.
///
/// **Membership is world truth, not knowledge-gated.** `social` folds over
/// every NPC the session derived, whether or not the agent has ever
/// encountered them — for seed 42 that is every derived NPC (7 entries) while
/// `sensed.present` (who is actually co-located right now) has 1. A
/// world-truth pane is a cheat pane: the redaction boundary this schema makes
/// structural is real for `sensed` vs. `known` vs. `social` as *channels*,
/// but nothing here narrows `social`'s membership to what the agent actually
/// knows. The first pane that renders `social` must filter it against
/// `known` (or an equivalent knowledge gate) itself; narrowing membership
/// later changes no field's shape, so it is not an epoch event, but until
/// some caller does the filtering, rendering this channel unfiltered ships a
/// cheat pane.
/// type-audit: bare-ok(index: entity), bare-ok(identifier-text: label), bare-ok(ratio: grievance), bare-ok(flag: hostile)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SocialEntry {
    /// The creature's ledger entity id.
    pub entity: u64,
    /// Its label.
    pub label: String,
    /// The additive fold over its committed `disposition-shift` facts.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub grievance: f64,
    /// Whether that fold has crossed the hostility threshold.
    pub hostile: bool,
}

/// The sim's own rendering of this turn.
/// type-audit: bare-ok(prose: prose)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Narration {
    /// The passage the transcript prints, byte-for-byte.
    pub prose: String,
    /// The examinable noun catalog, in prose order.
    pub nouns: Vec<NounEntry>,
}

/// Where the possession stands, as cells rather than as a picture.
///
/// A tagged union over the **band**, because the session already treats
/// indoors and out as mutually exclusive: `Session::handle`'s `map` arm
/// answers `map out` indoors with `INDOOR_CHART_REFUSAL`, so the walk-band
/// chart is not derivable while inside a building. One pane switches; two do
/// not coexist.
///
/// **Two variants, but the session now has more than two ways to be
/// somewhere** — and that asymmetry is deliberate rather than an oversight,
/// so read it before adding a variant. `Session` carries three "not out of
/// doors at ground level" states: `inside` (a built structure), `submerged`
/// (the water column, The Column), and `underground` (the cave lattice, The
/// Deep Realm). Only `inside` gets its own variant. The other two fold into
/// `Walk`, which is what the `map` VERB does in exactly the same states —
/// `map`'s band arms guard on `inside` alone, so `map` underground or
/// submerged draws the surface chart too. Pane and verb therefore still
/// cannot disagree, which is the property this union exists to hold; what
/// they agree ON, in those two bands, is a chart of the country overhead.
/// Whether that is the right answer is an open question, not a settled one
/// (`CLIENT-band-fold` in the idea registry) — but it is the *same* answer
/// the sim already gives, and changing it is a sim change before it is a
/// schema change. `the_underground_band_folds_into_walk_as_map_does` in
/// `session.rs`'s test module pins the fold (it lives there rather than in
/// `tests/session_snapshot.rs` because reaching an open cave needs the
/// private `delve_at`, seed 42's flagship having no cave under it), so a
/// fourth band cannot be added without meeting this question.
///
/// The wire tag is `band`, with values `walk` and `chamber`. A client reads
/// it before anything else, so renaming either is a `vessel/session/v2`.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "band", rename_all = "lowercase")]
pub enum SpatialChannel {
    /// Not inside a built structure: the walk-band chart,
    /// `scene/surrounds/v2` embedded verbatim. One schema, one owner — the
    /// same move `sensed.room` makes with `locale/room/v2`. Covers standing
    /// out of doors, **and** the two bands that fold into it (submerged,
    /// underground) — see the enum's own doc for why.
    Walk {
        /// The chart, as `windows/scene` renders it structurally.
        chart: hornvale_scene::SurroundsScene,
    },
    /// Inside a building: the chamber-band floor plan.
    Chamber {
        /// The plan, as `vessel/plan/v1`.
        plan: crate::plan::SessionPlan,
    },
}

/// One examinable noun and its datum.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(prose: datum)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct NounEntry {
    /// The noun as the prose mentions it.
    pub noun: String,
    /// What `examine` prints for it.
    pub datum: String,
}

/// Serialize a snapshot. Floats quantize at this boundary and nowhere else.
/// type-audit: bare-ok(artifact: return)
pub fn snapshot_json(snap: &SessionSnapshot) -> String {
    serde_json::to_string(snap).expect("a SessionSnapshot always serializes")
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Seed, WorldTime};
    use hornvale_locale::LocaleContext;
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn locale() -> Locale {
        let world = build_world(
            Seed(42),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds");
        let ctx = LocaleContext::build(&world).expect("the locale context builds");
        let agent = crate::mint_flagship(&world, &ctx).expect("seed 42 has a settlement");
        ctx.describe(&agent.position, WorldTime { day: 0.5 })
            .expect("the minted position describes")
    }

    /// A minimal `vessel/plan/v1` document, for tests that need a
    /// `SpatialChannel::Chamber` but not a full lattice derivation.
    fn minimal_plan() -> crate::plan::SessionPlan {
        crate::plan::SessionPlan {
            schema: crate::plan::PLAN_SCHEMA.to_string(),
            chamber: 1,
            at: 0,
            of: 1,
            extent: crate::plan::PlanExtent {
                x: 0,
                y: 0,
                w: 1,
                h: 1,
            },
            palette: vec![crate::plan::PaletteEntry {
                kind: "floor".to_string(),
                chambers: vec![0],
                color: None,
            }],
            cells: vec![0],
            you: crate::plan::PlanPoint { x: 0, y: 0 },
            marks: Vec::new(),
        }
    }

    fn minimal() -> SessionSnapshot {
        SessionSnapshot {
            schema: SESSION_SCHEMA.to_string(),
            turn: 0,
            day: 0.5,
            me: SelfChannel {
                agent: 1,
                species: "bugbear".to_string(),
                settlement: "X".to_string(),
                population: 1,
                room: 1,
            },
            sensed: SensedChannel {
                room: locale(),
                sky: String::new(),
                present: Vec::new(),
            },
            known: KnownChannel {
                entries: Vec::new(),
            },
            social: vec![SocialEntry {
                entity: 1,
                label: "a goblin".to_string(),
                grievance: 0.0,
                hostile: false,
            }],
            narration: Narration {
                prose: String::new(),
                nouns: Vec::new(),
            },
            spatial: SpatialChannel::Chamber {
                plan: minimal_plan(),
            },
        }
    }

    #[test]
    fn the_envelope_carries_the_schema_tag_and_channel_keys() {
        let snap = SessionSnapshot {
            schema: SESSION_SCHEMA.to_string(),
            turn: 3,
            day: 0.5,
            me: SelfChannel {
                agent: 7225590595188407000,
                species: "bugbear".to_string(),
                settlement: "Vngoashshngaoshshngoogootao".to_string(),
                population: 118,
                room: 738918402,
            },
            sensed: SensedChannel {
                room: locale(),
                sky: "Night.".to_string(),
                present: vec![PresentEntry {
                    entity: 1230,
                    label: "a goblin".to_string(),
                    felt: "is content".to_string(),
                }],
            },
            known: KnownChannel {
                entries: vec![KnownEntry {
                    key: "settlement/7/name".to_string(),
                    value: "Vngoashshngaoshshngoogootao".to_string(),
                }],
            },
            social: vec![SocialEntry {
                entity: 1230,
                label: "a goblin".to_string(),
                grievance: 0.0,
                hostile: false,
            }],
            narration: Narration {
                prose: "Tropical seasonal forest.".to_string(),
                nouns: vec![NounEntry {
                    noun: "sky".to_string(),
                    datum: "Night.".to_string(),
                }],
            },
            spatial: SpatialChannel::Chamber {
                plan: minimal_plan(),
            },
        };
        let json = snapshot_json(&snap);
        assert!(json.contains(r#""schema":"vessel/session/v1""#));
        for key in [
            "\"self\":",
            "\"sensed\":",
            "\"known\":",
            "\"social\":",
            "\"narration\":",
            "\"spatial\":",
        ] {
            assert!(json.contains(key), "channel key {key} missing from {json}");
        }
        assert!(
            !json.contains("\"me\":"),
            "the `me` field must serialize as `self`"
        );
    }

    #[test]
    fn floats_are_quantized_at_the_emit_boundary() {
        // 1/3 has no short decimal form; quantization pins it to 8
        // significant digits so the bytes are cross-platform stable.
        let mut snap = minimal();
        snap.social[0].grievance = 1.0 / 3.0;
        assert!(
            snapshot_json(&snap).contains("0.33333333"),
            "grievance must pass through quantize_serde::f64_field"
        );
    }
}

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
use serde::Serialize;

/// The schema tag every snapshot carries.
/// type-audit: bare-ok(identifier-text)
pub const SESSION_SCHEMA: &str = "vessel/session/v1";

/// One committed turn, as the client sees it.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(count: turn), waiver(decision-0014: day)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SessionSnapshot {
    /// Schema tag (`vessel/session/v1`).
    pub schema: String,
    /// Commits since the possession began; 0 is the opening.
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
    /// Committed, placeless, per-creature standing toward the player.
    pub social: Vec<SocialEntry>,
    /// The sim's own rendering. Carried verbatim: prose is the
    /// constitutional primary and the client never re-derives it.
    pub narration: Narration,
}

/// The possessed agent's own identity.
/// type-audit: bare-ok(index: agent), bare-ok(index: room), bare-ok(count: population), bare-ok(identifier-text: species), bare-ok(identifier-text: settlement)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SelfChannel {
    /// The agent's deterministic minted id.
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
                settlement: "Qvooshtvoagootao".to_string(),
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
                    value: "Qvooshtvoagootao".to_string(),
                }],
            },
            social: vec![SocialEntry {
                entity: 1230,
                label: "a goblin".to_string(),
                grievance: 0.0,
                hostile: false,
            }],
            narration: Narration {
                prose: "You stand in tropical seasonal forest.".to_string(),
                nouns: vec![NounEntry {
                    noun: "sky".to_string(),
                    datum: "Night.".to_string(),
                }],
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

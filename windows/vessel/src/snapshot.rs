//! `vessel/session/v2` — the per-turn structured session emit.
//!
//! One snapshot per committed turn, grouped by the epistemic channel each
//! datum belongs to rather than by data type: a pane reads one channel and
//! cannot see outside it, so the redaction discipline is structural rather
//! than conventional (The Snapshot spec §3).
//!
//! Save-format contract: additive changes are free, a meaning change mints
//! the next version, and nothing is ever renamed.
//!
//! **Why v2 (The Signet).** `EntityId` stopped being a small mint-counter
//! value and became a full-width 64-bit derivation of an entity's lineage, so
//! the `entity` key on `sensed.present` and `social` began emitting numbers
//! above 2^53 — silently lossy in any JavaScript consumer. Those two fields
//! now carry the same decimal-string encoding `self.agent` has always used.
//! That is a wire *type* change, not an addition, so it mints a version
//! rather than mutating `v1` in place. **The bump is exactly one schema
//! deep**: `sensed.room` (`locale/room/v2`), the walk band's chart
//! (`scene/surrounds/v2`) and the chamber band's plan (`vessel/plan/v1`) are
//! embedded verbatim, carry no `EntityId`, and each announces its own
//! version — the same reason the chart's own bump to `scene/surrounds/v2` did
//! not move the envelope around it.

use hornvale_locale::Locale;
use serde::{Serialize, Serializer};

/// The schema tag every snapshot carries.
/// type-audit: bare-ok(identifier-text)
pub const SESSION_SCHEMA: &str = "vessel/session/v2";

/// Serialize a `u64` as its decimal text rather than a JSON number. JSON has
/// no int64 type, and JavaScript's `number` is an IEEE-754 double: lossy
/// above 2^53. Both id families on this wire are full-width 64-bit draws that
/// routinely exceed it — `AgentId` (seed 42's `7225590595188407000`
/// round-trips through `JSON.parse` as `7225590595188407296`, and two ids
/// within 2048 of each other collapse to the same JS number) and, since The
/// Signet made ids lineage-derived, `EntityId`. Emitting the exact decimal
/// digits as a string side-steps the lossy conversion; the Rust field stays
/// `u64` in memory and this only governs the emit boundary. Private: not a
/// `pub` API boundary, so it carries no `type-audit:` tag of its own.
fn u64_as_decimal_string<S: Serializer>(x: &u64, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str(&x.to_string())
}

/// One committed turn, as the client sees it.
/// `day` stays a bare `f64`: it is the `vessel/session/v2` wire schema's own
/// field, a cross-repo client contract with its own quantizing serializer, not
/// the ledger's `Fact.day` (decision 0126, superseding 0014).
/// type-audit: bare-ok(identifier-text: schema), bare-ok(count: turn), waiver(decision-0126: day)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SessionSnapshot {
    /// Schema tag (`vessel/session/v2`).
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
    /// The driven body's own stable identity: its ledger `EntityId` (The
    /// Hand, Task 3 — no longer a separately-minted `AgentId`). Serializes
    /// as a decimal **string**, not a JSON number: JSON has no int64 type,
    /// and a lineage-derived `NonZeroU64` can still exceed the 2^53 a JS
    /// `number` can hold losslessly (see `u64_as_decimal_string`). The Rust
    /// type stays `u64`; only the emitted JSON shape differs.
    #[serde(serialize_with = "u64_as_decimal_string")]
    pub agent: u64,
    /// The species whose perception this agent carries.
    pub species: String,
    /// The settlement the agent belongs to, or a neutral fallback for one
    /// with none.
    pub settlement: String,
    /// How many live there.
    pub population: u32,
    /// The agent's room, as a packed `FacetId`.
    pub room: u64,
    /// What this body is holding, in [`crate::thing::held_by`]'s `EntityId`
    /// order — the same fold `carrying`, `drop` and `put` resolve against, so
    /// a pane and the three verbs cannot disagree about what is in hand.
    /// Empty for a possession carrying nothing, which is every possession
    /// that has not typed `take`.
    ///
    /// **On `self` rather than in a channel of its own, and the precedent is
    /// one field up.** `room` already puts the body's own STATE on this
    /// channel beside its identity, so custody needs no new one: both are
    /// facts about the body that outlive the room it stands in. Custody is
    /// not presence-gated (it survives walking out), not knowledge (a body
    /// needs no inference to know its own hands), and not a standing toward
    /// anyone, so none of the other four channels is its home.
    ///
    /// **This is an observable, not a vital** (decision 0400). The client's
    /// endpaper draws no hit points, stamina or hunger because the sim
    /// commits no such quantity and a strip printing one would be inventing
    /// it. A carried thing is the opposite: `located-in` naming the body as
    /// its object is a committed fact with an entity on each end, which
    /// `possess --out` saves and any reader can re-derive. Rendering it
    /// invents nothing.
    ///
    /// Additive on `vessel/session/v2` per the schema discipline: a mirror
    /// that does not know this key ignores it (serde skips unknown fields),
    /// so no version moves.
    pub carrying: Vec<CarriedEntry>,
}

/// One thing in the driven body's custody.
///
/// Two fields, each with a job: `entity` is the thing's identity across
/// turns — the key that a pane tracks when the same key leaves a storeroom,
/// crosses four chambers and is set down in another room — and `noun` is the
/// exact word the verbs take, since `Session::carried_named` matches a
/// player's word against the full noun, article and all.
/// type-audit: bare-ok(index: entity), bare-ok(identifier-text: noun)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CarriedEntry {
    /// The thing's ledger entity id. Serializes as a decimal **string**, for
    /// the FOURTH time on this wire and for the same reason the three before
    /// it do: since The Signet an `EntityId` is a full-width 64-bit
    /// derivation of the entity's lineage, and a JS `number` cannot hold one
    /// losslessly (see `u64_as_decimal_string`).
    ///
    /// **This said "third" and named two of the three, and two other sites
    /// copied the miscount with a DIFFERENT pair each** (corrected Task 13,
    /// fix round). The roster is not a memory: it is every
    /// `serialize_with = "u64_as_decimal_string"` in this file —
    /// [`SelfChannel::agent`], this field, [`PresentEntry::entity`] and
    /// [`SocialEntry::entity`], four of them. `grep -n u64_as_decimal_string
    /// windows/vessel/src/snapshot.rs` is the whole check, which is why the
    /// count is stated with the command that produces it rather than with a
    /// list to keep in step.
    #[serde(serialize_with = "u64_as_decimal_string")]
    pub entity: u64,
    /// The noun prose says it by (`"a key"`) — what `drop` and `put` accept,
    /// matched case-insensitively against the whole of it.
    pub noun: String,
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
    /// The creature's ledger entity id. Serializes as a decimal **string**
    /// for the same reason `SelfChannel::agent` does: since The Signet an
    /// `EntityId` is a full-width 64-bit derivation of the entity's lineage,
    /// which a JS `number` cannot hold losslessly (see
    /// `u64_as_decimal_string`). The Rust type stays `u64`.
    #[serde(serialize_with = "u64_as_decimal_string")]
    pub entity: u64,
    /// Its label, as the narration names it.
    pub label: String,
    /// Its felt state — a presence-gated read of another creature's
    /// interior, which is why it lives here and not in `social`.
    pub felt: String,
    /// What it is holding, in [`crate::thing::held_by`]'s `EntityId` order —
    /// the co-located half of [`SelfChannel::carrying`], resolved through the
    /// SAME fold, so a pane cannot disagree with a verb about what is in
    /// whose hands.
    ///
    /// **Here rather than on `social`, for `felt`'s reason exactly.** Custody
    /// is a presence-gated read of another creature's state: you learn it by
    /// standing in the room and looking. `SocialEntry`'s membership is world
    /// truth — every derived body, co-located or not — so the same field
    /// there would hand a possession that has never met a creature the
    /// contents of its hands, which is a materially more exploitable
    /// disclosure than a mood.
    ///
    /// Empty for a creature holding nothing, which is most of them. Additive
    /// on `vessel/session/v2` exactly as [`SelfChannel::carrying`] was, so no
    /// version moves.
    pub carrying: Vec<CarriedEntry>,
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
    /// The creature's ledger entity id. Serializes as a decimal **string**,
    /// exactly as `PresentEntry::entity` does — same id family, same 2^53
    /// hazard, so one encoding rather than two.
    #[serde(serialize_with = "u64_as_decimal_string")]
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
/// **Three variants, but the session has more than three ways to be
/// somewhere** — and that asymmetry is deliberate rather than an oversight,
/// so read it before adding a variant. `Session` carries three "not out of
/// doors at ground level" states: `inside` (a built structure), `submerged`
/// (the water column, The Column), and `underground` (the cave lattice, The
/// Deep Realm). `inside` and `underground` each get their own variant, and
/// each now has its own `map` arm to match (The Gallery, Task 8).
/// **`submerged` is the only one that still folds into `Walk`**, which is
/// what the `map` VERB does in that same state — `map`'s band arms guard on
/// `inside` and on `underground`, but there is no `submerged` arm, so `map`
/// while submerged falls through to the surface chart. Pane and verb
/// therefore still cannot disagree about `submerged`, which is the property
/// this union exists to hold there; what they agree ON is a chart of the
/// country overhead. Whether that is the right answer for `submerged` is an
/// open question, not a settled one (`CLIENT-band-fold` in the idea
/// registry, narrowed rather than closed by The Gallery — spec §5) — but it
/// is the *same* answer the sim already gives, and changing it is a sim
/// change before it is a schema change.
///
/// **The underground half of this doc used to read differently, twice, and
/// both corrections are worth keeping.** Before The Gallery's Task 7,
/// `underground` folded into `Walk` too, and `the_underground_band_folds_
/// into_walk_as_map_does` (`session.rs`'s test module) pinned that fold
/// under the same argument this doc now makes for `submerged` alone. Task 7
/// gave the pane its own answer — `band: "underground"`, carrying
/// `vessel/level/v1` — while the `map` VERB's own underground arm waited for
/// a later, separate task (The Gallery, Task 8; spec §5's "the fold,
/// retired"). Between the two, the pane and the verb answered the
/// underground question differently on purpose, and the pin was renamed
/// rather than deleted to say so — spec §5 itself states the disposition
/// ("replaced, not deleted"). **Task 8 has since landed that arm**: `map`
/// underground now draws the level from the very `SessionLevel` document
/// the pane emits, and the pin's own second name,
/// `the_pane_and_the_verb_agree_underground` (`session.rs`'s test module),
/// asserts the agreement rather than the interval before it.
///
/// The wire tag is `band`, with values `walk`, `chamber` and `underground`. A
/// client reads it before anything else, so renaming any of them is a
/// `vessel/session/v2`.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "band", rename_all = "lowercase")]
pub enum SpatialChannel {
    /// Not inside a built structure and not underground: the walk-band
    /// chart, `scene/surrounds/v2` embedded verbatim. One schema, one owner —
    /// the same move `sensed.room` makes with `locale/room/v2`. Covers
    /// standing out of doors, **and** the one band that still folds into it
    /// (submerged) — see the enum's own doc for why.
    Walk {
        /// The chart, as `windows/scene` renders it structurally. Boxed
        /// (The Grain) so `SpatialChannel`'s stack size stays close to
        /// `Chamber`'s — `SurroundsScene` grew a `resolution` block and
        /// tipped `clippy::large_enum_variant`; the box changes no byte on
        /// the wire, since `Box<T>` serializes exactly as `T`.
        chart: Box<hornvale_scene::SurroundsScene>,
    },
    /// Inside a building: the chamber-band floor plan.
    Chamber {
        /// The plan, as `vessel/plan/v1`.
        plan: crate::plan::SessionPlan,
    },
    /// Inside a generated cave descent (The Gallery, Task 7; spec §4).
    Underground {
        /// The level, as `vessel/level/v1`. Boxed for the same reason
        /// `Walk::chart` is: `Box<T>` serializes exactly as `T`, so keeping
        /// `SpatialChannel`'s own stack size close to its other variants'
        /// costs no byte on the wire.
        level: Box<crate::level_doc::SessionLevel>,
    },
}

/// One examinable noun and its datum.
///
/// # `affordances` is not here, and this is the measurement rather than the
/// omission
///
/// The Offer specified an `affordances` field beside `kind` and deferred it
/// on the grounds that nothing would fill it. The Chattel's Task 13 re-ran
/// that check before adding it, because a deferral's reason is what decays,
/// and the reason has not:
///
/// - **The mechanism.** [`crate::Session::snapshot`] builds this catalog from
///   `self.focalizer.render(&vantage)`, and `vantage` is `observable(…,
///   &self.position(), …)` — the WALK-band vantage. The chamber's `Interior`
///   is never consulted, indoors or out, so no anchor of the room a
///   possession is actually standing in reaches `narration.nouns` at all.
/// - **The measurement.** The committed chamber-band fixture
///   (`clients/game/core/tests/fixtures/session-seed-42-chamber.json`) was
///   taken one `enter` inside a structure and its `nouns` are the walk band's
///   six — the biome, the canopy, the settlement, the sky and two moons —
///   byte-identically the same list the walk-band fixture beside it carries.
///   The Chattel's own new fixture is a second, independent instance:
///   `session-seed-14-carrying.json` is a chamber-band snapshot taken on a
///   different seed, standing in a chamber with a key in hand — and its
///   `nouns` are the biome, the canopy, the settlement, the sky and the
///   sun. Five, where the seed-42 pair carry six (two moons rather than a
///   sun, because they are taken at a different hour). **The COUNT is not
///   the claim and stating it as one is how this bullet went wrong twice**
///   (corrected Task 13, fix round): what matters is that every entry is a
///   WALK-band noun and no chamber anchor is among them — no doorway, no
///   screen, no key — in a snapshot whose own `band` is `chamber`.
///
///   The first version of this bullet said the fixture was taken "standing
///   in a room whose prose names a doorway and a screen", which was true of
///   the room and unreadable from the artifact cited: that fixture's
///   `narration.prose` is the walk's LAST REPLY (`"You are carrying a
///   key."`), not a room description. The second said "the same six", and
///   the walk it describes then moved to an earlier hour. Read the `nouns`
///   array, which is the half that is both readable there and load-bearing
///   here.
/// - **The consequence.** An `affordances` field added today would serialize
///   `[]` for every entry of every snapshot in every world, which is the
///   third artifact in this arc that would read as delivered while doing
///   nothing. Shipping it is refused a second time.
///
/// **The prerequisite is named, so a successor gets a task rather than a
/// hunch: `narration.nouns` must first carry a CHAMBER-band noun catalog.**
/// That is a real feature — the chamber band builds a `Vec<&'static str>` of
/// nouns for its prose (`chamber_prose::chamber_nouns`) and constructs no
/// [`crate::Noun`] anywhere. **The element type is `&'static str` and this
/// said `Vec<String>`** (corrected Task 13, fix round); the load-bearing
/// half — that no chamber-band `Noun` is ever built — is unchanged. And
/// it is a producer-side change to the one surface with committed client
/// fixtures and a schema discipline, so it belongs to its own campaign and
/// not to the tail of this one. Registered as
/// `CLIENT-noun-catalog-is-walk-band-only`.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(prose: datum), bare-ok(identifier-text: kind)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct NounEntry {
    /// The noun as the prose mentions it.
    pub noun: String,
    /// What `examine` prints for it.
    pub datum: String,
    /// Coarse kind for completion-capable clients. Optional on the wire:
    /// older mirrors load unchanged (serde default), newer fixtures carry it.
    /// Additive on `vessel/session/v2` per the schema discipline.
    ///
    /// **The Offer, Task 7 (spec §4): investigated as one of the four
    /// advertisement surfaces and found NOT to route through
    /// [`crate::affordance::offered_to_observer`], and this is a finding
    /// rather than an oversight.** `kind` is populated in `Session::
    /// snapshot` from `focalize::NounKind::tag()` (`Creature`/`Place`/
    /// `Thing`/`Unknown`), which is fed exclusively by walk-band content —
    /// biome, regime, village, sky (`focalize.rs`'s own `render`). No
    /// `narration.nouns` entry has ever carried a chamber anchor's kind at
    /// any call site. The evidence is a grep for a thing-kind handle in the
    /// two files that build these nouns, with doc comments excluded so the
    /// citation cannot match itself:
    ///
    /// ```text
    /// grep -rn 'kinds::' windows/vessel/src/snapshot.rs \
    ///     windows/vessel/src/focalize.rs | grep -v '///'
    /// ```
    ///
    /// It exits 1 with no output (verified 2026-09-01), as the same grep for
    /// the deleted `AnchorKind` did before The Wicket. **The `grep -v` is
    /// load-bearing rather than tidiness**: without it the command matches
    /// this very sentence and returns one line, which is what the pre-Wicket
    /// citation quietly did once the identifier it named became the string it
    /// was searching for. The claim held throughout; the command stopped
    /// witnessing it. This holds while
    /// the possession stands indoors, too — `Session::snapshot` always renders
    /// `self.focalizer.render(&vantage)` from the walk-band `Vantage`,
    /// never from the chamber's own `Interior`, and a committed chamber-band
    /// fixture (`clients/game/core/tests/fixtures/session-seed-42-chamber.json`)
    /// carries only `"place"`/`"thing"` kinds, confirming this at the byte
    /// level.
    ///
    /// **The finding survives The Chattel's Task 9; its REASON did not, and
    /// the difference is worth stating because a stale reason is what a
    /// later reader reasons from.** This used to read "`offered_to_observer`
    /// requires a real anchor-kind variant, so there is no value this field
    /// could route through it" — true when written, false since Task 9 re-keyed
    /// that query to [`hornvale_kernel::KindId`], which any string can
    /// spell. What still holds is the substantive half: `NounKind::tag()`
    /// emits `creature`/`place`/`thing`/`unknown`, and none of those is a
    /// thing-kind [`crate::affordance::object_registry`] knows, so routing
    /// this field through the query would now COMPILE and answer
    /// `{Examine}` for every noun in the world — a worse outcome than the
    /// old type error, since it reads as coverage. Wiring it for real still
    /// needs either (a) a nonsensical mapping from a walk-band noun to a
    /// registered thing-kind, or (b) newly threading chamber anchors into
    /// `narration.nouns` at all — a real feature addition, not a re-point of
    /// existing ad hoc logic, and outside this task's "smallest risk" scope
    /// (spec §10.1).
    /// See the Task 7 report for the full investigation; this doc exists so
    /// the absence is a stated finding, not a silent gap the next reader
    /// has to rediscover.
    /// type-audit: bare-ok(identifier-text: kind)
    #[serde(default)]
    pub kind: String,
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
        let village = hornvale_settlement::village_info(&world).expect("seed 42 has a settlement");
        let entity = hornvale_kernel::EntityId::new(1).expect("1 is a valid nonzero entity id");
        let npc = crate::liveness::body_at(&world, &ctx, &village, entity);
        ctx.describe(
            &npc.home,
            WorldTime::from_std_days(0.5).expect("a day value is finite"),
        )
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
                carrying: Vec::new(),
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
                carrying: vec![CarriedEntry {
                    entity: 9223372036854775809,
                    noun: "a key".to_string(),
                }],
            },
            sensed: SensedChannel {
                room: locale(),
                sky: "Night.".to_string(),
                present: vec![PresentEntry {
                    entity: 1230,
                    label: "a goblin".to_string(),
                    felt: "is content".to_string(),
                    carrying: Vec::new(),
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
                    kind: "thing".to_string(),
                }],
            },
            spatial: SpatialChannel::Chamber {
                plan: minimal_plan(),
            },
        };
        let json = snapshot_json(&snap);
        assert!(json.contains(r#""schema":"vessel/session/v2""#));
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
        // The THIRD id family on this wire, and the one a `carrying` pane
        // tracks a thing by across turns. `9223372036854775809` is 2^63 + 1:
        // emitted as a JSON number it would arrive in a browser as
        // `9223372036854776000`, so the string encoding is what makes the
        // identity survive the crossing at all.
        assert!(
            json.contains(r#""carrying":[{"entity":"9223372036854775809","noun":"a key"}]"#),
            "a carried thing's id must cross as a decimal string: {json}"
        );
    }

    #[test]
    fn floats_are_quantized_at_the_emit_boundary() {
        // 1/3 has no short decimal form; quantization pins it to 8
        // significant digits so the bytes are cross-platform stable.
        //
        // A naive `contains("0.33333333")` passes with or without
        // quantization: the RAW `f64` serializes as
        // `0.3333333333333333`, which contains that exact substring too.
        // So this asserts against the field's own bounded serialization
        // (`"grievance":<value>,` or `"grievance":<value>}`, a hard
        // boundary the raw repr's extra trailing digits cannot satisfy)
        // and separately proves the raw form really is excluded, computing
        // both reprs from `quantize` itself rather than a hand-typed
        // literal.
        let raw = 1.0 / 3.0;
        let quantized = hornvale_kernel::quantize::quantize(raw);
        let raw_repr = serde_json::to_string(&raw).unwrap();
        let quantized_repr = serde_json::to_string(&quantized).unwrap();
        assert_ne!(
            raw_repr, quantized_repr,
            "the fixture must pick a value quantization actually changes, \
             or this test cannot distinguish quantized from raw output"
        );

        let mut snap = minimal();
        snap.social[0].grievance = raw;
        let json = snapshot_json(&snap);
        let quantized_needle = format!("\"grievance\":{quantized_repr}");
        let raw_needle = format!("\"grievance\":{raw_repr}");
        assert!(
            json.contains(&quantized_needle),
            "grievance must serialize as its QUANTIZED value ({quantized_needle}): {json}"
        );
        assert!(
            !json.contains(&raw_needle),
            "grievance must not serialize as the raw, unquantized f64 \
             ({raw_needle}) — quantize_serde::f64_field must have run: {json}"
        );
    }
}

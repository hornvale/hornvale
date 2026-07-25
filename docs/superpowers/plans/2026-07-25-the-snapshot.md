# The Snapshot Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Emit one structured `vessel/session/v1` snapshot per committed turn from `windows/vessel`, expose it over two new wasm exports, and refactor the Casement's transcript pane into a pure projection of it.

**Architecture:** A new `windows/vessel/src/snapshot.rs` owns the snapshot *types* and their serializer (pure, unit-testable). `Session::snapshot()` — which must live in `session.rs`, because it reads private fields and the private `felt_phrase`/`colocated_npcs` helpers — harvests those types from live session state. The wasm crate gains `hv_snapshot_ptr`/`hv_snapshot_len` beside the existing five exports; the TypeScript client gains a pure `parseSnapshot`/`narrationOf` module and renders the transcript through it. Prose stays byte-identical to the committed native transcript throughout, and that equality is the campaign's strongest test.

**Tech Stack:** Rust 2024 (`serde` derive + `serde_json`, both already vessel dependencies); `hornvale_kernel::quantize::quantize_serde` for float fields; `hornvale_kernel::golden::assert_golden` for the byte pin; TypeScript on Deno for the client; Node for the wasm smoke driver.

## Global Constraints

- **No new dependencies.** `serde` + `serde_json` only, workspace-wide (decision 0004; enforced by `cli/tests/architecture.rs`). Vessel already has both.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (decision 0005; enforced by `clippy.toml`).
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Quantize at the emit boundary only, never in the compute path** — every `f64` that reaches JSON carries `#[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]`.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field, and variant needs a one-line doc comment.
- **No new seed draws.** This campaign adds no `Stream` derivation, so `stream_labels()` and the generated stream manifest are unchanged.
- **Schema string is exactly `vessel/session/v1`.** Additive changes are free; a meaning change mints `v2`; nothing is ever renamed (save-format contract).
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the project's most common review finding.
- **Every task that adds or changes a `pub` item with a primitive field must run the type audit before committing:**
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
  It is default-deny (any untagged pub-boundary primitive fails) and CI-enforced, and it is a **separate tool outside the workspace** — `make gate` does not run it, so a green `cargo test` proves nothing about it. The only valid `bare-ok` classes are the eleven in `tools/type-audit/src/tag.rs`: `ratio`, `count`, `index`, `constructor-edge`, `envelope`, `identifier-text`, `prose`, `artifact`, `diagnostic-value`, `render-internal`, `flag`. Anything else fails, including plausible-sounding inventions like `identifier` or `quantity`. Ratified meanings are in decision 0028. Precedents this campaign relies on: a numeric id → `index`; a schema-string const → `identifier-text` (as `ROOM_SCHEMA` and `TILES_SCHEMA` do); a name/label/key → `identifier-text`; rendered text → `prose`; a serialized blob → `artifact`; a bare day → `waiver(decision-0014: day)`.
- **Commit gate:** `make gate` (fmt + clippy + nextest + doctests). Iterate with `cargo test -p hornvale-vessel` and run the full gate once, at the end.

---

## Three spec corrections this plan makes

Writing real code against the tree caught three things the spec got wrong. Each is a deliberate deviation, not a drift:

1. **There is no top-level `felt` channel in v1.** The spec assumed the possessed agent has an affect. It does not — `Session::needs()` reads *co-located NPCs'* felt states via `affect_of`, and the player agent has no drive or affect layer at all. So the NPC's presence-gated felt read goes in `sensed.present[].felt`, and a top-level `felt` channel is deferred until the player has an interior (a later campaign).
2. **`sensed.ways` is dropped.** `sensed.room` embeds `locale/room/v2` verbatim, and that object *already carries* `exits`. `Session::ways()` is just a filter over them (`ExitKind::Edge` + `Direction::Compass`). Emitting `ways` too would be two representations of one truth, free to drift. The client filters instead.
3. **Serialization is `#[derive(Serialize)]` with quantizing field attributes, not hand-rolled.** The spec said "hand-rolled serialization in the house style"; the actual house style in `windows/scene` is serde derive plus `quantize_serde::f64_field`, with `*_json` being a one-line `serde_json::to_string`.

Amend the spec's §3 and §4 to match at close, as part of Task 6.

---

## File Structure

**Create:**
- `windows/vessel/src/snapshot.rs` — the `vessel/session/v1` types, the `SESSION_SCHEMA` constant, and `snapshot_json`. Pure: no `Session`, no world, no I/O. Unit-testable by constructing values directly.
- `windows/vessel/tests/session_snapshot.rs` — the golden byte pin, the narration byte-identity check against the committed transcript, and the determinism checks.
- `windows/vessel/tests/fixtures/session-seed-42.json` — the golden (created by `REBASELINE=1`).
- `clients/vessel/src/snapshot.ts` — `parseSnapshot` / `narrationOf`. Pure module: no DOM, no worker globals, mirroring `protocol.ts`'s existing discipline.
- `clients/vessel/src/snapshot_test.ts` — unit tests over a committed fixture string.

**Modify:**
- `windows/vessel/src/lib.rs` — declare and re-export the new module.
- `windows/vessel/src/session.rs` — add `Session::day()` and `Session::snapshot()`.
- `clients/vessel/wasm/src/lib.rs` — two new exports; refresh the snapshot buffer wherever the prose buffer is refreshed.
- `clients/vessel/wasm/drive.mjs` — assert the snapshot; fix the seed-43 hardcode.
- `clients/vessel/src/protocol.ts` — add the snapshot to the worker→page message.
- `clients/vessel/src/worker.ts` — read and forward the snapshot.
- `clients/vessel/src/main.ts` — render the transcript through `narrationOf`.
- `Makefile` — add the new golden to `rebaseline-goldens`.

---

### Task 1: The snapshot types and serializer

Pure types plus a one-line serializer. No session, no world — so its tests are fast and its shape is reviewable in isolation.

**Files:**
- Create: `windows/vessel/src/snapshot.rs`
- Modify: `windows/vessel/src/lib.rs`
- Test: inline `#[cfg(test)] mod tests` in `snapshot.rs`

**Interfaces:**
- Consumes: `hornvale_locale::Locale` (already `Serialize`), `hornvale_kernel::quantize::quantize_serde::f64_field`.
- Produces: `SESSION_SCHEMA: &str`; `SessionSnapshot { schema, turn, day, me, sensed, known, social, narration }`; `SelfChannel`, `SensedChannel`, `PresentEntry`, `KnownChannel`, `KnownEntry`, `SocialEntry`, `Narration`, `NounEntry`; `snapshot_json(&SessionSnapshot) -> String`. Task 2 constructs all of these; Task 5's TypeScript mirrors the JSON field names.

- [ ] **Step 1: Write the failing test**

Add to a new `windows/vessel/src/snapshot.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn locale() -> hornvale_locale::Locale {
        // A snapshot test must not build a world (slow, and Task 3 covers the
        // real thing). The locale window's own Default-free constructor is not
        // public, so this test round-trips the ONE field we assert on — that
        // `sensed.room` is embedded verbatim — via Task 3 instead. Here we
        // assert the envelope, so we need any Locale; take it from the
        // locale window's public sampler.
        unimplemented!("replaced in Step 3 — see note")
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
        for key in ["\"self\":", "\"sensed\":", "\"known\":", "\"social\":", "\"narration\":"] {
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
```

Note on `locale()`: constructing a bare `Locale` in a unit test requires a world. Replace the `unimplemented!()` stub in Step 3 with a real `LocaleContext` build — it is the same ~10-line builder the crate's other tests already use (see `focalize.rs`'s `seam_world`/`vantage_at`), and a `minimal()` helper that reuses it.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --lib snapshot`
Expected: compile error — `SessionSnapshot`, `SESSION_SCHEMA`, `snapshot_json` and the channel structs are not defined.

- [ ] **Step 3: Write the types, the serializer, and the test helpers**

Full contents of `windows/vessel/src/snapshot.rs` above the `#[cfg(test)]` block:

```rust
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
/// type-audit: bare-ok(count: turn), waiver(decision-0014: day)
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
/// type-audit: bare-ok(index: agent), bare-ok(index: room),
/// bare-ok(count: population), bare-ok(identifier-text: species),
/// bare-ok(identifier-text: settlement)
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
/// type-audit: bare-ok(index: entity), bare-ok(identifier-text: label),
/// bare-ok(prose: felt)
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
/// type-audit: bare-ok(index: entity), bare-ok(identifier-text: label),
/// bare-ok(ratio: grievance), bare-ok(flag: hostile)
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
```

Then replace the test module's `locale()` stub with a real builder and add `minimal()`:

```rust
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
            known: KnownChannel { entries: Vec::new() },
            social: vec![SocialEntry {
                entity: 1,
                label: "a goblin".to_string(),
                grievance: 0.0,
                hostile: false,
            }],
            narration: Narration { prose: String::new(), nouns: Vec::new() },
        }
    }
```

- [ ] **Step 4: Declare the module**

In `windows/vessel/src/lib.rs`, beside the existing `pub mod` declarations and `pub use` re-exports:

```rust
pub mod snapshot;
pub use snapshot::{
    KnownChannel, KnownEntry, Narration, NounEntry, PresentEntry, SESSION_SCHEMA, SelfChannel,
    SensedChannel, SessionSnapshot, SocialEntry, snapshot_json,
};
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel --lib snapshot`
Expected: PASS, 2 tests.

- [ ] **Step 6: Lint and commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/snapshot.rs windows/vessel/src/lib.rs
git commit -m "feat(vessel): vessel/session/v1 snapshot types and serializer"
```

---

### Task 2: `Session::snapshot()`

The harvest. It must live in `session.rs`: it reads private fields (`day`, `npcs`, `ledger`, `ctx`) and the private `felt_phrase` and `colocated_npcs` helpers, which a sibling module cannot see.

**Files:**
- Modify: `windows/vessel/src/session.rs`
- Test: inline `#[cfg(test)] mod tests` in `session.rs` (the file already has one)

**Interfaces:**
- Consumes: Task 1's `SessionSnapshot` and channel structs; the existing private `Session` fields; `crate::liveness::affect_of`; the private `felt_phrase`, `colocated_npcs`, `grievance`, and the `HOSTILITY_THRESHOLD` constant.
- Produces: `Session::day() -> WorldTime`; `Session::snapshot() -> Result<SessionSnapshot, VesselError>`; a private `turn: u64` field on `Session`, incremented once per `handle` call that is not the empty line. Task 3 and Task 4 both call `snapshot()`.

- [ ] **Step 1: Write the failing test**

Add to `session.rs`'s existing test module:

```rust
    #[test]
    fn the_opening_snapshot_carries_every_channel() {
        let world = seam_world();
        let (session, opening) = Session::start(&world, &PossessOpts::default()).unwrap();
        let snap = session.snapshot().expect("a live session snapshots");

        assert_eq!(snap.schema, crate::SESSION_SCHEMA);
        assert_eq!(snap.turn, 0, "the opening is turn 0");
        assert_eq!(snap.day, 0.5, "PossessOpts::default() is noon");
        assert!(!snap.me.species.is_empty());
        assert_eq!(snap.me.room, session.agent().position.pack().unwrap().0);
        assert!(!snap.sensed.sky.is_empty());
        assert!(!snap.known.entries.is_empty(), "the opening projection lands");
        assert_eq!(
            snap.social.len(),
            session.npc_labels().len(),
            "social covers every derived NPC, co-located or not"
        );
        assert!(
            snap.social.iter().all(|s| s.grievance == 0.0 && !s.hostile),
            "an unprovoked world starts at zero grievance"
        );
        assert_eq!(
            snap.narration.prose.trim(),
            opening.trim(),
            "narration.prose IS the opening text"
        );
    }

    #[test]
    fn the_turn_counter_advances_with_committed_turns() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        assert_eq!(session.snapshot().unwrap().turn, 0);
        session.handle("look");
        assert_eq!(session.snapshot().unwrap().turn, 1);
        session.handle("whoami");
        assert_eq!(session.snapshot().unwrap().turn, 2);
    }

    #[test]
    fn a_snapshot_is_pure_taken_twice() {
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let a = crate::snapshot_json(&session.snapshot().unwrap());
        let b = crate::snapshot_json(&session.snapshot().unwrap());
        assert_eq!(a, b, "the read is pure — no hidden state advances");
    }

    #[test]
    fn provoking_shows_up_in_the_social_channel() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let before = session.snapshot().unwrap();
        assert!(before.social.iter().all(|s| s.grievance == 0.0));
        session.handle("provoke");
        let after = session.snapshot().unwrap();
        assert!(
            after.social.iter().any(|s| s.grievance > 0.0),
            "a provoked NPC's grievance surfaces in `social`"
        );
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --lib session::tests::the_opening_snapshot`
Expected: compile error — no method `snapshot` on `Session`, no field `turn`.

- [ ] **Step 3: Add the turn counter**

In `session.rs`, add to the `Session` struct beside `day`:

```rust
    /// Commits since the possession began; 0 is the opening. Advanced by
    /// `handle` for every non-empty verb line, so the snapshot can label
    /// which turn it describes.
    /// type-audit: bare-ok(count: turn)
    turn: u64,
```

Initialize it to `0` in `Session::start`'s constructor literal, and increment it at the top of `handle` once the line is known non-empty — inside the `match verb` arm order, immediately before the match, guarded so the `""` arm does not count:

```rust
        if !verb.is_empty() {
            self.turn += 1;
        }
```

- [ ] **Step 4: Add `day()` and `snapshot()`**

In `session.rs`, beside the other accessors:

```rust
    /// The frozen day this possession observes.
    pub fn day(&self) -> WorldTime {
        self.day
    }

    /// This turn as `vessel/session/v1` — a pure read, grouped by epistemic
    /// channel (The Snapshot spec §3). Never commits, never advances the
    /// turn counter, and costs nothing on turns where no caller asks: the
    /// CLI never does, so its measured per-turn cost is unchanged.
    pub fn snapshot(&self) -> Result<SessionSnapshot, VesselError> {
        let vantage = observable(self.world, &self.ctx, &self.agent, self.day)?;
        let focalized = self.focalizer.render(&vantage);

        let terrain = LocaleTerrain::with_fields(
            &self.ctx,
            self.calendar.as_ref(),
            self.predator.as_ref(),
            self.prey.as_ref(),
        );
        let present = self
            .colocated_npcs()
            .iter()
            .map(|npc| {
                let affect = affect_of(&self.ledger, npc, &self.npcs, self.day, &terrain);
                PresentEntry {
                    entity: npc.entity.0.get(),
                    label: npc.label.clone(),
                    felt: felt_phrase(&affect),
                }
            })
            .collect();

        let social = self
            .npcs
            .iter()
            .map(|npc| {
                let g = grievance(&self.ledger, npc.entity);
                SocialEntry {
                    entity: npc.entity.0.get(),
                    label: npc.label.clone(),
                    grievance: g,
                    hostile: g >= HOSTILITY_THRESHOLD,
                }
            })
            .collect();

        let entries = self
            .knowledge
            .0
            .iter()
            .map(|(key, value)| KnownEntry {
                key: key.clone(),
                value: value.clone(),
            })
            .collect();

        Ok(SessionSnapshot {
            schema: SESSION_SCHEMA.to_string(),
            turn: self.turn,
            day: self.day.day,
            me: SelfChannel {
                agent: self.agent.id.0,
                species: self.agent.species.clone(),
                settlement: self.agent.village.name.clone(),
                population: self.agent.village.population,
                room: self
                    .agent
                    .position
                    .pack()
                    .map_err(|e| VesselError::Build(format!("{e:?}")))?
                    .0,
            },
            sensed: SensedChannel {
                room: vantage.locale.clone(),
                sky: vantage.sky.clone(),
                present,
            },
            known: KnownChannel { entries },
            social,
            narration: Narration {
                prose: focalized.prose,
                nouns: focalized
                    .nouns
                    .into_iter()
                    .map(|(noun, datum)| NounEntry { noun, datum })
                    .collect(),
            },
        })
    }
```

Add the imports the method needs to `session.rs`'s `use` block:

```rust
use crate::snapshot::{
    KnownChannel, KnownEntry, Narration, NounEntry, PresentEntry, SESSION_SCHEMA, SelfChannel,
    SensedChannel, SessionSnapshot, SocialEntry,
};
```

**Note on `RoomAddr::pack()`:** confirm the exact name and return type before writing this line (`kernel/src/room.rs` defines `RoomId` as the packed form). If packing is infallible, drop the `map_err` and the `?`. If the method is named differently, use that name — the assertion in Step 1's test must match.

**Note on `narration.prose` vs the opening:** `Session::start` returns the opening text, which is `describe_here()` — prose plus the `Ways on:` line. `focalized.prose` is only the passage. If Step 5 shows the assertion failing on the `Ways on:` suffix, that is the real contract question: make `narration.prose` carry exactly what the transcript prints by calling the same `describe_here()` the opening and `look` use, rather than `focalizer.render`. Prefer that — Task 3's byte-identity test against the committed transcript is the whole point, and it must compare like with like.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel --lib snapshot`
Expected: PASS. If `the_opening_snapshot_carries_every_channel` fails only on the prose comparison, apply the Step 4 note and re-run.

- [ ] **Step 6: Lint and commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/session.rs
git commit -m "feat(vessel): Session::snapshot() harvests vessel/session/v1"
```

---

### Task 3: The byte pin and the transcript tie

The golden, plus the check that matters most: the snapshot's narration is byte-identical to the transcript the book has published since the Casement.

**Files:**
- Create: `windows/vessel/tests/session_snapshot.rs`
- Create (via REBASELINE): `windows/vessel/tests/fixtures/session-seed-42.json`
- Modify: `Makefile`

**Interfaces:**
- Consumes: Task 2's `Session::snapshot()`, Task 1's `snapshot_json`, `hornvale_kernel::golden::assert_golden`.
- Produces: the committed fixture, and a `make rebaseline-goldens` entry for it.

- [ ] **Step 1: Write the failing test**

Create `windows/vessel/tests/session_snapshot.rs`:

```rust
//! The `vessel/session/v1` byte pin and its tie to the published
//! transcript. This fixture changing is the epoch decision point (The
//! Snapshot spec §9): regenerate deliberately, never casually, with
//! `REBASELINE=1 cargo test -p hornvale-vessel --test session_snapshot`
//! (or `make rebaseline-goldens`), then review the diff as a contract
//! change.

use hornvale_kernel::{Seed, World};
use hornvale_vessel::{PossessOpts, Session, snapshot_json};

fn world() -> World {
    hornvale_worldgen::build_world(
        Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds")
}

/// The same script the committed transcript walks, up to its first `go`.
const SCRIPT: &[&str] = &["look", "examine sky", "whoami"];

fn snapshots(world: &World) -> Vec<String> {
    let (mut session, _) = Session::start(world, &PossessOpts::default()).expect("seed 42 possesses");
    let mut out = vec![snapshot_json(&session.snapshot().unwrap())];
    for line in SCRIPT {
        session.handle(line);
        out.push(snapshot_json(&session.snapshot().unwrap()));
    }
    out
}

#[test]
fn v1_bytes_are_pinned() {
    let world = world();
    let joined = snapshots(&world).join("\n");
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/session-seed-42.json"
        )),
        &joined,
        "vessel/session/v1 bytes moved — this is the epoch decision point (The Snapshot \
         spec §9); accept deliberately and review the diff as a contract change",
    );
}

#[test]
fn the_snapshot_sequence_is_deterministic() {
    let a = snapshots(&world());
    let b = snapshots(&world());
    assert_eq!(a, b, "same seed + same script must yield the same bytes");
}

#[test]
fn narration_is_byte_identical_to_the_published_transcript() {
    // The oldest golden in this seam is the committed transcript the book
    // publishes; tying the newest channel to it is the strongest available
    // check. This is drive.mjs's own trick, in Rust.
    let md = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../book/src/gallery/possession-seed-42.md"
    ))
    .expect("the committed transcript is readable");
    let fence = "```text\n";
    let start = md.find(fence).expect("the transcript has a ```text fence") + fence.len();
    let body = &md[start..];
    let opening = &body[..body.find("\n> ").expect("the transcript has a prompt line")];

    let world = world();
    let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let snap = session.snapshot().unwrap();
    assert_eq!(
        snap.narration.prose.trim_end(),
        opening.trim_end(),
        "narration.prose must be the transcript's own opening, byte for byte"
    );
}

#[test]
fn a_settlement_free_world_refuses_possession_rather_than_panicking() {
    // Some worlds generate no settlement at all, so there is no flagship to
    // mint and no snapshot to take; the refusal must be the sim's own
    // error. SCOUTED, never hardcoded: which seeds are settlement-free is a
    // worldgen output that moves, and hardcoding one is exactly the bug that
    // left `make vessel-check` red on main (Task 4 fixes the same mistake in
    // drive.mjs — do not reintroduce it here).
    let refused = (43u64..80).find_map(|seed| {
        let w = hornvale_worldgen::build_world(
            Seed(seed),
            &Default::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("the world builds even with no settlement");
        Session::start(&w, &PossessOpts::default())
            .err()
            .map(|e| (seed, e))
    });
    let (seed, err) = refused.expect("some seed in 43..80 has no settlement");
    assert!(
        matches!(err, hornvale_vessel::VesselError::NoSettlement),
        "seed {seed} refused for the wrong reason: {err}"
    );
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --test session_snapshot`
Expected: `v1_bytes_are_pinned` FAILS — the fixture does not exist yet. The other three should pass.

- [ ] **Step 3: Create the golden and review it**

```bash
REBASELINE=1 cargo test -p hornvale-vessel --test session_snapshot
git diff --stat windows/vessel/tests/fixtures/
```

Then **read the fixture** before accepting it. Check: the schema tag is `vessel/session/v1`; all five channel keys are present and `self` is spelled `self`, not `me`; `turn` reads 0,1,2,3 across the four snapshots; `sensed.room` is a full `locale/room/v2` object with its `schema` field intact; every float is at most 8 significant digits; `social` grievances are all `0.0`. Anything else is a bug to fix before committing a contract.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel --test session_snapshot`
Expected: PASS, 4 tests.

- [ ] **Step 5: Register the golden for deliberate rebaselining**

In `Makefile`, add to the `rebaseline-goldens` target's list:

```makefile
	REBASELINE=1 cargo test -q -p hornvale-vessel --test session_snapshot
```

- [ ] **Step 6: Lint and commit**

```bash
cargo fmt
git add windows/vessel/tests/ Makefile
git commit -m "test(vessel): pin vessel/session/v1 bytes and tie narration to the published transcript"
```

---

### Task 4: The wasm exports and the smoke driver

Two new exports; the existing five untouched, so the Casement keeps working. Also fixes the red gate this task's file edits would otherwise inherit.

**Files:**
- Modify: `clients/vessel/wasm/src/lib.rs`
- Modify: `clients/vessel/wasm/drive.mjs`

**Interfaces:**
- Consumes: Task 2's `Session::snapshot()`, Task 1's `snapshot_json`.
- Produces: wasm exports `hv_snapshot_ptr() -> *const u8` and `hv_snapshot_len() -> usize`. Task 5's worker reads both.

- [ ] **Step 1: Add the snapshot buffer and exports**

In `clients/vessel/wasm/src/lib.rs`, beside the existing output buffer statics, add a snapshot buffer and a setter, then refresh it everywhere the prose buffer is set — in `hv_start` after a successful possession, and in `hv_handle` after a turn. Follow the file's existing pattern for the out buffer exactly (same static shape, same `unsafe` discipline, same doc-comment density).

```rust
/// The current turn's `vessel/session/v1` JSON, which JS reads via
/// `hv_snapshot_ptr`/`hv_snapshot_len`. Empty when there is no live
/// possession, or when the snapshot read itself failed — the client then
/// degrades to the prose transcript rather than to a blank pane.
static mut SNAPSHOT: String = String::new();

/// Replace the snapshot buffer from the live possession, if any.
fn set_snapshot() {
    let json = current_session()
        .and_then(|s| s.snapshot().ok())
        .map(|snap| hornvale_vessel::snapshot_json(&snap))
        .unwrap_or_default();
    unsafe { SNAPSHOT = json };
}

/// Pointer to the current turn's snapshot JSON (UTF-8, `hv_snapshot_len`
/// bytes). Zero-length means "no snapshot this turn".
#[unsafe(no_mangle)]
pub extern "C" fn hv_snapshot_ptr() -> *const u8 {
    unsafe { SNAPSHOT.as_ptr() }
}

/// Length in bytes of the current snapshot JSON.
#[unsafe(no_mangle)]
pub extern "C" fn hv_snapshot_len() -> usize {
    unsafe { SNAPSHOT.len() }
}
```

**Note:** the existing file holds the possession in a static (`Possession`, with the world held raw so re-possession can reclaim it). Reuse whatever accessor it already has instead of inventing `current_session()` — read the file first and match its idiom, including its `unsafe` conventions. Call `set_snapshot()` at the end of `hv_start`'s success path and at the end of `hv_handle`, and clear it in the teardown path.

- [ ] **Step 2: Build to verify it compiles**

Run: `make wasm-vessel`
Expected: builds; `book/src/gallery/vessel.wasm` is refreshed (gitignored per decision 0052).

- [ ] **Step 3: Fix the red gate and assert the snapshot**

`drive.mjs` currently asserts seed-43 possession succeeds, but seed 43 has no settlements, so `make vessel-check` is red on `main`. Replace the hardcoded seed with a scout so the test exercises the teardown path it means to, not a geography accident. In `clients/vessel/wasm/drive.mjs`, replace step 6's seed-43 block:

```js
// 6. Re-possession with a DIFFERENT possessable seed (exercises teardown).
// Scouted, not hardcoded: many seeds generate no settlement at all, so
// `hv_start` returns 2 (possession refused) for them — that is a valid
// world, not a bug, and hardcoding one made this check fail whenever
// worldgen moved. 43 and 45 are both settlement-free today.
let other = null;
for (let seed = 43n; seed < 60n; seed++) {
  if (hv_start(seed) === 0) { other = seed; break; }
}
assert.notEqual(other, null, "some seed in 43..60 is possessable");
assert.notEqual(readOut(), golden, "a different seed is a different world");
```

Then extend the driver to check the snapshot. Add after step 7 (the return to seed 42):

```js
// 8. The snapshot rides alongside the prose, and its narration IS the prose.
const snapshotJson = () =>
  new TextDecoder().decode(
    new Uint8Array(memory.buffer, hv_snapshot_ptr(), hv_snapshot_len()),
  );
assert.ok(hv_snapshot_len() > 0, "a live possession carries a snapshot");
const snap = JSON.parse(snapshotJson());
assert.equal(snap.schema, "vessel/session/v1");
for (const key of ["self", "sensed", "known", "social", "narration"]) {
  assert.ok(key in snap, `snapshot carries the ${key} channel`);
}
assert.equal(
  snap.narration.prose.trimEnd(),
  golden.trimEnd(),
  "narration.prose === the transcript opening the prose ABI already returns",
);
```

Add `hv_snapshot_ptr` and `hv_snapshot_len` to the destructured `instance.exports` at the top of the file.

- [ ] **Step 4: Run the smoke driver to verify it passes**

Run: `node clients/vessel/wasm/drive.mjs book/src/gallery/vessel.wasm`
Expected: `casement smoke OK — <N> KiB wasm, seed-42 genesis <N> ms`, exit 0. This is also the first time `make vessel-check` can pass since seed 43 lost its settlements.

- [ ] **Step 5: Lint and commit**

```bash
cargo fmt --manifest-path clients/vessel/wasm/Cargo.toml
cargo clippy --manifest-path clients/vessel/wasm/Cargo.toml --target wasm32-unknown-unknown -- -D warnings
git add clients/vessel/wasm/src/lib.rs clients/vessel/wasm/drive.mjs
git commit -m "feat(vessel-wasm): expose the session snapshot; scout a possessable seed in the smoke driver"
```

---

### Task 5: The client renders through the snapshot

**Files:**
- Create: `clients/vessel/src/snapshot.ts`
- Create: `clients/vessel/src/snapshot_test.ts`
- Modify: `clients/vessel/src/protocol.ts`
- Modify: `clients/vessel/src/worker.ts`
- Modify: `clients/vessel/src/main.ts`

**Interfaces:**
- Consumes: Task 4's two wasm exports; the existing `WorkerResponse` union in `protocol.ts`.
- Produces: `Snapshot` interface; `parseSnapshot(json: string): Snapshot | null`; `narrationOf(snap: Snapshot): string`; `waysOf(snap: Snapshot): { dir: string; room: number }[]`. `main.ts` consumes `narrationOf`; `waysOf` exists to prove the client filters exits itself rather than needing a `ways` field.

- [ ] **Step 1: Write the failing test**

Create `clients/vessel/src/snapshot_test.ts`:

```ts
import { assertEquals } from "jsr:@std/assert";
import { narrationOf, parseSnapshot, waysOf } from "./snapshot.ts";

// A minimal fixture in the real schema's shape. Kept small on purpose: the
// full-fidelity byte pin is Rust's (windows/vessel/tests/fixtures).
const FIXTURE = JSON.stringify({
  schema: "vessel/session/v1",
  turn: 0,
  day: 0.5,
  self: { agent: 1, species: "bugbear", settlement: "X", population: 118, room: 7 },
  sensed: {
    room: {
      schema: "locale/room/v2",
      id: 7,
      exits: [
        { kind: "Edge", direction: { Compass: "Se" }, to: 8 },
        { kind: "Vertical", direction: "Exit", to: 9 },
      ],
    },
    sky: "Night.",
    present: [],
  },
  known: { entries: [] },
  social: [],
  narration: { prose: "You stand in a wood.\nWays on: SE.", nouns: [] },
});

Deno.test("parseSnapshot accepts a v1 payload", () => {
  const snap = parseSnapshot(FIXTURE);
  assertEquals(snap?.schema, "vessel/session/v1");
  assertEquals(snap?.turn, 0);
});

Deno.test("parseSnapshot rejects junk and a wrong schema rather than throwing", () => {
  assertEquals(parseSnapshot("not json"), null);
  assertEquals(parseSnapshot(""), null);
  assertEquals(parseSnapshot(JSON.stringify({ schema: "vessel/session/v2" })), null);
});

Deno.test("narrationOf returns the prose verbatim", () => {
  assertEquals(narrationOf(parseSnapshot(FIXTURE)!), "You stand in a wood.\nWays on: SE.");
});

Deno.test("waysOf filters compass edges, so no `ways` field is needed", () => {
  assertEquals(waysOf(parseSnapshot(FIXTURE)!), [{ dir: "Se", room: 8 }]);
});
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd clients/vessel && deno task test`
Expected: FAIL — `./snapshot.ts` does not exist.

- [ ] **Step 3: Write the module**

Create `clients/vessel/src/snapshot.ts`:

```ts
// The `vessel/session/v1` reader. Pure module: no DOM, no worker globals —
// everything here is unit-tested, matching protocol.ts's discipline.
//
// Every pane is a pure function of one snapshot (The Snapshot spec §3), and
// the snapshot is grouped by epistemic channel, so a pane reads one channel
// and cannot see outside it.

/** The schema tag this client understands. A different tag is refused. */
export const SESSION_SCHEMA = "vessel/session/v1";

/** One exit as `locale/room/v2` carries it. */
export interface Exit {
  kind: string;
  direction: { Compass: string } | string;
  to: number;
}

/** One turn, as the sim emitted it. Only the fields the client reads. */
export interface Snapshot {
  schema: string;
  turn: number;
  day: number;
  self: {
    agent: number;
    species: string;
    settlement: string;
    population: number;
    room: number;
  };
  sensed: {
    room: { schema: string; id: number; exits: Exit[] };
    sky: string;
    present: { entity: number; label: string; felt: string }[];
  };
  known: { entries: { key: string; value: string }[] };
  social: { entity: number; label: string; grievance: number; hostile: boolean }[];
  narration: { prose: string; nouns: { noun: string; datum: string }[] };
}

/** Parse a snapshot payload, or null if it is absent, junk, or a schema
 * this client does not understand. Never throws: a client that cannot read
 * the snapshot degrades to the prose transcript, which always works. */
export function parseSnapshot(json: string): Snapshot | null {
  if (json.length === 0) return null;
  let parsed: unknown;
  try {
    parsed = JSON.parse(json);
  } catch {
    return null;
  }
  const snap = parsed as Snapshot;
  return snap?.schema === SESSION_SCHEMA ? snap : null;
}

/** The prose this turn prints. Carried verbatim from the sim — the client
 * never re-derives narration from structure (decision 0022). */
export function narrationOf(snap: Snapshot): string {
  return snap.narration.prose;
}

/** The lateral ways on, filtered from the embedded room's own exits. The
 * snapshot carries no `ways` field on purpose: `locale/room/v2` already
 * owns exits, and two representations of one truth would drift. */
export function waysOf(snap: Snapshot): { dir: string; room: number }[] {
  return snap.sensed.room.exits
    .filter((e) => e.kind === "Edge" && typeof e.direction === "object")
    .map((e) => ({
      dir: (e.direction as { Compass: string }).Compass,
      room: e.to,
    }));
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cd clients/vessel && deno task test`
Expected: PASS, 4 new tests plus the existing protocol and transcript tests.

- [ ] **Step 5: Thread the snapshot through the worker protocol**

In `protocol.ts`, add the field to both worker→page responses that carry a turn, so a pane always has the snapshot that matches the text beside it:

```ts
/** Worker -> page: genesis succeeded; text is the opening. */
export interface StartedResponse {
  type: "started";
  text: string;
  /** The opening turn's `vessel/session/v1` JSON; "" if unavailable. */
  snapshot: string;
}

/** Worker -> page: one verb's response; released ends the possession. */
export interface OutResponse {
  type: "out";
  text: string;
  released: boolean;
  /** This turn's `vessel/session/v1` JSON; "" if unavailable. */
  snapshot: string;
}
```

In `worker.ts`, add `hv_snapshot_ptr`/`hv_snapshot_len` to the destructured exports, add a `readSnapshot()` helper mirroring the existing `readOut()`, and include `snapshot: readSnapshot()` in every `started` and `out` message it posts.

- [ ] **Step 6: Render the transcript through the projection**

In `main.ts`, where a `started`/`out` response's `text` is appended to the transcript, prefer the projection and fall back to the raw text:

```ts
const snap = parseSnapshot(msg.snapshot);
const text = snap ? narrationOf(snap) : msg.text;
```

Import `narrationOf` and `parseSnapshot` from `./snapshot.ts`. **The rendered output must be visually identical** — that is the whole point of this task, and Step 7 checks it.

- [ ] **Step 7: Verify the client gate and that nothing changed on screen**

Run: `cd clients/vessel && deno fmt && deno lint && deno task check && deno task test && deno task build`
Expected: all green; `deno task build` refreshes the committed `book/src/gallery/vessel.js` and `vessel-worker.js`.

Then confirm the pane is unchanged: `make wasm-vessel && mdbook serve book`, open the "A Possession, Live" chapter, possess seed 42, and check the opening text matches `book/src/gallery/possession-seed-42.md` exactly. A subagent cannot see a rendered page — if you are one, say so and leave this for the controller rather than claiming it passed.

- [ ] **Step 8: Commit**

```bash
git add clients/vessel/src/ book/src/gallery/vessel.js book/src/gallery/vessel-worker.js
git commit -m "feat(vessel-client): render the transcript as a projection of the session snapshot"
```

---

### Task 6: Bookkeeping and the gate

**Files:**
- Modify: `docs/superpowers/specs/2026-07-25-the-snapshot-design.md`
- Modify: `book/src/frontier/idea-registry.md`
- Create: `book/src/chronicle/the-snapshot.md`
- Modify: `book/src/SUMMARY.md`

- [ ] **Step 1: Amend the spec with the three corrections**

Edit §3 and §4 of the spec so they match what shipped: no top-level `felt` channel (with the reason — the possessed agent has no affect; NPC felt lives in `sensed.present[].felt`); no `ways` field (the embedded room owns exits); and serde-derive-with-quantizing-attributes rather than hand-rolled JSON. Keep the original reasoning visible — amend, do not silently rewrite.

- [ ] **Step 2: Flip the registry rows**

In `book/src/frontier/idea-registry.md`, change `CLIENT-one-snapshot` and `CLIENT-redaction-panes` from `raw` to `shipped` and repoint **Where** at the chronicle entry. Never delete a row.

- [ ] **Step 3: Write the chronicle entry**

Create `book/src/chronicle/the-snapshot.md` at the book's altitude — technical and mathematical, comprehensible without reading the code, no registry IDs (the drift check bans them outside `frontier/`). Cover: why prose alone could not feed a pane; what channel-grouping buys over provenance tags; the narration-equals-transcript tie; and the three corrections this campaign's implementation forced on its own spec. Add it to `book/src/SUMMARY.md`.

- [ ] **Step 4: Run the full gate**

```bash
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
make gate
make vessel-check
cargo test -p hornvale --test docs_consistency
mdbook build book
```

Expected: all green. `make vessel-check` passing is itself a fix — it was red on `main` before Task 4.

- [ ] **Step 5: Verify the generated artifacts are current**

```bash
git status --short
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Expected: only intended changes. If a committed artifact drifted, regenerate it via the CI step list in `.github/workflows/ci.yml` rather than hand-editing.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "docs(the-snapshot): chronicle, spec amendments, registry rows"
```

---

## Self-review

**Spec coverage.** §2 goal → Tasks 1–5. §3 schema → Task 1, with three documented corrections. §4 producer → Task 2. §5 ABI → Task 4. §6 client → Task 5. §7 testing: golden → T3 S3; narration byte-identity → T3 S1; determinism → T2 S1 and T3 S1; unpossessed-world path → T3 S1; Deno units → T5 S1; wasm smoke → T4 S3; `drive.mjs` fix → T4 S3; turn-cost guard → **partially covered** (T2's `snapshot()` is a separate call the CLI never makes, so the no-snapshot path is unchanged by construction; the `timed.sh` before/after row is left to review rather than given a step, because the full ratchet is explicitly a different campaign). §8 non-goals → nothing in any task touches the tile layer, vitality, verbs, or `windows/scene`. §9 flags → G3-cleared. §10 DoD → Task 6.

**Placeholder scan.** One deliberate `unimplemented!()` appears in Task 1 Step 1 and is replaced within the same task at Step 3, with the reason stated inline. Two "confirm before writing" notes remain — `RoomAddr::pack()`'s exact name/fallibility (T2 S4) and the wasm crate's existing possession accessor (T4 S1) — both because the plan should not invent an API name it has not read; each names the file to check and what the assertion must match.

**Type consistency.** `SessionSnapshot.me` serializes as `self` (T1) and is read as `snap.self` in TypeScript (T5) and `snap.self` in `drive.mjs` (T4) — consistent. `snapshot_json` is the name in T1, T2's tests, T3, and T4. `PresentEntry.felt` is a `String` in Rust and `string` in TS. `SocialEntry.grievance` is quantized `f64` → `number`. `waysOf` reads `sensed.room.exits`, which exists because `sensed.room` is a verbatim `locale/room/v2`; the fixture in T5 S1 mirrors that shape.

# The Signet — design

**Campaign:** The Signet (slug-named, decision 0026) · **Date:** 2026-08-09 ·
**Sequence:** the third and last — The Scaffold (merged) → The Salt (merged) →
**The Signet**. Unblocks The Particular.

## 1. Why

`Ledger::mint_entity` is a bare counter:

```rust
self.next_entity += 1;
```

An entity's id therefore *is* its position in mint order, and mint order is a
property of the whole build rather than of the entity. Inserting a stage
renumbers everything downstream of it. That is not a theoretical complaint —
it is measured. Absorbing main into The Particular on 2026-08-09 promoted 117
persons and moved six committed fixtures:

```
  windows/vessel/.../session-seed-42                537-543 -> 654-660   52589 -> 52589 bytes
  windows/vessel/.../snapshot-seed-42-walk          537-543 -> 654-660   13400 -> 13400 bytes
  windows/vessel/.../snapshot-seed-42-chamber       537-543 -> 654-660    8513 ->  8513 bytes
  windows/vessel/.../snapshot-seed-1-chamber-occ.   505-511 -> 637-643    7795 ->  7795 bytes
  clients/game/core/.../session-seed-42-turn-0      537-542 -> 654-659   13231 -> 13231 bytes
  clients/game/core/.../session-seed-42-chamber     537-542 -> 654-659    8306 ->  8306 bytes
```

**Byte length identical in all six; every entity displaced by exactly the
number of persons minted ahead of it.** Equal bytes with shifted ids is the
signature of a positional identifier doing a stable identity's job.

The count is growing while the defect waits: The Quire committed the two
`clients/game` fixtures during the eight days The Particular sat parked,
taking the count from four to six. Every campaign that commits a world-derived
fixture adds one.

The Scaffold split the bake's private handle from the ledger's permanent one.
The Salt stopped derived prose reading an id for its value. This campaign
changes the derivation itself, which is the only step that removes the churn
rather than relocating it.

## 2. What an id becomes

An id stops being a position in a global sequence and becomes a function of
the entity's **derivation path** — its parent, the role it fills for that
parent, and which sibling it is.

```
  id : NonZeroU64
       +-------------------------------- 48 bits ------------------+-- 16 --+
       |  path hash: derive(parent_id, role_label)                 | ordinal|
       +-----------------------------------------------------------+--------+
```

- **Path hash (48 bits)** — `Seed::derive` over the parent id and the role
  label, using the kernel's existing labelled-derivation primitive rather than
  a new hash. The label is declared as a `pub const` in the owning crate's
  `streams` module like every other save-format contract, and reaches the
  generated stream manifest through `stream_labels()`.
- **Ordinal (16 bits)** — which sibling this is among that parent's children
  *in that role*. Caller-supplied, because the caller is the code that knows
  "this is the third occupation of this settlement"; the ledger has no
  semantic basis for guessing it. 65,536 siblings per (parent, role).
- **Roots** — entities with no parent derive from a synthetic root:
  `derive(world.seed, "root", role)`. A world's own seed is already its
  identity, so roots need nothing further.

**Siblings share their high 48 bits and differ only in the low 16.** That is
deliberate: ids become inspectable, and a lineage is legible in a hex dump.

### Why not the material facts

The obvious derivation — hash what the entity *is* — is wrong here, and
`cli/tests/id_shift_invariance.rs:173` says so:

```rust
assert_ne!(member.id, first.id, "group members must carry different entity ids");
```

Seed 42 contains occupations with **identical material cores** (3 colliding
material groups, 7 records; 29 colliding founding-key groups, 61 records). A
material hash would give them one id and silently collapse two settlements
into one entity. Deriving from the path instead means the id depends on **no
material fact at all**, so those groups keep distinct ids and the test stays
non-vacuous. Its own anti-vacuity assert is the guard if this is ever violated.

## 3. Two fields, two jobs

`next_entity` does not go away; it stops being identity.

| field | job | shown |
|---|---|---|
| `EntityId` | durable identity, derived from lineage | never, raw |
| `next_entity` | an accession count — how many entities exist | never |

This is the archival distinction between an *accession number* (order of
arrival, meaningless, never reused) and a *call number* (derived from
provenance). The current code uses the accession number as the call number,
which is the defect stated in one sentence.

`minting_is_valid()` — today `next_entity >= max_entity_id()` — becomes
meaningless under derived ids and is **replaced, not deleted**: the ledger
asserts at mint that the derived id is not already in use.

**A collision fails loudly**, in the house style of pins. With 48 bits of path
hash over ~10^3 entities per world (seed 42 carries 536, or 653 after
promotion), cross-parent collision odds are ~1.8e-9 per world; siblings cannot
collide at all, since they differ by construction in the ordinal field. The
assert converts the residual case from silent corruption into a reproducible
panic.

## 4. The display handle

`windows/vessel/src/session.rs:3151` prints the raw id beside a label that
already exists:

```rust
lines.push(format!("  [{}] {}", npc.entity.0, npc.label));
```

and the id is **functional input**, not decoration — `"Why what? Name an NPC
(label or id — see 'npcs')"`, parsed at `session.rs:1104` and `:3170`. A
derived 64-bit id is unusable to type, so the printed number becomes a
**session-local ordinal** (1..n over the `npcs` listing): short-lived, stable
within a session, and explicitly *not* an `EntityId`. Label-substring matching
is unchanged.

This is the same handle-confusion The Scaffold removed from the history bake,
one layer out: a short-lived display handle wearing a permanent type. It
closes the family named by `SIG-agentid-entityid-confusion`.

## 5. Save format and the JSON boundary

**This is an epoch.** Every entity id in every saved world changes. Decision
0089 warns against *empty* epochs — epochs whose measurement comes back
byte-identical. This one cannot be empty: section 1 is the measurement, and it
moves.

**Ids must be string-encoded where they cross to JavaScript.** A full-width
64-bit id exceeds JS's 2^53 exact-integer range, so any scene or session JSON
carrying one as a bare number loses precision in the browser. This is forced
by the id becoming wide, not by a choice in this spec, and **the precedent
already exists in the same schema**: `AgentId(pub u64)` is a derived 64-bit id
and `vessel/session/v1` already serializes it as a string —
`"agent":"9947299063136102849"`. Copy that, do not invent. Scene schemas are
cross-repo contracts (decision 0055): additive-or-versioned only.

## 6. Preregistered measurement

Frozen before the code that would move it (decision 0016).

- **P1 (the headline, falsifiable).** Build seed 42 twice — once as today,
  once with an extra entity-minting stage inserted before the vessel's NPCs —
  and every entity id **not** in the inserted stage's lineage is identical.
  Today this prediction fails on six fixtures by construction; after The
  Signet it must hold on all six. This is a direct re-run of the probe in
  section 1, with "equal bytes + shifted ids" becoming "equal bytes +
  identical ids".
- **P2 (verification, not prediction).** `id_shift_invariance.rs` still
  reports at least one colliding material group, i.e. the new derivation has
  not vacuumed the keystone test out. Expected: unchanged at 3 material groups
  / 29 founding-key groups, since the derivation reads no material fact.
- **P3 (the artifact diff is readable).** Per The Salt's handoff, the prose
  files that move should be exactly `book/src/gallery/possession-seed-42.md`
  and `possession-over-time-seed-42.md`. **Anything else that moves is a
  channel The Salt missed, and is the cheapest possible signal that one
  exists.** Recorded as a finding, not fixed by retuning.
- **P4 (the honest limit, stated in advance).** Reordering entities *within*
  one lineage still moves that lineage's ids. The Signet contains the blast
  radius to a lineage; it does not abolish it. A campaign that reorders a
  settlement's own occupations will still churn that settlement's ids, and
  that is correct behaviour rather than a residual bug.

**No predicted value is offered for how many ids change on the epoch**
(trivially: all of them). Following The Scaffold, a measurement with no
predicted value still freezes the *definition*, which is what makes a later
drift check meaningful.

## 7. Testing

- **The insertion test is new and is the campaign's acceptance test** — P1
  above, as a test rather than a manual probe, so the property is guarded
  after the campaign ends rather than demonstrated once during it.
- **A forced-collision unit test** proving the mint-time assert fires. Per the
  mutation-proof rule, it must assert the collision was actually constructed,
  not merely that the code path exists.
- `id_shift_invariance.rs` runs unchanged. It is the keystone and must not be
  edited to accommodate the new derivation; if it goes red, the derivation is
  wrong.
- The three occupation decoders still agree. `layer_key`'s ties rest on
  `sort_by_key`'s stability over *commit* order, which no id-derivation change
  moves — the property that makes a tie-ing material key safe here, stated in
  `layer_key`'s doc and enforced by nothing
  (`SIG-layer-key-ties-rest-on-a-stable-sort`).
- Full artifact regeneration, with every gallery diff read **as prose**.
- `make game-check` and `make vessel-check` alongside `make gate`: the
  `clients/` fixtures are outside the cargo workspace and the gate cannot see
  them.

## 8. Non-goals

- **Widening `EntityId` beyond 64 bits.** 48+16 is sufficient at 10^3 entities
  per world by four orders of magnitude, and `EntityId(pub NonZeroU64)` is the
  most-used type in the system — `fact_index.rs`'s three `BTreeMap` keys,
  `Value::Entity`, every domain, the wasm ABI, the type-audit tags. Widening
  it would bury this campaign's artifact diff in exactly the noise The Salt
  spent a campaign clearing so that this diff could be read.
- **Self-describing ids** — encoding a domain tag, epoch, or schema version in
  spare bits so ids survive format changes legibly. A real design position and
  a different campaign's ambition; captured as a registry row instead.
- **Unparking The Particular.** That branch is absorbed and green as of
  2026-08-09 and resumes at its Task 3 *after* this lands.
- Re-pinning the six fixtures as an end in itself. They re-pin once here, as
  the epoch, and the point is that they stop moving afterwards.

## 9. Definition of Done

`make gate`, `make game-check`, `make vessel-check` green; artifacts
regenerated and diffed as prose; the four items in section 6 scored with any
falsification stated plainly; chronicle entry in `book/src/chronicle/`;
retrospective in `docs/retrospectives/` (decision 0020); registry rows flipped
with **Where** pointing at the chronicle; Confidence Gradient re-scored or an
explicit statement that no bet moved (decision 0030).

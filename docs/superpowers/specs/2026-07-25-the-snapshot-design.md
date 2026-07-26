# The Snapshot — Design

**Date:** 2026-07-25
**Status:** Shipped. **Amended 2026-07-25 post-implementation** — four §3/§4
claims were wrong and are corrected in place; each correction is recorded with
the original claim still visible under
[§3 What implementation corrected](#what-implementation-corrected-amended-2026-07-25).
This document is a corrected record, not a revision that pretends it was always
right.
**Campaign:** The Snapshot (Campaign 1 of the Rose Window program)
**Parent spec:** `2026-07-25-the-rose-window-metaplan-design.md` (§3.4 is the law this campaign implements; §6.1 its carve)
**Worktree:** `the-rose-window` (branch `the-rose-window`), off `main` at `3caf9055`
**Autopilot:** engaged (G3/G6 hard stops)
**Scope class:** producer-side schema addition + ABI widening + client refactor. **No new physics, no new seed draws, no new verbs, no epoch.**

---

## 1. Problem

The Casement's wasm ABI is prose-only: UTF-8 command bytes into a
4096-byte buffer, rendered text out by `hv_out_ptr`/`hv_out_len`. Meanwhile
`windows/vessel`'s `Session` already holds the structured truth in Rust —
`focalized()`, `ways()`, `agent()`, `knowledge()`, `context()`,
`npc_labels()`, `npc_grievance(who)`, `would_turn_hostile(who)`.

So every pane the Rose Window wants — a map, a status readout, an entity
list, a knowledge view — is blocked on the *producer*, not on JS. A client
cannot parse "You stand in tropical seasonal forest — buttressed canopy — in
the lands of Qvooshtvoagootao. Ways on: SE, N, SW." back into data it can
draw, and it must never try: that would make the client authoritative over an
interpretation the sim already owns.

## 2. Goal

Ship `vessel/session/v1`: **one structured emit per committed turn**, with
every datum grouped by the epistemic channel it belongs to; two new wasm
exports beside the existing five; and the Casement's transcript pane
refactored into a pure projection of it.

**Nothing new becomes playable. What is playable becomes legible.**

The prose ABI is retained unchanged, and the seed-42 opening stays
byte-identical to the committed native transcript. This campaign must not be
visible to a reader of the book except as a chapter that still works.

## 3. The schema: grouped by channel, not by type

The metaplan (§3.4) requires provenance per datum. Rather than tag each
field with a provenance marker, **the snapshot is organized by epistemic
channel**, which makes provenance structural: a pane reads one channel and
physically cannot see outside it.

The shape below is **as shipped** (corrected 2026-07-25; the four claims the
original draft got wrong, and why, are recorded immediately after the
per-decision notes):

```
{
  "schema": "vessel/session/v1",
  "turn":   3,                     // advances per non-empty verb line, not per commit
  "day":    0.5,                   // WorldTime, quantized at emit

  "self":   { "agent": 7225590595188407000,
              "species": "bugbear",
              "settlement": "Qvooshtvoagootao",
              "population": 118,
              "room": 738918402 },

  "sensed": { "room": <locale/room/v2 object, embedded verbatim — its
                       own `exits` are the authoritative ways on>,
              "sky":  "...",       // the sky provider's own rendering
              "present": [ { "entity": N, "label": "a goblin",
                             "felt": "is content" } ] },

  "known":  { "entries": [ { "key": "room/738918402", "value": "..." }, ... ] },

  "social": [ { "entity": N, "label": "a goblin",
                "grievance": 0.0, "hostile": false } ],

  "narration": { "prose": "<this turn's own response text, verbatim>",
                 "nouns": [ { "noun": "sky", "datum": "..." }, ... ] }
}
```

Four decisions inside that shape:

**`vessel/session/v1`, not `scene/session/v1`.** Schema families are named
after their owning window or domain — `locale/room/v2`,
`religion/deity/v2`, `settlement/name/v2` — and the `scene/*` family is
uniformly *world*-derived and static per query. A session snapshot is
session-derived and per-turn, and putting it in `windows/scene` would add a
`scene → vessel` window edge for nothing. `windows/vessel` owns it.

**`narration` carries the prose verbatim, and this duplication is
deliberate.** Prose is the constitutional primary (§3.5) and must stay
byte-identical to the native transcript, so the snapshot carries the rendered
text *and* the structure it was rendered from. The client never re-derives
prose from structure — that is the sim's rendering, and decision 0022 keeps
it there.

**`sensed.room` embeds the existing `locale/room/v2` object verbatim**
rather than re-describing it. One schema, one owner; if the room schema
mints a v3, this snapshot carries v3 and its own version is unaffected.

**`social` is a channel of its own, not part of `sensed`.** A grievance is
not a sense datum — it is `COMMIT`-tier, entity-keyed, placeless (metaplan
§3.2), and it survives leaving the room. Grouping it under `sensed` would
imply it evaporates with presence, which is exactly the confusion the
metaplan's position law exists to prevent.

### What implementation corrected (amended 2026-07-25)

This section was written before the code. Four of its claims turned out to be
wrong, and the corrections are recorded here with the original wording quoted
so the reasoning that produced the error stays visible. All four are
*subtractions or reattributions*, not scope changes: nothing the spec promised
went unshipped.

**1. There is no top-level `felt` channel.** The draft's example carried

> `"felt": { "label": "content", "valence": ..., "arousal": ..., "about": "..." }`

as a peer of `sensed` and `social`, on the assumption that the possessed agent
has an affect to read. **It does not.** The player has no drive layer and no
affect layer at all — `Session::needs()` reads the *co-located NPCs'* felt
states via `affect_of`, so what the draft mistook for the player's interior was
always a presence-gated read of somebody else's. It therefore belongs inside the
presence-gated channel, and ships as `sensed.present[].felt`: one felt string
per co-located creature, evaporating with presence exactly as the position law
requires. A player interior — a drive/affect layer the possessed agent actually
owns — is a later campaign, and when it exists a top-level `felt` channel is the
right home for it. The draft was not wrong about the shape; it was wrong about
whose interior existed.

**2. There is no `ways` field.** The draft's example carried

> `"ways": [ { "dir": "SE", "room": 738918402 }, ... ]`

beside `sensed.room`. But `sensed.room` embeds `locale/room/v2` *verbatim* — the
very decision recorded three paragraphs above — and that object already carries
`exits`. `Session::ways()` is not a source of truth; it is only a filter over
those exits (`ExitKind::Edge` + `Direction::Compass`). Emitting both would put
two representations of one truth in one document, which is precisely the drift
the "one schema, one owner" decision exists to prevent. So the snapshot emits
the exits once and **the client filters**, in `waysOf()` in
`clients/vessel/src/snapshot.ts`, applying the same two predicates
`Session::ways()` does. This is the embed decision taken seriously; the draft
had stated the principle and then violated it in its own example.

**3. `narration.prose` is the turn's own response text, not the focalized room
block.** The draft's example showed

> `"prose": "You stand in ...\nWays on: SE, N, SW."`

which is `describe_here()`'s output. That would have been a bug. `describe_here()`
serves only `look`, `go`, `back`, and the opening; `Session` records `last_text`
— the opening, then each verb's response — and the snapshot carries **that**,
verbatim. Emitting the room block unconditionally would make the client print
the room description on a `whoami` turn, i.e. the transcript pane would stop
being a projection of the session and start being a projection of the *room*.
Verified empirically over the 13-turn walker script: 8 of those turns return
text that is not the room block. The prose channel is the transcript, and the
transcript is what the player was just told.

**4. Serialization is serde-derive plus quantizing field attributes.** §4 below
described `snapshot_json` as "hand-rolled serialization in the house style".
That misidentified the house style. What shipped is `#[derive(Serialize)]` on
every channel struct with
`#[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]`
on each `f64` — which is what `windows/scene` already does, and therefore *is*
the house style. The constitutional requirement was never "write the JSON by
hand"; it was "quantize at the emit boundary and nowhere else", and a
field-level `serialize_with` attribute discharges that requirement more legibly
than a hand-rolled writer, because the boundary is declared on the field it
governs rather than reimplemented in a function that could forget one.

## 4. The producer

`windows/vessel` gains a `snapshot` module:

- `pub struct SessionSnapshot` and its channel structs, each field documented
  (`#![warn(missing_docs)]` is workspace-wide).
- ~~`pub fn snapshot(session: &Session) -> Result<SessionSnapshot, VesselError>`~~
  **Amended (final review fix wave, 2026-07-25):** shipped as a method,
  `Session::snapshot(&self) -> Result<SessionSnapshot, VesselError>`, in
  `session.rs`, not a free function in the `snapshot` module — that module
  holds only the channel types and `snapshot_json`. Still a pure read over
  accessors that already exist. **No new seed draws, so the stream manifest
  is unchanged.**
- `pub fn snapshot_json(snap: &SessionSnapshot) -> String` — ~~hand-rolled
  serialization in the house style~~, with every float through
  `hornvale_kernel::quantize` at the emit boundary and nowhere else.
  **Amended (correction 3, §3):** the serialization is `#[derive(Serialize)]`
  on each channel struct plus a `quantize_serde::f64_field` `serialize_with`
  attribute on every `f64`, as `windows/scene` does; `snapshot_json` is a
  one-line `serde_json::to_string`. The quantize-at-emit-only requirement is
  unchanged and is what the attributes discharge.

~~`Session::handle` is untouched.~~ **Amended (final review fix wave,
2026-07-25):** it is touched — `handle` gains a `turn` counter increment and
a `last_text` assignment (guarded to non-empty verb lines only, so a blank
line clobbers neither the counter nor the last narration). The snapshot
*read* is still taken *after* a turn commits, by the caller, so ~~the turn
path costs nothing when nobody asks for one~~ **Amended:** the turn path
costs one `u64` increment and one short `String` clone per turn, whether or
not any caller ever asks for a snapshot. That cost is small against the
measured 0.46 ms `look` and 1.15 ms movement figures, so the conclusion (no
CLI regression) survives — but "costs nothing" overstated it. **§7's
`scripts/timed.sh` turn-cost ledger step was not performed**; the cost above
was accepted analytically (the increment/clone argument, weighed against the
measured per-turn figures) rather than measured directly.

## 5. The wasm ABI

Two new exports; the existing five unchanged, so the Casement's smoke driver
keeps passing:

```
  hv_snapshot_ptr() -> *const u8     the current turn's JSON
  hv_snapshot_len() -> usize
```

Refreshed by `hv_start` and by `hv_handle`, exactly as the prose buffer is.
On a turn that fails to produce a snapshot the length is 0 and the prose
buffer still carries the sim's own error — the client degrades to the
transcript rather than to a blank pane.

## 6. The client

`clients/vessel` (TypeScript, Deno, bundled to committed JS per decision
0023):

- `protocol.ts` gains `SnapshotResponse { type: "snapshot"; json: string }`
  and the worker emits it alongside each `out`/`started`.
- `session.ts` (new): `parseSnapshot(json) -> Snapshot` plus the first
  projection, `narrationOf(snap)`. Pure module — no DOM, no worker globals,
  unit-tested, matching `protocol.ts`'s existing discipline.
- `main.ts`: the transcript pane renders `narrationOf(snapshot)` instead of
  the raw `out` text. **Visually identical output** — that is the point, and
  the test asserts it.

No new panes. This campaign proves the seam by moving the one existing pane
onto it.

## 7. Testing

- **Golden snapshot** (Rust): a committed `vessel/session/v1` JSON for the
  seed-42 opening plus a fixed command script, drift-checked in CI beside the
  existing generated artifacts.
- **Narration byte-identity** (Rust): the snapshot's `narration.prose`, over
  that script, is byte-identical to the corresponding lines of the committed
  `book/src/gallery/possession-seed-42.md`. This ties the new channel to the
  oldest golden and is the strongest available check — it is `drive.mjs`'s own
  trick, applied in Rust.
- **Determinism**: same seed + same script → byte-identical snapshot
  sequence; and a snapshot taken twice at the same turn is identical (the
  read is pure).
- **Unpossessed-world path**: `snapshot` on a session that never started is
  an error, not a panic.
- **Deno**: `parseSnapshot`/`narrationOf` unit tests over a committed
  fixture; no wasm in the unit tests.
- **wasm smoke**: `drive.mjs` gains an assertion that
  `hv_snapshot_len() > 0` after `hv_start` and that the parsed
  `narration.prose` equals the transcript opening it already checks.
- **Turn-cost guard**: record the CLI's per-turn cost in `scripts/timed.sh`'s
  ledger before and after, and assert in review that the no-snapshot path is
  unchanged. (The full ratchet is `CLIENT-turn-cost-ratchet`, not this
  campaign.)

**In scope because it is in the way:** `drive.mjs` currently asserts seed-43
possession succeeds, and seed 43 has no settlements, so `make vessel-check`
is red on `main` today. This campaign edits that file, and adding assertions
to a red gate is worse than fixing it: the driver will scout for a
possessable seed rather than hardcode one, so it tests the teardown path it
means to test instead of a geography accident.

## 8. Non-goals

- **No tile layer, no coordinates.** The metaplan's Campaign 3, and the
  relational-vs-Cartesian fork is unresolved. The schema is additive, so
  whichever way that lands, it lands as a new channel without an epoch.
- **No vitality, no action clock, no new verbs, no new panes.**
- **No removal of the prose ABI**, now or later. It is the constitutional
  primary and the book chapter depends on it.
- **No epoch, no new streams, no new concepts, no census regeneration.**
- **No `windows/scene` change.**

## 9. Flagged for G3

1. **`vessel/session/v1` is save-format-class.** It joins the schema families
   under the epoch discipline: additive changes are free, meaning changes
   mint `v2`, and nothing is ever renamed. This is the flag the metaplan
   promised would lead every campaign in this program.
2. **Channel grouping over per-field provenance** (§3). It makes the
   redaction discipline structural rather than conventional, and it is a
   shape that is awkward to change later — a pane written against
   `sensed.ways` cannot be cheaply repointed if the grouping is flattened.
   Worth a look before it sets. (**Amended:** `sensed.ways` does not exist —
   see correction 2. Read the example as `sensed.present`, where the same
   argument holds unchanged.)
3. **`social` as a top-level channel** rather than nested under `sensed`
   (§3). The argument is the position law; the cost is that a client wanting
   "everything about who is here" reads two channels.
4. **Folding the `drive.mjs` fix into this campaign** (§7) rather than
   leaving it as a followup.

## 10. Definition of done

Per decisions 0013 and 0020: `make gate` green; the golden and its
drift-check committed; `make vessel-check` green (which requires §7's fix);
a chronicle entry; a freshness sweep of the Casement's chapter; a
retrospective; and the registry rows `CLIENT-one-snapshot` and
`CLIENT-redaction-panes` flipped `raw` → `shipped` with **Where** repointed
at the chronicle.

**Amended on close:** only the one-snapshot row shipped outright. The redaction
row ships to a *floor* — the schema's channel grouping, which is the redaction
boundary made structural — and no further: there are no panes beyond the
transcript, and the knowledge-gated pane that row describes is untouched. It
therefore carries the registry's partial-ship idiom rather than a plain
`shipped`, because overclaiming there would misinform the next reader about
what exists.

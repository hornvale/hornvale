# The Vessel Stitch — Design

**Date:** 2026-07-19
**Status:** **COMPLETE — shipped (see [the chronicle](../../../book/src/chronicle/the-vessel-stitch.md)).**
**Campaign:** the first post-program campaign — the game-layer stitch the
self-writing-book program's C6 and C8 each filed as their named followup.
**Theory:** no new registry row — this executes LANG-39's recorded
"vessel esoteric unlock" followup and LANG-42's "vessel `--at` seam"
followup, over The Echo's transfer machinery (LANG-3 shipped floor).

---

## 1. What this is

The played game meets the self-written Book. A possessed session gains
one verb:

> `> consult`
> *The Reckoning, at day 12.*
> The sky keeps no dates to number.
> The Book holds more for the initiated.
>
> `> write Vebe is a planet with two moons`
> Written in the margin.
> `> consult`
> *The Reckoning, at day 12.*
> The sky keeps no dates to number.
> — two, as the initiated count.

`consult` renders, for the session's world, **The Reckoning at the
session's own day** (the C8 time axis, finally read from inside the
world) and **the initiated lines the session's Knowledge unlocks** (the
C6 esoteric layer, finally unlockable by play): what you have written
into your own margin — heard in play, or someday observed — is what the
Book confirms to you. Heard is not true (the Echo's standing point), but
written is *initiation*.

**The verb rename (G3 exchange, Nathan):** the Echo's `tell` becomes
`write` — the player writes what they have learned into their copy's
margin (the program's own margin device, turned toward the reader), and
the response says so: `Written in the margin.` The rename is total (no
`tell` alias — pre-1.0 game surface, no compatibility debt); every test
pinning `tell` re-pins in the rename task.

## 2. What exists (verified, main @7b02da0)

- `windows/vessel`: `Knowledge(BTreeMap<String, String>)` keyed
  `"{subject}::{predicate}"` → surface value; `absorb_common` (the Echo's
  transfer seam — its `tell` verb renames to `write` here); the session's `day: WorldTime` (the Quickening's tick);
  the verb dispatch match; vessel already depends on `hornvale-book`.
- `windows/book`: `esoteric_lines(world, reader: &BTreeSet<(String,
  String)>)` (C6 — the exact reader type the adapter produces);
  the `--at` single-epoch reckoning rendering (C8 — this survey line was
  STALE at writing: C8 T2 had already exposed `reckoning_at` pub and cut
  the CLI over; T1 measured this and shrank to the accessor law + docs).
- `make vessel-check` — the Casement's local gate; the wasm exhibit is
  deploy-built, never committed (nothing to release here).

## 3. Architecture

### 3.1 The book accessor (additive; `windows/book`)

`pub fn reckoning_at(world: &World, day: StdDays) -> ReckoningEpoch` —
the C8 `--at` internals exposed (verify the live factoring; if the CLI
path already routes through such a function, re-export it; otherwise
factor it out, behavior-identical, with the CLI cut over to it). No
committed-artifact change of any kind.

### 3.2 The adapter and the verb (`windows/vessel`)

- `fn reader_set(knowledge: &Knowledge) -> BTreeSet<(String, String)>` —
  key-split on `"::"` (keys without the separator are skipped — assert
  none exist today; the absorb path always writes the separator).
- `consult` (possession required, like every verb): renders the
  reckoning at `self.day` (heading `The Reckoning, at day ⟨integer⟩.`),
  then `esoteric_lines(world, &reader_set(...))`; when no initiated line
  unlocks, the closed fallback `The Book holds more for the initiated.`
  Output is a normal `Turn::Out` — a turn's worth of text, never the
  full volume.
- Unlock channels at the floor: `write` (the renamed `tell` — live
  today) and whatever walking already absorbs (verified at plan time; sky facts are not walked
  today). **The observational unlock — an astronomy verb that earns
  initiation by watching the night sky in-possession — is the recorded
  followup, not scope.**

### 3.3 The Casement

`make vessel-check` green is the whole obligation — the wasm exhibit is
deploy-built from the vessel crate and gains the verb automatically.

## 4. The laws (standing tests)

1. **The stitch law (end to end):** a fresh session's `consult` shows the
   fallback; after `write`-ing the moon sentence, `consult` shows the
   initiated line whose value equals the LEDGER's committed count
   (mutation-verified: a wrong WRITTEN count still unlocks the key but the
   rendered value is the ledger's — heard ≠ true, printed).
2. **The day law:** `consult` at day 0 and after `wait`-ing days renders
   the reckoning at the session's actual day (monotone with play).
3. **The purity law:** `consult` commits nothing — the ledger
   byte-identical before/after (the session mutates only its own
   Knowledge, and only via existing verbs).
4. **The accessor law:** `reckoning_at` equals the CLI `--at` path's
   output for the same day (one implementation, two callers).
5. **Additivity:** no committed artifact changes; census untouched;
   `vessel-check` green.

## 5. Determinism and blast radius

Zero draws, zero facts, zero concepts, zero census impact, zero
committed-artifact change. Session-state only. The quietest possible
campaign — by design, it is a stitch.

## 6. Non-goals

The observational-astronomy unlock verb (followup, recorded); consulting
the chorus/doctrine sections in-session (a future `consult ⟨section⟩`
argument); any wasm release machinery (nothing is committed); numeracy
filtering of the told value (LANG-44 stays banked).

## 7. Registry and decisions

No new rows; LANG-39's and LANG-42's Where notes gain the stitch's
chronicle link at close; the observational-unlock followup lands as a
registry note on LANG-42 (it is the diachronic axis meeting the
possession loop).

## 8. Flagged for G3

1. **Determinism:** nothing moves — no draws, facts, artifacts, census.
   The flag is the ABSENCE of flags; confirm the posture reads right.
2. **The heard≠true rendering (the one semantic choice):** a wrong
   WRITTEN count still initiates (the KEY unlocks) but the Book prints the
   LEDGER's value — the Book never repeats your error back to you; it
   confirms what the initiated may see. Alternative: refuse to unlock on
   a wrong value (initiation requires truth). I recommend the former
   (the Book as authority, not a mirror; and the Echo deliberately kept
   heard-knowledge outside the truth contract). Your call if you read
   initiation differently.
3. **Surface taste (settled at G3):** `consult` + `write` (Nathan's
   pick over `tell`/`scribe`/`annotate`), the margin response line, the
   heading, the fallback line.

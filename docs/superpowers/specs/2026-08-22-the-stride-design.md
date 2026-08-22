# The Stride — a walk input mode (design)

**Date:** 2026-08-22
**Status:** ratified in brainstorm with Nathan
**Surface:** `clients/game/bin` (+ the `Focus` enum in `clients/game/core`)

## Problem

Today the game binary has two input modes toggled by `Esc`: the CLI
(arrows = caret/history) and the map pane (arrows = screen-cursor moves,
best for looking at arbitrary things and zooming — not a primary mode).
There is no way to *play* from the keyboard: moving the character means
typing direction words into the command line every step.

## Decision

Add a third focus state, **Walk**, as the startup default and the player's
primary mode. Arrow keys and angle brackets become movement commands sent
straight to the session, roguelike-style.

### Mode model

- `hornvale_game_core::Focus` grows to three variants: `Walk`, `Cli`,
  `Map`. `Walk` is the default at session start.
- `Esc` cycles **Walk ↔ Cli only**. From `Map`, `Esc` returns to `Walk`
  ("back to playing").
- `Map` is entered by submitting the exact command `map`; the driver flips
  focus after handling, so the reply renders and the map is already
  focused.

### Key routing (`bin/src/input.rs`)

In Walk focus:

| Key | Action |
|---|---|
| ↑ ↓ ← → | `Action::Move("north"/"south"/"west"/"east")` |
| `<` / `>` | `Action::Move("up")` / `Action::Move("down")` |
| any other printable | `FocusAndType(c)` — bounce to CLI and type (same convention as Map) |
| Esc | `ToggleFocus` |

No diagonals, no wait key: the game is designed for laptops without
numpads; diagonal movement is not required anywhere.

Totality is preserved: every key has a defined destination in all three
focus states; chords (anything beyond bare SHIFT) stay inert; release
events stay inert. `<`/`>` are exceptions to the bounce rule just as
`-`/`+`/`=` already are under Map. The router stays pure — it knows
nothing of turns or sessions.

### Movement execution (`bin/src/driver.rs`)

`Action::Move(dir)` in `Driver::apply`:

1. builds the command string,
2. pushes it onto `history` and sets `echo` (so directions are recallable
   and the transcript shows ask-then-answer),
3. routes through the same path `Submit` uses (`self.handle(&taken)`),
   returning its release bool.

The buffer itself is untouched — no visible animation on movement keys.

### Mode entry on submit

After `Submit` takes the line, the driver trims it, splits off the first
token (mirroring `Session::handle`'s verb convention), and if it equals
`map`, flips focus to Map **after** handling. Exact whole-line match after
trim: `" map "` triggers; `"map x"` and `"examine map"` do not.

Deliberately NOT done:

- No structured verb exposure on `Turn` — the client mirrors the sim's
  stable first-token convention in one place (`driver.rs`, already the
  only session-aware module). Upgrade path exists if a second verb ever
  needs UI side effects.
- No `!map` variant — the sim has no `!` prefix idiom today; inventing
  one is sim-vocabulary work outside this feature.
- No new UI affordance for Walk mode — it draws exactly like today's
  CLI-focused view (cursor and strip still appear only under `Map`).

## Testing

Extends the existing property-style sweeps in `input.rs`'s test module:

- totality in all three foci;
- the six movement bindings + Esc under Walk; `<`/`>` are the only SHIFT
  characters that do not bounce;
- the bounce sweep runs over both Walk and Map;
- driver tests: `Move("north")` sends `"north"` through history/echo/
  handle, costs a turn, returns the release bool;
- `map` entry: exact-match-after-trim positives and negatives;
- Esc transition table: Walk→Cli→Walk, Map→Walk.

## Non-goals

Refining Map mode itself (zoom implementation, cursor usefulness) remains
deferred as before; diagonals/wait keys; any sim-side change.

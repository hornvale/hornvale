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

After `Submit` takes the line, the driver trims it and flips focus to Map
**after** handling if the whole trimmed line is exactly `map`. `" map "`
triggers; `"map x"`, `"map out 2"` and `"examine map"` do not.

**Correction, made during execution:** an earlier draft of this paragraph
stated two incompatible rules in consecutive sentences — "splits off the
first token … and if it equals `map`" (which admits `map out 2`) and
"exact whole-line match after trim" (which does not). The implementation
took the first reading and the test took the second, and the campaign found
it as a red suite rather than at review. The whole-line rule is the correct
one, for a reason neither sentence gave:

- `Session::map` takes `&self` and returns prose. **No argument form of
  `map` can move the plate**, so "it drew a chart, so focus it" — the
  intuition behind the first-token reading — is false.
- The plate is redrawn from `Spatial` every turn regardless
  (`spread::compose`). Submitting `map` in this client is therefore a
  **mode gesture**, not a fetch, and focusing after `map out 2` would put a
  cursor on an unzoomed plate the player did not ask about.
- Bare-vs-argument is a distinction the sim already draws, so mirroring it
  invents nothing: `Session::handle` guards its own `map` and `eyes` arms
  on `rest.is_empty()`.

Deliberately NOT done:

- No structured verb exposure on `Turn` — the client mirrors the sim's
  stable bare-`map` spelling in one place (`driver.rs`, already the
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

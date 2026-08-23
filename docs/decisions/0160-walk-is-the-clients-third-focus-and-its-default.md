# 0160. Walk is the client's third focus, and its default

**Status:** Accepted (2026-08-22) · **Decider:** Nathan

**Amends [0159](0159-focus-is-the-clients-one-input-mode.md)**, whose rule is
unchanged and whose state enumeration is not.

In the context of a terminal client that could be typed at and pointed at but
not *walked* in — taking one step north cost six keypresses and a submission,
for the most common act in the game — we decided that **`Focus` carries a
third state, `Walk`, and `Walk` is the startup default**: arrow keys and
`<`/`>` are movement commands sent straight to the session, and the client
starts able to play rather than able to type.

0159's rule survives intact and is not reopened: a key's meaning depends on
which pane has focus and on nothing else — no modifiers beyond `Shift`, no
chords, no timed sequences — the routing table is total, and text is the
default destination. A printable key under `Walk` bounces to the CLI and
types itself, which is 0159's own one-keypress convention extended to a third
state rather than a new idea.

## Context

The capability this restores was not missing; it was **spent**. At The Quire
the client had movement on arrows, numpad and vi-keys and no free-text entry
at all. 0159 closed that gap by making the routing table type a letter by
default — the only way a keyboard reaches ~25 verbs — which left no letters
over, so the arrows became caret and history motion. The Portolan's map
cursor lost its diagonals in the same squeeze. The bill came due as soon as
anyone tried to play.

Unbinding letters is not available: the keyboard really is full. A third
state is what buys the arrows back without spending the command line.

## Consequences

These three restate 0159's own consequence bullets where its two-state
enumeration no longer holds. Everything else in 0159 stands as written.

- **The routing table is total across THREE focus states**, not two. `Walk`'s
  column: six movement bindings, `Esc`, every other printable key bounces and
  types, everything else inert. Chords and release events stay inert in the
  third state exactly as in the other two.
- **`Esc` is no longer a route to the map, and the map's single point of
  failure is therefore gone.** 0159 named `Esc` as "the sole route" into the
  map focus and reserved a fallback binding (`` ` ``) against that risk.
  `Esc` now cycles Walk↔Cli and returns Map→Walk — it is always a departure
  from the map, never an arrival at it. The map is entered by submitting the
  command `map`, so reaching it depends on the command line, which is the
  client's most robust surface rather than its most fragile one. The reserved
  fallback binding is unspent and unneeded for this reason.
- **`Walk` claims neither hardware-cursor destination.** 0159's forced
  consequence — that focus is shown by the cursor's *location*, the caret
  under CLI and the map cursor under Map — needed only two answers. `Walk` is
  a third: the buffer is not being edited (printable keys bounce away) and
  the map cursor is not driving, so `render_with` reports no cursor at all.
  This does not weaken 0159's "never both" rule, which is about ambiguity
  *between* the two pane cursors; `Walk` claims neither, which is
  unambiguous. It does mean the cursor's location no longer distinguishes
  `Walk` from a Map focus that has no map cursor — both report none — and the
  client draws identically in `Walk` and `Cli`, so nothing on screen says
  which one you are in. Accepted deliberately: a mode indicator would be
  ornament occupying an informative cell, which the spread's row-reservation
  rule forbids, and the arrows announce themselves by working.

## Bare `map`, and why not first-token

Only a whole trimmed line of exactly `map` enters the map focus. `map x`,
`map out` and `map out 2` do not, though all three have `map` as a first
token. The reason is a fact about the sim, not a preference: `Session::map`
takes `&self` and returns prose, and the client's plate is redrawn from the
snapshot's `Spatial` channel every turn regardless of what was typed — so no
argument form of `map` can move the plate at all. Submitting `map` in this
client is a **mode gesture, not a fetch**, and focusing after `map out 2`
would hand the player a cursor on an unzoomed plate they never asked about.
The sim already draws this exact bare-from-argument line, guarding its own
`map` and `eyes` arms on `rest.is_empty()`, so the client mirrors an existing
convention rather than minting one.

This is recorded because the campaign's own spec stated both readings in
consecutive sentences and the discriminating case appeared in neither its
examples nor its first test table — a future reader is more likely to
"simplify" the rule back to a first-token check than to rediscover why it is
not one.

## See also

`docs/superpowers/specs/2026-08-22-the-stride-design.md` (§ Mode model, and
the § Mode entry on submit correction); `book/src/chronicle/the-stride.md`;
`book/src/chronicle/the-portolan.md` and `book/src/chronicle/the-stylus.md`
for the squeeze that spent the arrows in the first place.

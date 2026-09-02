# 0510. Compass input is 4-way primary and 8-way capable; no destination requires a diagonal

**Status:** Accepted (2026-08-31) · **Decider:** Nathan (autopilot, spec §3.5) ·
**Relates:** [0507](0507-every-lattice-in-the-project-is-eight-connected.md),
[0159](0159-focus-is-the-clients-one-input-mode.md) (the routing
this one extends)

In the context of eight headings becoming real at every band
([0507](0507-every-lattice-in-the-project-is-eight-connected.md)), facing The
Stride's ratified arrow-key routing and a laptop keyboard with no numpad, we
decided that **arrows stay 4-way and primary, diagonals are bound as a
convenience (the vi-keys `y`/`u`/`b`/`n`, plus the `ne`/`nw`/`se`/`sw` words the
CLI already parses), and no destination ever requires a diagonal to reach**,
accepting an awkward secondary binding.

## Context

Making the lattice 8-connected does not settle what a keyboard should do with
it. The Stride's routing is an INPUT decision, not a geometry one, and nothing
in this campaign gives a reason to reopen it: arrows are what a new player
reaches for, and there are only four of them.

The binding for the other four is genuinely awkward on the hardware this
project is played on. That is affordable only because of the clause below it.

## What was decided

- **Arrows stay 4-way and primary.**
- **Every destination remains reachable by cardinals alone.** This is what
  makes an awkward diagonal binding acceptable: a diagonal is never a
  competence requirement, only a saving of one step. It is a property of the
  lattice — four orthogonal moves span it — not a rule anyone polices.
- **Diagonals are to be bound** to the vi-keys `y`/`u`/`b`/`n` in the terminal
  client, alongside the `ne`/`nw`/`se`/`sw` words the session has always
  parsed (`windows/vessel/src/session.rs`, the compass token table) and which
  now RESOLVE rather than being refused. The word half is in force; the key
  half is a decision this record ratifies and the campaign's input task
  carries out — see the note below, which is a scope statement rather than a
  retraction of this bullet.
- **Travel and pathing commands use all eight**, since a route has no keyboard.

## Consequences

- **The parity contract is satisfied without a new binding requirement.** The
  Blocking's rule — every destination the render depicts must be reachable by a
  named command — holds on cardinals alone, so a player who never learns the
  vi-keys can still reach everywhere the chart draws.
- **A diagonal saves a step and costs `√2`** ([0508](0508-a-diagonal-costs-root-two.md)),
  so taking one is very slightly slower per unit of ground than the two
  orthogonal steps it replaces are per unit of ground — it is a convenience in
  keystrokes, not a shortcut in time.
- **What we give up:** an input scheme a numpad user would call natural.
  Rebinding remains available to a future campaign; this record settles the
  default, not the ceiling.

**What this record does NOT assert.** The word half is shipped — the compass
table has always parsed `ne`/`nw`/`se`/`sw`, and 0507 is what makes them
resolve. The KEY half lands with the campaign's input task; this record is the
choice, and `clients/game/bin/src/input.rs` is where to check whether it has
been carried out yet rather than a place this record claims it already was.

## See also

`docs/superpowers/specs/2026-08-30-the-pavement-design.md` §3.5;
`windows/vessel/src/session.rs` (the compass token table, `go`);
`clients/game/bin/src/input.rs` (the key routing).

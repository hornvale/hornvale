# 0389. A glyph carries order or identity, never an arbitrary category

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot, spec §2) ·
**Relates:** [0287](0287-a-zoom-rung-is-a-mesh-depth.md) (the same
register-before-render discipline applied to the zoom ladder rather than to
the glyph alphabet)

In the context of a terminal client that has been assigning display
characters one pane at a time with no central table, we decided that **a
glyph carries order (ink ascends with the quantity) or identity (the
character is the referent's own initial), and never an arbitrary category**;
colour carries category and substance; weight carries attention and
epistemic state; position carries position. A character means one thing
across the whole client.

## Context

Reading every glyph constant in the client and the sim's own renderer found
three characters already carrying two or three meanings each: `.` is land,
floor, and relief band 2; `+` is threshold, "everything else", and a water
glyph; `#` is wall and settlement. The one character every pane agrees on is
`@`, the only one ever assigned by a rule rather than by hand. The table is
spec §1 (`docs/superpowers/specs/2026-08-28-the-legend-design.md`).

The defect is not a shortage of characters — it is that no rule has ever
governed which pane may claim which one, so two panes converge on the same
mark for unrelated reasons and neither notices until a reader does.

## Decision

- **Glyph** carries *order* (ink ascends with the quantity — a density
  ladder like `_ . : ^ A`) or *identity* (the character is the referent's
  own initial, like `@` for the observer). **Never an arbitrary category** —
  a glyph is not a free-standing label for "swamp" or "goblin" with no
  ordering or mnemonic relationship to the mark itself.
- **Colour** carries category and substance. Colour may fail (`NO_COLOR`, a
  16-colour terminal, colour-blindness), so nothing a reader must trust may
  live only here.
- **Weight** carries attention and epistemic state. Already shipped,
  unchanged.
- **Position** carries position.
- **Allocation rule: a character means one thing across the whole client.**
  Two panes may use different *ladders* for different quantities — walk
  scale reads impedance, globe scale reads elevation — but they may never
  use the same character for two meanings.

### Why the 22-glyph rejection is narrower than it reads

`CLIENT-glyphs-22-rejected` is routinely read as "22 was too many." The
count was not the defect: it was **nominality**. A set of 22 nominal marks
needs a legend permanently, because nothing about `%` implies "swamp." The
five-glyph ladder `_ . : ^ A` needs no legend at all, because ink density
**is** the ordering and the reader decodes it unaided.

So the rule generalizes past that rejection: **ordinal marks self-legend;
nominal marks never do.** This decision supersedes nothing —
`CLIENT-glyphs-22-rejected` stays rejected — it states the principle that
rejection was one instance of.

## Consequences

- `CLIENT-four-channels` is promoted from `raw` to `ratified (0389)` in the
  idea registry.
- The register (`clients/game/core/src/register.rs`) reserves an unallocated
  `Subterranean` region rather than allocating into it now: `MAP-underworld-
  chart`'s campaign 2 (the unnamed second Delving campaign) is the claimant,
  and it claims from this register instead of minting a second vocabulary.
  A board `notice` naming the reservation goes out at that campaign's start.
- A future glyph allocation is checked against this rule before it is
  checked against anything else: is the character claiming order, or
  identity, and is it already bound to a different population in
  `REGISTER`?

## See also

Spec §§1-2, §9 (`docs/superpowers/specs/2026-08-28-the-legend-design.md`);
`clients/game/core/src/register.rs` (the `Population` enum, `Binding`
struct, `REGISTER` table, and `no_character_is_bound_twice`).

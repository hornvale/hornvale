# 0408. Visibility is an explicit per-cell state, never a shade

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot) ·
**Relates:** [0407](0407-vessel-level-v1-is-a-new-schema.md) (the schema this
constrains) · [The Gallery](../../book/src/chronicle/the-gallery.md)

In the context of a monochrome client needing to draw three distinguishable
epistemic states (lit, remembered, never-seen), we decided that **`here`/
`lit`/`remembered` are named states carried by the palette's `state` field,
never a colour or a `Weight::Dim` tint**, accepting a glyph-twin vocabulary
(a remembered wall draws `:`, not a dimmed `#`) instead of a shading rule.

## Context

A dimmed tint is invisible the moment colour is off, and colour is a client
choice this project does not control (`--script` already runs `Lens::Off`
for byte-stable transcripts). A state that survives only in colour is a
state that does not survive a monochrome client, which the systems audit's
own Field of View item already credits Hornvale for avoiding in the walk
band's `faded()` convention (`.` → `,`). `vessel/level/v1`'s palette
generalizes that convention to a second band rather than inventing a
different mechanism for it.

## The rule

Five kinds, each with a distinct "seen" glyph and a distinct "remembered"
twin (floor `.`/`,`, wall `#`/`:`, flooded `~`/`-`, stairs down `>`/`)`,
stairs up `<`/`(`); a never-seen cell is omitted from the document entirely
rather than drawn as anything. `you` is drawn a second time, Bold, identical
to `plan::draw`'s own two-pass convention.

## Consequences

- **No per-kind colour channel exists in this band**, and none is planned:
  `level.rs` never reaches for `Weight::Dim` the way the walk band's coarse
  `@`/`+` vocabulary must, because the glyph alone already carries the whole
  distinction here.
- **A field-of-view change (a wider or narrower reach) never needs a second
  representation.** The state a cell was drawn at is fixed the moment it is
  marked (decision 0409); this decision is what makes that representation
  legible without falling back to shading.

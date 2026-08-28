# 0351. Contents are revealed when a container is open or transparent

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (asked directly, at the
§3.6 stop) · **Relates:**
[0348](0348-object-properties-are-keyed-by-kind.md) (why the open/closed state
this rule needs is IV.b's),
[0346](0346-an-affordance-is-derived-never-committed.md),
[0069](0069-fine-position-is-never-serialized.md) ·
`MAP-if-world-conformance` · [The Offer](../../book/src/chronicle/the-offer.md)

In the context of deciding which anchors reveal what lies within them, we
decided to **take the interactive-fiction rule — contents show when a
container is OPEN or TRANSPARENT** — accepting that IV.a has no open/closed
state and therefore reveals unconditionally from every carrier.

## Context

The spec's first rule drew the line at *semantic* containment (a strongbox
contains) versus merely *spatial* containment (an alcove is a recess in a
wall), and gave `Encloses` to `Strongbox` alone. A census over all sixty
production gate combinations then measured the consequence: the grammar's only
`within` relation anywhere is `{(Hearth, Alcove): 3}`, and **nothing is ever
placed inside a strongbox** — `the-strongbox` is `Attach::Beside(Vessel)`, a
sibling, not a container. The semantic/spatial line put the property on the one
anchor that never holds anything, and the feature would have reported nothing,
forever.

Three alternatives were declined at the stop: author a `Within(Strongbox)`
pattern (changes world shape, moves fixtures and transcripts); ship the section
dormant as §3.5's gate is (two dormant sections of five is a pattern, not an
exception); drop containment entirely (reverses the G3 answer).

## The rule

Both `Alcove` and `Strongbox` carry `Encloses`. A nook is open and transparent,
so it shows its hearth; a chest is neither, so it does not. Inform and TADS
both work this way.

**IV.a ships the reveal without the state machine, and the silence has a
different cause than the rule gives it.** There is no open/closed/transparent
state to gate on, so every carrier reveals unconditionally; the strongbox is
silent because nothing is ever placed within it, not because it is closed.
`ObjectProperty::Encloses`'s own doc says exactly that, so the gap is stated
where a reader meets it.

## Consequences

- **This lands the arc cut in the same place for a better reason than the
  spec's original argument.** Open/closed is *durable object state* — the 90%
  rung IV.a defers by construction — so IV.b inherits a firing case rather
  than a redesign.
- The conformance question it raises, whether the object model obeys IF world
  rules generally, is banked as `MAP-if-world-conformance` in the idea
  registry: a proposed fourth sibling to `tropes/`, `systems/` and
  `sentences/`, resolved against the object model rather than the concept
  registry.
- `Interior` is derived per room and never serialized (decision 0069), so
  anything contained evaporates with the bubble. Correct for IV.a, which has
  no durable state at all, and IV.b's problem by design rather than accident.

# 0337. Possession, command, and charm are three mechanisms

**Status:** Accepted (2026-08-26) · **Decider:** Nathan (autopilot, spec §8) ·
**Relates:** [0168](0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md)
(the effect stays with the body under all three), [0336](0336-possession-by-another-is-possession.md)
· **Settles:** the taxonomy question `docs/superpowers/specs/2026-08-19-the-
bridle-metaplan.md` left open by naming the arc "The Coercion" over §4's own
argument that *coercion* names the command row

In the context of a design that keeps reaching for one abstraction to cover
every way one mind can move another, we decided that **possession replaces a
decision procedure, command constrains its outputs, and charm modifies its
inputs — three mechanisms, resting on three different subsystems, and no
general `Control` trait unifies them.**

## Context

The frontier essay's own enumeration of the social axis (command, persuasion,
deception, coercion, hire, inheritance, institution) makes the same mistake a
`Control` trait would: it treats "one will bends another" as one thing to
model, when the three candidates already in scope for the Bridle program do
different work on different data:

```
  mechanism           does what to the             subsystem
                      decision procedure
  -----------------   -------------------------   ----------------------
  possession          REPLACES it                 controller stack
  command              CONSTRAINS its outputs       gate table
  charm, suggestion    MODIFIES its inputs          drive valuations
```

**Charm is not a gate concept**, and this is the sharpest of the three
distinctions because it is the one a shared abstraction would get wrong in
both directions. A charmed creature is refused nothing — it genuinely wants
to help, because its valuations moved — so putting charm in the gate table
(`windows/vessel/src/gate.rs`) would refuse acts that should succeed. Putting
it in the controller stack would be worse: a charmed creature must keep
running its own arbitration, which is exactly the loop possession's
controller replaces. Charm needs the drive layer, untouched by either of the
other two.

**Control undead validates the taxonomy rather than straining it.** Socially
it resembles charm — no resistance, immediate cooperation — but mechanically
it is possession: an undead has no drives to reweight, so there is no
procedure left to modify, only one left to supply. The mechanism a case needs
is not always the mechanism its narrative dressing suggests.

A `Control` trait spanning all three was considered and rejected on the same
argument NIH would reject any premature abstraction: the three mechanisms
share a one-sentence description ("one mind moves another") and nothing else
— not a signature, not a subsystem, not a lifecycle — so unifying them would
force charm's input-modification into the controller stack's replace-the-
procedure shape, which is the wrong shape for it.

## Consequences

- **The arc's own name is now a recorded tension, not a silent one.** Under
  this taxonomy, *coercion* names the **command** row — making someone choose
  to act — while this arc, which ships an imposed controller, removes the
  choosing entirely. Nathan reviewed this at the spec's hard stop and kept the
  name "The Coercion" as written (spec §9, flag 2); this record is the
  argument a later rename would cite, not an instruction to make one.
- No `Control` trait exists anywhere in `windows/vessel/`, and a future
  campaign proposing one should read this record's "three subsystems, one
  sentence in common" argument before building it.
- Command (the gate table) and charm (drive valuations) are both unbuilt as of
  this decision; only possession (the controller stack, decisions 0228/0336)
  ships. The row documents where the other two belong when they do arrive, not
  that they have.

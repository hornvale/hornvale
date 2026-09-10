# 0958. Visual libraries are independent of the film

**Status:** Accepted (2026-09-10) · **Decider:** Nathan

In the context of a first graphical client intended for reuse, we decided on an
independent three-crate workspace: native source, Bevy presentation library and
Planetarium composition root, accepting independent client checks and a serialized
mirror that validates its consumed fields.

**Context.** Film direction, captions, interval and controls belong to the app.
Shared identity, document/time plumbing, astronomy presentation and capture
mechanisms belong to libraries that do not depend on it.

**Consequence.** Resolved all-feature metadata forbids source→Bevy/view/app and
view→simulation/source/app, including renamed, transitive, build and test edges.
Application→libraries is allowed. Independent CPU consumers prove reuse without
a second product. Rust 1.96.1, Bevy 0.19.1 and the client lockfile remain outside
the root allowlist. `visual-check-run` joins the existing canonical clients phase;
GPU qualification remains separately required.

A future situated game supplies its own limited producer/mirror, following
[0115](0115-a-clients-mirror-may-omit-a-channel.md); it cannot fetch scientific
truth and rely on UI hiding. Gameplay consequences and permitted time control
remain that future composition's responsibility. No second renderer or graphical
game is claimed.

**See also.** [Visual workspace guide](../../clients/visual/README.md),
[0117](0117-the-client-re-derives-nothing-the-sim-emits.md).

**Authority.** [The approved Planetarium design](../superpowers/specs/2026-09-10-the-planetarium-design.md).

# 0437. A view shows what exists, and one that cannot speak is skipped

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0436](0436-the-startup-is-a-frame-with-pluggable-views-not-a-screen.md),
[0016](0016-studies-preregister-hypotheses.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of four views watching a world arrive across the four rungs of
the `BuildDepth` ladder, facing the question of what a view draws before its
own data exists, we decided a view **shows what exists and never a placeholder
for what does not** — a view with nothing honest to say answers `can_speak`
with `false` and the frame **skips** it, accepting that the cycle's membership
changes as the build deepens.

**Context.** This settles preregistered hypothesis **H1** — *every view renders
at every rung* — and it settles it as the **null the preregistration named**.
Two of four views cannot speak at every rung: `atlas` is silent until
`BuildDepth::Terrain` (before it, `artifacts.terrain` is `None` and there is no
honest raster), and `tongue` is silent until `BuildDepth::Full` (no tongue
exists to realize a clause in). `sky` speaks from the shallowest rung because
everything a night sky needs is committed at 0.4 ms; `almanac` speaks
throughout because its *components* each declare their own rung.

The failure this forbids is specific. An empty region reads as a hang, and a
padded box of spaces reads as a broken map — precisely when the build is
healthiest. Declaring the rung is the honest statement; drawing nothing under a
view's name is not.

**Consequence.** `can_speak` must be a pure function of the rung, because the
frame calls it to decide cycle membership between rung boundaries, when no
world is in hand. A view that wants finer grain than "this whole screen is
silent" pushes the same rule one level down into components: a component
declares the rung it `needs`, and the registry never asks it to render before
that rung has landed — which is what keeps an ABSENT fact from being read as a
FALSE one.

**See also.** Spec §3, §10 (H1); `clients/game/bin/src/overture/view.rs`
(contract rule 2); `clients/game/bin/src/overture/component.rs`.

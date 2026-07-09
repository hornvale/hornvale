# 0022. Sim emits data, clients render

**Status:** Accepted (2026-07-09) · **Decider:** Nathan

In the context of charting rendering beyond committed artifacts — scene
descriptions, interactive viewers, an eventual tilemap game view — facing
the no-new-dependencies rule (decision 0004) and the constitutional deferral
of graphical clients (long-term plan §7), we decided that **the simulation
emits deterministic data and clients render pixels: every in-workspace
render is deterministic bytes (text, ASCII, PNG, PCM, scene JSON),
reproducible from seed and query alone, and any interactive or graphical
client lives outside the workspace, consuming the CLI's output across a
process boundary — driving `hornvale` as a subprocess or reading its
committed artifacts, never linking the crates**, accepting that rich clients
carry their own dependency stacks and live outside the workspace's test
gate.

**Context.** "Sim first, game as lens" applied to output. The seam extends
the constitutional layering one ring further — kernel → domains → windows →
cli → *clients* — and is concretized by scene JSON: semantic content only
(what an observer can see, never how to draw it), with schemas treated as
save-format-class contracts (versioned by epoch suffix, never renamed). The
rendering-strategy spec places every render on this axis; §7's deferral of
graphical clients stands — this decision settles the *shape* a client takes
whenever one arrives, without scheduling it.

**Consequence.** Decision 0004 is permanent rather than provisional: no
future client campaign amends it, because clients never link the workspace.
Scene emission is presentation and therefore a window (`windows/scene`,
surfaced as `hornvale scene`); committed example scenes join the gallery
drift check. Ring-3 clients (in-book web viewer, TUI viewer, tilemap game
view) are registry vision rows (RENDER-2..4), buildable at any time without
touching the workspace.

**See also.** The rendering-strategy spec
(`docs/superpowers/specs/2026-07-09-rendering-strategy-design.md`); decision
0004 (no new dependencies); decision 0018 (hand-rolled PNG); Constitution §7
(graphical clients deferred).

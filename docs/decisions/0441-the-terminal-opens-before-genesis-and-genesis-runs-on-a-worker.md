# 0441. The terminal opens before genesis, and genesis runs on a worker

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0001](0001-determinism-is-constitutional.md),
[0055](0055-external-clients-consume-a-versioned-wasm-catalog.md),
[0436](0436-the-startup-is-a-frame-with-pluggable-views-not-a-screen.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of a client that opened its terminal only after the build
returned, facing a wait that has to be *shown* rather than merely shortened, we
decided the terminal opens **before** genesis and genesis runs on a **worker
thread** while the main thread owns the terminal, polls input, cycles views and
redraws on a cadence — accepting that one phase of startup still cannot move
off the main thread.

**Context.** Redrawing only inside the rung observer gives four redraws across
the whole build, and the gaps are not small: the settlements rung alone is
1,840 ms today and ~36 s at the projected minute. Four frames in three seconds
is a progress indicator, not an overture — and both the `space` cycle and the
slideshow are meaningless without a loop to drive them.

**Determinism is untouched, and the strong form of that claim is the true one.**
The whole build still runs on one thread, in one order, with one stream of
draws; `build_world_observed` is called exactly once, from exactly one thread,
exactly as the CLI calls it. `clients/game` is outside the determinism boundary
anyway ([0055](0055-external-clients-consume-a-versioned-wasm-catalog.md)), but
nothing here relies on that.

**Consequence — the bound we accept, stated as a bound rather than an
oversight.** `WorldContext::build` — 27.2% of the wait — stays on the main
thread and cannot move: it stores a `Box<dyn PhenomenaSource>` and that trait
declares no `Send` bound, so the value cannot cross a thread boundary at all.
Adding one is a **kernel** change, and this campaign's only sim-side change is
the observer callback. `Driver` is additionally `!Send` because it holds a raw
world pointer, so moving `Driver::start` across would need an `unsafe impl
Send` — a safety claim that wants its own review.

So the screen holds its last frame for ~843 ms of 3,054 ms, and the frame it
holds is *correct* rather than blank: the `living` phase is marked in progress,
with a bar if a previous run measured it. Against the previous behaviour — a
blank terminal for the entire 3,054 ms — the campaign's premise is delivered
for **73% of the wait**.

Opening the terminal first also creates an obligation the campaign discharges:
a genesis that fails must leave the terminal sane, because there is now
something to restore.

**See also.** Spec §5, §7; `clients/game/bin/src/overture/genesis.rs`;
`kernel/src/phenomena.rs` (`PhenomenaSource`, no `Send` bound).

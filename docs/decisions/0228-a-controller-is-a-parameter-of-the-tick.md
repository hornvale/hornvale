# 0228. A controller is a parameter of the tick, not a property of the body

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Relates:**
[0167](0167-a-driver-is-interchangeable-and-a-possessed-body-is-a-creature.md)
(the requirement) · [0168](0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md)
(the same separation at the level of effects) ·
[0226](0226-a-possessed-host-is-co-present-not-displaced.md)

In the context of making keyboard input and a planner interchangeable drivers
of one creature, we decided that **the source of intent is a `Controller`
parameter threaded through the tick — GOAP is `DefaultController` and the
keyboard is `PlayerController` — and `advance_one` learns nothing about which
one asked.**

## Context

Decision 0167 stated the requirement as the visitor pattern: the creature is
the subject in every case, and where the intent came from is a parameter of
the visit. 0229 makes one subject exist; this record settles what a driver
*is* once it does.

**A first cut of the trait was too narrow to carry a real decision, and that
is the finding worth keeping.** `intend(body, view, mode)` could reach only
the Stage-0 thirst-only computation, while the real per-species arbitration
(`decide_step`) takes twenty threaded parameters — terrain, hazard, alarm,
visited set, frozen set, mesh memo, home-nav cache and the rest. A trait that
narrow cannot wrap what actually drives every creature; it can only run a
second, simpler computation *beside* the real walk and discard the result.
That is a controller stack in name.

The remedy was to widen the trait rather than to build the parallel pass.
`intend` now takes the `Resolution` the walk's own `arbitrate` call already
produced — the co-present read of 0226, computed before any controller is
consulted.

## Consequences

- **`DefaultController::intend` is `resolution.intent.clone()`** — a
  pass-through that is byte-identical to having no controller at all, by
  construction rather than by agreement. Every existing creature's committed
  trail is unchanged, verified across four seeds by an instrumented A/B of the
  decision stream: 224 decisions, 8 differences, every one of them the
  possessed body's own.
- **`PlayerController::intend` is `self.pending.take()`**, and nothing pending
  is `Intent::Hold`, never a GOAP fallback. A driven body with nothing queued
  waits on the player; it does not quietly act for itself.
- **A controller is consulted exactly once per decision point, and the shared
  object is the decision function, not the loop.** Every body — driven or
  not — reaches `advance_one`, and `advance_one` arbitrates first and asks the
  controller second, so no body is decided by a different rule. The *loop* is
  two calls: `Session::wait` runs `step_with_occupancy` over the other bodies
  and then `step_one_with_controller` over the driven one, in a solo
  band-of-one walk. That separation is deliberate and `step_one_with_controller`
  says so in its own doc — folding the driven body into the shared population
  would let its history reach `alarm_field_memo` and `WalkState::begin`'s band,
  changing another creature's committed trail depending on who is being
  ridden. An earlier draft of this bullet claimed the driven body "enters the
  same per-body loop as any other" and that the loop is therefore "not
  creatures, plus a special case". The code is two calls and labels the second
  one an exception, so that claim is withdrawn; 0226 already states the
  defensible version — the same `advance_one`, reached by a solo walk.
- **The controller answers only *what happens*, never *what the body feels*.**
  `resolution.mode` and `resolution.affect` are settled before `intend` is
  called and are not the controller's to change. That split is what lets 0226's
  co-presence survive an imposed controller in Arc III.
- **Ordering is load-bearing where a controller is stateful.** A
  `PlayerController` whose `intend` *consumes* a queued action must not be
  handed to a pre-pass: routing the catch-up walk through the live controller
  would let it take a queued action, break out of its own loop, discard it, and
  leave the real tick with nothing — the player's action silently vanishing.
  The catch-up walk gets a fresh controller for exactly this reason, which
  closes the hazard **by construction** rather than by the absence of a
  caller — a fresh controller has nothing to consume, whatever the live one is
  holding. `catch_up_does_not_consume_the_controllers_pending_action`
  (`liveness.rs`) exercises exactly the hazard: it queues a real `Drink` before
  the call and stages a three-day gap so catch-up's loop actually runs, and it
  asserts the action survives to be committed. What remains dormant is
  `queue()`'s use *in production* — the verb loop does not queue yet — so the
  hazard is recorded here for the shape of it, not because the fix is
  outstanding. An earlier draft of this bullet said the hazard was "dormant
  today only because `queue()` has no callers yet", which reads as though
  nothing had been done about it.
- **The mind-flayer shape falls out of this and must not be foreclosed.** If
  the intent source is a property of the *relationship* rather than of the
  body, "body A is driven by mind B" has the same structure as "body A is
  driven by a keyboard". Arc III ships the imposed controller against this
  trait, unchanged.

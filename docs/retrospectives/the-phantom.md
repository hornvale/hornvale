# The Phantom — retrospective

One page, process not product. Built transient-danger memory (PSY-11's capstone of
The Haunt + The Alarm): a creature remembers where a herd's alarm frightened it and
shuns the now-safe ground — the phobia, re-derived from the frozen ledger (the alarm
is never committed), read by The Haunt's existing planner.

## What worked

- **Two ideonomy passes *converged* on a perf answer none of the three menu options
  named — and confirmed the design was sound, not flawed.** When the health battery
  went from ~311s to >710s and I brought Nathan a three-option menu (memoize / commit
  a trace / defer the test), he pushed back: "run it through ideonomy — the
  fundamental approach might be subtly flawed." It was the right call. Abstracting the
  problem and re-instantiating it in four unfamiliar domains (event-sourcing
  materialized views, neural consolidation, epidemiological sentinels, traffic
  ghost-jams) surfaced a shared verdict — *record at write-time, never re-derive at
  read-time* — that my read-cache framing had missed. Then the second pass stress-
  tested that answer and *right-sized it down*: `believed_water` already pays the same
  O(ticks²) fold, so a full incremental materialized view was over-engineering; the
  fix was to remove only the pathological ×`affect_of` factor, reaching parity with the
  existing belief pattern. The lesson: when a decision reduces to picking from a menu I
  wrote, the menu is probably the thing to distrust — ideonomy is for finding the
  option that isn't on it.

- **The stability argument de-risked a cache that had looked too dangerous to attempt.**
  The implementer stopped at the attempt limit specifically because a cross-crate cache
  with byte-identity stakes, verifiable only through a ~12-min sim, felt too risky under
  time pressure — a correct instinct. What unblocked it was not more courage but a
  *proof*: a past day's primary-afraid verdict reads only positions and drives at-or-
  before that day, which never change as the ledger grows forward, so the memo is
  **append-only and never invalidated** — a pure memo of a stable pure function, byte-
  identical by construction. The generalizable move: before threading a cache through a
  determinism substrate, prove its key captures a *monotonically stable* quantity; if it
  does, the scariest failure mode (stale reads) is impossible, and the change stops
  being scary.

## What the campaign taught

- **A "byte-identical" feature can still be operationally unshippable, and the possession
  walk won't catch it.** T2 verified byte-identity and no hot-path regression on the
  seed-42 possession walk — genuinely clean — and I nearly took that as the performance
  all-clear. But the possession walk has a short history and no *primary-afraid* fauna;
  the health sim accumulates a long history and contains wary beasts, and only there did
  the O(history × roster × affect\_of) cost detonate. Byte-identity and per-tick timing on
  one world are not a performance proof; a feature whose cost scales with sim length must
  be timed on the *longest* sim in the suite (the health battery), not the demo. The plan's
  "re-time the possession walk" was the wrong probe.

- **The 3-attempt rule did its job — as a *stop*, not a *solve*.** The implementer tried
  five performance approaches and correctly refused a sixth under time pressure, handing
  back a precise diagnosis instead of a risky guess. The rule is not "give up"; it's
  "escalate with evidence." The escalation is what let the controller + Nathan + ideonomy
  find the real fix, and the eventual sixth approach succeeded *because* it was directed
  and de-risked rather than another shot in the dark.

## A follow-up worth promoting

The visceral **felt** phobia — a Danger-drive term so a creature feels dread standing on
now-safe remembered ground, reading only the transient subset of `believed_hazard` to stay
byte-identical — is the agreed next campaign, now with a proven-cheap memory to build on.
And the perf endgame is captured: a **joint incremental materialized view** for
`believed_water` + `believed_hazard` together, if session length ever becomes the
bottleneck the memo-to-parity fix deliberately did not solve.

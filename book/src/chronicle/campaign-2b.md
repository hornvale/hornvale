# Campaign 2b: The Sky's Debut

**July 2026 · 17 commits · outcome: complete, merged — Campaign 2's exit criterion proven**

## What was attempted

Wire the 2a generator into worlds. Typed quantities first (the design
conversation's newtype decision, retrofitted through the whole generator);
then graded moon pins with recorded refusals; the calendar layer; the
generated-sky provider; pins persisted as facts in each world's own ledger
with deterministic reconstruction at load; the pin flags and `scout` on the
command line; calendar sections in the REPL and almanac; and the exit demo.

## What landed

**The sky is real.** A new world's sun rises and sets on a day length drawn
from its seed; its year is however many of its own days fit around its own
star; its moons show phases; its night reveals the neighbor stars; its
almanac has a Calendar. And the exit criterion holds, end to end and now
committed as a [gallery pair](../gallery/the-sky.md): the same seed's
goblins worship **the Returning One** ("departs and returns every 0.88
days") under a spinning sun, and **the Unblinking Eye** ("has never
departed and will never blink") under a tidally locked one — one pinned
cause, legible downstream difference, with religion's code untouched since
Campaign 1b. Moons, by contrast, flip the calendar and the night sky while
leaving the faith constant, exactly as tier-0 religion (which heeds only
the most salient phenomenon) predicts.

**A world now carries its own experimental record.** Pins are committed as
scenario facts; the sky is rebuilt from them at load, and — the design's
quiet masterstroke, caught by review as a strength rather than added by
one — the world's religion is generated from the *reconstructed* provider
at birth, so any lossiness in the pin round-trip would contradict the
world's own facts immediately. Refusals are facts too: seed 23's almanac
says a third moon "was sought" and why there is none.

## What was learned

- **The typed quantities paid for themselves twice before the campaign
  ended.** The retrofit itself was proven behavior-free by a golden test
  (exact bit equality on seed 42, captured before a single type changed) —
  and then the final review's fuzzing found the two places raw numbers
  still leaked: a `u32` overflow in the graded-pin arithmetic and a
  hand-corrupted save that panicked instead of erroring. Both now fail
  loudly with reasons. Where the types ruled, no such bugs existed to find.
- **A failing exit test accused the wrong party.** The graded-pin test
  initially failed on seed 7 — because it had been written with the exact
  pin (`--moons 3`) instead of the graded one (`--moons 0+3`). Seed 7's
  loud refusal was the *contract working*; the test was asking the wrong
  question. The suite was fixed, not the physics — and the episode is a
  clean specimen of why assertions are reviewed as hard as code.
- **Local days are not standard days, and the book can prove it:** seed
  42's year is 368.1 standard days but 418.3 of its own days — its day is
  21.1 hours, so more of them fit. The two-clocks design renders this
  without ambiguity, and the tidally locked variant simply has no local
  column at all.

## Deferred, deliberately (Campaign 3's opening notes)

A `sky-epoch` fact so saves can detect being reopened under a changed
generator; `genesis_notes` reading the ledger's facts rather than
re-deriving; a calendar accessor at the composition root for climate to
consume; the `tidally-locked` predicate name versus the `is-<kind>`
convention, due at the registry review.

## Artifacts

[The Sky of Seed 42](../gallery/the-sky.md) — the paired almanacs,
regenerated and drift-checked by CI alongside their constant-sun ancestor,
which is preserved unchanged as Campaign 1b's artifact under a pinned
`--sky constant`.

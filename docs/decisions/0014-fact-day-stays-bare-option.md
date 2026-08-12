# 0014. `Fact.day` stays a bare `Option<f64>`

**Status:** Superseded by [0126](0126-fact-day-is-a-typed-world-time.md)
(2026-08-11; accepted 2026-07-05 — the "clear from context" premise got a
worked counterexample, a predicate no world could commit) · **Decider:** Nathan

In the context of the fact envelope's optional timestamp, facing whether to
wrap it in a richer type, we decided to **keep `Fact.day` a bare
`Option<f64>`**, accepting that it does not use a `WorldTime` newtype like other
time values.

**Context.** Recorded as a "do not relitigate" item in a plan's Self-Review
Notes. The field is simple, its meaning is clear from context, and wrapping it
buys no safety worth the churn. Noted here so the question does not resurface at
each review.

**Consequence.** No change; the record exists to close the question.

**See also.** Campaign 1b plan Self-Review Notes; `CLAUDE.md` "Ratified
decisions".

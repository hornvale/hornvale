# 0171. A player's acts are not filtered out of a saved played world

**Status:** Accepted (2026-08-22) · **Decider:** Nathan · **Derives from:**
[0168](0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md) (the
keystone) · **Relates:**
[0170](0170-an-out-of-character-act-bypasses-the-body-never-the-world.md) (the
stamp that makes an operator act auditable rather than hidden)

In the context of `Session::into_played_world` moving a session's ledger into
a saved `World`, we decided that **nothing filters a player's facts on the way
out** — a played world keeps the whole trail, in-character and stamped
out-of-character alike.

## Context

This resolves the metaplan's §6.5, and it is not an independent choice: it
falls out of the keystone. If the effect of an act belongs to the body, then
what the body did is part of what happened, and a saved world that omits it is
a record of a world that did not occur. A creature's walk and a possessed
body's walk are the same kind of event; filtering one is asserting they are
not.

The alternative — strip player facts at the save boundary so a saved world
reads as "pure" simulation — buys a purity nothing needs and costs the
property that makes a played world worth saving: that you can reload it and
the history is still true. It would also make an operator's `provoke` vanish
along with its stamp, destroying the auditability
[0170](0170-an-out-of-character-act-bypasses-the-body-never-the-world.md)
exists to provide.

## Consequences

- `into_played_world` performs no predicate-, subject- or provenance-based
  filtering, and a test asserts the saved count equals the in-session count.
- **That test carries a precondition assertion** (`in_session > 0`), and the
  reason is worth keeping: without it, a session that committed nothing
  compares `0 == 0` and passes while proving nothing. A guard against a filter
  is vacuous unless something was there to filter.
- A saved played world is therefore **not** distinguishable from a purely
  simulated one by absence — only by the operator stamps it honestly carries,
  and (today, and not by design) by the provenance vocabulary a possessed body
  draws from, which
  [0167](0167-a-driver-is-interchangeable-and-a-possessed-body-is-a-creature.md)
  records as a gap rather than a feature.
- Anything later added at that boundary — a filter, a rewrite, a redaction —
  reverses a ratified decision and needs its own record, not a quiet commit.

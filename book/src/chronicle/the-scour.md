# The Scour

A channel silts up from the sides. Nothing dramatic falls into it; the flow
simply slows, a little at a time, until the day someone measures the head loss
and finds most of it is not the works at all but the walls. Scouring is the
cheapest possible remedy and the least interesting to describe, which is
exactly why it is usually skipped in favour of building something.

The Penstock had measured the head loss. A simulation tick grew superlinearly
in the number of creatures — a fitted exponent of 1.43, and 2.17 across the
stretch from a hundred agents to two hundred, which is to say very nearly
quadratic precisely where it matters. Both of the terms it could measure
directly, planning and committing, were flat per creature. So the excess lived
in the one term it could not see, and the campaign named a suspect: twelve
places in the liveness engine that still asked the ledger for *every fact under
a predicate* and then threw away all but one creature's.

That is the same defect its predecessor had already fixed twice, at two other
sites, using a method it had built for the purpose. The remaining twelve had
simply not been found, because the search that looked for them was a single
line of pattern matching against a single predicate, and there were four.

## What was done

Twelve one-line substitutions. Each `find(predicate).filter(|f| f.subject == e)`
became `facts_of(e, predicate)`, which descends the subject-predicate index
instead of walking the log. No new types, no new machinery, nothing added to
the public surface. The diff removes more lines than it adds, because each
change collapses a two-line chain into one call.

```
                      before (2 runs)      after (2 runs)
  fitted slope         1.43 / 1.52          1.12 / 1.11
  tail, 100 -> 200     2.17 / 1.99          1.41 / 1.29
  ms per tick @ 200      5722.7            1752.2 / 1712.6
```

Three and a third times faster at two hundred creatures, and the near-quadratic
tail is largely gone.

## The check that mattered more than the speed

A performance change is only interesting if the world it produces is the same
world. The bench reports several counters that are *deterministic* — facts
committed per creature per tick, planning searches per creature per tick, bytes
held, and the raw totals behind each. Every one of them is **byte-identical**
across the change. Only wall time moved.

That is a stronger statement than a passing test suite, and it is corroborated
from two other directions: the committed seed-42 world still serialises to the
same bytes, and twenty-one scans deliberately left un-repointed inside the test
module — kept naive precisely so they remain an independent oracle against the
indexed path they check — still agree with it.

## The result subtracts work

The reason to record this campaign at all is what the measurement did to the
programme that motivated it.

The design this belongs to proposes a cached working set: materialised views
over the ledger, keyed on the axes the permutation indexes do not cover, filled
on demand and dropped when cold. Its justification was the superlinear read
cost. That cost has now mostly been removed by call sites rather than by
machinery, so the justification is weaker than it was this morning, and the
specification says so in the section that used to argue for it.

Something superlinear does remain — an exponent near 1.1, with a tail near
1.3 — and it has not been attributed to anything. The honest position is that
the quarry is smaller and its location is unknown, which is a better place to
stand than the one before, and not the same as finished.

What has not changed at all is the writing side. Facts committed per creature
per tick are identical before and after, because none of this touched the
path that commits them. The ledger still grows without bound over a long
enough run, and the remedy for that remains untouched and unavoidable.

# The Column — retrospective

Process lessons, not product.

## A bad measurement nearly became a design constraint

The first reachability probe reported six distinct rooms from four hundred
movement attempts, which reads unambiguously as "the walker cannot travel" — and
would have reshaped the whole campaign, most likely into building the depth band
behind a debug entry point because the sea was unreachable.

It was an artifact of the probe. The script cycled eight compass directions on a
mesh where every room has exactly three exits, so it was refused most of the
time and oscillated the rest. The tell was in the same output: **three hundred
of the four hundred moves had succeeded.** Movement worked; the walk was
circular.

**Lesson:** when a probe reports "impossible", check the probe's own success
counter before believing it. A measurement that implies a capability is missing
deserves the same scepticism as one that implies a bug is absent. This is the
third time in this stretch of work that a derived number was wrong in a way the
arithmetic could expose — the same family as counting occupation layers by the
wrong predicate.

## Reusing a band beat inventing one

The Stratum specced three options for making strata inhabitable: extend the room
address, add a band, or make depth a query parameter. The band won on precedent
rather than on elegance — The Lintel had shipped one days earlier, so the
pattern, its refusals, and its dispatch guards were all sitting there to copy.

The copy was close enough that the bugs were the same bugs: the bare-compass
fallthrough needed the submerged guard repeated on it, exactly as it had needed
the indoor guard repeated after The Lintel merged. Knowing that in advance made
it a line of code rather than a merge surprise.

**Lesson:** when a spec offers options and one of them mirrors something that
shipped recently, that similarity is worth real weight. Not because the code is
reusable — almost none of it was — but because the *shape of the mistakes* is
already known.

## Two gate failures, both the enforcer doing its job

The type audit rejected a field-level `type-audit:` tag on a struct that carries
its tags on the struct line, and the architecture test rejected the new
`windows/vessel → domains/climate` edge until the generated layering page was
rebaselined.

Neither was a defect and both were correct to stop the merge: the first is a
convention that is invisible until violated, and the second is a genuine
architecture change that should be reviewed as one. The dependency was accepted
rather than avoided — vessel now reasons about strata, so the edge is honest,
and routing it through a locale re-export would have hidden a real relationship
to keep a graph smaller.

**Lesson:** an enforcer that fails on deliberate changes is not noise. The
question it asks is "did you mean this", and the answer here was yes twice.

## Follow-ups

- **Swimming.** Lateral movement while submerged is refused. Making the column
  traversable needs the destination cell's floor depth before it can be honest
  about which stratum you arrive in.
- **The surface is not a stratum, quite.** `Stratum::Surface` now does double
  duty — the overworld's only layer, and the sea's air-water boundary. It works
  and reads correctly, but a reader of the enum will not guess it.
- **Reachability is a real cost.** Twelve hundred rooms to the coast is honest
  at walk depth and tedious as play. Nothing here is wrong; it is a hint that
  travel wants a coarser band of its own eventually.

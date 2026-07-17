# The Terminator

**July 2026 · outcome: merged — locked-world habitability corrected; the
motivating religion payoff measured and falsified, banked as a follow-up**

## What was attempted

An investigation into a stalled census number, run as part of a sibling
campaign's own dig into why locked worlds had stopped heading a
tide-driven pantheon, found the actual defect one layer downstream of
where it was first suspected. The habitability substrate's `insolation` field —
`windows/worldgen`'s per-cell input to which species dominates which
ground — was rotation-regime-blind: computed as a function of latitude
alone, with no branch on whether a world spins or hangs fixed above its
star. Applied to a tidally locked world, whose sky is organized around a
substellar point rather than a rotating day, the field read identical
insolation at the scorched point directly beneath the sun, the frozen point
on the world's far side, and the twilight band between them at the same
latitude. It could not see the terminator at all.

The tell was already sitting in the source. Both of the domain's other
substellar-driven fields — mean temperature and moisture — already branched
correctly on the locked regime, each carrying its own private copy of the
same substellar-point constant and the same cosine-of-angle arithmetic.
Insolation was the lone holdout, the one field that had never been given
the day/night treatment its two siblings already had, and its own test
destructured the rotation regime and threw it away unused without anyone
noticing. The consequence, once named, explained the stalled census number
precisely: settlement dominance on a locked world piled onto the false
latitude-only maximum at the substellar point, so the world's ruling belief
skewed toward the fixed sun instead of the tide the terminator's mild band
should have favored — silently masking a payoff an earlier campaign had
already shipped, not reversing it.

## The fix, and the guard against its own recurrence

The correction itself is small: on the day side, insolation now falls off
with the cosine of the angle from the substellar point, the same Lambert
relationship a flat surface receives real starlight by, floored to nothing
on the night side. What makes the fix durable is where it was put. Rather
than write a third private copy of the substellar geometry into the
insolation code, the campaign extracted the one the temperature and
moisture fields already duplicated into a single shared function the whole
climate domain now calls — and had temperature and moisture drop their own
copies to call it too, proving with an equality test that the extraction
moved no arithmetic, only its address. A future field that reads light or
heat from a locked world's sky inherits regime-awareness for free; the
specific class of bug this campaign fixed — a substellar field that forgets
to branch — cannot recur by omission, because the geometry now has exactly
one home.

The correction is scoped tightly on purpose. A spinning world's annual-mean
insolation genuinely is a function of latitude — averaged over a full day,
longitude washes out — so the spinning path was never wrong and stays
untouched, byte for byte, guarded by a direct equality check over every
cell of a spinning world's own field rather than left to inference. Only
locked worlds re-derive, and only their habitability, settlement dominance,
and presiding religion move as a result — a save-format change scoped to
one rotation regime, not a new terrain epoch.

## Measure before tuning, twice

The roster's per-species insolation preferences had been authored years
earlier against the old, badly narrow locked-world range the buggy field
produced, and the honest worry going in was that the fix would widen that
range enough to strand every species' preference back at the wrong end of
it, needing a rebalance before the payoff could be judged fairly. Rather
than guess, a read-only instrument applied the corrected field synthetically
to every locked seed it could find and asked, before touching a single
species definition, where the winning population's numbers actually peaked.
On every locked world found this way, the dominant people's habitat
clustered tightly in the terminator band, not at the sun-facing point — an
accident of scale, where preferences tuned for "faint light" under the old
model happened to land near "near the terminator" under the corrected one.
No roster change was needed, and none was made; the fix's real behavior was
measured first, and the measurement said the campaign's second-guess had
been unnecessary.

## What the acceptance battery found

The campaign's whole reason for existing was a specific, named prediction:
that correcting insolation would restore locked worlds to the tide-headed
pantheons an earlier campaign had shipped and a later one had silently
undone. A dedicated measurement, run once the fix and its guards were both
in place, checked that prediction directly against real worlds rather than
the synthetic probe's proxy — and the prediction did not hold. Every one of
the locked worlds sampled still headed a fixed, sun-derived belief; not one
headed the tide. The habitability half of the story was exactly right —
a cross-check against the same worlds confirmed the dominant people's
habitat really had moved into the terminator band on every one of them,
matching the earlier measurement precisely — but the world's *ruling*
belief turned out to be decided
by something the insolation fix never touches at all: which of a world's
peoples happens to found its very first settlement large enough to raise an
organized cult, in an alphabetical tie-break among founders that owes
nothing to which people the corrected physics actually favors. The
tide-born belief was there in every case, committed and real — just never
first in line.

That is not a null result dressed up as one; it is a second, independent
defect the first one had been quietly hiding behind. Fixing the field a
world's habitat reads was necessary for the tide to ever have a chance of
presiding again, and it was not sufficient, because presidency is decided
somewhere else entirely. The honest read, confirmed rather than assumed:
the habitability fix is complete and correct on its own terms, and it
uncovered rather than closed the gap between "a people is ecologically
dominant" and "a people's belief leads the pantheon." That gap is now named,
measured, and handed forward rather than left implicit.

## The road ahead

The habitability correction ships as what it is — a genuine fidelity
repair to a field every consumer downstream of it, from settlement
placement to the pantheon a flagship raises, was quietly reading wrong on
every locked world sampled. The religion payoff it was chased for
does not ship with it, and is banked precisely rather than papered over:
whichever people's belief presides over a world's pantheon should track
which people actually dominates that world, and today it tracks something
else — an artifact of founding order that has nothing to do with the
physics this campaign corrected. Restoring the tide waits on that second
fix, in a campaign of its own.

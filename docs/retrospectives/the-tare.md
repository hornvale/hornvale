# The Tare — retrospective

Process lessons, not product.

## "Modelled, authored, unreachable" is a defect class

`Kingdom::Crystalline` was a defined type with authored prose that appeared in
no candidate, so it could not occur in any world. The Shoal found the same
shape one campaign earlier: ten marine biomes the climate domain classified and
the prose layer had no words for.

Both were invisible to every test, and necessarily so — **a test can only fail
on behaviour that happens.** Neither a unit test nor a drift check nor a
property battery can observe the absence of a thing nothing produces.

The cheap guard, added here, is a reachability test: assert that every variant
of a closed enum is producible by the machinery that is supposed to produce it.
`every_kingdom_is_reachable` is four lines and would have caught this the day
`Crystalline` was declared.

**Lesson worth generalising:** for any closed set the world is supposed to draw
from — kingdoms, biomes, weather states, exit kinds — ask whether something
asserts each member can actually occur. Enumerable types make this trivial and
almost nobody writes it.

## A comment is a specification nobody runs

The fungal score's comment named three clauses and the code implemented one.
That is not a typo; it survived because comments are not executable and the
resulting behaviour — exotics being placed, plausibly, in quiet places — looked
entirely reasonable from outside.

The diagnosis only became possible once `locale --strange` listed the sites
side by side and the descriptor column made the monoculture visible. The
listing was built two campaigns ago for a different reason.

**Lesson:** when a comment enumerates conditions, check the code enumerates the
same ones. This is a cheap review question with a good hit rate, and it is
exactly the kind of thing that is obvious in hindsight and invisible in a diff.

## Measure the flagged risk first, not last

The Stratum's spec flagged O2 — that exotic placement feeds the `uncanny`
drive signal and therefore the population-health battery — and said explicitly
that any campaign touching the overlay owes a health run as its *first* step.

That instruction was followed, and it cost about ten minutes: run the battery
before touching anything, so that a later red is unambiguous. It stayed green,
which means the ten minutes bought certainty rather than a fix — which is the
normal and correct outcome for a risk check.

**Lesson:** a spec that flags a risk should also say *when* to measure it. "This
might interact with X" is much weaker than "measure X before you start".

## The fix was two fixes, and the first one was not enough

Rescaling the fungal score moved the share from 92–98% to 78–91%. Real, and
short of the goal. Stopping there and reporting "commensurability fixed" would
have been true and useless.

What closed the gap was noticing that the *candidate table* had three of twelve
possible kingdom/energy combinations, and that one kingdom had none at all.

**Lesson:** when a fix produces a real but insufficient improvement, that is
evidence the diagnosis was partial, not evidence the fix was wrong. Measure
against the goal, not against the previous number.

## A grep filter ate its own evidence

The artifact drift check was run as `git status --porcelain | grep -v "src/"`
to filter source files — which also filtered `book/src/gallery/...`, i.e. every
artifact being checked. It reported "no drift" for a change that had visibly
rewritten a page.

Caught within a minute because the page contents were inspected too, but the
mechanism is the recurring one: a filter narrower than the question. Same
family as truncating a discovery grep with `head`.

**Lesson:** when filtering output to check a claim, verify the filter cannot
remove the evidence the claim is about. Prefer showing everything when the
output is small enough to read.

## Follow-ups

- **Nine of twelve kingdom/energy combinations remain unused.** The candidate
  table now has four entries. This is authoring headroom rather than a defect,
  but the reachability test makes the gap explicit for the first time.
- **`DRAINAGE_SCALE` is a chosen constant.** Picked so an ordinary watercourse
  counts as damp and a single upstream cell does not, and its effect was
  measured across four seeds — but it was not derived from anything. If the
  drainage model changes, re-measure the distribution.

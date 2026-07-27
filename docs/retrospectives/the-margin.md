# The Margin — Retrospective

Process lessons only; the product is in
[the chronicle](../../book/src/chronicle/the-margin.md).

## What worked

**Working a retrospective's follow-up list as its own batch.** The Purview's
retro named ten items and triaged them. Taking them all at once, immediately
after the campaign closed, meant the context was still live — every fix could
be justified from a review finding rather than rediscovered. Two of them
(the biome double-noun, the drift-check rule) were not cleanups at all; they
were decisions the campaign had deferred, and they were cheaper to settle here
than they would have been to relitigate in six months.

**Reviewing a cleanup batch as if it were a campaign.** Ten small commits look
individually harmless, which is exactly why a batch review earns its keep: it
caught a false claim in a message written *to fix a false claim*, a reference
page left stale by a later commit in the same batch, and an inconsistency in
the exclusion list the batch had just edited. None of those were visible from
inside a single fix.

**Mutation-testing the two behavioural changes.** The reviewer inverted
`examine`'s catalog order and reverted the biome legend to the slug, confirming
each change was actually load-bearing and each test actually pinned it. Carried
over from The Purview and still the highest-value reviewer habit available.

## What to do differently

**A message that names a bound is making a claim.** Three times now the same
verb has shipped a refusal that was more specific than it was true: an internal
error about the canonical grid, then a `u32::MAX` ceiling the world does not
have. Both were written to be *more helpful* than a bare refusal, and both were
wrong in a way the next input exposed. The rule worth carrying: a refusal may
name a bound only if it is the bound actually enforced, on the same code path;
otherwise say no and let the real check speak.

**A doc comment that claims uniqueness invites a grep.** The extracted lat/lon
helper was documented as "the sim's *one* routing" while three byte-identical
copies survived in a domain crate nobody had searched. The claim was cheap to
make and cheap to check; nobody checked until review. Either make the claim
true in the same commit or do not make it.

**Generated prose is a bad authoring seam.** The gallery page began life as
fifty lines of `printf` inside the regeneration script, so a direct edit to the
committed `.md` would be silently destroyed on the next regen. It now uses
mdbook `{{#include}}` for the generated charts and hand-authored prose around
them — the pattern the laboratory pages already used, which nobody checked for
first. Look for the existing pattern before inventing an authoring seam.

## The item that was not ours

`make vessel-check`'s failure, attributed at The Purview's close to an inherited
regression, **did not reproduce** — the smoke driver already scouts past
settlement-free seeds, fixed on main by The Snapshot. The attribution was made
from a single run without re-checking against a current main, and it was wrong.
Correcting it here rather than leaving a false accusation in the record.

## Follow-ups

- The possession transcripts carry the same thresholded-classification exposure
  the new exclusions were written for, and are deliberately kept in the CI diff
  (decision 0078 states why). If CI ever flakes on them from a foreign platform,
  the fix is to exclude them, not to chase the classification.
- `examine`'s *miss* path still builds the whole chart; only the hit path is
  fast. Unmeasurable against debug-build worldgen noise, so left alone.
- Still deferred from The Purview, unchanged: per-species sense radius (EXP-3),
  a remembered cell that is wrong rather than stale (MEM-1), the anti-map
  (RENDER-7), an NPC's own purview, and the graphical tilemap client.

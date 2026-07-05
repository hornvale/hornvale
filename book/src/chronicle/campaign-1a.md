# Campaign 1a: The Kernel

**July 2026 · 17 commits · outcome: complete, merged**

## What was attempted

Build the substrate every future domain depends on — and *only* that:
hierarchical seeding, coherent noise, fields, the concept registry, the fact
ledger, the trivial refinement engine, the trace protocol's vocabularies, and
world assembly with deterministic persistence. No domains, no windows, no
game. The exit test: two runs from the same seed must produce byte-identical
serialized worlds, with every kernel capability exercised along the way.

## What landed

All of it. Fifty-five tests, including an integration suite that runs a
miniature genesis — derive seeds, sample noise and fields, refine a village
name against the ledger, observe phenomena, commit beliefs — twice, and
asserts the results are the same bytes.

## What was learned

- **The plan's sample code was the main defect source.** Three of the five
  review findings traced to the plan itself (missing documentation in its
  code samples, a test using a shared temp path). Lesson adopted: implementers
  are told the plan's samples may lapse below the plan's own standards, and
  the standards govern.
- **The final review earned its keep.** Two invariants were hardened before
  merge that would have been expensive to retrofit once real producers exist:
  the ledger now rejects non-finite numbers at commit time (they previously
  would have crashed at save time, far from the culprit, and broken fact
  identity), and loading a world validates that entity-minting state is
  consistent with the facts, so a corrupted save fails fast instead of
  silently violating the never-reuse guarantee.
- **Two save-format decisions were ratified explicitly** rather than left to
  drift: a fact's timestamp stays a bare number (the envelope stays dumb),
  and a predicate definition carries its own name redundantly with its
  registry key (values stay self-describing). Recorded so future campaigns
  don't relitigate them.

## Deferred, deliberately

Enumeration interfaces over the ledger and registry (the almanac needs them
on day one of 1b); precomputing per-octave seeds if terrain sampling becomes
hot; validation of phenomenon kinds against the registry at observation time
(a documented contract for now).

## Artifacts

See [First Light](../gallery/first-light.md) — the fractal noise surface of
seed 42, and the entire world of seed 42 as of this campaign, rendered
readable. The world contains one valley, one village, and one revered
phenomenon; its smallness is the honest measure of where things stand.

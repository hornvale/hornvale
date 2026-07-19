# Retrospective — The Single Sculpt

One page of process lessons. The change — build the terrain once and thread it —
is in [the chronicle](../../book/src/chronicle/the-single-sculpt.md).

## What worked

- **Map before design paid off, again.** The followup was filed as "genesis
  re-derives terrain ~9×." The Explore map corrected that to the *accurate*
  figure — 4 genesis sculpts (3 redundant) plus ~7 in the *almanac* render (a
  separate subcommand). Designing against the real call-count, not the
  remembered one, kept the fix scoped to the three leaking sites instead of a
  speculative sweep.

- **"Reconnect the flow," not "add a cache."** The ideonomy pass reframed the
  fix through connectivity: the terrain was *already* built and flowing through
  the middle of genesis; only two edges were cut (the terrain stage discarding
  its globe, the deep-time stage rebuilding blind). That framing pointed
  straight at explicit threading (extend the existing `_from` idiom) over
  memoization — which would have bolted hidden mutable state onto the most
  determinism-sensitive type to solve a problem that was really just a severed
  wire.

- **Byte-identity as the correctness proof, verified personally.** For a pure
  perf change the whole claim is "output unchanged," and the pipeline's purity
  (idempotency tests already present) makes the kept globe provably the same
  object the rebuild produced. The proof is simply that every artifact
  regenerates unchanged — and I ran that regen + `git diff --exit-code` myself
  rather than trusting the subagent, which mattered here because the subagent's
  final reports came back terse and incomplete (it kept ending its turn while a
  background test run was still pending). The verdict came from my own
  regen/gate, not its message.

## What to carry forward

- **A terse/incomplete subagent report is not a green light.** This agent
  returned "all confirmed, awaiting tests" with no verdict, timings, or
  byte-identity evidence — twice. The right response was to verify the worktree
  state directly (build, regen, diff, gate, time) rather than infer success.
  When a subagent's report lacks the evidence you asked for, treat the task as
  unverified and check it yourself.

- **The pure-and-expensive smell.** The general lesson worth keeping: a value
  that is a *pure, expensive* function of the seed should be built once and
  passed by hand. Grep for other `X_of(world)` re-derivations on hot paths (the
  sky, the demography field) before they accrete the same waste; (the sibling "hot-path" lesson from The Elements).

## Handed forward

- No new followups. The genesis + almanac terrain re-derivation is closed
  (idea-registry `PERF-genesis-terrain-rebuilds` → shipped). Other `_of(world)`
  hot-path re-derivations (sky, demography) were not measured here; worth a
  glance if their callers ever loop.

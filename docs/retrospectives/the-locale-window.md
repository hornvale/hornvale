# The Locale Window — retrospective

**Completed:** 2026-07-12 (name-only designation per decision
`slugs-not-numbers`; plan `docs/superpowers/plans/2026-07-12-the-locale-window.md`)

**Registry-first plus one ideonomy pass over the design caught four
interface-stability issues before a line of implementation code existed.**
The design session (`docs(spec): The Locale Window — P2 campaign 2 design`
followed by `docs(spec): refine locale window after an ideonomy pass`) fixed
a reusable `LocaleContext` instead of rebuilding the coarse world inside every
`describe` call, threaded `WorldTime` through the signature even though v1
only samples the time-independent annual mean, named the vertical verbs
`Enter`/`Exit` rather than `Up`/`Down`, and versioned the schema as
`locale/room/v1` from the start. None of these were forced by a failing test
— each is a signature or naming choice that would have been an expensive,
API-breaking retrofit had it surfaced only after `describe` had callers. The
durable lesson from The Room Mesh (spike the keystone risk before the spec
freezes) generalizes past algorithmic risk to interface risk: a design-time
pass whose only output is "don't paint yourself into this corner" is cheap
insurance against exactly the kind of churn a versioned save-format schema
cannot absorb after the fact.

**A cross-platform near-miss surfaced in review, not in CI.** Task 2's first
draft of the blend test pinned an exact `biome` string alongside the
quantized-float field values — a per-cell classification that is thresholded
on host-libm transcendentals, the identical class of value already keeping
main's CI red for an unrelated reason (a Linux/macOS divergence in a
different domain's homophony means and lands-drift seed). Review caught it
before the commit landed (`test(locale): make the pinned blend test
cross-platform-safe`), and the fix kept the platform-exact pins (weights,
quantized floats) while demoting biome to a membership check. The pattern is
now explicit enough to name as a standing rule: **any assertion on a
biome/classification value must never enter the both-platform `cargo test
--workspace` gate as a value pin** — it may only assert membership,
determinism-of-repeated-call, or be excluded the same way the scene-tiles and
biome-PNG artifacts already are. Catching this by inspection worked this
time; the project does not yet have a lint that would catch a future
recurrence automatically, which is worth a `TOOL-` entry rather than trusting
review a second time.

**The six-task TDD slice held with no task reordering and no scope bleed.**
Task 1 (the one new kernel surface) landed first because Task 5's CLI depends
on it; Tasks 2–4 built the schema, texture, and exits in the order the spec's
§13 laid out; Task 5 wired the CLI and the drift-checked artifact; Task 6 is
this document. Each task compiled and gated independently — the deliberate
temporary stubs Task 2 left (`texture_of`, `exits_of`, a string-y `Exit`)
were replaced on schedule by Tasks 3 and 4, never carried past their planned
lifetime. The one real correctness risk flagged in the plan's own notes — the
child-order match in `RoomAddr::containing` versus `corners`'s `match d`
arms — did not in fact trip; the round-trip property test passed on the
implementation's first pass, evidence that naming the risk explicitly in the
plan was enough to make the implementer verify it deliberately rather than
by accident.

**A subagent parked itself on a background test-monitor once during the
campaign; the foreground-only dispatch discipline recovered it without
losing work.** This is a process near-miss worth recording precisely because
it repeats across campaigns when a dispatch prompt does not explicitly ban
backgrounding long commands: the fix is not a one-off correction but the
standing preamble every subagent dispatch now carries, and this campaign is
one more data point that the preamble is necessary, not decorative.

## Estimate vs reality

The plan's six tasks ran close to estimate. Two small fixes landed inside
their own tasks without a separate review cycle: the cross-platform biome-pin
softening in Task 2, and a one-line correction to `Exit.to` (a packed room id
being treated as a count) caught and fixed within Task 4's commit sequence.
Neither required touching an already-closed task or re-running an earlier
gate. This endgame task found no genuinely stale room-scale or frontier
chapter beyond the two registry rows the spec already named as needing
updates (MAP-28's first-consumer note, MAP-29's inheritance-path note); one
adjacent registry row (MAP-30) was also sharpened, since its "biome-via-field"
phrasing predated this campaign's resolution that biome inherits categorically
rather than blending as a field. No Confidence-Gradient bet's checkability
tier moved: the "coarse constrains fine" bet's still-open half — the runtime
active-region render swap, its delta store, and the adaptive-depth walk — is
untouched by a text/data consumer of the same inheritance hooks, so the
open-questions chapter was left as is rather than re-scored on invented
grounds.

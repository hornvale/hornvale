# The Sextant — retrospective

Process lessons, not product. A measurement campaign that shipped an
instrument and no fix.

## Search the repo before accepting your own recommendation

The first answer to "what shape is the fixture?" was a standalone
`tools/scene-bench/` outside the workspace, with criterion for the
statistics. It was overturned by looking, not by arguing: the workspace
already holds four committed profilers (`profile_build.rs`,
`profile_terrain.rs`, and their siblings) plus a budget-test precedent
(`cli/tests/graph_cost.rs`), all in-workspace and all with zero new
dependencies.

The reasoning that killed it is worth keeping, because it is a general
mistake. The `tools/type-audit/` carve-out (decisions 0027/0028) is real
precedent for putting a tool *outside* the workspace — but that carve-out
exists so a tool can own its **dependencies** without importing them into
the dependency allowlist. This tool needed none. Citing a precedent whose
*rationale* does not apply is how a codebase grows a second convention for
the same job.

**Lesson:** before proposing a new structure, find the closest existing
instance and check whether its justification transfers. A precedent is a
reason, not a permission slip.

## The obvious workload would have measured nothing

The natural design — exercise each scene function once, exactly as
`profile_build.rs` exercises each build stage — would have missed the entire
finding. A single call to a region patch reports about seven hundred
milliseconds and reads as *merely expensive*. The 91% redundancy is only
visible when calls **repeat**, because redundancy is a property of a
sequence, not of a call.

The workload that found it is shaped like the *consumer's session*, not the
producer's function list. That choice came from asking why the producer-side
and client-side instruments were asymmetric, and noticing the defect lives in
the seam between them.

**Lesson:** a benchmark's workload is a hypothesis about where the cost is.
The default workload — one call per function — encodes the hypothesis "cost
is per-function", which is precisely the hypothesis that cannot see
redundancy, cache misses, or amortization. Ask what shape the *consumer*
makes before choosing the shape of the fixture. This is the sibling of The
Sounding's lesson (census the workload before you trust the timing): that one
says prove the phenomenon happened; this one says make sure the workload can
contain it at all.

## The strongest guard was coupled to the fix, and only the design found that

The campaign was scoped fixture-first by owner decision, on the assumption
that the fixture half could deliver the full guard set independently. It
could not, and that surfaced only while writing the design: the guard worth
having is structural (*derives terrain at most once per world*), and it has
no seam to observe against while every scene entry point takes a `&World` and
derives internally. Writing it today requires either counter instrumentation
existing solely for a test, or the artifact-taking API that the fix
introduces.

This materially changed what "fixture first" buys — wall-clock ceilings above
a known-bad number rather than a guard against reintroduction — so it was
escalated as an owner decision rather than absorbed silently.

**Lesson:** when a campaign is split into "measure now, fix later", check
during design which half each intended guard is anchored to. A guard coupled
to the fix is not a guard the measuring half can promise, and discovering
that at close is a scope surprise; discovering it at design is a decision.

## Two halves of one campaign wanted opposite build profiles

The plan's commands omitted `--release`, which the implementer caught. It was
not a typo with one right answer: the profiler is a *release* measurement,
because that is the build whose numbers describe the shipped path — but
`scripts/gate-full-heavy.sh` runs the heavy tier **without** `--release`, so
ceilings derived from release numbers would sit roughly 2× too tight against
the profile the gate actually runs. The test's ceilings had to be measured by
running the test itself, three times, taking the slowest value per metric.

**Lesson:** a plan that names a command must name its build profile, and a
measurement's provenance must travel with it. Both numbers are recorded in
`cli/tests/scene_cost.rs`'s module doc, labelled with which profile produced
them and which one is the ceiling basis — because the failure mode here is
not a wrong number but a right number applied to the wrong profile.

## Ceilings have one direction of travel unless you say otherwise

`graph_cost.rs`'s history is a clean, well-documented sequence of upward
re-baselines as the world grew. Each was correct. The mechanism nevertheless
only ratchets one way, and nothing in it distinguishes "the world got bigger"
from "we regressed and raised the bar." The response here was documentation,
not machinery: every constant records measured value, date, and host, and
raising one is an explicit reviewed act recorded in the constant's own
comment.

**Lesson:** a threshold with no asymmetry between tightening and loosening
decays into a record of the worst observed behaviour. The asymmetry can be
pure convention, but it has to be written down where the next person editing
the constant will read it.

## Follow-ups

- **The structural "derives once" guard** is deferred to the fix campaign,
  with the reasoning in the spec (§3.5) and the followup register. The fix's
  shape already exists in-repo: `windows/locale`'s `LocaleContext` performs
  the same `terrain_of` + `climate_from` pair once and reuses it across every
  `describe`, and says so in its doc comment. `windows/scene` has no
  equivalent.
- **Ceiling headroom is ~2.0×**, the plan-mandated floor.
  `cli/tests/graph_cost.rs:313-333` records a 6.6× swing (1.51 s – 10.00 s)
  for identical work under three-way parallel load on this box, which is why
  its author chose 3.0×-worst-loaded. Parallel campaign sessions are the
  norm, so the flake risk is real. **Recommendation: loosen to ~3×** — which,
  under the ratchet discipline above, is an explicit reviewed act and must be
  recorded in each constant's comment.
- **The cross-repo LOD constants** (`TILE_QUADS`, `REGION_MIN_LEVEL`) are
  mirrored from the Orrery with a source comment and nothing enforces the
  mirror. Accepted with eyes open (spec §5.3); the alternative is a shared
  manifest for two integers.

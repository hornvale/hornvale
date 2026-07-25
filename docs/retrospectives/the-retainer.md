# The Retainer — retrospective

**Completed:** 2026-07-23 (spec `docs/superpowers/specs/2026-07-23-the-retainer-design.md`,
plan `docs/superpowers/plans/2026-07-23-the-retainer.md`; Fix A: terrain+climate
dedup at 13 sites; Fix B: `sky_report_from` reuses the session's providers; Fix C
subsumed). Fourth perf campaign of the session. Ran under campaign-autopilot,
subagent-driven execution.

**Profiling the one entry point nobody ran found the biggest single win of the
session.** `possess` — the interactive game seam — was 5.4 s, ~9× a full genesis,
and had never been profiled because the session's perf work targeted generation
and the census. The lesson from The Bearing ("profile what you actually run,
enumerate the entry points") paid off again, larger: the *interactive* path,
where latency is felt by a human, was the slowest and most wasteful in the
codebase.

**The design's real content was what it *rejected*.** The obvious answer —
memoize built providers globally, keyed by world — is wrong here, and the
ideonomy's matrix made it undeniable: the census builds each world exactly once
(one-shot), so a cross-world cache has a ~zero hit rate for it, while risking OOM
across thousands of worlds and standing up a second source of truth against the
seed-and-ledger constitution. `possess`, by contrast, is *episodic* (many
observations of one frozen world), so reuse belongs at session scope. Cross-domain
(build systems, HTTP caches, `useMemo`) all say the same: scope reuse to the unit
of work, bound any shared cache. The campaign is defined by choosing scoped reuse
over a cache — the cache was the trap.

**Byte-safe by construction, because the seams already existed.** Fix A is
byte-identical *by definition*: `climate_of(world)` literally *is*
`climate_from(world, &terrain_of(world))`, so switching a caller from the former
to the latter (with the terrain it already has) cannot change a byte. `climate_from`
was already exported and already used in one lab path ("The Single Sculpt") — the
codebase had solved this once and not propagated it. Fix B mirrored that: a
`sky_report_from` twin taking the providers. The whole campaign was recognizing an
existing idiom and finishing its spread — which is why the artifact diff was empty
on the first try for both fixes.

**Diagnosis-before-fix was load-bearing in Fix B.** The plan deliberately left Fix
B's residual "to be pinned at execution," because after Fix A the profile still
showed terrain rebuilt ~5×, and the cause was not obvious from reading: `sky_report`
re-derives the whole world for a one-line weather phrase, and `observable` calls it
every glance. Guessing would have chased the wrong thing (the world-load, the NPC
derivation); the profile named it exactly. Fix C (a day-keyed sky memo) evaporated
once Fix B landed — with the providers threaded in, the per-glance sky call no
longer derives anything, so there was nothing left to memoize.

**The win, and the honest remainder.** `possess` fell 5.4 s → 2.27 s (~2.4×),
byte-identical (transcripts unmoved). Fix A also removed the census `WorldView`'s
double-terrain — a real census win, unmeasured directly because the census golden
regen is out of band. After the fixes, the top cost is `vessel::liveness` (~49%) —
the NPCs' homeostatic drive cycle — which is genuine simulation, not waste, and a
separate campaign's to optimize.

**Scope notes.** No `open-questions.md` bet moved; no idea-registry row (infra).
Three geosphere/provider-perf campaigns shipped this session (The Lookup, The
Bearing, The Retainer); The Commons (Arc-share the mesh) remains parked at G3.

## Follow-ups (promoted from the ledger)

- **`build_world` returns a derived-provider bundle** — the deeper reuse (a built
  world carries its providers, so loading + describing derive once) needs a change
  to the world-build contract; deferred.
- **`vessel::liveness` (~49% of the post-fix `possess`)** — the NPC homeostatic
  simulation is now the dominant possession cost; its own optimization pass.
- **`sky_of` per-observation** — `sky_report_from` still re-derives astronomy each
  call (cheap relative to terrain, but a session sky-of memo could shave it).
- **The Commons** (parked at G3) — Arc-share the cached mesh; the climate-provider
  clone is its own follow-up.

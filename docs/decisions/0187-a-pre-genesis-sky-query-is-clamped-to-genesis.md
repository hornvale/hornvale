# 0187. A pre-genesis sky query is clamped to genesis, deliberately

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Campaign:** The
Escapement · **Supersedes:** nothing

In the context of `hornvale-astronomy`'s one crossing from `WorldTime` to
`StdDays` (`GeneratedSky::t`, `provider.rs`) inheriting a bare
`time.day().max(0.0))`, justified only by a code comment
("genesis-time queries never predate the world"), and facing The Escapement's
retype of `WorldTime` to an exact tick count — which forces every such comment
to become either a recorded choice or a bug — we decided that **a pre-genesis
sky query answers as genesis, by clamping the negative time to zero rather
than returning an `Option` or an error**, accepting that a caller cannot
distinguish "the sky at genesis" from "the sky before genesis" through this
API.

**What was inherited.** The clamp predates this campaign and was never a
decision on record — only a comment beside the code. Nothing would have
noticed if the clamp were removed, inverted, or replaced with a panic; no
test exercised the negative branch at all.

**What was traced.** No caller can reach the negative path today.
`GeneratedSky::t` is the domain's only `WorldTime -> StdDays` funnel, and it
clamps. The only sites in the workspace that bypass the funnel and construct
a `StdDays` directly for a sky/calendar query (`windows/worldgen/src/lib.rs`,
two call sites) pass a hardcoded `StdDays::new(0.0).unwrap()` — genesis
itself, never a negative value. So the clamp is exercised only by a
deliberately pre-genesis instant, which no production caller constructs.

**The choice, and why not the alternatives.**

- **Clamp to genesis (chosen).** The sky before the world exists is not a
  physical question — there is no star system, no calendar, no illumination
  history to answer it from. Treating "before genesis" as "at genesis" gives
  every caller a total function with no failure mode to handle, for a case no
  caller can actually produce.
- **Return `Option<SkyReport>` or an error.** Rejected: it would force every
  one of `sky_at`'s callers (several, across `windows/`) to handle a case
  that is unreachable today, in exchange for defending a query that has no
  physical meaning either way. An error return does not make "the sky before
  the world" more answerable than genesis does — it just relocates the
  clamp's judgment call from this function to every caller.

**Consequence.**

- `GeneratedSky::t` (`domains/astronomy/src/provider.rs`) is ported to the
  tick-shaped surface (`WorldTime::as_std_days`) as a pure rename — the clamp
  itself is unchanged, now stated as a decision rather than a comment.
- `domains/astronomy/src/provider.rs`'s own test module holds
  `a_pre_genesis_query_has_a_documented_answer`, the first test to assert the
  clamp's behavior rather than merely state it in prose.
- The NaN half of the old `max(0.0)` guard (`f64::max`'s NaN-to-0.0
  semantics) is gone by construction: `WorldTime` is built from an integer
  tick count and cannot hold NaN.

**See also.** Decision 0186 (an instant is an exact tick count — the Phase A
migration this record's task is part of); `docs/superpowers/plans/2026-08-23-
the-escapement.md`'s "Execution phasing" (Task 4).

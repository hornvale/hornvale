# Retrospective: The Surmise

**Closed:** 2026-07-19 · **Outcome:** merged — process lessons only (decision
0020). Registry: UNI-1 first instantiation, UNI-20 third instance, UNI-16
noted as still untouched.

## What happened

The campaign shipped its mechanism (T1–T4) against a deliberately-generic
resource, then hit a real, measured problem at the close: on seed 42, the
only "water" the terrain field could name was the salt ocean, thousands of
BFS hops from any settlement and the wrong referent besides (nobody drinks
the sea). Rather than ship or fake a fix, the campaign **parked** at T4 and
waited for a domain-level fresh/salt classification — [The
Freshet](../chronicle/the-freshet.md) — to land on `main`. T5 (this task)
absorbed that campaign, re-pointed the resource at `WaterKind::is_fresh()`,
measured whether the real demo now fires, found a second, narrower gap
(exploration policy, not classification), reported it honestly, and closed.

## Lessons

1. **A resource-agnostic mechanism parks cleanly and re-points at a correct
   referent without touching its own tests' logic.** The re-wire touched
   exactly one predicate (`is_water`/`Terrain::is_fresh_water`) and one
   backing implementation (`LocaleTerrain`); every keystone test (multi-source
   destination divergence, discovery, tie-break, fold determinism) needed
   only its *terrain construction* changed (a planted elevation map to a
   planted elevation map **plus** a planted fresh-water set), never its
   assertions. This is the payoff of designing belief as a pure function
   over an abstract `Terrain` trait from T1 onward, before a real referent
   existed to test against — the abstraction bought exactly the flexibility
   it was supposed to.
2. **Measuring the real mechanism, not the function that only looks like
   it, changed the finding.** The first re-wire pass measured reachability
   via `nearest_water`'s own address-ordered BFS (a `BTreeSet` frontier —
   convenient for determinism, not for representing real walking) and got
   an answer (found at ~93,000 expansions) that had nothing to do with
   whether the *actual* mechanism — `DriveMovements`'s greedy-downhill,
   never-revisit-within-a-call walk — would ever get there. Running the
   real mechanism directly (not a proxy for it) surfaced a completely
   different and more specific finding: the walk gets topologically boxed
   into a riverless basin, a budget increase would not help, and the
   failure is settlement-placement-dependent (a ten-settlement sweep found
   it succeeds elsewhere on the same seed). The general shape: when a
   codebase has two different traversals over the same domain for
   different purposes (one for a reference value, one for the actual
   behavior), measuring the wrong one produces a plausible-looking but
   false answer — check which one the question is actually about before
   trusting the number.
3. **A parked campaign's own measured gap became the fix for the NEXT
   campaign, and re-reading its own parked test paid off directly.** T4's
   `seed_42_home_settlement_water_reachability_is_a_measured_t5_gap` (this
   task's inherited name, later renamed) had already done the hard
   diagnostic work of ruling out "the field is broken" (a global sample
   found water everywhere) in favor of "the search doesn't scale to the
   mesh's real fineness" — that framing transferred directly to this task's
   own finding once the referent changed, saving a re-diagnosis from
   scratch. Parking with a precise, measured, non-vague note ("here is
   exactly what's wrong and why, not just 'doesn't work'") is what made
   picking it back up two campaigns later fast rather than a cold restart.
4. **A vestigial cross-window save-format coupling disappeared for free
   once its reason for existing did.** The old sea-level model needed
   `LocaleTerrain` to read `domains/terrain`'s `"sea-level-m"` genesis fact
   by a hardcoded string (no real dependency existed to check the name at
   compile time), plus a tripwire test whose entire job was catching that
   string drifting silently. The Freshet's `LocaleFields.water` field
   replaced the need for that derivation entirely — `is_fresh_water` reads
   a field that was already being computed and exposed through the normal
   `LocaleContext::describe` path, no cross-window string coupling
   required. Deleting the tripwire test alongside the code it protected
   (rather than leaving it green-but-pointless) kept the test suite honest
   about what it actually still guards.
5. **Discovery prose that "narrates the mechanism" needed the same
   discipline the mechanism itself did.** The provenance strings
   (`"walking to water"` → `"went down to the river it knew"`, `"seeking
   water"` → `"wandered, having found no water yet"`) are read by `why`
   through the historiography window and asserted on by three separate test
   files. Changing them consistently — source, unit tests, and an
   integration test's doc comments, in one pass — avoided leaving stale
   substring assertions in a state where they'd pass today by coincidence
   and break on an unrelated future change.

## Follow-ups

Carried in the worktree's `.superpowers/sdd/followups.md`, promoted here for
visibility (also see [the-freshet.md](the-freshet.md)'s own list, which named
follow-up 6 — "the Surmise rewiring" — as exactly this task):

1. **Smarter, info-gain-directed exploration** (decision-ledger followup 6,
   the campaign's own reserved next step for the greedy walker). This is
   the fix for the T5 finding: a walker that never revisits within a call
   is not a global search and can wall itself into a resource-less local
   basin even when a resource exists elsewhere in the same connected
   region. A frontier-based or backtracking search (or a coarser
   exploration LOD, or settlement placement biased toward water access)
   would close it; none was attempted here, deliberately, since diagnosing
   and fixing an exploration-policy defect is a different-shaped task from
   re-pointing a resource referent.
2. **Staleness — cache invalidation** (the false-positive polarity). This
   slice's truth is static; giving water truth-volatility (depletion,
   seasonal sources) and having belief lag it is the reserved next
   divergence.
3. **The L2 credulity threshold — deception** (UNI-16, reserved as **The
   Discernment**, the next campaign). One truth-to-belief channel exists
   today (the agent's own perception); a second, untrusted channel (a
   lying NPC) needs a signal-detection threshold, the immune model's
   actual mechanic.
4. **Belief made visible to the possessed player** — a `know`/`recall` verb
   surfacing the *player's own* believed-nearest-water, riding the possess
   loop and `why`.
5. **A rich believed-map, re-anchored to the moving agent** — this slice's
   belief anchors to home (a single stable scalar); nearest-to-current-
   position, and a full per-agent sparse overlay of remembered geography,
   are richer representations reserved as follow-ups.
6. **Probabilistic / noisy perception** — perception here is a binary
   spatial filter (occupy the room, learn its water-truth); a
   confidence-weighted, distance/time-decaying version is the substrate
   the immune threshold (follow-up 3) will eventually weight.

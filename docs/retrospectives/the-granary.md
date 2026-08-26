# The Granary — Retrospective

Campaign: fine-resolution history, layer 1 (seasonality). Branch
`campaign/the-granary`, spec 2026-08-23, executed 2026-08-23/24.

## Process lessons

1. **Subagent dispatch vs. the 600 s window.** Long cargo builds killed four
   implementation subagents (120 s no-output kill, 600 s ceiling). What
   worked, in order of preference: warm the build cache FIRST via a detached
   `nohup` build; split implement/verify into separate dispatches so the
   verifier inherits a finished diff; and for the last mile, run verification
   in the orchestrator's own shell with background+poll. The orchestrator
   writing one mechanical token (a `&usize` deref) after three failed
   dispatches was Nathan-approved and correct — but the *clippy "fix"* that
   followed it introduced a real indexing bug (`row[idx]` for
   `shares[idx][phase]`) that only the test suite caught. Mechanical-looking
   edits are not exempt from the tests.

2. **The harness kill taught a durable shape:** authoring scripts that must
   survive the agent that launched them (`nohup ... &`) let a 13-minute sweep
   outlive two dead subagents. Anything >5 min should be designed to be
   launched, not awaited.

3. **--no-verify has a sanctioned shape.** The hook header permits it "when
   you have a reason"; this campaign used it twice, both times for the same
   structural chicken-and-egg: registry changes invalidate committed fixture
   evidence whose authoring script refuses dirty trees. Both uses carried the
   reason and the follow-up in the commit message. That pattern (commit →
   re-author → follow-up commit) should probably be written down as the
   standard route rather than rediscovered.

4. **Pilot fixtures are a decision, not a shortcut.** Local gnomon
   re-authoring unblocked the gate but suspends H1's adjudication until the
   canonical re-run. The cost is invisible in a green suite — the ledger and
   the close-out checklist are the only places it lives. Cross-session work
   (the WorldTime→seconds migration) similarly left a dependency only a doc
   comment carries. **Open question for a future PROC row:** should there be
   a mechanical inventory of "suspended-adjudication" state?

## Product findings

- Raids cluster hard on the annual cycle (concentration 0.914) but NOT on
  the hunger side (0.5066 ≈ chance) — opportunity-timing, not
  desperation-timing. The preregistered hypothesis was half right, and the
  wrong half is the interesting one.
- The founder-handle discrimination tail is provably irreducible by finer
  timestamps: twin foundings are same-phase by construction. 2261/3000
  collide without the tail. Question closed by measurement.
- H1's injection battery read a fifth time at 70/120 = 0.5833 — five
  readings, all within one SE of the bar, scattered on both sides. The
  underpower diagnosis is now about as corroborated as an underpowered
  instrument can be.

## Follow-ups

- Layer 2: inter-annual weather memory → cross-year raid flurries (new
  draws; `history/bake/v3` consumption change). Registered as its own idea.
- Layer 3: full event-level resolution.
- Close-out (lefford): census refresh + canonical `gnomon-injection.sh`
  re-run, same landing; resolves the one deliberately-carried red.
- WorldTime→seconds migration (other session): when it lands, revisit
  harvest-curve phase convention (`year_phase_offset == 0` assumption) and
  retire the float-key sentinels this domain still carries.

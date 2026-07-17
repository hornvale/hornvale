# The Terminator — retrospective

Process lessons, not product. One page.

## What worked

- **The ideonomy pass caught a framing error before any code moved.** The
  opening question read as a design fork — "accept the latitude-only field
  as a deliberate approximation, or spend a campaign correcting it" — and a
  monotonic-vs-peaked probe against the actual physics reframed it in one
  pass: a locked world's insolation genuinely peaks at the substellar point
  and falls to zero at the antistellar one, so a latitude-only field isn't a
  coarser model of the same shape, it's the wrong shape entirely. That
  reframe turned a taste call into a bug report, and everything downstream
  (spec, plan, execution) inherited the correct scope on the first pass
  instead of relitigating it mid-campaign.
- **Probe before tuning paid off exactly as designed.** The decision to
  measure where dominant-species habitat actually peaks under the corrected
  field, before touching a single roster value, is banked-on-demand
  discipline (decision 0057) applied to a fidelity fix rather than a
  generation mechanism — and it worked the same way: the probe found the
  roster's authored preferences already terminator-seeking by numeric
  accident, so a whole conditional task (the roster re-tune) was correctly
  never activated. Measuring first didn't just save the work; it produced a
  positive finding (the accident of scale) that guessing first would have
  missed entirely.
- **The acceptance battery caught the falsification before it shipped as a
  claim.** The campaign's spec preregistered a specific, named payoff — tide
  religion recovering on locked worlds — and the battery measured that exact
  claim against real generated worlds rather than accepting the Stage-0
  probe's habitability proxy as good enough. It came back negative, and the
  negative result is what makes the discipline real: a campaign that only
  ever measures the thing it expects to confirm isn't measuring anything.
  Finding the falsification here, at the acceptance gate, is strictly better
  than finding it after a chronicle had already claimed the payoff shipped.

## What the layered bug names

The campaign's real process lesson is structural, not procedural: fixing
one gate on a payoff can fully reveal, rather than close, the gate behind
it. The habitability fix was correct and complete on its own terms, and the
acceptance battery's diagnosis showed the presiding-belief mechanism it
never touches was the actual reason the payoff still didn't land — a
mechanism nobody had reason to suspect until the first gate stopped being
the bottleneck. This is the same "reconcile rung has no infrastructure"
shape seen elsewhere in this project: a system can be locally correct at
every layer a spec inspects and still produce the wrong aggregate outcome,
because the layers compose in an order nobody modeled explicitly. The
actionable version for future specs: when a payoff has multiple plausible
gates (here: does the physics reward the right place, and does belief
selection track what's rewarded), name and probe *all* of them before
committing to a single-campaign scope, rather than discovering the second
gate only once the first is shipped.

## What to carry forward

- **Stale brief cruft in shared scratch struck twice in one campaign.**
  Tasks 3 and 5 both found `.superpowers/sdd/task-N-brief.md` files left
  over from unrelated, already-shipped campaigns, misdirecting effort until
  cross-checked against the actual plan file. Both were caught before any
  wrong work landed, but only because each task's own report flagged the
  mismatch explicitly rather than trusting the file. This is the same
  worktree-scratch hazard named after an earlier campaign lost real time to
  it — recurring enough now that scratch cleanup between campaigns, not
  just careful reading within one, deserves its own fix.

## Deferred at close (Nathan's call)

- **The census regen was explicitly skipped** ("it's coming soon") — the
  campaign merges to local main with the standard census staleness accepted
  per the project's tolerated-lag policy, no push until the pending regen
  greens main.
- **The presiding-belief dominance-blindness the acceptance battery
  surfaced was banked as its own campaign, not folded in.** Extending this
  campaign to also fix belief selection was a live option; Nathan's ruling
  kept campaigns small and shipped the habitability fix as the correct,
  complete-on-its-own-terms improvement it is.

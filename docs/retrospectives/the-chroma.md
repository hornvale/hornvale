# The Chroma — retrospective

**In flight** (merge pending). Process lessons only. The result is in
[the chronicle](../../book/src/chronicle/the-chroma.md).

## The per-task brief verification caught a plan defect before it cost a task

The plan asserted `ChartCell` already mirrored `color` on the wire. The
dispatch skill's step 1 — grep every named identifier, one task ahead,
against the tree the implementer will find — found the field did not
exist. The fix (add the mirror, licensed by schema.rs's own module doc)
folded into Task 4's dispatch as a controller correction and cost minutes.
The Stylus paid a whole task (its Task 0) for the same class of defect
caught later.

## A green gate assumption was wrong at exactly one call site

The hermetic `NO_COLOR` test helpers carried SAFETY comments claiming
nextest's process-per-test isolation. True under nextest; false under
`make game-check`, which runs plain threaded `cargo test` — where a
chart test setting the env var raced plan tests reading it, and the full
gate failed intermittently on a suite that had passed twice. Fixed with a
shared `ENV_LOCK` across every env-touching or ink-asserting test. The
general lesson restates an old one: **a safety argument that names the
runner must be true of every runner that exists**, and `game-check` vs
`nextest` are different runners with different isolation models. The fix's
own review then caught the *same class* missed in the bin integration
binary — cross-task seams are where runner assumptions rot.

## Two subagent timeouts, two clean recoveries

Two of seven dispatches hit the 600 s tool timeout mid-task. Both resumed
cleanly by preserving the partial diff to scratch and dispatching a
successor instructed to CONTINUE (review `git diff`, keep good parts,
complete) rather than restart — the second predecessor turned out to have
been essentially correct and complete. The recovery recipe (preserve, then
resume-not-restart) held both times; killing and restarting fresh would
have re-paid reasoning already done.

## Environment constraint: no sonnet tier available

The dispatch policy bans haiku and sets a sonnet floor. This environment
has no Anthropic/Bedrock key; subagents ran on the controller's own model
(openrouter ox-alpha), which satisfies the floor's intent trivially.
Ledgered (#8) rather than silently deviated.

## A registry write truncated the file, and the local gate caught it in
## one run

Compacting the new registry row with a Python one-liner evaluated
`open(path,'w')` before its own argument's read, emptying 1,867 lines.
`docs_consistency` went red on the very next local run (the waiver list
named IDs the registry no longer carried) and the branch never left the
machine. Decision 0125's fear — "no CI, a red main is invisible" — has a
working inverse: gates run locally before submission see everything. The
specific lesson is Python's evaluation order in
`open(p,'w').write(open(p).read())`; the general one is that the DoD gates
are load-bearing even mid-campaign, not just at close.

## Deferred and parked

- **CLIENT-unlit-is-uncoloured** — the campaign's product finding;
  registered, deliberately unfixed (Nathan's call). Producer-side
  light-model campaign.
- **Vision sketch disposition** — kept as diagnostic; delete or promote to
  a reference-lens idea row at some future close if it goes unused.
- **PROC candidate**: subagents editing by relative path can land edits in
  the main checkout when harness cwd ≠ worktree — two near-misses this
  campaign, both self-reverted by the agents. The preamble's cd-first rule
  bound their *shell*, not their edit tool. An absolute-paths line in the
  preamble may be cheaper than the recoveries.

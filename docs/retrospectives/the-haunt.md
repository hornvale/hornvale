# The Haunt — retrospective

One page, process not product. Built remembered danger (PSY-11's reserved instance): a
`believed_hazard` fold (the inverted twin of `believed_water`) read as a finite planner
route-cost, so a creature's plans bend around ground it was frightened at. The Danger
flow drive is unchanged; memory lives at the planning layer.

## What worked

- **Implementation-grounding overturned a converged ideonomy conclusion — and that is
  the system working, not failing.** Two ideonomy passes converged (0 overturns) on a
  "simplest kernel": a one-hop additive term in the Danger drive. Reading the actual
  planner code before writing the spec showed that kernel was a near-*no-op* — a
  statically-dangerous cell is already sensed at one hop, so remembering it at the same
  horizon changes nothing. The real dual of `believed_water` (which drives *planning*)
  is a planner route-cost. The lesson generalizes past this campaign: **"simplest" is
  meaningless until checked against what the existing machinery already does.** An
  ideonomy pass reasons about the idea; only the code reveals whether the idea's
  simplest form reduces to a no-op. Ground the G1 decision against the seam it will
  touch *before* the spec, not during execution.

- **The implementer stopped on a falsified premise instead of forcing it green.** The
  spec promised seed-42 byte-identity ("the settled peoples are never frightened →
  empty set"). The implementer found the premise false — the possession walk's wild
  fauna *do* get frightened — isolated it rigorously (forcing the set empty restored
  byte-identity exactly, proving the code clean), and returned BLOCKED with the options
  rather than editing a baseline to pass. That is exactly the behaviour the "drift =
  BLOCKED, do not paper over" instruction is for, and it turned a silent
  determinism-baseline change into an explicit owner decision.

## What the campaign taught

- **The additive-latent byte-identity pattern has a boundary, and this campaign found
  it.** Four prior campaigns (Quarry/Wilding/Teeth/Alarm) shipped byte-identical because
  their new signal happened to be *empty* for every existing agent on seed 42. I
  assumed The Haunt would too — reasoning only about the settled peoples and forgetting
  the fauna The Wilding had made into agents. But remembered danger is the **first
  drive-layer signal that is genuinely not dormant for existing agents**: the wild
  beasts roam dangerous ground and remember it. The pattern's guarantee holds only when
  the *whole* population's source set is empty — and "the peoples are calm" is not the
  same claim as "the population is calm" once the fauna walk. The check that catches
  this is not a reviewer's memory but the drift diff; regenerate the artifacts and read
  what actually moved.

- **A busy main is now the dominant close cost.** The merge absorbed six campaigns
  across three fetch-rounds (The Tidings, PROC-19, The Eremite, The Solitary Tongue,
  then The Deep), and `windows/vessel/src/liveness.rs` collided with The Tidings'
  belief-sharing — six union hunks plus a hidden planner-signature mismatch (my new
  `avoid` arg). The reconciliation held because the conflicts were all "both campaigns
  added complementary things" (keep-both unions), and the full re-gate on the composed
  result (1922 tests green, zero unexpected drift) is what proved the compose was
  clean, not the clean textual merge. Diff the full `branch..origin/main` range, not the
  tip commit — an artifact-regen tip can mask a whole code campaign behind it (the
  lesson carried from The Alarm's close, and load-bearing again here).

## A follow-up worth promoting

The behaviour/felt-state split recurs: the planner carries the memory, but a creature
standing on remembered-bad ground *feels* nothing (the Danger drive is present-only).
That is correct for v1's static terrain (the cell is still dangerous, so it is sensed),
but the moment transient-danger memory lands — remembering where the alarm or a moving
predator frightened it — felt dread at a now-safe remembered cell becomes the phobia,
and the staleness rule becomes load-bearing. Recorded on PSY-11 as the next rung. The
general shape, now seen twice (The Alarm, The Haunt): **when a new signal reaches only
one of {mover, felt-state}, decide the other deliberately and record the gap.**

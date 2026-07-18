# Retrospective — The Individuation (ECS Campaign 5: instance ⋈ ledger)

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## What went well

- **A well-specified spec again turned a campaign into a small number of
  genuine decisions.** Nine crux items (#71–#80) settled the spelling of
  `instance-of`'s temporality, the lens's no-cache posture, the four kinds'
  component shapes, and the `NonZeroU64` sequencing before a single task
  started; execution was six clean, independently-reviewable strangler-fig
  stages with no re-litigation.
- **A measured blast radius de-risked the mechanical task.** `NonZeroU64`
  touched 40 `EntityId(` sites across 14 files (measured before, not during,
  T1) and landed byte-identical on the first pass — the same "measure the
  call surface before choosing the API" discipline that made c4's index
  integration a zero-churn decision.
- **The shadow-posture discipline held for real, not just on paper.** Every
  task through T5 produced registry-only drift (new predicate/component
  rows), verified against the merge base at every task boundary; no almanac,
  gallery, or census artifact moved a byte across the whole campaign. The
  pinned zero-instances genesis assertion (T2) meant this was never a hope,
  it was a test.

## What the reviews caught that the tasks did not

- **The JOIN≡SCAN generator gap — the c4 signed-zero lesson, recurring.**
  T3's per-task review found the property battery's "valid kind" label pool
  (`owlbear`, `woolly-mammoth`, `giant-elk`) drew three kinds that all carry
  `potency: 0.0` in the biosphere registry, so the property never exercised
  potency-default resolution after a kind change: a bug that resolved the
  *wrong* kind's potency would compute `0.0` under both the lens and the
  naive oracle for every generated entity and never redden the battery.
  Fixed by swapping one zero-potency label (`giant-elk`) for a mighty one
  (`treant`, potency 0.6) — a one-line pool change, not a rewrite. **Lesson,
  restated because it recurred within one program:** a property test titled
  "X equals Y over random inputs" is only as strong as its generator's
  coverage of the value space that could make X and Y *disagree*, and that
  coverage does not audit itself — a generator that type-checks and compiles
  can still silently avoid the one input class where the bug lives. This is
  now the second time in two consecutive campaigns; it belongs on a
  checklist a per-task review runs deliberately, not on hoping the next
  reviewer remembers the last campaign's postmortem.
- **A convention question resolved wrong in the brief, caught before it
  shipped.** T4's dispatch brief suggested per-field `type-audit:` tags on
  the three new component structs' members. The task found the established
  convention is struct/function-level doc tags, not per-field, and followed
  the convention instead of the brief. Noted here because it is the kind of
  small brief error that costs nothing when a task is trusted to check
  against the actual codebase pattern rather than execute the brief
  literally.

## Process miss to carry forward

- **Scratch is lost the moment its worktree goes away — plan for it before
  the fact, not after.** The pre-campaign decision ledger and drill
  documents for this program died with the `coexistence-stack` worktree's
  teardown; only the entries that had already been promoted into a spec
  survived (ledger #79). This campaign's own decision ledger opens by
  naming that loss as the reason the spec-promoted record, not the scratch
  ledger, is treated as the durable one. **Lesson:** before removing any
  campaign worktree, promote its `.superpowers/sdd/` scratch into a durable
  location (the spec, the decision log, or an explicit archive commit) —
  never assume a worktree that is "done" is safe to delete without that
  step, even when its deliverable already merged. This is now the second
  campaign to state the lesson; the fix is mechanical enough that it belongs
  in the `closing-a-campaign` skill's checklist, not just in prose.
- **The freeze-fixture.sh plan bug (named in the c4 retro) recurred
  verbatim in this campaign's own plan.** Both T2's and this task's briefs
  named `bash scripts/freeze-fixture.sh` (bare) as a local freshness step;
  it takes required arguments and is scoped to *historical* pins only
  (`world-seed-42.json` is explicitly excluded in its own header comment).
  T2 caught it, documented the correct command
  (`REBASELINE=1 cargo test -p hornvale --test lens_purity`), and flagged it
  forward to T3 and T6 — which is exactly why it cost nothing here. Worth
  noting that the correction did not, on its own, stop the same wrong text
  from being copied into T3's and T6's briefs again; a plan-authored line
  survives even after being named wrong once, unless the plan document
  itself is edited, not just the running commentary around it.

## Followups (register, promoted verbatim)

1. **Genesis instantiation of deity / culture / material instances** — the
   first campaign with a consumer that renders instances mints them at
   genesis; that campaign owns the artifact-drift + census-regen call
   (Nathan's authorization; #72). Until then instances exist only behind
   the mint API (tests / lab / possess seam).
2. **Per-tick lens cache** — c6 (the tick is the invalidation boundary;
   `FactIndex`'s absent-or-complete `#[serde(skip)]` lifecycle is the model;
   #74).
3. **Kind-transition guards** — c6's capability schema: a system declaring
   `writes instance-of` can carry transition-guard predicates; c5 leaves
   the machine open (#73).
4. **Lab study / census metric over instances** — when instances reach
   shipped worlds (#78).
5. **Structured-trait overrides** — c5 overrides numeric scalars (mass,
   potency) only; niche/structured overrides need a Value-envelope story
   (out of scope, §2 of the spec).
6. **`kind_history` named read** — an entity's kind state-machine trace is
   readable today via `facts_about` + filter; add the named kernel read if
   a consumer (explain/almanac) wants it.

## Confidence Gradient

No open-questions bet was re-scored. Grepped `book/src/open-questions.md` for
ledger/query/ECS/component/kind/instance terms; the only substantive hits are
*The Menagerie*'s (campaign 1) resource-niche and per-axis-field debt, not
this campaign's mechanism. Same shape as the c4 close: this campaign is
substrate *beneath* the generation/taste bets — it gives the ledger a way to
say what an entity individually is, in shadow, with zero worlds changed — not
a resolution of a taste-gated bet. A substrate milestone tracked by the
UNI-22 registry row is not, by itself, a movement in a generation/taste bet.

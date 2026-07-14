# The Datum — retrospective

**Completed:** 2026-07-13 (name-only designation per decision 0026; plan
`docs/superpowers/plans/2026-07-12-the-datum.md`; first half of the Kernel
Units pair, with `docs/retrospectives/temperature.md`)

**Reviews that execute beat reviews that read.** The plan's own verbatim
sample code for Task 1 — written with care, reviewed at planning time —
failed the default-deny type audit on first contact: it carried a stale
`bare-ok(constructor-edge: return)` qualifier on a `Result` return that has
no tracked primitive, and the plan simultaneously *required* the audit to
pass four tasks later. Nobody caught the self-contradiction by reading; the
task reviewer caught it by *running* `type-audit -- check` against the
committed code. The correction was adopted as a standing constraint for the
rest of the campaign (any task touching audit tags runs the check inside its
own gate), and the general lesson is worth stating flatly: a review that
executes the relevant tool catches a class of error that no amount of
careful reading catches, because the error was already present in the
carefully-read source.

**A plan can sequence a workspace invariant into silent redness.** Task 1's
module doc cited the `shared-units-live-in-the-kernel` decision; the
decision *record* was scheduled as Task 6. In between, the workspace-level
`decision_cites_in_sources_resolve` check was red for five consecutive
tasks — and nobody saw it, because each task's gate was deliberately
crate-scoped for speed and the full workspace gate only ran at Task 5. The
fix at execution time was to run Task 6 before Task 5's commit; the fix at
planning time is the durable one: **when an early task creates a reference
that a later task's artifact satisfies, either reorder the tasks or note
explicitly which workspace check is expected red in the interval.** A
crate-scoped task gate is a speed optimization that silently narrows what
"green" means; the plan has to carry that awareness, because the executor
demonstrably won't re-derive it.

**Plans should carry the operational form of an expensive step, not its
abstract name.** The byte-identity proof step said "run the canonical
regeneration"; the controller read that literally and launched the full
artifact regeneration — including the censuses, which are hours-scale and
had already been designated remote-only territory — burning 48 minutes
before the correction landed mid-flight (`SKIP_CENSUS=1`, diff `book/`
only). The policy existed; the plan text predated it and named the abstract
action instead of the exact command. The scrub that followed (a guard in
`regenerate-artifacts.sh` itself, plus every doc that said "regenerate
locally") is the right shape: encode the operational form at the point of
use, in the script and in the plan step, because a mid-campaign executor
reads the step it is on, not the policy corpus around it.

**Origin-first sequencing is a real trade, chosen consciously.** Converting
the datum origin (terrain) first meant every downstream crate failed to
compile until its own conversion task — so four of the campaign's commits
do not build standalone, and a future bisect across this range will need
`--first-parent` care. What was bought: at no commit did a half-typed
boundary exist, and the compiler enumerated every conversion site instead
of a grep hoping to. For a type migration whose entire risk is a silently
mixed boundary, that is the right side of the trade — but it should be a
per-campaign decision, not a default; a migration whose risk profile is
different (behavioral change, not representation change) should weigh
per-task compile checkpoints more heavily.

## Estimate vs reality

Six tasks; one fix loop (the stale audit tag, Task 1), one blocked commit
(Task 5 on Task 6's decision record), one mid-flight redirection (the
census regeneration). Byte-identity held on the first real measurement and
at every subsequent check — the migration mechanics themselves (the
conversion-rules table, compiler-driven enumeration) produced zero rework
across the four ripple crates. The audit ledger closed exactly as planned:
sixteen datum tags retired, `pending` 162 → 129, `waiver` 14 → 5, with
`crust-km-convention` untouched for its own family's turn.

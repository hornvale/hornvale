# 0598. Per-turn work is a counted budget in the commit gate

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Campaign:** The Rack

In the context of a per-turn cost that had grown roughly twenty-one-fold
without a single gate going red, facing the choice between tightening the
wall-clock ceiling that missed it and measuring something else entirely, we
decided that **per-turn work is asserted as a COUNT of operations in the
commit gate, and the wall clock is demoted to a stage- and heavy-tier
instrument that measures the box**, accepting that a count cannot see a
regression that performs the same operations more slowly.

## Context

`cli/tests/suite/session_cost.rs` was the only per-turn performance gate this
project had. Its own doc records that 20 of the 50 pooled samples its median is
drawn from exceed the ceiling individually while the gate passes. The Roll then
multiplied a turn's work by roughly ten — sixty-seven present bodies re-folded
on every `snapshot()` — and it stayed green.

**The reason is sharper than "a pooled median is blunt", and it is the argument
this decision rests on.** The test's absolute millisecond assertions are gated
by `BASIS_HOST = "aarch64-10"` (the Mac), and since decision 0133 the heavy
tier runs **only** on lefford (`x86_64-40`). So on every run that actually
happens, `bases_apply` is false and the millisecond ceilings never fire at all.
Measured on 2026-09-02, on a quiet lefford in the dev profile, both readings
taken by the same test on the same box:

| reading | main `0dccce029` (Roll + Pawl) | `campaign/the-rack` `813c74726` | pre-Roll basis |
| --- | ---: | ---: | ---: |
| `Session::start` | 1752.8 ms | — | 3442.2 |
| pooled turn `handle+snapshot+json` | **81.490 ms** | **17.706–17.959 ms** | 3.906 |
| indoor `snapshot()+json` | **78.416 ms** | **21.384–21.607 ms** | 18.720 |

**The test passed at main's tip with a 9 ms ceiling and an 81 ms reading.** A
ceiling that a nine-fold overshoot walks through is not a slack gate; it is a
gate that is not running. A wall-clock ceiling in the commit gate is no
alternative either — it flaps on a contended box, which is what put this one on
the heavy tier in the first place (The Repose).

## The decision

1. **`TurnWork` (`windows/vessel/src/turn_work.rs`) counts a turn's work**:
   drive folds, plan searches, ledger position folds, shadowcasts derived, and
   bodies scanned. It is reset at the top of `Session::handle` and read after
   it through `TurnWorkRead`.

2. **`windows/vessel/tests/suite/turn_budget.rs` asserts those counts per verb
   class** at seed 42's flagship and in a chamber: a `snapshot()` performs 0
   drive folds, 0 position folds and 0 plan searches; a chamber `go` folds
   nothing; a turn derives at most one shadowcast; a `wait` folds at most the
   roll's length. **These are counts, so they are identical on every box and
   cannot flap** — the test belongs in the commit gate rather than the heavy
   tier. (`HomeNavCache::searches` is the shape this borrows.)

3. **Every counter keeps at least one live writer, and this is load-bearing.**
   Once the turn stopped folding, `affect_folds` had no bump site left and the
   compiler said so — a counter nothing bumps is a zero that cannot fail, and
   the test would have read green while asserting nothing. `affect_folds` is
   now bumped by `seed_felts` (the append's one surviving stateless read) and
   `position_folds` by `position_of` (the last `agent_position` call on any
   session path, which is the SCAN half of decision 0597's VIEW ≡ SCAN).

4. **The wall clock stays, demoted and honest.** `session_cost.rs` measures the
   box, not the code; its module doc points at the counted instrument, and its
   ceilings are re-pinned by the campaign that measures them.

## Consequences

- **A regression of the shape The Roll introduced now reddens on the commit
  that introduces it**, at the moment a fold is added, on any machine, instead
  of being invisible until a campaign goes looking. The counted tests went red
  on the pre-change tree by exactly the preregistered numbers (67 drive folds,
  137 position folds per snapshot) and green after — the positive control was
  measured before the change, not asserted after it.
- **What a count cannot see**, stated so nobody reads more into it: the same
  operations performed more slowly, allocation, serialization, and everything
  outside a counted call. The Rack's own residue is exactly that shape —
  ~70 KB of snapshot JSON and one ~8 ms chamber shadowcast — and no counter
  bounds it.
- **`TURN_BASIS_MS` and `TURN_BUDGET_MS` are raised, as a reviewed act**:
  3.906 → 17.959 (the slowest of three quiet lefford runs) and 9.0 → 36.0
  (the file's customary ~2× margin), carrying the table above and its
  attribution in the constant's own doc. The ratchet in that file says a raise
  must be explicit and reviewed; this is that. **The asymmetry between the two
  instruments is the point**: a wall-clock ceiling is re-pinned after the fact
  by whichever campaign notices, while the counted budget went red the moment a
  fold was added and cannot be re-pinned upward without changing what it
  counts.
- **The vacuity itself is not closed here.** A Mac-keyed basis on a
  lefford-only tier is the pre-existing follow-up
  `TOOL-session-cost-has-no-canonical-basis`, whose **Where** cell now carries
  this table. Closing it means recalibrating the whole constant set together
  (the precedent is `scene_cost.rs`'s `BASIS_HOST = "x86_64-40"`), which is a
  campaign, not a task.
- A new vessel test is not in `gate-commit` until the next green chamber job
  rewrites `docs/timings/subfloor-roster.tsv`; `turn_budget.rs` runs in the
  stage gate from its first commit, and its module doc says so rather than
  claiming a gate it does not yet have.

## See also

Spec §3.5 and §4 P2 (`docs/superpowers/specs/2026-09-02-the-rack-design.md`);
decision 0133 (the heavy tier runs on lefford, which is what made the ceiling
vacuous); decision 0093 (`claim:`-shaped ignore reasons);
the Penstock metaplan §7 ("deterministic budgets are drift-checked and
gate-able"); `windows/vessel/src/turn_work.rs`;
`windows/vessel/tests/suite/turn_budget.rs`; `cli/tests/suite/session_cost.rs`;
`docs/superpowers/ledgers/2026-09-02-the-rack.md` entry #4 and the control-run
entry; `book/src/chronicle/the-rack.md`.

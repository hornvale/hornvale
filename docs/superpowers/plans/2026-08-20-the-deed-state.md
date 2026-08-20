# The Deed — campaign state

*Committed on purpose. This campaign opened by discovering that its
predecessor's handoff had evaporated: it lived in `.superpowers/sdd/`, which
is git-ignored and per-worktree, and `worktree-take.sh` recycled the worktree
out from under it. A handoff between arcs of one program has no other
committed home, so this is it. Update it at each controller handoff; never
move it into scratch.*

**Branch:** `campaign/the-deed` · **Spec:** `2026-08-20-the-deed-design.md` ·
**Plan:** `2026-08-20-the-deed.md`

---

## Status: Tasks 1-5 complete, 6-8 remain

| Task | Ships | State |
|---|---|---|
| 1 | `GapReason::Extradiegetic` + the draw filter | complete, review clean |
| 2 | 14 concepts, accession cohort 11, exposure classification | complete, clean after 1 fix round |
| 3 | `Mood` as a property of the action | complete, clean after 1 fix round |
| 4 | The body-state gate table + roster tripwire | complete, clean after 1 fix round |
| 5 | The `!` namespace; group A's bare forms retired | complete, clean after 1 fix round |
| **6** | **Group B's out-of-character halves** | **next** |
| 7 | In-character acts charge time and post facts | not started — the acceptance test |
| 8 | Close the campaign | not started |

Main absorbed at `c45d3120`. `gate-commit` green throughout.

**The invariant that has held at every step:** no day-stamp has moved in any
committed artifact. Time-charging belongs to Task 7 alone, so a moved
timestamp before then means something is charging time that should not.

---

## What Tasks 6-8 need to know

**Task 6 — a warning inherited from Task 5's review.** `session.rs`'s
dispatch was re-indented one level into an `else` block, which is most of
Task 5's `+343`. Task 6 adds group B's arms to that same block: **extract a
named handler rather than widen the re-indent.** `git blame` on the densely
commented dispatch arms already points at Task 5 in a 5,800-line file.

**Task 6 — the discriminator test is the point.** Its brief requires an
`assert_ne!` between the bare and sigil forms, with an explicit instruction to
STOP if no discriminating fixture can be built. An out-of-character form that
is observationally identical to its in-character twin is a no-op, and shipping
it would be worse than not shipping it.

**Task 7 — the sleep verb.** The acceptance test ("the player sleeps, IC
refuses, OOC works") cannot pass without a verb that puts the body to sleep,
and none of the 26 existed. Nathan approved adding one, routed to the existing
`Action::Rest` machinery. Both `sleep` and `rest` are already registered
concepts — mint neither, and do not repoint `Action::Rest`'s `concept_name`,
which would drift committed artifacts.

**Task 7 — this is where the galleries move.** Its drift table is the one
place where "STOP" versus "commit it" is a judgement against a diff rather
than a rule. Read it, do not accept it.

**Task 8 — two book-freshness items already found**, so they cannot be
rediscovered late:

- `book/src/chronicle/the-quickening.md:150` still calls bare `npcs` a live
  verb. That is now a **false statement in a published chapter**.
- `cli/src/repl.rs:50,277` keep bare `help`/`why` on the **world REPL** — a
  different surface from `Session::handle`, outside spec §3.2's 26 verbs, so
  not a defect. But the two surfaces now spell the same word differently, and
  the retrospective should say so.

**Task 8 — Arc I.c is named The Waybill**, not The Tally. The Tally merged to
main mid-session, hours after the name was verified free.

---

## Open items

- **`clients/game/bin` has no gate at all.** Task 5 found a production key
  mapping there (`?` → `"help"`) that the rename would have broken silently.
  Tasks 6-7 touch the same dispatch and could break it the same way, with
  nothing to catch it.
- **The composite universe rule is hand-tracked in two places.**
  `!is_unnameable(..) && !is_extradiegetic(..)` appears in `cli/src/proto.rs`
  and a worldgen golden test. `is_extradiegetic` was hoisted; `is_unnameable`
  cannot be — it is `&World`-keyed and cannot move below `cli`. A third
  exclusion added to `proto_root_universe`'s filter still needs manual
  addition at both sites, silently. Documented bug class, weaker form.
- **Eight pre-existing malformed `type-audit: bare-ok(return)` tags** (on
  `Action`, and seven in `liveness.rs`). All inert; each detonates the day its
  item gains a tracked primitive. Worth its own look: a default-deny audit
  that silently accepts a malformed tag on an unaudited item.
- **Registry rows to mint at close** (against the merged tree, never reserved
  in advance): the CLI hint path for retired bare forms; The Tackle's
  origin-of-intent taxonomy and durations/interruption candidates.

---

## The pattern worth carrying into Tasks 6-8

Every substantive defect on this branch originated in **plan text or dispatch
prompts**, not in implementer code. Seven instances: a location test standing
in for a semantic one; "all three sites" when there were four; a `type-audit`
tag that does not parse; a cited symbol that does not exist; "17 call sites"
when there were 46, including production code outside the workspace; a test
that could not fail; and a remedy that would have left the hole it closed.

Zero substantive defects were introduced by an implementer and caught by a
reviewer.

What caught them was always something **executable or independent** — a
compiler, a mutation, a re-derived grep, or a reader who did not write the
claim. Two practices did most of the work and should continue:

1. **Verify the brief against the code immediately before dispatching** — one
   task ahead, never in a batch. It caught the missing test helper, the 17-vs-46
   count, and the `packs.rs` trap that would have given every culture a word for
   `!why` with nothing going red.
2. **Tell implementers to re-derive lists rather than trust them**, including
   lists in the very text making the claim. That instruction is why the
   46-site radius and the fourth match site were found at all.

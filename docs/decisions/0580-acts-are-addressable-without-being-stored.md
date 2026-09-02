# 0580. Acts are addressable without being stored — a derived act view, session-home wired

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot; the
reification call itself was Nathan's, at brainstorm — see below) · **Campaign:** The Avowal

## Context

`witnessed`, `present-at`, `deed-of`, `act-precedes` and `act-occurred-on` all
presuppose that an act has identity, and none existed: `grep -n EventId`
returns nothing in the tree, and `occ-cause`/`occ-ended-by` are `Text` labels,
not references (spec §3.1). The controller costed reifying acts as if
addressability implied storage — one committed `Fact` per act — and
recommended against it on that basis: `bundle:witnessing` and half of
`bundle:act-chronology` would stay permanently blocked.

**That costing was wrong, and Nathan corrected it at brainstorm (ledger
entry #4), citing a decision-log fact the controller had not checked**:
0366 makes passage state "a pure function of the seed and the committed
ledger, and only the CHANGE is written"; 0346 makes an affordance derived and
never committed; 0368 makes live-play facts persist only when a snapshot
(`--out`) asks. Storage was never the cost an act's identity owed. This is
the campaign's first genuine ideonomy overturn — the controller's own
recommendation reversed on a fact already in force.

Decision 0576 built `Provision`; decisions 0578 (Task 5) and 0579 (Task 6)
wired the ledger and component homes. The session home (`Home::Session`)
carried the uninhabited `Unwired` placeholder — unreachable by construction —
until this task.

## The decision

1. **An act gets a derived identity, `ActHandle` — a pure function of its
   own constituents (`actor`, `deed`, `patient`, `day`) — and is NEVER
   committed to the ledger.** `windows/vessel/src/act.rs` (new file; the
   session home's implementation lives beside the capability it serves, the
   same placement `windows/sentiment` has for the component home). Same
   shape as the two precedents Nathan named: `hornvale_history::flesh::
   RoleHandle` (identity without materialization — "a record can reference
   many unnamed roles without ever materializing them until something
   actually observes one") and `windows/worldgen::character::barrier_of`
   (state without storage — recomputed on every read, never cached).

2. **The hazard `hornvale_history::descent::ancestor`'s own doc records —
   "a fixed permutation iterated has fixed points, and `(RoleHandle(0),
   Seed(0))` was one" — is avoided structurally** (`ActHandle::of` folds
   four *different* constituents through separate steps, never the same
   step over itself) **and was still caught empirically, exactly as that
   precedent warns a designer must check.** An early draft folded the
   patient-presence tag directly against a raw `EntityId` (`mix(1,
   entity.get())`); `mix`'s first step is a bare XOR, so `mix(x, x) == 0`
   for *any* `x`, and `EntityId::new(1)` — the smallest legal id — collided
   with the tag `1` on the first property test run, not on inspection. The
   fix folds each presence tag against the already-avalanched accumulator
   instead of a raw id (`PATIENT_SOME_TAG`/`PATIENT_NONE_TAG` in `act.rs`),
   and the degenerate all-zero-shaped case (`EntityId(1)`, an empty deed
   string, no patient, genesis) is now its own named test,
   `act_degenerate_all_zero_shaped_case_still_distinguishes_a_neighbour`.

3. **`predicate:witnessed`, `predicate:present-at`, `predicate:deed-of`,
   `predicate:act-precedes` and `predicate:act-occurred-on` are all declared
   `Present(Home::Session(session_act_view_holds))`** — one shared resolver
   (`cli/src/provision.rs`), the same way both affect tokens share
   `sentiment_affect_holds`. `Home::Session` now carries `SessionResolver`
   (`fn() -> bool`, exactly [`ComponentResolver`]'s shape) instead of the
   uninhabited `Unwired`: neither resolver ever has a live world or session
   to read, since `Provision::build`/`serves` are asked with a bare
   `&ConceptRegistry` alone, so "session state" names WHERE the capability
   would live if a caller ever committed it, not an object the resolver
   reads. `predicate:history-now` — the `act-chronology` bundle's fourth
   token — is **not** declared here: it is already a registered predicate
   the ledger home serves (`hornvale_history::HISTORY_NOW`, committed by the
   deep-history bake), not a derived session read, and stays on the ledger
   home (`history_now_resolves_through_the_ledger_home_not_the_session_home`
   pins this).

4. **What the derived act view can and cannot read, stated honestly rather
   than worked around.** `Session` exposes exactly three things publicly
   an act view needs: `Session::day` (`act_occurred_on`'s constituent),
   `Session::agent_entity` (`deed_of`'s constituent for the possessed
   body's own acts), and `Session::purview` (the walk-band chart, whose
   cells carry `hornvale_scene::Mark`s of kind `"agent"`). `anyone_present`
   reads that last one, but a mark carries no entity id — the chart can say
   *whether* someone is present, never *who*. `present_at`/`witnessed`
   therefore take an explicit `&[EntityId]` pool a caller with entity-level
   knowledge supplies; widening the chart to carry entity ids is future
   work this task does not attempt. `windows/vessel/src/session.rs` was not
   edited — five other live campaigns were mid-flight in that file at the
   time of this task, and the constraint against a sixth concurrent editor
   was honoured by building `act.rs` entirely against `Session`'s existing
   public surface.

5. **Nothing is committed generating many acts, proven with a positive
   control.** `windows/vessel/tests/suite/act.rs::generating_many_acts_
   commits_nothing_and_the_ledger_comparison_can_detect_a_real_commit`
   derives 400 acts and every read this task ships off one live session,
   asserts the session's serialized ledger and fact count are byte-identical
   before and after, and only THEN performs a real in-character walk (the
   same committing action `player_acts_commit.rs` pins) to prove the
   comparison is not vacuously equal by construction. The same test then
   calls `into_played_world` and confirms the walk's ordinary `agent-at`
   fact survives — decision 0368's "persist only when a snapshot asks"
   mechanism still carries a real commit through; no act-derived read in
   this task ever produces one for it to carry.

6. **`bundle:witnessing` reads 2/2 tokens; the null this task predicted
   held.** Both `predicate:present-at` and `predicate:witnessed` now
   resolve, so `predicate:witnessed`/`predicate:present-at` vanish from
   every situation's `missing` list that named them (both frozen corpora,
   e.g. polti-06-disaster's list shrinks from 6 tokens to 4). No situation
   in either corpus requires `bundle:witnessing` alone — every one that
   named it also named at least one still-missing bundle — so none crosses
   into a bare `Blocked(["witness:absent"])` reading; that reason is pinned
   only by this task's own synthetic-corpus test
   (`bundle_witnessing_reads_two_of_two_but_stays_blocked_on_absent_witness`).
   Stageable counts: `polti-1895` 0 of 36, `tvtropes-2012` 0 of 409 —
   unchanged from spec §5's preregistered prediction. Token completion is
   necessary, not sufficient, exactly as the witness (decision 0577) exists
   to prove.

## Consequences

- `cli/tests/fixtures/world-seed-42.json` and every other byte-golden fixture
  are unmoved — this task commits no fact under any name, so no ledger-shaped
  artifact drifts.
- `docs/audits/trope-coverage-polti-1895.md`, `docs/audits/trope-coverage-
  tvtropes-2012.md` and `docs/audits/trope-matrix.md` regenerate:
  `predicate:present-at`/`predicate:witnessed` drop out of every `missing`
  list that named them, shortening those situations' `Blocked` reasons —
  no situation's outcome changes from `Blocked` to `Stageable`, and no
  situation in either corpus reaches a bare `witness:absent` reading (see
  point 6 above).
- `docs/audits/type-audit-report.md` regenerates: `ActHandle`'s tuple field,
  `Act::deed`, and every new bare `bool`/`flag` return in `windows/vessel::act`
  and `cli::provision` carry `type-audit:` tags.
- `docs/digest/decisions-in-force.md` regenerates to list `0580` as in force.

## See also

Spec §4.5 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
`docs/superpowers/ledgers/2026-09-01-the-avowal.md` ledger entry #4 (the
overturn) and entry #5's item 4 (the reification precedent —
occupation-as-entity); `windows/vessel/src/act.rs`;
`domains/history/src/flesh.rs::RoleHandle`;
`domains/history/src/descent.rs::ancestor`;
`windows/worldgen/src/character.rs::barrier_of`; decisions 0346, 0366, 0368,
0576, 0577, 0578, 0579.

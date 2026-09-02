# 0583. The witness limits list is open, not closed

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Campaign:** The Avowal · **Supersedes:** 0582

## Context

Every record in this sub-chain has closed its own disclosure of the
witness gate's limits with a claim of completeness, and every one of those
claims has been falsified by the next review round:

- Decision 0577 (round 1's original gate) said a relation-less witness
  "binds vacuously to any situation whose requirements name no predicate
  token" — false; `witness_binds`'s `all()` was one-directional and bound
  to ANY situation regardless of what it required (0581/0582's finding).
- Decision 0582 (the bidirectional fix) closed with "actant-role assignment
  ... is now the ONLY disclosed limit." A third review proved this false
  with a live probe on the real corpora:

  ```
  situation requires [predicate:instance-of, phenomenon:eclipse]
  witness stages cast [goblin, drow] + instance-of(0,1), no phenomenon of any kind
  -> outcome: Some(Stageable)
  ```

  `witness_binds` is scoped to `predicate:` tokens — correctly, since
  nothing in `Tableau` can state a `concept:`/`phenomenon:` requirement —
  but 0582 documented that scoping as an implementation note two paragraphs
  before contradicting it with "the ONLY disclosed limit." Both frozen
  corpora genuinely require such tokens: `concept:child/die/god/parent/
  person/sibling/spirit`, `phenomenon:eclipse/heliacal-rising/night-star/
  wandering-star`, and `phenomenon:cold/heat` in `polti-1895`. A reader of
  the committed report who saw a `phenomenon:eclipse` requirement resolve
  `Stageable` would have concluded the eclipse was witnessed. It was not.

**The pattern is now the finding, not any one instance of it.** "The limits
include X and Y" is falsified by discovering a third limit Z; "X is the
only limit" is falsified by ANY Z, discovered or not. Three attempts at the
second phrasing have each been found false by the next reader who looked
harder. A fourth enumeration invites a fourth falsification.

## The decision

**Stop asserting the limits list is exhaustive. Name known limits as an
explicitly open list, never a closed one.** `witness_binds`'s own doc, the
shared report constant `WITNESS_BOUNDARY_WHAT`, and this record all now
list what is KNOWN not to be checked without ever punctuating that list as
complete — no "the one limit that remains," no "the ONLY disclosed limit,"
no phrasing that would need a fourth review round to falsify.

**Three limits are named today, and the list may grow without contradicting
this record, because this record makes no claim about the list's size:**

1. **Actant ROLE assignment is unverified.** `Situation::actants` is
   prose-valued (a Greimas role name mapped to a free-text description), so
   there is no mechanical role check available the way there is for a
   predicate token. This is the limit spec §4.2 itself states for the whole
   witness bar ("does not prove any world produces the situation").
2. **A `concept:`/`phenomenon:` requirement is never realized by the
   witness at all.** `witness_binds`'s set-equality check is scoped to
   `predicate:` tokens because nothing in `hornvale_vessel::Tableau`
   represents a concept or a phenomenon — a `StagedRelation` states only a
   predicate between two cast members. A situation requiring
   `phenomenon:eclipse` passes its token check (decision 0576: the ledger
   home serves the token) and is never asked to stage an eclipse by
   anything downstream. This is the limit this record exists to name.
3. **The bar is name-level, not aptness-level.** `PredicateDef` is `{ name,
   functional, doc }` with no object-type constraint (spec §4.2's own
   opening line, restated here because it is also a witness limit, not
   only a token-check one): a tableau relating two goblins by `latitude`
   stages and counts as realizing `predicate:latitude`. This is
   `Provision`'s own pre-existing limit (decision 0576), inherited by the
   witness rather than introduced by it — named here so a reader does not
   have to rediscover it a fourth time.

**Requiring the full token set (including `concept:`/`phenomenon:`) to
match the staged-relation set was considered and rejected**, the same as
in 0582: it would make every situation requiring either kind permanently
unwitnessable, which is a worse failure mode than an honestly-disclosed
gap. Closing limit 2 mechanically would need `Tableau` to gain a way to
state a concept or a phenomenon being true of the staged scene — out of
this task's scope, and not requested.

## Consequences

- No code behavior changes. `witness_binds`'s implementation (decision
  0582's set-equality check) is untouched; only its own doc, the shared
  report constant, and this decision chain's prose change.
- `docs/decisions/0582-the-witness-binding-is-bidirectional.md` is restored
  to its original text (per this project's append-only convention — see
  0577's and 0581's own restorations) with only its **Status** line marked
  `Superseded by 0583`. Its false closing clause remains visible in that
  restored text as the historical record of what was claimed and when.
- The report artifacts (`docs/audits/trope-coverage-*.md`,
  `docs/audits/trope-matrix.md`) move as PROSE only — the open-list
  rewording of `WITNESS_BOUNDARY_WHAT` — never a verdict, confirmed by
  regenerating and diffing: every table row is byte-identical.
- A future reader who finds a fourth limit adds it to the list. Nothing in
  this record needs to change for that to be true, because this record
  never claimed the list was finished.

## See also

Spec §4.2 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
decision 0582 (superseded by this record); decision 0581; decision 0577
(the original gate, and the `Provision` name-level limit this record
restates); decision 0576 (the provision table, source of limit 3);
decision 0330 (the sibling precedent on `sentences/`);
`docs/superpowers/ledgers/2026-09-01-the-avowal.md` entries #8-#11;
`cli/src/tropes.rs`; `cli/tests/suite/trope_witness.rs`.

# 0584. Kinship direction and the `parent-of` generation cut

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Campaign:** The Avowal · **Supersedes:** 0578

## Context

Decision 0578 shipped `parent-of`/`kin-of` committed `(descendant, predicate,
forebear)` — subject the founder being resolved, object their forebear —
with `parent-of` firing for every `Kinship::Ancestor(n)`, any `n`, and both
predicates declared `functional: true`. Review round 1 found two defects,
both load-bearing:

**C1 — `Ancestor(n)` collapsed to `parent-of` for every `n`.** Measured on
seed 42 under 0578's shipped code: `Ancestor(1)` = 32 edges, `Ancestor(2)` =
17, `Ancestor(3..9)` = 22, `Ancestor(11..37)` = 13. **52 of 84 `parent-of`
facts — 61.9% — were not parent-child**, the deepest 37 generations removed.
The registered lexical concept `parent` means "one's father or mother";
committing a 37-generations-removed forebear as `parent-of` contradicted the
world's own vocabulary, inside the keystone golden, in every world this
project will ever generate.

**I4 — the direction was backwards against registry naming rule 4.** Rule 4
reads a verb/preposition predicate strictly left-to-right from the subject.
Under `(descendant, parent-of, forebear)` the committed sentence asserts
*the descendant is the parent of their own ancestor* — false whenever the
remove is nonzero. The golden showed the shape plainly: 84 facts, 84
distinct subjects, only 69 distinct objects (one object named by 3 different
subjects) — the fan-in that belongs on the PARENT side, sitting on the wrong
column.

## The decision

**Both predicates now commit `(forebear, predicate, descendant)` — the
forebear is the subject.** This is what makes the sentence true under rule
4: "the forebear is the parent of [or: is kin of] the descendant."
`functional` follows the direction rather than the other way round: **both
are now `functional: false`.** A forebear may found more than one daughter
community (seed 42 has one with three, confirmed structurally reachable and
pinned by `windows/worldgen/tests/suite/kinship_facts.rs::
a_forebear_with_more_than_one_descendant_carries_more_than_one_fact_without_contradiction`),
so the subject side is not single-valued; it is the DESCENDANT side
(`records[i].founded_from`, at most one recorded forebear per occupation)
that is structurally single-valued, and that fact now lives in the OBJECT
position, where `functional` does not apply.

**`parent-of` fires ONLY for `Kinship::Ancestor(1)`.** Every other
classification — `Sibling`, and `Ancestor(n)` for any `n != 1` — commits
`kin-of` instead. `kin-of` is therefore not "the non-parent-child edge from
0578's original two-way split" but "everything that is not exactly one
generation removed": it is true at any remove, because kinship (unlike
parenthood) is not generation-scoped.

**`kin-of`'s direction is a convention, not a truth requirement, and is
disclosed as such.** Unlike `parent-of`, `kin-of` reads true in EITHER
direction — "the forebear is kin of the descendant" and "the descendant is
kin of the forebear" are both true at any remove, since kinship is
symmetric. It keeps `parent-of`'s forebear-as-subject direction so the two
predicates share one implementation, not because either direction is
required for truth. **This has a real, disclosed cost**: `kin-of` is
committed asymmetrically, so it is queryable from the forebear's end only —
a descendant cannot look up their own more-distant kin through this
predicate without walking every forebear's facts and checking objects for
their own `EntityId`. No second, reverse-direction fact is committed to
close this; the cost is accepted, not hidden.

**`place`/`day` are the descendant's, disclosed (review round 2).** Every
committed fact is `place`d at the daughter community and `day`-stamped at
its founding — the OBJECT's community and founding day, not the
forebear-subject's. That was the subject's own community before the
direction reversed above; the code did not change, only which end of the
edge is now the subject. Defensible (the fact becomes observable the moment
the daughter is founded), but undocumented until now — see
`domains/person/src/lib.rs`'s `PARENT_OF` doc for the full note.

## Consequences

- Relative to pre-campaign `main` (commit `93ef987e9`), the change remains
  **strictly additive**: 93 facts added, 0 removed, seed 42's total fact
  count unchanged at the +93 spec §5 preregistered. The SPLIT changed:
  0578 shipped 84 `parent-of` / 9 `kin-of`; this decision measures **32
  `parent-of` / 61 `kin-of`** on seed 42 (not predicted in advance — measured
  after implementation, per the review's explicit instruction not to treat
  an illustrative estimate as a target).
- `bundle:consanguineal-kin` is unaffected and still reads 5/5: bundle
  satisfaction is registry-membership-only (`cli/src/provision.rs::
  Provision::from_registry`), and both predicate NAMES were already
  registered under 0578 — only their direction and `functional` value
  changed. `polti-1895` stageable holds at 0 of 36 and `tvtropes-2012`
  stageable holds at 0 of 409, unchanged, exactly as spec §5 preregistered.
- `kernel/src/ledger.rs` gains
  `functional_contradiction_is_rejected_for_an_entity_object` and a new
  local test predicate (`belongs-to`) — the functional-contradiction guard
  had never been forced on a `Value::Entity` object before (both prior cases
  used `Value::Text`), a gap the review found independent of C1/I4.
- `windows/worldgen/tests/suite/kinship_facts.rs`'s determinism test
  (`kinship_pass_is_deterministic_across_two_independent_builds`, formerly
  `kinship_resolution_draws_no_stream`) is renamed to state only what it
  proves — two live builds of the same code are equal, which holds
  regardless of whether a stream draw was added, as the review demonstrated
  by inserting one and watching every assertion still pass. The real
  "no stream draw" evidence is a new test,
  `person_facts_are_unperturbed_relative_to_the_pre_task_baseline`, which
  compares every `is-person`-scoped fact against an independent snapshot
  frozen at commit `93ef987e9` (`windows/worldgen/tests/fixtures/
  pre-kinship-person-facts-seed-42.json`) rather than against another run of
  the current code.

## Correction owed, recorded here rather than only in the campaign ledger

The review dispatch that led to 0578's original `functional: true` claim
asserted `parent-of` "would be the workspace's first `functional: true`
relation." That was false: `pays-tribute-to`, `person-founded` and
`occ-founded-from` are all `functional: true` with `Value::Entity` objects
and all predate this campaign — 0578 itself cites `pays-tribute-to` twice on
the same page as a relation, which is what made the claim self-contradicting
on its own evidence. The genuine gap was narrower (no test forced a
`Contradiction` on an `Entity` object specifically) and is closed above.

## See also

Spec §4.3 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
decision 0578 (superseded by this record);
`docs/superpowers/ledgers/2026-09-01-the-avowal.md` entries #12 (corrected)
and #13 (review round 1); `domains/person/src/lib.rs`; `windows/worldgen/src/
person_promote.rs`; `windows/worldgen/tests/suite/kinship_facts.rs`;
`kernel/src/ledger.rs`.

# 0582. The witness binding is bidirectional

**Status:** Superseded by [0583](0583-the-witness-limits-list-is-open-not-closed.md) (2026-09-02, same review cycle — a third review round proved this record's own closing clause false: it claimed actant-role assignment was "the ONLY disclosed limit," and a `phenomenon:eclipse` requirement realized by nothing was never named) · **Decider:** Nathan · **Campaign:** The Avowal · **Supersedes:** 0581

## Context

Decision 0581 closed 0577's bare id lookup with `witness_binds`: every
predicate a witness's tableau states as a relation had to be among the
tokens the situation's own `requires` list named. That check is
`tableau.relations.iter().all(|rel| required.contains(...))`, and `all`
over an EMPTY iterator is `true` — a review probe built
`Tableau::new().with_cast(["goblin","drow"])`, two creatures and **zero**
relations, filed it under a situation requiring **five** predicate tokens
it never touches, and it resolved `Stageable`; `witness_stages` returned
`Ok(())`. The only thing 0581's design ever refused on the staging path was
an empty CAST, not empty relations.

**The disclosure 0581 shipped named a strictly smaller hole than the one
that existed.** `WITNESS_BOUNDARY_WHAT` and 0581's own text said a
relation-less witness binds vacuously "to any situation whose requirements
name no predicate token" — a qualifier that reads as confining the gap to
situations with no predicate requirements at all. `witness_binds` is
`all()` over the tableau's relations, and `all()` on an empty iterator is
`true` **regardless of what `required` contains** — the qualifier was
false, and the hole was universal, not confined.

**The empty case is the degenerate member of a family, not a special
case.** `witness_binds` asked that the tableau state nothing extraneous; it
never asked that it state anything. A tableau relating by 1 of a
situation's 10 required predicates bound exactly as well as one relating
by all 10 — a partial-coverage witness was never distinguished from a
complete one, and 0581 disclosed neither the universality of the empty case
nor the existence of the partial-coverage family at all.

## The decision

`witness_binds` now requires SET EQUALITY between two collections of
`predicate:`-namespaced tokens, not a one-directional subset check: the
situation's own required `predicate:` tokens, and the predicates the
witness's tableau states as relations.

```rust
fn witness_binds(corpus: &Corpus, situation: &Situation, tableau: &Tableau) -> bool {
    let required_predicates: BTreeSet<String> = situation
        .requires
        .iter()
        .flat_map(|r| expand(corpus, r))
        .filter(|t| t.starts_with("predicate:"))
        .collect();
    let staged_predicates: BTreeSet<String> = tableau
        .relations
        .iter()
        .map(|rel| format!("predicate:{}", rel.predicate))
        .collect();
    required_predicates == staged_predicates
}
```

This closes the empty-relations case and the partial-coverage family in one
move: every predicate the tableau relates by must be one the situation
requires (0581's original direction), AND every `predicate:` token the
situation requires must be realized by at least one staged relation (the
direction 0581 never checked). A witness with no relations at all can now
bind only to a situation that requires no `predicate:` token — the
qualifier 0581 stated, made true by the code rather than left false beside
it.

**Scoped to `predicate:` tokens deliberately.** A `concept:` or
`phenomenon:` requirement cannot be stated by a `StagedRelation` at all —
nothing in `hornvale_vessel::Tableau` represents either — so requiring the
FULL required-token set (including those) to equal the staged-relation set
would make every situation requiring a `concept:`/`phenomenon:` token
permanently unwitnessable, which is not what this check is for and not
what spec §4.2 asks.

**What this still does not prove, restated because it is the one limit
that survives this fix.** It does not verify that the tableau's cast fills
the situation's actant ROLES: `Situation::actants` is prose-valued (a
Greimas role name mapped to a free-text description), so there is no
mechanical role check available the way there is for a predicate token.
Role assignment stays entirely unverified — exactly the limit spec §4.2
itself states for the whole witness bar ("does not prove any world produces
the situation"), and now the ONLY disclosed limit, since the binding gap
0581 left open is closed.

**The report prose is corrected alongside, not merely the code.**
`WITNESS_BOUNDARY_WHAT` (the constant `render` and `render_matrix` share)
now states the bidirectional claim exactly, and its final clause states
only the actant-role limit — the false "binds to any situation whose
requirements name no predicate token" qualifier is gone, because it is no
longer a qualifier: it is now a proven property of the check.
`WITNESS_BOUNDARY_COMPARABILITY` was also reworded (from "This number" to
"A count taken before...") since it is shared between a single-corpus
report (one count) and the matrix (one count per column, several at once) —
"This number" read as singular prose pasted under a multi-column table.

**Two further corrections landed in the same pass, closing the same class
of defect this decision itself exists to fix.**

- `render`'s Leverage section computed `closest` (the fewest unheld bundles
  among token-blocked situations) with `.min().unwrap_or(0)`. If EVERY
  blocked situation turned out to be witness-blocked, that iterator is
  empty and `unwrap_or(0)` reprints "the closest blocked situation is still
  missing 0 bundles" — the exact corrupted sentence 0581 fixed for the
  general case, reopened in a narrower window. `closest` is now kept as
  `Option<usize>`, with the `None` case handled by a distinct sentence
  rather than collapsed to a number that reads as a false claim.
- The Leverage section's `blocked` count (token-blocked situations, the
  ranking's actual denominator) and the Demand table's total silently
  disagreed once witness-blocked situations existed to be excluded, with no
  reconciling sentence — the same disclosure gap 0581 already closed for
  `inapplicable` situations, left open for this new exclusion. The
  paragraph now states `witness_blocked` explicitly and reconciles all four
  counts (`stageable + inapplicable + blocked + witness_blocked =
  out.len()`) the way the `inapplicable` disclosure always did.
- `blocked_by_witness`'s doc claimed "no real corpus token can ever
  collide" with the `witness:` sentinel prefix because `expand` "only ever
  produces tokens namespaced `predicate:`/`phenomenon:`/`concept:`" — false:
  `expand`'s `None` arm returns any non-`bundle:` requirement UNCHANGED,
  validating no namespace. True of the two frozen corpora as authored
  today (verified by inspection), not a guarantee the code provides for a
  future one. Corrected to say so — the same class of defect as the false
  `Provision` claim decision 0577 shipped and 0581 fixed.
- `describe_witness_reason`'s catch-all silently mapped any unrecognized
  sentinel to `"the registered witness failed to stage"`. A `&str` match
  cannot be exhaustive in the sense the compiler checks, so the fallback
  now panics instead of guessing — a fourth sentinel added later without a
  matching arm here fails loudly the first time it is exercised, rather
  than rendering a plausible but wrong description forever.

## Consequences

- `witness_binds`'s signature is unchanged; only its body strengthened.
  Every existing test in `cli/tests/suite/trope_witness.rs` and `cli/src/
  tropes.rs`'s own `mod tests` still passes unmodified under the new check
  — each already staged either zero relations against zero required
  predicates, or exactly the required predicates, both of which satisfy
  set equality the same way they satisfied the old one-directional check.
- Two new regression tests reproduce the review's exact probe
  (`a_relation_less_tableau_does_not_bind_to_a_situation_requiring_
  predicates`) and the partial-coverage family it named
  (`a_tableau_covering_only_some_required_predicates_is_unbound`).
- Migration cost remains zero — `tropes::witnesses()` is still empty, and
  wiring this correction moved no verdict on either frozen corpus, verified
  by regenerating both reports and the matrix and diffing: only the header
  prose and the Leverage paragraph's wording changed; every table row
  (Demand and fan-in alike) is byte-identical.

## See also

Spec §4.2 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
decision 0581 (superseded by this record); decision 0577 (the original
witness gate); decision 0330 (the sibling precedent on `sentences/`);
`docs/superpowers/ledgers/2026-09-01-the-avowal.md` entries #8, #9 and #10;
`cli/src/tropes.rs`; `cli/tests/suite/trope_witness.rs`.

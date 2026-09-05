# Task 2 report — independent checker

DONE: implemented and committed the finite checker, 32 hand-derived tests, and
independent derivation/disclosure document.

## Identity and scope

Working directory: `/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker`
Branch: `codex/counterpart-checker`
Commit: `26c693ce027bbfb8c937e4f36f024949a6b97589`
Checker SHA256: `ee14515c833ddb43397b25ad9b035dcc18d07c748d7f1a1f3effa6b9f8902c90`
Unchanged frozen contract SHA256: `37ea8fa06f086000f94cf215a391b5bcbd74bef92b9502bdc02ebb0a9ad32b34`

Only checker.py, test_checker.py, and checker-derivation.md are committed.
The supplied contract remains untracked; docs/timings.md is the controller's
prewarm row, left untouched and unstaged. The controller confirmed debug
prewarm readiness before the ordinary commit. No census or full-workspace
intermediate test was run. No hook bypass was used. The hook selected its
normal prose-subject path and passed 75 tests (246 skipped).

## Derivation and interpretation

Full disclosure and predicate derivation are in
`tools/digest/experiments/the-counterpart/checker-derivation.md`.
Read shared task/contract documents and production ownership docs/APIs. The
Thing symbol search also exposed matching production test comments/assertions,
explicitly disclosed. Did not read Charter contributor/verdict implementation,
whole campaign plan, other authors' owner records/specimens, or candidate
answers. Did not choose, design, or reveal a reserved challenge.

All four questions execute on every valid input. Counter/list grouping retains
multiplicity. Membership checks cover both component directions; ownership
requires each source name's independently expected owner, forbids extra
Thing-owned names, and preserves every lender concept, including unborrowed
ones. Settlement refusal makes lender judgments unknown unless declarations
already prove a violation; Thing refusal alone leaves borrowing judgable.
Either refusal makes ownership unknown. Malformed facts/contracts always raise
ValueError before any judgment. Duplicate JSON keys require raw parsing and
remain the runner's responsibility. Contract byte identity is a runner check;
the checker validates schema/object shapes/known IDs and basic metadata types.

## RED/GREEN and corrections

Tests were written before implementation. An empty dictionary scaffold caused
54 assertion failures and 23 KeyErrors; those missing-result errors were not
claimed as behavioral proof. A complete always-satisfied scaffold then produced
74 assertion failures, zero errors, across 32 methods. This is the behavioral
RED run. Implementation passed all 32 tests on the first complete run.

An isolated actual mutation removed `or set(observed) != expected` from the
ownership comparison. Both mutated checker and test fixture compiled with
py_compile. The literal required wrong-owner fixture then executed and failed
on satisfied versus violated, with no import/compile error. The original file
was never mutated and passed all 32 methods again afterward. Full receipts
follow, including the original incomplete scaffold failures.

## Self-review and concerns

Self-reviewed staged files and `git diff --cached --check` (exit 0), verified
branch/pwd immediately before committing, and read successful hook/commit
output. No unresolved implementation/test failure remains. Integration review
should retain the explicit independence disclosure and distinguish schema
validation here from frozen-identity/attempt checks in the runner. The ordinary
hook does not discover these Python tests automatically; controller should run
the documented targeted command as part of integration.

Test command:
`python3 -m unittest discover -s tools/digest/experiments/the-counterpart -p 'test_checker.py' -v`

## Original scaffold RED (not behavioral proof)

```text
test_ambiguous_lender_does_not_collapse_to_matching_owner (test_checker.CheckerTests.test_ambiguous_lender_does_not_collapse_to_matching_owner) ... ERROR
test_array_order_has_no_semantic_role (test_checker.CheckerTests.test_array_order_has_no_semantic_role) ... ERROR
test_borrowed_name_must_retain_its_declared_owner (test_checker.CheckerTests.test_borrowed_name_must_retain_its_declared_owner) ... ERROR
test_conflicting_borrowing_owners_cannot_be_overwritten (test_checker.CheckerTests.test_conflicting_borrowing_owners_cannot_be_overwritten) ... ERROR
test_duplicate_borrowing_violates_borrowing (test_checker.CheckerTests.test_duplicate_borrowing_violates_borrowing) ... ERROR
test_duplicate_component_violates_components (test_checker.CheckerTests.test_duplicate_component_violates_components) ... ERROR
test_duplicate_observed_owner_does_not_collapse_to_one (test_checker.CheckerTests.test_duplicate_observed_owner_does_not_collapse_to_one) ... ERROR
test_duplicate_source_violates_components (test_checker.CheckerTests.test_duplicate_source_violates_components) ... ERROR
test_extra_component_violates_components (test_checker.CheckerTests.test_extra_component_violates_components) ... ERROR
test_extra_thing_concept_violates_ownership (test_checker.CheckerTests.test_extra_thing_concept_violates_ownership) ... ERROR
test_inputs_are_not_changed (test_checker.CheckerTests.test_inputs_are_not_changed) ... ok
test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) ... 
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='schema', value='other') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='contributor', value='other') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='scope', value='other') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='questions', value={}) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='questions', value=[None]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='authorities', value=[]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='authorities', value=[4]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='id', value=[]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='question', value='') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='packages', value='wrong') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='inputs', value=[None]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='sources', value=[]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract=None) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract=[]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract={'schema': 'counterpart-v1', 'contributor': 'hornvale.thing', 'scope': 'domains/thing', 'questions': [{'id': 'registration', 'question': 'Does Settlement then Thing registration complete without refusal?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['hornvale_settlement::register_concepts', 'hornvale_thing::register_concepts', 'ConceptRegistry::concepts'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}, {'id': 'components', 'question': 'Do Thing source and component rosters agree in both directions, with no duplicates?', 'packages': ['hornvale-thing'], 'inputs': ['THING_KINDS', 'thing_registry'], 'sources': ['domains/thing/src/lib.rs']}, {'id': 'borrowing', 'question': 'Are borrowing declarations unique, members of the Thing roster, non-self, and supplied by the stated owner before Thing registration?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['BORROWED', 'ConceptRegistry::concepts (before Thing)'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}, {'id': 'ownership', 'question': 'Does the completed registry give every Thing name its stated owner, contain no extra Thing-owned names, and preserve lender owners?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['ConceptRegistry::concepts (before and after Thing)', 'THING_KINDS', 'BORROWED'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}], 'authorities': ['domains/CLAUDE.md', 'domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs', 'kernel/src/registry.rs'], 'unknown': True}) ... FAIL
test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) ... 
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value='key') ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value=[None]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value=['']) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='component_kinds', value={}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value={}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value=['hearth']) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value=[{'name': 'hearth'}]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='before_concepts', value=[{'name': 'hearth', 'owner': 7}]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='after_concepts', value=[{'name': 'key', 'owner': ''}]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='thing_registration', value={'outcome': 'unknown', 'detail': ''}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='thing_registration', value={'outcome': 'accepted'}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='settlement_registration', value={'outcome': 'accepted', 'detail': None}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='settlement_registration', value=None) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts=None) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts=[]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts={'source_kinds': ['hearth', 'key'], 'component_kinds': ['hearth', 'key'], 'borrowed': [{'name': 'hearth', 'owner': 'settlement'}], 'before_concepts': [{'name': 'hearth', 'owner': 'settlement'}], 'after_concepts': [{'name': 'hearth', 'owner': 'settlement'}, {'name': 'key', 'owner': 'thing'}], 'settlement_registration': {'outcome': 'accepted', 'detail': ''}, 'thing_registration': {'outcome': 'accepted', 'detail': ''}, 'candidate': {}}) ... FAIL
test_legitimate_borrowing_satisfies_all_four_questions (test_checker.CheckerTests.test_legitimate_borrowing_satisfies_all_four_questions) ... FAIL
test_missing_component_violates_components (test_checker.CheckerTests.test_missing_component_violates_components) ... ERROR
test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) ... 
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='schema') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='contributor') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='scope') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='questions') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='authorities') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='id') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='question') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='packages') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='inputs') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='sources') ... FAIL
test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) ... 
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='source_kinds') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='component_kinds') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='borrowed') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='before_concepts') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='after_concepts') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='settlement_registration') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='thing_registration') ... FAIL
test_missing_lender_violates_borrowing (test_checker.CheckerTests.test_missing_lender_violates_borrowing) ... ERROR
test_missing_thing_concept_violates_ownership (test_checker.CheckerTests.test_missing_thing_concept_violates_ownership) ... ERROR
test_refusal_does_not_hide_independent_declaration_violation (test_checker.CheckerTests.test_refusal_does_not_hide_independent_declaration_violation) ... ERROR
test_retained_unborrowed_lender_concept_is_allowed (test_checker.CheckerTests.test_retained_unborrowed_lender_concept_is_allowed) ... ERROR
test_self_borrowing_violates_borrowing (test_checker.CheckerTests.test_self_borrowing_violates_borrowing) ... ERROR
test_settlement_refusal_does_not_certify_partial_lender_state (test_checker.CheckerTests.test_settlement_refusal_does_not_certify_partial_lender_state) ... FAIL
test_thing_refusal_does_not_hide_missing_lender (test_checker.CheckerTests.test_thing_refusal_does_not_hide_missing_lender) ... ERROR
test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown (test_checker.CheckerTests.test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown) ... FAIL
test_unborrowed_lender_concept_must_not_change_owner (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_change_owner) ... ERROR
test_unborrowed_lender_concept_must_not_disappear (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_disappear) ... ERROR
test_undeclared_collision_violates_ownership (test_checker.CheckerTests.test_undeclared_collision_violates_ownership) ... ERROR
test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) ... 
  test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='unknown') ... FAIL
  test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='duplicate') ... FAIL
  test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='missing') ... FAIL
test_unused_borrowing_violates_borrowing (test_checker.CheckerTests.test_unused_borrowing_violates_borrowing) ... ERROR
test_wrong_lender_violates_borrowing (test_checker.CheckerTests.test_wrong_lender_violates_borrowing) ... ERROR
test_wrong_owner_without_borrowing_is_not_satisfied (test_checker.CheckerTests.test_wrong_owner_without_borrowing_is_not_satisfied) ... ERROR

======================================================================
ERROR: test_ambiguous_lender_does_not_collapse_to_matching_owner (test_checker.CheckerTests.test_ambiguous_lender_does_not_collapse_to_matching_owner)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 134, in test_ambiguous_lender_does_not_collapse_to_matching_owner
    self.assertEqual(self.outcome("borrowing"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'borrowing'

======================================================================
ERROR: test_array_order_has_no_semantic_role (test_checker.CheckerTests.test_array_order_has_no_semantic_role)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 166, in test_array_order_has_no_semantic_role
    self.assertEqual(self.outcome("ownership"), "satisfied")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_borrowed_name_must_retain_its_declared_owner (test_checker.CheckerTests.test_borrowed_name_must_retain_its_declared_owner)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 112, in test_borrowed_name_must_retain_its_declared_owner
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_conflicting_borrowing_owners_cannot_be_overwritten (test_checker.CheckerTests.test_conflicting_borrowing_owners_cannot_be_overwritten)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 74, in test_conflicting_borrowing_owners_cannot_be_overwritten
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_duplicate_borrowing_violates_borrowing (test_checker.CheckerTests.test_duplicate_borrowing_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 70, in test_duplicate_borrowing_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'borrowing'

======================================================================
ERROR: test_duplicate_component_violates_components (test_checker.CheckerTests.test_duplicate_component_violates_components)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 100, in test_duplicate_component_violates_components
    self.assertEqual(self.outcome("components"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'components'

======================================================================
ERROR: test_duplicate_observed_owner_does_not_collapse_to_one (test_checker.CheckerTests.test_duplicate_observed_owner_does_not_collapse_to_one)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 130, in test_duplicate_observed_owner_does_not_collapse_to_one
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_duplicate_source_violates_components (test_checker.CheckerTests.test_duplicate_source_violates_components)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 96, in test_duplicate_source_violates_components
    self.assertEqual(self.outcome("components"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'components'

======================================================================
ERROR: test_extra_component_violates_components (test_checker.CheckerTests.test_extra_component_violates_components)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 88, in test_extra_component_violates_components
    self.assertEqual(self.outcome("components"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'components'

======================================================================
ERROR: test_extra_thing_concept_violates_ownership (test_checker.CheckerTests.test_extra_thing_concept_violates_ownership)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 104, in test_extra_thing_concept_violates_ownership
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_missing_component_violates_components (test_checker.CheckerTests.test_missing_component_violates_components)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 92, in test_missing_component_violates_components
    self.assertEqual(self.outcome("components"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'components'

======================================================================
ERROR: test_missing_lender_violates_borrowing (test_checker.CheckerTests.test_missing_lender_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 62, in test_missing_lender_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'borrowing'

======================================================================
ERROR: test_missing_thing_concept_violates_ownership (test_checker.CheckerTests.test_missing_thing_concept_violates_ownership)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 108, in test_missing_thing_concept_violates_ownership
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_refusal_does_not_hide_independent_declaration_violation (test_checker.CheckerTests.test_refusal_does_not_hide_independent_declaration_violation)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 161, in test_refusal_does_not_hide_independent_declaration_violation
    self.assertEqual(self.outcome("borrowing"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'borrowing'

======================================================================
ERROR: test_retained_unborrowed_lender_concept_is_allowed (test_checker.CheckerTests.test_retained_unborrowed_lender_concept_is_allowed)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 126, in test_retained_unborrowed_lender_concept_is_allowed
    self.assertEqual(self.outcome("ownership"), "satisfied")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_self_borrowing_violates_borrowing (test_checker.CheckerTests.test_self_borrowing_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 84, in test_self_borrowing_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'borrowing'

======================================================================
ERROR: test_thing_refusal_does_not_hide_missing_lender (test_checker.CheckerTests.test_thing_refusal_does_not_hide_missing_lender)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 148, in test_thing_refusal_does_not_hide_missing_lender
    self.assertEqual(self.outcome("borrowing"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'borrowing'

======================================================================
ERROR: test_unborrowed_lender_concept_must_not_change_owner (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_change_owner)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 117, in test_unborrowed_lender_concept_must_not_change_owner
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_unborrowed_lender_concept_must_not_disappear (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_disappear)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 121, in test_unborrowed_lender_concept_must_not_disappear
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_undeclared_collision_violates_ownership (test_checker.CheckerTests.test_undeclared_collision_violates_ownership)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 58, in test_undeclared_collision_violates_ownership
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
ERROR: test_unused_borrowing_violates_borrowing (test_checker.CheckerTests.test_unused_borrowing_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 79, in test_unused_borrowing_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'borrowing'

======================================================================
ERROR: test_wrong_lender_violates_borrowing (test_checker.CheckerTests.test_wrong_lender_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 66, in test_wrong_lender_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'borrowing'

======================================================================
ERROR: test_wrong_owner_without_borrowing_is_not_satisfied (test_checker.CheckerTests.test_wrong_owner_without_borrowing_is_not_satisfied)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 54, in test_wrong_owner_without_borrowing_is_not_satisfied
    self.assertEqual(self.outcome("ownership"), "violated")
                     ~~~~~~~~~~~~^^^^^^^^^^^^^
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 32, in outcome
    return evaluate(self.facts, self.contract)[question]["outcome"]
           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^
KeyError: 'ownership'

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='schema', value='other')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='contributor', value='other')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='scope', value='other')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='questions', value={})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='questions', value=[None])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='authorities', value=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='authorities', value=[4])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='id', value=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='question', value='')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='packages', value='wrong')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='inputs', value=[None])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='sources', value=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract=None)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 252, in test_invalid_contract_shapes_raise_value_error
    with self.subTest(contract=contract), self.assertRaises(ValueError):
                                          ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 252, in test_invalid_contract_shapes_raise_value_error
    with self.subTest(contract=contract), self.assertRaises(ValueError):
                                          ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract={'schema': 'counterpart-v1', 'contributor': 'hornvale.thing', 'scope': 'domains/thing', 'questions': [{'id': 'registration', 'question': 'Does Settlement then Thing registration complete without refusal?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['hornvale_settlement::register_concepts', 'hornvale_thing::register_concepts', 'ConceptRegistry::concepts'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}, {'id': 'components', 'question': 'Do Thing source and component rosters agree in both directions, with no duplicates?', 'packages': ['hornvale-thing'], 'inputs': ['THING_KINDS', 'thing_registry'], 'sources': ['domains/thing/src/lib.rs']}, {'id': 'borrowing', 'question': 'Are borrowing declarations unique, members of the Thing roster, non-self, and supplied by the stated owner before Thing registration?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['BORROWED', 'ConceptRegistry::concepts (before Thing)'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}, {'id': 'ownership', 'question': 'Does the completed registry give every Thing name its stated owner, contain no extra Thing-owned names, and preserve lender owners?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['ConceptRegistry::concepts (before and after Thing)', 'THING_KINDS', 'BORROWED'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}], 'authorities': ['domains/CLAUDE.md', 'domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs', 'kernel/src/registry.rs'], 'unknown': True})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 252, in test_invalid_contract_shapes_raise_value_error
    with self.subTest(contract=contract), self.assertRaises(ValueError):
                                          ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value='key')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value=[None])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value=[''])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='component_kinds', value={})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value={})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value=['hearth'])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value=[{'name': 'hearth'}])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='before_concepts', value=[{'name': 'hearth', 'owner': 7}])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='after_concepts', value=[{'name': 'key', 'owner': ''}])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='thing_registration', value={'outcome': 'unknown', 'detail': ''})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='thing_registration', value={'outcome': 'accepted'})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='settlement_registration', value={'outcome': 'accepted', 'detail': None})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='settlement_registration', value=None)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts=None)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 203, in test_invalid_fact_shapes_raise_value_error
    with self.subTest(facts=facts), self.assertRaises(ValueError):
                                    ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 203, in test_invalid_fact_shapes_raise_value_error
    with self.subTest(facts=facts), self.assertRaises(ValueError):
                                    ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts={'source_kinds': ['hearth', 'key'], 'component_kinds': ['hearth', 'key'], 'borrowed': [{'name': 'hearth', 'owner': 'settlement'}], 'before_concepts': [{'name': 'hearth', 'owner': 'settlement'}], 'after_concepts': [{'name': 'hearth', 'owner': 'settlement'}, {'name': 'key', 'owner': 'thing'}], 'settlement_registration': {'outcome': 'accepted', 'detail': ''}, 'thing_registration': {'outcome': 'accepted', 'detail': ''}, 'candidate': {}})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 203, in test_invalid_fact_shapes_raise_value_error
    with self.subTest(facts=facts), self.assertRaises(ValueError):
                                    ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_legitimate_borrowing_satisfies_all_four_questions (test_checker.CheckerTests.test_legitimate_borrowing_satisfies_all_four_questions)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 36, in test_legitimate_borrowing_satisfies_all_four_questions
    self.assertEqual(
    ~~~~~~~~~~~~~~~~^
        {key: value["outcome"] for key, value in results.items()},
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        {"registration": "satisfied", "components": "satisfied",
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         "borrowing": "satisfied", "ownership": "satisfied"},
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    )
    ^
AssertionError: {} != {'registration': 'satisfied', 'components'[61 chars]ied'}
- {}
+ {'borrowing': 'satisfied',
+  'components': 'satisfied',
+  'ownership': 'satisfied',
+  'registration': 'satisfied'}

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='schema')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='contributor')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='scope')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='questions')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='authorities')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='id')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='question')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='packages')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='inputs')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='sources')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='source_kinds')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='component_kinds')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='borrowed')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='before_concepts')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='after_concepts')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='settlement_registration')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='thing_registration')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_settlement_refusal_does_not_certify_partial_lender_state (test_checker.CheckerTests.test_settlement_refusal_does_not_certify_partial_lender_state)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 152, in test_settlement_refusal_does_not_certify_partial_lender_state
    self.assertEqual(
    ~~~~~~~~~~~~~~~~^
        {key: row["outcome"] for key, row in evaluate(self.facts, self.contract).items()},
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        {"registration": "violated", "components": "satisfied",
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         "borrowing": "unknown", "ownership": "unknown"},
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    )
    ^
AssertionError: {} != {'registration': 'violated', 'components':[56 chars]own'}
- {}
+ {'borrowing': 'unknown',
+  'components': 'satisfied',
+  'ownership': 'unknown',
+  'registration': 'violated'}

======================================================================
FAIL: test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown (test_checker.CheckerTests.test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 139, in test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown
    self.assertEqual(
    ~~~~~~~~~~~~~~~~^
        {key: row["outcome"] for key, row in evaluate(self.facts, self.contract).items()},
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        {"registration": "violated", "components": "satisfied",
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         "borrowing": "satisfied", "ownership": "unknown"},
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    )
    ^
AssertionError: {} != {'registration': 'violated', 'components':[58 chars]own'}
- {}
+ {'borrowing': 'satisfied',
+  'components': 'satisfied',
+  'ownership': 'unknown',
+  'registration': 'violated'}

======================================================================
FAIL: test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='unknown')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 230, in test_unknown_duplicate_or_missing_question_ids_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='duplicate')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 230, in test_unknown_duplicate_or_missing_question_ids_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='missing')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 230, in test_unknown_duplicate_or_missing_question_ids_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

----------------------------------------------------------------------
Ran 32 tests in 0.024s

FAILED (failures=54, errors=23)
```

## Behavioral RED (exit 1)

```text
test_ambiguous_lender_does_not_collapse_to_matching_owner (test_checker.CheckerTests.test_ambiguous_lender_does_not_collapse_to_matching_owner) ... FAIL
test_array_order_has_no_semantic_role (test_checker.CheckerTests.test_array_order_has_no_semantic_role) ... ok
test_borrowed_name_must_retain_its_declared_owner (test_checker.CheckerTests.test_borrowed_name_must_retain_its_declared_owner) ... FAIL
test_conflicting_borrowing_owners_cannot_be_overwritten (test_checker.CheckerTests.test_conflicting_borrowing_owners_cannot_be_overwritten) ... FAIL
test_duplicate_borrowing_violates_borrowing (test_checker.CheckerTests.test_duplicate_borrowing_violates_borrowing) ... FAIL
test_duplicate_component_violates_components (test_checker.CheckerTests.test_duplicate_component_violates_components) ... FAIL
test_duplicate_observed_owner_does_not_collapse_to_one (test_checker.CheckerTests.test_duplicate_observed_owner_does_not_collapse_to_one) ... FAIL
test_duplicate_source_violates_components (test_checker.CheckerTests.test_duplicate_source_violates_components) ... FAIL
test_extra_component_violates_components (test_checker.CheckerTests.test_extra_component_violates_components) ... FAIL
test_extra_thing_concept_violates_ownership (test_checker.CheckerTests.test_extra_thing_concept_violates_ownership) ... FAIL
test_inputs_are_not_changed (test_checker.CheckerTests.test_inputs_are_not_changed) ... ok
test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) ... 
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='schema', value='other') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='contributor', value='other') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='scope', value='other') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='questions', value={}) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='questions', value=[None]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='authorities', value=[]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='authorities', value=[4]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='id', value=[]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='question', value='') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='packages', value='wrong') ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='inputs', value=[None]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='sources', value=[]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract=None) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract=[]) ... FAIL
  test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract={'schema': 'counterpart-v1', 'contributor': 'hornvale.thing', 'scope': 'domains/thing', 'questions': [{'id': 'registration', 'question': 'Does Settlement then Thing registration complete without refusal?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['hornvale_settlement::register_concepts', 'hornvale_thing::register_concepts', 'ConceptRegistry::concepts'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}, {'id': 'components', 'question': 'Do Thing source and component rosters agree in both directions, with no duplicates?', 'packages': ['hornvale-thing'], 'inputs': ['THING_KINDS', 'thing_registry'], 'sources': ['domains/thing/src/lib.rs']}, {'id': 'borrowing', 'question': 'Are borrowing declarations unique, members of the Thing roster, non-self, and supplied by the stated owner before Thing registration?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['BORROWED', 'ConceptRegistry::concepts (before Thing)'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}, {'id': 'ownership', 'question': 'Does the completed registry give every Thing name its stated owner, contain no extra Thing-owned names, and preserve lender owners?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['ConceptRegistry::concepts (before and after Thing)', 'THING_KINDS', 'BORROWED'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}], 'authorities': ['domains/CLAUDE.md', 'domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs', 'kernel/src/registry.rs'], 'unknown': True}) ... FAIL
test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) ... 
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value='key') ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value=[None]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value=['']) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='component_kinds', value={}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value={}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value=['hearth']) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value=[{'name': 'hearth'}]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='before_concepts', value=[{'name': 'hearth', 'owner': 7}]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='after_concepts', value=[{'name': 'key', 'owner': ''}]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='thing_registration', value={'outcome': 'unknown', 'detail': ''}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='thing_registration', value={'outcome': 'accepted'}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='settlement_registration', value={'outcome': 'accepted', 'detail': None}) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='settlement_registration', value=None) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts=None) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts=[]) ... FAIL
  test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts={'source_kinds': ['hearth', 'key'], 'component_kinds': ['hearth', 'key'], 'borrowed': [{'name': 'hearth', 'owner': 'settlement'}], 'before_concepts': [{'name': 'hearth', 'owner': 'settlement'}], 'after_concepts': [{'name': 'hearth', 'owner': 'settlement'}, {'name': 'key', 'owner': 'thing'}], 'settlement_registration': {'outcome': 'accepted', 'detail': ''}, 'thing_registration': {'outcome': 'accepted', 'detail': ''}, 'candidate': {}}) ... FAIL
test_legitimate_borrowing_satisfies_all_four_questions (test_checker.CheckerTests.test_legitimate_borrowing_satisfies_all_four_questions) ... ok
test_missing_component_violates_components (test_checker.CheckerTests.test_missing_component_violates_components) ... FAIL
test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) ... 
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='schema') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='contributor') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='scope') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='questions') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='authorities') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='id') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='question') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='packages') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='inputs') ... FAIL
  test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='sources') ... FAIL
test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) ... 
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='source_kinds') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='component_kinds') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='borrowed') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='before_concepts') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='after_concepts') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='settlement_registration') ... FAIL
  test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='thing_registration') ... FAIL
test_missing_lender_violates_borrowing (test_checker.CheckerTests.test_missing_lender_violates_borrowing) ... FAIL
test_missing_thing_concept_violates_ownership (test_checker.CheckerTests.test_missing_thing_concept_violates_ownership) ... FAIL
test_refusal_does_not_hide_independent_declaration_violation (test_checker.CheckerTests.test_refusal_does_not_hide_independent_declaration_violation) ... FAIL
test_retained_unborrowed_lender_concept_is_allowed (test_checker.CheckerTests.test_retained_unborrowed_lender_concept_is_allowed) ... ok
test_self_borrowing_violates_borrowing (test_checker.CheckerTests.test_self_borrowing_violates_borrowing) ... FAIL
test_settlement_refusal_does_not_certify_partial_lender_state (test_checker.CheckerTests.test_settlement_refusal_does_not_certify_partial_lender_state) ... FAIL
test_thing_refusal_does_not_hide_missing_lender (test_checker.CheckerTests.test_thing_refusal_does_not_hide_missing_lender) ... FAIL
test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown (test_checker.CheckerTests.test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown) ... FAIL
test_unborrowed_lender_concept_must_not_change_owner (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_change_owner) ... FAIL
test_unborrowed_lender_concept_must_not_disappear (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_disappear) ... FAIL
test_undeclared_collision_violates_ownership (test_checker.CheckerTests.test_undeclared_collision_violates_ownership) ... FAIL
test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) ... 
  test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='unknown') ... FAIL
  test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='duplicate') ... FAIL
  test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='missing') ... FAIL
test_unused_borrowing_violates_borrowing (test_checker.CheckerTests.test_unused_borrowing_violates_borrowing) ... FAIL
test_wrong_lender_violates_borrowing (test_checker.CheckerTests.test_wrong_lender_violates_borrowing) ... FAIL
test_wrong_owner_without_borrowing_is_not_satisfied (test_checker.CheckerTests.test_wrong_owner_without_borrowing_is_not_satisfied) ... FAIL

======================================================================
FAIL: test_ambiguous_lender_does_not_collapse_to_matching_owner (test_checker.CheckerTests.test_ambiguous_lender_does_not_collapse_to_matching_owner)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 134, in test_ambiguous_lender_does_not_collapse_to_matching_owner
    self.assertEqual(self.outcome("borrowing"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_borrowed_name_must_retain_its_declared_owner (test_checker.CheckerTests.test_borrowed_name_must_retain_its_declared_owner)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 112, in test_borrowed_name_must_retain_its_declared_owner
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_conflicting_borrowing_owners_cannot_be_overwritten (test_checker.CheckerTests.test_conflicting_borrowing_owners_cannot_be_overwritten)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 74, in test_conflicting_borrowing_owners_cannot_be_overwritten
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_duplicate_borrowing_violates_borrowing (test_checker.CheckerTests.test_duplicate_borrowing_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 70, in test_duplicate_borrowing_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_duplicate_component_violates_components (test_checker.CheckerTests.test_duplicate_component_violates_components)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 100, in test_duplicate_component_violates_components
    self.assertEqual(self.outcome("components"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_duplicate_observed_owner_does_not_collapse_to_one (test_checker.CheckerTests.test_duplicate_observed_owner_does_not_collapse_to_one)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 130, in test_duplicate_observed_owner_does_not_collapse_to_one
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_duplicate_source_violates_components (test_checker.CheckerTests.test_duplicate_source_violates_components)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 96, in test_duplicate_source_violates_components
    self.assertEqual(self.outcome("components"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_extra_component_violates_components (test_checker.CheckerTests.test_extra_component_violates_components)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 88, in test_extra_component_violates_components
    self.assertEqual(self.outcome("components"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_extra_thing_concept_violates_ownership (test_checker.CheckerTests.test_extra_thing_concept_violates_ownership)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 104, in test_extra_thing_concept_violates_ownership
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='schema', value='other')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='contributor', value='other')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='scope', value='other')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='questions', value={})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='questions', value=[None])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='authorities', value=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (field='authorities', value=[4])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 242, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='id', value=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='question', value='')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='packages', value='wrong')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='inputs', value=[None])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (question_field='sources', value=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 249, in test_invalid_contract_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract=None)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 252, in test_invalid_contract_shapes_raise_value_error
    with self.subTest(contract=contract), self.assertRaises(ValueError):
                                          ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 252, in test_invalid_contract_shapes_raise_value_error
    with self.subTest(contract=contract), self.assertRaises(ValueError):
                                          ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) (contract={'schema': 'counterpart-v1', 'contributor': 'hornvale.thing', 'scope': 'domains/thing', 'questions': [{'id': 'registration', 'question': 'Does Settlement then Thing registration complete without refusal?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['hornvale_settlement::register_concepts', 'hornvale_thing::register_concepts', 'ConceptRegistry::concepts'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}, {'id': 'components', 'question': 'Do Thing source and component rosters agree in both directions, with no duplicates?', 'packages': ['hornvale-thing'], 'inputs': ['THING_KINDS', 'thing_registry'], 'sources': ['domains/thing/src/lib.rs']}, {'id': 'borrowing', 'question': 'Are borrowing declarations unique, members of the Thing roster, non-self, and supplied by the stated owner before Thing registration?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['BORROWED', 'ConceptRegistry::concepts (before Thing)'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}, {'id': 'ownership', 'question': 'Does the completed registry give every Thing name its stated owner, contain no extra Thing-owned names, and preserve lender owners?', 'packages': ['hornvale-settlement', 'hornvale-thing'], 'inputs': ['ConceptRegistry::concepts (before and after Thing)', 'THING_KINDS', 'BORROWED'], 'sources': ['domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs']}], 'authorities': ['domains/CLAUDE.md', 'domains/thing/src/lib.rs', 'domains/settlement/src/lib.rs', 'kernel/src/registry.rs'], 'unknown': True})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 252, in test_invalid_contract_shapes_raise_value_error
    with self.subTest(contract=contract), self.assertRaises(ValueError):
                                          ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value='key')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value=[None])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='source_kinds', value=[''])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='component_kinds', value={})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value={})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value=['hearth'])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='borrowed', value=[{'name': 'hearth'}])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='before_concepts', value=[{'name': 'hearth', 'owner': 7}])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='after_concepts', value=[{'name': 'key', 'owner': ''}])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='thing_registration', value={'outcome': 'unknown', 'detail': ''})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='thing_registration', value={'outcome': 'accepted'})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='settlement_registration', value={'outcome': 'accepted', 'detail': None})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (field='settlement_registration', value=None)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 200, in test_invalid_fact_shapes_raise_value_error
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts=None)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 203, in test_invalid_fact_shapes_raise_value_error
    with self.subTest(facts=facts), self.assertRaises(ValueError):
                                    ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts=[])
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 203, in test_invalid_fact_shapes_raise_value_error
    with self.subTest(facts=facts), self.assertRaises(ValueError):
                                    ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) (facts={'source_kinds': ['hearth', 'key'], 'component_kinds': ['hearth', 'key'], 'borrowed': [{'name': 'hearth', 'owner': 'settlement'}], 'before_concepts': [{'name': 'hearth', 'owner': 'settlement'}], 'after_concepts': [{'name': 'hearth', 'owner': 'settlement'}, {'name': 'key', 'owner': 'thing'}], 'settlement_registration': {'outcome': 'accepted', 'detail': ''}, 'thing_registration': {'outcome': 'accepted', 'detail': ''}, 'candidate': {}})
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 203, in test_invalid_fact_shapes_raise_value_error
    with self.subTest(facts=facts), self.assertRaises(ValueError):
                                    ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_component_violates_components (test_checker.CheckerTests.test_missing_component_violates_components)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 92, in test_missing_component_violates_components
    self.assertEqual(self.outcome("components"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='schema')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='contributor')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='scope')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='questions')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (field='authorities')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 211, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='id')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='question')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='packages')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='inputs')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) (question_field='sources')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 217, in test_missing_contract_fields_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='source_kinds')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='component_kinds')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='borrowed')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='before_concepts')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='after_concepts')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='settlement_registration')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) (field='thing_registration')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 180, in test_missing_fact_fields_are_invalid_even_after_refusal
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_missing_lender_violates_borrowing (test_checker.CheckerTests.test_missing_lender_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 62, in test_missing_lender_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_missing_thing_concept_violates_ownership (test_checker.CheckerTests.test_missing_thing_concept_violates_ownership)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 108, in test_missing_thing_concept_violates_ownership
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_refusal_does_not_hide_independent_declaration_violation (test_checker.CheckerTests.test_refusal_does_not_hide_independent_declaration_violation)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 161, in test_refusal_does_not_hide_independent_declaration_violation
    self.assertEqual(self.outcome("borrowing"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_self_borrowing_violates_borrowing (test_checker.CheckerTests.test_self_borrowing_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 84, in test_self_borrowing_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_settlement_refusal_does_not_certify_partial_lender_state (test_checker.CheckerTests.test_settlement_refusal_does_not_certify_partial_lender_state)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 152, in test_settlement_refusal_does_not_certify_partial_lender_state
    self.assertEqual(
    ~~~~~~~~~~~~~~~~^
        {key: row["outcome"] for key, row in evaluate(self.facts, self.contract).items()},
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        {"registration": "violated", "components": "satisfied",
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         "borrowing": "unknown", "ownership": "unknown"},
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    )
    ^
AssertionError: {'registration': 'satisfied', 'components': 'satisfied', 'b[44 chars]ied'} != {'registration': 'violated', 'components': 'satisfied', 'bo[39 chars]own'}
- {'borrowing': 'satisfied',
+ {'borrowing': 'unknown',
   'components': 'satisfied',
-  'ownership': 'satisfied',
+  'ownership': 'unknown',
-  'registration': 'satisfied'}
?                   ^  ----

+  'registration': 'violated'}
?                   ^^^^


======================================================================
FAIL: test_thing_refusal_does_not_hide_missing_lender (test_checker.CheckerTests.test_thing_refusal_does_not_hide_missing_lender)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 148, in test_thing_refusal_does_not_hide_missing_lender
    self.assertEqual(self.outcome("borrowing"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown (test_checker.CheckerTests.test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 139, in test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown
    self.assertEqual(
    ~~~~~~~~~~~~~~~~^
        {key: row["outcome"] for key, row in evaluate(self.facts, self.contract).items()},
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        {"registration": "violated", "components": "satisfied",
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         "borrowing": "satisfied", "ownership": "unknown"},
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    )
    ^
AssertionError: {'registration': 'satisfied', 'components': 'satisfied', 'b[44 chars]ied'} != {'registration': 'violated', 'components': 'satisfied', 'bo[41 chars]own'}
  {'borrowing': 'satisfied',
   'components': 'satisfied',
-  'ownership': 'satisfied',
+  'ownership': 'unknown',
-  'registration': 'satisfied'}
?                   ^  ----

+  'registration': 'violated'}
?                   ^^^^


======================================================================
FAIL: test_unborrowed_lender_concept_must_not_change_owner (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_change_owner)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 117, in test_unborrowed_lender_concept_must_not_change_owner
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_unborrowed_lender_concept_must_not_disappear (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_disappear)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 121, in test_unborrowed_lender_concept_must_not_disappear
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_undeclared_collision_violates_ownership (test_checker.CheckerTests.test_undeclared_collision_violates_ownership)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 58, in test_undeclared_collision_violates_ownership
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='unknown')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 230, in test_unknown_duplicate_or_missing_question_ids_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='duplicate')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 230, in test_unknown_duplicate_or_missing_question_ids_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) (operation='missing')
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 230, in test_unknown_duplicate_or_missing_question_ids_are_invalid
    with self.assertRaises(ValueError):
         ~~~~~~~~~~~~~~~~~^^^^^^^^^^^^
AssertionError: ValueError not raised

======================================================================
FAIL: test_unused_borrowing_violates_borrowing (test_checker.CheckerTests.test_unused_borrowing_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 79, in test_unused_borrowing_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_wrong_lender_violates_borrowing (test_checker.CheckerTests.test_wrong_lender_violates_borrowing)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 66, in test_wrong_lender_violates_borrowing
    self.assertEqual(self.outcome("borrowing"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


======================================================================
FAIL: test_wrong_owner_without_borrowing_is_not_satisfied (test_checker.CheckerTests.test_wrong_owner_without_borrowing_is_not_satisfied)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/tools/digest/experiments/the-counterpart/test_checker.py", line 54, in test_wrong_owner_without_borrowing_is_not_satisfied
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


----------------------------------------------------------------------
Ran 32 tests in 0.017s

FAILED (failures=74)
```

## Implemented GREEN (exit 0)

```text
test_ambiguous_lender_does_not_collapse_to_matching_owner (test_checker.CheckerTests.test_ambiguous_lender_does_not_collapse_to_matching_owner) ... ok
test_array_order_has_no_semantic_role (test_checker.CheckerTests.test_array_order_has_no_semantic_role) ... ok
test_borrowed_name_must_retain_its_declared_owner (test_checker.CheckerTests.test_borrowed_name_must_retain_its_declared_owner) ... ok
test_conflicting_borrowing_owners_cannot_be_overwritten (test_checker.CheckerTests.test_conflicting_borrowing_owners_cannot_be_overwritten) ... ok
test_duplicate_borrowing_violates_borrowing (test_checker.CheckerTests.test_duplicate_borrowing_violates_borrowing) ... ok
test_duplicate_component_violates_components (test_checker.CheckerTests.test_duplicate_component_violates_components) ... ok
test_duplicate_observed_owner_does_not_collapse_to_one (test_checker.CheckerTests.test_duplicate_observed_owner_does_not_collapse_to_one) ... ok
test_duplicate_source_violates_components (test_checker.CheckerTests.test_duplicate_source_violates_components) ... ok
test_extra_component_violates_components (test_checker.CheckerTests.test_extra_component_violates_components) ... ok
test_extra_thing_concept_violates_ownership (test_checker.CheckerTests.test_extra_thing_concept_violates_ownership) ... ok
test_inputs_are_not_changed (test_checker.CheckerTests.test_inputs_are_not_changed) ... ok
test_invalid_contract_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_contract_shapes_raise_value_error) ... ok
test_invalid_fact_shapes_raise_value_error (test_checker.CheckerTests.test_invalid_fact_shapes_raise_value_error) ... ok
test_legitimate_borrowing_satisfies_all_four_questions (test_checker.CheckerTests.test_legitimate_borrowing_satisfies_all_four_questions) ... ok
test_missing_component_violates_components (test_checker.CheckerTests.test_missing_component_violates_components) ... ok
test_missing_contract_fields_are_invalid (test_checker.CheckerTests.test_missing_contract_fields_are_invalid) ... ok
test_missing_fact_fields_are_invalid_even_after_refusal (test_checker.CheckerTests.test_missing_fact_fields_are_invalid_even_after_refusal) ... ok
test_missing_lender_violates_borrowing (test_checker.CheckerTests.test_missing_lender_violates_borrowing) ... ok
test_missing_thing_concept_violates_ownership (test_checker.CheckerTests.test_missing_thing_concept_violates_ownership) ... ok
test_refusal_does_not_hide_independent_declaration_violation (test_checker.CheckerTests.test_refusal_does_not_hide_independent_declaration_violation) ... ok
test_retained_unborrowed_lender_concept_is_allowed (test_checker.CheckerTests.test_retained_unborrowed_lender_concept_is_allowed) ... ok
test_self_borrowing_violates_borrowing (test_checker.CheckerTests.test_self_borrowing_violates_borrowing) ... ok
test_settlement_refusal_does_not_certify_partial_lender_state (test_checker.CheckerTests.test_settlement_refusal_does_not_certify_partial_lender_state) ... ok
test_thing_refusal_does_not_hide_missing_lender (test_checker.CheckerTests.test_thing_refusal_does_not_hide_missing_lender) ... ok
test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown (test_checker.CheckerTests.test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown) ... ok
test_unborrowed_lender_concept_must_not_change_owner (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_change_owner) ... ok
test_unborrowed_lender_concept_must_not_disappear (test_checker.CheckerTests.test_unborrowed_lender_concept_must_not_disappear) ... ok
test_undeclared_collision_violates_ownership (test_checker.CheckerTests.test_undeclared_collision_violates_ownership) ... ok
test_unknown_duplicate_or_missing_question_ids_are_invalid (test_checker.CheckerTests.test_unknown_duplicate_or_missing_question_ids_are_invalid) ... ok
test_unused_borrowing_violates_borrowing (test_checker.CheckerTests.test_unused_borrowing_violates_borrowing) ... ok
test_wrong_lender_violates_borrowing (test_checker.CheckerTests.test_wrong_lender_violates_borrowing) ... ok
test_wrong_owner_without_borrowing_is_not_satisfied (test_checker.CheckerTests.test_wrong_owner_without_borrowing_is_not_satisfied) ... ok

----------------------------------------------------------------------
Ran 32 tests in 0.005s

OK
```

## Applied executable mutation (expected test exit 1)

```text
Applied mutation: remove observed-owner versus expected-owner comparison
Original SHA256: ee14515c833ddb43397b25ad9b035dcc18d07c748d7f1a1f3effa6b9f8902c90
Mutated SHA256: 48025a267eca811cf5748b678a488b3d5f7f28984e4c6bbf4595914d1cf6706e
checker.py and test_checker.py compile: PASS
Behavioral test exit: 1
test_wrong_owner_without_borrowing_is_not_satisfied (test_checker.CheckerTests.test_wrong_owner_without_borrowing_is_not_satisfied) ... FAIL

======================================================================
FAIL: test_wrong_owner_without_borrowing_is_not_satisfied (test_checker.CheckerTests.test_wrong_owner_without_borrowing_is_not_satisfied)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/private/var/folders/_0/j0_zkq_d3jn0klz033gq33cc0000gn/T/counterpart-checker-mutation-rlb57g6g/test_checker.py", line 54, in test_wrong_owner_without_borrowing_is_not_satisfied
    self.assertEqual(self.outcome("ownership"), "violated")
    ~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
AssertionError: 'satisfied' != 'violated'
- satisfied
+ violated


----------------------------------------------------------------------
Ran 1 test in 0.001s

FAILED (failures=1)
```

## Ordinary commit hook and commit (exit 0)

```text
pre-commit: no Rust-relevant paths staged — running the prose-subject tests instead of 'make gate-commit'.
    Finished `test` profile [optimized + debuginfo] target(s) in 0.05s
────────────
 Nextest run ID 9d12dd46-fea8-4ce1-8d4b-758bebfa04cb with nextest profile: default
    Starting 75 tests across 1 binary (246 tests skipped)
        PASS [   0.015s] ( 1/75) hornvale::suite audio_artifacts::committed_audio_is_exactly_the_page_referenced_set
        PASS [   0.019s] ( 2/75) hornvale::suite architecture::a_new_concept_and_phenomenon_kind_need_no_god_enum_or_domain_edit
        PASS [   0.023s] ( 3/75) hornvale::suite architecture::every_client_workspace_declares_an_optimised_dev_profile
        PASS [   0.024s] ( 4/75) hornvale::suite census_duration::a_census_over_the_alarm_threshold_owes_a_profiling_followup
        PASS [   0.016s] ( 5/75) hornvale::suite census_duration::the_chronologically_latest_row_wins_even_when_it_is_not_last_in_the_file
        PASS [   0.015s] ( 6/75) hornvale::suite census_duration::the_latest_census_is_under_the_refusal_ceiling
        PASS [   0.027s] ( 7/75) hornvale::suite census_duration::the_census_ledger_has_rows_this_test_can_read
        PASS [   0.050s] ( 8/75) hornvale::suite architecture::windows_depend_only_on_kernel_domains_and_windows
        PASS [   0.051s] ( 9/75) hornvale::suite architecture::the_layering_page_matches_the_enforced_graph
        PASS [   0.052s] (10/75) hornvale::suite architecture::domains_depend_only_on_the_kernel
        PASS [   0.053s] (11/75) hornvale::suite architecture::external_dependencies_are_allowlisted
        PASS [   0.053s] (12/75) hornvale::suite architecture::the_layering_render_is_deterministic_and_grounded
        PASS [   0.015s] (13/75) hornvale::suite docs_consistency::an_escaped_pipe_is_not_a_column_separator
        PASS [   0.057s] (14/75) hornvale::suite architecture::the_kernel_depends_on_no_workspace_crate
        PASS [   0.011s] (15/75) hornvale::suite docs_consistency::cite_errors_in_catches_line_wrapped_cites
        PASS [   0.011s] (16/75) hornvale::suite docs_consistency::cite_error_resolves_the_known_forms
        PASS [   0.011s] (17/75) hornvale::suite docs_consistency::cite_errors_in_is_case_insensitive
        PASS [   0.015s] (18/75) hornvale::suite docs_consistency::committed_reconciliation_schema_is_parseable
        PASS [   0.027s] (19/75) hornvale::suite docs_consistency::campaign_reconciliation_covers_every_campaign_record
        PASS [   0.022s] (20/75) hornvale::suite docs_consistency::campaign_record_paths_enumerate_the_actual_audit_directories
        PASS [   0.011s] (21/75) hornvale::suite docs_consistency::decision_numbers_are_unique
        PASS [   0.028s] (22/75) hornvale::suite docs_consistency::committed_reconciliation_rows_satisfy_semantic_rules
        PASS [   0.057s] (23/75) hornvale::suite docs_consistency::a_decision_records_title_matches_its_filename
        PASS [   0.013s] (24/75) hornvale::suite docs_consistency::every_frontier_section_is_listed_in_the_contents
        PASS [   0.015s] (25/75) hornvale::suite docs_consistency::every_refuted_row_cites_its_evidence
        PASS [   0.011s] (26/75) hornvale::suite docs_consistency::reconciliation_parser_keeps_all_five_record_columns
        PASS [   0.020s] (27/75) hornvale::suite docs_consistency::every_registry_row_carries_a_pointer
        PASS [   0.025s] (28/75) hornvale::suite docs_consistency::every_campaign_with_a_spec_and_a_plan_has_a_ledger
        PASS [   0.014s] (29/75) hornvale::suite docs_consistency::every_registry_table_row_is_a_parseable_id_row
        PASS [   0.017s] (30/75) hornvale::suite docs_consistency::no_new_numbered_registry_ids
        PASS [   0.011s] (31/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_a_short_row
        PASS [   0.011s] (32/75) hornvale::suite docs_consistency::reconciliation_parser_requires_evidence
        PASS [   0.012s] (33/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_terminal_rows_with_a_destination
        PASS [   0.013s] (34/75) hornvale::suite docs_consistency::reconciliation_parser_requires_residue_for_partial_rows
        PASS [   0.014s] (35/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_unknown_disposition
        PASS [   0.010s] (36/75) hornvale::suite docs_consistency::reconciliation_validation_rejects_shipped_registry_targets
        PASS [   0.016s] (37/75) hornvale::suite docs_consistency::reconciliation_validation_rejects_duplicate_keys_and_wrong_record_columns
        PASS [   0.012s] (38/75) hornvale::suite docs_consistency::refuted_is_an_admissible_status
        PASS [   0.018s] (39/75) hornvale::suite docs_consistency::registry_idea_cells_are_within_budget
        PASS [   0.020s] (40/75) hornvale::suite docs_consistency::registry_ids_are_unique
        PASS [   0.012s] (41/75) hornvale::suite docs_consistency::status_normalization_handles_the_documented_forms
        PASS [   0.021s] (42/75) hornvale::suite docs_consistency::registry_rows_have_five_columns
        PASS [   0.020s] (43/75) hornvale::suite docs_consistency::registry_statuses_use_the_closed_vocabulary
        PASS [   0.012s] (44/75) hornvale::suite docs_consistency::the_confidence_gradient_links_resolve
        PASS [   0.012s] (45/75) hornvale::suite docs_consistency::the_history_page_prose_names_the_vertex_it_renders
        PASS [   0.014s] (46/75) hornvale::suite docs_consistency::the_decision_log_starts_at_0001
        PASS [   0.013s] (47/75) hornvale::suite docs_consistency::the_unmatched_plan_count_has_not_moved
        PASS [   0.015s] (48/75) hornvale::suite docs_consistency::the_ledger_exemption_list_only_shrinks
        PASS [   0.011s] (49/75) hornvale::suite generated_paths::every_declared_path_names_a_known_author
        PASS [   0.019s] (50/75) hornvale::suite docs_consistency::the_waiver_list_only_shrinks
        PASS [   0.014s] (51/75) hornvale::suite generated_paths::every_declared_generated_path_is_written_by_its_author
        PASS [   0.021s] (52/75) hornvale::suite generated_paths::an_overriding_declaration_must_be_measured
        PASS [   0.012s] (53/75) hornvale::suite generated_paths::known_authors_agree_with_the_roster
        PASS [   0.013s] (54/75) hornvale::suite generated_paths::no_generated_artifact_is_routed_through_a_regenerating_merge_driver
        PASS [   0.010s] (55/75) hornvale::suite generated_paths::the_declared_list_is_not_empty
        PASS [   0.024s] (56/75) hornvale::suite generated_paths::no_two_declared_rows_tie_for_precedence_with_different_authors
        PASS [   0.020s] (57/75) hornvale::suite generated_paths::the_root_guide_names_the_declared_path_list
        PASS [   0.012s] (58/75) hornvale::suite lexicon_guard::a_waiver_needs_a_reason
        PASS [   0.014s] (59/75) hornvale::suite lexicon_guard::the_tokenizer_counts_affixed_bare_and_shouting_forms
        PASS [   0.015s] (60/75) hornvale::suite lexicon_guard::the_inventory_is_sorted_and_not_empty
        PASS [   0.190s] (61/75) hornvale::suite docs_consistency::all_knowledge_doc_links_resolve
        PASS [   0.076s] (62/75) hornvale::suite generated_paths::no_claude_md_restates_the_declared_path_list
        PASS [   0.255s] (63/75) hornvale::suite docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary
        PASS [   0.332s] (64/75) hornvale::suite docs_consistency::decision_block_declaration_count_has_not_dropped
        PASS [   0.326s] (65/75) hornvale::suite docs_consistency::decision_blocks_do_not_overlap_across_campaigns
        PASS [   0.035s] (66/75) hornvale::suite temp_path_ratchet::a_reasonless_roster_entry_is_a_parse_error
        PASS [   0.110s] (67/75) hornvale::suite subfloor_roster_coverage::every_workspace_crate_has_a_roster_entry_or_a_declared_reason
        PASS [   0.313s] (68/75) hornvale::suite repose_byte_identity::no_rendered_artifact_names_a_geohazard
        PASS [   0.242s] (69/75) hornvale::suite temp_path_ratchet::no_new_fixed_temp_path_appears
        PASS [   0.582s] (70/75) hornvale::suite docs_consistency::decision_cites_in_sources_resolve
        PASS [   0.632s] (71/75) hornvale::suite lexicon_guard::no_vertex_sense_cell_comes_back
        PASS [   1.439s] (72/75) hornvale::suite generated_paths::every_declared_generated_path_is_tracked
        PASS [   2.237s] (73/75) hornvale::suite repose_byte_identity::seed_42_almanac_is_unmoved_by_the_repose
        PASS [   4.183s] (74/75) hornvale::suite repose_byte_identity::seed_42_world_json_is_unmoved_by_the_repose
        PASS [   4.722s] (75/75) hornvale::suite repose_byte_identity::seed_42_scene_output_is_unmoved_by_the_repose
────────────
     Summary [   4.949s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-checker 26c693ce0] Add independent raw-fact ownership checker for The Counterpart
 3 files changed, 553 insertions(+)
 create mode 100644 tools/digest/experiments/the-counterpart/checker-derivation.md
 create mode 100644 tools/digest/experiments/the-counterpart/checker.py
 create mode 100644 tools/digest/experiments/the-counterpart/test_checker.py
```


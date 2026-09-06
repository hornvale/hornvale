# Task 6 reserved authorship report

READY for controller scoring once normal Stage 2 is green. This report records
independent challenge authorship and compile/raw qualification only, not a
formal score or Task 6 acceptance.

## Worktree and immutable preregistration

`/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker`
`codex/counterpart-challenge`
Source base: `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`
Freeze commit: `16861a886a7069cf01123d0cde5688364781cff9`
Preregistration commit: `fbb3949a067dbd2b24e93ad4912a069921b6cea3`
Preregistration UTC: `2026-09-05T20:30:32.690483+00:00`
Patch: `tools/digest/experiments/the-counterpart/patches/reserved-inferred-owner.patch`
Patch SHA256: `77fde4719e32393242c057bf3cdbfff9edd34b688ffee18ca8ddc1a280bcedcf`

The preregistration artifact and exact patch were committed through normal
hooks before applying the patch or running the compiler/raw observer. The
normal preregistration hook passed 75 tests, 246 skipped. Its setup rebuild
(vessel/lab/CLI after branch switch) overlapped the controller merge build;
this is recorded as ambient/setup cost, not challenge compile timing. The
actual challenge compile waited for explicit controller slot release.

## Independently selected correlated error

One patch, two scoped source hunks, selected by the earlier checker author:

1. Thing really registers ordinary `key` under owner `settlement`, though the
   authored roster and component roster still include it and BORROWED does not.
2. The candidate collector derives `expected_owner` from the observed composed
   registry rather than independently from the source/borrowing contract.

No outcome is replaced by a constant. Registration, component, missing-concept,
extra-Thing-name, unused-borrowing, and duplicate branches still execute. The
preregistered all-satisfied declared-answer record consequently agrees with
actual candidate checks despite a real source ownership violation. This is
stronger than merely supplying an always-satisfied declaration.

## Actual qualification, not scoring

```text
Starting build through charter_measure.measure
build: rc=0, elapsed=6.398s, cleanup_error=None
Starting observation through charter_measure.measure
observation: rc=0, elapsed=0.177s, cleanup_error=None
Source and lock restoration verified byte-exact
```

Build started `2026-09-05T20:33:17Z`; observation started
`2026-09-05T20:33:24Z`. The existing frozen `charter_measure.measure` supervised
both with output retained, a 3600-second deadline, and 16777216-byte per-stream
bound. The compiler used `--locked --offline` and this worktree's dedicated
`target/counterpart-reserved-author`; no target was shared with another author.
One build and one raw observation ran, both completed without timeout, output
bound violation, launch/cleanup error, or interruption. No failed attempts or
patch corrections occurred. Original measurement outputs, base64 raw streams,
console output, harness text and restoration receipt are retained in
`tools/digest/experiments/the-counterpart/evidence/reserved-author/`.

Author interpretation of the raw facts: `key` is present in both source and
component rosters, absent from borrowing and the before registry, but present
under `settlement` afterward. Both registrations accepted. The candidate
contribution accepted and its three actual observations all say `satisfied`.
Under the accepted ownership question, this ordinary nonborrowed Thing name
must instead belong to `thing`. The intended raw effect therefore occurred.
No frozen checker, comparator, or selection/scoring function was invoked.
The controller must evaluate acceptance after the normal stage prerequisite.

Raw stdout SHA256:
`4811a0d01f1211c981f36020ebb64be4932532fbc914b1b925c0ecadf382c3b0`
Raw stdout length: 5166 bytes. Raw stderr: zero bytes. Retained base64 stream
lengths and SHA256 hashes were verified, along with the exact raw-output copy.

## Disclosure and effort

Earlier context is the independent Task 2 checker derivation, including its
required literal wrong-owner fixture and disclosed production Thing test
comments. After committed freeze, the author first read the actual candidate
collector `tools/digest/packages/thing/src/lib.rs`, including its tests, to
select a correlated check failure. Also read allowed production registrars,
measurement supervisor, and build manifests. Extracted only freeze identities,
file identities, checker_author and top-level key names programmatically;
never inspected source_arms values. Did not read owner records, other author
patches, panel arms, unreserved outputs, comparison implementation, or other
worker reports. The controller mentioned new main-side Thing traits during
slot coordination; the fixed source base was unchanged and those changes were
not read. No filesystem-blindness claim is made.

Authored effort: one independent selection; one two-hunk patch; one committed
preregistration before qualification; one successful build and one successful
raw observation; zero post-preregistration patch edits. The precise interval
from preregistration timestamp to restoration is retained in
`author-interpretation.json`; it includes hook/setup and queue wait, not only
active work. Earlier selection/reading labor was not separately instrumented.

## Restoration and boundaries

Production changes were applied only temporarily to this owned source checkout.
Both changed source files and root/tool Cargo lockfiles were restored exactly;
locks did not change during qualification. `git diff --exit-code` over those
files and Settlement source was clean after restoration. All supervisor-owned
processes were reaped. No frozen checker, contract, raw observer, rule,
owner-record, or panel write occurred. The source patch is artifact data only;
no production-source commit, full panel, workspace run, census, or shared-main
write was made. Controller owns source-object/bundle/panel extension and formal
judgments. Final evidence-commit receipt follows after its normal hook.

## Final evidence commit

`41ceed7a270771833e8b738366e61721e5ab1ab7`

Normal hook and commit completed with exit 0. Working tree status is clean.

```text
pre-commit: no Rust-relevant paths staged — running the prose-subject tests instead of 'make gate-commit'.
   Compiling hornvale-thing v0.1.0 (/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/domains/thing)
   Compiling hornvale-worldgen v0.1.0 (/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/windows/worldgen)
   Compiling hornvale-locale v0.1.0 (/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/windows/locale)
   Compiling hornvale-book v0.1.0 (/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/windows/book)
   Compiling hornvale-scene v0.1.0 (/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/windows/scene)
   Compiling hornvale-vessel v0.1.0 (/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/windows/vessel)
   Compiling hornvale-lab v0.1.0 (/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/windows/lab)
   Compiling hornvale v0.1.0 (/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker/cli)
    Finished `test` profile [optimized + debuginfo] target(s) in 5.02s
────────────
 Nextest run ID 2bef1daa-e774-4838-89d8-cdb8fcf751b3 with nextest profile: default
    Starting 75 tests across 1 binary (246 tests skipped)
        PASS [   0.239s] ( 1/75) hornvale::suite architecture::a_new_concept_and_phenomenon_kind_need_no_god_enum_or_domain_edit
        PASS [   0.246s] ( 2/75) hornvale::suite architecture::every_client_workspace_declares_an_optimised_dev_profile
        PASS [   0.254s] ( 3/75) hornvale::suite census_duration::a_census_over_the_alarm_threshold_owes_a_profiling_followup
        PASS [   0.022s] ( 4/75) hornvale::suite census_duration::the_census_ledger_has_rows_this_test_can_read
        PASS [   0.274s] ( 5/75) hornvale::suite architecture::external_dependencies_are_allowlisted
        PASS [   0.274s] ( 6/75) hornvale::suite audio_artifacts::committed_audio_is_exactly_the_page_referenced_set
        PASS [   0.274s] ( 7/75) hornvale::suite architecture::domains_depend_only_on_the_kernel
        PASS [   0.274s] ( 8/75) hornvale::suite architecture::windows_depend_only_on_kernel_domains_and_windows
        PASS [   0.274s] ( 9/75) hornvale::suite architecture::the_layering_render_is_deterministic_and_grounded
        PASS [   0.283s] (10/75) hornvale::suite architecture::the_kernel_depends_on_no_workspace_crate
        PASS [   0.284s] (11/75) hornvale::suite architecture::the_layering_page_matches_the_enforced_graph
        PASS [   0.039s] (12/75) hornvale::suite census_duration::the_chronologically_latest_row_wins_even_when_it_is_not_last_in_the_file
        PASS [   0.034s] (13/75) hornvale::suite census_duration::the_latest_census_is_under_the_refusal_ceiling
        PASS [   0.014s] (14/75) hornvale::suite docs_consistency::an_escaped_pipe_is_not_a_column_separator
        PASS [   0.065s] (15/75) hornvale::suite docs_consistency::a_decision_records_title_matches_its_filename
        PASS [   0.116s] (16/75) hornvale::suite docs_consistency::cite_errors_in_catches_line_wrapped_cites
        PASS [   0.127s] (17/75) hornvale::suite docs_consistency::committed_reconciliation_rows_satisfy_semantic_rules
        PASS [   0.139s] (18/75) hornvale::suite docs_consistency::campaign_reconciliation_covers_every_campaign_record
        PASS [   0.169s] (19/75) hornvale::suite docs_consistency::cite_error_resolves_the_known_forms
        PASS [   0.159s] (20/75) hornvale::suite docs_consistency::cite_errors_in_is_case_insensitive
        PASS [   0.156s] (21/75) hornvale::suite docs_consistency::committed_reconciliation_schema_is_parseable
        PASS [   0.032s] (22/75) hornvale::suite docs_consistency::decision_numbers_are_unique
        PASS [   0.040s] (23/75) hornvale::suite docs_consistency::every_campaign_with_a_spec_and_a_plan_has_a_ledger
        PASS [   0.183s] (24/75) hornvale::suite docs_consistency::campaign_record_paths_enumerate_the_actual_audit_directories
        PASS [   0.090s] (25/75) hornvale::suite docs_consistency::reconciliation_parser_keeps_all_five_record_columns
        PASS [   0.105s] (26/75) hornvale::suite docs_consistency::every_frontier_section_is_listed_in_the_contents
        PASS [   0.106s] (27/75) hornvale::suite docs_consistency::every_registry_table_row_is_a_parseable_id_row
        PASS [   0.100s] (28/75) hornvale::suite docs_consistency::no_new_numbered_registry_ids
        PASS [   0.110s] (29/75) hornvale::suite docs_consistency::every_registry_row_carries_a_pointer
        PASS [   0.112s] (30/75) hornvale::suite docs_consistency::every_refuted_row_cites_its_evidence
        PASS [   0.310s] (31/75) hornvale::suite docs_consistency::all_knowledge_doc_links_resolve
        PASS [   0.043s] (32/75) hornvale::suite docs_consistency::reconciliation_validation_rejects_shipped_registry_targets
        PASS [   0.075s] (33/75) hornvale::suite docs_consistency::reconciliation_parser_requires_evidence
        PASS [   0.080s] (34/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_terminal_rows_with_a_destination
        PASS [   0.083s] (35/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_a_short_row
        PASS [   0.075s] (36/75) hornvale::suite docs_consistency::reconciliation_validation_rejects_duplicate_keys_and_wrong_record_columns
        PASS [   0.076s] (37/75) hornvale::suite docs_consistency::reconciliation_parser_requires_residue_for_partial_rows
        PASS [   0.081s] (38/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_unknown_disposition
        PASS [   0.306s] (39/75) hornvale::suite docs_consistency::decision_blocks_do_not_overlap_across_campaigns
        PASS [   0.349s] (40/75) hornvale::suite docs_consistency::decision_block_declaration_count_has_not_dropped
        PASS [   0.060s] (41/75) hornvale::suite docs_consistency::status_normalization_handles_the_documented_forms
        PASS [   0.057s] (42/75) hornvale::suite docs_consistency::the_decision_log_starts_at_0001
        PASS [   0.067s] (43/75) hornvale::suite docs_consistency::registry_rows_have_five_columns
        PASS [   0.067s] (44/75) hornvale::suite docs_consistency::registry_statuses_use_the_closed_vocabulary
        PASS [   0.070s] (45/75) hornvale::suite docs_consistency::registry_ids_are_unique
        PASS [   0.071s] (46/75) hornvale::suite docs_consistency::registry_idea_cells_are_within_budget
        PASS [   0.107s] (47/75) hornvale::suite docs_consistency::the_history_page_prose_names_the_vertex_it_renders
        PASS [   0.168s] (48/75) hornvale::suite docs_consistency::the_confidence_gradient_links_resolve
        PASS [   0.104s] (49/75) hornvale::suite docs_consistency::the_waiver_list_only_shrinks
        PASS [   0.110s] (50/75) hornvale::suite generated_paths::an_overriding_declaration_must_be_measured
        PASS [   0.016s] (51/75) hornvale::suite generated_paths::every_declared_path_names_a_known_author
        PASS [   0.014s] (52/75) hornvale::suite generated_paths::known_authors_agree_with_the_roster
        PASS [   0.188s] (53/75) hornvale::suite docs_consistency::refuted_is_an_admissible_status
        PASS [   0.019s] (54/75) hornvale::suite generated_paths::every_declared_generated_path_is_written_by_its_author
        PASS [   0.127s] (55/75) hornvale::suite docs_consistency::the_ledger_exemption_list_only_shrinks
        PASS [   0.128s] (56/75) hornvale::suite docs_consistency::the_unmatched_plan_count_has_not_moved
        PASS [   0.041s] (57/75) hornvale::suite generated_paths::no_generated_artifact_is_routed_through_a_regenerating_merge_driver
        PASS [   0.043s] (58/75) hornvale::suite generated_paths::the_declared_list_is_not_empty
        PASS [   0.056s] (59/75) hornvale::suite generated_paths::the_root_guide_names_the_declared_path_list
        PASS [   0.058s] (60/75) hornvale::suite generated_paths::no_two_declared_rows_tie_for_precedence_with_different_authors
        PASS [   0.511s] (61/75) hornvale::suite docs_consistency::decision_cites_in_sources_resolve
        PASS [   0.108s] (62/75) hornvale::suite generated_paths::no_claude_md_restates_the_declared_path_list
        PASS [   0.138s] (63/75) hornvale::suite lexicon_guard::the_tokenizer_counts_affixed_bare_and_shouting_forms
        PASS [   0.141s] (64/75) hornvale::suite lexicon_guard::the_inventory_is_sorted_and_not_empty
        PASS [   0.176s] (65/75) hornvale::suite lexicon_guard::a_waiver_needs_a_reason
        PASS [   0.393s] (66/75) hornvale::suite docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary
        PASS [   0.123s] (67/75) hornvale::suite temp_path_ratchet::a_reasonless_roster_entry_is_a_parse_error
        PASS [   0.183s] (68/75) hornvale::suite subfloor_roster_coverage::every_workspace_crate_has_a_roster_entry_or_a_declared_reason
        PASS [   0.330s] (69/75) hornvale::suite repose_byte_identity::no_rendered_artifact_names_a_geohazard
        PASS [   0.219s] (70/75) hornvale::suite temp_path_ratchet::no_new_fixed_temp_path_appears
        PASS [   0.554s] (71/75) hornvale::suite lexicon_guard::no_vertex_sense_cell_comes_back
        PASS [   0.862s] (72/75) hornvale::suite generated_paths::every_declared_generated_path_is_tracked
        PASS [   1.818s] (73/75) hornvale::suite repose_byte_identity::seed_42_almanac_is_unmoved_by_the_repose
        PASS [   3.573s] (74/75) hornvale::suite repose_byte_identity::seed_42_world_json_is_unmoved_by_the_repose
        PASS [   4.137s] (75/75) hornvale::suite repose_byte_identity::seed_42_scene_output_is_unmoved_by_the_repose
────────────
     Summary [   5.049s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-challenge 41ceed7a2] Retain reserved challenge compile and raw observation receipts
 10 files changed, 411 insertions(+)
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/attempt-01-build.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/attempt-02-observation.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/author-interpretation.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/observation.stderr.txt
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/observation.stdout.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/preregistration-hook.txt
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/qualification-console.txt
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/qualification-context.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/qualification-harness.py.txt
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-author/restoration.json
```

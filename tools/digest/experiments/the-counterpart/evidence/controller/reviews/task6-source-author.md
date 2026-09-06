# Reserved source packaging report

DONE: existing reserved challenge packaged as experimental Git source data;
no new build, raw observation or score was taken.

Working directory: `/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker`
Branch: `codex/counterpart-challenge`
Artifact commit: `fa707895e761b944794207180a1218784f591cdd`
Arm ID: `reserved-inferred-owner`
Source object: `b0ae89d8a93cda6fe8932ed24e38db8d64865613`
Source tree: `17905910c5b081c3c706daa0b6e0709f0f3a4432`
Only parent / bundle prerequisite: `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`
Bundle SHA256: `f164f67e4803b522f1a17b70fd7fa61255944d8913be7296576f4483f3d50d9c`
Bundle bytes: 5443
Unchanged patch SHA256: `77fde4719e32393242c057bf3cdbfff9edd34b688ffee18ca8ddc1a280bcedcf`

## Construction and verification

Used the frozen run.py `construct_sources`, `git`, `input_hashes`, and supervised
Git helpers from the controller worktree, verified SHA256
`4890c2185e175a47ad4cd90b7612c4149c93f9efe82b3498f5df281428e08a5f`.
One variant with exactly the two preregistered owned paths; no pairs or other
arms. The helper created its own scratch clone and a commit-tree object whose
single parent is the frozen base. No source changes were made in the author's
campaign checkout.

A separately initialized recovery repository fetched only the base with
`--depth=1 --no-tags`. Its entire initial commit traversal was exactly the base.
The bundle verified there and supplied the reserved object. Its recovered
commit/tree, frozen input hashes and both modified source blob hashes matched
the record and preregistration. A fresh exact patch application at the base in
that recovery repository yielded the identical full tree. Changed paths are
exactly `domains/thing/src/lib.rs` and
`tools/digest/packages/thing/src/lib.rs`.

The extra collector before/after SHA256 and byte sizes are stored separately in
`reserved-source.json`; the frozen arm.inputs list is unchanged and does not
include it. The entire source commit/tree includes the collector change.

All 43 supervised Git commands completed with exit 0, no timeout/output-bound,
interruption or cleanup failure. Their retained stream hashes and lengths were
verified. Total supervised Git elapsed time was 5.666311958339065 seconds.
The existing 3600-second and 16 MiB bounds were used through the frozen helper.
Construction driver, provenance, all samples, recovery proof and console output
are committed under `evidence/reserved-source-construction/`.

One original startup failure is preserved: the first driver passed a Path to
`run.load_json`, which expects raw bytes. This raised TypeError before any Git
command or construction. Only the caller was corrected to read bytes. The
initial driver and full traceback remain retained beside the corrected driver.
No patch, preregistration, source behavior, or frozen code was changed.

## Additional disclosure and scope

After preregistration, read selected run.py helper definitions, imports/constants
and function names for packaging mechanics. Importing the authorized frozen
runner also imports checker/comparator definitions; no scoring functions were
invoked. No panel arms, owner records, unreserved behavioral evidence or other
worker reports were read. No original frozen-checker score was taken. No Cargo,
raw observer, full panel, source enrollment extension, push, remote update or
controller edit occurred. The new bundle/source record remain authorship data;
controller integration and formal acceptance are separate.

## Ordinary commit hook

Working tree is clean after the artifact-only commit. Full hook output follows:

```text
pre-commit: no Rust-relevant paths staged — running the prose-subject tests instead of 'make gate-commit'.
    Finished `test` profile [optimized + debuginfo] target(s) in 0.08s
────────────
 Nextest run ID ea268f2b-70d3-45f8-956e-ca88a4549d1d with nextest profile: default
    Starting 75 tests across 1 binary (246 tests skipped)
        PASS [   0.012s] ( 1/75) hornvale::suite architecture::a_new_concept_and_phenomenon_kind_need_no_god_enum_or_domain_edit
        PASS [   0.012s] ( 2/75) hornvale::suite audio_artifacts::committed_audio_is_exactly_the_page_referenced_set
        PASS [   0.015s] ( 3/75) hornvale::suite architecture::every_client_workspace_declares_an_optimised_dev_profile
        PASS [   0.017s] ( 4/75) hornvale::suite census_duration::a_census_over_the_alarm_threshold_owes_a_profiling_followup
        PASS [   0.010s] ( 5/75) hornvale::suite census_duration::the_chronologically_latest_row_wins_even_when_it_is_not_last_in_the_file
        PASS [   0.013s] ( 6/75) hornvale::suite census_duration::the_census_ledger_has_rows_this_test_can_read
        PASS [   0.015s] ( 7/75) hornvale::suite census_duration::the_latest_census_is_under_the_refusal_ceiling
        PASS [   0.036s] ( 8/75) hornvale::suite architecture::windows_depend_only_on_kernel_domains_and_windows
        PASS [   0.037s] ( 9/75) hornvale::suite architecture::the_layering_page_matches_the_enforced_graph
        PASS [   0.012s] (10/75) hornvale::suite docs_consistency::an_escaped_pipe_is_not_a_column_separator
        PASS [   0.038s] (11/75) hornvale::suite architecture::the_layering_render_is_deterministic_and_grounded
        PASS [   0.038s] (12/75) hornvale::suite architecture::the_kernel_depends_on_no_workspace_crate
        PASS [   0.039s] (13/75) hornvale::suite architecture::external_dependencies_are_allowlisted
        PASS [   0.039s] (14/75) hornvale::suite architecture::domains_depend_only_on_the_kernel
        PASS [   0.010s] (15/75) hornvale::suite docs_consistency::cite_error_resolves_the_known_forms
        PASS [   0.010s] (16/75) hornvale::suite docs_consistency::cite_errors_in_is_case_insensitive
        PASS [   0.019s] (17/75) hornvale::suite docs_consistency::campaign_reconciliation_covers_every_campaign_record
        PASS [   0.011s] (18/75) hornvale::suite docs_consistency::committed_reconciliation_schema_is_parseable
        PASS [   0.014s] (19/75) hornvale::suite docs_consistency::cite_errors_in_catches_line_wrapped_cites
        PASS [   0.024s] (20/75) hornvale::suite docs_consistency::campaign_record_paths_enumerate_the_actual_audit_directories
        PASS [   0.012s] (21/75) hornvale::suite docs_consistency::decision_numbers_are_unique
        PASS [   0.023s] (22/75) hornvale::suite docs_consistency::committed_reconciliation_rows_satisfy_semantic_rules
        PASS [   0.015s] (23/75) hornvale::suite docs_consistency::every_frontier_section_is_listed_in_the_contents
        PASS [   0.048s] (24/75) hornvale::suite docs_consistency::a_decision_records_title_matches_its_filename
        PASS [   0.022s] (25/75) hornvale::suite docs_consistency::every_campaign_with_a_spec_and_a_plan_has_a_ledger
        PASS [   0.015s] (26/75) hornvale::suite docs_consistency::every_refuted_row_cites_its_evidence
        PASS [   0.011s] (27/75) hornvale::suite docs_consistency::reconciliation_parser_keeps_all_five_record_columns
        PASS [   0.017s] (28/75) hornvale::suite docs_consistency::every_registry_table_row_is_a_parseable_id_row
        PASS [   0.017s] (29/75) hornvale::suite docs_consistency::every_registry_row_carries_a_pointer
        PASS [   0.017s] (30/75) hornvale::suite docs_consistency::no_new_numbered_registry_ids
        PASS [   0.010s] (31/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_a_short_row
        PASS [   0.013s] (32/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_terminal_rows_with_a_destination
        PASS [   0.011s] (33/75) hornvale::suite docs_consistency::reconciliation_parser_requires_residue_for_partial_rows
        PASS [   0.012s] (34/75) hornvale::suite docs_consistency::reconciliation_parser_requires_evidence
        PASS [   0.015s] (35/75) hornvale::suite docs_consistency::reconciliation_parser_rejects_unknown_disposition
        PASS [   0.012s] (36/75) hornvale::suite docs_consistency::reconciliation_validation_rejects_duplicate_keys_and_wrong_record_columns
        PASS [   0.016s] (37/75) hornvale::suite docs_consistency::reconciliation_validation_rejects_shipped_registry_targets
        PASS [   0.012s] (38/75) hornvale::suite docs_consistency::refuted_is_an_admissible_status
        PASS [   0.014s] (39/75) hornvale::suite docs_consistency::registry_idea_cells_are_within_budget
        PASS [   0.014s] (40/75) hornvale::suite docs_consistency::registry_ids_are_unique
        PASS [   0.015s] (41/75) hornvale::suite docs_consistency::registry_statuses_use_the_closed_vocabulary
        PASS [   0.012s] (42/75) hornvale::suite docs_consistency::status_normalization_handles_the_documented_forms
        PASS [   0.019s] (43/75) hornvale::suite docs_consistency::registry_rows_have_five_columns
        PASS [   0.011s] (44/75) hornvale::suite docs_consistency::the_confidence_gradient_links_resolve
        PASS [   0.011s] (45/75) hornvale::suite docs_consistency::the_decision_log_starts_at_0001
        PASS [   0.012s] (46/75) hornvale::suite docs_consistency::the_history_page_prose_names_the_vertex_it_renders
        PASS [   0.011s] (47/75) hornvale::suite docs_consistency::the_unmatched_plan_count_has_not_moved
        PASS [   0.015s] (48/75) hornvale::suite docs_consistency::the_ledger_exemption_list_only_shrinks
        PASS [   0.014s] (49/75) hornvale::suite docs_consistency::the_waiver_list_only_shrinks
        PASS [   0.020s] (50/75) hornvale::suite generated_paths::an_overriding_declaration_must_be_measured
        PASS [   0.014s] (51/75) hornvale::suite generated_paths::every_declared_generated_path_is_written_by_its_author
        PASS [   0.011s] (52/75) hornvale::suite generated_paths::every_declared_path_names_a_known_author
        PASS [   0.010s] (53/75) hornvale::suite generated_paths::known_authors_agree_with_the_roster
        PASS [   0.013s] (54/75) hornvale::suite generated_paths::no_generated_artifact_is_routed_through_a_regenerating_merge_driver
        PASS [   0.011s] (55/75) hornvale::suite generated_paths::the_declared_list_is_not_empty
        PASS [   0.018s] (56/75) hornvale::suite generated_paths::no_two_declared_rows_tie_for_precedence_with_different_authors
        PASS [   0.013s] (57/75) hornvale::suite lexicon_guard::a_waiver_needs_a_reason
        PASS [   0.024s] (58/75) hornvale::suite generated_paths::the_root_guide_names_the_declared_path_list
        PASS [   0.013s] (59/75) hornvale::suite lexicon_guard::the_inventory_is_sorted_and_not_empty
        PASS [   0.008s] (60/75) hornvale::suite lexicon_guard::the_tokenizer_counts_affixed_bare_and_shouting_forms
        PASS [   0.177s] (61/75) hornvale::suite docs_consistency::all_knowledge_doc_links_resolve
        PASS [   0.068s] (62/75) hornvale::suite generated_paths::no_claude_md_restates_the_declared_path_list
        PASS [   0.241s] (63/75) hornvale::suite docs_consistency::decision_block_declaration_count_has_not_dropped
        PASS [   0.233s] (64/75) hornvale::suite docs_consistency::decision_blocks_do_not_overlap_across_campaigns
        PASS [   0.192s] (65/75) hornvale::suite docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary
        PASS [   0.015s] (66/75) hornvale::suite temp_path_ratchet::a_reasonless_roster_entry_is_a_parse_error
        PASS [   0.147s] (67/75) hornvale::suite repose_byte_identity::no_rendered_artifact_names_a_geohazard
        PASS [   0.043s] (68/75) hornvale::suite subfloor_roster_coverage::every_workspace_crate_has_a_roster_entry_or_a_declared_reason
        PASS [   0.367s] (69/75) hornvale::suite docs_consistency::decision_cites_in_sources_resolve
        PASS [   0.124s] (70/75) hornvale::suite temp_path_ratchet::no_new_fixed_temp_path_appears
        PASS [   0.358s] (71/75) hornvale::suite lexicon_guard::no_vertex_sense_cell_comes_back
        PASS [   0.567s] (72/75) hornvale::suite generated_paths::every_declared_generated_path_is_tracked
        PASS [   1.403s] (73/75) hornvale::suite repose_byte_identity::seed_42_almanac_is_unmoved_by_the_repose
        PASS [   2.787s] (74/75) hornvale::suite repose_byte_identity::seed_42_world_json_is_unmoved_by_the_repose
        PASS [   3.299s] (75/75) hornvale::suite repose_byte_identity::seed_42_scene_output_is_unmoved_by_the_repose
────────────
     Summary [   3.499s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-challenge fa707895e] Package reserved challenge source object with audited reconstruction
 52 files changed, 2326 insertions(+)
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/attempt-01-startup-failure.txt
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/attempt-02-console.txt
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/audit-summary.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/driver-attempt-01.py.txt
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/driver.py.txt
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/09fcd3ea8cd54b1384e61f3e23a6793e.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/21753b3ee8ba40929f83d341a5bff299.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/228e850ed8b74684bf0ca385c595bf0b.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/2796386f549a4ac8aaa5e6014c1dd388.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/27c3fa98af734afb8107545b0883e9bf.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/2c2a2015bc0d4851bb45beb943bb4543.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/317f4c8ba4624c3c807b5d75e904408c.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/6019e763d35f4173a0f4eccd18d94ce8.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/69bc42803536427cb90944a23bb91163.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/753b20f06b024891b3f25a703195398b.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/75e4f1edc00247eb8a4923505d323ea8.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/761151335e3a47e7bcc131218202e92f.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/7aec9984f732458d92b7d55f463e7dc8.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/8412a9ff1ad54bc6a53705779895fce0.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/86cd8b56cfa84b4399f0f7196679e4d3.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/8875318bbdf54a388d330a22484e1933.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/88f3423f4d574b548835e845157d05a9.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/928bcab9ff81453cb046585f2d3dd396.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/92bb7780f2b7414787b966d9cc40f8ce.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/96008eed6a2c4a7eba8edd00a798ae45.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/9ecb3a3f3ea442a8abca6635d20c0f59.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/a1d5293ceae94fc083714622e7c657c8.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/a28e2221d70942ca8d8278fc2ddbebab.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/a5fdd1fb43594d80acb65a430aed16d3.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/a76d5b0e4fb741a388e70c610bbcdb44.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/a81a935a0ca3432a8bfe6d29a6b6547e.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/a86cc929524845d39c0a38e80f32c495.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/b9b3c7152ca147aaacc645fb22fa6243.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/bb91e8ba386b47e384c9e54f26aa1e94.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/ca68398662c545d1b83498280616a1ac.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/d8ee83262bf948be89dc9c6b13b7dc3a.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/d9dd4f0828e74bb382c1f3f8c5a1a62c.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/da1318ee87154690b8516f1117daf7c3.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/da7b1c63227c4896b68f38173e21c69c.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/e711586f6dc3446eb62b7e700f14bb3c.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/e8904a66dff74d87b016f2b07ab73318.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/f113bc12376e4ab4886be21f97ce86d0.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/f17dc4723ff54387b9d6740a78837348.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/f1d2494510524a499028384f12a4ed5b.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/f38f64a3cdd74224ac0bca1a6533b5f4.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/f8a5c2a4118642df841e205f990f26aa.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/f939c14ca8244fe4a08477dd7851f029.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/git-audit/ff234cb597c2454bbf61461e3d3cb0d0.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/provenance.json
 create mode 100644 tools/digest/experiments/the-counterpart/evidence/reserved-source-construction/verification.json
 create mode 100644 tools/digest/experiments/the-counterpart/reserved-source.json
 create mode 100644 tools/digest/experiments/the-counterpart/specimens-reserved.bundle
```

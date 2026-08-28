# Heavy-tier adjudication

Campaign: The Governor. Task 4a — `windows/hearsay`.

**The rule (spec §3.1, `docs/superpowers/specs/2026-08-28-the-governor-design.md`), quoted verbatim:**

> If this test went red tomorrow, what would we do?
>
> - *"Investigate a regression in the world or the program"* — it is a
>   **witness**. Stays `heavy:`.
> - *"Note that the number moved and update it, because the question this test
>   asked was answered in campaign X and the pin is just the answer we
>   recorded"* — it is a **report**. Demote to `probe:`.
>
> **The burden is on keeping a test in `heavy:`.** A test with no answer to
> the first branch is demoted.

Date: 2026-08-28. Roster read at commit `c8e0b34ff305fe49fc28ca1ad52fe3030d445b01` (branch `campaign/the-governor`).

## Population

Enumerated with:

```
grep -rEA1 '#\[ignore = "heavy:' --include='*.rs' windows/hearsay \
  | grep -oE 'fn [a-z0-9_]+' | sed 's/^fn //' | sort -u
```

23 tests — matches the plan's expected figure; the tree has not moved on this count.

`wall_s` and `last result` come from the two heavy runs on the canonical box
reported into this campaign's context (SHA `2f8faf243`, "main today" in the
spec's §1.1 baseline table). Nine of the 23 were named there with a measured
wall time and a `FAILING` flag. The spec's own §1.1 states the entire 118-test
heavy tier carries exactly **10** failures, of which the tenth
(`the_sub_floor_raider_reading_is_pinned_as_a_witness`) is outside
`windows/hearsay`. Since all 9 named-failing hearsay tests are accounted for
above, the remaining 14 hearsay tests in this population are, by elimination,
currently **green** — no wall time was supplied for them in this task's
context, so their `wall_s`/`last result` cells say so rather than guessing a
number.

## A cross-cutting finding, surfaced during adjudication, not asked for

Six files in this population (`suite/parley_readout.rs`, `undertow_readout.rs`,
`probe_crossing_scale.rs`, `probe_seam_direction.rs`, `probe_tiebreak_rules.rs`,
`probe_argmin_defect_crossing_arms.rs`) all import or duplicate the **same**
`BASELINE_ENDINGS_12` / `BASELINE_FOREIGN_12` / `BASELINE_MUTUALLY_EXCLUSIVE_12`
constants, sourced from `parley_readout.rs`'s committed §3 figures. All six
were already updated once, at commit `44ea8d5a`, when The Underworld changed
settlement placement and the pre-absorption figures (5,913/138/…) went stale
— each file's own comment records this ("RE-DERIVED AGAINST THE MERGE
PRODUCT"). All six carrying this baseline are *also* the same six now marked
`FAILING` in the current run (`parley_readout` itself is not separately
flagged `FAILING` in the supplied wall-time list, but is architecturally
identical and shares the same constants). This is direct, observed evidence —
not a hypothetical — of the exact failure mode spec §3.1's REPORT branch
describes: a live-worldgen substrate number drifts for reasons unrelated to
program correctness, and a human has to "note that the number moved and
update it." It has already happened once to this family and has now happened
again. This is the strongest evidence in the whole population for the
demotions below, and it is also why several of these files' own module docs
independently declare themselves reports (`grep`-verified per row).

## Adjudication

| test | binary | wall_s | last result | verdict | reason |
|---|---|---|---|---|---|
| `cupel_readout` | `cupel_readout` | not measured in supplied data | presumed PASS (not among the 10 known failures) | **KEEP** | Myth campaign 7 (The Cupel), the campaign's headline instrument, still in active use. Its headline finding (`day_tail`) is deliberately **printed, never hard-asserted** — the module doc states this explicitly (controller ruling, pre-unblinding). What *is* asserted: `negative_tail == 0.0`, a provable theorem (a same-people route pays zero penalty under every `PenaltyModel` arm, by construction of `derive.rs`'s `from == to` guard) plus non-vacuity/finiteness controls and one loose `< 0.50` gross-breakage tripwire explicitly labelled "NOT the hypothesis." If `negative_tail` goes non-zero, the crossing-penalty math itself broke — a program regression to investigate, not a number to update. |
| `cupel_substrate_probe` | `cupel_substrate_probe` | not measured | presumed PASS | **DEMOTE** | Myth campaign 7 (The Cupel), Task 0 — a preregistration/viability probe whose own comment says it plainly: *"NOT a hypothesis assertion — this probe informs the freeze, it does not test it… This is a FINDING; record it in the spec and stop, do not build the readout."* The freeze it gates already happened — `cupel_readout` (Task 3) was built and shipped. A red today answers a go/no-go question the campaign already answered; nothing here is a program-correctness invariant. |
| `do_the_filter_keys_vary_between_witnesses_of_one_event_on_seed_42` | `suite` | not measured | presumed PASS | **DEMOTE** | Mechanical pre-filter: zero assertions, file-wide, verified with `grep -n 'assert'` (no hits) and checked for assertion-helper calls in the body (none — every call is a stdlib/domain accessor or `.expect("seed 42 builds")`). Module doc: "Reports only. Asserts nothing about outcomes." The Retelling, substrate probe 2. |
| `does_generation_length_vary_by_people_on_seed_42` | `suite` (`probe_teller_relations.rs`) | not measured | presumed PASS | **DEMOTE** | Explicitly `"SUBSTRATE ONLY"` per its own doc comment, part of Myth campaign 3 (The Palimpsest)'s design-phase probing (see file module doc, campaigns 2–3). Its only assertion is a non-vacuity control (`peoples.len() >= 2`), not a witness of any program behaviour; the interesting content (generation-length distribution) is printed, not asserted. |
| `does_the_crossing_penalty_change_the_non_argmin_defect` | `probe_argmin_defect_crossing_arms` | 1029.3 | **FAILING** | **DEMOTE** | The Undertow (Myth campaign 5), Task 3. Module doc: `"## Reports only, with one exception"` — every assertion is a positive control except one, and that one is explicitly pinned "not because the defect is expected to hold at that exact count forever, but because two independently-restructured instruments landing on the same integer is the strongest evidence…". The same doc documents this file's own substrate having already moved once (The Underworld changed settlement placement) and instructs future readers to "re-measure rather than extrapolate" — the file itself predicts and accepts drift. Currently red, and acceptable to demote red: this is the same drift family described in the cross-cutting finding above (its `BASELINE_ENDINGS_12`/`BASELINE_FOREIGN_12` are the shared, already-once-updated Parley constants). |
| `how_many_generations_does_one_retelling_span_on_seed_42` | `suite` (`probe_teller_relations.rs`) | not measured | presumed PASS | **DEMOTE** | Same file/campaign as the row above. Asserts are non-vacuity guards only (`!amp.is_empty()`, `!life_days.is_empty()`) — they catch "the pipeline produced literally nothing," not a specific regression in program behaviour. The distribution itself (the actual content) is printed, not asserted. Exploratory substrate report feeding Myth campaign 3's design decision. |
| `how_much_the_twelve_seed_prefix_over_reads` | `probe_seam_direction` | 805.3 | **FAILING** | **DEMOTE** | The Undertow (Myth campaign 5). Module doc, verbatim: *"Reports only. Every assertion is a POSITIVE CONTROL — reproducing a published count, or proving the probe reached the population it reports on… No assertion here is about an outcome."* Shares the `BASELINE_ENDINGS_12`/`FOREIGN_12` family (cross-cutting finding above). Currently red; acceptable per the same reasoning — the pin records The Parley's/The Undertow's answer and the world has since moved again. |
| `how_often_does_each_teller_hearer_quadrant_occur_on_seed_42` | `suite` (`probe_lossy_quadrants.rs`) | not measured | presumed PASS | **DEMOTE** | Mechanical pre-filter: zero assertions file-wide (confirmed by `grep`; also one of the two tests spec §2.2 names explicitly as zero-assertion). Module doc: "Reports only. Asserts nothing about outcomes." The Retelling, substrate probe 3. |
| `the_palimpsest_readout_over_a_seed_panel` | `suite` (`palimpsest_readout.rs`) | not measured | presumed PASS | **DEMOTE** | Myth campaign 3 (The Palimpsest)'s preregistered readout (spec §6). Module doc, verbatim: *"The hypotheses are REPORTED. This file must never be edited to rescue a prediction."* and separately "a campaign readout answers a frozen hypothesis once." |
| `the_palimpsest_unit_corrected_exploratory_readout` | `suite` (`palimpsest_readout_units.rs`) | 510.8 | presumed PASS (not flagged FAILING) | **DEMOTE** | Explicitly `"NOT PREREGISTERED"` per its own header — a post-hoc, exploratory follow-up to the row above, written after unblinding to measure one specific unit-conversion defect's consequence. "Every number it prints is exploratory… the hypotheses themselves are REPORTED." Same campaign (Myth campaign 3, Palimpsest erratum). |
| `the_parley_readout_over_a_seed_panel` | `suite` (`parley_readout.rs`) | not measured | presumed PASS | **DEMOTE** | The Parley's preregistered readout (spec §6). Module doc, verbatim: *"is REPORTED, printed against its own §6 decision table. A falsified prediction is a finding. This file must never be edited to rescue one."* Also the **origin** of the `BASELINE_ENDINGS_12`/`FOREIGN_12`/`MUTUALLY_EXCLUSIVE_12` constants copied into five other files in this population (cross-cutting finding above) — the clearest single case of "the pin is just the answer we recorded." |
| `the_undertow_readout_over_a_seed_panel` | `undertow_readout` | 504.6 | **FAILING** | **DEMOTE** | The Undertow's preregistered readout (spec §6). Module doc, verbatim: *"Every hypothesis is REPORTED… A falsified prediction is a finding. This file must never be edited to rescue one."* and its own assertions are headed "SUBSTRATE CONTROLS ONLY (spec §6.4). No hypothesis is asserted" — the file's own authors classify even its shipped-walk-equivalence checks as controls supporting the report, not as the file's reason to exist. That specific invariant (a private probe-only reimplementation of the walk agreeing with the shipped one) is not itself production-consumed the way `traced_walk.rs`'s equivalent check is (see KEEP row below) — it exists only to validate this probe's own numbers. Currently red, and the same `BASELINE_ENDINGS_12`/`FOREIGN_12`/`MUTUALLY_EXCLUSIVE_12` re-derivation (cross-cutting finding) is the most likely cause. |
| `touchstone_controls_probe_negative_population` | `touchstone_controls_probe` | not measured | presumed PASS | **KEEP** | Myth campaign 6 (The Touchstone), Task 1. Asserts `Some(free_claim) == weighted.get(holder)` for every people-homogeneous-ancestry holder on a live 12-seed panel — a **provable theorem** about `derive.rs`'s `crossing_penalty` (`from == to` guard), reproduced on real worlds rather than only the hand-built fixture. The module doc states the theorem was "proven by mutation, not by assertion" (the guard was deleted and this reddened). A red here means the guard itself broke — a program regression, not a stale number. Unlike the `probe_*`/`*_readout` rows above, this file carries **no** blanket "reports only / hypotheses reported" declaration. |
| `touchstone_controls_probe_positive_signature` | `touchstone_controls_probe` | not measured | presumed PASS | **KEEP** | Same task. Asserts `shipped_absent == 0` — the shared enumerator (`tests/common/mod.rs`, also used by `probe_tiebreak_rules.rs`) must contain the shipped walk's own answer on every holder. If this fails, the enumerator and the shipped selection logic (`derive.rs`) have silently diverged: a real bug to investigate, not world drift. (The same test also carries one exploratory, finding-shaped assertion — `best.is_some()`, explicitly annotated "report it and stop, do not lower the floor" — which is report-shaped in isolation; the test as a whole is kept on the strength of `shipped_absent == 0`, which is not.) |
| `touchstone_readout` | `touchstone_readout` | 404.9 | **FAILING** | **DEMOTE** | Myth campaign 6 (The Touchstone), Task 4 — the campaign's headline battery. Module doc, verbatim: *"A falsified prediction is a finding, not a failure (spec §4). If `positive_tail < 20%` … that null is a legitimate headline — nothing here is retuned to rescue it."* It also hard-pins `arm_a_mutex == 10` and `arm_b_mutex == 14`, explicitly labelled as reproducing "Task 1's frozen value" — live-worldgen-derived counts of exactly the kind already shown to drift with unrelated worldgen changes (cross-cutting finding). Currently red; acceptable to demote red because the file's own governing philosophy treats this outcome as a reportable finding, not a defect to chase. |
| `traced_agrees_with_the_shipped_walk_holder_for_holder` | `traced_walk` | not measured | presumed PASS | **KEEP** | Task 2's guard that `traced_variants_about_accumulating` — consumed by `cupel_readout` and `touchstone_readout`, among others — stays byte-identical to the shipped `variants_about_accumulating` across every `Accumulation` × `Contact` × `Crossing` combination, on 4 live-worldgen seeds. Pure implementation-equivalence assertion; no historical magic numbers anywhere in it. If red, the traced module used by multiple live instruments has diverged from the shipped one — an unambiguous program regression. Module doc calls this "the load-bearing one" of its file's two batteries. |
| `what_does_stance_cost_and_how_are_stance_pairs_distributed_on_seed_42` | `suite` (`probe_stance_cost.rs`) | not measured | presumed PASS | **DEMOTE** | Mechanical pre-filter: zero assertions file-wide (confirmed by `grep`; the second of spec §2.2's two named zero-assertion tests). The Retelling, substrate probe 4. |
| `what_scale_is_the_teller_remove_amplitude_on_seed_42` | `suite` (`probe_teller_relations.rs`) | not measured | presumed PASS | **DEMOTE** | Same file/campaign as the two `probe_teller_relations.rs` rows above. Explicitly `"SUBSTRATE ONLY"`: *"Deliberately reports NOTHING about resulting rungs — that is the readout's job."* Asserts are substrate non-vacuity controls only (`zeros < deltas.len()`, `ladder.len() >= 2`). |
| `where_can_a_claim_cross_a_people_boundary_on_seed_42` | `suite` (`probe_filter_mismatch.rs`) | not measured | presumed PASS | **DEMOTE** | Mechanical pre-filter: zero assertions file-wide (confirmed by `grep`; not one of the spec's two pre-named examples, found independently during this task — see report). Own doc, verbatim: "It asserts nothing about outcomes." The Retelling, substrate probe. |
| `whether_the_crossing_penalty_reaches_the_ladder` | `probe_crossing_scale` | 339.8 | **FAILING** | **DEMOTE** | The Undertow (Myth campaign 5). Module doc, verbatim: *"Reports only. Every assertion is a POSITIVE CONTROL — reproducing a [published finding]… No assertion here is about an outcome."* Shares the `BASELINE_ENDINGS_12`/`FOREIGN_12` family (cross-cutting finding). Currently red; source file is also the one whose own doc records the concentration-figure drift the campaign's spec cites ("48% of all crossings at edges = 25" moved after The Underworld changed settlement placement). |
| `whether_the_tiebreak_or_the_contact_pooled_the_accounts` | `probe_tiebreak_rules` | 629.3 | **FAILING** | **DEMOTE** | The Undertow (Myth campaign 5). Module doc, verbatim: *"Reports only. Every assertion is a POSITIVE CONTROL."* Shares the `BASELINE_*` family (cross-cutting finding). Currently red for the same documented reason. |
| `which_teller_event_relations_flip_more_than_once_on_seed_42` | `suite` (`probe_teller_relations.rs`) | not measured | presumed PASS | **DEMOTE** | Same file/campaign as the other three `probe_teller_relations.rs` rows. Directly reproduces "Campaign 2's published ceiling" via `assert_eq!(stance_retained_max, 1, …)`. The file's own history is direct evidence for the REPORT branch: an earlier version of this exact control asserted a different value, went red, and the fix was **to the probe's own reasoning**, not an investigation of a program or world regression — the comment says so explicitly ("That is a correction to this probe's reasoning, not to campaign 2's number"). |
| `which_way_the_account_crosses_the_seam` | `probe_seam_direction` | 306.9 | **FAILING** | **DEMOTE** | The Undertow (Myth campaign 5), same file as `how_much_the_twelve_seed_prefix_over_reads` above, same "Reports only. Every assertion is a POSITIVE CONTROL" module doc, same `BASELINE_*` family. Currently red for the same documented reason. |

## Summary

- **KEEP: 4** — `cupel_readout`, `touchstone_controls_probe_negative_population`,
  `touchstone_controls_probe_positive_signature`,
  `traced_agrees_with_the_shipped_walk_holder_for_holder`. All four assert a
  provable theorem or a cross-implementation equivalence invariant, never a
  historical live-worldgen count.
- **DEMOTE: 19** — the rest. Every DEMOTE row above names the campaign whose
  question the test answered. Nine of the nineteen are currently red; each
  says so and gives the specific, already-precedented reason (a shared family
  of live-worldgen baseline constants — `BASELINE_ENDINGS_12` /
  `BASELINE_FOREIGN_12` / `BASELINE_MUTUALLY_EXCLUSIVE_12`, all sourced from
  `parley_readout.rs` — that has already drifted and been manually re-pinned
  once, at commit `44ea8d5a`, after The Underworld changed settlement
  placement, and has now drifted again).
- **UNDECIDED: 0.**

No fixture was regenerated and no test file, `#[ignore]` string, or code was
modified to produce this table — this task is the adjudication only; Task 5
acts on the DEMOTE rows.

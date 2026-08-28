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

`wall_s` and `last result` for every one of the 23 rows below are read directly
from the heavy run at commit `1710e2f11`, log
`heavy-20260828T162043Z-602166.log`, on the canonical box — **8 FAIL, 15
PASS**, every row a real measured wall time and a real result, none inferred
or estimated. (An earlier draft of this table sourced only 9 of the 23 from a
partial context excerpt and inferred the rest by elimination against the
tier-wide failure count; that inference produced a wrong count — nine where
the truth is eight, one row cited a wall time that belonged to a different
test, and one currently-passing row was mislabelled "presumed PASS" instead
of being read off the log. Corrected here against the full log rather than
re-derived.)

## A cross-cutting finding, surfaced during adjudication, not asked for

Six files in this population (`suite/parley_readout.rs`, `undertow_readout.rs`,
`probe_crossing_scale.rs`, `probe_seam_direction.rs`, `probe_tiebreak_rules.rs`,
`probe_argmin_defect_crossing_arms.rs`) all import or duplicate the **same**
`BASELINE_ENDINGS_12` / `BASELINE_FOREIGN_12` / `BASELINE_MUTUALLY_EXCLUSIVE_12`
constants, sourced from `parley_readout.rs`'s committed §3 figures. All six
were already updated once, at commit `44ea8d5ab`, when The Underworld changed
settlement placement and the pre-absorption figures (5,913/138/…) went stale
— each file's own comment records this ("RE-DERIVED AGAINST THE MERGE
PRODUCT"). All six carrying this baseline are *also* among the tests now
`FAIL`ing in the current run (log `heavy-20260828T162043Z-602166.log`, commit
`1710e2f11`, verified above). This is direct, observed evidence — not a
hypothetical — of the exact failure mode spec §3.1's REPORT branch describes:
a live-worldgen substrate number drifts for reasons unrelated to program
correctness, and a human has to "note that the number moved and update it."
It has already happened once to this family and has now happened again. This
is the strongest evidence in the whole population for the demotions below,
and it is also why several of these files' own module docs independently
declare themselves reports (`grep`-verified per row).

## Adjudication

| test | binary | wall_s | last result | verdict | reason |
|---|---|---|---|---|---|
| `cupel_readout` | `cupel_readout` | 186.835 | PASS | **KEEP** | Myth campaign 7 (The Cupel), the campaign's headline instrument, still in active use. Its headline finding (`day_tail`) is deliberately **printed, never hard-asserted** — the module doc states this explicitly (controller ruling, pre-unblinding). What *is* asserted: `negative_tail == 0.0`, a provable theorem (a same-people route pays zero penalty under every `PenaltyModel` arm, by construction of `derive.rs`'s `from == to` guard) plus non-vacuity/finiteness controls and one loose `< 0.50` gross-breakage tripwire explicitly labelled "NOT the hypothesis." If `negative_tail` goes non-zero, the crossing-penalty math itself broke — a program regression to investigate, not a number to update. |
| `cupel_substrate_probe` | `cupel_substrate_probe` | 270.365 | PASS | **DEMOTE** | Myth campaign 7 (The Cupel), Task 0 — a preregistration/viability probe whose own comment says it plainly: *"NOT a hypothesis assertion — this probe informs the freeze, it does not test it… This is a FINDING; record it in the spec and stop, do not build the readout."* The freeze it gates already happened — `cupel_readout` (Task 3) was built and shipped. A red today answers a go/no-go question the campaign already answered; nothing here is a program-correctness invariant. |
| `do_the_filter_keys_vary_between_witnesses_of_one_event_on_seed_42` | `suite` | 21.036 | PASS | **DEMOTE** | Mechanical pre-filter: zero real assertion-macro invocations, file-wide, verified with `grep -nE 'assert(_eq|_ne)?!'` (no hits — a plain `grep -n 'assert'` is not sufficient evidence, it also matches English prose like "asserts"; see report) and checked for assertion-helper calls in the body (none — every call is a stdlib/domain accessor or `.expect("seed 42 builds")`). Module doc: "Reports only. Asserts nothing about outcomes." The Retelling, substrate probe 2. |
| `does_generation_length_vary_by_people_on_seed_42` | `suite` (`probe_teller_relations.rs`) | 18.531 | PASS | **DEMOTE** | Explicitly `"SUBSTRATE ONLY"` per its own doc comment, part of Myth campaign 3 (The Palimpsest)'s design-phase probing (see file module doc, campaigns 2–3). Its only assertion is a non-vacuity control (`peoples.len() >= 2`), not a witness of any program behaviour; the interesting content (generation-length distribution) is printed, not asserted. |
| `does_the_crossing_penalty_change_the_non_argmin_defect` | `probe_argmin_defect_crossing_arms` | 1029.259 | **FAIL** | **DEMOTE** | The Undertow (Myth campaign 5), Task 3. Module doc: `"## Reports only, with one exception"` — every assertion is a positive control except one, and that one is explicitly pinned "not because the defect is expected to hold at that exact count forever, but because two independently-restructured instruments landing on the same integer is the strongest evidence…". The same doc documents this file's own substrate having already moved once (The Underworld changed settlement placement) and instructs future readers to "re-measure rather than extrapolate" — the file itself predicts and accepts drift. Currently red, and acceptable to demote red: this is the same drift family described in the cross-cutting finding above (its `BASELINE_ENDINGS_12`/`BASELINE_FOREIGN_12` are the shared, already-once-updated Parley constants). |
| `how_many_generations_does_one_retelling_span_on_seed_42` | `suite` (`probe_teller_relations.rs`) | 28.746 | PASS | **DEMOTE** | Same file/campaign as the row above. Asserts are non-vacuity guards only (`!amp.is_empty()`, `!life_days.is_empty()`) — they catch "the pipeline produced literally nothing," not a specific regression in program behaviour. The distribution itself (the actual content) is printed, not asserted. Exploratory substrate report feeding Myth campaign 3's design decision. |
| `how_much_the_twelve_seed_prefix_over_reads` | `probe_seam_direction` | 805.271 | **FAIL** | **DEMOTE** | The Undertow (Myth campaign 5). Module doc, verbatim: *"Reports only. Every assertion is a POSITIVE CONTROL — reproducing a published count, or proving the probe reached the population it reports on… No assertion here is about an outcome."* Shares the `BASELINE_ENDINGS_12`/`FOREIGN_12` family (cross-cutting finding above). Currently red; acceptable per the same reasoning — the pin records The Parley's/The Undertow's answer and the world has since moved again. |
| `how_often_does_each_teller_hearer_quadrant_occur_on_seed_42` | `suite` (`probe_lossy_quadrants.rs`) | 20.583 | PASS | **DEMOTE** | Mechanical pre-filter: zero real assertion-macro invocations file-wide (confirmed by `grep -nE 'assert(_eq|_ne)?!'`; also one of the two tests spec §2.2 names explicitly as zero-assertion). Module doc: "Reports only. Asserts nothing about outcomes." The Retelling, substrate probe 3. |
| `the_palimpsest_readout_over_a_seed_panel` | `suite` (`palimpsest_readout.rs`) | 486.966 | PASS | **DEMOTE** | Myth campaign 3 (The Palimpsest)'s preregistered readout (spec §6). Module doc, verbatim: *"The hypotheses are REPORTED. This file must never be edited to rescue a prediction."* and separately "a campaign readout answers a frozen hypothesis once." |
| `the_palimpsest_unit_corrected_exploratory_readout` | `suite` (`palimpsest_readout_units.rs`) | 510.835 | PASS | **DEMOTE** | Explicitly `"NOT PREREGISTERED"` per its own header — a post-hoc, exploratory follow-up to the row above, written after unblinding to measure one specific unit-conversion defect's consequence. "Every number it prints is exploratory… the hypotheses themselves are REPORTED." Same campaign (Myth campaign 3, Palimpsest erratum). |
| `the_parley_readout_over_a_seed_panel` | `suite` (`parley_readout.rs`) | 467.281 | **FAIL** | **DEMOTE** | The Parley's preregistered readout (spec §6). Module doc, verbatim: *"is REPORTED, printed against its own §6 decision table. A falsified prediction is a finding. This file must never be edited to rescue one."* Also the **origin** of the `BASELINE_ENDINGS_12`/`FOREIGN_12`/`MUTUALLY_EXCLUSIVE_12` constants copied into five other files in this population (cross-cutting finding above) — the clearest single case of "the pin is just the answer we recorded." Currently red — an earlier draft of this table missed this row's wall time entirely; corrected against the full log. |
| `the_undertow_readout_over_a_seed_panel` | `undertow_readout` | 504.562 | **FAIL** | **DEMOTE** | The Undertow's preregistered readout (spec §6). **The demotion's subject is the drifting `BASELINE_ENDINGS_12`/`FOREIGN_12`/`MUTUALLY_EXCLUSIVE_12` pins (cross-cutting finding above), not the file's "substrate controls only" framing** — a control is precisely the kind of thing spec §3.1's witness branch is about, and framing on the preregistration axis rather than the witness/report axis was the wrong test to apply here. The file's own module doc still matters as independent evidence (*"Every hypothesis is REPORTED… A falsified prediction is a finding. This file must never be edited to rescue one."*), but the concrete cost of demoting it is real and is named here rather than argued away: `descent_moved == 0` is computed from **real, shipped** `variants_about_accumulating` calls (via this file's own `shipped()` wrapper, not a probe-private copy) under both `Crossing` arms, over **every** descent-reached holder on every ending of the panel — a general-population invariant. Demoting this test loses that general form. What remains after demotion is `touchstone_controls_probe_negative_population` (KEPT, below), which retains a **strictly weaker** version of the same claim: bit-identical descent claims under both crossing arms, but restricted to the sub-population of **people-homogeneous-ancestry holders only**, not every descent holder. That narrowing is the real, named cost of this demotion. |
| `touchstone_controls_probe_negative_population` | `touchstone_controls_probe` | 281.104 | PASS | **KEEP** | Myth campaign 6 (The Touchstone), Task 1. Asserts `Some(free_claim) == weighted.get(holder)` for every people-homogeneous-ancestry holder on a live 12-seed panel — a **provable theorem** about `derive.rs`'s `crossing_penalty` (`from == to` guard). The sibling test in the same file, `touchstone_controls_probe_negative_theorem` (fast, not `#[ignore]`d, and proven by mutation per its own doc — the guard was deleted and it reddened), already covers this theorem on one hand-built, minimal fixture. What this live-panel test adds is not a new theorem but coverage of the theorem's *precondition* across real, generated ancestry shapes: it asks whether the "people-homogeneous ancestry" sub-population the theorem needs actually recurs, and with what shape, on real worlds — a hand-built fixture cannot exercise the diversity of real founding trees the way a live 12-seed panel does. A red here would mean either the guard broke (the fast test would also catch that) or that this file's own read of "people-homogeneous ancestry" on a real world no longer matches what the theorem assumes — either way, a program regression to investigate, not a stale number. Unlike the `probe_*`/`*_readout` rows above, this file carries **no** blanket "reports only / hypotheses reported" declaration. |
| `touchstone_controls_probe_positive_signature` | `touchstone_controls_probe` | 378.693 | PASS | **KEEP** | Same task. Asserts `shipped_absent == 0` via a hand-rolled `Enumerator` (declared in `tests/common/mod.rs`, shared with `probe_tiebreak_rules.rs`) — the enumerator must contain the shipped walk's own answer on every holder. **The real discriminator, stated explicitly because two DEMOTE rows carry the identical assertion:** `Enumerator` is probe-private — it exists in three places, all under `windows/hearsay/tests/` (`common/mod.rs`, `probe_tiebreak_rules.rs`, `probe_argmin_defect_crossing_arms.rs`), none in `src/` — which is the same "no production consumer" property used to demote `undertow_readout` above. `probe_tiebreak_rules.rs` (DEMOTE) carries the byte-identical `assert_eq!(shipped_absent, 0, …)` control, swept up there under its own "every assertion is a POSITIVE CONTROL" module doc; `probe_argmin_defect_crossing_arms.rs` (DEMOTE) uses the same `Enumerator` for its own least-damage control. So this file is not being kept because its enumerator-agreement control is uniquely load-bearing in kind — it is not; three files share it. It is kept as **the one place this population retains that control at all**: demoting all three would drop the enumerator-vs-shipped agreement check from the tier entirely, with no other test covering it. (The same test also carries one exploratory, finding-shaped assertion — `best.is_some()`, explicitly annotated "report it and stop, do not lower the floor" — which is report-shaped in isolation and does not itself justify KEEP; the verdict rests on `shipped_absent == 0` and on this file being the sole surviving instance of that check.) |
| `touchstone_readout` | `touchstone_readout` | 404.931 | **FAIL** | **DEMOTE** | Myth campaign 6 (The Touchstone), Task 4 — the campaign's headline battery. Module doc, verbatim: *"A falsified prediction is a finding, not a failure (spec §4). If `positive_tail < 20%` … that null is a legitimate headline — nothing here is retuned to rescue it."* It also hard-pins `arm_a_mutex == 10` and `arm_b_mutex == 14`, explicitly labelled as reproducing "Task 1's frozen value" — live-worldgen-derived counts of exactly the kind already shown to drift with unrelated worldgen changes (cross-cutting finding). Currently red; acceptable to demote red because the file's own governing philosophy treats this outcome as a reportable finding, not a defect to chase. |
| `traced_agrees_with_the_shipped_walk_holder_for_holder` | `traced_walk` | 52.556 | PASS | **KEEP** | Task 2's guard that `traced_variants_about_accumulating` stays byte-identical to the shipped `variants_about_accumulating` across every `Accumulation` × `Contact` × `Crossing` combination, on 4 live-worldgen seeds. **The durable reason this matters:** `traced` (`windows/hearsay/src/traced.rs`) is a shipped `src/` module, consumed by `src/touchstone.rs` (the belief-delta instrument itself, `use crate::traced::HeldTelling`) — not merely by test files, several of which (including `touchstone_readout`, demoted above) are themselves adjudicated separately in this table and cannot be cited as the reason to keep this one. Pure implementation-equivalence assertion; no historical magic numbers anywhere in it. If red, a shipped `src/` module consumed by shipped production code has diverged from its sibling — an unambiguous program regression. Module doc calls this "the load-bearing one" of its file's two batteries. |
| `what_does_stance_cost_and_how_are_stance_pairs_distributed_on_seed_42` | `suite` (`probe_stance_cost.rs`) | 25.251 | PASS | **DEMOTE** | Mechanical pre-filter: zero real assertion-macro invocations file-wide (confirmed by `grep -nE 'assert(_eq|_ne)?!'`; the second of spec §2.2's two named zero-assertion tests). The Retelling, substrate probe 4. |
| `what_scale_is_the_teller_remove_amplitude_on_seed_42` | `suite` (`probe_teller_relations.rs`) | 11.802 | PASS | **DEMOTE** | Same file/campaign as the two `probe_teller_relations.rs` rows above. Explicitly `"SUBSTRATE ONLY"`: *"Deliberately reports NOTHING about resulting rungs — that is the readout's job."* Asserts are substrate non-vacuity controls only (`zeros < deltas.len()`, `ladder.len() >= 2`). |
| `where_can_a_claim_cross_a_people_boundary_on_seed_42` | `suite` (`probe_filter_mismatch.rs`) | 15.483 | PASS | **DEMOTE** | Mechanical pre-filter: zero real assertion-macro invocations file-wide, verified with `grep -nE 'assert(_eq|_ne)?!'` (no hits). **Correction to the evidence, not the conclusion:** a plain `grep -n 'assert'` returns one hit here — line 10, the module doc's own prose, "It **asserts** nothing about outcomes" — which is not an assertion, it is English describing the absence of one; a substring grep is not sufficient evidence for this filter (see report). Found independently during this task, not one of spec §2.2's two pre-named zero-assertion examples. Own doc, verbatim: "It asserts nothing about outcomes." The Retelling, substrate probe. |
| `whether_the_crossing_penalty_reaches_the_ladder` | `probe_crossing_scale` | 339.831 | **FAIL** | **DEMOTE** | The Undertow (Myth campaign 5). Module doc, verbatim: *"Reports only. Every assertion is a POSITIVE CONTROL — reproducing a [published finding]… No assertion here is about an outcome."* Shares the `BASELINE_ENDINGS_12`/`FOREIGN_12` family (cross-cutting finding). Currently red; source file is also the one whose own doc records the concentration-figure drift the campaign's spec cites ("48% of all crossings at edges = 25" moved after The Underworld changed settlement placement). |
| `whether_the_tiebreak_or_the_contact_pooled_the_accounts` | `probe_tiebreak_rules` | 629.343 | **FAIL** | **DEMOTE** | The Undertow (Myth campaign 5). Module doc, verbatim: *"Reports only. Every assertion is a POSITIVE CONTROL."* Shares the `BASELINE_*` family (cross-cutting finding) and carries the same `Enumerator`/`shipped_absent == 0` control kept alive by `touchstone_controls_probe_positive_signature` above (see that row's reason). Currently red for the same documented `BASELINE_*` reason. |
| `which_teller_event_relations_flip_more_than_once_on_seed_42` | `suite` (`probe_teller_relations.rs`) | 21.386 | PASS | **DEMOTE** | Same file/campaign as the other three `probe_teller_relations.rs` rows. Directly reproduces "Campaign 2's published ceiling" via `assert_eq!(stance_retained_max, 1, …)`. The file's own history is direct evidence for the REPORT branch: an earlier version of this exact control asserted a different value, went red, and the fix was **to the probe's own reasoning**, not an investigation of a program or world regression — the comment says so explicitly ("That is a correction to this probe's reasoning, not to campaign 2's number"). |
| `which_way_the_account_crosses_the_seam` | `probe_seam_direction` | 306.932 | **FAIL** | **DEMOTE** | The Undertow (Myth campaign 5), same file as `how_much_the_twelve_seed_prefix_over_reads` above, same "Reports only. Every assertion is a POSITIVE CONTROL" module doc, same `BASELINE_*` family. Currently red for the same documented reason. |

## Summary

- **KEEP: 4** — `cupel_readout`, `touchstone_controls_probe_negative_population`,
  `touchstone_controls_probe_positive_signature`,
  `traced_agrees_with_the_shipped_walk_holder_for_holder`. All four assert a
  provable theorem or a cross-implementation equivalence invariant, never a
  historical live-worldgen count.
- **DEMOTE: 19** — the rest. Every DEMOTE row above names the campaign whose
  question the test answered. **8 of the 19** are currently `FAIL` (verified
  against log `heavy-20260828T162043Z-602166.log`, commit `1710e2f11`); each
  says so and gives the specific, already-precedented reason (a shared family
  of live-worldgen baseline constants — `BASELINE_ENDINGS_12` /
  `BASELINE_FOREIGN_12` / `BASELINE_MUTUALLY_EXCLUSIVE_12`, all sourced from
  `parley_readout.rs` — that has already drifted and been manually re-pinned
  once, at commit `44ea8d5ab`, after The Underworld changed settlement
  placement, and has now drifted again). The other 11 DEMOTE rows currently
  `PASS`.
- **UNDECIDED: 0.**

No fixture was regenerated and no test file, `#[ignore]` string, or code was
modified to produce this table — this task is the adjudication only; Task 5
acts on the DEMOTE rows.

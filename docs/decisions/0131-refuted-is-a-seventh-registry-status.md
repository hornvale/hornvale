# 0131. `refuted` is a seventh registry status

**Status:** Accepted (2026-08-13) · **Decider:** Nathan · **Relates to:**
[0016](0016-studies-preregister-hypotheses.md),
[0026](0026-slugs-not-numbers.md),
[0043](0043-numbers-not-slugs.md)

In the context of a project whose method is preregistered falsification and
several of whose campaigns ship a falsified prediction as the headline
finding, facing the fact that the idea registry's status vocabulary — `raw`,
`elaborated`, `spec'd`, `shipped`, `ratified (NNNN)`, `rejected` — has no
token for "tested and found false," we decided that **the vocabulary opens
exactly once to admit `refuted (evidence)` as a seventh status**, accepting
that the vocabulary is closed again immediately after and that this record is
not a precedent for an eighth.

**The distinction, in one line.** `rejected` is a decision — we considered an
idea and set it aside before building anything to test it. `refuted` is a
measurement — we built the thing, ran the study, and reality said no. The two
read alike in casual English and are opposite in kind: one closes a question
by judgment, the other closes it by evidence.

**Why the project needs it.** A grep for falsification vocabulary
(`falsifi|refut|overturn|the null|disconfirm`) anywhere in a registry row
finds 47 candidates, split `raw` 19, `shipped` 13, `elaborated` 11, `rejected`
1, `ratified` 1. Only two carry it in the Status cell itself — one
`shipped (slices 1–2 — falsified twice)` row and one
`shipped (qualified: …)` row. Every other falsification the registry knows
about lives in row *prose*, where nothing greps or counts it as what it is.
The registry's whole reason to exist is to be a scannable, countable index;
a project whose method preregisters a null result and then routinely finds it
should be able to answer "how many predictions did we test and lose" by
reading a column, not by re-reading forty-seven rows of prose.

**The admission rule.** A row takes `refuted` when **the row's own central
claim was tested and found false, and no artifact shipped from it.** A row
that shipped a mechanism while also refuting a prediction about that
mechanism stays `shipped` — a single status token cannot say both "we built
this" and "the thing we thought we'd find wasn't there," and demoting a
shipped row to `refuted` would erase the shipped fact to make room for the
measured one. This is why the audit against the 47 candidates found a small
set: 13 of them are `shipped`, and most of those shipped something real
alongside the falsified prediction.

**The required citation, and why it is stricter than every other status.**
Every `refuted` row's Status cell must carry a parenthetical naming what
refuted it — a campaign name or a decision number, e.g.
`refuted (The Mire)` or `refuted (0117)`, mirroring `ratified (NNNN)`.
`cli/tests/docs_consistency.rs` enforces this with a dedicated test,
`every_refuted_row_cites_its_evidence`, checked only against `refuted` — no
other status in the vocabulary requires a citation to be well-formed. The
asymmetry is deliberate: an uncited `refuted` is an assertion with no way to
check it, exactly the defect `PROC-project-epistemology` names, and a status
whose whole point is "reality said no" is the one place in the registry where
saying so without pointing at the evidence is worse than not saying it at
all.

**What was rejected.** `shipped → refuted`, using the registry's existing
arrow notation for a status transition, was considered as the mechanism and
set aside. `normalize_status` reduces a Status cell to its bare token by
taking the **head** of a `→` arrow —
`normalize_status("rejected → ratified") == "rejected"` is an asserted test
(`cli/tests/docs_consistency.rs`, `status_normalization_handles_the_documented_forms`,
line 396) — so the drift check validates a row's *former* status, not its
current one. An arrow-form `shipped → refuted` row would therefore pass the
closed-vocabulary check by virtue of its `shipped` head while its Status cell
visually asserted `refuted`, silently defeating the citation requirement this
record exists to add. This is a latent defect in `normalize_status` — it is
tail-blind by construction — but fixing it would silently re-validate every
existing arrow row in the registry against today's rules, which is out of
this record's scope; it is captured as a followup rather than fixed here.

**The vocabulary is closed again at seven.** This record opens the
vocabulary once, deliberately, the same way 0026 and 0043 each state a
closed convention for a specific class of identifier. It is not a precedent
for an eighth status; a future need for one is a future decision record, not
an inference from this one.

**Consequence.** `REGISTRY_STATUSES` in `cli/tests/docs_consistency.rs`
widens from `[&str; 6]` to `[&str; 7]`. `book/src/frontier/idea-registry.md`'s
"How to read a row" and `book/src/frontier/CLAUDE.md`'s row-form rule both
list the seventh status and its required citation. `docs/CLAUDE.md`'s
enumerated drift-check list updates its count. A capped reclassification
audit (≤ 12 rows) applies the admission rule to the 47 candidates found by
the falsification grep above; its result is reported in the campaign's
retrospective rather than restated here, since a decision record fixes a
rule, not a rule's one-time application.

**See also.** `book/src/frontier/idea-registry.md` (the row form and "How to
read a row"); `book/src/frontier/CLAUDE.md` (the authoring rule);
`docs/CLAUDE.md` (the drift-check's enumerated assertions);
`PROC-project-epistemology` (the registry row this citation requirement
answers); decision 0016 (studies preregister hypotheses — the practice this
status makes countable).

# Campaign Y2-1 (The Peoples) — retrospective

**Merged:** 2026-07-07

**Recurring findings.** A third implementer evidence-fabrication incident
(Task 3's report claimed a gate result the controller's independent rerun
did not reproduce) triggered a standing process rule: haiku-implementer
gate claims are now void by default, and a controller or reviewer
independently reruns the gate at every haiku task boundary rather than
trusting the self-report. Tasks 4 and 5 separately shipped without their
RED/GREEN transcripts attached to the report (caught and verified
out-of-band both times) — a milder version of the same evidence-discipline
gap. Transcripts-as-evidence, not prose-as-evidence, is the pattern to
carry forward.

**Estimate deltas.** The book close (this task) ran long against a "read
two chronicles and write one" mental model: the freshness sweep alone
touched five living pages once code-vs-prose drift was checked line by
line, and one 10,000-seed census had to be rerun by hand to re-derive
drifted prose numbers rather than trust a stale committed summary.

**Spec vs. reality.** The spec assumed both species place a flagship on
every world; reality delivered bidirectional competitive exclusion (25
kobold-wins, 20 goblin-wins, 3 neither, at 10k) — absorbed as a discovery
about joint greedy placement, not chased as a defect. The byte-identity
contract was restated once already during planning, from strict ledger
identity to a superset (new facts only under new predicates), because
universal `peopled-by` facts are incompatible with strict identity — and
the restated contract still caught a real bug (entity-mint order) in
Task 8, validating the restatement rather than exposing it as too loose.
Two smaller reality-checks: the superset-contract test found a genuine
entity-id drift pre-merge (species genesis needed to run after culture and
religion, not before); and adding a literal role-ladder metric exposed a
latent CSV-writer bug (unquoted commas corrupting rows) that had been
sitting in `write_csv` since before any column needed quoting. A
`serde_json` 1-ULP float round-trip quirk (`str::parse` inverts correctly,
`to_string` does not always) was independently reproduced and worked
around by comparing worlds through the save/load path the CLI actually
uses.

**Do differently next time.** Regenerate author-time (non-CI) census
summaries as part of the task that changes their inputs, not as a
freshness-sweep discovery two tasks later — Study 004's prose drifted
silently for two tasks after the two-species re-baseline already changed
the numbers underneath it.

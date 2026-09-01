# The Hallmark — campaign ledger

Campaign: type-placement criterion, cross-domain consolidation, and the
placement ratchet. Spec: `docs/superpowers/specs/2026-09-01-the-hallmark-design.md`.

#1 [G1] — What justifies moving a type into the kernel, given the coming
Entity-Component layer? · **Re-anchor the gate, don't relax it**: keep 0044's
clauses (a) more-than-one-domain and (b) originates-in-a-kernel-type, add
(c) appears in the wire schema of a component registered for cross-domain
query, and add a stability-graduation modifier (volatile types iterate
domain-side, register their identity early, move when settled) · Why:
decision 0216 already shipped the roster/meaning split ("the kernel holds
the type, the domain holds the meaning"); the ECS metaplan §4.7 puts the
mechanism in the kernel but is silent on wire types, which is the gap
clause (c) closes; registration is a deliberate, contract-bound act, which
answers `DOM-kernel-owns-vocabulary`'s junk-drawer warning structurally ·
Alternatives discarded: (i) proactive placement by anticipation — rejected,
kernel churn is the most expensive rebuild tier and "might be queried
someday" is not a commitment; (ii) keep the count-based gate unchanged —
rejected, it is a lagging indicator once arbitrary cross-domain queries
exist, and waiting for the second domain is how `DelveZone`, `LineSentiment`
and `Stratum` each cost a shim, a mirror test and a later migration ·
Ideonomy: 1 pass (substitution × cross-domain re-instantiation × dictionary;
polarity/materiality/cyclicity axes), which **overturned both starting
framings** — the materiality axis split "type in the kernel" into the
compiled sense vs. the catalog sense, and the DBMS/immunology
re-instantiations both converged on "centralize the frame, not the
content" · Capture: spec §2; `DOM-kernel-owns-vocabulary` to be re-scored at
close; EC wire-type invariant recorded as the EC campaign's hook, not this
one's deliverable.

#2 [Q] — Should the reconsider-on-touch trigger fire on every type edit, or
only on roster-listed (declared-debt) types? · **Roster-listed only**: a
fingerprint mismatch reopens the placement question solely for types already
carrying a `placement:` tag; new twins are caught by the novelty ratchet;
untagged, untwinned types are silent · Why (precedent): the repo's own
ratchet doctrine — seam-guard, `tropes check`, the timings baseline — fails
on novelty, never on existence, because "a gate red on day one is a gate
everyone learns to ignore"; a placement tax on every feature commit is that
failure at commit frequency · Alternatives discarded: fire on every pub type
edit (nag budget exceeded; trains ignoring); fire never, report-only (a
check that never fails is ignored just as fast — same doctrine, other pole)
· Ideonomy: covered by #1's pass (the cyclicity axis is where graduation and
touch-triggered re-measurement came from; the polarity flip surfaced the
nag-budget failure mode) · Capture: spec §3.

#3 [Q] — Does the tool ever decide a placement? · **No — detect and demand,
never decide**: shape identity is not semantic identity (the survey's
rejected near-misses: `RotationRegime` vs `Rotation`, `HabitatRealm` vs
`Realm` are deliberate lossy projections that a shape-matcher cannot
distinguish from forced duplicates), and an auto-promoter would be
self-modifying infrastructure with no human in the loop, the same shape the
board's lane rules forbid · Precedent: 0216's forced-vs-deliberate test
("whether deleting one side would remove an independent answer") requires
semantics; decision 0011's data/code split keeps judgment out of
instruments · Ideonomy: covered by #1's pass (the dictionary organon's
"wire type" / "deliberate projection" entries are where the boundary fell
out) · Capture: spec §3.

#4 [Q] — Is the `f64`-day residue a single migration item? · **No — split
by tag class, verified in source**: `domains/person`'s `PersonSeed` days
carry `waiver(decision-0126: …)` and its doc states the DTO is deliberately
bare (each field becomes a `WorldTime` in `fact()`); `domains/paleoclimate`'s
`day` fields carry `pending(wave-2)` — acknowledged debt. The survey agent
had filed both as residue; reading the source falsified half the claim ·
Why: the verify-the-proposition discipline — a `waiver` and a `pending` are
opposite speech acts and the spec must not migrate a field its own tag
declares deliberate · Ideonomy: 1 micro-check pass on the corrected claim
(inversion: "what would make paleoclimate's ALSO deliberate?" — nothing in
its docs claims a DTO boundary; the fields are stored samples, not
pre-commit staging) · Capture: spec §4, scope note.

#5 [G4] — Task order: ratchet before or after the promotions? · **After** —
Tasks 2-8 delete the six §1 twins, so the tool's baseline tagging pass
(Task 11 Step 2) starts near-empty instead of tagging types the next task
deletes · Why: tag churn is pure waste, and a promote-worthy finding at
baseline becomes a STOP signal (a Batch task missed something) instead of
routine debt · Alternatives discarded: tool-first (spec §3's presentation
order) — would tag six twins and immediately delete them · Ideonomy:
covered by #1's pass (cyclicity: the ratchet's job is FUTURE twins) ·
Capture: plan Tasks 9-11 ordering; this entry.

#6 [Q] — The detector's exact member-set match misses subset mirrors
(Horizon's five inside Stratum's eleven — the very shape Task 7 fixes by
hand) · **Accepted for v1**: the ratchet guards novelty going forward, and
the known subset mirrors are all resolved or adjudicated by this campaign's
own tasks · Why: a subset detector (≥k shared members) is noisy in
proportion to k's arbitrariness, and a noisy gate is an ignored gate (the
repo's own ratchet doctrine) · Ideonomy: 1 micro-pass (inversion: "what
would a subset detector flag today?" — every enum sharing three common
words like North/South; enrichment, not overturn) · Capture: Task 12 may
add a TOOL- row if a real subset mirror recurs; this entry records the
limitation.

#7 [G4] — Plan-time verification falsified two survey claims and one spec
simplification: person's f64 days are waivered-deliberate (ledger #4);
worldgen stores a YEAR in EraClimate.day on the bake path
(windows/worldgen/src/lib.rs:3876, history_bake.rs:1651), so Task 5 is
diagnosis-gated with a STOP branch; Formation's cave variants carry corpus
spellings that genuinely differ from CaveKind's legend (genus_of's own doc),
so Task 8 adjudicates embed-vs-deliberate by evidence · Why: the
verify-the-brief discipline, applied pre-dispatch · Capture: plan Tasks 5
and 8 branch tables.

#8 [Q] — GenesisOutcome<T> renames the payload field (system/globe →
value) at ~15 sites · **Proceed per the approved spec** (§4 item 2 names
the generic explicitly; the rename is the cost of the unification Nathan
approved at G3) · Ideonomy: covered by #1's pass · Capture: plan Task 3.

#9 [Q] — Does Task 2's `UnitError` collapse also fold in the other
non-kernel error shapes turned up nearby? · **Deferred, not consolidated**:
`terrain/src/crust.rs`'s `Result<CrustKm, String>` and
`windows/worldgen/src/harvest.rs`'s `LatError` are error-convention
outliers deferred (the latter dissolves with the queued angle family).
Neither is a shape twin the detector sees. · Capture: Task 2 (`UnitError`
collapse) scope note.

#10 [Q] — Task 5's gate: is `EraClimate.day` one axis (standard days
throughout, merely misleading `year` variable names) or two axes sharing one
slot? · **Two axes — STOP the migration; the retype is blocked on a semantic
repair this campaign did not scope.** The trace, per the brief's deciding
observable: `history_bake.rs:1648` `era_index_for(&self, eras, year: f64)`
compares `e.day <= year` (`:1651`); its only production call site is
`:4357`, inside the epoch loop opened at `:4355` as `let mut year =
cfg.start_year; while year < cfg.end_year { … year += cfg.epoch_years; }` —
so `year` is a bake YEAR (`BakeConfig::default_millennia()` = start `0.0`,
end `2000.0`, epoch `25.0`, `history_bake.rs:903-905`). The `EraClimate.day`
values it is compared against are written by `bake_eras` from that same year
axis: `day: cfg.start_year` (`windows/worldgen/src/lib.rs:3876`, the
constant-sky arm) and `bake_day = cfg.start_year + e*(cfg.end_year -
cfg.start_year)/(CLIMATE_ERAS-1)` (`:3921-3924`). The OTHER producer,
`paleoclimate_from`, writes the same field from `era_day =
-DEEP_TIME_WINDOW_DAYS + e*DEEP_TIME_WINDOW_DAYS/(CLIMATE_ERAS-1)`
(`:3733-3741`), where `DEEP_TIME_WINDOW_DAYS = 1_000_000.0 * 365.25`
(`:3405`) — absolute standard DAYS, matching `IceState.day`'s
`-k*ICE_STEP_DAYS` samples (`:3894-3902`) and the field's own doc,
"Absolute standard day of the era" (`domains/paleoclimate/src/strata.rs:15`).
The project already names this crossing elsewhere and declines to apply it
here: `windows/worldgen/src/history_emit.rs:62`
`ledger_day_of_bake_year(year: f64) -> f64`, whose doc (`:23-31`) states the
rule outright — "The history bake reasons in years and is right to —
`BakeConfig::start_year`/`end_year` stay years … What is constrained is what
*crosses into the ledger*, which is days" · Why STOP rather than retype: each
path is internally consistent, so nothing is broken today, and every
available retype makes it worse. `WorldTime::from_std_days(cfg.start_year)`
would silently reinterpret year 2000 as day 2000 — a 365.25x error where the
bake's era boundaries meet the deep-time axis; converting at the construction
site instead would move the bake's own era boundaries relative to
`era_index_for`'s unconverted `year`, changing behaviour and committed bytes;
converting both is a repair of the bake's whole time axis, not a type
migration. `PaleoRecord.glacial_maximum_day` inherits the ambiguity rather
than escaping it — `strata.rs:126` copies it straight out of a peak era's
`day` — so the committed-ledger surface at `facts.rs:88` cannot be soundly
typed while its source is two-valued · Residual scope, recorded not taken:
`IceState.day` and `integrate_ice`'s `samples: &[(f64, f64)]`
(`ice.rs:54,70`) are unambiguously days on BOTH paths (compared only against
`era_day`, which is days in `bake_eras` too, `:3909-3913`), so a narrower
migration of those two alone is sound. It was not attempted — Task 5's
branch table says STOP the migration, and a partial retype nobody scoped is
not a null result · Capture: idea-registry row `DOM-era-day-axis` (status
`raw`); citations added to the four `pending(wave-2: …)` tag doc lines in
`domains/paleoclimate` (tags themselves untouched, per the brief). No code
retyped; Steps 3-5 (baseline, retype, byte-identity verdict) not reached.

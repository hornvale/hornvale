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

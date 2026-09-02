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
than escaping it — `strata.rs:142` copies it straight out of a peak era's
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

#11 [Q] — Task 8's gate: does `Formation`'s `KarstCave`/`LavaTube`/
`FractureCave` embed into `Formation::Cave(CaveKind)` now that `CaveKind`
moved to the kernel, or stay climate's own projection? · **Stay a
projection — keep the three variants as they are.** The brief's embed
condition is "matched only as enum values and their corpus strings produced
by an explicit spelling table [an embed could preserve]." Neither half of
that held: `grep -rn 'KarstCave\|FractureCave' domains/climate/src/ | wc -l`
returned 4 sites, all a bare enum match (`facets.rs:209,215` the
declarations, `facets.rs:305`'s `unreachable!` arm, `variants.rs:741`'s
`(Formation::KarstCave | Formation::LavaTube | Formation::FractureCave, _)
=> &[]`) — no spelling table converts a `Formation`/`CaveKind` value into a
corpus string anywhere. The corpus strings themselves
(`"karst-cave"`/`"lava-tube"`/`"fracture-cave"`) are hand-authored string
*literals*: `domains/climate/src/axes.rs:288-290`'s three `a("karst-cave",
…)` rows, and `domains/climate/src/underworld.rs:194-198`'s `KARST`/`TUBE`/
`FRACTURE`/`KARST_AND_FRACTURE` genus-pointer constants, referenced by name
across 22 hand-authored `c(…)` corpus rows in `underworld.rs`'s `build()`
(the karst/lava-tube/fracture sections, `underworld.rs:254-613`). An embed
would touch none of that data — the strings are independent of the enum
shape — but it would also gain nothing towards the brief's stated payoff
("keep every emitted string identical" through a single spelling-table
edit), because no such table exists to update; the corpus's spelling is
already decoupled from the variant name and would stay that way either
way. Absent the payoff, the embed is pure churn against a `#[derive]`d
`Formation` used in `BTreeMap` keys and matched exhaustively at several
non-wildcard sites (`facets.rs::biome()`, `variants.rs::variant_pool`),
each of which would need a new nested-pattern arm for no behavioural gain
· Committed-artifact porcelain check (Step 4): `git status --porcelain
book/src/laboratory/generated/ docs/audits/system-coverage-wolverson-2021.md`
— empty, consistent with never having touched the corpus · Action taken:
`Formation::KarstCave`/`LavaTube`/`FractureCave` doc comments
(`domains/climate/src/facets.rs`) reworded to cite `hornvale_kernel::CaveKind`
(post-move) and state the deliberate-projection rationale; `cli/tests/suite/
cave_kind_correspondence.rs` left exactly as the exhaustive-match guard it
already was (decision 0094) · Capture: this entry; no idea-registry row
opened — 0094 already covers the "why a duplicate roster" question and
nothing new was learned about it.

#12 [R] — Task 11 Step 3's decision rule: does placement-audit's warm `check`
land at or under the ~12.8 s threshold (2× type-audit's measured ~6.3-6.4 s),
which decides full commit-gate wiring vs. lane-outboard-only? · **Under —
full commit-gate wiring.** Measured on this Mac, `time cargo run --quiet
--manifest-path tools/placement-audit/Cargo.toml -- check` against the real
tree (post-baseline-adjudication, zero findings): first run 4.882 s, second
(warm) 5.418 s; two further warm runs 4.817 s and 4.483 s — settling in the
~4.5-5.8 s band, comfortably under the ~12.8 s threshold and even under
type-audit's own ~6.3-6.4 s, despite scanning `kernel`+`domains` non-recursively
against type-audit's four roots (the brief's own caveat held: "the
expectation decides nothing; the measurement does" — the smaller-root
expectation and the measurement agreed here, but only the measurement was
load-bearing) · Action taken: `placement-audit`/`placement-audit-report`
Makefile targets added, modeled on the type-audit pair (Makefile ~486-511);
both names added to `.PHONY` and to **both** `quick-run` and `style-run`
prerequisite lists, keeping the four-then-six-name lockstep the brief
required; `cargo build --manifest-path tools/placement-audit/Cargo.toml`
added beside type-audit's in `prewarm-run`; `tools/placement-audit/` (and,
by the same rationale as `type-audit-report.md`'s existing entry, the
generated `docs/audits/placement-audit-roster.md`) added to
`scripts/hooks/pre-commit`'s Rust-relevant staged-path filter;
`run "tools/placement-audit" cargo test --manifest-path
tools/placement-audit/Cargo.toml` added to `scripts/lane-outboard.sh` beside
type-audit's line; `scripts/regenerate-artifacts.sh` Group C gained the
report-emit line directly below the seam-guard roster line, output path
literal; `docs/generated-paths.txt` gained
`docs/audits/placement-audit-roster.md	artifacts` beside the other
individually-declared `docs/audits/` files (tracked-count comment updated
16→17 / 11→12 to match), and the file was `git add`-ed in the same commit as
its declaration · Verified both directions: `make quick` green end to end
(fmt-check, clippy, type-audit, type-audit-report, placement-audit,
placement-audit-report all pass, rc=0); `cargo run --quiet --manifest-path
tools/placement-audit/Cargo.toml -- check tools/placement-audit/tests/
fixtures/verdicts` prints its two known fixture findings and exits 1,
proving the binary `make placement-audit`'s bare `cargo run` recipe calls
fails the target on findings by make's own semantics · `cargo test -p
hornvale --test suite -- generated_paths lane_sets` — 15 passed, 0 failed ·
Capture: this entry; no idea-registry row opened — the decision rule and its
threshold were already spec'd in the brief, and the measurement confirmed
rather than revised it.

#13 [G] — Task 12's capture close-out: re-score `DOM-kernel-owns-vocabulary`
and record the spec's §5 Batch C deferrals · **Flip applied**:
`DOM-kernel-owns-vocabulary` (`book/src/frontier/idea-registry.md:1794`)
`raw` → `ratified (0517)`, Where cell appended (never rewritten) with
[decision 0517](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0517-the-kernel-membership-criterion.md)
and this campaign's spec, matching how `PROC-kernel-units` shows its 0044
ratification · **Four Batch C rows added**, all `raw`, each citing The
Hallmark spec §5 plus file:line evidence: `DOM-two-environment-bases` (the
kernel's `environment_v1_basis()` six-axis basis vs the hard-coded
temperature/moisture/insolation/elevation quartet spelled twice in
`species::ConditionNiche` and worldgen's `Substrate`; flags that
campaign/the-sources is concurrently claiming `ResourceAxis` id 6 and
extending the basis, so any consolidation coordinates with or follows it,
not ahead of it), `DOM-manikin-vectors` (`species::MindVector`/
`SocietyVector` shadowed as bare scalars/bools by `culture::PsychSummary`,
which hard-codes species defaults as literals; candidate kernel citizen on
the `kernel/src/color.rs::Observer` analogy), `DOM-object-property-altitude`
(`ObjectProperty` is window-owned (`windows/vessel`) but hand-mirrored by
`domains/thing` and `domains/language`, whose own doc states it cannot
import the type because layering runs backwards there; candidate promotion
under decision 0517 clause (a)), and `DOM-biome-name-roster`
(`species::BiomeAffinity` string-keys against `climate::Biome::name()` and
silently falls to `default` on a miss; narrow fix a kernel biome-name
roster or `BiomeId` newtype, decision 0517 clause (a) the applicable route
since 0044 fenced "biome" out of the units library) · **Step 3 needed no
action**: Task 5's STOP branch already registered `DOM-era-day-axis`
(`idea-registry.md:1614`) — no duplicate opened; Task 8's Formation
adjudication stayed ledger-only per entry #11 (no descope, so no registry
row) · Verification: `cargo test -p hornvale --test suite -- docs_consistency
2>&1 | tail -3` — `test result: ok. 28 passed; 0 failed` (registry row-form,
status vocabulary, Idea-cell budget, and Where-link resolution checks all
green against the new/flipped rows) · Capture: this entry is the capture —
the chronicle entry, book freshness sweep, and retrospective remain
campaign-close work under the `closing-a-campaign` skill, out of this
task's scope per the plan's Interfaces note.

#14 [G6] — Nathan's rulings at the merge stop (2026-09-02), overriding two
execution-time adjudications: (1) the `EraClimate.day` two-axes defect
(ledger #10, `DOM-era-day-axis`) is fixed IN THIS CAMPAIGN — the bake path
converts years to days at the named crossing so the field holds one axis,
then the blocked `WorldTime` migration proceeds; (2) `Formation`'s cave
half (ledger #11) is UNIFIED with the kernel's `CaveKind` — structurally,
as `Formation::Cave(CaveKind)` with an explicit spelling map preserving the
frozen corpus strings ("karst-cave"/"lava-tube"/"fracture-cave"); committed
bytes still may not move without escalation · Why: decider's call at G6 —
the deferrals were controller adjudications, not spec constraints, and the
decider values the unification and the axis repair over the deferral's
safety margin · Capture: plan addendum Tasks 13-15; `DOM-era-day-axis`
re-scored at close.

#15 [T13] — How should the bake path stop writing bake YEARS into
`EraClimate.day`, a paleoclimate field whose contract is absolute standard
days (ledger #10's defect; ruling #14 orders it fixed now)? · Decision: the
bake path keeps its year axis in a **bake-owned parallel vector**, and
`EraClimate.day` takes the era's true deep-time day on both producers.
`bake_eras` returns a third `Vec<f64>` of bake years beside the existing
`Vec<EraClimate>` and `Vec<EraAdjust>`; `history_bake::bake` takes it as a
new `era_years: &[f64]` argument with a length assert; `Bake::era_index_for`
compares that slice against the epoch loop's `year`, so both sides of its
`<=` are bake years. `EraClimate.day` on the forced arm becomes the
`era_day` the ice lookup in the same loop already ran against — the
identical `-DEEP_TIME_WINDOW_DAYS + e * DEEP_TIME_WINDOW_DAYS /
(CLIMATE_ERAS - 1)` expression `paleoclimate_from` uses — and `0.0` (the
present) on the constant-sky arm, replacing `cfg.start_year` · Why: the
consumer table decided it. Bake-path `EraClimate.day` has exactly two
readers, both inside `bake`: `era_index_for`'s comparison against a bake
year, and the `min_by` that picks the oldest era. No bake-path value reaches
`strata::extract`, `facts.rs`, the ledger, or any artifact — `extract` is
only ever called on `paleoclimate_from`'s day-valued series. So the only
reader that needs a UNIT needs years, and the only other reader needs an
ORDERING. Converting the field to days and converting the comparand back
(the `day_of_bake_year` sibling-crossing shape ruling #14's wording
anticipates, and which the brief lists as its option (a)) would have been a
smaller diff, but it buys unit-consistency by storing `bake_year × 365.25`
— the bake's year axis in a day costume. A reader would then find an era
stamped "day 91312.5 after genesis" that is physically a glacial state
750,000 years BEFORE genesis: single-axis in unit, still false in referent.
It also needs a floating-point monotonicity argument (multiplying both sides
of a `<=` by a constant is non-decreasing but not injective on doubles, so
`x > y` collapsing to `f(x) == f(y)` is a real, if unreachable, failure
mode) · Selection semantics: preserved BY CONSTRUCTION, not by argument.
`era_years[e]` holds the identical `cfg.start_year + e * (end - start) /
(CLIMATE_ERAS - 1)` f64 the field used to hold, unconverted, and `year` is
untouched, so every `<=` outcome — exact-equality grid alignments included
(e.g. era 3 at exactly `250.0` against the epoch loop's exactly `250.0`) —
is bit-for-bit what it was. The `min_by` is an ordering read whose numeric
value is used for nothing else: bake days ascend (most negative first) where
bake years ascended, `min_by` returns the first minimum, so it selects
`eras[0]` before and after · Constraint 4 (comparator twins) not engaged:
neither `strata::extract`'s peak comparator nor worldgen's `:3763-3764` twin
was touched · Verification: seed-42 world byte-IDENTICAL (sha256
`e70ca3d0d782f095ded80071bbafe11e64f19b61ecffa127414ed5a57e9970ef` before
and after); `cargo nextest run -p hornvale-worldgen -p hornvale-paleoclimate`
— 822 passed, 0 failed; `git status --porcelain book/ clients/` empty ·
Capture: this entry; the `DOM-era-day-axis` registry row re-scored to
`shipped`; the `EraClimate.day` / `glacial_maximum_day` / `IceState.day`
doc comments corrected. The `pending(wave-2: day)` type-audit tags are
deliberately UNCHANGED — the blocker is discharged, the `WorldTime` retype
is Task 15's job and this task retyped nothing.

#16 [T14] — Executing ruling #14's second half: `Formation`'s cave half
(ledger #11's projection) is UNIFIED with the kernel's `CaveKind`
structurally, as `Formation::Cave(CaveKind)`, replacing the three-variant
`KarstCave`/`LavaTube`/`FractureCave` roster · Action taken:
`domains/climate/src/facets.rs` — the three variants collapse into one
`Cave(CaveKind)` tuple variant, doc-commented to cite decision 0517 clause
(a) and this ruling, superseding ledger #11's projection rationale, and to
state explicitly that the frozen corpus spellings
(`"karst-cave"`/`"lava-tube"`/`"fracture-cave"` in `axes.rs`/
`underworld.rs`) are freestanding literals with zero linkage to the enum
and do not move; `use hornvale_kernel::CaveKind;` added. Every grouped arm
the compiler named followed: `facets.rs`'s `biome()` `unreachable!` arm,
`variants.rs:744`'s empty-pool arm, `windows/worldgen/src/lib.rs:677-679`'s
`BiomeClass::Barren` arm, `windows/locale/src/surface.rs:148-150`'s `0.0`
arm — all four collapse their three-variant patterns to
`Formation::Cave(_)`. The compiler surfaced no other site: a full
`cargo build --workspace --all-targets` after the edit found nothing
further to fix · Step 3 (the correspondence test): DELETED
`cli/tests/suite/cave_kind_correspondence.rs` (and its `mod` wiring in
`cli/tests/suite.rs`) with a pointer to
`windows/worldgen/src/delve_seating.rs`'s `every_cave_kind_matches_a_corpus_genus`
and `the_genus_extends_the_cave_kinds_own_name` tests in the commit
message. Judgment: the deleted test asserted only that `CaveKind`'s three
values map to three *distinct* `Formation` values — a claim the embed now
makes structurally true (two different `CaveKind`s wrapped in the same
`Formation::Cave` variant are unequal by construction), so the test had
become an assertion about the type system rather than about the program.
It never touched the corpus-spelling join (`genus_of`), which operates on
`CaveKind` directly and was untouched by this change; that join's coverage
— every `CaveKind` reaches a genus string (`genus_of`'s exhaustive, no
wildcard, match), every emitted genus occurs in the corpus
(`every_cave_kind_matches_a_corpus_genus`'s `rows > 0` per kind), and the
mapping is not transposed (`the_genus_extends_the_cave_kinds_own_name`) —
already pinned the map in both directions asked about, independent of
`Formation`'s shape, both before and after this task · Lexicon check:
`book/src/reference/lexicon-of-place.md` names none of the three variants
(`grep` empty), so nothing to update there and `docs_consistency` needed
no re-run beyond the full suite pass below · Verdicts (Step 4): `git status
--porcelain book/src/laboratory/generated/
docs/audits/system-coverage-wolverson-2021.md clients/` — empty;
`cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml --
check` — exit 0 (`Formation` and `CaveKind` still differ in member sets:
`Formation` carries 18 other variants beside `Cave` (19 total), so embedding
one inside the other does not make the two enums shape twins) · Verification:
`cargo build --workspace --all-targets` clean; `cargo fmt --check` and
`cargo clippy --workspace --all-targets -- -D warnings` clean; `make
quick` rc=0; `cargo run --manifest-path tools/type-audit/Cargo.toml --
report` and the `placement-audit` report both diffed byte-identical
against their committed artifacts (no new bare primitive at the pub
boundary — `CaveKind` is a typed enum, not a primitive); `cargo nextest
run -p hornvale-climate -p hornvale-worldgen -p hornvale-locale -p
hornvale-vessel` — 1916 passed, 0 failed, 121 skipped; `cargo nextest run
-p hornvale` (the workspace-wide enforcement suite, including
`architecture`, `docs_consistency`, `generated_paths`) — 414 passed, 0
failed, 11 skipped; doctests for all four scoped crates — 0 tests, all
green (none carry doc examples) · Capture: this entry.

#17 [T15] — Task 15: retype `EraClimate.day`, `PaleoRecord.
glacial_maximum_day`, `IceState.day` and `integrate_ice`'s `samples` to
`hornvale_kernel::WorldTime`, unblocked by Task 13's source fix (ledger
#15) · Action taken: `domains/paleoclimate/src/strata.rs` —
`EraClimate.day: WorldTime`, `PaleoRecord.glacial_maximum_day: WorldTime`;
the peak comparator's `b.day.total_cmp(&a.day)` becomes `b.day.cmp(&a.day)`
(`WorldTime` derives `Ord`); the `None` sentinel becomes
`WorldTime::GENESIS` (the same committed value, 0.0 days, by construction);
both `pending(wave-2: day)`/`pending(wave-2: glacial_maximum_day)` tags
removed (the fields are no longer bare primitives) · `domains/paleoclimate/
src/ice.rs` — `IceState.day: WorldTime`; `integrate_ice(samples: &[(f64,
f64)])` becomes `&[(WorldTime, f64)]`; `(day - p) / DAYS_PER_KYR` becomes
`(day - p).as_std_days() / DAYS_PER_KYR` (`Sub` yields `TickSpan`); the
struct's `pending(wave-2: day)` tag is deleted outright (no bare primitive
remains on `IceState`) and `integrate_ice`'s `pending(wave-2: samples)`
becomes `bare-ok(ratio: samples)` (the tuple's second element, the caloric
index, stays a bare dimensionless ratio — `type-audit check` demanded
exactly this after the edit, confirming the classification rather than
assuming it) · `domains/paleoclimate/src/facts.rs` —
`Value::Number(record.glacial_maximum_day)` becomes
`Value::Number(record.glacial_maximum_day.as_std_days())`, the same f64
back out · `windows/worldgen/src/lib.rs` — every construction site converts
once at the crossing with `WorldTime::from_std_days(_).expect("era day
within tick range")`: `climate_at_era`'s `EraClimate` (from `EraInputs.day`,
which stays a private, untyped worldgen-internal field — out of the
brief's scope), both ice-sample-builder loops (`paleoclimate_from` and
`bake_eras`), and `bake_eras`'s forced-arm `EraClimate`; `bake_eras`'s
constant-sky arm's `day: 0.0` becomes `WorldTime::GENESIS`; the worldgen
twin of the peak comparator (`eras[j].day.total_cmp(&eras[i].day)` inside
`paleoclimate_from`'s glacial-maximum selection) becomes `.cmp(&...)`,
matching `strata.rs`'s comparator exactly, as the twin's comment requires ·
`windows/worldgen/src/history_bake.rs` — the `earliest` era's
`a.day.total_cmp(&b.day)` becomes `a.day.cmp(&b.day)`; every test-fixture
`EraClimate { day: 0.0, .. }` (6 sites) becomes `day: WorldTime::GENESIS`;
the two `|day: f64| EraClimate { day, .. }` closures (3 sites: 2 identical
two-argument forms plus one one-argument form) and the `era_at(day: f64)`
helper wrap with `WorldTime::from_std_days(day).expect(...)` · Test
fixtures elsewhere followed the compiler the same way:
`windows/worldgen/tests/suite/history_bake.rs` (9 sites: 6 literal
`day: 0.0`, 3 closures) and `domains/paleoclimate/tests/paleo_properties.rs`
(the `series` helper's return type and the sawtooth test's era-shift, which
needed `TickSpan::from_std_days` since `WorldTime` has no `Add<f64>` — only
`Add<TickSpan>`) · **The nearest-sample search form chosen: f64 standard
days, not `TickSpan`s.** `paleoclimate_from` (~lib.rs:3736) and `bake_eras`
(~lib.rs:3933) each pick the ice-history sample nearest an f64 `era_day` via
`min_by` on `|history sample day − era_day|`. Converting `era_day` to
`WorldTime` and comparing `TickSpan`s was the alternative; f64 was chosen
because it keeps the SAME doubles being compared as before this task's
edit — `a.day` was already an f64 field before the retype, and
`a.day.as_std_days()` round-trips it losslessly at every magnitude this
window ever samples. **That states the safe direction only** (ticks→days
is exact below ~2.47e8 years; the deep-time window here is 1 Myr) — the
real invariant this argument leans on is the OTHER direction: `f64`→ticks,
which the `WorldTime::from_std_days` calls building `samples` and
`era_day`'s comparands perform, always ROUNDS, and is a no-op here only
because every era day this window constructs lands on an exact whole-day
integer (`ICE_STEP_DAYS = 730500.0`; `era_day = -365_250_000 + e ×
15_218_750` for `e` in `0..CLIMATE_ERAS`). So nearest-selection is
bit-for-bit identical by construction, not merely argued to be — but a
future non-integral step (a fractional `ICE_STEP_DAYS`, say) would need
this argument re-verified, not assumed to still hold. The seed-42 diff is
the check on that argument, not a substitute for it, and it came back
identical (below)
· Verdicts: seed-42 world before/after — `sha256sum` **identical**,
`e70ca3d0d782f095ded80071bbafe11e64f19b61ecffa127414ed5a57e9970ef` both
sides, matching ledger #15's own recorded hash (the tree's only paleoclimate
state has not moved since Task 13) · `cargo build -p hornvale-paleoclimate
-p hornvale-worldgen -p hornvale --tests` clean, no warnings · `cargo fmt`
clean · `cargo clippy --workspace --all-targets -- -D warnings` clean ·
`cargo run --manifest-path tools/type-audit/Cargo.toml -- check` rc=0 (no
stale tag positions, no missing tags) · `cargo run --quiet
--manifest-path tools/placement-audit/Cargo.toml -- check` rc=0 · `cargo
nextest run -p hornvale-worldgen -p hornvale-paleoclimate --no-fail-fast` —
822 passed, 0 failed, 115 skipped · `cargo nextest run -p hornvale -p
hornvale-lab --no-fail-fast` — 917 passed, 0 failed, 45 skipped · `cargo
test -p hornvale --test suite -- docs_consistency` — 28 passed · type-audit
report regenerated (`docs/audits/type-audit-report.md`: `pending` 311→307,
`bare-ok(ratio)` 671→672, `paleoclimate` row 26/0/9/35 → 27/0/5/32,
`wave-2` 105→101) · `git diff --exit-code` against
`docs/generated-paths.txt`'s declared paths shows only the type-audit
report moving; `git status --porcelain book/src/laboratory/generated/
book/src/domesday/ clients/` empty · Docs updated: `strata.rs`/`ice.rs`
field and struct docs re-worded from "the retype has not been done yet" to
recording Task 15's completion; `DOM-era-day-axis`'s Where cell (idea
registry) now cites this entry and states the retype is done · Capture:
this entry.

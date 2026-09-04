# The Housemark — decision ledger

Campaign: **The Housemark** — The Staple's R1, “a dwelling belongs to its
people.” Branch: `campaign/the-housemark`. Decision block: **0746–0755**.

## Entries

#1 [G1] — **How should a dwelling acquire a legible cultural signature?** ·
**Decision: derive one small housemark signature from the occupying people's
existing `SocietyVector`, then use it to admit culturally diagnostic relational
patterns from the one shared `Pattern` inventory. Universal necessities remain
universal.** · Why: The Hearth §6 already fixes the architecture: one authored
inventory, per-culture derived selection, and one validator, transposed from
phonology. The Staple measured `Brief::people` as the only rich living-place
axis already carried to the vessel and read nowhere. The live society registry
contains fifteen peoples, fifteen distinct full vectors, and all six categorical
`Sociality × StatusBasis` cells, so it supports both variation and compression
without branching on species ids. The cultural signal must live in relations
(what is screened, central, shared, enclosed, or displayed), not in a catalogue
of fifteen room templates or decorative labels. · Alternatives discarded:
(a) a `KindId -> Vec<Pattern>` table — directly violates decision 0021's
anti-lookup-table discipline and makes every new people an architecture code
edit; (b) derive fifteen unique complete inventories — confuses distinguishable
with one-to-one and authors rooms rather than a language; (c) make
`SocietyVector` rewrite `Role` — role says what a chamber is for, while culture
says how that purpose is arranged, so the axes compose rather than replace one
another; (d) change locale-band selection — it feeds committed thermal history,
while R1 is explicitly the chamber-band, no-epoch reading rung. · ideonomy
passes / overturns: **2 passes, no overturns, two material additions.** Pass 1
(organon-construction, tree-finding, cross-domain re-instantiation; periodic
grid + matrix) separated universal substrate from diagnostic accents and exposed
the six-cell categorical matrix as the compression boundary. Pass 2 (negation;
animacy + age) exposed the temporal limit: this is the living occupier's
housemark, not an eternal species essence, and it does not preserve a previous
people's style in ruins. · Capture: this entry; the design's architecture,
non-goals, and preregistered distinguishability claim; the browser companion at
`.superpowers/visualizations/housemark-design.html`.

#2 [Q] — **Which society axes should architecture read, and how should they be
represented?** · **Decision: read `sociality` as an authority mark and
`in_group_radius` as a three-band threshold posture; represent them as two
independent fields, not one six-case cultural type. Do not read `status_basis`
in this campaign.** · Why: authority has a direct spatial expression in
seating — `Hierarchic` admits the existing command seat, while `Communal`
admits one new common bench. The documented radius already means how widely
“us” is drawn, so it directly supports an inward threshold (screen), an
unmarked middle, and an outward threshold (guest water). Bands `≤ 0.35`,
`0.5..=0.6`, and `≥ 0.65` contain all fifteen current peoples and make all six
cross-product cells non-empty; there are no current values in the gaps. The
gaps remain deliberately unclassified rather than silently rounded: adding a
people there must force a design choice. `status_basis` produced no equally
direct chamber relation — mapping Knowledge to a loom or Generosity to a jar
would turn a social value into an occupational stereotype — and a consumer is
not required to consume every field it can see. · Alternatives discarded:
(a) one enum with six variants — freezes today's cross-product and couples two
independent axes; (b) thresholding at the neutral midpoint only — gives two
postures and erases the roster's real middle cluster; (c) using exact floating
values as style identities — makes all fifteen rows distinct on paper while
providing no architectural meaning; (d) consume `status_basis` for symmetry —
symmetry is not evidence. · ideonomy passes / overturns: **1 pass, 1 material
improvement** (substitution; periodic grid; modularity and
discovery-vs-invention). It replaced the proposed six-case enum with two
recombinable fields and surfaced the band thresholds as newly authored policy
whose gaps must fail rather than round. · Capture: this entry; the design's
type/API section and six-cell acceptance probe.

#3 [G2] — **Does the design remain observable across the structure-size
distribution?** · **Decision: both housemark axes must appear in the threshold
chamber, the only chamber every structure owns.** · Why: the design draft put
the communal bench in `Role::Hearthroom`, assuming the first two rooms existed.
Reading `Structure` during self-review showed `chambers.len()` is
`1..=MAX_CHAMBERS`; only `chambers[0]`, the threshold, is guaranteed. A
one-chamber communal dwelling would therefore lose its authority mark, and a
fixture fixed at two rooms could hide the defect. The bench moves beside ground
in `Role::Threshold`; the command seat already belongs there. · Alternatives
discarded: require two chambers (changes the causal structure draw and is R2,
not R1); repeat the mark in every chamber (turns a signature into wallpaper and
can duplicate kinds); accept partial observability in small houses (fails the
headline on a legal production structure). · ideonomy passes / overturns:
**1 pass, 1 correction** (tree-finding; chart; distribution and size). The size
substitution exposed the missing one-chamber leaf. · Capture: corrected spec
§3 and H2/H3; corrected visual matrix.

#4 [G4] — **Does the implementation plan faithfully and executably cover the
approved design?** · **Decision: execute the five-task plan at
`docs/superpowers/plans/2026-09-04-the-housemark.md`: pure derivation, shared
inventory admission, production `Brief` wiring, living five-seed readout, then
artifact/document closure.** · Why: every preregistered claim H1–H4 has a named
test surface; the locale/epoch boundary is tested before the chamber signature
is admitted; fallibility is propagated at the production boundary; and the
artifact step is a response table over observed paths rather than a prediction.
The tasks follow the existing vessel split and keep the one-inventory
architecture binding. · Alternatives discarded: combine all implementation
into one task (too broad for meaningful review); put the five-seed proof in the
lab (the observable belongs to the vessel and is not a census golden); defer
artifact classification to close (would let an epoch misclassification survive
the implementation). · ideonomy passes / overturns: **1 pass during G4
self-review, no overturns, one material sharpening** — tree-finding across the
plan's intentional and inherited branches exposed the old locale-visible screen
as a separate-age constraint: its one row may narrow at the chamber band only
because locale selection explicitly ignores `HousemarkGate`; the inherited
locale output itself may not narrow. · Capture:
this entry; the committed
plan; the temporary repository-required `IMPLEMENTATION_PLAN.md` stage tracker.

#5 [G5] — **Does Task 2 append four cultural rows or reuse the existing
screen?** · **Decision: gate the existing screen row as `Threshold(Inward)` at
the chamber band and append exactly three new rows.** · Why: approved spec §3
explicitly says the screen is reused and its locale admission remains
unchanged; §4 supplies the mechanism because locale `selection` ignores
`HousemarkGate`. The plan's earlier “append four” imperative contradicted both
clauses and would duplicate `SCREEN` in inward threshold compositions. ·
Alternatives discarded: append a second screen row (breaks H2's no-duplicate-
kind claim); leave the old row universal in chambers (all three threshold
postures would read inward). · ideonomy passes / overturns: **1 prior G4 pass,
no new pass** — this is correction of the exact inherited-screen implication
that pass surfaced, not a new question. · Capture: corrected plan before Task 2
dispatch; this ruling; scratch pre-flight ledger.

#6 [Q] — **Does the new `BENCH` kind take the observed concept-accession
epoch?** · **Decision: yes; take concept accession epoch 20 and re-pin the
additive world and language-root artifacts.** · Why: the Task 2 reviewer ran
the world-golden seam and measured the serialized `ConceptRegistry` growing
from 5,587,823 to 5,587,963 bytes at the new `bench` entry. Decision 0648 says
registering a Thing with identity is a concept-accession epoch, and decision
0618 records `door` paying the same cost as epoch 19. Nathan approved the epoch
explicitly on 2026-09-04. · Alternatives discarded: reuse an existing kind
(would erase the common-seat/command-seat distinction the approved design
needs); leave the world golden stale (turns an observed contract change into a
permanent red); add or bump a seeded stream (no draw moved, so that would be a
false hierarchical escalation). · ideonomy passes / overturns: **1 pass, no
overturns** (negation on reversibility and hierarchicalness; scale). It placed
the choices from reversible chamber-only reading through additive registry
accession to derivation-stream replacement; the measured change sits at the
additive accession point, one level below any stream epoch. · Capture: revised
spec §7, plan constraints/artifact table, this ledger entry; Task 5 will author
the binding decision record and re-pin exact observed artifacts.

#7 [Q] — **Should H3 accept the current lossy occupant surface or repair the
production lookup?** · **Decision: repair the lookup by indexing living
occupations with the exact production settlement-room address; H3 covers every
distinct player-addressable built settlement room.** · Why: the diagnostic
enumerated 1,275 live `(vertex, rung)` occupation records and measured 1,259
distinct rooms. Only 744 addresses reverse through `containing_vertex` to their
source vertex; 531 resolve to a direct neighbor, yielding 222 rooms with no
people and 41 with the wrong people. The existing warning in `brief.rs` already
states the cube-sphere facet and icosphere vertex meshes are not inverses.
Accepting only today's 1,037 inhabited outputs would canonize that known lossy
projection and contradict the campaign's purpose. Sixteen duplicate rooms are
intentional surface/subterranean coexistence; one player-addressable room can
expose one occupation, selected in the same deterministic first-settlement
order already used for its name. · Alternatives discarded: redefine “living”
as whatever the broken lookup happens to expose (hides 222/41 defects); require
1,275 distinct dwellings (the product has 1,259 addresses, so sixteen cannot be
expressed without an R2 topology/rung campaign); silently filter mismatches
(invalidates preregistration). · ideonomy passes / overturns: **1 pass, no
overturns, one clarification** (organon-construction; notation over purpose and
side effects). Writing the flow as `settlement -> room -> brief` made the
reverse `room -> nearest vertex -> occupation` an inexpressible extra leg and
separated its hidden neighbor-substitution side effect from the intended
reading. · Capture: revised spec §4/§5/H3, revised Task 4, this entry; the raw
1,275-row diagnostic remains in the task report.

## Implementation reviews

### Task 1 — complete

The pure derivation shipped after a clean review. Boundary tests cover all six
inclusive band endpoints, both open gaps, and values outside `[0, 1]`; errors
retain the rejected `f64`. The live registry test derives all fifteen rows,
populates all six housemark classes, and freezes coverage properties rather
than proper-name membership. No new ruling was needed beyond #1 and #2; their
ideonomy passes already cover the implemented shape.

### Task 2 — complete after one fix round

Review found two Important guard gaps and the epoch boundary. The copied
six-case H2 fixture became a generated authority/posture cross-product with an
exact six-combination ratchet, and name-only assertions became checks over the
selected pattern's kind, attachment, and prerequisite. A mutation of the
command-seat attachment produced the intended semantic RED. The existing
screen row is chamber-gated to `Inward`; locale selection still ignores that
gate, and exactly three cultural rows were appended with `at_locale: false`.
The measured `BENCH` registry movement was accepted as concept accession epoch
20 under #6. No further finding remained after the fix review.

### Task 3 — complete after one fix round

Review found that `Session::go` could charge time, extend the trail, and commit
the destination before a fallible arrival brief was validated. The fix derives
and validates the destination brief before the first mutation, then renders
from that exact successful value. A production-path regression also proves an
unoccupied brief carries neither people nor a MANIKIN-derived housemark. The
review rerun was clean; this preserves #1's explicit `None` boundary.

### Task 4 — complete after one design correction and one fix round

The preregistered H3 first exposed the lossy address reversal recorded in #7:
1,275 living `(vertex, rung)` records produced 531 neighbour reversals, 222
absent people, 41 wrong people, and 16 shared-room collisions. Production now
builds one exact room-keyed ordered index for settlement names and living
occupations. Review then corrected the Stage 5 status, made the signature push
the actual composed anchor kind rather than the checked pattern kind, and
corrected the fixture account to distinguish narration from spatial data.
Focused regressions, the semantic screen-gate mutation, and the final review
were green.

### Task 5 — complete after one fix round

Review found that the purportedly complete artifact classification omitted the
campaign's world-build guard golden and two generated audit reports. The fix
round added their paths, producers, exact movements, and source causes below,
while preserving the distinction between `artifacts`-authored reports and a
deliberately maintained test roster. It also corrected 0750's description of
0084 from epoch granularity to the committed-derivation-moved rule. A direct
decision-digest render remained byte-identical, documentation tests passed
63/63, and `make gate-commit` passed all 4,014 subfloor tests plus its lint and
audit checks.

## H3 result

The final ignored foreground readout over seeds `[42, 13, 7, 1, 100]` reported:

| Seed | Built | Inhabited | Unoccupied | Later collisions |
|---:|---:|---:|---:|---:|
| 42 | 389 | 389 | 0 | 1 |
| 13 | 259 | 259 | 0 | 3 |
| 7 | 250 | 250 | 0 | 0 |
| 1 | 301 | 301 | 0 | 12 |
| 100 | 60 | 60 | 0 | 0 |
| **Total** | **1,259** | **1,259** | **0** | **16** |

All 1,259 inhabited threshold structures recovered their housemark class from
ordered `(kind, relation-to-required-kind)` values: **1,259/1,259**, across 15
peoples and all six classes. No room was dropped. Temporarily admitting the
screen universally made a common/plain and common/inward signature collide,
proving the classifier observes the cultural gate rather than prose.

## Artifact classification

The documented non-census commands were run against the integrated branch:
`make rebaseline` exited 0 and `make rebaseline-goldens` passed all 46 scoped
golden tests. The complete observed artifact and guard movement is attributable
to accession epoch 20, the chamber-rendering arm of design §7, and the required
audit/guard maintenance:

- `cli/tests/fixtures/world-seed-42.json` gained only the additive six-line
  `bench` concept object; `windows/worldgen/tests/fixtures/proto-goblinoid-root-table-seed-42.txt`
  gained one `bench` root row.
- `windows/vessel/tests/fixtures/snapshot-seed-0-chamber-occupied.json` and
  `snapshot-seed-42-chamber.json` changed only `narration.prose`, adding a
  bench; their complete `.spatial` values and narration nouns are identical.
- The full regeneration added only `bench` projections to the concept registry
  and manifest, per-people dictionary, three proto-family pages, solitary-tongue
  lexicon fixture, and trope provision reports.
- The two gallery possession transcripts changed only their chamber-furnishing
  sentences. The seed-42 client fixture changed only `narration.prose`. The
  seed-14 carrying fixture changed only the rendered chamber marks: one screen
  became one bench plus one guest-water vessel; its chamber grid and every
  non-mark value are unchanged.
- `docs/audits/type-audit-report.md` is an `artifacts`-authored generated
  report. It records the three new tagged vessel primitives: the
  `UnclassifiedRadius(f64)` diagnostic value, `WorldContext::built_rooms`'s
  identifier-text return, and `settlement_room_collision_count`'s count return
  (`diagnostic-value`, `identifier-text`, and `count` each `+1`; vessel total
  `534 -> 537`). `docs/audits/plumb-roster.md` is likewise an
  `artifacts`-authored generated report: the new `housemark.rs` raises parsed
  and kind-adjacent files `311 -> 312` and `133 -> 134`; the `BENCH: KindId`
  declaration raises excluded non-quantities and all constants touched
  `498 -> 499` and `1366 -> 1367` (`KindId` `18 -> 19`); and Task 4's added
  `OccupationRecord` import, together with the adjacent kernel import reflow,
  shifts sixteen existing `liveness.rs` finding locations by one line without
  changing their findings or classifications.
- `cli/tests/fixtures/world-build-sites.tsv` is not an `artifacts`-authored
  projection. It is the world-build guard's deliberately updated byte golden:
  `windows/vessel/tests/suite/housemark_readout.rs` adds one sanctioned
  `identity:1` site because H3 must build non-fixture world identities for
  seeds `[42, 13, 7, 1, 100]` rather than read only the default seed-42 fixture.
- `docs/digest/decisions-in-force.md` gained the five accepted records after
  the final-state regeneration. `docs/timings.md` absorbed the inherited
  post-Task-4 gate row and records both Task-5 rebaseline runs, the original
  Task-5 gate, and the fix round's gate instrumentation. No ledger fact, census
  file, lab golden, stream roster, or unrelated world datum moved.

## Binding records

The implemented technical rulings are now durable in
[0746](../../decisions/0746-a-dwellings-culture-is-a-derived-admission-signature.md),
[0747](../../decisions/0747-the-housemark-crosses-authority-with-threshold-posture.md),
[0748](../../decisions/0748-both-housemark-axes-live-at-the-entry-threshold.md),
[0749](../../decisions/0749-a-dwelling-reads-the-first-living-occupation-keyed-by-its-exact-room.md),
and [0750](../../decisions/0750-bench-is-concept-accession-epoch-twenty.md).
Ledger #4 remains the campaign's execution choice rather than being padded into
an architectural record. `SOC-staple-ladder` is still an elaborated metaplan;
only its R1 Housemark slice is marked shipped. R2, R3, and the dynamics arc are
unchanged.

## Capture manifest

- `book/src/frontier/idea-registry.md` — `SOC-staple-ladder` now marks only R1
  shipped and points it to The Housemark spec; the metaplan remains
  `elaborated`, with R2, R3, and the dynamics arc open.
- This ledger's Follow-ups retain the temporal-style and cultural-evolution
  branches. Neither is smuggled into R1.
- Rejected design branches — per-people inventories, a six-case culture enum,
  exact-float styles, forced `status_basis` consumption, and an R2 chamber-count
  change — are recorded in entries #1–#3 with reasons.
- `.superpowers/visualizations/housemark-design.html` is the ignored visual
  companion; it is explanatory scratch, not a product artifact.

## Close routing

The close sweep read every Task 1–5 brief and report, `progress.md`, both review
reports, and every capped/full review package under
`.superpowers/sdd/2026-09-04-the-housemark/`. The diff packages contain no
independent rulings; they are reproducible copies of the commit ranges supplied
to reviewers. No ruling was found only in scratch: the exact-room correction is
entry #7, and the three-attempt-per-issue interpretation is retained in the
Task 4 implementation-review account and the campaign retrospective.

Every review minor is closed rather than deferred: Task 3's production
unoccupied regression, Task 4's actual-anchor signature and fixture wording,
Task 5's 0084 annotation and plumb provenance, and final review's
label-independent H3 decoder all landed in their named fix rounds above. Their
process consequences and exact code/document homes are enumerated in
`docs/retrospectives/the-housemark.md`.

The two speculative limits have registry homes. The measured builder/stratum
gap is now explicit in `CLIENT-housemark-provenance`, beside
`CLIENT-ruin-signature` and `CUL-6`; culture changing through time is carried by
`CUL-14` and `CUL-18`. R2, R3, and the dynamics wheel remain open in
`SOC-staple-ladder`. The bench's absent sitting interaction is a scope boundary
routed to `MAP-19` and `MAP-27`. The book freshness sweep updates both species
accounts, The Hearth's original promise, The Staple's accession boundary, The
Blocking's epoch taxonomy, the brief-contract registry row, and the frontier's
architecture entry. The Confidence Gradient is N/A: no standing bet in
`book/src/open-questions.md` moved.

At close-artifact authoring time no census, merge, or push has occurred.

## Follow-ups

- A future reading of inherited or hybrid architecture needs a durable builder
  or occupation-stratum signal. `Brief::people` supplies only the living
  occupation, and no R1 code should manufacture historical attribution from it.
  This measured gap is carried by `CLIENT-housemark-provenance`.
- Cultural evolution within one people is absent: `SocietyVector` is an authored
  species component, not a world-time value. That is a separate dynamics or
  historiography question, not hidden scope for R1.

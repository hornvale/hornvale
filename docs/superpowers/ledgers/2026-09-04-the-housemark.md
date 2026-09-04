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

## Capture manifest

- `book/src/frontier/idea-registry.md` — `SOC-staple-ladder` now points from
  R1 to The Housemark spec; its status remains `elaborated` until implementation.
- This ledger's Follow-ups retain the temporal-style and cultural-evolution
  branches. Neither is smuggled into R1.
- Rejected design branches — per-people inventories, a six-case culture enum,
  exact-float styles, forced `status_basis` consumption, and an R2 chamber-count
  change — are recorded in entries #1–#3 with reasons.
- `.superpowers/visualizations/housemark-design.html` is the ignored visual
  companion; it is explanatory scratch, not a product artifact.

## Follow-ups

- A future reading of inherited or hybrid architecture needs a durable builder
  or occupation-stratum signal. `Brief::people` supplies only the living
  occupation, and no R1 code should manufacture historical attribution from it.
- Cultural evolution within one people is absent: `SocietyVector` is an authored
  species component, not a world-time value. That is a separate dynamics or
  historiography question, not hidden scope for R1.

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

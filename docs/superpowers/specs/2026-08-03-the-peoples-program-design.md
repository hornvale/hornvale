# The Peoples Program — metaplan

**Status:** metaplan, awaiting G3 review. **Design-only: no implementation.**
The deliverable is this document, the campaign sequence it proposes, and the
first campaign's spec (`2026-08-03-the-generalist-design.md`).
**Date:** 2026-08-03

## 1. What this is

The roster holds five peoples: goblin, kobold, hobgoblin, bugbear, gnoll. This
program takes it to seventeen, by authoring humans, dwarves and elves.

It exists because that sentence is much larger than it sounds. `BIO-three-probes`
records the intent in three sentences and carries the status `spec'd`; there is
no spec, and two of its three claims do not survive contact with the code. The
brainstorm that produced this document found that the elf's defining trait is
**inexpressible** in the shipped model and that the dwarf's named axis models
the **wrong relation**. Neither is a defect to route around. Each names a real
piece of modelling the roster expansion was silently assuming.

So the program is five campaigns, not one, and three of them build mechanism
before any of them adds a people.

## 2. What the brainstorm found

**F1 — An elf cannot be long-lived.** `hornvale_species::lifespan` is a pure
function of `(Mass, MetabolicClass)` (`domains/species/src/allometry.rs`):
`pace(class) · (60/40^0.25) · mass^0.25`. It takes no `KindId` and there is no
per-kind override anywhere in the workspace. A 60 kg elf gets ~66 years; a
70 kg human ~69. A 750-year elf would mass ~977,000 kg. Dragons are long-lived
here *only because they are heavy* — longevity is not an authorable property,
it is a side effect of mass.

The arithmetic is a source read validated against two values the repository
states itself (`windows/worldgen/src/lib.rs`, `LIFESPAN_THRESHOLD_YEARS`'s
doc): bugbear 132.0 kg → ~80.9 yr, white/black dragon 2200.0 kg → ~163.4 yr.
Both reproduce. **It has not been confirmed by a live call**; C2b's spec owes
that run before it depends on the numbers.

**F2 — A long-lived elf would still draw goblin's drift regime.**
`cascade_regime_of` matches on `SocialForm`, and only the `Solitary` arm
consults lifespan; `Settled` returns `CascadeRegime::SETTLED` unconditionally.
Decision 0066 ships as "drift = f(sociality × lifespan)", but the `Settled` row
of that product is constant in lifespan — and `Settled × long-lived` is exactly
the cell the elf was supposed to probe. The model can neither express the trait
nor notice it.

**F3 — `MINERAL` is a trophic axis.** `kernel/src/ecology.rs` defines it as
consumed "soil/rock nutrients", `ResourceKind::Stock`; its only occupants are
xorn and rust monster at weight 1.0. Authoring dwarves onto it would make them
lithovores in Pianka competition with an elemental. The owner's picture —
fungiculture, fermentation, farmed cave fish — is `DETRITUS`-based subterranean
agriculture. **The interesting thing about a dwarf was never its diet; it is
its realm.**

**F4 — The Underdark seam is already cut.** `Realm { medium, access }` in
`domains/climate/src/facets.rs` is documented as "never an enumerated world, so
a later sky realm — or an elemental plane — is a new value rather than a new
axis", and `Stratum`'s doc states that "the pelagic zones and (later) the
underworld's geological layers are the same construct at different realms."
The Stratum chose the depth-band mechanism *because* it generalizes to MAP-69.
MAP-10 is `shipped` — The Deep gave the rock column, The Lode made caves and ore
one substrate, The Vestige added residue decay. Placement is realm-agnostic
already: "Nothing in this assembly special-cases water; a marine kind authored
onto the `MARINE_FORAGE` axis would get a non-zero K at sea from the supply
field alone."

**F5 — The realm has residents waiting for it.** Xorn's niche doc reads
"subterranean/mineral … reads as nearly climate-indifferent **on the surface
axes**"; rust monster is a "subterranean/cave mineral-eater" with "a strong
low-insolation (cave-dark) preference". Both are cave creatures faked onto
surface cells, with darkness approximated by a low-insolation curve. Rehoming
them is C2a's measurable payload, and it depends on no new kind.

**F6 — The human probe is live today.** `hornvale_demography::BETA = 2.0`, with
a calibration sweep over 13 seeds × 10 β. Humans need no new mechanism at all.

## 3. The probe-validity ladder

Every campaign here claims to measure something. The ladder is what separates a
measurement from a number:

```
  rung  state                     what it means
  ----  ------------------------  ---------------------------------------
   1    inexpressible             the trait cannot be authored at all
   2    expressible, unread       authorable, but no consumer reads it
   3    read, but miscoupled      read through a relation that is wrong
   4    genuinely measured        the readout differentiates the axis
```

At brainstorm time the three probes sat on rungs 1, 3 and 4 respectively — elf,
dwarf, human.

**Rung 2 is the trap, and it is the one a campaign walks into by accident.**
Fix only the authoring and an elf gets 750 years in the almanac that no system
reads: a number that looks exactly like a result and measures nothing. Fix only
the consumer and a live branch has no kind that exercises it.

**Therefore every campaign in this program owes a mutation step**, not merely a
passing test: a demonstration that the readout *would report differently* if the
axis moved. C2b's is the clearest — a test that goes RED if `cascade_regime_of`
ignores lifespan. A green test proves the code ran; only the mutation proves the
axis is visible.

This is the program's single shared acceptance criterion, and it is the direct
descendant of the finding that produced this document.

## 4. The sequence

```
    C2-0  THE GENERALIST   human x1
      |
      v
    C2t   THE TOLERANCE    dispersion; warlikeness becomes DERIVED
      |                    (added mid-C2-0; moved ahead of C2a/C2b)
      +-----------+-----------+
      |           |           |
      v           v           |
    C2a         C2b           |
  DEEP REALM   LONG AGE       |
  Underdark    lifespan       |
      \           /           |
       \         /            |
    +-------+-------+         |
    |               |         |
   C2c             C2d <------+
 THE DELVERS   THE RADIATION   HARD DEPENDENCY:
 dwarf x5      elf x6          selection needs a distribution
```

**C2t was added on 2026-08-03, mid-C2-0.** C2-0, C2a and C2b were originally
three independent roots; C2t now precedes C2a and C2b, because every people
authored before it is authored in a frame it changes — authoring five dwarves
and six elves first would mean authoring eleven peoples twice. See §4b.

**C2-0 — The Generalist.** Humans, one kind, surface only. No new mechanism.
Full spec: `2026-08-03-the-generalist-design.md`.

**C2a — The Deep Realm.** A subterranean `Realm`: new `Medium` and `Access`
values, a stratum column, cave `Formation`s, a subterranean supply field, and
per-stratum occupancy so that a cell's population is keyed `(cell, stratum)`
rather than `(cell)`. Rehomes xorn and rust monster off their faked surface
niches — the campaign's payload, and its proof, without a new kind. Follows The
Stratum's recommended depth-band mechanism rather than extending `RoomAddr`.

**C2b — The Long Age.** Lifespan gains an authoring channel: a *curve* or
strategy rather than a scalar override, with metamorphosis as the sibling case
the design must leave room for. `cascade_regime_of`'s `Settled` arm learns to
read it. Preregister byte-neutrality for the five existing peoples — that null
is the result, and it is what makes the epoch cheap. Owes the live confirmation
of F1's arithmetic.

**C2c — The Delvers.** Dwarves ×5: Hill (surface), Mountain and Duergar
(subterranean, different strata), Gully, Desert. Their shared `dwarf` family
label crosses two members, which makes a `family_proto` entry in
`hornvale_language` **mandatory** — today only goblinoid, draconic and plant
are multi-member families. First people to live in the Deep Realm.

**C2d — The Radiation.** Elves ×6: Wood (temperate forest, the ancestor the
proto sits at), High (the family's only *social* rather than environmental
divergence), Drow (Underdark), Sea (Waterworld), Desert, Snow. Plus derived
divergence and LANG-53.

### 4b. C2t — The Tolerance, and the finding that added it

**A people has no interior, and the program did not notice until a code review
found that no human raids.**

The Generalist authored humans at `threat_response = 0.5`; `RAID_DISPOSITION_MIN
= 0.6` gates raiding on that dimension. The sentence *"humans do not raid"* is
only sayable because every human in every world is one human.

The diagnosis generalises past the symptom. A type is being used as its own only
instance — the species row is at once the type description and the sole
exemplar. That is macroeconomics' **representative agent**, and its standing
critique is predictive rather than merely descriptive: it names in advance the
phenomena a one-agent model cannot produce. Here those are deviance, the
exceptional individual, sorting (two settlements of one people differing from
each other), lineage drift — **and selection**, which is why this blocks C2d.
A radiation splits a population under selection; selection acts on a
distribution; a point has no tails.

The keystone is that **variability is itself a species trait**. A eusocial
insect has near-zero behavioural variance and a generalist has high variance, so
dispersion is authored per species. Which also means the thing that makes humans
generalists in the sense the owner intends is *psychological* breadth, not the
ecological breadth C2-0 measures.

Cheaper than it appears: `instance_biosphere` already implements prototype
inheritance and is the workspace's **only** instance lens — the body can vary
per individual, the mind cannot. C2t completes a pattern already committed to.

Full spec: `2026-08-03-the-tolerance-design.md`. Its §7 records the
placement, resolved at the owner's direction on 2026-08-03: C2t precedes C2a
and C2b, since every people authored before it is authored in a frame it
changes.

### The sequencing rule

C2-0, C2a and C2b are mutually independent roots. **They must still run in
sequence.** Each carries an epoch and a census regen; censuses serialize on one
heavy writer per box (0081); and a mid-run epoch poisons a census regen *with
nothing turning red*. The dependency graph permits a parallelism the epoch
discipline forbids, and this paragraph exists so a later session does not
rediscover the former without the latter.

The Mac is otherwise free as of 2026-08-03, so the ceiling is not the binding
constraint here — the epoch/census seam is.

## 5. The roster

Five peoples become **seventeen** — twelve new kinds, of which eleven come
after this program's first campaign.

```
  family    kinds                                          realm
  --------  ---------------------------------------------  ----------------
  human     human                                          overworld
  dwarf     hill, mountain, duergar, gully, desert         over + under
  elf       wood, high, drow, sea, desert, snow            over + under + sea
  (extant)  goblin, kobold, hobgoblin, bugbear, gnoll      overworld
```

Every kind means rows in six registries and a validation in a seventh
(`BIO-kind-authoring-seam`): biosphere, family, and the kind concept in
`domains/species`; psyche, society, perception; articulation and lexicon in
`domains/language`; `family_proto` once a family label is carried twice; and the
invariants (`speech ⊆ perception ⊆ mind`, society ⟺ minded ∧ social) enforced in
`windows/worldgen`. Both census fixtures refresh each time — 31 rows in
`the-census` and 3 in `census-of-the-meeting`.

### Elves are the roster's instrument for measuring realms

The owner's direction is that elves radiate: proto-elves split rather than
overburden a niche, and diverge to match where they land — strongly, in the
Drow's case. Under the `Realm` design that stops being flavour and becomes a
**prediction**: every realm the project ever adds gets an elf, and how alien
that elf is, is a function of how far its realm sits from temperate forest.
Space and abyssal elves are not a wishlist; they are what the rule says arrives
when a sky realm or a plane does.

The program ships this as **authored roster, derived divergence**: the six kinds
are authored, but their language divergence is computed from the radiation
topology (time-since-split × environmental distance) over `family_proto` and
0066's drift machinery. A **world-derived** roster — daughters generated against
each world's own niches — is the ambitious version and is blocked hard:
`KindId(pub &'static str)` is compile-time, and so are `KIND_CONCEPTS`,
`family_of`, and every census metric keyed by species name. Captured as
`BIO-elf-radiation`, not attempted here.

## 6. Non-goals

- **Half-elves and half-dwarves** (`mul`, which the owner prefers to call
  half-dwarf). That is `BIO-17`: "a species is a discrete registry key, but a
  hybrid is a *gradient between two*, attacking the data model itself." A
  data-model campaign in a roster costume.
- **A world-derived species roster.** `BIO-elf-radiation`; see §5.
- **Characterizing goblin.** The Manikin §8.2 argues it "should ride a campaign
  already paying for an epoch", and this program pays for five. It still should
  not: `MetabolicClass::Autotroph`'s own doc states the counter-rule for exactly
  this situation — BIO-autotroph-physics is "deliberately NOT bundled with the
  roster expansion that would destroy its attribution." Moving goblin's vectors
  inside a roster epoch makes every census movement unattributable, and
  attribution is this program's entire product.
- **The full MAP-69 relationship.** C2a builds the realm; over/under commerce,
  predation-inversion, chthonic emergence and subterranean valence are that
  row's own campaign. The owner's "they prosper when they can trade with the
  Overworld, but can close their borders for centuries" is over/under commerce
  plus an autarky property, and belongs there.
- **Detailed design of C2c and C2d.** Deliberate; see §7.

## 7. What this metaplan does not contain, and why

It is thin on C2c and C2d on purpose. A five-campaign plan written today will be
wrong about its last two campaigns by the time they start — C2a and C2b will
have changed what is expressible, which is the whole reason they run first. A
metaplan that specified dwarf trait values now would be specifying them against
a model that does not yet exist, and would then be *read* as authoritative after
it stopped being true. That is the stale-claim failure the project already names.

What does not rot, and so is stated here in full: the dependency graph, the
sequencing rule, the ladder, the roster's final shape, and the six-registry
authoring cost. Each campaign writes its own spec when its inputs exist.

## 8. Per-campaign spine

Each of the five follows the same shape:

```
  absorb main; make preflight (from the branch)
  author or extend
  preregister the prediction on a NAMED axis
  MUTATION STEP: show the readout would differ if the axis moved   <- §3
  declare the epoch only if a derivation actually moved (0084)
  census regen on lefford (0079 / 0086 / 0081) -- AUTHORIZATION REQUIRED
  refresh BOTH census fixtures (31 + 3)
  book chapter + chronicle + freshness sweep + retrospective
```

## 9. Frontier bookkeeping

- `BIO-three-probes` — correct the text (the `MINERAL` claim is wrong per F3;
  the elf claim is blocked per F1/F2), drop `spec'd` to reflect that its
  **Where** cell cites a brainstorm rather than a spec, and repoint it at this
  metaplan.
- `BIO-elf-radiation` — **new row.** World-derived species roster; blocked on
  `KindId` being compile-time. Cross-link `BIO-17` and MAP-69's
  speciation-by-stratum.
- `BIO-17` — note the `mul` → "half-dwarf" rename and that this program declined
  to bundle hybrids, with the reason.
- `LANG-53` — repoint at C2d, which is the roster it has been waiting for.
- `MAP-69` — note that C2a builds the realm the row's five mechanisms need, and
  that the mechanisms themselves remain the row's own campaign.
- `PSY-goblin-characterization` — record that this program declined to bundle
  it, and why (§6).
- `MAP-11` — the habitat-medium axis referenced at `windows/worldgen/src/lib.rs`
  as "a real habitat-medium axis (MAP-11) would state this"; C2a should say
  whether the Deep Realm supersedes it or sits beside it.
- `PSY-dispersion` — **new row.** A species is a distribution, not a point;
  dispersion is itself a per-species trait. Carries the representative-agent
  diagnosis and the list of phenomena a point-valued psychology forecloses.
  Cross-link `BIO-17` (is species a type or a position in a continuous space?),
  `PSY-five-is-lexical`, decision 0064 (personhood is a *region* of
  component-space — ratified for the personhood boundary, then contradicted by
  authoring every people as a point inside it), and C2d.
- `PSY-raid-gate-axis` — **new row.** `RAID_DISPOSITION_MIN` gates a *proactive*
  behaviour on `threat_response`, a *defensive* axis (flee ↔ stand). It would
  misclassify any people, not only humans. Owner ruled 2026-08-03 to disclose
  rather than fix, because fixing moves raid behaviour for all six peoples
  inside a roster campaign and destroys its attribution.
- `BIO-instance-lens-gap` — **new row.** `instance_biosphere` is the workspace's
  only instance lens: mass and potency vary per individual, mind/society/
  perception cannot. The prototype-inheritance mechanism is built and tested;
  only the body is wired to it. Subsumed by `PSY-dispersion`'s individual tier
  but worth its own row, since it is a *completed pattern left half-applied*
  rather than a missing design.

## 10. Flagged for review

1. **Five epochs and five census regens.** Census regen is a carve-out
   requiring explicit authorization per campaign (0081), runs on lefford
   (0079 / 0086), and is a ~7-min local run — so the real cost is fixture churn
   and the serialization, not compute. **Authorization is not requested here**;
   each campaign asks at its own close.
2. **`(cell, stratum)` occupancy is a save-format-class change.** C2a changes
   the key a population is stored under. It follows The Stratum's recommended
   band mechanism rather than touching `RoomAddr`, which is the cheaper of the
   two, but it is still the program's most invasive single change and it lands
   in its second campaign.
3. **F1's arithmetic is a source read, not a run.** Validated against two
   in-repo stated values, but C2b owes a live confirmation before depending on
   it.
4. **The roster more than triples the peoples.** Sixteen peoples against a
   coexistence stack tuned at β = 2.0 with five. C2-0 is the first probe of
   whether that knob survives; if it does not, C2c and C2d land on a stack that
   needs retuning first, and the program should expect to discover that early
   rather than at C2d.
5. **An unmerged branch edits two rows this program depends on.**
   `the-commonplace` (living in the `the-teller` worktree) has 6 unmerged
   commits and sits **373 commits behind main**, branched 2026-07-31; its last
   commit ledgers a pre-merge gate, so it reached the doorstep and stopped. Its
   frontier commits edit **`PSY-2`** — the row The Manikin just flipped — and
   **`LANG-53`**, the row C2d claims to unblock. Both appear in §9's
   bookkeeping. Either it lands before C2d and this program reads its version,
   or it lands after and conflicts with ours. Its own absorption of 373 commits
   is a separate problem and not this program's to solve, but the collision is
   real and is flagged here rather than discovered at C2d's close. `the-tithe`
   is merged and clean — its worktree is an orphan to sweep.

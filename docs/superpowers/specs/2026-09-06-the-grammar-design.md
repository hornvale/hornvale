# The Grammar — design

**Campaign:** The Grammar — BIO-3 reproductive architecture and the SOC-2
social derivations it enables. **Branch:** `campaign/the-grammar`.
**Decision block:** 0876–0885. **Status:** at G3 (spec review).

The Grammar asks whether biology and society can be represented as small,
composable grammars rather than as a catalogue of species exceptions. A body
plan exposes only the reproductive affordances the next layer needs. BIO-3
composes those affordances into reproductive profiles and demographic outputs.
SOC-2 consumes those outputs, plus ecology, property, subsistence, history,
and institutions, and derives households, kinship, gender arrangements, and
social classifications.

The campaign is grounded in ordinary biology but must remain expressive of
fantastic life. Magical sex or reproductive-state transition is a supported
future pathway, not an implemented feature here. Hybrids are represented by
compatibility relations rather than by a prematurely fixed genus tree.

## 1. The governing boundary

The Murrain's two-tier population design is load-bearing here. The population
substrate is the causally authoritative statistical model: cohorts,
distributions, relationships, and connected host populations may exist there
without materializing every person. The projection layer materializes an
aggregate, composite case, individual, or salient character for observation
and play. The Lot is a consumer of the substrate, never its authority.

The Grammar therefore has this flow:

```text
body plan
  -> reproductive affordances
  -> reproductive grammar
  -> population/cohort distributions
  -> social inputs
  -> SOC-2 social grammar
  -> households, institutions, and projections
```

No downstream layer reaches through its immediate input to inspect a distant
implementation detail. Detailed anatomy is private knowledge; reproductive
affordances are the narrow public interface.

## 2. Scope and non-goals

### In scope

- A body-to-reproduction affordance vocabulary.
- A minimal set of composable reproductive operations.
- A distinction between possibility, typicality, and realized events.
- Reproductive profiles for ordinary and fantastic probe species.
- Compatibility relations for hybrid outcomes.
- A future-compatible transition seam for magic.
- The BIO-3 to population-substrate handoff.
- The population-substrate to SOC-2 handoff.
- A small SOC-2 vocabulary for recognition, care, descent, status, access,
  and institutional change.
- Time-evolving social rules: founding, transmission, reinforcement,
  contest, reform, suppression, decay, memory, and forgetting.
- A preregistered probe and measurement panel.

### Not in scope

- A magic system or a `changeGender` spell implementation.
- A complete genus taxonomy for the world.
- A complete household implementation.
- A universal theory of human gender, romance, or kinship.
- Assigning existing Hornvale species a new reproductive canon before their
  profiles have evidence.
- Importing real-world racial, religious, sexual, or gender hierarchies as
  defaults.
- Graphic narrative treatment of severe harms.

SOC-2 is specified here as the consumer and successor boundary. Its full
household and institution implementation remains a follow-on stage after the
BIO-3 substrate contract is stable.

## 3. Body-plan affordances

`ReproductiveAffordances` is a conceptual interface, not a commitment to a
particular Rust shape yet. It answers only the questions required by the
reproductive grammar:

```text
what can this body produce?
what can those products combine with?
where can development happen?
what support does development require?
how does the offspring separate?
what natural transitions can the body undergo?
what constrains compatibility?
```

Possible products include gametes, spores, buds, embryos, templates, and
host-seeds. Possible development sites include a body, an egg, a brood
structure, a colony, a host, an environment, or a workshop. The interface
must be able to say "none" for a non-reproducing or externally manufactured
kind without treating that as an error.

The interface is a projection over a detailed body plan:

```text
detailed anatomy and development
  -> reproductive affordance projection
  -> reproductive grammar
```

The body plan may eventually include organs, segments, metamorphosis,
regeneration, sensory systems, and magical channels. BIO-3 does not expose
all of those details to SOC-2 or to the population substrate.

## 4. The reproductive grammar

The smallest useful operation vocabulary is:

```text
make       create reproductive material, a bud, a spore, or a template
join       combine compatible inputs
grow       develop an organism from an input
support    carry, nourish, brood, or sustain development
release    separate offspring from a parent or group
copy       replicate without combining distinct inputs
change     move an organism between developmental or reproductive states
build      manufacture a body
convert    use another organism as the developmental substrate
```

These are causal operations, not narrative labels and not a demand to
simulate every microscopic event. A species profile composes them with
guards:

```text
compatibility
environment
development site
required support
role availability
resource cost
time
```

Readable categories are derived summaries, never foundational types:

```text
sexual reproduction    = make + join
live-bearing           = grow + support + release
egg-laying             = grow + release
communal breeding      = support by a group
asexual reproduction   = copy
manufactured life      = build
parasitic reproduction = convert
sequential sex         = change
```

The grammar is expressed as relations and transitions over bodies, materials,
developmental states, support groups, and environments. It must not require a
species-specific branch for each probe profile.

## 5. Three kinds of truth

Every reproductive fact is classified as one of three kinds:

```text
possibility  what the body and grammar permit
typicality   how often or under what conditions it occurs
realization  what happened to a cohort or projected individual
```

"Possible" is not "common," and "common" is not what happened to one
person. This distinction prevents a population rate from becoming an
individual biography or an unusual projection from rewriting population
truth.

BIO-3's substrate outputs include:

```text
maturity age
generation length
offspring distribution
survival to independence
dependency duration
care burden
reproductive-role distribution
hybrid viability and fertility
population persistence
```

Realized relationships may include `produced-by`, `derived-from`,
`supported-by`, `belongs-to-brood`, `shares-parent`, `compatible-with`, and
`transformed-from`. The exact committed fact vocabulary is an implementation
decision after the substrate probe, not an assumption in this spec.

## 6. Compatibility and hybrids

Hybridization is a relation between reproductive affordances and development
constraints, not a hardcoded property of a species-name pair. Outcomes are
typed:

```text
fertile
viable-but-sterile
viable in one parental direction only
viable with developmental assistance
viable only through magic
unstable or low-survival
impossible
```

Compatibility may depend on inherited material, body plan, developmental
timing, role availability, environment, and future magical assistance. The
initial world may therefore support half-elf, half-orc, and half-dwarf cases
without first deciding which peoples share a genus.

The compatibility graph must not imply social meaning. A hybrid can be
ordinary, exceptional, politically useful, stigmatized, or celebrated only
through SOC-2 and historical institutions.

## 7. Natural and future magical transitions

BIO-3 supports natural maturation, metamorphosis, seasonal change, and
sequential reproductive roles through `change`. The future magic system may
later invoke a corresponding transition operation, conceptually like
`changeGender($creature, $gender)`.

This campaign does not choose the future operation's frequency, cost,
accessibility, reversibility, danger, or social meaning. It does impose one
negative constraint: the biological model must not make reproductive state
permanently immutable or conflate reproductive state with social gender,
personal identity, or body presentation.

## 8. The BIO-3 to SOC-2 handoff

BIO-3 exposes a reproductive-social substrate, not anatomy:

```text
reproductive roles
offspring pathway
dependency profile
care topology
descent relation
compatibility relation
transition history
```

SOC-2 combines these with:

```text
subsistence
property
inheritance
mobility
population pressure
authority
religion
contact with other peoples
```

It derives households, parenthood recognition, kinship, partnership norms,
gender arrangements, hybrid recognition, child-rearing institutions,
inheritance structures, and social classifications.

BIO-3 asks: **who can do what biologically?** SOC-2 asks: **how does a society
organize, recognize, reward, restrict, or reinterpret those possibilities?**

The same biological profile must be able to yield multiple stable social
arrangements. Biology constrains the affordance space; it does not select one
canonical culture.

## 9. The SOC-2 social grammar

SOC-2's small operation vocabulary is:

```text
recognize   treat a relation as socially meaningful
associate   group people into a household, brood, clan, or community
bind        create a durable partnership or obligation
care        assign responsibility for another person's welfare
assign      attach a role, status, or expectation
inherit     transmit membership, property, rights, or obligations
adopt       create recognized kinship without biological descent
exchange    move people, care, property, or obligations between groups
exclude     restrict access to membership, resources, or institutions
dissolve    end a household, partnership, role, or affiliation
```

Readable categories are derived summaries:

```text
household  = associate + care + shared resources
kinship    = recognize + inherit + descent or adoption
marriage   = bind + recognize + often inherit
gender role= assign + repeat + institutionalize
caste      = assign + inherit + exclude
citizenship= recognize + associate + protect + exclude
stigma     = classify + exclude + transmit
adoption   = adopt + recognize + care + inherit
```

No operation carries an automatic moral valence. `exclude` may describe
quarantine, citizenship boundaries, exile, or persecution; the target,
justification, enforcement, and material consequences distinguish them.

## 10. Social rules over time

Social arrangements are stateful and historical:

```text
latent
  -> customary          repeated practice
  -> institutionalized  authority, property, or law
  -> contested          contradiction or organized resistance
  -> reformed           rule changes while retaining continuity
  -> residual           old practice survives its formal repeal
  -> forgotten          no active transmission
```

Institutionalized rules may instead be reinforced, suppressed, or decay when
their supporting conditions move. The model measures belief prevalence, norm
strength, institutional enforcement, material disadvantage, exit cost, dissent
bandwidth, and intergenerational persistence separately.

This is the mechanism for endogenous prejudice. The world does not import
antisemitism, white supremacy, homophobia, transphobia, or an equivalent
real-world hierarchy as a default. It may generate stigmatization from local
classification, resource competition, conquest history, inheritance, pathogen
fear, or elite incentives. No fantasy species is a one-to-one allegory for a
real-world marginalized group.

Severe harms may be represented as abstract historical or demographic
mechanisms with non-graphic narrative surfaces.

## 11. Probe atlas

Probe species are authored data fixtures, not automatically canon:

```text
Pairborn    make -> join -> grow -> support -> release
Turning     make -> join -> grow -> change -> reproduce
Broodweave  make -> join -> grow -> brood -> release
Budded      copy -> grow -> support -> release
Forged      build -> awaken -> mature -> build
Guestborn   make -> convert -> grow -> release
Crossing    species A + species B -> hybrid outcome
```

The first panel uses Pairborn, Turning, Broodweave, Budded, and Forged as
core probes. Guestborn and Crossing are stress probes. A non-reproducing or
externally manufactured kind is a negative control. Existing Hornvale peoples
are calibration anchors; they are not assigned new reproductive canon merely
to fill the panel.

Every probe must answer:

1. Can the substrate represent it statistically?
2. Can population dynamics consume it without individual materialization?
3. Can a projection materialize a coherent individual from it?
4. Can SOC-2 derive more than one social outcome from it?

## 12. Coupled experiments and measurements

### One biology, many societies

Hold Pairborn biology constant and vary pair-household, communal-care, and
residence-inheritance arrangements. Measure household shape, caregiver
networks, parenthood recognition, inheritance, dependency burden, gender-role
differentiation, and household dissolution.

### One society, many biologies

Hold social inputs constant and substitute Pairborn, Broodweave, Budded, and
Turning. This isolates biological effects from institutional effects.

### Historical shock

Apply resource collapse, migration, war, new hybrid contact, or future magical
availability as controlled perturbations. Observe adaptation, reinforcement,
new classification, abolition, residual practice, and reappearance.

### Hybrid contact

Increase contact from absent to rare to routine to shared-household scales and
measure hybrid births, household recognition, inheritance, citizenship,
partnership, and institutional conflict.

The shared measurement panel is:

```text
birth intensity
generation length
offspring distribution
survival to independence
dependency duration
care burden and topology
parents and caregivers per offspring
descent and inheritance paths
hybrid frequency and outcome
household formation and dissolution
belief, norm, enforcement, and material effect
cross-seed distribution and deterministic repeatability
```

Not-applicable values are explicit; a missing measurement is never silently
treated as zero.

## 13. Success criteria

The design succeeds when:

- the core probes require no species-specific grammar branches;
- changing BIO-3 changes expected demographic outputs;
- changing SOC-2 does not alter biological population totals by accident;
- the same biology can yield multiple stable social arrangements;
- hybrid outcomes follow compatibility and contact rather than name-pair
  exceptions;
- a non-reproducing kind is representable without a special failure path;
- The Murrain can consume host-population summaries without named individuals;
- The Lot can project lives without becoming the population authority;
- future magical transitions remain possible without inventing their rules;
- the same seed and selection key reproduce the same projections;
- probe definitions remain data and mechanisms remain code.

## 14. Implementation sequence

The eventual implementation plan should stage work as follows:

1. Freeze the affordance vocabulary and probe data shape.
2. Implement the reproductive grammar over plain, testable values.
3. Add the population-substrate projections and shared measurement panel.
4. Add compatibility and hybrid probes.
5. Wire the stable handoff consumed by SOC-2.
6. Implement SOC-2's social operations and lifecycle experiments as a
   successor stage, not by reaching into body-plan internals.

No implementation begins from this spec until G3 review is complete and a
separate execution plan has been approved through the campaign process.

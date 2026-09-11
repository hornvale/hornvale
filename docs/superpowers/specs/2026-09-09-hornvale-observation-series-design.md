# Hornvale Observation Series Design

> **Partial supersession (2026-09-10):** [The Planetarium](2026-09-10-the-planetarium-design.md)
> and [decision 0956](../../decisions/0956-planetarium-schedules-a-directed-graphical-study.md)
> schedule the graphical study and amend Observation Series direction, cadence,
> release staircase and reserve requirements. Existing manifests and records retain
> their historical meaning and approval status. The ten-second study is not a
> public episode; exact video/copy publication still requires approval.

## Status

Design approved in conversation on 2026-09-09. This document defines the
strict public episode type and the internal planning and production system for
a months-long sequence of short Hornvale observations.

## Purpose

Hornvale will be presented as an open-ended fantasy fishbowl: a deterministic
world simulation that can be inspected through many objects, scales, axes,
processes, and lenses. The sequence is not a port of, adaptation of, or
public comparison series about any other simulation. Literature and external
models may remain in the internal research ledger, where they can help test
coverage and functional correspondence, but they are not the primary public
organizing principle.

The public sequence enumerates realized spaces in which Hornvale can be
observed and explored. It does not promise a conclusion, summarize the entire
project, or serve as a general container for all Hornvale media.

## Definition: observation episode

An observation episode is a 30–60 second audiovisual record of one realized
phenomenon in one deterministic Hornvale world instance, accompanied by two or
three concise public posts.

An episode has exactly one primary feature, axis, or dimension. It may show
several behaviors within that space, but it must not silently change the
object, scale, or claim being observed.

Every episode has:

- one object or feature under observation;
- one explicit scale;
- one primary observable axis;
- one visual grammar;
- one bounded observation sentence;
- a reproducible seed, revision, and frame or time selection;
- an internal evidence record;
- public copy reviewed and approved by Nathan before publication.

The episode type excludes retrospectives, conclusions, recaps, technical
tutorials, implementation notes, comparison essays, and long-form research
reports. Those may be created separately with their own models and lengths.

## Public editorial contract

The public sequence is a cumulative exploration whose individual episodes are
standalone. A viewer does not need to watch earlier episodes to understand an
episode's immediate claim.

The public deliverable does not mention:

- unshown capabilities;
- planned features or roadmap items;
- missing or absent capabilities;
- future episodes or where the project is heading;
- the comparison source or comparison ledger as the episode's framing.

The public copy may be casual, curious, wordy, and technically literate. It
should sound like a person showing friends something interesting. It may use
metaphor and personality, but it must not change the exact object or scale of
the internal observation.

The default video rhythm is:

```text
0–04s    immediate visual hook
04–12s   identify the place, time, object, or phenomenon
12–48s   let the behavior or variation unfold
48–60s   state the observed consequence
```

The public package contains one video, one casual primary post, and up to two
optional replies. Replies are used only when they add useful context, not to
force a methods paper into the feed. A seed, revision, or exact term may be
included when it is interesting or useful, but is not required in every post.

## Editorial approval boundary

Caption generation is advisory. Publication is manual and requires Nathan's
approval of the exact video and copy package.

The lifecycle is:

```text
draft → reviewed → approved → published
```

- `draft`: candidate video or copy exists;
- `reviewed`: terminology, evidence, and visual correspondence have been
  checked;
- `approved`: Nathan approved the exact package for publication;
- `published`: Nathan published it manually.

No tool, script, or client in this project may publish directly to a social
network or treat generated copy as approved copy.

## Internal episode record

The internal record is the source of truth for the episode's claim and
provenance. It may contain comparison fields and planning information that
never appear in the public package.

```text
episode_id
title
object
scale
primary_axis
phenomenon
visual_grammar
observation_sentence
world_revision
seed_or_seed_family
time_or_frame_selection
controlled_inputs
comparison_reference
evidence_status
capability_state
lead_time
source_data
render_output
caption_draft
editorial_status
```

`observation_sentence` is the anchor. It describes what the rendered evidence
shows, not what the implementation intends to show.

The record must state the counted or observed unit whenever the distinction
could matter. Population, settlement, occupation, residue, and projected
individual are separate terms and must not be substituted for one another.

## Evidence rubric

Every episode record carries the following internal evidence fields:

1. **Contract** — object, actor if any, scale, spatial and temporal scope,
   interface, and the exact observable being claimed.
2. **Witness** — reproducible revision, seed or seed family, command or client
   path, selected frames, and rendered output.
3. **Discrimination** — a plausible case in which the claimed property is
   false, or a controlled comparison that distinguishes the claim from a
   nearby interpretation.
4. **Attribution** — which values are authored, derived, dynamically changed,
   or merely correlated; causal language is permitted only when the witness
   supports it.
5. **Scope** — sample, denominator, time window, failures, uncertainty, and
   dependence among measurements.
6. **Wording** — the strongest public sentence justified by the evidence,
   with any stronger interpretation retained only as an internal note.

Words such as `chooses`, `seeks`, `learns`, `remembers`, `thrives`, `stronger`,
`successful`, `drives`, `makes`, and `because` require an actor, state, or
causal witness. If the evidence only shows a distribution or transition, the
title and caption use that neutral description.

## Episode atlas

The planning instrument is an atlas indexed by object or scale and observable
axis. A feature or distinction may receive its own episode when it has an
independent object, state, or observable and a visually distinct treatment.
Importance is not reduced merely because two features are related.

### Objects and scales

- world;
- geography;
- field;
- habitat;
- organism;
- population;
- settlement;
- occupation;
- residue;
- relation;
- language;
- belief or practice;
- observer or lens.

Population, settlement, occupation, and residue are separate layers. They
represent different sociological, anthropological, historical, and
archaeological objects and can vary independently.

### Observable axes

- quantity;
- distribution;
- spatial arrangement;
- topology or connectivity;
- relation;
- transition;
- persistence or duration;
- provenance;
- variation across seeds;
- viewpoint or resolution.

An episode occupies a cell or small neighborhood in this atlas. The atlas is
open-ended: new features may add cells without renaming existing ones.

### Expansion operations

The atlas supports three deliberate expansions:

- **Scale substitution** — hold a phenomenon constant and examine it at
  organism, population, settlement, occupation, or residue scale.
- **Axis substitution** — hold the object constant and change quantity,
  distribution, relation, transition, provenance, or viewpoint.
- **Combination** — cross an object with a process or lens, such as settlement
  × transition or language × provenance.

These operations generate candidates, not automatic episodes. Every candidate
still needs an independent observation, visual treatment, and evidence record.

## Dependency model

The planning system keeps three graphs separate.

### Evidence graph

The evidence graph records what must be established before a claim is
interpretable. It preserves the principle that coarse fields constrain finer
observations while allowing cross-branch dependencies.

```text
world → fields and geography → habitats → life and niches
life and geography → populations → settlements → occupations and residue
cycles and populations → history → contact → language and culture
multiple branches → observer and interaction lenses
```

This graph is explanatory, not a release order.

### Production graph

The production graph groups episodes that can share world data, renderers,
visual treatment, terminology review, or verification. It is an internal
manufacturing graph and never needs to be visible to the audience.

### Release graph

The release graph is a topological walk through the evidence graph that also
optimizes visual rhythm. It may alternate between physical, ecological,
historical, linguistic, and interactive observations when all prerequisites
are already available.

The release graph is not required to be a numbered staircase. It may branch,
interleave, revisit an atlas cell at a finer scale, or pause one branch while
another produces a more visually legible episode.

### Public release spine

The audience-facing sequence uses a scale ladder as its primary orientation:

```text
astronomical neighborhood → system → world → region → settlement
→ occupation layer → population → individual → language, belief, and practice
```

This is a presentation spine, not a claim that every episode belongs to one
continuous world or that coarse conditions causally determine every fine
observation. The production and evidence graphs retain their independent
dependencies. Within each scale cluster, episodes may branch across spatial,
temporal, relational, and interpretive axes, and the sequence may revisit a
scale when a distinct object or observable warrants its own episode.

The first episodes should establish astronomical and system-level variation
before descending into planetary, regional, and social objects. A later
episode may follow one selected world inward, but the record must distinguish
demonstrated causation from nested description, correlation, or shared
provenance. The scale ladder supplies continuity; the atlas determines whether
an episode is independently warranted.

## Visual grammars

Every episode chooses one primary visual grammar:

1. **Spatial film** — maps, fields, gradients, distributions, and arrangements.
2. **Temporal film** — timelines, life courses, occupation intervals,
   population change, and cycles.
3. **Relational film** — food webs, contact, tribute, ancestry, and other
   typed relations.
4. **Close reading** — one organism, settlement, occupation, place, utterance,
   belief, or residue.

Secondary overlays are allowed only when they clarify the primary grammar.
Two episodes may share a data source, but they should not share the same
dominant treatment unless their scale or axis is the subject of the contrast.

## Production batches

Production is batched even though publication is daily. A batch contains four
to eight episodes sharing one or more of:

- world family or controlled seed set;
- authoritative data export;
- renderer;
- visual palette and typography;
- terminology pass;
- verification pass.

A batch may include different episode cells and visual grammars. For example,
one history batch can contain a founding map, an occupation-duration timeline,
a raid relation view, a tribute relation view, and a close reading of residue.
They remain separate episodes because they observe different objects or axes.

The production flow is:

```text
select atlas cells
  → identify capability requirements
  → implement or instrument required surfaces
  → generate authoritative observation data
  → render candidate episodes
  → inspect phone and laptop outputs
  → draft public copy
  → review evidence and terminology
  → Nathan approves exact package
  → manual publication
```

## Capability planning and buffer

The atlas must identify capability work early enough that an episode is never
dependent on an emergency implementation immediately before publication.

Each candidate carries internal planning fields:

```text
capability_state:
  existing
  needs_observation_surface
  needs_renderer
  needs_simulation_extension

readiness:
  candidate
  in_production
  reviewed
  approved
  backlog
  published
```

Capability suggestions are scheduled at least two weeks before the episode's
demonstration window. The episode must be rendered and tested at least seven
days before its intended publication date.

The standing reserve is at least seven fully approved video and copy packages.
If the reserve falls below seven, the release schedule pauses or reorders from
already approved work; it does not rush an unfinished episode.

The intended internal pipeline is:

```text
atlas research batch
  → capability implementation batch
  → render and review batch
  → seven-plus approved backlog
  → nightly manual publication
```

The seven-episode reserve is an operational target, not a public claim and not
a requirement that the series contain a fixed number of episodes.

## Candidate population

The initial candidate population is broad rather than a fixed numbered list.
It covers these families:

- world and geography;
- matter, terrain, and environmental fields;
- climate and cycles;
- life, bodies, and niches;
- populations, settlements, occupations, and residue;
- deep-time history and relations;
- language and cultural practice;
- observation lenses and interaction.

The candidate population should be expanded and pruned through the atlas,
not by silently collapsing meaningful distinctions. A candidate is removed or
combined only when it lacks an independent observable, lacks a distinct visual
grammar, or is better represented as an internal prerequisite than as a public
episode.

## Campaign stages

### Stage 1: Vocabulary and atlas

**Goal:** Establish the object, scale, axis, visual grammar, and title rules.

**Success criteria:** Existing Hornvale capabilities are mapped into atlas
cells; population, settlement, occupation, and residue remain distinct; the
internal evidence rubric is usable on sample candidates.

### Stage 2: Capture proof

**Goal:** Demonstrate a reproducible path from a deterministic world state to
phone- and laptop-legible video frames.

**Success criteria:** A small batch can be generated, inspected, and stored
with its internal record and without manual reconstruction of hidden state.

### Stage 3: Opening run

**Goal:** Produce the first eight to twelve approved episodes.

**Success criteria:** The public voice, pacing, visual grammars, caption length,
and review checklist have been calibrated against real outputs while the
seven-episode reserve is preserved.

### Stage 4: Batch production

**Goal:** Traverse the atlas through dependency-valid release branches.

**Success criteria:** Batches of four to eight episodes can move from data to
approved backlog with predictable review and terminology work.

### Stage 5: Expansion and return passes

**Goal:** Add new atlas cells and revisit existing cells when richer realized
observations become available.

**Success criteria:** New work enters the atlas without renaming or silently
altering prior episode claims; superseding episodes cite the earlier record
and state exactly what changed in object, scale, axis, or evidence.

## Non-goals

This design does not:

- define a social-network publishing integration;
- require a fixed season length or conclusion;
- make public posts autonomous;
- collapse Hornvale into a simpler model for presentation;
- treat a comparison catalogue as the public narrative;
- establish that a correlation is a causal mechanism;
- replace existing simulation, client, or save-format contracts.

## Acceptance criteria for this design

The design is complete when:

- the observation episode is a bounded and reusable media type;
- the public and internal vocabularies have distinct responsibilities;
- the episode atlas preserves meaningful object and scale distinctions;
- evidence, production, and release dependencies are separate graphs;
- four visual grammars support varied short-form output;
- the internal package records reproducibility and claim scope;
- public copy is drafted but never published without Nathan's approval;
- capability work is scheduled two weeks ahead;
- a seven-episode approved reserve is maintained;
- the design can support an open-ended sequence without requiring a final
  conclusion.

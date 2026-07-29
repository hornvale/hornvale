# The Occlusion — design

**Status:** COMPLETE — merged. See [the chronicle](../../../book/src/chronicle/the-occlusion.md).
**Date:** 2026-07-27

## 1. Motivation

The sim is richer than its windows. Every finding below is a presentation
layer discarding detail the domains already compute — which is why the
campaign is cheap: the expensive part is built.

The name is literal. Three of the four findings are the world hiding
something it already knows; the fourth is the world failing to hide
something it should.

### 1.1 Measured findings (seed 42, this box, release build)

**F1 — the game seam rejects the directions it prints.** `possess` renders
`Ways on: SE, N, SW.` and then:

```
> n
No verb 'n' ('help' lists them).
```

Movement is `go n`. `parse_compass` (`windows/vessel/src/session.rs:1301`)
already accepts `n`/`north`/`se`/…; the verb dispatch
(`windows/vessel/src/session.rs:563`) simply has no fallthrough to it. The
first token anyone types into a text world is a bare direction.

**F2 — the sky contradicts itself.** Both the almanac and possession render:

> …the stars keep their stations: one smoldering red, one warm yellow… **The
> sky is a flat overcast.**

`sky_report_from` (`windows/worldgen/src/lib.rs:5692`) appends the weather
phrase *after* the celestial enumeration and never gates it. The salience
list keeps ranking `night-star` phenomena under a rain deck.

**F3 — weather is pinned to the capital, not to the observer.**
`sky_report_from` (`:5679`) resolves the *flagship settlement's* cell and
reads `weather_at`/`cloud_type_at` there, ignoring the walker's room; it
falls back to `CellId(0)` when a world has no settlement — which happens
(seed 123 generates none). Walking never changes your weather.

The temporal axis was measured before blaming the system, and is **correct**:
over 1,480 simulated days a possession sees 23 overcast / 11 fair / 7 rain.
Time works; place is pinned.

**F4 — strangeness is generated but unreachable.** Seed 42 places **101**
exotic sites. `locale --sample 60` reports strangeness `0.0` for all 60, and
no CLI path exposes `LocaleContext::strange_sites()`. The rarity budget works
as designed; there is simply no way to find one.

### 1.2 Explicitly not in scope

Two further findings were surveyed and deferred by owner decision:

- Marine biomes have no prose. `variety_pool`
  (`windows/locale/src/grammar.rs:132`) covers **0 of 10** marine biomes, so
  `bathypelagic` renders as "broken terrain sun-warmed **dry** on a rise".
  Authoring work; its own campaign.
- Settlement names are phonotactically unpronounceable
  (`Bzhobkobzhshashzhzash`). Touches language-domain seed derivation; needs
  epoch care.

The 61 lexeme / 94 percept registry gaps are **not** defects — they are
tagged `pending(wave-cognition)`, correctly typed negative space.

## 2. Design

### 2.1 Occlusion is the second occluder (F2)

The codebase already has one occluder and does not name it as such:
`Venue::DaySky`/`NightSky` *is* an occlusion partition — daylight hides the
stars. Cloud is the second occluder, and it was never built.

Mechanism: **graded multiplicative attenuation, then a visibility floor.**

- `hornvale_kernel::PerceptionLens` is already multiplicative per-venue and
  `observe` already skips *all* arithmetic when the lens is identity
  (`kernel/src/phenomena.rs:135`). That is the byte-identity escape hatch,
  pre-built.
- A new `worldgen::occlusion_lens(state, cloud) -> PerceptionLens` returns
  **exactly `identity()` under clear skies, by construction** — the same
  discipline `perception_lens` documents at the goblin baseline
  (`windows/worldgen/src/lib.rs:3065`).
- It **composes** with the species lens component-wise (`PerceptionLens::
  compose`). This is load-bearing: `perception_lens` is already non-identity
  for non-goblin species, so occlusion must multiply with it, not replace it.
- `observe` gains a **visibility floor**: after weighting, phenomena whose
  salience falls below the floor are dropped, not merely demoted.

Why graded rather than binary: a vast moon behind overcast is still a glow; a
dim red star is gone. The floor produces *different survivor sets* per weather
state — cirrus keeps most of the sky, overcast keeps the moons as a smear,
storm keeps nothing — from one multiplication and one comparison.

Two consequences fall out for free:

- **Ambient rises.** Attenuating the sky venues raises Ambient's relative rank
  in the existing salience sort. Physically right: on an overcast night you
  notice the closeness of the air, not the stars.
- **The occluder promotes itself.** When the deck hides the sky, the deck is
  the salient thing.

**Prose is rendered from the surviving phenomena**, never gated
independently. One invariant, one source of truth. Two independent guards on
one invariant is The Turnstile's documented failure mode — each layer green,
neither able to see the other.

> **Corrected at plan time (ledger #6).** This was not directly implementable:
> `GeneratedSky::sky_at` builds its description as a monolithic `String`, so
> there are no per-clause phenomena to render from. The shipped mechanism
> passes an abstract `Visibility` ratio into the sky providers, and astronomy
> decides for itself what survives — which is *truer* to this section's own
> constraint, since a domain still never learns that weather exists, and the
> knowledge of which bodies are bright stays where it belongs. The
> one-source-of-truth property holds: prose and salience both derive from the
> same weather at the same cell. The function is `occlusion(state, cloud)`,
> returning both the lens and the visibility, not `occlusion_lens` as named
> above.

In-repo precedent for suppressing a contradictory clause at all:
`REGIME_FLOOR_MM` (`windows/worldgen/src/lib.rs:2333`) drops the
seasonal-regime word when rainfall would make "monsoon" read as nonsense.

### 2.2 The observer's own sky (F3)

`sky_report_from` gains an explicit observer-cell parameter, supplied by the
caller rather than resolved from the flagship. `windows/vessel/src/vantage.rs:34`
passes the walker's room cell; the almanac passes its sample site.

The kernel already models this: `ObserverContext.position` is documented as
the observer's placed position, consumed by providers to cull the visible sky
(SEQ-4/SEQ-5, `kernel/src/phenomena.rs:91`). Weather should not be the one
axis pinned to the capital.

The settlement-less fallback becomes **named behaviour** rather than an
accidental `CellId(0)`.

### 2.3 Bare compass tokens (F1)

The verb dispatch gains a fallthrough: an unrecognised verb that
`parse_compass` accepts routes to `go`. `n`, `north`, `se`, `southeast` all
work; the error message for a genuine non-verb is unchanged. No new parsing —
the parser exists and is already tested.

### 2.4 Two surfaces for strangeness (F4)

Deliberately **two surfaces at different altitudes**, because a single
god's-eye list would destroy the thing it exposes:

- **Verification surface** — `hornvale locale --strange [--limit N]`, listing
  coordinate, biome, and *descriptor* per site, published as a drift-checked
  book artifact. The descriptor is required, not cosmetic: the sites are
  differentiated by negation vector (energy × kingdom × endemic), and a bare
  coordinate list would render 101 wonders as 101 identical rows.
- **In-world gradient** — surface strangeness on the **existing** `map` verb,
  so a walker gets a direction to follow rather than an answer. No new
  mechanic.

> **Not shipped (ledger #7).** The `map` verb proved to be a substantial chart
> renderer with its own zoom and bounds handling rather than the wiring job
> this section assumed, so the gradient was dropped at plan time under §5's own
> escape hatch. The verification surface shipped alone; the gradient is
> registered as `LOC-strangeness-gradient`.

## 3. Determinism analysis

> **Corrected during execution — read this first.** The claim below that
> "phenomena are a *read*, not committed facts" is **false for the genesis
> path**, and was corrected in Task 3. `derived-from-phenomenon` is a
> *committed predicate*: genesis observes phenomena to derive which deities a
> people believes in and how its settlements are named. Wiring occlusion into
> the observation path took seed 42 from 7,350 facts to 7,126 and its pantheon
> from **48 deities to 25**.
>
> The no-epoch conclusion survives, but only because occlusion is now confined
> to *presentation* accessors and kept off the genesis path entirely — not for
> the reason this section originally gave. See ledger entry #8 and the
> chronicle.
>
> The methodological lesson: verifying one route (`SkyReport` carries no
> `Serialize` — true, and checked) is not verifying the claim. The check that
> actually caught it is the total one: build a seed-42 world with both
> binaries and `cmp`.

**No epoch suffix is required.** ~~Phenomena are a *read*, not committed facts:
no ledger content, no save-format field, no stream-label change, no change to
stream consumption order.~~ *(Superseded — see the correction above.)* The
conclusion holds because occlusion touches only presentation accessors: no
ledger content, no save-format field, no stream-label change, and no change to
stream consumption order. World bytes are unaffected — verified by `cmp`, not
inferred — and only rendered artifacts move.

**Byte-identity for clear-sky worlds is by construction** (identity lens ⇒
`observe` performs no arithmetic at all) and must be **tested, not asserted** —
a determinism test pins a clear-sky world to its pre-campaign bytes.

**Expected artifact drift, verified rather than inferred:** seed 42 at day 0
*is* overcast, so the drift is real. `grep -rl "keep their stations"
book/src/` returns exactly 3 files; the wider cloud-phrase grep returns 5
gallery files:

- `book/src/gallery/almanac-seed-42-sky.md`
- `book/src/gallery/almanac-seed-42.md`
- `book/src/gallery/almanac-seed-42-locked.md`
- `book/src/gallery/possession-seed-42.md`
- `book/src/gallery/possession-over-time-seed-42.md`

These regenerate as part of the campaign.

**`knows` must not be corrupted.** Possession absorbs the room projection into
its knowledge ledger each turn (`windows/vessel/src/session.rs:596`).
Occlusion attenuates what is *seen now*; it must not overwrite or erase
knowledge the walker already holds. This gets an explicit test.

## 4. Testing

- **F1:** scripted possession asserting every token in the rendered
  `Ways on:` line is accepted — the exact bug, closed against regression.
- **F2:** per weather state, the survivor set is asserted (storm ⇒ no
  celestial phenomena; overcast ⇒ moons survive, faint stars do not; clear ⇒
  set unchanged). Plus a **composition** test: prose and the phenomena list
  never disagree about what is visible.
- **F2 byte-identity:** a clear-sky world is byte-identical to its
  pre-campaign bytes.
- **F2 composition with species:** occlusion × a non-goblin species lens
  equals the component-wise product.
- **F3:** two observers, same day, different cells, differing weather — the
  assertion that currently cannot hold.
- **F3:** a settlement-less world (seed 123) renders its named fallback rather
  than silently reading cell 0.
- **F4:** the strange-site listing is non-empty on seed 42, carries distinct
  descriptors, and round-trips as a drift-checked artifact.
- **`knows`:** a walker who observed a clear sky retains that knowledge after
  the sky clouds over.

Property tests follow the existing batteries' shape
(`domains/astronomy/tests/genesis_properties.rs`).

## 5. Risks

- **Salience-floor tuning is a judgement call.** The floor value decides what
  survives an overcast. It is a rendered-output constant, not a save-format
  one, so it is revisable — but it should be chosen against the measured
  seed-42 distribution, not guessed.
- **Artifact churn is the campaign's largest diff.** Five gallery files move;
  reviewers should read the code diff separately from the regenerated prose.
- **F4's `map` gradient is the least-specified piece.** If it proves to need
  real design, it drops to the followup register and the verification surface
  ships alone.

## 6. Out of scope / registered

To the idea registry, not built here:

- Cloud-base altitude: an observer above the deck sees clear sky.
- Moonlight as an occluder — a full moon washing out faint stars, which
  astronomy could own with no weather knowledge at all.
- NPC rumor propagation and Book/`consult` integration as strange-site
  discovery paths.
- Marine biome prose (10 biomes) and pronounceable settlement names.

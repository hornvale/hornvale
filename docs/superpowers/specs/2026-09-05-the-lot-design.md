# The Lot — design

**Campaign:** The Lot · **Decision block:** 0796–0805 · **Ledger:**
[`2026-09-05-the-lot.md`](../ledgers/2026-09-05-the-lot.md)

**Status.** Spec at G3 — written under autopilot, awaiting Nathan's review
before the implementation plan is written. Task 0 (§1) has run; nothing
else has.

*A lot is what chance draws, and a person's lot is the life they were dealt.
Any Human Ever (anyhumanever.com) draws one life from the hundred billion
humans who have ever lived and tells it, every sentence sourced, as well as
the data allow. Hornvale can do the same thing to a seeded world — and
because the world's only data is its own ledger, every sentence the story
cannot say is a measurement.*

---

## 1. Task 0 — the population a lot is drawn from

§1 is filled from `windows/worldgen/tests/suite/lot_probe.rs`, run before
any mechanism was written, over the Living Community's nine seeds
(1 / 2 / 3 / 7 / 13 / 42 / 100 / 256 / 777). The probe reads the COMMITTED
occupation facts only — `founded`, `ended`, `peak_population`, `people`,
`cause` — and proxies person-years as `tenure × peak`, which is an **upper
bound** (a community spends part of every tenure below its peak). The
numbers below are the run's output on the worktree at `d17645ea5` plus the
probe file (47.3 s for all nine worlds, dev profile); nothing in this
section is inferred from the bake's code, and the raw output is in the
ledger's Task 0 record.

The nine worlds, from the probe's own output (person-years proxy
`tenure × peak`; Q1–Q4 are the shares of that proxy falling in each 500-year
quarter of the span; "ended" is the share lived in occupations that had
ended by `now`; "25–49" is the share lived in communities whose peak was in
that band; the curve column is `sum of peaks` over alive occupations at
years 0 and 1900):

```
  seed  occs  alive/ended   Q1    Q2    Q3    Q4    ended  25-49  >=50   peoples  curve 0 -> 1900
  ----  ----  -----------  -----  -----  -----  -----  -----  -----  -----  -------  ----------------
     1  1238   313 /  925  0.115  0.210  0.295  0.380  0.301  0.591  0.286    15     1679 ->  8945
     2  1847   508 / 1339  0.093  0.180  0.310  0.417  0.335  0.512  0.276    15     1512 -> 12784
     3   712   287 /  425  0.113  0.198  0.295  0.394  0.138  0.725  0.203    15     1602 ->  9728
     7   656   250 /  406  0.139  0.236  0.295  0.331  0.125  0.584  0.288    15     1931 ->  7539
    13  1061   262 /  799  0.146  0.226  0.284  0.343  0.249  0.479  0.383    15     2043 ->  7248
    42  1212   390 /  822  0.105  0.197  0.300  0.398  0.182  0.579  0.251    15     1636 -> 10754
   100   159    60 /   99  0.235  0.257  0.257  0.252  0.051  0.366  0.000    15      882 ->  1061
   256  1470   385 / 1085  0.079  0.120  0.285  0.516  0.253  0.480  0.298    15     1135 -> 10846
   777  1049   331 /  718  0.109  0.182  0.290  0.420  0.219  0.518  0.346    15     1786 -> 10245
```

Tenure: `p10 = 0` years on eight of nine seeds (at least a tenth of all
occupations open and close inside one epoch), `p50` 50–200, `p90` 650–2000.
Peak: `p50` 12–18, `p90` 35–45, max 81–88 on every seed but 100 (max 36).
`peak == 0`: none. By community fate, `Migrated` is the dominant ending
on the eight growing seeds (7–26% of person-years), then `Fled` (3–7%); `Famine` never
exceeds 1.3% and `Breached` 2.7%. Fifteen peoples appear on every seed —
four goblinoids, three dwarves, five elves, drow, human, gnoll — and no people
holds more than 34% of a world's person-years.

**Verdicts, read off the branch table below:**

- **Person-years skew to the last quarter on 8 of 9 seeds** (Q4 0.33–0.52
  against Q1 0.08–0.15; the sum-of-peaks curve rises 3.5–10× over the span).
  So the site's sentence — *almost everyone was born recently* — is true of
  these worlds too, for the same reason: the world's population is still
  growing at `now`. It was not obvious in advance; a capacity-bound world
  would have read flat. **Seed 100 IS flat** (0.235 / 0.257 / 0.257 / 0.252):
  a 159-occupation world that saturated at 60 living communities by year 400
  and never moved again. The exhibit's hint text therefore says *this world's*
  fact, computed, not the site's sentence copied.
- **The modal lot lives in a community of peak 25–49** (48–72% of
  person-years), not under 25 (the guess this table was written with). The
  hamlet register is right; the number in it is "two or three dozen", not
  "a handful".
- **5–34% of person-years were lived in occupations that later ended**, so
  the community-fate hazard (§4.3) is load-bearing on eight seeds and nearly
  idle on seed 100. That fraction is of *person-years*; a *lot* only
  witnesses the end if its own span overlaps the end year, which H-P3 sizes.
- **`tenure × peak` is a bad proxy in exactly the way §4.1 predicts**: a
  tenth of occupations contribute zero under it because they lived and died
  inside one epoch, and every community spends its rise below peak. The
  committed integral is required, not optional.

**What §1 decides, as a branch table rather than a prediction** (the
autopilot rule: write decision rules, not expectations):

```
  finding in the probe                              consequence for the design
  ------------------------------------------------  ------------------------------------------
  person-years skew to the LAST quarter of the span the When graph tells the site's story:
                                                    "almost everyone was born recently"
  person-years roughly FLAT across quarters         the When graph tells the OPPOSITE story
                                                    — a capacity-bound world — and the
                                                    exhibit's hint text must say so, not
                                                    copy the site's sentence
  >= 50% of person-years in communities of peak <25 the modal lot is a hamlet-dweller; the
                                                    story's "community" slot needs a
                                                    hamlet register, not a town one
  >= 25% of person-years in occupations that ENDED  the community-fate hazard (§4.3) is
                                                    load-bearing, not decorative
  a material share of occupations with peak == 0    tenure x peak is a bad proxy and
                                                    §4.1's committed integral is required
                                                    (it would be anyway; this sizes it)
```

## 2. What The Lot is

### 2.1 The source, and the one rule it runs on

Any Human Ever draws in four staged reveals — **When** (a birth year off the
population curve; log-scaled, because on a linear axis all of history is a
spike at the right edge), **Where** (a place off a population map for that
year), **Life** (the demographic odds at that people/place/year — life
expectancy, died before 5 and 15, causes of death, marriage and children —
then one life drawn from them as a timeline), **Story** (a templated
biography with two dozen stat tiles, each carrying its sources). One seed
pre-rolls the whole human; the permalink is the seed. The story is
introduced by a line saying it is *"created from the statistical reality of
<place> in <year>"*. And the rule everything else follows from: **a tile is
absent, never invented** — where no source covers the region and era, the
site says nothing, because *"an invented figure would be worse than
silence"*.

### 2.2 The analogue, and why it is an instrument here

Hornvale's version draws one life from everyone who ever lived in a seeded
world's 2,000-year history, weighted by births over time and place, and
tells it from the committed ledger alone. The rule that makes the site
honest is, in this repo, already constitutional: `windows/explain` narrates a
world "by reading only committed facts, never the in-memory system, which is
how it validates that the ledger is sufficient" (`CLAUDE.md`, Architecture).
So a slot the story cannot fill is not a rendering gap. It is a fact about
the world's own record: **the ledger holds nothing that would let anyone
say this about an individual.** The lot counts those silences, and the count
is the campaign's headline measurement (§8).

That makes the lot the **dual of the census**. The census aggregates the
ledger into ~200 columns over many worlds; the lot disaggregates one world
into one row. Same instrument family, opposite direction — and the lot sees
a resolution the census cannot: whether the ledger can say anything about a
*person* at all, as opposed to a settlement, a people, or a sky.

### 2.3 Four kinds of individual, and which one this is

The world already holds three kinds of individual, and the lot is a fourth.
The book chapter carries this table; the code carries the distinction in
type names.

```
  kind             who drives it   committed?   where it lives           span
  ---------------  --------------  -----------  -----------------------  ----------------
  founder          the world       YES          domains/person           a dated life
  roster member    the world       no           windows/vessel           the present
  possessed body   the reader      no           windows/vessel           the present
  lot              the reader      no           windows/lot (this)       a whole life,
                                                                          usually dead
```

A founder is the only one the world *remembers*; when a lot's community
descends from a founder the story says so, and that sentence cites a fact
the world committed about someone else. A lot is otherwise the site's
disclaimer made exact: *"This is not a real person, but their life story is
created from the statistical reality of…"* — here, of one occupation record
and everything else the ledger says about that vertex in those years.

## 3. The draw is an observation, not a fact (decision 0796)

A window may not draw world-state (`windows/CLAUDE.md`), and the rule's
reason is that a window which draws "has quietly become a domain with no
registry entry and no pin-isolation test" — a domain in disguise
*commits*. A lot commits nothing and moves no saved world, so the hazard is
absent, and the machinery that guards it (a `Stream` label, a manifest row,
a pin-isolation test) would promise a save-format contract for a value that
is never saved.

So the randomness is the **reader's**, exactly as the site's permalink seed
is the reader's. `LotIndex(u64)` is an input on the same footing as a world
seed, and every choice in a life is a pure hash-expansion of
`(world.seed, index, <choice label>)` in the style of
`hornvale_history::flesh::persona_of` — splitmix over the arguments, no
kernel `Stream`, nothing consumed, nothing committed. The same `(seed,
index)` draws the same life until the ledger it reads changes, which is the
right shelf life for an observation and is pinned by a byte-identity test on
the JSON payload.

**Two key forms.** A drawn lot is `<seed>/<index>`. A *picked* lot — the
exhibit's "select a year" / "select a location" — cannot come from an index
(the site notes the same: a chosen place "cannot come from a seed"), so it is
`<seed>/<index>?year=<Y>&site=<V>`: the index still drives every remaining
choice, the pins override two. The CLI accepts the same pins as `--year` and
`--site`; a pinned year must fall inside the bake span and a pinned site must
be an occupation alive in that year, and both fail loudly otherwise
(`LotError`, the physical reason named — the pin discipline of `GenesisError`).

## 4. Mechanism

Everything in §4 is deterministic in `(World, LotIndex, pins)`, uses the
kernel's `math.rs` for every transcendental, quantizes nothing in the compute
path, and reads the ledger and the authored species registries only.

### 4.1 The draw weight: `occ-person-years` (decision 0797)

The ledger commits per occupation `founded`, `ended` (absent while alive) and
`peak_population`, and **no trajectory** — `Occupation::delve_depth_m`'s doc
says so in as many words and then makes the argument this section reuses:
*"the working is the integral of a live quantity over a tenure … the ledger
keeps neither trajectory … so the field commits exactly the half nothing can
re-derive."*

The bake therefore tallies, per community, the integral of its live
population over its tenure (`Σ population × epoch_years` across the epochs it
was open, plus the closing partial epoch), and `history_emit` commits it as
**one new functional Number fact per occupation, `occ-person-years`**, in the
`domains/history` predicate roster beside `occ-peak`. It is additive, draws
nothing, and moves no seeded value; every saved world's JSON grows by one
fact per occupation, so the byte-golden fixtures, the three seed-42
almanacs' underlying worlds, and the census regenerate (§7). A world saved
before this campaign has no such fact; `occupation_records` reads it as
`None`, and the lot refuses such a world with a message naming the missing
predicate rather than approximating.

**The within-tenure shape.** Between occupations the weight is now exact.
Within one, the lens needs a curve `p(t)` over `[founded, end]` with
`max p = peak` and `∫p = person-years`, and it reconstructs a **rise-then-
plateau**: population rises linearly from the bake's own opening population
for that founding kind (`GENESIS_POP = 10` for a genesis community,
`DAUGHTER_POP = 8` for a daughter — both private constants in
`history_bake.rs` today, to be made `pub` and re-exported) to `peak` over a rise time `t_r`, then holds
`peak` to the end. `t_r` is solved from the area; if no `t_r ∈ [0, tenure]`
satisfies it (a community that died before plateau, or one whose committed
area exceeds `peak × tenure` by quantization), the shape degrades to the
triangle or the rectangle that is nearest in area, and the JSON payload's
`shape` field says which of the three was used. The shape is a **stated
approximation** — it is the one place the lot interpolates — and the spec
states it so the story never has to.

### 4.2 Births

Births per person-year are the stationary-population rate **`b = 1 / e₀`**,
with `e₀` the life expectancy at birth §4.3's hazard yields for that people at
that site. The world's birth curve is then `Σ_occupations b × p(t)`, the
**When** stage's graph, and its integral over the span is the world's
**souls-ever** — how many lives the world has held — the hero number the
exhibit opens on ("`N` lives have been lived in seed 42. Choose one.") and a
census column (§8).

Stationarity is an assumption, and it is stated: the bake grows a community
logistically toward capacity, so most tenures sit near their plateau
(§1's tenure distribution says how often: median tenure 50–200 years against a
rise the bake's `GROWTH_RATE = 0.2` per epoch completes in a few epochs), and a stationary population is the only closed-form
that needs no new authored number. The lens weights births, not deaths;
under stationarity the two agree, which is what makes "everyone who ever
lived" and "everyone who ever died" the same population up to the living.

### 4.3 Mortality: a Siler hazard plus the community's own fate

No mortality model exists in the tree. The lot adds one to `windows/lot`
(a derived read over committed and authored inputs — not a domain, because it
draws nothing and commits nothing), and it has two parts.

**The continuous part** is a Siler hazard in *scaled* age `u = x · 60 / L`,
where `L` is the people's allometric lifespan
(`hornvale_species::allometry::lifespan`, anchored so a 40 kg endotherm reads
60 years):

```
  h(u) = a1 · exp(−b1 · u)          infant: falling exponential
       + a2 · (1 + s)                background: constant, scaled by strife
       + a3 · exp(b3 · u)            senescent: Gompertz, rising exponential
```

`s ∈ [0, 1]` is the site vertex's value in the strife field
(`hornvale_demography::byproducts::strife`), the one measured field that
already means "how dangerous it is to live here". It is not committed; the
lot reaches it through the composition root's `demography_report_from`, the
same derived, seed-free read the predator-pressure field already takes —
the `terrain_of` shape, no draw.
The five constants are authored, frozen here before any life is drawn, and
carry `plumb:` tags naming this section:

```
  a1 = 0.35 /yr    b1 = 1.0 /yr    a2 = 0.012 /yr    b3 = ln 2 / 8 /yr    a3 = 1.0e-4 /yr
```

They are chosen so that a 60-year-lifespan people at zero strife reads
**`e₀ ∈ [28, 36]` years and died-before-scaled-15 `q₁₅ ∈ [0.35, 0.45]`** — the
band the site's own sources give for pre-modern humans (Coale–Demeny West
levels 1–5; the forager syntheses it cites). That band is §8's mechanism-half
assertion H-M1, pinned by a unit test that integrates the survival curve;
the constants may move to land inside the band before unblinding and never
after. **The era does not enter the curve.** An earlier draft scaled `a2`
by tech horizon; the site's own tables show `e₀` barely moving across
pre-modern eras, and every such multiplier would be an authored number with
no measurement behind it. Strife is measured; the era is not; only the
measured thing modulates.

`e₀` and the survival function `S(x)` are obtained by trapezoidal
integration of `h` over `[0, 2L]` at one scaled-year steps — deterministic,
`libm`-only, and cheap enough to compute per `(people, site)` on demand
without caching.

**The discrete part** is where Hornvale is richer than the site. A lot's
occupation may **end inside the life**, and the ledger says how: `occ-cause`
is one of `Famine` `Burned` `Plague` `Fled` `Migrated` `Breached`, `occ-ended-by`
names the agent where there is one, and `occ-founded-from` on some later
record names the daughter community the survivors went to. So at the
occupation's end year the life meets a **discrete hazard**: it ends there
with the probability the bake's own loss constants imply —
`WAR_LOSS = 0.3` for `Burned` / `Breached`, `1 − MIGRATE_SURVIVAL = 0.1` for
`Fled` / `Migrated`, and for `Famine` / `Plague` the same `WAR_LOSS`, because the bake commits no
loss fraction for those two endings and an authored second number would be no
better founded; the payload's `sources` names the constant used — and otherwise
**continues in the daughter community** if the record names one, or ends
with the community if none does (`Fled` with no daughter: the people
scattered, and the story says the ledger does not know where). The
worldline then has two sites, and every site-keyed slot (§5) is asked twice.

**The life course** is the inverse-CDF draw of the death age from `S(x)` with
the discrete hazard spliced in at the end year, using one hash-expanded
uniform per event. A death year past `history-now` means the lot is **alive
at now** with a current age, and the story's tense changes. Maturity is
`age_at_maturity(mass, class, schedule)`; a life ending before it is a
child's.

### 4.4 What is deliberately NOT drawn

Sex (no species carries a model; BIO-3 / SOC-2 are the prerequisite —
ledger #6), marriage and children (no fertility or household model), income,
literacy, height, and an occupation beyond the community's subsistence mode.
Each is a slot in §5 marked **SILENT by design**, so the coverage readout
counts it as a silence the world has and not one the lens introduced.

## 5. The story's slots, and where each answer lives

A **slot** is a named question the story asks of the ledger. The narrator
asks every slot for every life; a slot resolves to one or more `(entity,
predicate)` facts, to a derived read over committed facts (allowed: "a
derived read over an existing field consumes nothing", `windows/CLAUDE.md`),
or to **SILENCE**, which is rendered as an honest sentence ("nothing in the
record says what tongue they spoke") in the prose and as `null` with a
`silent` reason in the payload. The narrator never fills a silence.

```
  slot                 answered by                                          expected today
  -------------------  ---------------------------------------------------  -----------------------
  when                 the drawn birth year (§4.1–4.2); history-now         FILLED
  where                occ-site → the vertex; the settlement's `name`,      FILLED (name only for
                       `latitude` `longitude` `biome` facts via `cell-id`     a living settlement;
                                                                              a dead one is "a
                                                                              <biome> site nobody
                                                                              names now")
  people               occ-people                                           FILLED
  name                 persona_of(handle, seed).name_seed → the people's    FILLED where the
                       Namer (`language/<species>/name/person`)              people's tongue has
                                                                              a person namer; else
                                                                              "no name survives"
  community size       occ-peak; §4.1's shape at the birth year             FILLED
  founded / from       occ-founded, occ-founded-from (the mother community) FILLED
  founder kinship      descent::forebear_of / kinship, generation length    FILLED
  the community's fate occ-ended, occ-cause, occ-ended-by; the daughter     FILLED where it ended
                                                                              in the life; else
                                                                              "still standing"
  tech horizon         occ-tech                                             CONSTANT on the living
                                                                              (The Staple §1)
  function             occ-function                                         NEAR-CONSTANT (98.6%
                                                                              Agrarian)
  tongue               occ-tongue → the language entity's facts             FILLED
  belief               occ-deity → deity-name, deity-epithet, cult-form     FILLED
  what they held true  windows/hearsay over the community's ended kin       FILLED where any
                                                                              ending is remembered
  subsistence          `subsistence` (domains/culture) on the SETTLEMENT     SILENT for a dead
                                                                              occupation — culture
                                                                              facts exist only for
                                                                              living settlements
  standing             `has-caste` on the settlement                        as above
  tribute              pays-tribute-to                                      FILLED where it holds
  dwelling             history::flesh::structures_of (derived)              FILLED
  the mine             occ-delve-depth (Mine function only)                 FILLED for ~1.4%
  climate              `ambient` `heat` `cold` `rain` `snow` at the place    FILLED
  the sky they saw     astronomy eclipse_events over the life span, at the  FILLED (a count and
                       site's longitude                                       the first total one)
  what the ground did  worldgen::hazard::events_in, span CLAMPED to the     FILLED (often "nothing")
                       life (the unbounded-span footgun is named in its doc)
  diet                 the people's authored resource niche                 FILLED (species-level)
  sex                  —                                                    SILENT by design
  marriage, children   —                                                    SILENT by design
  work, income,        —                                                    SILENT by design
  literacy, height
```

The "expected today" column is a **prediction the readout tests**, not a
promise: §8's H-P4 is that `subsistence` and `standing` are silent for the
majority of lots, because culture facts attach to living settlements and most
person-years were lived in communities that have since ended. If that is
falsified the finding is that the ledger's cultural record reaches further
back than anyone thought, and it goes in the chronicle either way.

**Every sentence carries its sources.** The payload's `sources` array lists,
per slot, the `(entity, predicate)` pairs read, and the prose renderer emits
a numbered reference per sentence with a source list at the end — the site's
"Full Sources for this Story" panel, populated from the ledger's own
provenance instead of a bibliography. `windows/historiography::recount` is
the precedent for reading a predicate's registry doc back as prose, and the
narrator reuses it for the source list's captions.

## 6. Delivery

### 6.1 `windows/lot` — the window

A new window crate, `hornvale-lot`, depending on `hornvale-kernel`,
`hornvale-history`, `hornvale-species`, `hornvale-demography`,
`hornvale-astronomy`, `hornvale-language`, `hornvale-worldgen` (for
`occupation_records`, `present_year`, `descent`, `hazard::events_in`,
`terrain_of`) and `hornvale-historiography` (for source captions). Public
surface, every primitive tagged for `type-audit`:

```
  LotIndex(u64)                                    the reader's key
  Pick { year: Option<f64>, site: Option<Vertex> } the exhibit's overrides
  draw(&World, LotIndex, &Pick) -> Result<Life, LotError>
  curve(&World) -> Curve                           births per epoch, per people; souls_ever
  places(&World, year: f64) -> Vec<Place>          occupations alive that year, with birth weight
  odds(&World, people, site, year) -> Odds         e0, q_maturity, the hazard's parts, strife
  narrate(&Life) -> String                         the four stages as prose, sources numbered
  life_json(&Life) -> String                       `lot/life/v1`, quantized at emit
  curve_json / places_json / odds_json             the exhibit's other three payloads
```

`Life` carries the worldline, the drawn events, every slot's value-or-silence
with its sources, and `shape` (§4.1). Nothing in the crate holds a `Stream`.

### 6.2 `hornvale lot` — the CLI

```
  hornvale lot (--world <PATH> | --seed <N>) [--index <K>] [--year <Y>] [--site <V>]
               [--count <M>] [--json]
```

Prints the four stages as prose (default) or the `lot/life/v1` payload
(`--json`); `--count M` prints lots `K..K+M`. `--year`/`--site` are the pins
of §3 and refuse an impossible pin with the reason.

### 6.3 The gallery page — the committed artifact

`book/src/gallery/generated/the-lot-seed-42.md`: **Ten Lives of Seed 42**,
lots 0–9, prose with sources, preceded by the world's souls-ever line and its
births-per-century table (the When graph in text). Written by
`scripts/regenerate-artifacts.sh`, declared **by file name** in
`docs/generated-paths.txt` (the already-declared-directory hazard, The
Stope), `git add`-ed in the same commit that introduces it, and drift-checked
like every other. It is the text the exhibit must reproduce byte for byte
from the same seed.

### 6.4 The exhibit — the four-stage reveal in the book

`clients/lot/` (Deno, pinned to the same 2.9.2 as the other two clients)
bundled to `book/src/gallery/lot.js` (committed, drift-checked) and mounted
on `book/src/gallery/the-lot.md`, the way the Casement mounts on
`possession-live.md`. It builds the world in a worker through
`clients/world-wasm` (deploy-built wasm, never committed — decision 0052)
and reads four new ABI entry points, all `hw_*`, all returning the window's
JSON into the existing out buffer:

```
  hw_lot(index: u64) -> i32                       `lot/life/v1`
  hw_lot_pinned(index: u64, year: f64, site: u32)  the picked form
  hw_lot_curve() -> i32                            `lot/curve/v1`
  hw_lot_places(year: f64) -> i32                  `lot/places/v1`
```

The client owns the theatre and nothing else: the log/linear axis switch,
the spin-and-settle reveals, the crosshair hopping between populated sites,
the timeline that assembles the life's sentence event by event, the
"select a year" / "select a location" pick modes, and the permalink
(`#<seed>/<index>` or the pinned form). Every number it shows is a field of a
payload; it computes no demographic quantity of its own. Its gate is
`make lot-check` — `deno fmt --check`, `lint`, `check`, `test`, a
build-and-bundle-diff, and the **byte-identity smoke**: `hw_lot(0)` for seed
42 against `hornvale lot --seed 42 --index 0 --json`. It joins the
`clients` lane set so the chamber runs it at every stage gate and merge.

**Two things the exhibit does not do.** It shows no portrait — the site's
optional locally-generated image is a client nicety with nothing behind it
in the world, and models author, dice roll (0009). And it never draws a life
client-side from the odds payload: two readers of one permalink must see one
life, which is only true if the wasm draws it.

## 7. Determinism obligations

- **No new stream.** `LotIndex` expansion is pure hash arithmetic over
  `(seed, index, label)`; no `streams.rs` label, no manifest row. A
  source-scan test pins that the crate never names `Stream` (the kernel
  exports one, so the guarantee is by inspection, not by the dependency
  graph).
- **One new fact, additive.** `occ-person-years` is a derived tally, not a
  draw; it consumes nothing and moves no seeded value. It does change every
  saved world's bytes, so: `make rebaseline-goldens`, `make rebaseline`, and
  a census refresh at close (`make sluice-census`). Old world files refuse to
  draw rather than approximate.
- **Quantize at emit only.** The four JSON payloads quantize through
  `hornvale_kernel::quantize`; `e₀`, the survival integral and the shape run
  at full precision.
- **Byte-identity across the ABI.** The smoke test compares `hw_lot(0)` to
  the CLI for seed 42, the same instrument `world-check` already runs for
  the scene payloads.
- **No wall clock; no `HashMap`.** Unchanged.
- **Time.** The bake reasons in years and the ledger in days
  (`history_emit::ledger_day_of_bake_year`); the lot reasons in bake years
  like `present_year`'s consumers do, crosses the seam exactly once on read,
  and reports years in every payload.

## 8. Preregistered measurement (decision 0016)

Frozen here, before the mechanism is written. Two halves, asserted
separately (the mechanism half cannot be rescued by the prediction half and
vice versa).

**Mechanism half — unit-pinned, world-independent.**

- **H-M1** For `L = 60`, `s = 0`: `e₀ ∈ [28, 36]` and `q₁₅ ∈ [0.35, 0.45]`.
  The constants of §4.3 may be moved to land here before unblinding, never
  after, and every move is in the ledger.
- **H-M2** `e₀` is strictly increasing in `L` and strictly decreasing in `s`
  (monotone in both inputs, checked on a grid).
- **H-M3** The reconstructed shape integrates to the committed
  `occ-person-years` to within `1e-9` relative on every occupation of seed
  42, and `max p = peak` wherever the rise-then-plateau or triangle applies.
- **H-M4** A pinned lot with `year`/`site` inside the span draws; one outside
  refuses with `LotError` naming the reason; the same `(seed, index)` yields
  a byte-identical payload across two builds.

**Prediction half — over the nine seeds, 200 lots each (indices 0–199), read
by a lab study `studies/the-lot.study.json` whose metrics are code in
`windows/lab`.**

- **H-P1** The share of lots born in the last quarter of the span is
  **≥ 0.33 on the eight growing seeds and in `[0.20, 0.30]` on seed 100** —
  births track person-years under §4.2's stationarity, and §1 measured the
  person-years shares directly. The exhibit's When-stage hint is written
  from this number per world, never from the site's sentence.
- **H-P2** The median scaled age at death `u` across a seed's lots lies in
  `[10, 35]` on every seed — a life in this world is short, and the
  short end is children.
- **H-P3** Between **3% and 20%** of lots on each of the eight growing seeds
  witness their community's end inside their own life (the discrete hazard
  of §4.3 fires), and **under 3%** on seed 100. Derived from §1: 12–34% of
  person-years sit in occupations that ended, a life spans ~30 years against
  tenures of 50–200, and only a life overlapping the end year witnesses it.
- **H-P4** `subsistence` and `standing` are **SILENT for a majority** of lots
  on every seed — the ledger's cultural record attaches to living
  settlements only.
- **H-P5** The mean number of FILLED slots per lot, out of the §5 table's
  non-by-design rows, is at least **14 of 22** on every seed; the by-design
  silences are excluded so the number measures the world, not the lens.
- **H-P6** `souls-ever` for seed 42 lies within a factor of 2 of
  `Σ person-years / 30` (the stationary rate at the calibration band's
  centre) — a consistency check that the births curve integrates to what
  §4.2 says it should.

A falsified prediction is the finding, not a failure; the chronicle carries
it either way, and no constant moves after unblinding without a ledger entry
saying so.

## 9. Non-goals

Sex, marriage, children, income, literacy, height (§4.4); a per-epoch
population series (§4.1 commits the integral, never the path); an authored
era multiplier on mortality (§4.3); a portrait (§6.4); a lives corpus
(`NARR-lives-corpus`, captured); possessing a lot (`PLAY-possess-a-lot`,
captured); any change to the bake's dynamics — the campaign reads the bake,
it does not tune it.

## 10. Risks

- **Culture facts are alive-only**, so most lots' cultural slots are silent
  (H-P4 predicts it). This is the intended finding, but the prose must read
  as an honest silence, not a broken template.
- **The site → biome join** goes vertex → `cell-id` → place entity, and only
  vertices that condensed a settlement have a place entity. A vertex with an
  occupation but no place entity (a ruin whose settlement was never
  condensed) makes `where`'s biome silent; the readout counts it.
- **`events_in`'s unbounded span** — clamp to the life's own years, as its
  doc demands; a test pins that a 2,000-year life does not iterate a
  million blocks.
- **A people whose tongue has no person namer** produces an unnamed lot.
  Legal, and the site's own common case ("most lives here are anonymous").
- **Genesis in the browser** is the Casement's cost ("takes a few seconds"),
  now paid by a second exhibit; nothing new, but the page must say so as the
  Casement's does.
- **Goldens and census move** — every saved world gains facts. The
  rebaseline lands in its own commit before any prose-moving change, for
  attribution.

## 11. Definition of Done

Chronicle entry (`book/src/chronicle/the-lot.md`) carrying §1's numbers
and §8's verdicts; retrospective (`docs/retrospectives/the-lot.md`); the
gallery page and the exhibit page in `book/src/SUMMARY.md`; a book chapter
section on the four kinds of individual (§2.3); a freshness sweep of
`book/src/open-questions.md` if a Gradient bet moves; the idea registry
flipped (`NARR-the-lot` → shipped, `NARR-lives-corpus` and
`PLAY-possess-a-lot` raw with Where cells, BIO-3 / SOC-2 Where cells noting
the sex slot); decisions 0796–0798 recorded; `docs/generated-paths.txt`
carrying the gallery file by name; census refreshed with the `souls-ever`
column; the lot probe left in the tree as a committed, `#[ignore]`d readout.

## 12. Decisions expected

- **0796 — A lot is an observation, not a fact.** Reader-keyed derivations
  that commit nothing need no stream label and no pin-isolation test; the
  hash-expansion pattern of `persona_of` is the sanctioned form, and the
  permalink is the key.
- **0797 — Commit the integral, never the path.** Generalizing
  `delve_depth_m`'s argument: a live quantity's tenure-integral is committed
  when a consumer needs it, as one Number fact; the trajectory is not.
- **0798 — Silence is a measurement.** A lens that narrates from the ledger
  must count its own unfilled slots and publish the count; an invented value
  is a defect, and a by-design silence is declared as such so the count
  measures the world.

## 13. Staging (for the plan)

Sequenced for attribution inside one campaign (the fixed per-campaign toll
argues against splitting; the producer change lands before anything that
can move prose):

1. **The fact.** `occ-person-years` tallied and committed; goldens and
   artifacts rebaselined in their own commit; H-M3's shape.
2. **The mechanism.** The Siler hazard and its calibration test (H-M1,
   H-M2); the draw, the life course, the discrete fate hazard (H-M4).
3. **The window and the readout.** `windows/lot`, the slots, the narrator,
   the payloads; `hornvale lot`; the gallery page; the lab study and the
   H-P verdicts; the census column.
4. **The exhibit.** The ABI, `clients/lot/`, the page, `make lot-check`,
   the lane-set row.
5. **DoD.** §11.

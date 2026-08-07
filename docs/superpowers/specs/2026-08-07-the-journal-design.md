# The Journal — a visual design brief for the game client

**Status**: spec, awaiting review (G3)
**Deliverable**: [`2026-08-07-the-journal-brief.md`](2026-08-07-the-journal-brief.md)
— the prompt handed to claude.ai/design.

## 1. What this campaign produces

Not an interface. A **brief** for one, and the reasoning that constrains it.

Hornvale's game client has shipped incrementally — the possession seam (The
Seam), the structured per-turn emit (The Snapshot), the two-pane view (The
Panes), creatures on the map (The Sighting) — and at no point did anyone decide
what it should *look like*. Every visual choice so far was made locally and
defensibly by an implementer with a glyph to pick. The result is legible and has
no design.

This spec fixes that gap by writing the constraints down, so the visual design
can be found rather than accumulated. The output of the design session is a
design system (foundations + previewed components), which is the shape a
claude.ai/design project takes and the shape an implementer can consume.

## 2. Scope

**In**: the game client and every surface inside its frame — the map (both
bands), the prose column, the status strip, object/creature detail views, long
scrollable in-world text, and menus.

**Out**: the Book (`hornvale.github.io`), the Atlas, the Orrery, the Casement
exhibit page. Decision 0059 makes the Book the primary artifact and it has a
register already; the Atlas and Orrery have theirs. A whole-project identity
brief would have diluted the exploration of the one surface that has no design
at all. The brief's foundations are written so they *could* extend later; nothing
in it forecloses that.

## 3. The register

**The Field Journal, palimpsest-inflected.** The interface is a diegetic object —
a bound journal the possessed creature keeps — opened to a two-page spread:
plate left, written entry right, endpaper below.

The register was selected from a 4×4 chart of *animacy of the surface* ×
*hierarchicalness of the frame* (see §8). Three candidates were put to Nathan —
The Field Journal, The Instrument, The Palimpsest — and he chose the Field
Journal as the base with palimpsest marks folded in, adding the governing
constraint in his own words: he wants "a beautiful and effective roguelike
display," and skeuomorphism is a bonus, not the goal. That ordering is written
into the brief as the **legibility law**.

Two properties make the register load-bearing rather than decorative:

- **It resolves the prose-primacy tension structurally.** `CLIENT-prose-primacy`
  flags a real conflict: Constitution §1 and `RENDER-4` make focalized prose
  primary and the tilemap secondary, but layout is not a preference the
  constitution can enforce, and every roguelike player's eye goes to the grid.
  A journal map is *a drawing the character made*, so it carries no truth claim
  and cannot compete with the prose as a rival account of the world. This is
  `CLIENT-alive-map`'s resolution, arrived at independently and adopted.
- **It unifies four main-pane modes under one metaphor.** Map, object study,
  long text, and menu stop being four unrelated screens and become four kinds of
  page in one object.

## 4. What was verified, not assumed

Per the autopilot verification rule, claims about live behaviour were settled by
reading the code, not by reasoning about it.

| claim | how it was settled | result |
|---|---|---|
| the map's epistemic vocabulary | read `windows/scene/src/surrounds.rs:113` | **three** states, not two: `"here"`, `"sensed"`, `"remembered"`; a never-known cell is simply not emitted |
| what a status strip could show | read the `Snapshot` struct in `windows/vessel/src/snapshot.rs` | channels are `me`, `sensed`, `known`, `social`, `narration`, `spatial`, plus `turn`/`day` — **no player vitals of any kind** |
| whether the player's affect is available | grepped `windows/vessel/src/` for drives/affect | the machinery exists (`session.rs` imports `Affect`, `DriveKind`, hunger, fatigue) but is **not emitted for the player**; only `sensed.present[].felt` — *another* creature's interior — reaches the client |
| whether a spec may cite registry IDs | `grep -rl "CLIENT-" docs/superpowers/specs/` | yes — five existing specs do; `docs_consistency`'s ID rules bind `book/`, not `docs/superpowers/` |
| whether the prose carries any span structure | read `Narration` in `snapshot.rs:162` | none: `prose` is a flat `String`, `nouns` carry no offsets |
| whether the kernel's salience reaches the client | `grep -rln "phenomena::"` | no — only `kernel/` and `windows/worldgen/`; the vessel session never consumes it |
| what marks carry | read `Mark` (`surrounds.rs:69`) and `PlanMark` (`plan.rs:112`) | both bands carry marks; `salience` is a **`u32` rank key, lower is more salient**, tagged `bare-ok(index: salience)` |
| whether `social` can be knowledge-filtered | read `knowledge.rs` writers + `grep` for projection call sites | **no join exists**: `social` is keyed by `entity: u64`; `IdentityProjection` writes only `room/<id>` and `settlement/<id>/*`, and `absorb_common` only writes what was *said* |
| whether ground carries an attention quantity | read `hazards_at` (`windows/locale/src/lib.rs:741`) | `(uncanny, heat, cold)` in [0,1] exist, but as a **hazard** triple feeding the danger drive, and explicitly "never a serialization boundary" |
| how many categories colour must carry | counted `Biome` variants; read `RELIEF_LEGEND`, `WaterKind::LEGEND`, `pane_chart.ts`, `SURROUNDS_LENSES` | **22** biomes, **6** ordered relief bands, **4** water kinds — drawn today through **two glyphs** (`~` and one land glyph, "coarse on purpose") under **two** lenses (`terrain`, `colour`) |

The second and third rows changed the design materially. A conventional status
bar has **nothing to render**, so the brief specifies an identity strip and
forbids health, mana, hunger, inventory, XP, level, and gold outright. The
asymmetry the third row names — you can read what others feel and not what you
feel — is written into the brief as something to express rather than repair.

The first row changed the palimpsest layer. `RENDER-11` records that `unknown`
is absorbing, so nothing is ever forgotten and a remembered cell is always a
*true* record; an erasure implying the character had been wrong would be a
fiction today. But re-entering a remembered place moves its cells back to
`sensed`, so the honest gesture available now is **confirmation** — a memory
sketch gone over in confident ink. The brief makes that the register's signature
motion, defines the full correction vocabulary as a **reserved, unused** token
set for when `RENDER-11` ships, and forbids its appearance in any composed
screen.

## 5. Decisions

| # | gate | decision | why |
|---|---|---|---|
| 1 | Q | **Visual-primary hierarchy**: the plate is the large pane, prose a persistent column | Nathan's call, put to him because `CLIENT-prose-primacy` flags it as a constitutional-ordering question rather than an autopilot-resolvable one. Safe under the alive-map framing, which removes the map's truth claim |
| 2 | Q | **Scope = the game client only** | Precedent: decision 0059 (the Book is primary and has a register); YAGNI. Not asked |
| 3 | Q | **ASCII and tiles are one design, two renderings** | Precedent: decision 0022 (the sim emits data, clients render) puts glyph choice entirely client-side over an identical semantic cell stream; 0023 keeps clients' toolchains their own. Not asked. Expressed in the brief as the *cell law* |
| 4 | G1 | **Register = Field Journal + palimpsest**, legibility-first | Nathan's call after an ideonomy pass (2 operators, 1 organon, 3 dimension-prompts; 0 overturns, 1 empty cell found — see §8) |
| 5 | Q | ~~Palette organizes on certainty~~ → **superseded by 7** | Was: ink weight encodes epistemic state, so a danger-coded palette would fight it. The second ideonomy pass showed certainty was on the wrong channel entirely |
| 6 | Q | **The brief ships as a separate file** from this spec | The prompt is the artifact and gets pasted whole; duplicating it inside the spec would drift |
| 7 | Q | **Four channels, assigned: glyph = identity, grid = position, colour = substance, weight = attention** | Weight is the only channel surviving monochrome, a 16-colour terminal, and colour-blindness intact, so it must carry the most important axis. Colour is the channel that may fail and must not carry whether you can trust what you see. Ideonomy pass 2 (2 operators, 1 organon, 3 dimension-prompts; 1 overturn — decision 5) |
| 8 | Q | **A weight level is never transmitted; the quantity is** | Precedent, and it is the same sentence one level down: `kernel/src/phenomena.rs:79` — a phenomenon carries no *text* because the producer cannot know who is looking. It cannot know who is *rendering* either. Not asked |
| 9 | Q | **Marks are ordered by their rank, never weighted by it** | `Mark.salience` is a `u32` rank, not a magnitude; rank 3 of 4 and rank 3 of 40 are different situations, so bucketing a rank into three ink weights is unsound. Not asked |
| 10 | Q | **No standing/disposition surface in this design** | `social` is world truth and its own doc demands a knowledge filter, but no join exists — creatures are not in the knowledge ledger. Cut from The Margin and added to the forbidden list. Not asked; this is the schema's own stated rule applied |

## 6. The attention model, and what the producer owes it

Nathan asked to work the *weight* channel — `dim`/`normal`/`bold` in a terminal,
the full range in CSS — from the data side. The second ideonomy pass produced
the model; reading the code produced the bill.

### The channel assignment

A rendering channel can carry identity, position, substance, or attention.
Glyph carries identity, the grid carries position, colour carries substance,
**weight carries attention**. The argument is robustness, not taste: weight is
the only one of the four that survives monochrome, a sixteen-colour terminal,
and colour-blindness intact, so it must carry the axis a reader most needs to
trust. That is what overturned decision 5 — certainty is important enough that
it cannot live on the channel that may fail.

Attention decomposes into *presence* (the map's three epistemic states),
*salience* (how much a thing demands notice on its own), *novelty* (whether the
referent is already in `known`), and *addressability*. One rule spans both
panes: on the map, attention is presence; in the prose, it is salience and
novelty. A reader learns it once.

Two properties fell out of the dimension prompts and both are load-bearing:

- **Weight runs opposite to durability.** `here` lasts one turn, `sensed` lasts
  while presence does, `remembered` lasts forever (unknown is absorbing). So the
  boldest thing on the page is the most *perishable* thing, not the most
  important. This is a better player-facing rule than "bold = important," and it
  is the actual shape of the data.
- **Below the floor is absence, not a fainter ink.** `VISIBILITY_FLOOR`'s own
  doc: "a star dimmed to a fiftieth is not a faint star, it is a star you cannot
  see." The producer already draws that line, and it is the same line as
  unknown-is-unmarked-paper, reached from the other direction.

Reversibility confirms the register rather than decorating it: `unknown →
sensed` is irreversible (absorbing) and `remembered ↔ sensed` is reversible.
Ink on paper has exactly those two properties.

### What the producer already gives, free

**The noun is the join key across all three surfaces**, and the code says so
deliberately — `surrounds.rs:66` ("`noun` is the examinable key — it is what
joins this chart to the prose's own noun catalog") and the `LegendEntry` doc
("deliberately the same shape as the focalizer's `Focalized.nouns`, because that
identity is what makes map and prose two grains of one lens"). Cross-pane focus
linkage costs nothing today and is the strongest thing the two-page spread can
do that a single pane cannot.

Novelty in the prose is also free: join `narration.nouns` against
`known.entries` client-side. Ship the free joins before spending a schema epoch
— they test whether weight-in-prose earns one.

### The gaps, and what they cost

1. **Prose has no span structure.** `Narration { prose: String, nouns: [...] }`,
   with no offsets anywhere. Recommended shape: append `narration.spans` last in
   key order (the move `spatial` already made — additive, not an epoch), each
   span carrying its own `text` plus optional referent and salience, with `prose`
   retained as the exact concatenation. Assert `spans.join("") == prose`; it is
   cheap and mutation-provable. **Do not use offsets** — UTF-8 byte offsets into
   a string a JS client indexes as UTF-16 is silent mis-highlighting, the
   plausible-and-wrong failure mode the pane code guards against everywhere else.
2. **Salience never reaches the client.** `Phenomenon.salience` is a ratio in
   [0,1], venue-weighted and floored, quantized to 2dp — render-ready, and
   consumed only by `kernel/` and `windows/worldgen/`. Plumbing it into the
   session is the one real producer change this design wants.
3. **`social` has no knowledge join.** Its own doc requires filtering against
   `known`, but creatures are not in `known` — the identity projection writes
   only rooms and settlements, and `absorb_common` writes only what was said. So
   the standing surface is *cut*, not specified (decision 10).

### The substance channel is bigger than the client uses

Colour's load is 22 biomes, 6 ordered relief bands and 4 water kinds. Twenty-two
is past a categorical palette's ceiling and impossible in a sixteen-colour
terminal, so the brief forbids designing twenty-two swatches and asks for
structure instead: the 12 terrestrial biomes lie on a temperature × moisture
classification and *both axes are emitted per cell*, so land colour is a **2-D
surface** over them; the 10 marine biomes are mostly a depth ladder, so they are
a **sequential ramp**; relief is ordinal and reads as elevation shading.
Derived, not authored — which also means it degrades gracefully as the palette
shrinks.

The forced consequence is the finding. In monochrome, colour is gone, so **the
glyph is the only channel that can carry biome** — and the client draws two
(`~` and one land glyph, "coarse on purpose"). Two glyphs against twenty-two
biomes is the largest unused capacity in the interface, and it sits in the
channel a *roguelike* is supposed to be best at. The brief promotes the
character vocabulary from fallback to a first-class foundation.

Minor, but it would have produced a wrong component: there are **two** lenses on
this surface (`terrain`, `colour`), not the four `RENDER-9` discusses — that
registry is the Atlas's.

### One error caught in this spec's own brief

The first draft told the design to be lavish with a Study Plate "of one object
or creature." Checking what backs it: an object carries a name and **one
sentence** (`examine`'s output); a creature carries a label and a felt phrase.
That is a lavish frame around one line — the exact failure the brief was written
to prevent, committed inside the brief, and invisible until someone read the
schema rather than the sentence.

The subject with real depth is **a place**: `locale/room/v2` carries latitude,
longitude, refinement depth, biome, temperature, moisture, elevation, signed
height above sea level, water kind, a strangeness descriptor plus its inner noun
plus a magnitude, the exits, and the three lattice corner cells with integer
blend weights — a locale *is* a weighted blend of three points, which is a fact
worth drawing. The Study Plate is retargeted at places; objects and creatures get
a new, deliberately small **Caption** component instead.

Worth stating as a pattern, because it is the third time this shape has appeared
in this project's plan text: *a component named after a subject is a claim about
that subject's schema, and it needs the same verification as any other claim.*

### Two hazards recorded

- **Two opposite salience conventions share one word.** Kernel:
  `f64` in [0,1], higher is more salient, sorted descending. Scene/vessel:
  `u32`, lower is more salient, sorted ascending, tagged `index` not `ratio`.
  A consumer reading both will invert one eventually. The emit-side field wants
  renaming to `rank`; that is a schema epoch, so it is a note for whenever those
  schemas are next opened, not an action here.
- **The uncanny is a hazard, not a salience.** `hazards_at` returns
  `(uncanny, heat, cold)` in [0,1], feeding the danger drive. Putting `uncanny`
  on weight would reintroduce a threat display on the attention channel — the
  precise thing decision 7 just took off colour. If it ever surfaces, it is
  substance.

## 7. Flagged for review

1. **Decision 5 was overturned by pass 2 and is now decision 7.** Certainty does
   not belong on colour; it belongs on weight, because weight is the only
   channel that survives monochrome and colour-blindness. Colour carries
   substance. Reviewed and accepted by Nathan, 2026-08-07.
2. **Three producer changes are now named, none of them made.** Plumbing
   `Phenomenon.salience` into the session, appending `narration.spans`, and (much
   later) an `agent/<id>` knowledge projection so `social` can be filtered. All
   three are recorded in §6 as *what the design wants*; this campaign changes no
   code. Whether any of them is worth doing is a separate call.
3. **The reserved-correction-marks instruction** asks a design tool to produce
   something and then not use it. That is unusual enough to be worth confirming;
   the alternative is omitting the vocabulary entirely and adding it when
   `RENDER-11` ships.
4. **No save-format, epoch, or determinism-contract surface is touched.** This
   campaign produces documents. Nothing in it constrains a producer; the cell
   law and the data inventory are statements *about* existing schemas, not
   changes to them.

## 8. The ideonomy passes

### Pass 1 — the register

Tuple: operators **organon-construction** + **negation**; organon **chart**;
dimension-prompts **animacy**, **hierarchicalness**, **polarity**. No overturns,
two findings promoted:

- **The empty cell.** Charting *animacy of the surface* × *hierarchicalness of
  the frame* filled fifteen of sixteen cells. *Alive × flat* has no occupant,
  and the reason is structural: an alive surface is alive because it has a
  voice, and a voice needs a position to speak from. Flattening the hierarchy
  deletes the maker of the artifact. This retroactively justifies decision 1 —
  the visual-primary hierarchy is what keeps every alive register reachable, and
  a co-equal pane grid would have foreclosed the whole bottom row of the chart.
- **The polarity flip.** Negating "colour marks danger" yields a palette that
  encodes *how sure the character is* rather than *how dangerous the thing is*.
  Adopted as decision 5 and flagged.

Two further negations were folded into the brief because they cost nothing: the
*diegetic* negation ("the interface sits outside the world" → the UI is an object
the character carries), which is what collapses four main-pane modes into one
metaphor; and the *surface* negation ("the screen is a window" → the screen is a
page the world is drawn on), which is what makes unknown territory read as
unmarked paper rather than as black fog.

### Pass 2 — the weight channel

Tuple: operators **combination** + **tree-finding**; organon **tree**;
dimension-prompts **longevity**, **complexity**, **reversibility**. **One
overturn** (decision 5 → decision 7) and three findings promoted, all in §6:

- **The sibling walk produced the channel assignment.** Walking up from "the
  epistemic state" until the map pane and the prose pane shared a parent gave
  *attention*, whose siblings are identity, position and substance — which is
  the four-channel table. The overturn came out of that walk: certainty had been
  put on colour without asking which channel was the most robust.
- **Longevity ran backwards to weight**, giving "bold means perishable, not
  important" — the single most useful player-facing rule in the design, and one
  that was not visible from the register alone.
- **The combination table priced the work.** Crossing the four attention
  subtypes against the two panes: three cells are free client-side joins, one is
  shipped, one is genuinely absurd (ground does not demand notice — which is
  *why* the map's axis is presence and the prose's is salience), and exactly one
  is a real producer gap.

## 9. Capture manifest

Discards and directions raised and not adopted, recorded so they are not lost:

- **The Instrument** (mechanical × fixed frame — brass, graticules, permanent
  lens caption). Rejected: it is the register the Book, Atlas and Orrery already
  occupy, and an instrument *claims* truth, which fights the alive-map framing.
- **The Vessel** (alive × counter-hierarchical — the frame is the possessed
  body's own sense, attention is the layout because the attention is the
  creature's). Not rejected on merit; deferred as the most ambitious cell on the
  chart and the one furthest from "an effective roguelike display."
- **The Weather Glass**, **The Attending Eye**, **The Scriptorium**, **The
  Familiar**, **The Fading Chart** — the remaining occupied cells, held in §8's
  chart rather than elaborated.
- **The tense pane** (a pane showing past or future rather than the present).
  Costs *no new producer work* — `why` and The Foresight both ship — but is out
  of scope for a first visual language. Belongs to `CLIENT-replay-and-tenses`.

**Rows added** (2026-08-07): `CLIENT-journal-register` (`spec'd`) and
`CLIENT-voice-needs-a-position` (`raw`, the alive × flat finding).

**Rows added** from pass 2 and the data-side read (2026-08-07):
`CLIENT-four-channels`, `CLIENT-bold-means-perishable`, `CLIENT-two-saliences`,
`CLIENT-mandated-filter-without-a-key`, `CLIENT-glyph-carries-biome`,
`CLIENT-substance-is-a-surface-not-a-swatch-set` — all `raw`. Their content:

- The **four-channel assignment** and the robustness argument that orders it —
  the reusable half, since it is a claim about rendering channels generally and
  not about this register.
- **Bold means perishable**: weight running opposite to durability, and the
  three-state longevity ladder that produces it.
- **The two salience conventions**, as a defect row: `f64` [0,1] higher-is-more
  in the kernel against `u32` lower-is-more in scene/vessel, one word, opposite
  directions. Names the rename (`rank`) and that it is a schema epoch.
- **The `social` join that does not exist** — a channel whose own doc mandates a
  filter for which no key is emitted. The general shape (a schema documenting a
  discipline that nothing can currently perform) is worth a row of its own.

## 10. Non-goals

- No implementation. This campaign ends at an approved brief; building the
  design into `clients/vessel` is separate work.
- No producer change. If the design wants a channel that does not exist (player
  affect being the obvious candidate), that is a finding to record, not a change
  to make here.
- No change to the Book, Atlas, Orrery, or Casement.
- No decision about *when* the client gets built out. The brief is a document
  that can sit until it is wanted.

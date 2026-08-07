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
*hierarchicalness of the frame* (see §7). Three candidates were put to Nathan —
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
| 4 | G1 | **Register = Field Journal + palimpsest**, legibility-first | Nathan's call after an ideonomy pass (2 operators, 1 organon, 3 dimension-prompts; 0 overturns, 1 empty cell found — see §7) |
| 5 | Q | **Palette organizes on certainty, not danger** | Derived from the palimpsest choice: ink weight already encodes epistemic state, so a danger-coded palette would fight it. Belief has been a derived fold since The Surmise, so this is renderable. **Flagged for review** — Nathan did not explicitly buy this |
| 6 | Q | **The brief ships as a separate file** from this spec | The prompt is the artifact and gets pasted whole; duplicating it inside the spec would drift |

## 6. Flagged for review

1. **Decision 5 (certainty-not-danger palette)** is an inference from the
   register, not something Nathan chose. It is the ideonomy pass's sharpest
   output and also the easiest thing in the brief to cut without disturbing
   anything else.
2. **The reserved-correction-marks instruction** asks a design tool to produce
   something and then not use it. That is unusual enough to be worth confirming;
   the alternative is omitting the vocabulary entirely and adding it when
   `RENDER-11` ships.
3. **No save-format, epoch, or determinism-contract surface is touched.** This
   campaign produces documents. Nothing in it constrains a producer; the cell
   law and the data inventory are statements *about* existing schemas, not
   changes to them.

## 7. The ideonomy pass

Tuple: operators **organon-construction** + **negation**; organon **chart**;
dimension-prompts **animacy**, **hierarchicalness**, **polarity**. One pass, no
overturns, two findings promoted:

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

## 8. Capture manifest

Discards and directions raised and not adopted, recorded so they are not lost:

- **The Instrument** (mechanical × fixed frame — brass, graticules, permanent
  lens caption). Rejected: it is the register the Book, Atlas and Orrery already
  occupy, and an instrument *claims* truth, which fights the alive-map framing.
- **The Vessel** (alive × counter-hierarchical — the frame is the possessed
  body's own sense, attention is the layout because the attention is the
  creature's). Not rejected on merit; deferred as the most ambitious cell on the
  chart and the one furthest from "an effective roguelike display."
- **The Weather Glass**, **The Attending Eye**, **The Scriptorium**, **The
  Familiar**, **The Fading Chart** — the remaining occupied cells, held in §7's
  chart rather than elaborated.
- **The tense pane** (a pane showing past or future rather than the present).
  Costs *no new producer work* — `why` and The Foresight both ship — but is out
  of scope for a first visual language. Belongs to `CLIENT-replay-and-tenses`.

Registry rows to add before this campaign closes: a `CLIENT-*` row for the
journal register itself, and one for the alive × flat structural finding in §7.
No row is added by this spec; that is a close-time action.

## 9. Non-goals

- No implementation. This campaign ends at an approved brief; building the
  design into `clients/vessel` is separate work.
- No producer change. If the design wants a channel that does not exist (player
  affect being the obvious candidate), that is a finding to record, not a change
  to make here.
- No change to the Book, Atlas, Orrery, or Casement.
- No decision about *when* the client gets built out. The brief is a document
  that can sit until it is wanted.

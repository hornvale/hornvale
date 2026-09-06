# The Ken — design

**Campaign A of three**, over a batch of 13 client bug reports (Nathan,
2026-09-05). Decision block **0816–0825**.

*Ken*: the range of what a creature perceives and could know.

## 1. The five reports, and their one cause

| # | Report | Verified as |
|---|---|---|
| 5 | "The room's description does not need to indicate that the sun is a yellow dwarf." | `The sun, a yellow dwarf (G), climbs the morning sky.` |
| 6 | "Day 0.5 is not helpful" | `day 0.01172` |
| 7 | "`[room $whatever, day 0.5]` is not helpful." | `[room 3733133217, day 0]` |
| 2 | "'No direction here is closed' is not helpful." | that clause plus all eight bearings, every outdoor turn |
| 12 | "I see a white dragon… it says 'a black-dragon'." | two defects; see §3.4 |

They are one campaign because they are one defect class: **content from a
frame the player character does not occupy is being spoken in the character's
voice.** Three of the five leak *author's-frame* knowledge (a spectral
classification, a facet id, a decimal day); two leak *engine-frame* structure
(a vacuous exits clause, a display noun built from a field the parser never
reads).

## 2. The organising principle: two frames, one of which already exists

This project already draws the distinction and already has a marker for it —
the `!` prefix. `!whoami`, `!why` and `!examine` are the author's frame;
everything else is the creature's.

| frame | surfaces | may contain |
|---|---|---|
| **author's** | `windows/book`, the almanac, every `!`-prefixed verb | ids, sim quantities, taxonomy, provenance |
| **creature's** | `look`, `examine`, the turn header, the presence line | only what this body could perceive or a person here could know |

The rule this campaign enforces is not new either. `domains/astronomy/src/facts.rs:14`
already states it, and the leak is a violation of a rule the domain wrote down:

> The host star's spectral class, committed as its registered concept id
> (e.g. `"yellow-dwarf"`), **never as Morgan-Keenan prose — no creature in
> this world could have invented that taxonomy.** `windows/book` renders the
> id as the author's-frame display…

So §4.1 is not a new policy; it is enforcement of decision-grade prose that
already exists, at the one surface that ignores it.

**Nothing is deleted, only re-framed.** Every datum this campaign removes from
the creature's frame already has an author's-frame home: the facet id and the
day fraction are both in `!whoami` today (verified: `A white-dragon of the
wilds (agent 9630022852472602624), day 0, room 3733133217.`).

## 3. What was verified before this spec was written

All at `5dbfefee3`, in this worktree. No claim below is an inference.

1. **The star-class leak** is in committed output —
   `book/src/gallery/possession-over-time-seed-42.md`.
2. **The header** is `[room <id>, day <f64>]`, and in the interior band
   `[chamber 978618474718145, day 0.01172]`.
3. **The exits clause** is built at `windows/vessel/src/session.rs:7063` as
   `"{lead}; the nearest ground lies {ways}."`, where `lead` is
   `"No direction here is closed"` whenever no bearing is refused. Bearings
   are refused only at the 24 cube-corner facets, so the common case states
   twice, vacuously, that everything is open.
4. **Report 12 is two defects, not one**, reproduced with a staged tableau:
   - `presence_line` (`session.rs:9866`) renders a wild group as
     `format!("a wild {species}")`, **discarding `labels`** — so the noun
     shown is built from a different field than the noun `examine` matches
     (`npc.label.to_lowercase() == wanted`, exact). The game displays a noun
     and then denies it exists.
   - an ambiguous needle silently resolves to the **first roster match**:
     `examine dragon`, with three dragons present, answers `black-dragon`.

## 4. The changes

### 4.1 The star class leaves the creature's frame (#5)

Drop the appositive from the sky sentence. The clause that follows it is
already the creature-legible half:

- now: `The sun, a yellow dwarf (G), climbs the morning sky. The light is golden.`
- after: `The sun climbs the morning sky. The light is golden.`

`daylight_words` keeps reading `class_name` — the class still *drives* the
prose, it just stops *appearing* in it. `windows/book` is untouched: the
author's frame is where that display legitimately lives.

### 4.2 The turn header becomes orientation, not telemetry (#6, #7)

`[room 3733133217, day 0]` -> a place and a time of day, in the character's
own terms. The time-of-day vocabulary is **not invented**: `domains/astronomy`
already produces phase language (`Night`, `Twilight`, `climbs the morning
sky`, `stands high in the sky`, `sinks toward evening`), and the header reuses
that source rather than minting a second one that could disagree with the sky
line one row below.

The id and the fractional day are not lost — see §2.

**Implementer's latitude.** The exact header shape is a rendering decision to
be settled against real transcripts, not prescribed here from outside the
code. What is fixed: no raw facet id, no decimal day, and the time phrase must
derive from the same source as the sky line.

### 4.3 The exits clause reports the exception, not the rule (#2)

Openness is the default; a wall is news. The clause earns its line only when
something is closed.

| condition | line |
|---|---|
| nothing refused, every bearing has ground | **no line at all** |
| nothing refused, some bearings lack ground | name the ground that exists |
| a bearing refused (the 24 corner facets) | name the refusal, as today |

The middle row is deliberately a branch rather than a prediction: whether
`ways` is ever a proper subset while `refused` is empty is a property of
`exits` vs `heading_rose`, two different computations, and the implementer
will determine it by reading them.

### 4.4 A displayed noun must be a resolvable noun (#12a)

`presence_line` renders the **label**, which is what `examine` matches. This
is the repo's own §6 contract ("every depicted noun must answer") and its
`a_noun_at_both_grains_resolves_to_one_datum` test, applied to the one roster
that escapes them.

This surfaces the known wild-label defect recorded at `liveness.rs:8783` —
labels read `"a wild {species}"`, which renders as *"The a wild
carrion-crawler looks lost"*. Displaying the label unchanged would move that
defect into the presence line. So the wild label convention loses its article,
matching the settled convention the same doc comment states ("the label
carries NO article").

**This moves committed goldens.** See §6.

### 4.5 An ambiguous noun is refused, not guessed (#12b)

Returning an arbitrary creature for an ambiguous needle is the worst available
behaviour: it is confidently wrong and gives the player no signal. Refuse, and
say what would have worked.

| typed noun matches | answer |
|---|---|
| exactly one sensed creature | its datum, as today |
| more than one | a refusal naming the candidates |
| none | `nothing_here_named`, as today |

## 5. Testing

Test-driven, per the project's standing practice. Each of §4.1–4.5 gets a test
that **fails on `main` first** — the behavioural red, not a compile error.
§3's reproductions are the starting point: the tableau
(`{"cast":[…three dragons…]}`, seed 42) is a deterministic fixture for §4.4
and §4.5 and should become one.

The §4.3 branch table is three cases and needs three tests; the implementer
determines whether the middle row is reachable before writing its test, and
records the finding either way (an unreachable branch is a result, not a gap).

## 6. Flagged for review

- **Committed goldens and book galleries move.** §4.1, §4.2, §4.3 and §4.4 all
  change possession prose, and four gallery transcripts plus the client
  session fixtures are generated from it. This is expected and is the campaign
  working, but it makes the diff large and mostly mechanical. Regeneration is
  `make rebaseline`; the review surface is the non-generated half.
- **No save-format or determinism contract is touched.** Nothing here changes
  a stream label, a seed derivation, or a quantization boundary. The prose is
  downstream of the ledger, not part of it.
- **§4.4 lands a defect the codebase deliberately deferred.** `liveness.rs:8783`
  left the wild-label article alone specifically because its blast radius is
  committed goldens. This campaign accepts that cost rather than displaying a
  noun it knows to be malformed.

## 7. Non-goals

- The `Room`/`Chamber` vocabulary question (Campaign C).
- Command grammar and rendering — `>`/`<` band transit, `enter <name>`, the
  marquee, facet-level terrain colour, open-water day/night (Campaign B).
- `You see no a key here.` — a deliberate verbatim echo pinned by several
  tests (`session.rs:10925`). Changing it is a separate call.
- The vowel-article defect *"a orange dwarf (K)"* (`windows/book/src/lib.rs:3259`).
  It lives in the author's frame, which §4.1 does not touch.

# Campaign 3 (Year 2): The Tongues — Design

**Date:** 2026-07-08
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-07-year-2-metaplan-design.md` (§7 binds this campaign; Constitution §2 governs)
**Provenance:** The third spine campaign of Year 2, following Campaign Y2-2
(The Eyes, merged a357cd9). The Eyes gave each species its own perception and
therefore its own gods; The Tongues gives each its own **voice** — the sounds
its mouth can make, the shape of the names it coins, and the register in which
it tells its myths. It deletes the vocabulary and syllable stopgaps Y2-1 and
Y2-2 shipped as placeholders. Downstream: The Meeting (comparative studies over
two fully-voiced peoples) consumes what it ships.

---

## 1. Goal

Give the world **language, plural**: a phonology substrate (`domains/language`)
holding a feature-bearing phoneme model and a generative engine; an
**articulatory envelope** as the species crate's third closed vector; per
species-culture phoneme inventories and phonotactics drawn under that envelope;
generative **naming grammars** for settlement names, deity names, and deity
epithets; and a **register** layer that renders belief tenets through
per-species voice — built behind a permanent content→render seam so the future
generative prose grammar can reoccupy it without disturbing anything. The
deliverable is one almanac with **zero English-template proper nouns**, the two
species' pages legibly different in name mouth-feel and myth voice, each
difference recountable by `why` to a named vector dimension.

Explicitly **later** (bright scope line, per the metaplan): no lexicon, no
syntax, no sound-change, no etymology, no language demographics. Names have
*shape and voice*, not glossable meaning. Meaningful toponyms ("Cold-Ford"),
sound-change fossils, pidgins/creoles, and the L2-simplification law belong
with deep time and the interaction layer (MAP-3, LANG-1) and stay there.

## 2. Design principles

1. **Phonemes are feature-bearing segments; spellings are views** (metaplan
   §7). The internal truth is a segment inventory with articulatory features
   (place, manner, voicing; vowel height/backness/rounding). Every surface form
   is a *view* of that truth: a **romanization** (the almanac) and an **IPA
   transcription** (the book, the `phonology` verb). Nothing persists a
   spelling as state. This principle generalizes one level up in §6: a rendered
   tenet is a view of structured belief content, exactly as a romanization is a
   view of a phoneme sequence.
2. **The content→render seam is the campaign's permanent contribution**
   (approved design call). Religion stops building prose inline and commits
   structured *meaning*; a renderer turns meaning into voiced text. v1 fills the
   renderer with templates under psychology-derived voice knobs; the frontier's
   generative oral-formulaic grammar (EXP-1) reoccupies the *same interface*
   later, with the content producer and every consumer untouched. String
   post-processing of finished English was rejected as a dead end — it cannot
   grow into grammar-generation. The seam ships; the simple renderer is the only
   temporary part.
3. **The envelope is authored; the phonology is drawn.** The articulatory
   envelope (which segments the anatomy affords) is authored per-species data,
   the species crate's third closed vector. The phoneme inventory and syllable
   phonotactics are *drawn* from labeled streams, each draw constrained to the
   envelope. Model-card discipline (ADR 0009): every dimension declared authored
   vs. drawn.
4. **Goblin is the baseline; every modulation is identity at the baseline.**
   The articulation vector's scalars are `[0,1]` with 0.5 ≡ goblin; enum
   dimensions default to the goblin variant. Voice-parameter and morphology
   derivations reproduce a neutral baseline at the goblin vector by construction.
   (Byte-identity itself does *not* hold this campaign — §8 — because every
   proper noun regenerates; the baseline discipline instead makes divergence
   mechanical and recountable.)
5. **Ontology-trap posture.** The articulation vector is closed and small
   (§4). The naming grammars are a fixed set of morphological operations, not an
   extensible morphology DSL. Register is three closed voice parameters, not an
   open style system. Widening any of these requires its own campaign.
6. **Layering holds.** `domains/language` depends on `hornvale-kernel` and
   nothing else. It never imports religion, culture, or species; it defines its
   own input structs (`LineContent`, `VoiceParams`) that the composition root
   populates (the `SocietySummary` pattern). Adding language edits no existing
   domain's interface; the composition root wires it.

## 3. The language crate

`domains/language` (crate `hornvale-language`, kernel-only):

- **The phoneme model.** A `Segment` is an articulatory feature-bundle
  (consonant: place, manner, voicing; vowel: height, backness, rounding).
  Segments outside standard IPA are permitted as **authored extensions**
  carrying a defined romanization and a best-effort IPA-adjacent notation. A
  segment renders two ways — `romanize(&Segment) -> String` and `ipa(&Segment)
  -> String` — never stored, always derived.
- **The phonology engine.** Per species-culture, drawn from labeled streams
  (`language/<species>/phonology/…`) under the species' envelope (§4):
  - a **phoneme inventory** (a subset of the envelope-permitted segments,
    biased by the voice-loudness dimension — a low-loudness species down-weights
    high-sonority segments, its exotic manner included);
  - **syllable phonotactics** — onset · nucleus · coda templates with drawn
    complexity.
  The engine exposes name construction (§5) and the renderer (§6). Everything
  is pure and deterministic: same seed + labels → same language.
- **Stream labels** (permanent contracts): `language/<species>/phonology/inventory`,
  `language/<species>/phonology/phonotactics`, plus the name-draw labels in §5.
  Exact label strings settle at the manifest review; published via
  `stream_labels()`.

## 4. The articulation vector

The species crate's **third closed vector** (after 6-D psychology, 3-D
perception), authored per species, goblin = baseline. Abstract feature
dimensions — each a named capacity the phonology engine intersects with the
drawn inventory, so every phoneme difference recounts to a dimension:

| Dimension | Type | Goblin (baseline) | Kobold | Rationale (kobold) |
|---|---|---|---|---|
| labiality | `f64` [0,1] | 0.5 | 0.1 | reptilian/draconic mouth — few bilabials/labiodentals |
| vowel-space breadth | `f64` [0,1] | 0.5 | 0.3 | a tighter vowel set |
| voicing contrast | `f64` [0,1] | 0.5 | 0.6 | — |
| sibilance | `f64` [0,1] | 0.5 | 0.9 | the draconic hiss — sibilant-rich |
| voice loudness | `f64` [0,1] | 0.5 | 0.2 | small/frail/stealthy → a dampable phonology; **authored now, model-card-noted to derive from body/frailty (BIO-1) later** — the nocturnality-banking precedent |
| exotic manner | enum `{None, Trill, Click, Ejective}` | None | Trill | a signature resonant manner the anatomy affords (down-weighted in the draw by low voice-loudness) |

Kobold values are authored data — Nathan's to retune; the design requires only
meaningful contrast and identity at the goblin baseline. The **voice-loudness ×
exotic-manner interaction** is the load-bearing one: a species may be
anatomically capable of a loud signature manner (Trill) yet keep it out of its
names because it is stealthy (low loudness) — recountable as "kobolds can trill,
but their names hiss."

## 5. Naming grammars

Three name-kinds this campaign generates: **settlement names, deity names,
deity epithets.** Place names are excluded (meaningful toponyms are deep-time;
no terrain feature carries a generated name today).

- **Stem** — a sequence of syllables built from the drawn phonotactic templates
  over the drawn inventory; stem length drawn per name-kind (settlements longer,
  deities weightier). Drawn under name-draw labels
  (`language/<species>/name/settlement`, `…/name/deity`, `…/name/epithet`).
- **Morphology per name-kind** — a fixed set of operations: *compounding* (two
  stems joined), *reduplication* (a syllable doubled), *honorific affixation* (a
  bound affix class). Settlement names are bare or lightly-affixed stems; deity
  names are weighty bare stems; **epithets** are the morphologically rich kind
  (compound or reduplicated descriptive stem + optional honorific).
- **Psychology keying** — deity-epithet morphology keys to the psychology
  vector's **status basis**: a `Rank` society (goblin) affixes dominance
  honorifics; a `Knowledge`/`Generosity` society (kobold) builds descriptive
  compounds without dominance marking. The *shape* of a god's title recounts to
  how its people organize authority — identity at the goblin baseline.
- **Uniqueness** — in-world collisions resolved by deterministic re-draw within
  the same labeled stream (advance the draw until the name is unused),
  byte-identical on pinned and unpinned paths and pin-isolation-tested. Retires
  the "Bolzag ×3" collisions the metaplan's observable ending names.
- **Views** — every generated name carries a romanization (almanac) and an IPA
  transcription (book / `phonology` verb), per §2.

## 6. The content→render seam and register

- **Religion emits structured content, not prose.** Today `genesis` builds an
  English tenet string inline and commits it as a `tenet` Text fact. Under the
  seam it commits the **meaning** as structured facts: the deity's generated
  name and epithet (§5), the phenomenon it mythologizes (already committed via
  `derived-from-phenomenon`), its periodicity (eternal vs. cyclic + period),
  its sentiment (watched / mourned-and-feasted / feared), and its rank (already,
  via `high-god`). New registered predicates cover deity name, epithet, and
  sentiment; exact names settle at the concept-registry review.
- **The renderer lives in `domains/language`, behind an input struct.**
  `render_line(content: &LineContent, voice: &VoiceParams) -> String`; language
  defines `LineContent` and `VoiceParams` and never imports religion. The
  composition root maps a belief's committed facts → `LineContent`, derives the
  species `VoiceParams`, and calls the renderer; the almanac and REPL render
  tenets this way at display time. **This signature is the permanent seam** — v1
  fills it with templates, the future grammar fills it later.
- **Voice parameters, derived from psychology** (identity at goblin baseline):
  **formality** (archaic vs. plain connectives), **repetition** (refrain
  echoing), **epithet density** (how many honorifics stack). Keyed to the
  psychology vector — `Rank` → honorific-dense and formal (goblin);
  `Communal`/`Knowledge` → repetitive and descriptive (kobold) — so a myth's
  *telling* recounts to the same vector its gods' *names* do.
- **v1 rendering** — templates assembled under the knobs: choose connective by
  formality, echo a phrase by repetition, stack epithets by density. Register
  applies to the belief tenets religion already produces; **no new myth-corpus
  system is invented this campaign** (bright scope line).
- **Provenance consequence:** `why` now recounts a belief through its structured
  facts ("mythologizes the moon; sentiment mourned-and-feasted; presides") —
  richer than replaying a frozen sentence.

## 7. Composition root and rendering

`windows/worldgen` constructs each species' language from its envelope + drawn
phonology, and:
- generates settlement names (replacing `generate_name`/`generate_species_name`
  and their syllable pools), deity names, and epithets (replacing the deleted
  religion epithet pools), committing names as facts;
- populates `LineContent` from each belief's committed facts and `VoiceParams`
  from the psychology vector, and the almanac/REPL render tenets through
  `hornvale_language::render_line`.
Culture and religion define their own small input structs where they need
language output; no domain-to-domain edge is added.

## 8. Save format, identity discipline, and the re-baseline

**Byte-identity does not hold this campaign** — every proper noun regenerates,
goblin's included — so the identity contract becomes **structural invariants**,
asserted in CI:
- **entity structure unchanged**: every settlement still gets exactly one name,
  every deity a name + epithet; the language layer changes name *text*, never
  the entity graph (settlement and belief entity ids unmoved);
- **phonotactic validity**: every generated name is well-formed for its
  language (a testable property over a census);
- **uniqueness**: no in-world name collision survives the deterministic re-draw;
- **pin-isolation**: `--species goblin` consumes the same language draws as the
  unpinned path (the standard pin test, as for sky/terrain/species).
Determinism stays constitutional beneath all of it: same seed + pins →
byte-identical worlds, almanacs, artifacts.

**Save-format migration (ADR 0006):**
- Settlement-name generation moves to a new stream label `settlement/name/v2`;
  the old label is never renamed; the `name` predicate keeps its identity with
  new content.
- New worlds no longer commit a `tenet` text fact — the structured-content
  predicates (§6) are the durable truth. The old `tenet` predicate is never
  renamed; pre-Tongues saves keep loading and `why`-recounting their committed
  `tenet` facts. Historiography is domain-agnostic and recounts whatever facts
  exist, so both eras render.
- The fixed epithet pools in `domains/religion` (`ETERNAL_EPITHETS`,
  `CYCLIC_EPITHETS`) are **deleted**; epithets come from the language engine,
  wired at the composition root.
- New labels (`language/<species>/…`, `settlement/name/v2`) are permanent
  manifest additions; no reordering of existing consumption.

**Re-baseline, once:** every committed artifact (almanacs, censuses, reference
dumps) regenerates with new names and tenets — larger churn than prior
campaigns because all proper nouns move — as the campaign's final act before the
book close.

## 9. The Lab

Preregistered before any census runs (ADR 0016):
1. **Phonotactic validity** — 100% of generated names well-formed for their
   language, row-by-row over the census (the instrument reproducing its own
   grammar exactly).
2. **Low pre-redraw collision rate** — the drawn name space is combinatorially
   large; the fraction of names requiring a uniqueness re-draw is small and
   pinned as a calibration row (contrast the 10-syllable pool's ~1,100 names).
3. **Voice/morphology calibration keyed to status basis** — goblin deity
   epithets carry an honorific affix; kobold epithets never do — asserted
   row-by-row from independent metric columns, like The Eyes' head-domain
   calibration.
Optional anti-reskin echo (defined here, may run in The Meeting): attributing a
name to its species from phonology alone (the sibilance/labiality signature) —
the blind-attribution pattern applied to sound. New metrics: per-species
phonotactic-validity flag, epithet-honorific flag, name-length distribution.
**Study 008** records the naming/voice baseline; all censuses re-run at the
re-baseline.

## 10. Reference verb, TTS

- **`hornvale phonology`** joins `concepts` and `streams` as a drift-checked
  book-page generator: per-species inventory tables (segments with features),
  IPA transcriptions of sample names, phonotactic templates. Committed as a book
  reference page, regenerated in CI.
- **TTS** is an ungated offline `scripts/` convenience (espeak-ng at phoneme
  level; Kirshenbaum notation fallback if Unicode IPA input proves awkward).
  Never in the CI gate; no committed audio artifacts.

## 11. The book (opens the campaign)

Per book-driven development, the campaign opens with
**`book/src/domains/language.md`**: the phoneme-as-features model and
"spellings are views"; the articulation vector and its closed-ness, with the
model card (every dimension authored, each kobold value with its rationale,
**voice-loudness banked to derive from BIO-1**); the authored-envelope /
drawn-phonology split; the naming grammars and the status-basis keying; the
content→render seam and register, with the honest framing that v1 is the simple
renderer behind a permanent interface. At close: religion, culture, and
settlement chapters updated (stopgaps gone, names generated); the `phonology`
reference page; Study 008; chronicle; retrospective (decision 0020); a freshness
sweep dropping every "vocabulary stopgap / syllable pool / deleted by The
Tongues" note across the book; concept-registry review for the new predicates.
Registry: a new `LANG-2` row (phonology + naming + register substrate) flips
`spec'd` → `shipped`, MAP-3's sound-change/fossil core stays `elaborated` for
deep time, and a new `raw` row captures **paralinguistic vocal signaling**
(mating/alarm calls — distinct from speech; cross-linked to BIO-3 mating
systems and the biosphere fauna row).

## 12. Success criteria

1. Seed 42's almanac shows **zero English-template proper nouns**; the goblin
   and kobold pages are legibly different in name mouth-feel and myth voice; and
   `why` recounts a deity's epithet-shape to its status basis and a name's
   sounds to the articulation vector.
2. The structural-invariant suite (entity structure, phonotactic validity,
   uniqueness, pin-isolation) passes in CI — the identity discipline replacing
   byte-identity.
3. All preregistered calibrations (§9) confirmed and pinned; the full gate
   (`cargo test --workspace`, fmt, clippy) and the artifact drift check green.
4. The book carries the language chapter, the `phonology` reference page, Study
   008, and the chronicle — comprehension-gated as always; the Bolzag ×3
   collision era over.

## 13. Explicitly deferred

Lexicon, syntax, glossable meaning; sound-change, etymology, names-as-fossils
(MAP-3's core); meaningful toponyms; language demographics — pidgins, creoles,
the L2-simplification law, language shift, lingua-franca selection (LANG-1, the
interaction layer); the full generative oral-formulaic prose grammar (the seam
ships this campaign; the grammar reoccupies it later — EXP-1); paralinguistic
vocal signaling (the new raw row); music and the other three media of the
expressive engine; per-individual idiolect; the articulation vector's
derivation from a body/frailty vector (voice-loudness is banked to BIO-1).

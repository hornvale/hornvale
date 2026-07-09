# Campaign: The Words — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-07-year-2-metaplan-design.md` (Constitution §2 governs);
sequence number assigned at scheduling.
**Provenance:** Follows The Tongues (Y2-3), which gave each species a voice —
phonology, naming grammars, register — and drew a bright line in front of the
lexicon. This campaign steps over that line deliberately: a shared **concept
layer** so things in the world can be referred to (or pointedly not), and a
per-language **lexicon with real history** — the etymology substrate MAP-3 and
LANG-1 will later hang loanwords, trees, and the comparative method on.
Registry context: LANG-3 (conversation) and MAP-3 (deep language) consume what
this ships; neither is in scope.

---

## 1. Goal

Give the world a closed, kernel-registered **concept inventory** (what can be
meant) and each species-culture a **lexicon** over it (how, or whether, it is
said). Every word carries recountable answers to three questions:

- **What does it name?** A registered concept — and, for proper names, the
  site facts the name glosses to.
- **How is it built?** An evolved root, or a transparent compound of held
  roots ("big-water" for the sea a people knows of but was not steeped in).
- **Where did it come from?** A proto-form drawn in that language's own
  proto-tongue, evolved through a drawn cascade of sound-change rules —
  own-line descent, no cross-species tree — with the derivation committed as
  data and replayed by `why`.

Proper names regenerate as glossable compounds over facts the ledger actually
holds (a settlement's biome, its sky, its presiding belief), so every name is
a small true story: "Ice-Home" sits in tundra by construction.

**Success shape:** a dictionary reference page per species, a `word` REPL
verb, gloss views on every proper name, and a Lab battery proving the
etymologies regular, the gaps reasoned, and the name-glosses true.

## 2. Design principles

1. **Concepts are promises.** A registered concept asserts that the word
   refers to something the sim represents or deliberately authors. The
   inventory is **closed** (ontology-trap posture, as in The Tongues):
   widening it is a campaign decision, never an incidental edit.
2. **Etymology is data, not inference.** A word's derivation (proto-form,
   rule applications, modern form) is committed structure; surfaces render
   views of it. Nothing re-derives history at display time.
3. **History lands on the shipped present.** The sound-change cascade's
   codomain is the present-day phonology The Tongues shipped. Proto-forms and
   cascades are strictly new draws under new labels; no existing stream's
   consumption changes; the present stays byte-stable except where §7
   deliberately re-baselines.
4. **Every absence has a name.** A lexicon lacks a word for exactly one
   recountable reason: **experiential** (the world never showed the species
   the thing — an exposure fact) or **perceptual** (its senses never carved
   the distinction — a vector fact). No drawn gaps.
5. **Regularity is an invariant.** Sound change applies uniformly wherever
   its conditioning environment occurs — the Neogrammarian hypothesis as a
   CI-testable property. This is what makes a future comparative-method
   study honest.
6. **Layering holds.** `domains/language` stays kernel-only; concepts live in
   the kernel registry beside predicates; exposure is derived at the
   composition root from ledger facts via an input struct (the
   `SocietySummary` pattern). No domain-to-domain edge is added.

## 3. The concept layer (kernel)

`ConceptDef` joins `PredicateDef` in the kernel's concept registry:

- **Fields:** `id` (kebab-case, e.g. `water`, `moon`, `goblin-kind`),
  owning domain, one-line English gloss (English is the design language, as
  for predicate docs), and a small closed `kind` enum (substance, living,
  celestial, terrain, social, body, kin, quality).
- **Registration:** each domain registers the concepts it owns at
  construction, exactly as it registers predicates. Astronomy: sun, moon,
  star, night. Climate: the biome concepts, snow, rain, ice. Terrain:
  stone, mountain, sea. Species: the peoples (each *language* will hold its
  own words for goblin-kind and kobold-kind — endonym and exonym fall out
  free). Settlement: home, hearth. Religion: god, spirit. Language: the
  Swadesh core (§4).
- **Uniqueness** enforced as for predicates; the `concepts` verb and its
  drift-checked book page extend to dump `ConceptDef`s.
- **New predicates** (exact names settle at the concept-registry review):
  a word-for fact linking a species to a concept with its committed form(s),
  and a glosses-to fact linking a proper name to the concepts it compounds.

## 4. The Swadesh core as packs

A flat universal core would contradict The Eyes: species perceive different
worlds, so the "starting vocabulary" cannot be species-independent. The core
is therefore **authored packs** (~60–100 entries total, model-carded,
Nathan's to tune):

- **Universal stratum** — what our actual species share by being embodied,
  terrestrial, mortal, and social: water, stone, sun, night, eat, sleep,
  die, kin-basics. Small and defensible, not aspirationally universal.
- **Color pack** — carries the Berlin & Kay implicational ladder (dark/light
  → red → green/yellow → blue → …). A species descends the hue ladder to a
  depth derived from its perception vector; a rod-rich night-tuned people
  stops early and instead extends a **luminance ladder** (more words for
  kinds of dark). Recountable: "kobolds have no word for blue; their eyes
  are tuned for the dark."
- **Body pack** — should key to body plan; no body vector exists (BIO-1
  banked), so v1 ships a shared humanoid pack **model-carded to re-key to
  BIO-1** — the voice-loudness banking precedent.
- **Kin pack** — minimal shared core (parent, child, sibling), banked to the
  social-structure axes.

The registry holds each pack's full ladder; a species' lexicon stops partway
down, and the stopping point is a **perceptual gap** with a vector-keyed
reason (principle 4). Pack structure is authored data inside the closed core,
not an extensibility mechanism.

## 5. The lexicon engine (`domains/language`)

- **Proto-roots.** Per (language × concept the language lexicalizes), a
  proto-form drawn under new labels
  (`language/<species>/lexicon/root/<concept>`), built from the language's
  phonotactic templates — the same stem machinery as names.
- **Sound-change cascade.** Per language, a drawn ordered cascade from a
  small closed rule family (lenition, fortition, vowel shift, cluster
  simplification, final-segment loss), applied over a nominal time depth,
  drawn under `language/<species>/lexicon/cascade`. Rules map proto-segments
  onto the **shipped present-day inventory** (principle 3). Derivations are
  committed (principle 2); regularity is asserted (principle 5).
- **Compounds.** Modern roots combine via The Tongues' compounding
  morphology, plus one new drawn typological parameter per language:
  **compound headedness** (head-first vs. head-last), drawn under
  `language/<species>/lexicon/headedness` — the first drawn syntax-adjacent
  parameter, recountable in every compound's gloss order.
- Everything pure and deterministic: same seed + labels → same lexicon.

## 6. Exposure (composition root)

Only `windows/worldgen` sees all domains, so it derives each species'
**ExposureMap** from ledger facts and hands it to the language engine via an
input struct:

- **STEEPED** — the concept saturates the species' world: its settled
  biomes and their contents, its sky, its own social kinds, the universal
  stratum and its pack-granted entries → an evolved **root**.
- **KNOWS-OF** — adjacent or peripheral: a biome one step from its range,
  the sea for a near-coast people → a transparent **compound** from held
  roots.
- **UNKNOWN** — no path to it → a **gap**, with the experiential reason
  recorded.

Pack depth (§4) applies before exposure: a concept a species cannot perceive
is a perceptual gap regardless of exposure. Both gap provenances recount to
named facts or vector dimensions.

## 7. Glossed names and the re-baseline

- Proper names regenerate as compounds over **site-fact concepts**: a
  settlement name draws from its cell's biome, notable sky, and presiding
  belief's phenomenon; deity names and epithets gloss to their phenomenon
  and sentiment. Psychology keying from The Tongues (status-basis morphology
  on epithets) is preserved on top.
- Names remain pure single draws keyed by `(seed, species, kind, salt)`;
  site facts are the entity's own, so **pin-isolation holds by
  construction** (The Tongues' argument, unchanged).
- **Epoch bump:** name semantics change, so name streams move to
  `language/<species>/name/<kind>/v2` (the decision-log's epoch discipline;
  old labels stay documented as retired, never renamed).
- Every name carries three views: romanization, IPA, gloss.
- **Re-baseline, once:** all proper nouns regenerate (re-baseline #2, the
  acknowledged cost of glossed names); every committed artifact regenerates
  as the campaign's final act. Byte-identity breaks for proper nouns and
  holds everywhere else; the identity contract is the structural-invariant
  suite (§8).

## 8. Save format, determinism, invariants

- **New labels:** `language/<species>/lexicon/root/<concept>`,
  `…/lexicon/cascade`, `…/lexicon/headedness`; name epochs at
  `…/name/<kind>/v2`. Permanent manifest additions; no existing label's
  consumption changes; pin-isolation tests extend to the new draws.
- **Structural invariants (CI):** entity graph unchanged; every name
  well-formed for its language *and* every glossed concept present among its
  entity's facts; every (language × concept) resolves to exactly one of
  root / compound / gap; every derivation replays from proto-form to
  committed modern form; every gap carries exactly one provenance.
- Pre-Words saves keep loading: old name facts render as before (no gloss
  view available — absence, not error); historiography recounts whatever
  facts exist, both eras render.

## 9. The Lab (preregistered, ADR 0016)

1. **Neogrammarian regularity** — 100% row-by-row: every proto-lexicon
   entry containing a rule's conditioning environment undergoes it; the
   derivation replay equals the committed modern form.
2. **Exposure soundness** — zero roots for UNKNOWN concepts; every gap and
   every compound recounts to a ledger-fact or vector reason. Content-derived
   cross-check, not a config echo: the flag re-derives the exposure class
   from the ledger independently of the lexicon pipeline.
3. **Name-gloss truthfulness** — for each named entity, the glossed concepts
   appear among that entity's actual facts, row-by-row; a broken gloss
   pipeline is falsifiably caught.
4. **Pack-depth calibration** — each species' color-ladder depth matches its
   perception vector's derivation; luminance-ladder extension appears iff
   hue depth is curtailed.
5. **Collision rate re-measured** — glossed compounds shrink the name space
   versus free stems; the calibration row re-pins and confirms the rate
   stays low.

Study number assigned at preregistration; all censuses re-run at the
re-baseline.

## 10. Surfaces

- **Dictionary reference page** per species (sibling of `phonology`):
  concept, gloss, modern form (roman + IPA), proto-form, one-line
  derivation; generated by a drift-checked verb, committed to the book.
- **REPL `word` verb:** `word water` → each language's word with derivation,
  or its gap with the reason.
- **`why <name>`** recounts the gloss and the site facts behind it;
  **`why` on a word** replays its derivation.

## 11. The book (opens the campaign)

Opens with the lexicon chapter (extend `book/src/domains/language.md` or add
a sibling — implementer's call at book review): concepts as promises, packs
and the Berlin & Kay keying with the model card, own-line descent and the
regularity invariant, exposure and the two gap provenances, glossed names.
At close: chronicle entry, campaign retrospective (decision 0020), freshness
sweep (every "names have shape, not meaning" note across the book falls),
concept-registry review for the new predicates, dictionary and updated
almanac artifacts committed. Registry: new LANG row for the lexicon substrate
flips `spec'd → shipped` at merge; MAP-3's row annotated that own-line sound
change shipped ahead of the tree; LANG-3 row annotated that concepts/frames
now partially exist.

## 12. Success criteria

1. Seed 42's almanac shows every proper name glossed, the gloss true against
   the ledger, and `why` recounting name-glosses, word derivations, and gap
   reasons to named facts or vector dimensions.
2. The dictionary pages ship for both species, legibly different in *both*
   form (mouth-feel, as before) and now content (different gaps, different
   ladder depths, different compound headedness).
3. Structural-invariant suite and all preregistered calibrations green; full
   gate (test, fmt, clippy) and artifact drift check green.
4. The book carries the lexicon chapter, dictionary pages, the study, and
   the chronicle; the re-baseline is complete and committed.

## 13. Explicitly deferred

Cross-language borrowing, loanwords, and calque *events* (need contact
history — LANG-1); a shared proto-tree, cross-species cognates, and the
comparative-method Lab study (need species phylogeny — MAP-3/SEQ-3); semantic
frames, parsing, and dialogue (LANG-3); syntax beyond compound headedness;
meaningful terrain toponyms ("Cold-Ford" waits for fords); player-knowledge-
gated gloss views (TOOL-2); tenet re-rendering through the lexicon; body-pack
re-keying to BIO-1 (banked); per-individual idiolect.

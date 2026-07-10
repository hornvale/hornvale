# Campaign: The Branches — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-07-year-2-metaplan-design.md` (Constitution §2 governs);
sequence number assigned at scheduling.
**Depends on:** **The Words** (`2026-07-09-campaign-the-words-design.md`), which
ships the concept layer, the lexicon engine, and the etymology substrate
(proto-roots, the drawn Neogrammarian cascade, `Derivation`). The Words is
in-flight on `campaign-the-words` (core engine complete). The Branches is a
**follow-on** — it executes after The Words merges, and its delta is stated
against The Words' shipped structures. Nothing here is buildable before that
merge.
**Provenance:** The Words drew a bright line — *"own-line descent, no
cross-species tree."* Each people evolves its own proto-tongue in isolation, so
the world holds no shared ancestry to measure retention against. The Branches
steps over that line deliberately, in the smallest way that produces a real
**language family**: one authored **proto-goblinoid**, three daughters
(goblin, hobgoblin, bugbear) that descend from it through their own regular
cascades, and cognate sets that are true **by construction**. This is L1 of a
four-part arc (see §8); it builds the tree. Registry context: LANG-4 (The
Words) is the substrate; MAP-4 / LANG-1 (replacement, contact, the comparative
method) are the later layers that ride on the tree this campaign plants.

---

## 1. Goal

Give the world its **first language family**: a shared ancestor,
proto-goblinoid, and three living daughters that descend from it. After this
campaign, goblin, hobgoblin, and bugbear words for the same concept are the
**same proto-root** run through three different regular sound-change cascades —
cognates the sim can prove, not an author's resemblance. Kobold, a draconic
stock, is untouched: it shares no ancestor with the goblinoids and stands as
the negative control.

Every cognate carries a recountable descent: a single proto-form, and three
lineage-specific derivations landing on three present-day phonologies.

**Success shape:** two new species (hobgoblin, bugbear) with authored vectors;
a proto-goblinoid reference page; a dictionary that shows cognate sets across
the triad with descent visible in each gloss; and a Lab battery proving the
cognates **regular**, the family **monophyletic**, and the kobold outgroup
**clean** (zero cognates with any goblinoid).

## 2. Design principles

1. **A family shares one proto; a lineage owns its cascade.** The single
   structural change from The Words: proto-roots lift from *per-species* to
   *per-family*. One proto-goblinoid draws one proto-root per concept; each
   daughter descends it through its **own** drawn cascade landing on its
   **own** present phonology. Cognacy is not authored — it falls out of a
   shared source evolved down three paths.
2. **Descent under anatomical constraint.** A daughter does not invent its
   sound system; it inherits proto-goblinoid and erodes it *the way its mouth
   would*. The articulation vector (The Tongues' closed six-dimension vector)
   stops being an independent generator and becomes the **codomain** the
   lineage cascade lands on. The cascade's rule *outputs* stay inventory-bound
   as in The Words (an off-inventory output applies as identity); but an
   inherited proto segment that no rule moves into the daughter's inventory is
   **nativized** — merged to the nearest segment the daughter's inventory
   holds, by articulatory-feature distance. `evolve` thereby gains a stronger
   postcondition — **the modern form is a subset of the daughter's inventory**
   — and descent genuinely *loses* ancestral contrasts, the way a real
   inventory absorbs a sound it no longer keeps. Nativization is pure and
   draw-free (a function of segments and inventory alone), and it is a no-op
   wherever the proto inventory already sits inside the daughter's — every
   self-proto (singleton) lineage, hence kobold and every pre-Branches world,
   is untouched by it.
3. **The proto is a language too.** Proto-goblinoid is not a new kind of
   object. It is an ordinary language — an authored ancestral articulation
   vector, a phonology drawn under it, a proto-root per concept — that happens
   to have no speakers. This recursion is what lets L4 later treat the proto
   as a reconstruction *target* rather than a special case.
4. **The family generalizes; a singleton reproduces today.** "Family" is the
   general case; a family of one is the degenerate case. Kobold is a family of
   one: its proto *is* itself, so its proto-roots draw from its own phonology
   under its own labels — **byte-identical to The Words**. The mechanism has
   one shape; kobold is that shape with a single member.
5. **History lands on the shipped engine.** The cascade family, the `evolve`
   core, the lexicon assembly (`build_lexicon`, compounds, gaps, headedness),
   and the three surface views are all reused **unchanged**. This campaign
   adds a proto layer and two species; it rewrites no rule.
6. **Regularity is still the invariant.** Each lineage cascade applies
   uniformly wherever its environment occurs — The Words' Neogrammarian
   property, now the thing that makes cross-daughter correspondences
   *consistent* and therefore a future comparative method honest.
7. **Layering holds.** `domains/language` stays kernel-only. The two new
   species are data in `domains/species`. The family↔daughter membership and
   the shared proto-roots are assembled at the composition root
   (`windows/worldgen`) and handed to `build_lexicon` as input — no
   domain-to-domain edge is added.

## 3. Proto-goblinoid (the ancestor)

Proto-goblinoid is authored as a language with no speakers:

- **An ancestral articulation vector** — the same closed six dimensions every
  species carries, authored as the anatomy the three daughters diverged
  *from*. It is a distinct point, equal to no daughter's vector, so that "all
  three diverge" is literally true (the alternative — proto ≈ goblin — was
  considered and rejected: it makes goblin the frozen ancestor, which no
  living language is, and collapses L4's reconstruction target to a trivial
  copy of goblin).
- **A proto-phonology** — inventory and phonotactics drawn under that
  ancestral vector, by the existing phonology machinery. This is an
  independent draw from every daughter's present phonology.
- **One proto-root per concept** — drawn from the proto-phonology by the
  existing `proto_root` mechanism, under **family-level** seed labels
  (`language/goblinoid/lexicon/root/<concept>`), *once*, and shared by all
  three daughters.

Proto-goblinoid is not committed to the ledger (a world is a seed plus a
ledger; the proto re-derives like every daughter phonology does). It surfaces
only as a reference page and as the head of each cognate set's derivation.

## 4. The family and the three daughters

The goblinoid family is `{goblin, hobgoblin, bugbear}`; kobold is outside it.
Species are already a string-keyed data registry (goblin at the 0.5 baseline,
kobold authored from SRD lore), so the two new peoples are additive data — no
enum widened, no existing species edited.

The family divides along **voice loudness**, an axis The Words already made
load-bearing (it down-weights exotic manners and biases inventories):

| People | Role in family | Voice loudness | Signature (authored from SRD lore) |
|---|---|---|---|
| **Hobgoblin** | the loud daughter | ≈ 0.8 | martial, disciplined, commanding; a diurnal legion society. Cascade favours fortition, full voicing, clear onsets. |
| **Goblin** | the conservative daughter | ≈ 0.5 (baseline) | unchanged as a vector; the least-drifted anatomy, but a genuine descendant (its vocabulary re-baselines — §6). |
| **Bugbear** | the quiet daughter | ≈ 0.3 | large, hairy, stealthy ambush predator; guttural (high voicing, low sibilance). Cascade favours lenition and gutturalization. |

Each daughter also gets an authored psychology vector and perception vector
(a species needs all three), from SRD lore, matching kobold's authoring
method. Those two vectors do **not** drive phonological descent — they exist
because a species is a whole people, not to shape the family tree. The exact
scalars and the closed-enum choices (sociality, status-basis, activity-cycle,
exotic-manner) are pinned in the implementation plan, where the enum variants
are verified against `domains/species`.

## 5. Descent: shared roots, per-lineage cascades

The Words' `build_lexicon` draws, per species, a headedness and one cascade,
then in pass one gives every `Steeped` concept its own proto-root drawn from
*that species' phonology* and evolves it to a modern root. The Branches
changes **only the source of the proto-root**:

- **Family step (new, at the composition root):** draw proto-goblinoid's
  phonology and its one-proto-root-per-concept table, once.
- **Daughter step (per lineage):** `build_lexicon` for a goblinoid daughter
  takes the **shared** proto-roots as input rather than drawing its own;
  draws its own cascade (`language/<species>/lexicon/cascade`, unchanged
  labels) and its own present phonology (unchanged); and `evolve`s each
  shared proto-root through its cascade into its present inventory,
  **nativizing** any inherited segment the cascade left outside that inventory
  (a deterministic, draw-free merger to the nearest neighbour by feature
  distance — see principle §2.2). For a singleton the proto-roots already
  come from the daughter's own inventory, so nativization never fires and the
  path is byte-identical to The Words.
- **Singleton step (kobold):** proto-roots come from its own phonology, its
  own labels — the family-of-one path, byte-identical to The Words.

The result: for concept `water`, one proto-root `*gʷat` yields `wat-`
(goblin), `vad-` (hobgoblin), `gwad-` (bugbear) — a cognate set whose members
are related by the *same* proto-form and whose correspondences are regular
because each cascade is Neogrammarian. `LexEntry`, compounds, gaps, and the
three `WordViews` are unchanged; only the provenance behind each root now
points at a shared ancestor.

Headedness (compound modifier-head order) stays drawn per species — daughters
may legitimately differ in it, a real typological axis, left to vary.

## 6. The re-baseline (goblin's vocabulary, epoch discipline)

Because goblin's proto-roots now descend from proto-goblinoid rather than
being drawn from goblin's own phonology, **goblin's present-day vocabulary and
every proper name built over it change.** This is a deliberate regeneration,
governed by the project's epoch discipline (never a silent rename):

- Goblin's lexicon root draws move under the family:
  `language/goblin/lexicon/root/<concept>` is **retired**, replaced by the
  family-level `language/goblinoid/lexicon/root/<concept>`. The retired label
  stays in `stream_labels()` with a `(retired at The Branches)` doc; old
  saves keep loading and render their committed `name`/`name-gloss` facts as
  before.
- Proper-noun byte-identity does not hold across this campaign (as it did not
  across The Words); determinism (same seed → identical output across runs)
  always holds and is tested. The committed seed-42 galleries and the
  dictionary regenerate; the drift check re-pins them **in this campaign's
  commits**, not deferred (re-baseline discipline).
- Kobold's labels and outputs are **unchanged** and asserted so — the
  singleton path is the byte-stability keystone.

## 7. Success shape and the Lab battery

Surfaces:
- A **proto-goblinoid reference page** (its inventory, phonotactics, and the
  proto-root table) generated like the other reference dumps.
- The **dictionary** grows cognate columns: for each concept, the three
  goblinoid daughter forms beside the proto-form, with descent visible in the
  gloss (`*gʷat → wat- / vad- / gwad-`).
- The `word` REPL verb (from The Words) shows a cognate set and its three
  derivations when asked about a goblinoid word.

The Lab battery is the honest core — it proves the family is real, not
decorative:
1. **Regularity across the family:** every daughter's realization of a given
   proto-segment in a given environment is consistent across all words (the
   Neogrammarian property, cross-checked cognate-set-wide, not just
   per-word).
2. **Monophyly by construction:** every goblinoid cognate set traces to one
   proto-root; no goblinoid root lacks a proto-ancestor.
3. **Clean outgroup:** no proto-root is shared between kobold and any
   goblinoid; kobold's derivations descend from its own isolated proto. (This
   is the property L4's blind reconstruction must later recover — here it is
   asserted from ground truth.)
4. **Determinism and singleton stability:** same seed → identical family;
   kobold output byte-identical to the pre-Branches (post-Words) fixture.
5. **Inventory closure:** every goblinoid daughter's every modern form is a
   subset of that daughter's own phonology inventory — the nativization
   postcondition, asserted family-wide so no word can use a sound the
   daughter's inventory (and its phonology reference page) does not list.

## 8. Non-goals (the rest of the arc)

The Branches builds the tree and nothing else. Named here so the scope stays
honest — each is a later campaign:

- **No lexical replacement.** Every concept keeps its inherited root; no
  daughter innovates or loses a root. Differential retention — the true
  Dolgopolsky ranking — is **L2 (The Drift)**.
- **No contact or borrowing.** Daughters do not exchange words; the tree does
  not yet reticulate. Loanword resistance — the true Leipzig-Jakarta ranking —
  is **L3 (The Weave)**.
- **No blind reconstruction.** The comparative method — inferring the tree and
  the proto from daughter wordlists alone and scoring the inference against
  this campaign's ground truth — is the capstone, **L4 (The Reckoning)**.
- **No species-tree coupling.** Adding the goblinoids as a linguistic family
  implies they are a species clade, but wiring the language tree to the
  deep-time species phylogeny (the gene-tree/language-tree correlation) is out
  of scope; L1 only asserts the linguistic family and notes the coupling as a
  future consistency constraint.

## 9. Determinism and layering checklist

- No wall-clock; no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only);
  float sorts via `total_cmp`. Randomness only from kernel `Seed`/`Stream`.
- New seed labels (`language/goblinoid/lexicon/...`, proto-goblinoid's
  phonology draws) are declared as constants and published via
  `stream_labels()` into the generated manifest; the retired goblin-species
  root label is kept with its retirement doc.
- Stream-consumption isolation: the family path must consume proto-roots in a
  fixed order independent of daughter count; the singleton (kobold) path must
  consume exactly the draws The Words consumed. Pin-isolation tests extend,
  never change.
- Nativization consumes no draws — it is a pure function of (segments,
  inventory) applied after the cascade, so it perturbs no stream and cannot
  break stream-consumption isolation. It is a no-op on every self-proto
  lineage, so kobold and every pre-Branches world stay byte-identical;
  `evolve`'s new postcondition (modern ⊆ inventory) holds trivially for those
  and is what the §7 inventory-closure battery asserts.
- `serde` + `serde_json` only; no new crates. `#![warn(missing_docs)]`; a
  one-line doc comment on every public item, field, and variant. `cargo fmt`
  as the final step before every commit.
- Definition of Done includes the book: a chronicle entry, the freshness
  sweep of the language chapter (which The Words also touches — sequence the
  prose so The Branches appends, not contradicts), and a one-page
  retrospective.

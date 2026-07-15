# Campaign: The Speakable — Design

**Date:** 2026-07-14
**Registry row:** LANG-32 (flip to `spec'd` with this commit)
**Predecessors:** The Words (glossed names, repair), The Branches (descent,
nativization)
**Status basis:** brainstormed under campaign-autopilot; decision ledger in
the worktree's `.superpowers/sdd/decision-ledger.md`

## 1. Goal

Every generated language can say its own words. Today a species whose
drawn syllable templates happen to reject its evolved lexicon's word
shapes — roughly half of all drawn phonologies, and 3 of seed 42's 4
species — has every glossed name crushed by phonotactic repair into the
same minimal fallback syllable: every bugbear deity is *Bvaash*, every
goblin deity *Neb*, regardless of gloss. Glosses stay truthful; the
phonetic side of the entire lexicon machinery is inaudible.

After this campaign: a name compounded from a language's own lexicon words
**surfaces those words verbatim** — *Daoqao* the shadow-god is named with
the actual bugbear word for shadow — and repair fires only on material the
language has genuinely never attested.

## 2. The defect, precisely

- `draw_phonology` draws onset/coda manner templates independently of
  everything else (`phonology.rs::draw_phonotactics`): onsets length 1–2,
  codas length 0–1. Nothing guarantees a single-consonant onset (P(none)
  ≈ 1/4–1/8) or an open coda (P(none) ≈ 1/4–1/2).
- Lexicon modern forms descend from proto-roots via the cascade and are
  nativized **to the inventory only** (`etymology.rs::nativize`); template
  conformance is explicitly not guaranteed (documented in `naming.rs`).
- `repair_phonotactics` reconciles the two with epenthesis (insert vowels)
  and deletion only. A word whose consonants no template can host in any
  position is deleted wholesale, and the all-deleted case falls back to
  `minimal_syllable` — a **constant** per phonology. All distinct words map
  to one surface form.

The asymmetry is the root: words must bend to templates; templates never
learn from words. Real phonotactics is a *generalization over the
lexicon*, not a law imposed on it.

## 3. The fix: an attested tier (descriptive phonotactics)

The admitted parse set for glossed names becomes two-tiered:

1. **Canon tier** — the drawn onset/nucleus/coda templates, unchanged.
   Still the grammar for freshly drawn material: v1 bare stems, the
   settlement unique-stem, honorific affixes, proto-roots.
2. **Attested tier** — the language's own lexicon root modern forms,
   admitted **verbatim** as parse units. A sequence conforms if it parses
   as any interleaving of canon syllables and whole attested words.

Consequences, by construction (not by tuning):

- A lexicon word conforms trivially (it is its own attested unit).
- A concatenation of conforming pieces conforms (parse each in turn), so
  every glossed compound — site word(s) + drawn stem + affixes, in any
  headedness order — parses with **zero edits**. Repair's identity
  short-circuit fires; the collapse is unreachable for native material.
- Compounds of two roots need no separate admission (their join is two
  attested units in sequence).
- The repair engine (DP, `EPENTHESIS_COST`/`DELETION_COST`, the
  `minimal_syllable` fallback) survives untouched as the adapter for
  genuinely foreign input — after this fix no native material needs its
  edits, so its edit paths go quiet until exonyms/loans (LANG-10) feed it
  foreign words; that campaign owns making its edits gentler (followup
  register).

Linguistic posture: the canon tier is the morphology's phonology, the
attested tier is the lexicon's — affixal phonotactics being narrower than
root phonotactics is typologically ordinary. The alternatives —
draw-time typological axioms, evolve-time conformance, substitution-only
repair, diachronic template inheritance — are ledgered with reasons
(entry #5) and stay named in LANG-32's row as the deeper root-cause
directions.

## 4. Mechanics

All in `domains/language`, plus one consumer each in `windows/worldgen`
and `windows/lab`.

- `naming.rs::conforms(segments, ph)` →
  `conforms(segments, ph, attested: &[Vec<Segment>])`: the backtracking
  parse gains one branch — at position `i`, any attested word `w` with
  `segments[i..i+w.len()] == w` advances to `i + w.len()`.
- `naming.rs::repair_phonotactics` likewise: the DP gains
  `RepairStep::Attested { word }` at cost 0. Deterministic tie-break, in
  order: attested words (longest first, then lexicographic by segment
  order) before canon template pairs, deletion last — tie-breaks only
  affect *which* zero-cost parse is found, never the emitted segments, so
  previously-conformant input remains byte-identical (identity
  short-circuit) and the DP stays a pure function of
  `(segments, ph, attested)`.
- New helper `attested_forms(lexicon: &Lexicon) -> Vec<Vec<Segment>>`:
  every `LexEntry::Root`'s `derivation.modern`, deduplicated and sorted
  longest-first with `Segment`'s total order breaking ties —
  deterministic, draw-free. Gaps and compounds contribute nothing.
- `Namer::glossed_name` passes its lexicon's attested forms at the one
  existing repair call site. The v1 `name()` path and the empty-gloss
  bare-stem fallback never call repair and are untouched.
- `windows/lab`'s romanization-level conformance validator re-derives the
  same lexicon it already re-derives for gloss metrics and checks against
  the widened set.
- Post-repair morphology (honorific prefix after repair) is unchanged: a
  canon-conformant prefix on a conforming body still conforms.

**No stream consumption changes anywhere.** No draw is added, removed, or
reordered; phonologies, cascades, lexicons, dictionaries, v1 names, and
proto-roots are byte-identical. Only glossed names whose repair previously
*edited* change; every name repair previously passed through unedited is
byte-identical.

## 5. Save format, determinism, epoch (G3 flag)

- **Rebaseline, not epoch.** The Words' "permanent repair formula" doc
  comment gains a tier, but no stream path is retired or reused — epochs
  exist to retire streams (deity-name /v2, decisions 0050/0051); pure-
  function output changes rebaseline in the changing commit (libm
  precedent, decision 0041). Ledger #2.
- The Words spec §8's structural invariant is **redefined, not dropped**:
  "every name well-formed for its language" now reads *well-formed =
  parseable over canon templates plus the language's own attested words*.
  The Lab validator enforces the new reading.
- Golden pins that will drift (the settlement-name collision-rate pin
  ~4.70%, any naming calibration pins) are re-pinned **in the same
  commit** (rebaseline-golden-pins posture).
- Committed seed-42 artifacts (almanacs, possession transcript, gallery
  pages, lab study outputs) regenerate via
  `SKIP_CENSUS=1 regenerate-artifacts.sh`. Census fixtures lag until the
  pre-merge AWS regen with warning to Nathan first — the standing trade.

## 6. Evidence battery

- **Unit (naming.rs):** an attested word conforms; a compound of two
  attested words + a canon stem conforms; repair of any such compound is
  the identity; genuinely foreign segments still repair (and the
  degenerate fallback still fires) exactly as before when `attested` is
  empty — the old tests keep passing with `&[]`.
- **Property (seed-swept, `domains/language` battery):** for 64 seeds ×
  the shipped articulation envelopes (commit-gate tier; sized to the
  existing genesis/tectonic property batteries): every lexicon root conforms under
  its own (phonology, lexicon); `repair(compound of roots)` is the
  identity; glossed names *contain each glossed concept's modern form as
  a contiguous segment subsequence* — the "names are audibly made of
  words" invariant, the direct negation of the collapse.
- **Integration (worldgen):** seed 42 regression — bugbear deity names
  are pairwise distinct wherever their glosses differ; the committed
  name-gloss facts remain truthful subsets of site concepts (existing
  test, must stay green).
- **Distinctness readout (one-shot, in-PR evidence, not a battery):**
  before/after count of distinct deity-name surfaces per species over a
  seed sweep, quoted in the PR/chronicle — the measured before is ~1 per
  collapsed species.

## 7. Surfaces and the book

- No CLI changes. `hornvale dictionary` output is unchanged by
  construction; almanac/gallery names change only for previously-collapsed
  species.
- Book freshness sweep: the language chapter's naming/repair section
  (describe the two tiers and why phonotactics is descriptive); chronicle
  entry; campaign retrospective (decision 0020). LANG-32 flips `spec'd` →
  `shipped` at close; LANG-34 (name taboo) and the followup register
  entries stand as captured.

## 8. Success criteria

1. Seed 42 rebuilt: no two bugbear deities with different glosses share a
   name; *Daoqao* (or its correctly compounded form) is audible in the
   shadow-god's name; kobold names byte-identical except any whose repair
   previously edited.
2. The property battery (§6) green over its full seed sweep.
3. `make gate` green with all rebaselines and re-pins in the same commit;
   artifact drift-check green modulo the standing census lag.
4. Dictionary, phonology dump, proto table: byte-identical.

## 9. Explicitly deferred

- Substitution edits / input-derived fallback in repair — to the exonym
  campaign (LANG-10), where foreign material actually reaches repair.
- Draw-time typological axioms and diachronic phonotactic inheritance —
  the root-cause remodels, named in LANG-32's row.
- Deity descriptor breadth — LANG-33, the sequel campaign.
- Any change to nucleus counts, tone, or the capacity floor.

---

**SHIPPED (2026-07-14).** Implemented as planned (attested tier;
rebaseline-not-epoch); measured outcome in the chronicle
(`book/src/chronicle/the-speakable.md`). LANG-32 flipped to shipped.

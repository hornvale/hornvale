# 0257. Felt states are concepts, and they accede as an appended cohort

**Status:** Accepted (2026-08-25) · **Decider:** Nathan · **Relates:**
[0256](0256-a-hosts-testimony-is-fallible-by-construction.md) (what this makes
possible),
[0172](0172-a-concept-with-no-possible-referent-is-an-extradiegetic-gap.md)
(the gap vocabulary this joins),
[0246](0246-a-renamed-concept-keeps-its-serialized-spelling-forever.md),
[0007](0007-seed-is-identity.md);
[The Confidant](../../book/src/chronicle/the-confidant.md)

In the context of wanting a culture to be able to *lack a word* for a feeling,
we decided that **the six felt states of the affect circumplex are registered
concepts under a new `ConceptKind::Affect`, acceding as epoch 12 of
`EPOCH_COHORTS`, appended and never merged into an earlier cohort** — accepting
a permanent widening of the concept registry to buy the lexical gap.

## Context

`ConceptKind` had nine kinds and zero affect concepts. For a culture to lack a
word for `Helpless`, `helpless` must *be* a thing a culture can hold a word
for — that is, a concept — because the whole lexical machinery (exposure,
proto-root assignment, sound change, gaps and their reasons) is keyed on the
concept registry and on nothing else. There is no shorter route; a parallel
affect-word table would be a second lexicon with none of the first one's
properties.

The brainstorm predicted this would cost a world-generation epoch, on the
argument that new concepts mean new per-culture word draws and therefore a
changed stream consumption order. **That was checked before the cohort was
designed, and it is false.** `accession.rs` sorts by accession epoch first, so
an appended cohort lands strictly last — the one position that provably
displaces nothing. The first task of the campaign reproduced the result
empirically against the real 176-concept table across eight seeds, with an
anti-vacuity control inside the same test that folds the same concepts into
epoch 0 and asserts something *does* move.

## Consequences

- **`ConceptKind::Affect` serializes by name, not by ordinal**, so the new
  variant is additive to the save format: a committed world carries the string
  `"Affect"`, and nothing renumbers.
- **The registry grew by exactly six**, 221 to 227, verified set-wise rather
  than by eye — the one artifact with deleted lines rewrites two list lines and
  a total, and the set difference is *added six, removed none*.
- **The roster is kept in step by a test, not by an import.** A domain may not
  depend on a window (`domains/CLAUDE.md`'s one rule), so `felt_state_pack` in
  `domains/language` and `AffectLabel` in `windows/vessel` are held against
  each other from the window side. A seventh `AffectLabel` variant fails to
  *compile* there rather than slipping past a runtime comparison.
- **Appending a cohort exposed a latent over-claim in an existing test**, and
  the finding is worth keeping. The insertion-stability test iterated *all*
  non-elf concepts and asserted invariance to the elf cohort's presence — which
  is broader than the algorithm guarantees, since a concept sorting after the
  appended cohort legitimately may move. It had passed only because nothing had
  ever sorted after elf. Epoch 12 is the first thing that does, and it made one
  concept move at one seed. The invariant was narrowed to what is true and
  strengthened where the campaign depends on it, rather than re-pinned.
- **The cost we accept:** every future concept addition also renames
  settlements. `Namer::glossed_name` runs phonotactic repair over the attested
  forms of a species' *whole* lexicon, so adding any concept changes which
  forms are attested and therefore what places are called. This campaign moved
  exactly two proper nouns in a cross-repo tile fixture. That coupling is not
  new; it had simply never been written down.

## See also

Spec §2.2 and §5.2; `domains/language/src/accession.rs` (the module's own
absolute rule and its withdrawn cohort-0 exception);
`book/src/reference/concept-registry.md`.

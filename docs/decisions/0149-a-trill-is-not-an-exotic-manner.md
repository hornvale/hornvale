# 0149. A trill is not an exotic manner

**Status:** Accepted (2026-08-19) · **Decider:** Nathan · **Relates:**
[0089](0089-an-epoch-freezes-when-a-world-saved-from-main-can-carry-it.md)
(what owes an epoch)

In the context of 18 of 19 shipped tongues carrying **zero** words with a
liquid, we decided that **the alveolar trill `r` is an ordinary manner, drawn
like any stop or nasal, not a member of the exotic tier that gates clicks and
ejectives**, because gating a sound that most human languages have behind the
tier reserved for the rare ones is a category error that made the single most
common liquid unreachable across the whole roster.

## Context

`Manner::Trill` sat behind the same `ExoticSeg` gate as `Click` and `Ejective`.
A species drew a trill only if its envelope admitted the exotic tier, which
almost none did — so the reachable region of the phoneme space had no liquid in
it, and the tongues came out uniformly liquid-less (kobold alone carried one, by
the accident of its envelope). Ungating the trill is the first change The Burr
made, and on its own it is a defect repair independent of whether any typology
bundle ever ships.

## Consequences

- A trill is now drawn by the ordinary manner path; the exotic tier keeps only
  the genuinely rare articulations (clicks, ejectives).
- This changes **the phonology the assignment algorithm draws from**, which
  reseeds every root at any label. By the reasoning in decision 0089, such a
  change **owes no epoch of its own** — the label documents a regeneration, it
  does not cause one; the campaign's single `ROOT_EPOCH` bump (`v4`) is owed by
  the root-and-pattern change to `assign_proto_roots`, not by this.
- Ungating puts the trill *in reach* but does not put it *in a word*: the
  inherited lexicon is gated by the proto's phonotactic templates, so audible
  liquids required the per-family phonotactic law as well. The trill gate was
  necessary and not sufficient — the campaign measured both halves.

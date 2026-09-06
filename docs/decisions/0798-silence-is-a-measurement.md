# 0798. Silence is a measurement

**Status:** Accepted (2026-09-05) · **Decider:** Nathan

In the context of a lens that narrates one individual life from the committed
ledger alone, facing the fact that most of what a biography wants to say the
ledger cannot say at all, we decided that **an unfilled slot is counted and
published rather than filled — a lens asks a fixed roster of named questions,
renders "the record does not say" where no fact answers one, declares its
by-design silences separately, and publishes the fill rate as a measurement** —
accepting that the headline number of a narrative feature is a report on how
thin the world's record is.

**Context.** The rule is inherited, not invented: `windows/explain` narrates a
world by reading only committed facts, never the in-memory system, "which is
how it validates that the ledger is sufficient" — so a slot the story cannot
fill is not a rendering gap but a fact about the world's own record. An
invented value would be strictly worse than the silence, because it would make
the instrument read healthy while measuring nothing. The lot asks twenty-six
slots, four of which are silent **by design** (sex, marriage and children,
work and income, literacy and height — each blocked on a model no domain has),
and the fill rate is reported over the remaining twenty-two so the number
measures the world rather than the lens's own restraint. See ledger #1.

**A silence must be sourced as carefully as an answer.** Three of the spec's
slot sources did not exist as written, and the spec had asserted their shape
from field names it had not opened: `Occupation.tongue` and `Occupation.deity`
are struct fields the bake writes as `None` that nothing ever commits — there
is no such predicate, and the almanac's own source says so — and the five
climate kinds are phenomenon kinds reached through the observed-phenomena path,
not facts on a settlement. Grepping the *readers* found them; reading the field
names had not. All three were re-sourced before any code was written (ledger
#8). A slot whose source does not exist reports a silence that is real for the
wrong reason, and it is indistinguishable in the output from one that is real.

**Consequence.** The published fill rate is a standing report on the ledger's
resolution at the level of an individual, and it can go down: a campaign that
adds a slot faster than it adds facts lowers it, which is the intended
pressure. It also localises a silence to a cause. The measured cultural
silences are not distributed noise — the record's cultural facts attach to
*living* settlements, and most person-years were lived in communities that have
since ended, so subsistence and standing are silent for 76–95% of lots
depending on the seed while the mean fill rate stays at 17.5–18.2 of 22. The
same discipline applies to any future ledger-only lens: name the questions,
count the ones the world cannot answer, and declare which of those are the
lens's choice rather than the world's poverty.

**See also.** Spec
[`2026-09-05-the-lot-design.md`](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-09-05-the-lot-design.md)
§5, §8; ledger
[`2026-09-05-the-lot.md`](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/ledgers/2026-09-05-the-lot.md)
#1, #8, and the Readout section;
[0016](0016-studies-preregister-hypotheses.md) (preregistration);
[0009](0009-models-author-dice-roll.md) (nothing writes the prose but the
ledger and a template); [The Lot](../../book/src/chronicle/the-lot.md).

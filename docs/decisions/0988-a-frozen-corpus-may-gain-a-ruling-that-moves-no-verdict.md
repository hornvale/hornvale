# 0988. A frozen corpus may gain a ruling that moves no verdict

**Status:** Accepted (2026-09-12) · **Decider:** Nathan (autopilot) ·
**Relates:**
[0016](0016-studies-preregister-hypotheses.md),
[0095](0095-a-corpus-is-an-instrument-never-a-standard.md),
[0986](0986-a-technology-corpus-is-a-sixth-family-and-a-capability-can-be-lost.md),
[0987](0987-a-corpus-is-closed-under-its-sources-dependency-relation.md);
[The Kiln](../../book/src/chronicle/the-kiln.md), [The
Cadastre](../superpowers/ledgers/2026-09-12-the-cadastre.md)

In the context of `technologies/`'s cross-corpus rule — a registry row cited
as an anchor by any corpus in the family must be explicitly ruled on, cited
or refused in writing, by every sibling corpus — facing a case that rule
guarantees will recur, where widening one corpus cites rows a frozen sibling
has never mentioned, we decided that **a frozen corpus may gain a written
cross-corpus ruling in its `provenance`, and may gain nothing else**,
accepting that the freeze in decision 0016 is narrower than "the file does not
change" and is instead "the file's verdicts do not change."

## Why this is not covered by existing law

`technologies/CLAUDE.md` states a re-freeze prohibition: any session that has
read the distribution a criterion **bands** is disqualified from re-banding
it, because band quality can only improve before first measurement. That
prohibition is about **criteria and bands** — the judgement calls that a
reviewer's own foreknowledge could corrupt if reopened. A written refusal that
moves no verdict, cites no new item, and changes no anchor, statistic, or
criterion is not a band. Nothing in the existing rule reaches it, and nothing
in the existing rule needed to, until the cross-corpus obligation created a
case where a frozen corpus is compelled to act by a sibling's own growth
rather than by anything its own author chose to revisit.

## The mechanism that forces the case

The family's cross-corpus rule (decision 0986, `technologies/CLAUDE.md`) makes
silence in one column, not disagreement between two, the signal it watches
for. Growing one corpus's item population necessarily cites registry rows the
corpus never cited before; if a sibling has never ruled on those rows, the
rule requires it to now, regardless of whether the sibling's own scoring work
is otherwise finished and frozen. This campaign exercised that path directly:
widening `technologies/asimov-1989` from 41 to 301 items moved 260 new items
onto real anchors, nine of which — `MAP-9c`,
`MAP-the-toll-is-the-inverse-desire-path`, `BIO-9`, `SKY-guest-stars`,
`SKY-10`, `SKY-magnetic-north`, `SKY-13`, `TECH-4`, `UNI-38` — neither corpus
had cited before. The rule then required `henrich-2004-extended`, frozen and
scored by a prior campaign, to rule on all nine.

## The decision

**A frozen corpus may gain a written cross-corpus ruling in its
`provenance`, and may gain nothing else.** The ruling states, per newly-cited
row, which of the corpus's own items (if any) it discharges, or refuses it in
writing with a reason specific to that column's own subject matter — never a
blanket dismissal. No item is added; no verdict, anchor, statistic, or
criterion already in the file may change as a side effect of writing the
ruling.

**Verified rather than asserted.** `henrich-2004-extended`'s edit to satisfy
this campaign's nine new rows was diffed against its pre-ruling state:

```text
top-level keys changed: ['provenance']
item count old/new: 41 41
ITEM-LEVEL changes: 0 []
```

`provenance` moved; the `items` array is byte-identical. Every one of the nine
rows was refused, each for a reason specific to the corpus's own subject
matter (a Tasmanian-loss ethnography with no captive-display, fortification,
stimulant, or astronomical item in it) rather than a single blanket dismissal
of the batch.

## Consequence

**This is the most contestable act this campaign took**, and the decision
record exists because of that, not despite it. The defence is narrow and
should be read narrowly by future sessions: a written refusal that moves no
verdict is not a re-banding, so the re-freeze prohibition does not reach it.
It does not license reopening a frozen corpus's scoring, its criterion, its
anchors, or any existing item's verdict under this cover — a future session
that wants to do any of that still needs the re-freeze prohibition's own
justification, which this record does not supply.

**A future sibling that must be edited under this rule owes the same two
things `henrich-2004-extended`'s edit owed here**: a written reason specific
to its own subject for each row it refuses (never a batch dismissal), and a
verified, not merely claimed, guarantee that nothing but `provenance` moved.

**See also.** `technologies/CLAUDE.md` ("A row cited by ONE corpus must be
ruled on by EVERY corpus", and the re-freeze prohibition under "The freeze");
`docs/superpowers/specs/2026-09-12-the-cadastre-design.md` §8;
`technologies/henrich-2004-extended.technology.json`'s `provenance`; the
campaign ledger, entry #13.

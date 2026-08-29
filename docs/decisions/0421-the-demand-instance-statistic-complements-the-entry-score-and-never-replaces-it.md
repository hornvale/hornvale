# 0421. The demand-instance statistic complements the entry score and never replaces it

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0330](0330-the-corpus-score-is-demonstrated-not-declared.md),
[0386](0386-a-corpus-declares-its-demands-or-derives-them-never-both.md) ·
[The Rail](../../book/src/chronicle/the-rail.md)

In the context of a 139-utterance corpus whose covered-entry count read **0
of 139** before this campaign and reads **0 of 139** after it — while the
grammar gained five capabilities and met 95 more of that corpus's demand
instances — we decided to publish a **demand-instance statistic beside** the
entry count rather than redefine what *covered* means, accepting that the
report carries two numbers per corpus where a reader might have wanted one.

## Why the headline number is not wrong

Entry coverage is **conjunctive**: an entry is covered when every token it
demands is implemented. The flood-watch corpus averages roughly **eight
demand tokens per entry** across 1128 instances, so a single unimplemented
token anywhere in an entry's list holds the whole entry out, and every entry
still has one. A zero that cannot move is uninformative, and it is not a
defect in the resolver — the resolver runs over that corpus and returns the
truth about it.

The right response to an uninformative statistic is a second statistic that
sees what the first cannot, not a looser definition of the first. Loosening
`entry_covered` would have moved the flood-watch headline and, in the same
edit, made **the merchant corpus's 5 of 12 incomparable with the three
campaigns that reported it** — which is exactly what 0016's discipline
forbids: it binds the scoring method as well as the corpus.

## Why not replace the entry score outright

This project has already paid for the lesson that a replacement statistic
needs its blind zone measured before it is trusted, and that **complementing
beats replacing** — a practice that only ever swaps the instrument will
eventually swap away a working one. Entry coverage answers a real question
(*can the grammar say this whole sentence?*) that the instance count cannot:
21 of 30 met merchant instances would be equally true of a grammar that could
complete no sentence at all.

## What ships

`demand_instance_coverage(entries) -> (met, total)`: a count over every
`(entry, demand)` pair a corpus states, published per corpus in
`docs/audits/sentence-coverage.md` beside the entry count and pinned by
`demand_instance_coverage_matches_the_campaigns_prediction`. It is **code, not
data** (0011): no corpus file gained a field, and both dialogue corpora are
byte-unchanged.

Across this campaign the two instruments moved as follows:

```
                   entries covered      demand instances met
  the-merchant     5 → 6 of 12          19 → 21 of 30  (63.3% → 70.0%)
  the-flood-watch  0 → 0 of 139        175 → 270 of 1128 (15.5% → 23.9%)
```

The merchant entry count moved for the first time in four campaigns. The
flood-watch entry count did not move at all, and 95 met instances — a 54%
relative gain — is the thing the binary statistic cannot express.

## Consequences we accept

- **Two numbers must be quoted together or neither is honest.** A campaign
  citing "0 of 139" alone reports a grammar standing still; one citing "23.9%"
  alone reports progress toward sentences none of which can be said.
- **The instance statistic is not demonstrated the way the entry score is.**
  0330's realization witness pins a committed `Clause` and its Common surface
  for every *covered entry*; there is no per-instance witness, and at 1128
  instances there will not be one. What backs the instance count is the same
  `demand_covered` predicate the entry count uses, which the witness does pin
  at the entry level for the merchant corpus and the ladder.
- **A coverage resolver tuned to this corpus is still unwritten**, and remains
  the open question it was: what *covered* should mean for a corpus the
  grammar was never built toward is a decision to take before a number exists
  to chase, not after.

# 0387. Coverage is reported per direction, and an absent direction is unknown

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot) ·
**Relates:** [0016](0016-studies-preregister-hypotheses.md),
[0330](0330-the-corpus-score-is-demonstrated-not-declared.md),
[0353](0353-a-regression-test-is-specified-by-the-mutation-it-must-fail.md) ·
[The Stile](../../book/src/chronicle/the-stile.md)

In the context of a sentence corpus that states, per utterance, whether the
grammar must **parse** it (a player line) or **produce** it (an NPC line, a
ladder rung), we decided that **coverage is reported per direction**, and
that an entry whose corpus states no direction resolves as
**direction-unknown** rather than having one inferred from `speaker` or any
other field — accepting that the report carries three numbers where a reader
might have wanted one.

## Why the blur stopped being free

Parsing a demand and producing it are different capabilities. Until this
campaign the resolver asked one blurred question — can the grammar produce
*or* parse this — and one corpus of 139 utterances now splits **68 parse
against 71 produce**. At that ratio a single score can rise while the half
that matters does not move at all, and nothing in the number would say so.

## Why absence is its own row

`the-merchant` carries a `speaker` field valued `"player"` or `"merchant"`,
which looks exactly like a stand-in for direction and is not one. Mapping it
would author a fact the corpus does not carry: the corpus was transcribed
from a live brainstorm and its annotator never made that call, so any
direction attached to it would be this campaign's opinion wearing the
corpus's provenance. **Three numbers where three are true is honest; two
numbers achieved by inventing the third is not.**

## What ships

`DirectionCounts { parse, produce, unknown }` over
`Entry::direction`, reported per corpus in
`docs/audits/sentence-coverage.md`: the-merchant 0/0/12, the-flood-watch
68/71/0, the-ladder 0/214/0.

The prohibition is guarded by a test that must be able to fail, and its
mutation was run rather than described (0353): splicing the forbidden
mapping — `"player" => Parse`, `"merchant" => Produce` — into the declared
reader turns `merchant_entries_resolve_as_direction_unknown` red reading
`parse: 4, produce: 8, unknown: 0`, which is literally the forbidden
speaker mapping and not a proxy failure. Fifteen of the file's other tests
stay green under it, so the guard is specific as well as loud.

## Consequences we accept

- **The merchant corpus's 12 entries sit permanently in the unknown row**
  unless a future campaign re-annotates it, which is a corpus edit and
  therefore a deliberate act under 0016 — not something a resolver may do
  on its behalf.
- **A per-direction score is not comparable to the single blurred score**
  three campaigns reported. The merchant total (5 of 12) is what stays
  comparable, and it is unchanged; the direction breakdown is new
  information beside it, not a re-statement of it.

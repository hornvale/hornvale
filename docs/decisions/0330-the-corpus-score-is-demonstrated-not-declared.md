# 0330. The corpus score is demonstrated, not declared

**Status:** Accepted (2026-08-27) · **Decider:** Nathan · **Relates:**
[0296](0296-tense-is-stated-never-derived.md),
[0297](0297-a-predicates-valence-is-stated-once-and-commons-parts-are-selected-from-it.md);
[The Inquest](../../book/src/chronicle/the-inquest.md),
[The Mortise](../../book/src/chronicle/the-mortise.md)

In the context of `sentence_corpus.rs`'s own module doc naming its weak point
— `IMPLEMENTED_DEMANDS` is *"a hand-maintained declaration and nothing
mechanically proves it,"* and *"a token added on optimism moves the score
without moving the grammar, which would make the instrument worse than no
instrument"* — we decided the merchant corpus score gets a **realization
witness**: for every entry the resolver calls covered, a committed `Clause`
(or `Coordination`) and the Common surface it actually realizes, checked by a
test that must be able to fail.

## Why a passing test alone does not settle this

A test that has never gone red proves nothing about what it would catch. The
witness was built to go red on arrival, and it did: `m10` was already scored
covered while `realize_common` panicked on `know`, which had no
`PREDICATE_VALENCE` row before this campaign. §3's `know` row is what turns it
green — a line declined on purpose by The Inquest, for a reason 0297 already
states, and this campaign is the one that has a reason to add it.

**Being red once, at the start, is not sufficient either.** A later review
found the witness's own RED had never been demonstrated for the specific
mutation it exists to catch — an implementation could satisfy every assertion
while silently NOT exercising the machinery a covered entry's demand tokens
actually credit it for. §6 criterion 9 requires the implementer to find and
apply such a perturbation from inside the code, not have one prescribed from
outside it: `elide_coordinated_subjects` was neutralized (coordination) and
`Subject::Clause`'s realization was stubbed (embedding) for `m07`, both
restored and md5-verified, both demonstrated to redden the witness.

## What ships

`MERCHANT_WITNESS`, a `(id, surface)` table beside `MERCHANT_COVERED_IDS`,
asserted equal in membership and, per entry, equal to what `realize_common`
(or `realize_common_coordination`) actually produces. It does **not** assert
equality with the corpus's literal English — Common is a limited register,
and that assertion would fail for reasons unrelated to grammar. The witness
records the realized surface and lets a reader see the distance, honestly,
including the ugly ones: `m07` realizes as *"they killed them killed me and
knowed me,"* a genuine garden path this decision's own campaign diagnosed
rather than hid (see the chronicle's "what this does not reach" section).

The merchant score moved **2 → 5** under this discipline
(`m05 m06 m07 m09 m10`), each entry backed by a witness row rather than a
declaration.

## Consequences we accept

**A corpus score without a witness is now a known-weaker claim.** Any sibling
corpus (`sentences/`, `tropes/`, `systems/`) that declares coverage without an
equivalent mechanical check inherits the same risk this record names, and a
future campaign adding one should look here first rather than re-deriving it.

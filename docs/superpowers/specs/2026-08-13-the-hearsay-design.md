# The Hearsay — myth gets its channel, and echo stops counting as evidence

**Status:** DRAFT, at G3 · **Campaign:** the-hearsay (`campaign/the-hearsay`) ·
**Program:** Myth (campaign 1 of 4) · **Depends on:** decision
[0100](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0100-fact-phenomenon-myth.md),
merged

Decision 0100 ratified three registers — *fact* (committed, contradiction-
checked), *phenomenon* (derived, coherent), *myth* (derived, free, **not
required to be coherent**) — and then said, in as many words: **"Myth is new,
and has no channel today."** This campaign builds that channel.

It is deliberately the smallest of the four. It adds a representation and one
measurement and diffuses nothing; the interesting dynamics are campaign 2's.
The reason to do it alone is that it carries near-zero save-format risk, and
campaigns 2–4 all carry real risk that should not be entangled with a type
decision.

## 1. What this campaign produces

1. **A provenance grade in the kernel** — `Provenance {Witnessed, Taught,
   Inferred}`, the epistemic ladder, distinct from its grammatical marking.
2. **A derived `Claim`** — holder, subject/predicate/object, grade, hops,
   ancestry. Derived only; **never serialized**, so it is not a save-format
   contract and triggers no epoch.
3. **A witness derivation** — `windows/hearsay`, which reads committed facts
   and produces the claim set for a world. Reads only; draws nothing.
4. **The independence test** — stemmatics' *eliminatio codicum descriptorum*
   as executable code: a holder whose derivation ancestry is a subchain of
   another's contributes **no independent weight**.
5. **A preregistered measurement** of the echo ratio (§6), with its null.

## 2. Non-goals

- **Diffusion.** Nobody tells anybody anything in this campaign. Claims arise
  from witnessing and are inherited along the *already-committed* founding
  tree. Transmission dynamics are campaign 2 (`DOM-transmission`).
- **The play ledger.** 0100 designed it; no code implements it (verified:
  `grep -rn "PlayLedger" kernel/ windows/` returns nothing). Campaign 3.
- **Creatures as listeners.** `absorb_common` already transfers a falsehood
  into a *player*. Turning it around is campaign 4.
- **Distortion.** A claim in this campaign has the content of the fact it
  descends from. Mutation is campaign 2, and §7 records the candidate model.
- **Any change to `Fact`, the ledger, or the concept registry.** Myth reads
  fact; fact never reads myth (0100 rule 1). The dependency stays acyclic.
- **Replacing `domains/language`'s `Evidential`.** See §3.2.

## 3. Where the types live

### 3.1 `Provenance` belongs to the kernel

The trace protocol lives in the kernel because it is how domains speak to each
other without depending on one another. A provenance grade is the same kind of
thing: universal, domain-free, and needed by any domain that will eventually
hold or transmit a claim.

**The load-bearing reason is layering, not taste.** This campaign's only
consumer is a window, and a window could perfectly well define the type itself.
But campaign 2 introduces `domains/transmission`, and **a domain may not depend
on a window** — the rank check forbids depending upward. If `Provenance` lands
in `windows/hearsay` now, campaign 2 must move it to the kernel anyway, and a
type that has already been published is more expensive to move than one that
was born there. Putting it in `kernel/` now costs nothing and is where it ends
up regardless.

### 3.2 This does **not** collapse `Evidential`

`domains/language`'s `Evidential {Witnessed, Taught, Inferred}`
(`morphology.rs:31`) is a **grammatical category**, drawn per species per
tongue, with a grammaticalization depth (unmarked / particle / affix). The
registry already presupposes the split it would be a mistake to collapse —
`KNOW-evidential-invisibility`:

> A tongue whose evidential category is ungrammaticalised **cannot mark whether
> a claim was witnessed or taught**, so trace provenance is linguistically
> invisible.

That row is only meaningful if the provenance exists independently of whether
the tongue can say it. So: **the kernel's `Provenance` is the epistemic fact;
`domains/language`'s `Evidential` is whether a tongue can express it.** Sharing
three variant names is a coincidence of English, not evidence they are one type.
`domains/language` is not edited by this campaign.

### 3.3 The derivation is a window, not a domain

A domain models a slice of the world and **owns seed labels**; a window reads
the committed ledger and presents. This campaign draws nothing — every claim is
a function of facts already committed — so it is a window, and
`windows/explain` is the precedent (it narrates a world reading only committed
facts, which is how it validates that the ledger suffices).

**Known churn, accepted deliberately:** campaign 2 *does* draw (who tells whom
is a seeded decision) and will therefore introduce `domains/transmission`. The
window will then consume it, exactly as `windows/lab` consumes
`windows/worldgen`. Putting an empty domain shell in now to avoid that would
mean a crate with a `streams` module and no draws, which is worse: a stream
label is a permanent contract and must not be minted speculatively.

## 4. The derivation

### 4.1 The substrate is already committed

Measured on the seed-42 world, not assumed
(`cargo run -p hornvale -- new --seed 42`, then a read of the ledger):

```
  is-person          148      occ-founded        704
  person-born        148      occ-founded-from   704
  person-died        144      occ-ended          474
  history-now          1      occ-ended-by       474
```

And the founding tree those parent links describe:

```
  occupations with a parent link   704
  max depth                         22
  median depth                       9
  max children of one parent         5
```

**`occ-founded-from` is already an inheritance chain**, 704 nodes deep enough
to matter, and `domains/history/src/descent.rs` already walks generational
distance (`remove`, `kinship`, `ancestor`). Nothing new needs generating.

### 4.2 Who holds a claim

**The holder is an occupation, not a person.** 0100 rule 2 requires a holder
and does not require it to be an individual — *"the dwarves say X, the goblins
say Y"* is the record's own example, and both holders are peoples.

This is also forced by the data: there are 148 persons and they are *founders*,
which is neither a large sample nor an unbiased one. Occupations give n=704 with
a real tree. Persons as holders is a candidate refinement for campaign 2, once
there is a reason to distinguish two holders inside one community.

### 4.3 The two rules

1. **Witness.** An occupation present at an event (its own founding, its own
   ending, a raid it was party to) holds a `Witnessed` claim about it.
2. **Inheritance.** An occupation founded from a parent inherits the parent's
   claims at `Taught`, with `hops` incremented. Grade only ever moves *down*:
   `Witnessed → Taught` is legal, the reverse is not. That single anti-symmetry
   is why a rumour decays instead of strengthening on retelling.

`Inferred` is reserved and unused in this campaign; it is where campaign 2's
deduction lands (`KNOW-deduction-gain`).

## 5. The independence test

Ported from stemmatics, where the problem — reconstructing what was true from
copies that corrupt — has two centuries of worked answers.

**The rule (*eliminatio codicum descriptorum*):** a witness whose entire
derivation ancestry is contained in another surviving witness's ancestry
contributes nothing independent. In a manuscript tree, a copy made from a
surviving copy is discarded for reconstruction. Here: an occupation that knows
of an event *only* because its parent did is not a second witness to it.

This is `KNOW-independence` — "two traces are independent if disjoint derivation
ancestry" — with an algorithm attached, and it is the direct fix for
`SOC-reputation-provenance`:

> ten goblins who all heard it from one goblin is **one** observation wearing
> ten mouths … without a grade a rumour strengthens by retelling.

`independent_witnesses(claim_set, event)` returns the count of maximal
disjoint-ancestry chains, never the holder count.

## 6. Preregistration

Frozen here, before the code that would move it (decision 0016). The study
JSON carries no hypothesis field, so this section is the freeze.

**An *event*, for this section, is a committed history fact with a time and a
locus: a founding (`occ-founded` + `occ-founded-from`) or an ending
(`occ-ended` + `occ-ended-by`). 704 and 474 of them respectively on seed 42.
Nothing else counts, so the denominator is fixed before the measurement.**

**H1 — the echo ratio is small.** Define

```
  echo_ratio(event) = independent_witnesses(event) / holders_of_a_claim(event)
```

Range is `(0, 1]`: 1.0 when every holder witnessed it independently, `1/N` when
all N inherited it from one ancestor.

**Prediction: median `echo_ratio` < 0.5** over events with ≥3 holders — most
apparent corroboration is inherited, not independent.

**The decision rule, not just the prediction** (a branch table cannot be wrong
where a prediction can):

```
  median < 0.5   -> H1 CONFIRMED. Report the ratio and the distribution's
                    shape, not only the median; a bimodal result means two
                    populations of event and is the more interesting finding.
  0.5 <= m < 0.8 -> H1 REFUTED, echo present but weak. Report as refuted;
                    do NOT retune the >=3-holder cutoff to rescue it.
  median >= 0.8  -> H1 REFUTED, and this is the headline: Hornvale's committed
                    history does not naturally produce echo chambers. That is
                    a finding about the history bake, and campaign 2's
                    diffusion is then the thing that would create them.
  <30 qualifying events across the seed set -> NO VERDICT. The measurement did
                    not have a denominator; say so rather than reporting a
                    median over a handful.
```

The founding tree branches at ≤5 children with median depth 9, which may well
be bushy enough to put this in the third row. That outcome is a result, not a
failure.

**H2 — echo ratio falls with event age.** Older events have had more
generations to propagate along the tree, so their holders should be
increasingly dominated by inheritance. **Prediction: negative rank correlation
between event age and `echo_ratio`.**

**Stop rules.** H1 is decided on the median over all qualifying events across
the census seed set, not on seed 42 — one world is an anecdote. H2 requires the
sign of the correlation, not a magnitude threshold; a null result is |rho| <
0.1.

**Not preregistered here, deliberately:** *lectio difficilior potior* — that
surviving variants are simpler than their archetypes — is the strongest
prediction stemmatics offers, and it **cannot be tested until claims can
mutate**. It belongs to campaign 2 and is recorded in §7 so it is not lost.

## 7. Carried forward

- **`lectio difficilior potior`** → campaign 2's headline hypothesis.
- **Distortion model: *original antigenic sin*.** From immunology — a holder
  interprets a new variant using memory of the *first* one it met, badly. It is
  deterministic and a function of the holder's own history, which makes it
  strictly cheaper than a seeded random walk and consistent with belief-as-fold.
  Campaign 2.
- **Persons as holders** — refinement, campaign 2 (§4.2).
- **`KNOW-lost-revision`** — keeping the contradiction when a belief changes.
  Needs revision, which needs diffusion. Campaign 2.

## 8. Definition of done

- `Provenance` and `Claim` in the kernel, `#![warn(missing_docs)]`-clean, with
  `type-audit:` tags on every primitive at a `pub` boundary.
- `windows/hearsay` deriving claim sets from committed facts; layering test
  green (a window may depend on domains; it must not be depended on).
- `independent_witnesses` unit-tested against hand-built trees, including the
  subchain case that must return 1 and not N.
- Lab metrics for `echo_ratio` registered, **with the metric cost measured
  before registration** — nine studies declare `"metrics": "all"` and there is
  no opt-out flag, so an expensive metric is a permanent ~2000-world cost
  (`windows/lab/CLAUDE.md`).
- The preregistered readout run and reported **at the strength the measurement
  supports**, null or not.
- Chronicle entry, book freshness sweep, retrospective, and the registry rows
  in §7 filed — no idea dies in conversation.

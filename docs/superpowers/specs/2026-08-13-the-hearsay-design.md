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
4. **A preregistered measurement of transmission depth** (§6), with its null —
   the **no-decay baseline** every later campaign's forgetting is measured
   against.

## 2. Non-goals

- **Diffusion.** Nobody tells anybody anything in this campaign. Claims arise
  from witnessing and are inherited along the *already-committed* founding
  tree. Transmission dynamics are campaign 2 (`DOM-transmission`).
- **Independence, and the conflict measurement with it.** Both need a claim to
  reach a holder by two routes, and **two routes is diffusion** — so both are
  campaign 2 by this spec's own first non-goal. See §5, which records why this
  was not obvious.
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

**`occ-founded-from` is a sum type, and reading it as a plain parent pointer is
wrong.** `history_emit.rs:282` writes one of two shapes:

```
  Founding::Genesis(cell)  ->  Value::Number(cell_id)     NO parent: a root
  Founding::From(entity)   ->  Value::Entity(parent_occ)  a real parent link
```

A `Number` is a *site*, not an ancestor. The corrected tree on seed 42:

```
  occupations                      704
    roots (Genesis at a cell)       46
    with a real parent (From)      658
  max depth                         21
  median depth                       8
  max children of one parent         5
  parents having any children      516
```

**The 46 roots are load-bearing, not a rounding detail.** A root lineage
witnessed its own origin with no ancestor to inherit from, so a root caps the
hop count of everything descending from it: no claim can be more hops old than
its lineage is deep. Reading all 704 as one chain — as an earlier draft did —
would have overstated the reachable depth by a factor of several.

`domains/history/src/descent.rs` already walks generational distance
(`remove`, `kinship`, `ancestor`). Nothing new needs generating.

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

## 5. Why independence is NOT measured here

This section replaces an independence measurement that was specified, approved
at G3, and then found unfalsifiable before any of it was built. The finding is
worth more than the metric was.

**What was wrong.** Claims reach holders by exactly one route: an occupation
witnesses an event, and its descendants inherit. The holder set of any event is
therefore a single subtree, every holder's earliest holding ancestor is the
witness, and `independent_witnesses` returns **1 for every event on every
world**. The ratio it fed was `1/N`, and since the metric only reported when
N >= 3, it was always <= 1/3 against a predicted median of 0.5. The hypothesis
was true by construction. Traced, not argued: a subtree of six holders returns
1 independent witness and a ratio of 0.167.

**Why the obvious repair was refused.** An ending has two parties —
`occ-ended-by` names an Entity ender on 234 of 474 endings — so extending the
witness rule to both parties would have produced two origins and rescued the
number. It was refused because **that is a rescue, not a correction**: it
changes the model to save a measurement, removing a constraint rather than
adding one.

**The root cause, one level above the repair.** Independence requires a claim
to arrive by two routes, and *two routes is diffusion*. Diffusion is this
campaign's first non-goal. So the hypothesis was a campaign-2 hypothesis
wearing campaign-1 clothes, and no witness-rule patch fixes that — it only
smuggles a second route in under another name. The generalisable form: **a
measurement that requires the mechanism a campaign has excluded is not a hard
measurement, it is a scope error**, and it will read as a merely difficult
metric right up until you notice it cannot fail.

`independent_witnesses` itself is correct and worth having; it is deferred
whole to campaign 2, which supplies the second route. `KNOW-independence`
stays `spec'd` against this document, because the definition landed here even
though the measurement could not.

## 6. Preregistration

Frozen here, before the code that would move it (decision 0016). The study
JSON carries no hypothesis field, so this section is the freeze.

**What is measured.** For every held claim, its `hops` — the number of
inheritance steps between the witnessing occupation and the holder. Zero is the
witness itself. The population is every (event, holder) pair the derivation
produces, across the census seed set.

**This is the no-decay baseline, and that is its point.** Nothing in this
campaign forgets, decays, or declines to pass a claim on: every descendant
inherits, always. So the distribution measured here is the *ceiling* on
transmission depth — the shape myth takes when nothing opposes it. Campaign 2's
forgetting is a **shift against this curve**, and without it that campaign has
nothing to measure its own mechanism against.

**Known before predicting** (§4.1): the founding tree has 704 occupations, 46
roots, max depth 21, median depth 8, branching at most 5. **Not computed at
the time of writing:** the distribution of hop counts over (event, holder)
pairs, which is what the prediction is about. A branching tree concentrates
pairs at short distances; a chain-like one spreads them evenly. 704 nodes over
46 roots is about 15 per lineage against depths reaching 21, which does not
settle it either way.

**H1 — myth stays near its source.** Median `hops` over all pairs is **<= 2**.

**Decision rule** (a branch table cannot be wrong where a prediction can):

```
  median hops <= 2   -> H1 CONFIRMED. Lineages are bushy: most communities
                        holding a claim are close to the event. Report the
                        full distribution, not only the median.
  2 < median <= 5    -> H1 REFUTED, and the honest reading is that the hop
                        distribution is simply the tree's own shape restated.
                        Say so: myth adds no structure the founding tree did
                        not already carry, and the baseline is still the
                        baseline.
  median > 5         -> H1 REFUTED, and this is the headline: lineages are
                        chain-like, so a typical holder is remote from the
                        event it holds, and campaign 2's decay will bite
                        hard and early.
  fewer than 500 pairs across the seed set -> NO VERDICT; report the count.
```

**H2 — the tail is thin.** Fewer than 10% of pairs sit at `hops >= 10`. The
null is a fat tail, which would mean whole lineages carry founding-era claims
to the present unchanged — interesting, and an argument that decay is the
first thing campaign 2 must add rather than a refinement.

**Do not retune the pair population to move either number.** If the result is
uncomfortable, the result is the finding.

## 7. Carried forward

- **`lectio difficilior potior`** → campaign 2's headline hypothesis.
- **Distortion model: *original antigenic sin*.** From immunology — a holder
  interprets a new variant using memory of the *first* one it met, badly. It is
  deterministic and a function of the holder's own history, which makes it
  strictly cheaper than a seeded random walk and consistent with belief-as-fold.
  Campaign 2.
- **Persons as holders** — refinement, campaign 2 (§4.2).
- **`independent_witnesses` and the echo ratio** — correct, deferred whole to
  campaign 2, which supplies the second route that makes them measurable (§5).
- **Conflict between lineages** — whether two peoples hold incompatible
  accounts of one event. Needs two parties per event *and* distortion, so it
  is campaign 2 at the earliest.
- **`KNOW-lost-revision`** — keeping the contradiction when a belief changes.
  Needs revision, which needs diffusion. Campaign 2.

## 8. Definition of done

- `Provenance` and `Claim` in the kernel, `#![warn(missing_docs)]`-clean, with
  `type-audit:` tags on every primitive at a `pub` boundary.
- `windows/hearsay` deriving claim sets from committed facts; layering test
  green (a window may depend on domains; it must not be depended on).
- A lab metric for the median hop count registered, **with the metric cost
  measured before registration** — nine studies declare `"metrics": "all"` and
  there is no opt-out flag, so an expensive metric is a permanent ~2000-world
  cost (`windows/lab/CLAUDE.md`).
- The preregistered readout run and reported **at the strength the measurement
  supports**, null or not.
- Chronicle entry, book freshness sweep, retrospective, and the registry rows
  in §7 filed — no idea dies in conversation.

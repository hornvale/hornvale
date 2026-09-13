# The Cadastre — design

**Status:** approved at G3 (Nathan, 2026-09-12); merge gate passed, shipped as decisions 0987 and 0988 · **Date:** 2026-09-12 · **Campaign:** The Cadastre ·
**Ledger:** `docs/superpowers/ledgers/2026-09-12-the-cadastre.md`

Complete `technologies/asimov-1989` from 41 items to **301**: the catalogue's
three named arcs, union everything it attests before 1700, closed under its own
`Built on` relation. Repairs a truncation The Kiln documented rather than hid,
and ratifies the rule that makes the repair non-optional for every future
corpus drawn from a linked source.

## 1. The defect this repairs

Decision 0386: a corpus's demands are **derived** by transitive closure over
`presupposes`, never written down. So what a corpus asserts is not its item
list — it is the closure of its item list.

`presupposes` may name only in-corpus items. Any corpus drawn from a *subset*
of a linked catalogue therefore loses its outside edges at authoring time,
silently, before any resolver could see them. The Kiln states the damage
plainly:

> `presupposes` NAMES ITEMS IN THIS CORPUS AND NOTHING ELSE, WHICH DROPS REAL
> PREREQUISITES ON PURPOSE. … The consequence is that the derived demand set
> UNDER-DESCRIBES every such item's real prerequisites.

`inv-printing-press` really is built on steel; the lattice says it is built on
nothing. **The corpus cannot compute what it already claims.**

This is structural, not an authoring slip, and it will recur in every family.
Hence §7: the durable deliverable is the rule, not the 260 items.

## 2. What is NOT being claimed

**This is not a census.** A census of the whole catalogue was specified,
adopted at ledger #1, and withdrawn at #7. Two measurements killed it:

- **It cannot be scored honestly at that size.** The cheap method — a keyword
  sieve over the 1,787 idea-registry rows — was tested against the only answer
  key that exists, The Kiln's 41 hand-scored items with their 6 `deferred`. It
  recovered **3 of 6** while flagging 23 of 41 for adjudication. Every miss
  lands on `absent`, which ledger #12 established is *this family's flattering
  result*. An instrument whose error runs entirely toward the self-serving
  answer is broken, not noisy. Tuning it against those 6 is refused too: 6
  positives is the only key this campaign will ever have, and fitting to it
  consumes the control.
- **~1,400 of its cells could never move.** 43% of the catalogue is attested
  1900 or later and 64% of it is filed under "Science". Those items score
  `absent` today and under every model Hornvale will plausibly have. A
  falsifier's worth is its power to separate the model we have from the model
  we want; permanently-frozen cells have none.

The census keeps one good question — does the `absent` *fraction* differ
between the arc sample and the whole population? — which survives as an
idea-registry row, not a campaign.

## 3. Scope — four acts, three of them in play

```
                        | blind? | in scope | why
------------------------+--------+----------+---------------------------
1. enumerate + close    | yes    | YES      | §4, the selection rule
2. import the lattice   | yes    | YES      | the source's own edges
3. score                | NO     | YES      | 260 items, by hand
4. band a criterion     | NO     | NO       | FORBIDDEN -- see below
```

**Act 4 is closed to this campaign and every future one.** Family law: any
session that has read the distribution a criterion bands is disqualified from
re-banding it, and band quality can only improve before first measurement.
Every session now alive has read it. This campaign authors **no `criterion`
and no `statistic`** on any new item — consistent anyway, since those are
`None` wherever an item carries no measurable claim and an `absent` item
carries none.

## 4. Selection — two blind rules, unioned, then closed

> **Seed:** every item linked from the catalogue's three named story arcs,
> plus every item the catalogue attests before **1700**.
> **Then:** close the seed under the catalogue's own `Built on` relation.
> Nothing is added or removed by hand at any point.

Measured: **301 items, 401 edges, 0 cycles, 1 root (`biped`)**. 260 new to
score. The closure adds only 7 above the era cut (the steam chain), so the cut
is very nearly self-closing.

**The catalogue holds 1,486 items, not the 1,484 every prior count reported**
(Task 1, ledger #9). Two slugs — `2,4-d` and `mössbauer-effect` — are dropped
by an ASCII-only slug pattern, one for its comma and one for its `ö`. The
Kiln's committed provenance carries 1,484, and so did this campaign's own
"third independent fetch", which reused the same pattern and therefore
confirmed the instrument rather than the fact. Both recovered items postdate
1700, appear in no arc, and are named in no item's `Built on` list, so the
seed, the closure and the population are unchanged. The population is right;
the population's stated denominator was not.

### 4.1 Why two rules rather than one

They are near-independent. Measured at the 500 CE cut where the comparison was
first run: closure alone 77, era alone 98, **overlap 41**. Each catches what
the other drops.

Closure alone excludes `pottery` — which the idea registry's `TECH-2` names
explicitly as *"the pyrotechnology ladder — pottery/ceramics gate
storage→surplus"* — along with irrigation, calendar, law, medicine, coin,
arch, aqueduct, sickle, fermentation. A rule that drops the capability this
project's own registry most explicitly plans is not principled, whatever its
pedigree.

Era alone reintroduces the §1 truncation and is refused for that reason.

### 4.2 Why `Built on` and not `Led to`

Prerequisites terminate; consequences do not. `Built on` closure bottoms out at
a single root, `biped`. `Led to` closure would propagate across a connected
catalogue without limit. **Downward closure is the only direction that is both
blind and finite**, which makes it a rule rather than a preference.

`Led to` is also redundant: over the 179 item pages fetched for this spec it is
the *exact* inverse of `Built on` — 210 comparable edges each way, **zero**
present in one direction and absent in the other. Task 1 re-checks this over
the full graph and records any disagreement; a site inconsistency silently
unioned would launder the source's error into ours.

### 4.3 Why an era cut cannot flatter us

Selection keyed to our own code makes the selection the measurement (The Kiln,
ledger #7), and "items appropriate to our scenario" looks like exactly that.
It is not, and the reason is §5: **there is no living-world technology model,
so no item's verdict depends on its date.** Every item scores on the same
basis whatever era it comes from. The cut changes *which capabilities are
examined*, never *how well the world does on them* — the gaming vector is
absent, not merely unlikely.

### 4.4 The threshold is a judgement, recorded as one

1700 comes from the setting's intellectual reach as its author reads it —
*"perhaps we've had a Descartes somewhere; I don't think we've had a
Newton"* — with deliberate headroom. Stated plainly so no later reader mistakes
it for a derivation:

`< 1700` **includes** Newton (`Calculus` 1669, `Laws of motion` and `Universal
gravitation` 1687, 57 items in the 1650–1700 band). `< 1650` matches the phrase
literally and yields 249 items. 1700 was kept deliberately: **a corpus that
stops exactly where the world is thought to stop can never report that the
world stops too early.** An `absent` on universal gravitation is a finding;
excluding it is a decision that leaves no trace in the output.

## 5. What the instrument is actually pointed at

`TechHorizon`'s four values date **discovered ruins**, not living peoples.
Every consumer is occupation- or vestige-side:

```
domains/history/src/record.rs:142    OccupationRecord.tech
domains/history/src/flesh.rs:644     occ.core.tech >= Iron
windows/worldgen/src/vestige.rs:321  tech: TechHorizon::Iron
domains/history/src/lib.rs:73        OCC_TECH = "occ-tech"
windows/worldgen/src/history_bake.rs:2900  tech_for(year) -> a PAST occupation
```

**There is no living-world technology model at all.** This strengthens The
Kiln's F2 finding rather than weakening it: it is not merely that the shipped
model cannot express loss — the thing modelled is ruin-dating, and the living
world has no capability state to lose in the first place.

Two consequences bind this spec. It is why §4.3 holds. And it is why §6 scores
no `inapplicable`: that verdict means the world **deliberately** lacks a
precondition, and nothing here was decided.

## 6. Verdicts

`absent` unless an anchor exists. `deferred` where an idea-registry row plans
the capability and is not `shipped`. **No `inapplicable` anywhere**, per §5 —
and because 0136 deliberately leaves its tally unratcheted, so routing mass
through it puts the campaign where nothing watches.

Every item whose verdict is **chosen** — nothing `absent` anywhere in its
`presupposes` closure forces it — carries a `disclosure` stating what its
author knew of the model. Ledger #12 is binding here and runs against our
interest: *an `absent` whose verdict turned on having read the model owes a
disclosure.* Do not key this on roots; family law refuses that proxy
explicitly, and the resolver computes the chosen set itself.

**Scoring is by hand, in batches, and the 41 arc items are a held-out
control.** Scoring agents receive the item pages and the registry, never the
committed corpus, so they cannot see The Kiln's answers. If the sweep does not
independently reproduce those 6 `deferred`, the sweep is unreliable and the
report says so with the number.

## 7. The durable deliverable — family law and a decision record

> A corpus drawn from a source that carries its own dependency relation is
> **closed under that relation**, in the direction that terminates. A corpus
> that cannot close states the truncation and its size in `provenance`.

This generalises past `technologies/` to every family whose source is
graph-structured, and it is worth more than the 260 items. It lands as family
law in `technologies/CLAUDE.md` and as a decision record.

## 8. The cross-corpus obligation

A row cited by one corpus must be ruled on by every corpus. Verified rather
than reasoned — one synthetic `deferred` citing `registry:TECH-4` was added to
`asimov-1989` and the **sibling** was audited:

```
error: technology coverage audit found 1 finding(s) for `henrich-2004-extended`:
registry:TECH-4 is cited by asimov-1989's item inv-probe-xyz but is never
mentioned anywhere in henrich-2004-extended ...
```

So the scaling cost is distinct `registry:` citations, not items, and each
reddens a frozen sibling until it rules.

**Discharge: one written class refusal in `henrich-2004-extended`'s
`provenance`,** naming each newly-cited row and the single demand none
discharges — that corpus scores loss *trajectory*, and an invention-side reach
row cannot discharge a trajectory demand. The rule's own text permits this
("refuse it in writing… naming the demand it does not discharge") and calls
itself "a grep-level floor, not the judgement itself."

**This edits a frozen corpus and is the campaign's most contestable act.** The
defence: the re-freeze prohibition is written about criteria and bands, and a
refusal that moves no verdict is not a band. No item, verdict, anchor,
statistic or criterion in that file changes. It wants its own decision record.

## 9. Reporting

**The 41 arc items keep their ids and arc attribution.** 0136 clause 2 forbids
an instrument silently switching what it measures; the committed 35/6/0 reading
stays comparable precisely because its population stays identifiable. The
report renders both series.

**The headline is a rate, never the raw `absent` count.** `absent` rises
because the denominator did. The comparable quantity is the fraction, and the
finding is whether it *moved* between the arc sample and the completed corpus —
a real question with a real answer in either direction, including "it did not."

## 10. What moves

- `technologies/asimov-1989.technology.json` — 41 → 301 items; the dropped
  edges restored and the per-item `note` apologies for them deleted, since they
  will no longer be true; `ordered` stays `true`, so items are inserted in
  attested-date order with every prerequisite preceding its dependent.
- `technologies/henrich-2004-extended.technology.json` — `provenance` only (§8).
- `cli/src/technologies.rs` — `novelty_baseline("asimov-1989")` off 35.
- `cli/tests/suite/technology_corpus.rs` — the two item-count assertions.
- `docs/audits/technology-coverage-asimov-1989.md` — regenerated; already
  declared in `docs/generated-paths.txt`.
- `technologies/CLAUDE.md` — §7's rule, and §6's no-`inapplicable` rule.
- Two decision records: §7's closure rule, §8's post-freeze sibling edit.
- Chronicle, retrospective, freshness sweep.

**Not prose-only** — it touches Rust, so it pays the full phase ladder at merge.

## 11. Success criteria

1. `technologies check` exits 0 for both corpora.
2. The corpus parses at 301 items with an acyclic lattice; the count
   assertions pin 301.
3. Every one of the 41 arc items is present by id with its arc named.
4. Every `presupposes` edge the source states between two in-corpus items is
   present — closure is complete, not approximated.
5. The report renders both series and states each `absent` fraction.
6. The held-out control (§6) is reported with its number, whatever it says.
7. No item carries `inapplicable`, `criterion` or `statistic` that did not
   before.
8. `make gate-commit` green; the merge passes the full ladder.

## 12. Stop conditions

- **A cycle in the full graph.** `parse` rejects one, so this stops authoring.
  Breaking a cycle is a selection decision and must be stated before it is
  applied — a spec amendment and a ledger entry, never an implementer's call.
- **The held-out control fails badly.** If hand-scoring does not reproduce The
  Kiln's 6 `deferred`, report the number and stop before scoring the remainder;
  the method is the deliverable's foundation and a broken one invalidates the
  rest.

## 13. Out of scope

- **Replacing `tech_for` with an acquisition model** (`TECH-acquisition-model`).
  That is the successor campaign; this one sharpens its falsifier.
- **Any criterion or band** (§3).
- **A second source**, and **a census** (§2).
- **Reading the column as a roadmap.** Decision 0095: an instrument with known
  bias is never a standard and never a backlog. `provenance` says so.

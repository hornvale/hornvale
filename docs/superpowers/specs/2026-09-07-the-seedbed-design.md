# The Seedbed — a corpus family for grown regularities

**Campaign:** The Seedbed · **Date:** 2026-09-07 · **Status:** design, awaiting G3

**Ledger:** [`docs/superpowers/ledgers/2026-09-07-the-seedbed.md`](../ledgers/2026-09-07-the-seedbed.md)

---

## 1. The question this family asks

Hornvale has three corpus families and they ask three versions of one question:

| family | question | resolved against |
| --- | --- | --- |
| `tropes/` | can the world **represent** this situation? | the concept registry |
| `systems/` | does the program **implement** this capability? | repository facts |
| `sentences/` | can the grammar **produce or parse** this demand? | `domains/language` |

All three measure *static reach*. `docs/audits/trope-matrix.md` says so in its
own header: "it scores **representability only**". None of them asks whether the
world, left to run, **produces** anything.

Epstein and Axtell's *Growing Artificial Societies* (1996) is one long argument
that this is the question that matters:

> Perhaps one day people will interpret the question "Can you explain it?" as
> asking "Can you grow it?" … it is not the emergent macroscopic object per se
> that is surprising, but the **generative sufficiency of the simple local
> rules**.

A fourth family, `regularities/`, asks: **does the world grow this?** It
resolves against measurement over the committed census.

### 1.1 Why this is a family and not an extension

Decision 0135 opens a family when the *resolution basis* differs, accepting a
duplicated loader, renderer and ratchet across families that will never share
code. Measurement over the census is a fourth basis, distinct from a registry,
from repository facts, and from a grammar.

The near neighbour is the Domesday, not `systems/`. `studies/expectations.json`
already holds 30 preregistered claims checked by frozen detectors
(`windows/lab/src/domesday/detect.rs`) over this same census. It is not the same
instrument, and the difference is exactly provenance: **the Domesday holds
claims Hornvale makes about itself** ("temperature tracks year-length,
negative") and checks them for internal coherence. **A regularity corpus imports
a claim from outside** and asks whether the world reproduces it. Under 0095 —
a corpus is an instrument carrying a declared provenance and bias, never a
standard — those are opposite objects. Folding one into the other would give
`expectations.json` a provenance field it has no business carrying.

They compose: the Domesday says the world is *coherent*; a regularity corpus
says the world is *right*.

## 2. What the family measures, and the one thing it must not claim

An item is a **macro-regularity**: a statistical or structural pattern that
belongs to a collection and is drawn from outside Hornvale. The book supplies
the taxonomy, and the corpus carries it as a declared field:

- **Type 2** — only the collective property is meaningful. A skewed wealth
  distribution; an individual has no distribution.
- **Type 1** — the individual property is meaningful but only the collective
  exhibits it. Sugarscape's diagonal migration wave: "the group adopts a heading
  unavailable to any individual."

Declaring the type is not decoration. Hornvale today has two distributional
statistics in 290 census columns and, as far as this campaign has found, no
type-1 statistic at all. A corpus that cannot report that asymmetry is hiding
its most useful finding.

**What a verdict never claims.** Under 0095 a `grown` verdict is a reading
through one biased instrument, not a grade. Sugarscape is an abstract lattice
model with two commodities, combat and bilateral barter; Hornvale is a
deep-history settlement simulator with no economy. Many items will be
structurally inapplicable, and that is a property of the instrument. The
`systems/wolverson-2021` precedent is followed exactly: an inapplicable item is
**included with its reason**, never dropped, so the reason travels with it.

## 3. Schema

`regularities/<corpus>.regularity.json`. Both of decision 0135's schema
commitments are inherited: a generalized item unit, and ordinal readings only
for a corpus that declares its ordering (this one does not — Sugarscape's rules
compose, they do not ladder).

```json
{
  "corpus": "sugarscape-1996",
  "unit": "regularity",
  "ordered": false,
  "population": "the-census",
  "provenance": "...",
  "frozen": "before first measurement, The Seedbed",
  "items": [
    {
      "id": "sug-wealth-skew",
      "title": "Holdings are distributed far more unequally than the endowments producing them",
      "source": "Ch. II, 'Emergence'; Animation II-3",
      "emergence_type": 2,
      "statistic": "rank-size-slope",
      "criterion": { "kind": "median-in-band", "lo": -1.2, "hi": -0.8 },
      "verdict": "flat",
      "anchor": "doc:book/src/domesday/demography.md",
      "note": "..."
    }
  ]
}
```

**The criterion is selected, never defined** (decision 0011). `kind` is a closed
enum implemented and unit-tested in the resolver; the corpus supplies
parameters only. Wave one implements four kinds, and no more:

| kind | parameters | meaning |
| --- | --- | --- |
| `median-in-band` | `lo`, `hi` | the statistic's median over the population lies in `[lo, hi]` |
| `fraction-in-band-at-least` | `lo`, `hi`, `min_fraction` | at least `min_fraction` of worlds lie in the band |
| `median-at-least` / `median-at-most` | `bound` | one-sided |
| `present-on-fraction` | `min_fraction` | the statistic is non-`absent` on at least this share of worlds |

A fifth kind is a code change, a review and a test — deliberately, so that
"add a criterion shape" is never a data edit.

## 4. Verdicts

Six-valued, extending 0136's refused/deferred/absent triple with the
distinction that is this family's entire reason for existing:

| verdict | meaning | anchor |
| --- | --- | --- |
| `grown` | measured; the criterion is met | `doc:` |
| `flat` | measured; the criterion is **not** met | `doc:` |
| `refused` | a ratified decision says Hornvale deliberately will not | `decision:` |
| `deferred` | the statistic cannot be computed yet | `registry:` |
| `absent` | cannot be computed and nobody has registered it — the honest red | none |
| `inapplicable` | about Sugarscape's own abstraction, not a world regularity | `reason:` |

### 4.1 `unmeasured` — a lifecycle state, not a verdict

**Amended after G3, during planning.** The criterion is the *prediction* and is
frozen before measurement (0016); the verdict is the *record* of what measuring
found. Those cannot both be authored at freeze time without one of them being a
guess. So a measurable item is authored `"verdict": "unmeasured"`, and the first
run records what the census says.

`unmeasured` is **not a coverage verdict**: the report tallies the six above and
lists unmeasured items separately, and a ratchet test refuses any that survive
the first measurement. The value of the distinction is that it makes the freeze
*structural* — the corpus is authored and committed in a task that runs before
any evaluation code exists (plan Task 2), so items cannot be authored while
their answers are visible.

`flat` is the addition. The other three families have no way to say **"we do
this, and we get the wrong answer"** — their vocabulary distinguishes only
degrees of absence, because a grammar either parses a sentence or does not.
A generative test's most valuable output is precisely the measured miss, and
collapsing it into `absent` would discard the finding.

## 5. Documentation as the terminal anchor

Ledger entry #2. A new anchor kind, `doc:<path>`, and one rule that makes it
mechanical rather than aspirational:

> `doc:<path>` resolves only if `docs/generated-paths.txt` gives that path a
> generator. Its second column names the path's author; a path whose author is
> `none(hand-written prose, never regenerated)` is **refused as an anchor**.

Hand-written prose asserting a capability is a restatement of the failure
decision 0330 names — "a token added on optimism moves the score without moving
the grammar". Generated, drift-checked prose cannot overclaim, because it is a
pure function of the committed census.

**Arity picks the surface.** A regularity is a property of a *population* of
worlds; `windows/explain` and the almanac narrate *one*. A within-world claim
may anchor to a per-world surface; a cross-world claim anchors to the Domesday
or a laboratory page. Wave one's items are all cross-world, so all anchor to the
Domesday.

**The bound on what this buys, stated so it is not later mistaken for a second
witness.** A passage generated from the same census the verdict is computed from
is the same fact rendered twice. It buys **falsifiability by a reader**, not
independent evidence. Its teeth therefore depend entirely on the passage
carrying the criterion and the verdict, not just the number. Today the Domesday
renders:

```
n = 1000 present, 0 absent (of 1000 worlds)
| min | p25 | median | p75 | max | mean |
```

which nobody can be wrong about. The renderer gains a claim line for any metric
a frozen corpus scores:

```
sugarscape-1996 `sug-wealth-skew`: predicted median in [-1.2, -0.8]
(Zipf/Auerbach rank-size); measured -0.578, 1.7% of worlds in band. FLAT.
```

## 6. The regression guard

The reversibility finding from ledger #1, and the one place this family departs
from 0135's template. Coverage in `tropes/`, `systems/` and `sentences/` is a
ratchet: a built capability stays built. **A grown regularity is emergent, and
any retune of the history bake can destroy it silently** while every existing
gate stays green.

So the authored verdict and the computed verdict must **agree in both
directions**, the same shape decision 0261 requires of any rule duplicated on
purpose:

- authored `grown`, computed flat → **RED**. A regularity was lost.
- authored `flat`, computed grown → **RED**. Delete the stale pessimism; a real
  gain is claimed deliberately, in a commit that says so.

Only the two measured verdicts participate. `refused`/`deferred`/`absent`/
`inapplicable` are anchor-checked, exactly as `systems/` checks them.

**Where it runs — a branch table, not a prediction.** The resolver reads a
committed CSV and computes summary statistics; it builds no world. Measure the
test's duration at implementation, then:

- under the sub-floor roster's threshold → it belongs in `gate-commit`, and gets
  a roster row like any other test;
- over it → it belongs to the stage gate, and the commit gate keeps only the
  corpus's parse and freeze checks.

Do not assume the first branch. A new crate cannot pass the commit gate without
a hand-authored roster row, and only a green chamber run writes one.

## 7. Scope

**In, wave one:**

1. `regularities/` with `sugarscape-1996.regularity.json`, frozen before
   measurement.
2. A resolver — `cli/src/regularities.rs` plus `hornvale regularities
   report|check|matrix`, mirroring `cli/src/systems.rs`.
3. The `doc:` anchor kind and its `generated-paths.txt` resolution rule.
4. The Domesday claim line (§5).
5. The two-way regression guard (§6).
6. `docs/audits/regularity-coverage-sugarscape-1996.md`, drift-checked, and its
   row in `docs/generated-paths.txt`.

**Out, with reasons:**

- **New census metrics, and therefore any census refresh.** Checked: the
  committed census already carries `rank-size-slope`, `settlement-count`,
  `mean-population`, `total-population`, `raid-victim-rate`,
  `raid-initiator-rate`, `climate-displacement-events`,
  `tribute-relations-standing`, `granary-raid-phase-concentration` and
  `cascade-rules-fired-*`. Wave one scores against the census exactly as it
  stands and moves no reference.
- **The time axis.** A study is `seeds x pin_sets x metrics` — one number per
  *finished* world. The census is structurally blind to the oscillating
  (migration waves, boom–bust) and accumulating (concentration over time)
  regularities that carry most of the book's findings. Those items are declared
  in the corpus and score `deferred` against a registry row. The deferred set is
  therefore principled rather than residual, and it is the roadmap.
- **Generalizing `doc:` to the other three families.** A cross-family change
  that needs its own decision record. `regularities/` demonstrates it first.
- **The environment back-arrow.** The book's coupling is `A' = f(A,E)`,
  `E' = g(A,E)`. Hornvale has the first: `eff_capacity`
  (`windows/worldgen/src/history_bake.rs:1754`) is niche capacity times an era
  climate factor, with no occupation term — population does not degrade the land
  that feeds it. That is a world-mechanism campaign, and it should be *driven by
  this instrument's red* rather than bundled with it.

## 8. Freeze and falsification

The corpus is frozen before measurement (decision 0016), with one **disclosed
exception that must not be quietly absorbed**: `rank-size-slope`'s distribution
was measured during the brainstorm that motivated this campaign, before any
corpus existed. The `sug-wealth-skew` item is therefore **not a blind test**,
and its entry says so in its own `note`. Every other item is authored against
the book and frozen before its statistic is looked at.

**Falsification clause.** If more than half the corpus scores `inapplicable`,
the instrument is measuring a world Hornvale is not, and the finding is that
Sugarscape is the wrong first corpus — not that Hornvale is failing. Report it;
do not re-author the corpus to raise the score.

## 9. Success criteria

1. `regularities/sugarscape-1996.regularity.json` is frozen, provenance-stamped,
   and its item count is asserted by a test.
2. Every verdict except `absent` cites an anchor the resolver re-checks.
3. Every `doc:` anchor resolves to a path with a declared generator; a `doc:`
   anchor into hand-written prose is refused, and a test proves the refusal.
4. The two-way regression guard fails in both directions, each proven by a test
   that has been observed red.
5. The Domesday renders a criterion-and-verdict claim line for every scored
   metric.
6. The committed coverage report is drift-checked and declared in
   `docs/generated-paths.txt`.

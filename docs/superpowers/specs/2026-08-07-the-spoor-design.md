# The Spoor: what a fact leaves behind — Design

**Campaign:** The Spoor · **Date:** 2026-08-07 · **Branch:** `the-spoor`

The question that opened this was "a rule system that observes the ledger and
executes code in response to matching events." It collapsed, under four
ideonomy passes, into something smaller and better specified: the world already
has a read channel that costs nothing, and the interesting class of thing it
cannot currently express is **the residue of a committed event**.

This spec builds that class. It does not build a rule engine, and section 2
records why not.

## 1. The state of play, verified rather than assumed

Every claim in this section was checked against the tree at `d9c734e0`, and
**re-checked at `bb16001b`** after The Reassay landed ~40 commits and decisions
0101–0112 on top of it. All counts and shapes below are unchanged across that
absorption; the one thing that did change is recorded in §6.1.

**The read channel exists and is pure.** `PhenomenaSource::phenomena(&self,
ctx) -> Vec<Phenomenon>` (`kernel/src/phenomena.rs:233`), documented as
"Implementations must be pure: same context → same phenomena."
`ObserverContext` carries `place`, `time`, `lens`, and `position`.

**Locality already works, at planetary radius.** `domains/climate/src/
provider.rs:622` returns nothing for a position-blind observer;
`domains/astronomy/src/provider.rs:1755` culls the visible sky by hemisphere.
The machinery for "you can only perceive this from here" is shipped and has
never been pointed at a five-metre radius.

**Perceive-or-not is already the contract.** `PerceptionLens` weights salience
per venue and `VISIBILITY_FLOOR` **culls** rather than dims — the constant's own
doc: "a star dimmed to a fiftieth is not a faint star, it is a star you cannot
see."

**The game layer commits consequences only.** `windows/vessel` commits
`agent-at`, `drank`, `rested`, `eaten`, `disposition-shift`, `turned-hostile`.
There is no committed perception predicate. `disposition-shift` and
`turned-hostile` are the tell: the *seeing* was never stored, the *reaction to
it* was.

**Three gaps, each verified:**

- **`Phenomenon` has no duration field.** It is `{kind, referent, period_days,
  salience, venue}`. `period_days` is *recurrence*, not *duration*. A source can
  compute a falling salience, but the type cannot say how long anything lasts,
  so nothing downstream can reason about it.
- **`Venue` is `DaySky | NightSky | Ambient`**, and `PerceptionLens` is three
  weights keyed on it. A scent is `Ambient` — the same bucket as weather — so
  "keen nose, poor eyes" is currently inexpressible.
- **`Phenomenon` carries no link to its cause**, and `Fact` carries
  `provenance: String` and `place: Option<EntityId>`. The write channel knows
  where a thing came from; the read channel discards it.

**Refactor surface, counted rather than guessed:**

```
  grep -c 'Phenomenon {'   -> 46 literal construction sites
  grep -c 'PerceptionLens {' -> 15 construction sites
  grep -c 'Venue::'        -> 75 use sites
```

**Blast radius, checked:** `Venue` and `Phenomenon` appear **nowhere** in
`clients/`. Widening this type is *not* a cross-repo scene-schema change. It
does reach committed artifacts through `windows/almanac`, `windows/lab/src/
metrics.rs`, `windows/book`, and `cli/src/concepts.rs`, so it carries an
artifact regen (three seed-42 almanacs, the concept registry and manifest).

## 2. Why this is not a rule engine

The original framing was push: a matcher hung on `Ledger::commit` that fires
code. Four objections, each concrete, retired it:

1. **Commit order becomes an unnamed save-format contract.** Which rule fires
   first, one hop or fixpoint, whether a registration reorder is stable — every
   answer is as load-bearing as a seed label (0006) with none of the naming
   discipline that makes seed labels greppable.
2. **Reentrancy is structural.** `Ledger::commit` takes `&mut self`
   (`kernel/src/ledger.rs:233`). A rule that commits during a commit needs a
   deferred agenda, and an agenda drained elsewhere is the schedule again.
3. **The trigger is three-valued and ambiguous.** `commit` returns `Ok(true)`
   on append, `Ok(false)` on idempotent dup, `Err(Contradiction)` on clash.
   Firing on dedup-no-ops makes worldgen sensitive to arrival multiplicity.
4. **`kernel/src/schedule.rs` already is the declarative version.** A `System`
   declares `reads`/`writes`; the schedule is the topological order of that DAG,
   tie-broken by stable label, with a single-writer check. Cross-domain coupling
   without cross-domain dependency is *already solved*, in the pull direction.

**The governing distinction:** rules that write **facts** are expensive and
belong in the schedule; rules that emit **phenomena** are nearly free, because
nothing is committed — no save-format contract, no ordering-at-commit, no
reentrancy, no memory growth. Everything this campaign builds is on the free
side.

## 3. The keystone: a trace is the transducer between the registers

> **trace** — *a phenomenon whose salience is a decaying function of the age of
> a committed fact.*

A footprint. A bloodstain. A scorch. Ash. A ruin. A scar. A corpse's smell. All
one family, all the same shape: **an irreversible event happened, and the world
is legible about it for a while, and then it isn't.**

Under decision 0100's three registers, a trace is what carries a datum from one
to the next:

```
  FACT        "a hill dwarf died here, day 300"    committed, checked
                     |
                     v
  PHENOMENON  "a smell of rot"                     derived, coherent
              + an opaque origin handle            by construction, free
                     |
                     v   (an observer perceives, and concludes)
                     |
  MYTH        "a dwarf died here" -- held by X     derived, FALLIBLE,
              (or "orcs passed through")           holder required (0100 r2)
```

0100 says "Myth is new, and has no channel today." That is the program's
endpoint and **not** this campaign. This campaign builds the middle row and the
arrow into it, so that the myth channel has something to be generated *from*.

**Why the trace earns the origin handle.** Decision 0003 accepts as a cost that
"a consumer may never learn which system produced a given observation," and
CLAUDE.md's gloss is explicit that this is a cost, not a prohibition: "a
consumer must not be *handed* a source, but a future campaign is free to let an
observer **achieve** an identification and be wrong about it."

Without a link from appearance back to cause, **an observer cannot be wrong in
a checkable way.** If an NPC smells rot and concludes "a corpse," nothing can
adjudicate the conclusion. Achieving an identification and being wrong about it
requires the truth to be available to the *adjudicator* while withheld from the
*consumer* — and those are different readers, which a missing field cannot
distinguish.

## 4. The decisions

### 4.1 `Phenomenon.origin` — withheld by discipline, not absent by construction

Add an opaque handle to the causing fact. Consumers may not branch on it; only
inference/adjudication may compare it.

The precedent for a field with a stated read-discipline is in this same file:
`Referent`'s doc says "This is **all** a phenomenon says about what it is about,
and the only field a consumer may branch on." The new field carries the mirror
rule, stated in its own doc comment, and — per the *name-the-direction* rule —
states which direction it enforces: *it permits adjudication of an achieved
identification; it does not permit a consumer to shortcut one.*

Three things this is **not**, and the distinction matters because I conflated
them earlier in the brainstorm:

- **not the producer** (which subsystem emitted it) — that stays withheld;
  it is 0003's architectural decoupling and is still correct.
- **not the holder** (whose claim this is) — that is 0100 rule 2, belongs to
  the myth register, and is out of scope here.
- **the source** (what in the world caused it) — this, and only this.

### 4.2 `Phenomenon.modality` — a new field, not a wider `Venue`

`Venue` is overloaded: it carries both *sense channel* and *extent*. The
temptation is to widen it. The cheaper and more honest move is to **split by
adding**: `Venue` keeps meaning "character — where this lives"; a new `Modality`
means "which sense," and `PerceptionLens` gains matching weights.

Rationale: additive, does not disturb 75 `Venue::` sites, and leaves each field
with one job. Cost is 46 `Phenomenon {}` sites and 15 `PerceptionLens {}` sites,
which the plan must budget for explicitly.

### 4.3 Decay is a source-side function, not a stored lifetime

A trace has no lifecycle, no spawn, no cleanup. `phenomena(&self, ctx)` is pure
and `ctx` carries `time`, so a trace is `salience = f(ctx.time − fact.day)` —
a function that happens to return a non-zero value for a while. Nothing exists
between observations, and `as-of` works for free: ask about day 400 and you
smell what was there on day 400.

**A duration field is added anyway**, because the type currently cannot *say*
how long a thing lasts and so nothing downstream can reason about it. It is
declarative metadata, not a countdown.

### 4.4 The co-driven seam is the real mechanism

`PhenomenaSource` is constructed by the composition root via `WorldContext`
(`kernel/src/domain.rs`), which works at genesis where the fact set is frozen
and is awkward in a live session where corpses accrue. A trace source needs to
read *live* committed history.

This is a borrow, not a rule engine. It is the smallest change that makes the
co-driven case work, and the plan must state the lifetime shape explicitly
rather than discovering it in execution.

## 5. Scope

**In:**

1. `Modality` + `Phenomenon.modality` + `PerceptionLens` modality weights.
2. `Phenomenon.origin`, with its read-discipline stated in the doc comment and
   the direction it enforces named.
3. A duration field on `Phenomenon`.
4. The co-driven source seam (a phenomena source reading live committed facts).
5. **One instance end-to-end**: a corpse-scent trace — place-gated, age-decayed,
   perceived through the possessed species' lens.

**Out, and registered rather than dropped:**

| deferred | where it goes |
|---|---|
| the myth channel + holder | next campaign; 0100 rule 2 already binds it |
| holder-scoped contradiction check | argues against 0100's `KNOW-unchecked-store` answer; needs its own record |
| the committed myth (a lie) in the play ledger | a gap in 0100's own authorship table |
| an `Interval`/`Span` type | none exists anywhere; MAP-19 already names Allen intervals |
| splitting salience into attention + resolution | The Vantage's orthogonal-axes keystone, applied to perception |
| push-on-the-read-side (standing interests) | a performance question, not a correctness one |

## 6. Preregistration

Frozen before the code that would move it (0016). Both hypotheses are measured
on the **real census population**, not on fixtures — the 28/255-authored versus
2/255-real failure is on the record.

**H1 (the axis discriminates).** Adding modality is only worth its 61 touched
sites if species actually differ on it. Predict: **at least two species in the
census differ in whether they perceive a given trace at a fixed age and
distance.** Falsification: if modality weights derive from existing traits that
do not vary across species, the axis is decorative and the campaign **ships the
null** — modality reverts to a constant and section 4.2 is withdrawn.

This is a discriminates-check, not a coverage-check, because a guard that is
green because nothing varies is vacuous and green.

**H2 (the window has both ends).** A trace's perceptibility window must have a
floor *and* a ceiling — a median-only floor cannot see a tail. Predict the
median time-to-imperceptible for a corpse trace under the median species lens
falls in **(0, 365) days, exclusive at both ends**, and additionally that the
**99th percentile is under 365 days**. A trace perceptible for a year is a
monument, not a spoor.

**H3 (the origin handle is adjudicable).** A test must demonstrate an observer
achieving a *wrong* identification that the adjudicator can detect as wrong.
Without that, 4.1 has shipped a field nobody can use.

### 6.1 How these are tested — decision 0112 forecloses the cheap route

Added on re-verification at `bb16001b`. **Decision 0112** (2026-08-07, one day
after this spec's other sources) rules that "the synthetic route is available
only when the behaviour under test reads committed facts; a behaviour that
re-derives from a generated sky, a sculpted globe, or any other live
computation cannot be synthesized."

A trace is *definitionally* a re-derivation — `salience = f(ctx.time −
fact.day)` computed at observation, from a source that reads live history
(§4.4). So **the hand-built synthetic world is not available to H1 or H3**, and
the plan may not budget for one.

Consequences, stated now rather than discovered in execution:

- **H1** already says "measured on the real census population, not on
  fixtures," which is the right route by accident. It is now the right route
  by rule, and it means H1's measurement is census-adjacent work with the cost
  that implies — not a unit test.
- **H3** did not name a route and now must: it needs a real generated world
  with a committed cause, an observer, and an adjudicator. That is the most
  expensive test in this campaign and the plan must size it explicitly.
- **The `#[ignore]` reason token** for anything that lands in the heavy tier is
  verbatim-matched; the plan states it rather than paraphrasing it.

0112's own retrospective note is the reason this subsection exists: it records
that the infeasibility "was found *after* the spec had passed G2 self-review,
the G3 package, and Nathan's approval." This spec is at exactly that point, so
the check is run here instead.

## 7. Risks

- **A vacuous modality axis.** Mitigated by H1's explicit falsification clause
  and its ship-the-null branch.
- **Artifact regen churn.** `Phenomenon` reaches the almanac, lab metrics, the
  book, and `cli/src/concepts.rs`. Adding a lab metric has reddened 34 tests
  before. The plan gets an explicit regen step, in the same commit that drifts
  the artifacts.
- **The origin handle leaking into a consumer.** A doc-comment discipline is
  enforced by review, not by the compiler. The plan should carry a test that
  asserts the direction, and the test must be shown to fail when the discipline
  is violated — "fails to compile" is not "would have caught the defect."
- **Worktree ceiling.** Two campaigns are already live (`the-delvers`,
  `the-lantern`); CLAUDE.md puts the Mac's working ceiling at two to three, and
  gates must be staggered.

## 8. Provenance of this design

Four ideonomy passes, each with a drawn method tuple:

1. *(none — direct analysis)* retired the rule engine against `schedule.rs`.
2. **dimension-identification + substitution / map / size, hierarchicalness,
   autonomy** — found the empty middle of the extent×persistence map, the
   missing duration field, and that salience carries both attention and
   resolution.
3. **combination + organon-construction / dictionary / connectivity,
   modularity, rate** — coined *trace*, found the missing interval type, and
   corrected "ephemeral" to "re-derivable."
4. **abstraction-lift + negation / periodic-grid / decomposability,
   discovery-vs-invention, naturalness** — lifted myth to a modal operator,
   decomposed it into holding × telling, and predicted *the secret* and *the
   accidental truth* as cells a plain belief store cannot represent.

Passes 2 and 3 independently found the same two empty cells by different
operators, which is the strongest evidence this design produced.

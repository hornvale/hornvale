# 0306. A host's cooperation is derived from doctrine and conduct, never authored

**Status:** Accepted (2026-08-26) · **Decider:** Nathan · **Relates:**
[0259](0259-conceptual-deficiency-is-derived-not-authored.md) (the discipline
this extends), [0021](0021-no-alignment-axis.md), [0256](0256-a-hosts-testimony-is-fallible-by-construction.md);
[The Reticence](../../book/src/chronicle/the-reticence.md)

In the context of a possessed host needing a reason to refuse, lie to, or
answer honestly the player riding it, we decided that **its willingness is
computed from two things the world already commits — what its people
believes a rider is, and what this rider has actually made it do — and no
table of per-people or per-host hostility is ever authored**, accepting a
willingness whose shape nobody chose in exchange for a distribution nobody
chose either.

## Context

The tempting implementation is a hostility scalar per people, or per
individual host, hand-tuned so the "hostile" cultures read as hostile. That is
exactly the failure decision 0259 already named for conceptual deficiency: if
the distribution is authored and then measured, the measurement recovers the
constant that was typed in.

Two inputs the world already computes carry the question honestly. The
**doctrine prior** is read off lexical coverage (`god`/`spirit`, or neither)
and, among settled peoples, the `cult-form` fact already held at their sites —
no willingness table, a lookup over facts and a derived lexicon state. The
**conduct fold** is the accumulation, per `DriveKind`, of every decision this
particular rider's arbitration wanted and did not pursue
(`Session::driven_overrides`) — a record the world was already computing and
discarding every tick before this campaign kept it.

## Consequences

- **The prior's sign inverts the intuitive reading, and the inversion is
  derived from the frontier essay, not chosen for effect.** A people with
  apparatus for a rider "knows what to do about it", which the essay reads as
  *less* cooperative, not more; a people with nothing to invoke has nothing to
  guard and is the most open. `docs/audits/the-reticence-report.md` publishes
  the full per-people table (9 organized/`God`/`Guarded`, 6
  folk/`Spirit`/`Wary`, 0 unsettled/`Wordless`/`Open`) rather than a summary,
  the same discipline 0259 states for a derived distribution. **`Open` is the
  MOST cooperative arm, not the least**: it is `ImprovisedName::Wordless`, an
  unsettled people with no word for a rider at all. So the zero means *every
  one of the fifteen peoples is settled enough to have a word for `god` or
  `spirit`* — it does not mean "no people holds a doctrine". No rider concept
  is registered in this world, so `ImprovisedName` **has no doctrine arm at
  all**; a doctrine-holding people, were one ever added, would sit *below*
  `Guarded`, never at `Open`.

  *(Text correction, final-fix wave — the substance of this decision is
  unchanged and nothing here is superseded. The clause above previously read
  "0 doctrine/`Open`", which attached the doctrine label to the most
  cooperative arm and so stated the campaign's central finding backwards. The
  count `0` was correct throughout; only the label was wrong.)*
- **The only hand-picked numbers in the campaign are three patience
  thresholds** (`stance::patience`: 2/4/8), and they are thresholds on a
  derived count, not an authored disposition — the quantity they gate is
  earned, only the step size is chosen.
- **What we give up:** no author may place a specific host's hostility for
  narrative reasons. Any such authorship must enter through the doctrine
  prior or the conduct fold, where it is subject to the same derivation
  discipline as everything else those two draw from.

## See also

Spec §3.2-3.5; `windows/vessel/src/doctrine.rs`, `windows/vessel/src/stance.rs`.

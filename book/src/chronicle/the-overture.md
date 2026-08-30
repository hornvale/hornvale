# The Overture

*An overture is not the delay before the opera. It is the first part of it.*

World generation cost **3,054 ms** and the terminal was not opened until it
returned, so a player saw nothing at all for the whole of it. That is the
premise, and the intent behind it is Nathan's: genesis will grow — plausibly to
a minute or more on slower hardware, and explicitly never to *Dwarf Fortress*
lengths. This campaign made the wait into the first part of the game rather than
the delay before it, and made it not recur.

Two of its three preregistered hypotheses came back the way the spec hoped.
The headline one did not.

## The measurement that shaped everything else

Before any code, the build was decomposed end to end — five runs, `--release`,
seed 42, agreeing within 4%:

```
  phase                      ms      share    projected at 60 s
  ------------------------  ------  -------   -----------------
  astronomy                    0.4    0.01%       0.01 s
  terrain (genesis)          202      6.6%        4    s
  settlements               1840     60.2%       36    s
  deep time                  181      5.9%        3.5  s
  WorldContext::build         830     27.2%      16    s
      demography report       480     15.7%       9.5  s
      terrain sculpt          199      6.5%       4    s
  possession                   13      0.4%
  ------------------------  ------  -------
  TOTAL                     3054     100%        60    s
```

Two items are **76% of the wait**. The rate is wildly uneven, and that single
fact settled the design's most visible question: a global progress bar would
spend 60% of its life inside one phase, crawling, reading as a hang precisely
when the build is healthiest. So there is no global percentage anywhere in this
work. The substrate names the phases, marks the ones that are done, and bars
only the phase you are in — and the bar it draws comes from the **previous
run's own duration** for that same phase, never from a model, because
determinism makes yesterday's measurement exact for a repeat and roughly right
for a new seed on the same hardware.

A first-ever run has no such record and therefore draws no bar. That is the
honest state, not a fallback, and it is the same rule the views obey.

## A frame with pluggable views

The deliverable is not a loading screen. The candidate designs — a checklist, a
prologue, a drawn world — looked like alternatives and were not: each is a
**view**, and the interesting artifact is the contract between them and the
frame that owns the chrome, the progress substrate and the `space` cycle.

Four views shipped, in the order the world can justify them:

| view | speaks from | what it shows |
| --- | --- | --- |
| `sky` | astronomy — **0.4 ms** | the night sky, complete at the shallowest rung |
| `atlas` | terrain — 202 ms | the world as drawn, settlements appearing as they are placed |
| `almanac` | throughout | component-composed facts, each component declaring its own rung |
| `tongue` | full — the whole build | one clause realized twice: in Common, and in the flagship settlement's own tongue |

`tongue` matters out of proportion to its size. The peoples phase is 60% of the
wait, and until this view existed nothing had anything to say during it. It is
the first caller outside `domains/language` and its own tests ever to reach
`realize_tongue`, and on seed 42 it produces a world genuinely speaking:

```
  flagship settlement: "Doaba"   species: "bugbear"
  common: "the Doaba are bugbears."
  tongue: "the Doaba Doodoo Dvo."
```

The object word is byte-identical to the lexicon's own generated root for
`bugbear-kind` — the language stack's whole chain, drawn for this species, read
back out at the one moment nobody was watching.

## H1: the null was the design

**Every view renders at every rung** was the hypothesis. Two of four do not,
and the preregistration had already named what to do about it: a view with
nothing honest to say **declares that rung and is skipped**, never rendered
blank.

`atlas` is silent until terrain exists — before it there is no raster to draw,
and an empty plate reads as a broken map rather than as "not yet". `tongue` is
silent until the build is complete, because no tongue exists to realize a clause
in. `sky` speaks from 0.4 ms because everything a night sky needs is committed
by then; `almanac` speaks throughout because its *components* each declare
their own rung, which is the same rule one level down.

That last distinction earned its keep immediately. A component answers two
independent questions — *is it too early to ask?* and *is this true of this
world?* — and conflating them is how a startup screen tells you a world has no
oceans when what has actually happened is that terrain has not run yet.

## H2: the cache came back null, and shipped anyway

**A cached start under 1 s**, expected ~870 ms. Measured:

```
  cached     ~1.2 – 1.4 s
  generated  ~3.6 – 3.7 s
```

The cache itself is not the problem: `load_if_valid` costs **~9 ms**. The whole
gap to the estimate is `Driver::start_from_world`'s own tail — terrain
re-derivation for the map index, and session start — which this cache does not
and should not touch.

It ships regardless, and the reason is written down so it can be re-examined
rather than assumed: that residual is **independent of genesis's own cost**, so
the saving grows as genesis grows toward the projected minute. If a later
campaign finds the tail scaling with world size too, this arithmetic weakens and
the cache should be re-priced.

## H3: the tripwire held

Validity is decided in three layers, cheapest refusal first — seed and pins (a
free string compare, no world loaded), the world's `derived_under` roster
diffed against the current stream labels, and an astronomy-prefix tripwire that
rebuilds 0.4 ms of world and compares the facts in order.

The spec flagged the third as **likely to fail**, and said so in advance
precisely so a null would not be read as the cache being unsafe. It did not
fail: mutating a moon-density physics constant that is not a stream label
changed the astronomy facts and the tripwire refused the stale cache.

Its limit is still real and is not oversold anywhere: a settlements-only formula
change is invisible to an astronomy prefix. Layers 1 and 2 are the protocol.
Layer 3 is a cheap extra chance.

## The bound, stated as a bound

The terminal now opens **before** genesis, and genesis runs on a worker thread
while the main thread owns the screen, polls input and redraws on a cadence.
Determinism is untouched in the strong sense rather than the permissive one:
the build still runs on one thread, in one order, called exactly once, exactly
as the CLI calls it.

One phase cannot follow it over. `WorldContext::build` — 27.2% of the wait —
holds a `Box<dyn PhenomenaSource>`, and that trait declares no `Send` bound, so
the value cannot cross a thread boundary at all. Adding one is a kernel change,
and this campaign's only sim-side change is a single additive observer callback
on rung boundaries that already existed as early returns.

So the screen holds its last frame for ~843 ms of 3,054 ms. The frame it holds
is correct rather than blank — `living` marked in progress, with a bar if a
previous run measured it — and against a blank terminal for the entire build,
the premise is delivered for **73% of the wait**.

The one cost worth quoting, because it was measured rather than assumed: the
observer hands *borrows*, and the client's observer has to leave the thread, so
it clones what a view needs. Across all four rungs that is **21.9 / 24.5 /
23.7 ms — 0.85% to 0.94%** of the build. A plain clone is the right answer and
an `Arc` is not needed; the numbers are recorded so a later campaign that grows
the ledger can tell whether that still holds.

## What was held, with its price

A row with a number is worth several without.

| held | measurement |
| --- | --- |
| a `chronicle` view replaying history | deep time completes at 2,224 ms — most expensive to feed, least wait left to spend it in; its moment is a **cached** start |
| caching `GeneratedTerrain` | 199 ms |
| caching the demography report | 480 ms — the largest single item nobody has looked at |
| a living-world view | 480 ms of invisible compute that completes **last** |
| music, literature | view slots, not work: when a generator exists it registers a view and the frame does not change |

The chronicle view is held for a reason worth naming on its own. The wait is
paced by the **build clock**; a world's history is paced by the **world clock**;
they are not the same instrument, and a view that confuses them either races
through millennia or stalls.

Nothing here let you play early, and that was measured rather than assumed:
possession resolves a target settlement and fails without one, and settlements
land at 67% of the wait.

# The Overture — the wait made worth having

**Branch:** `campaign/the-overture`, from `origin/main` @ `2f8faf243` ·
**Decision block:** 0357–0366 · **Drafted:** 2026-08-28 ·
**Status:** G3 package pending.

*An overture is not the delay before the opera. It is the first part of it.*

---

## 0. What this is

World generation takes **3.05 s today** and the screen is blank for all of it —
`main.rs` opens the terminal only after `Driver::start` returns. Nathan's
standing intent is that genesis will grow, plausibly to **a minute or more on
some hardware**, and explicitly never to *Dwarf Fortress* lengths.

This campaign makes that wait into the first part of the game rather than the
delay before it, and makes it not recur.

**The deliverable is a frame with pluggable views, not a screen.** That framing
is Nathan's, and it is the design's load-bearing idea: the alternatives that
looked like a choice — a checklist, a prologue, a drawn world — are *views*, and
the interesting artifact is the contract between them and the frame.

## 1. The cost model, measured end to end

Taken 2026-08-28 on the M1 Max at load 5–11 (`--release`, seed 42, five runs
agreeing within 4%). Probes are in the campaign scratch; **absolutes are upper
bounds, proportions are robust.**

```
  phase                      ms      share    projected at 60 s
  ------------------------  ------  -------   -----------------
  astronomy                    0.4    0.01%       0.01 s
  terrain (genesis)          202      6.6%        4    s
  settlements               1840     60.2%       36    s
  deep time                  181      5.9%        3.5  s
  ------------------------  ------  -------
  WorldContext::build         830     27.2%      16    s
      demography report       480     15.7%       9.5  s
      terrain sculpt          199      6.5%       4    s
      climate                  69      2.3%
      locale context           72      2.4%
      components              0.1      ~0%
  possession                   13      0.4%
  ------------------------  ------  -------
  TOTAL                     3054     100%        60    s
```

**Two items are 76% of the wait: settlements and the demography report.**

**The rate is wildly uneven, and that is a design input rather than trivia.** A
global progress bar would spend 60% of its life inside one phase. That single
fact is why §3's progress substrate names phases and bars only the phase you are
*in*.

## 2. The frame

The frame owns the chrome, the progress substrate and navigation. A view owns
the middle. `space` cycles; with no input the frame slideshows.

```
  hornvale - seed 42                                     [ view ]  < space >
  -------------------------------------------------------------------------
  |                                                                       |
  |                        the view's own region                          |
  |                                                                       |
  -------------------------------------------------------------------------
  the sky [x]  the land [x]  the peoples #####.....  deep time .  living .
                                             28,110 facts · ~1.1 s remaining
```

**The progress substrate is honest by construction.** Genesis appends facts, so
the counter needs no estimate. The per-phase bar is sized from **the previous
run's timings on disk**, not from a prediction — determinism makes that exact for
a repeat and roughly right for a new seed on the same hardware. A first-ever run
shows phase names and a fact count and no bar, which is the honest state.

## 3. The view contract

A view is handed the build state, the partial world, and a pacing hint. It must:

1. **Render honestly at ANY rung, including the first.**
2. **Show what EXISTS — never a placeholder for what does not yet.** This is
   `BuildDepth`'s prefix property doing work: earlier rungs are a byte-identical
   prefix of later ones, so a view at rung *n* is looking at a real world, not a
   half-built one.
3. **Fill the time it is given without implying a total it cannot know.**

**Two plugin levels**, per Nathan:

- **Screen level** — a view joins the `space` cycle.
- **Component level** — a view composes registered pieces, each declaring the
  rung it needs and skipped until that rung lands. "It has no oceans." "It is
  tidally locked." Each is one component with one precondition.

The component level is what lets a view grow without a redesign, and it is why
the contract is settled now: **when a music or literature generator exists, it
registers a view and the frame does not change.**

## 4. The four views, in the order the world can justify them

| view | first has data | draws |
|---|---|---|
| **sky** | **0.4 ms** | neighbours by RA/dec, moons, wanderers, the ecliptic; captions the eclipse ladder |
| **atlas** | 202 ms | the world as drawn, settlements and caves appearing as placed |
| **tongue** | 2,043 ms | one clause, realized in Common and in a tongue, glossed |
| **almanac** | grows throughout | prose from committed facts; the "what is strange" component |

**`sky` opens complete while everything else opens empty** — astronomy finishes
before anything else exists. **`tongue` covers the long tail**: the peoples phase
is 60% of the wait and nothing else has anything to say during it.

**Verified reachable, not assumed** (all four had a claim checked before scoping):
`Neighbor` carries `declination`/`right_ascension`
(`domains/astronomy/src/neighborhood.rs:14`); `NightSkyLines` already computes
the pole star, heliacal returns, wanderers, constellation figures, the two-year
eclipse ladder and the flagship sightline (`windows/almanac/src/lib.rs:104`);
`realize_common(&Clause, &CommonVocabulary)` and `realize_tongue(…)` both exist
(`domains/language/src/{clause,grammar}.rs`); the atlas's renderer shipped in
The Quadrat.

## 5. Opening the screen first

`main.rs` opens the terminal after `Driver::start` returns, deliberately: a
genesis failure prints to a clean shell rather than needing raw mode torn down.

Inverting that is the precondition for everything above, and **the campaign owes
the cost back**: a failing genesis must still leave the terminal sane. That is a
test, not a hope.

## 6. The cache, and how validity is decided

`hornvale new --out world.json` already writes a world. First visit generates and
caches; later visits load. **3,054 ms → ~870 ms**, and 60 s → ~17 s.

Validity is three layers, and **the middle one already exists**:

1. **Seed and pins** are data — mismatch, regenerate. Free.
2. **`derived_under` diffed against current labels.** `World` already carries a
   map of `label → version` written at save time by the composition root, and
   `cli::streams::what_moved` / `reload_notice` already diff it and **name the
   label that moved** rather than flagging staleness generically. An empty stamp
   makes no claim and must report *nothing moved* — that subtlety is already
   handled.
3. **A 0.4 ms astronomy tripwire.** Regenerate to `BuildDepth::Astronomy` and
   byte-compare the prefix. Catches an **undeclared** change that no label bump
   records. It does not prove validity — a settlements-only formula change would
   not show — and the spec says so rather than implying otherwise.

Decision 0189 already establishes that a world file written before a format flip
deliberately does not load. Refusing a stale cache is in keeping, not new.

## 7. What this does NOT do

- **It does not let you play early.** Possession resolves a target settlement and
  returns `VesselError::NoSettlement` without one (`session.rs:990`), and
  settlements land at 67% of the wait. "Play while it builds" was measured
  impossible and is not in this design.
- **It does not need resumability.** `build_to` always starts from
  `World::new(seed)` and has no resume entry point — but it already carries the
  rung boundaries as early returns, so an **observer callback** is sufficient.
  That is the one sim-side change and it is additive.
- **It does not touch the determinism contract.** No seed label, no stream order,
  no quantized value. The frame and views live in `clients/`; the observer is a
  callback on an existing function.
- **It does not cache `WorldContext`.** It holds `&World` plus derived structures
  and is not serialisable; the ~870 ms floor is that.

## 8. Held for later, with their measurements

A row with a number is worth several without.

| held | measurement | why not now |
|---|---|---|
| **chronicle** view (history replayed on the *world* clock) | deep time completes at 2,224 ms | most expensive; its value is highest on a **cached** start, where the history already exists and there is ~870 ms to fill |
| cache `GeneratedTerrain` | **199 ms** | needs its own serialisation answer |
| cache the demography report | **480 ms** | the largest single item nobody has looked at |
| **living world** view | 480 ms of invisible compute | completes **last**, so no time remains to show it — and caching it would remove it from startup entirely. A view for something we intend to make disappear is the wrong order. |
| music, literature | — | **view slots, not work**: when a generator exists it registers a view and the frame does not change |

## 9. What is unverified, and how each is settled

| claim | status | settled by |
|---|---|---|
| an observer callback at the rung boundaries is sufficient | **verified** — the boundaries are early returns in `build_to` | read at spec time |
| a view can render at any rung without placeholders | **hypothesis (H1)** | each view tested at every rung, including rung 0 |
| the cached path beats generation | **hypothesis (H2)** | measured: load + context vs. full build |
| the astronomy tripwire catches an undeclared change | **hypothesis (H3)** | mutate a genesis constant without bumping a label; the tripwire must redden |
| a failing genesis leaves the terminal sane after §5 | **unverified** | a test that forces a genesis error with the screen open |

## 10. Preregistered measurement

- **H1** — every view renders at every rung. **Null is a result:** if a view
  cannot say anything honest at an early rung, it declares that rung and is
  skipped in the cycle rather than showing a placeholder.
- **H2** — cached start under 1 s (expected ~870 ms). Null: the cache is not
  worth its complexity and the campaign says so.
- **H3** — the tripwire reddens on an undeclared genesis change. **This one has a
  real chance of failing**, because the astronomy prefix is small; if it does,
  the honest report is that layers 1 and 2 are the whole protocol and the
  tripwire is theatre.

## 11. Flagged for Nathan at G3

1. **The scope grew during brainstorming, twice, both times on Nathan's
   direction** — from one screen to a frame with views, then from three views to
   four. Recorded so it is not later read as drift.
2. **`clients/game` gains a dependency on the language domain** for the `tongue`
   view. `clients/` is outside the cargo workspace so decision 0004's allowlist
   does not bind, and `hornvale-language` is already in the graph via
   `hornvale-vessel` — but it is a new direct edge and deserves an explicit yes.
3. **H3 may fail**, and the design is sound without it. Flagged so a null is not
   read as the cache being unsafe.
4. **The observer callback is the only sim-side change.** Everything else is
   `clients/` and docs.

## 12. Decisions to promote (0357–0366)

- **0357** — the startup is a frame with pluggable views, not a screen.
- **0358** — a view shows what exists and never a placeholder for what does not.
- **0359** — progress is named by phase, never by a global percentage, because
  one phase is 60% of the whole.
- **0360** — an estimate comes from the previous run's timings, not from a model.
- **0361** — a cached world's validity is seed + pins + the `derived_under` diff,
  with a prefix tripwire for undeclared change.
- **0362** — the wait is paced by the build clock; a world's own history is paced
  by the world clock, and they are not the same instrument.

## 13. Task outline

1. Open the terminal before genesis; prove a failing genesis still restores it.
2. The observer callback on `build_to`'s existing rung boundaries.
3. The frame: chrome, progress substrate, `space` cycle, slideshow.
4. View: `sky` (first, because its data is complete at 0.4 ms).
5. View: `atlas`.
6. View: `almanac`, with the component registry and the "what is strange" piece.
7. View: `tongue`.
8. The cache: write, load, and the three-layer validity protocol.
9. Artifacts, book, chronicle, retrospective, decisions, registry.

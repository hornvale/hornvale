# Retrospective — The Lantern

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-lantern.md): a built cell has a
fabric derived from its ground, an interior has light derived from placed
sources through the shipped shadowcaster, the blackbody moved into the kernel
and became a band integral, and the chamber pane finally carries colour.

Nine tasks. Twenty-one ledger entries, of which six were structural findings
that changed the campaign's shape after the spec was written.

---

## The dominant lesson: every defect the campaign found in its own planning came from my plan or spec text

This is now the sixth consecutive campaign reporting it, and this one is the
cleanest instance because *no implementer error was found at all*. Four
authoring defects, all mine, all found before or during implementation rather
than by the gate:

| where | the defect | how it surfaced |
|---|---|---|
| plan | the remaining test batteries were placed in `windows/worldgen`, a crate that **cannot compile them** — worldgen is upstream of vessel, and the batteries needed vessel types | the implementer hit a compile error; the plan had asserted the location without checking the dependency direction |
| spec §3 | "cob is tinted by the soil's iron" — **there is no continuous iron axis** anywhere in the soil model to tint anything by | reached for at implementation time and found absent |
| spec §11 risk 5 | predicted the band-integral change would produce a **~34 % rendered shift**; the measured shift was **one `u8` step**, and three of five rows did not move at all | a spike, run because the risk was written down |
| spec §4.2 | listed the hearth as a placed light source "already in built interiors"; **`Cell` appears nowhere in `windows/vessel/src/interior/`**, so the hearth had no lattice position to be a light at | planning grep at Task 5 |

The last one is the most instructive, because the spec sentence was *true* —
the hearth is placed, in the anchor graph — and false about the model the claim
needed to hold in. The interior model is topological; a light must be
positioned in the spatial one; nothing joined them. That is the
right-measurement-wrong-attribution shape, arriving in a spec rather than in a
measurement.

## Three framing errors in one session, all one shape, and the third was structural

Recorded here because the count is the point. Within one design session I
reached for a shipped, plausible-sounding thing and asserted it answered the
question asked, three times:

1. `snow_fraction` as ground cover — it is precipitation.
2. The chlorophyll red edge as a colour signal — real, and located where human
   cones are blind.
3. The underground band as this campaign's render target — it exists, and it
   *folds into the surface chart*, so a light field would have been computed and
   displayed nowhere.

The measurements were right every time; the **attributions** were wrong. The
third one was load-bearing: it forced the campaign to be re-cut, because it
revealed that materials without light and light without materials each render
nothing, and I had written exactly that sentence in my own option preview and
split them anyway.

**The countermeasure that worked** was the graph organon's own test — *if your
graph splits into pieces, the idea is two ideas under one name*. Applied to the
re-cut it did not split: `sense()` is a cut vertex with two required inputs. The
re-cut was originally decided on judgment and later confirmed by structure,
which is the order that should be reversed next time.

## An empty diff is not evidence, and this campaign mutation-proved it

The single most transferable finding. Deciding whether the band-integral change
was safe, the obvious check was `make rebaseline` followed by
`git diff --exit-code`. It came back empty. It also came back **empty under a
gross mutation** — halving one band of the illuminant outright:

| run | result |
|---|---|
| full suite, band integral | pass |
| `make rebaseline` + `git diff` | **empty** |
| `make rebaseline` + `git diff`, gross mutation | **empty** ← the check was vacuous |
| full suite, gross mutation | 1 fail: `the_client_fixtures_are_current` |

`make rebaseline` regenerates nothing that carries a daylight-derived colour, so
the artifact diff had no opinion whatsoever about the change. The live guard is
a **test fixture** — `windows/vessel/tests/fixtures/session-seed-42.json`, 124
colour keys — not a rebaseline artifact.

The generalization: **mutation-prove the evidence command, not only the test.**
"I ran X and it was clean" is worth nothing until X has been shown to be dirty
under a change that ought to dirty it. This was one step away from shipping a
byte-identity claim backed by a check that could not see the bytes.

It also produced a good outcome: with the real guard identified, the band
integral was proven byte-neutral in *both* directions, and a decision I had told
the owner was "epoch-class and breaks byte-identity" turned out to cost nothing.
I was wrong at a gate, and a spike is what corrected it — the kernel's own doc
had already said which way to go (`BAND_CENTERS_NM`: "anything integrating over
a band wants the **edges**") and I had not read it closely enough.

## Two implementer reports had correct measurements and wrong mechanisms

Both were caught by re-deriving the *because* rather than by doubting the
number:

- Task 2 reported that the band integral's large relative errors were cancelled
  by **peak normalization**. The figures were already post-normalization, so
  dividing by the peak band cannot be the cancelling step. The real reason is
  that the large relative errors live entirely in the **dimmest bands**, where
  they have almost nothing to be a fraction of.
- A threshold doc attributed the live guard to the artifact sweep rather than to
  the fixture test — the same confusion the mutation above had already resolved,
  re-entering through a different door.

Every measurement in both reports survived scrutiny. Neither mechanism did. The
standing rule holds: read a subagent's *because*, not just its number.

## The countermeasures that fired on their own

Two, and both are worth keeping because they were caught by implementers rather
than by review:

- **Task 5's first mutation left the suite fully green.** The implementer
  noticed, and strengthened the guard rather than reporting the mutation as
  performed.
- **Task 8's first draft of its own artifact guard was vacuous**, and its author
  said so.

Naming the defect class forward in the dispatch — "a guard that cannot fail" —
continues to convert reviewer-caught findings into implementer-caught ones.

## Preregistering a *reading* rather than a claim

H4a was written into the spec as **reported, not predicted**: "how dark does a
chamber cell actually get?", with an explicit note that it might report H4's
regime unreachable, and that the attenuation constant may not be tuned to make
it come out otherwise.

It read negative, and the negative is the campaign's most useful structural
finding — the chamber band cannot present darkness to a possession, because
symmetric shadowcasting plus an implicit torch makes the lit set equal the FOV
set by construction. Because the reading had been framed as a reading, that was
publishable as a finding on the spot instead of looking like a failure to be
argued around.

Two mechanics made it stick:

- The constant it rides on was **documented as un-tunable in its own doc
  comment**, before the reading was taken, with the reason. An earlier spec
  draft had called it cosmetic and was wrong.
- The result is pinned as an **inverted tripwire** — the test asserts *zero*
  achromatic cells — so a future red is a finding to read, never a value to
  relax.

## H1 held on the population and a player samples its head

H1 measured 1505 settlements over eight seeds: median pair 41 `u8` steps, max
102. Comfortably held. But `p10 = 1`, and a later sweep found all four sampled
**flagship** settlements standing on alluvium — and a possession starts at a
flagship.

Both readings are true. The chronicle nearly claimed "walls vary visibly" on the
strength of the population number alone, which would have been the
measure-the-population-you-apply-to error: the metric's population and the
player's sample are different sets. Caught before the chronicle was written, and
it went on to constrain the lens — a transform whose slope drops below 1
anywhere would erase that decile while the median went on looking fine.

## The lens was built last, and that ordering was the point

Building the presentation filter as the final task, with an explicit standing
instruction that every preregistered claim reads unlensed colour, is what made
H1 falsifiable at all. A saturation boost applied earlier would have hidden
exactly the failure H1 exists to detect, and afterwards no one could have told
whether the room looked right because the model worked or because the filter was
doing the work.

Worth generalizing: **when a campaign has both an honest path and a pretty one,
the pretty one is a separate task gated behind the honest one's measurements**,
not a parameter of it.

## Process notes

- **The capture gap.** The ledger promised registry rows at three separate
  entries and none were written; a `git diff` against the registry was empty and
  there was no follow-ups file. Caught mid-campaign and fixed in one commit, but
  the promise-to-capture lag was several tasks long. A ledger entry saying
  "capture: registry row" is a *debt*, and nothing tracks it.
- **A stale shell cwd verified nothing.** The first run of the docs-consistency
  check after that fix ran in a *different campaign's worktree* and reported
  green about the wrong tree. Re-run in the correct worktree, it was still green
  — but it had proven nothing the first time.
- **The absorption cadence held.** Main moved only twice during the close
  (docs-only spec commits), and re-running every readout after absorbing it cost
  minutes and confirmed all four numbers unmoved.

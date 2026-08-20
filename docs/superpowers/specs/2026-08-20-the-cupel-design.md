# The Cupel — design spec

**Myth thread, campaign 7.** Settling arm for
`KNOW-derived-vs-constant-penalty`. Autopilot engaged. Branched from main
`74aac3f8` (substrate byte-identical to the kickoff's `eb64dced`).

> A cupel is the assayer's porous vessel: molten under a blast, it drinks the
> lead away and leaves a bead of the true metal, so the assayer can weigh what
> was really precious against what was only filler. This campaign is that assay,
> run on the crossing penalty's derivation.

## 1. The question

The myth thread's crossing penalty (`windows/hearsay/src/derive.rs::crossing_penalty`,
arm `Crossing::ContactWeighted`) is

```text
span(FINEST) / (1 + edges_between(a, b))
```

— a story crossing a people boundary garbles by one finest rung, **discounted
by how many raids the two peoples have on record between them.** The magnitude
is *derived from world history*, and that derivation is the entire licence the
penalty runs on under **decision 0021**: the engine holds no authored
prejudice; a bias between two peoples must be a fact read from the world, never
a constant an author picked.

**Nobody has tested whether the derived magnitude does anything a well-chosen
constant would not.** If a flat constant at the same average garble level
produces the same *beliefs*, the derivation is behaviourally decorative and the
0021 licence protects a distinction with no difference. That is a real,
publishable finding, and a **null is a legitimate headline** (decision 0016);
nothing here is retuned to rescue a prediction.

The Undertow (campaign 5) left this explicitly open — "there is no arm anywhere
in this campaign separating a derived magnitude from a constant one" — and noted
the danger that motivates the control below: the tercile ordering came out
*inverted*, and an inverted ordering is **equally consistent with a constant**,
because crossings concentrate on a handful of people-pairs. The Touchstone
(campaign 6) then built the precondition this campaign needs: a per-holder
belief-delta instrument that can *see* the penalty where the divergence
aggregate cannot.

## 2. The experiment — a dose-matched placebo trial

Three transmission arms, identical in every respect except the per-crossing
penalty magnitude, each walked by the Touchstone's traced walk and diffed by
its belief-delta instrument:

```text
  arm       penalty on a cross-people step        trial role
  -----     ------------------------------        ---------------------
  Free      0                                     no treatment
  Constant  unit / D        (D a constant)        placebo at matched dose
  Derived   unit / (1 + edges_between(a, b))      active drug at matched dose

  unit = span(FINEST) of the originating witness's people's ladder
         (identical in all three arms; a same-people step pays 0 in all three)
```

`D` is chosen so the **mean penalty over the derived arm's realized crossings
matches** — so Constant delivers the same average garble *level* as Derived, and
any belief difference between them is attributable to the penalty *varying by
people-pair* (the derived part, the `1/(1+edges)` shape), not to the average
level. The exact estimator for `D` is **frozen in §4 from the Task 0 substrate
probe**, not from reasoning.

**Three arm-comparisons, one headline comparison** (and NOT an additive
decomposition — a holder moved by both level and shape is counted in both, so
`tail(Derived,Free) ≠ tail(Constant,Free) + tail(Derived,Constant)` on counts;
the three are reported separately):

- **Derived vs Constant → the derivation's marginal effect. THE HEADLINE
  COMPARISON.** Does the derivation's *shape* buy any belief difference *beyond*
  its level?
- Constant vs Free → the level effect exists at all — an anchor that this axis
  moves the instrument, so a small headline is a real null and not a dead
  instrument.
- Derived vs Free → the total penalty effect, for context.

Within that headline comparison, belief is read on **two co-primary channels**
(next paragraph): the remembered day and the winning account.

**Endpoints (two co-primaries, set by §3's substrate finding):** the
**remembered day** (`day_changed`) — what a holder believes — and, because the
Task 0 probe found the day channel is quantization-bounded on the real ladders,
the **winning account** (`route_changed`) over the small discriminating
population, the one belief channel the ladder does *not* quantize. `rung_changed`
/ `hops_changed` are secondary. `width_changed` is **reported but not a
headline**: any off-`D` crossing changes the internal accumulator near-trivially,
so a headline on width would inflate the finding. Both primaries are stated over
explicitly named denominators (§4).

## 3. Substrate findings (Task 0 probe) — FROZEN

*Measured by `windows/hearsay/tests/cupel_substrate_probe.rs` on this tree
(main `74aac3f8`), before §4. The probe is a pure read over the shipped derived
arm under `(WithRaidSeam, ContactWeighted, Multiplicative)`; it builds no
instrument and needs no new library code. 12-seed panel, none skipped, 1,084
winning-route crossings, 1,046 cross-people holders.*

- **P1 — edge-count distribution over winning-route crossings.** **8 distinct
  edge counts** (1,2,3,4,5,6,10,11), modal **31.09%** at edges=3 — *less*
  concentrated than the Undertow's 41.6%. Discounts `1/(1+edges)` span
  **[0.0833, 0.5000]**. **VIABILITY: passed — the derived magnitude genuinely
  varies across real crossings.**
- **P2 — discriminating population (power).** On the least-damage WINNING route,
  **1,008 of 1,046** cross-people holders cross exactly **once**; **38** cross
  twice; **none** more; only **15** cross at ≥ 2 *distinct* edge counts. (The
  least-damage selection avoids crossings, so the winning route rarely crosses
  twice — a different population from the Undertow's "≈55% cross > once", which
  counted all traversals, not the winning route. Stated as observed; nothing is
  built on the reconciliation.)
- **P3 — the matched-mean constant.** Panel-wide per-crossing mean
  `1/(1+edges)` = **0.208852** ⇒ `D` = **4.7881**; dispersion sd = **0.095459**
  (≈ 46% of the mean — wide, so the derivation has real room to differ from a
  constant *per crossing*).
- **P4 — `unit` variation.** `span(FINEST)` is **constant within each world**
  (all 15 peoples share it — it is the world's shortest astronomical period),
  and ranges **[0.765, 6.072] days across worlds**. So panel-wide vs per-world
  `D` is a real distinction, and the constant-*denominator* control is clean:
  `unit` cancels within a world.
- **P5 — day-channel upper bound (two-sided, nearest rung boundary).** Only
  **≤ 4.685%** (49/1,046, nearest boundary within `n·unit`) — tighter
  **≤ 2.964%** (31/1,046, within `n·unit/2`, since a derived penalty is
  ≤ `unit/2`) — of cross-people holders sit near enough a rung boundary that
  *any* sub-`unit` penalty change could move their remembered day. **The
  precision ladder is far coarser than the penalty, so the derivation's
  variation is quantized away before it reaches a belief.** (An earlier
  one-sided draft checked only the upward boundary and reported 0.11%; the
  two-sided figure is the honest one.)

**What the probe establishes.** The finding is not that `edges` fails to vary —
it varies a lot. It is that the belief ladder is too coarse to *express* that
variation: the derivation's entire realized effect lives in the sub-rung
**width** (which no belief reads), and its reach into the **remembered day** is
bounded at ≤ ~5% of cross-people holders by ladder geometry alone. The one
belief channel the ladder does **not** quantize is **route identity** (which
account wins), and that is where the derivation's effect could genuinely
survive — over at most the 15–38 multi-crossing holders. This reshapes §4.

## 4. Preregistration — FROZEN

*The hypotheses and numeric criteria, frozen here before any readout code, on
the substrate §3 measured (decision 0016). `D` and the arms are re-derived from
the tree the readout runs on; nothing is retuned to move a tail.*

- **`D` estimator. Primary: a single panel-wide `D` = 4.7881** — the value a
  hypothetical author would pick, which is exactly what 0021 forbids ("a
  constant an author picked"). Computed as
  `1 / mean_over_winning-route-crossings[1/(1+edges)]`. **Robustness arm:
  per-world matched-mean `D_world`** (removes residual per-world level mismatch;
  `unit` cancels in-world per P4). Re-derived by the readout on its own tree;
  4.7881 is this tree's value and moves if the substrate moves.
- **Denominator (named).** Cross-people holders reached under **both** arms
  whose winning route crosses ≥ 1 people boundary. Same-people holders are
  provably identical (§5 negative) and are excluded from the primary rather than
  padding the denominator toward zero.

**Primary endpoint — the remembered day.** The derived-vs-constant `day` tail
over that denominator. The probe bounds it small by ladder geometry
(P5: **≤ 4.685%**), so this channel is *confirmatory*: the campaign's job is to
measure the **exact** value with the real constant arm (including route-shift
effects P5 cannot see) and to prove the smallness is a **substrate** fact, not
instrument blindness (§5 fine-ladder control). Interpretation bands, stated
before the number and explicitly not as suspense:

```text
  day tail (derived vs constant, over cross-people holders, both arms)
  --------------------------------------------------------------------
  <= 5%   a matched constant reproduces the remembered day for >= 95%
          of cross-people holders: DECORATIVE on the day channel — on
          this substrate the 0021 licence protects a distinction the
          belief ladder cannot render.  (Expected, and P5-bounded.)
  > 5%    the derivation moves more remembered days than ladder
          geometry alone predicts (P5) — a real surprise requiring the
          route/width decomposition to attribute.
```

**Route co-primary — the winning account.** The derived-vs-constant `route`
tail over the **15 discriminating** (≥ 2 distinct edge counts) and **38
multi-crossing** holders — the derivation's only belief effect the ladder does
NOT quantize, and the genuinely open question. Reported as an exact count,
examined holder-by-holder; `> 0` means the derivation changes *which account*
some holder holds even where it cannot change the remembered day. No floor is
frozen (the population is enumerated, not sampled); the finding is the count and
its mechanism.

**Mechanism (secondary, reported not headlined).** The `width` tail — expected
large — is the disclosure that the derivation *does* compute something; it is
explicitly not a belief. The finding names *which channel carries the
difference*: width (large, unbelieved), day (≤ 5%, quantized), route (the
15–38-holder redoubt).

A falsified prediction is the finding; nothing is retuned to rescue it.

## 5. Controls (Touchstone discipline — mutation-proven, not asserted)

- **Negative (provable zero):** on the people-homogeneous-ancestry
  sub-population every step is within one people, so `crossing_penalty`'s
  `from == to` guard returns 0 under *any* magnitude — Derived and Constant are
  bit-identical there. `negative_tail == 0` by construction. Reuses the
  Touchstone's theorem and its mutation proof (delete the `from == to` guard →
  the negative control reddens). The controller re-fires the mutation
  personally (a negative control is the one place a vacuous green is invisible).
- **Day-channel liveness / mechanism isolation (the key control, forced by
  §3).** On the real panel a sub-`unit` penalty change barely moves the day
  channel (P5), so Derived-vs-Free would NOT light the day channel either — a
  panel positive control cannot prove the instrument's day channel is alive. So
  the control is a **hand-built FINE-ladder fixture** (four-moon style, reusing
  `two_peoples_with_raid_count`'s engineered rungs, milliseconds) where Derived
  and Constant land the accumulated width on **different rungs**, so
  `day_changed` fires. This proves (a) the instrument's day channel discriminates
  the two magnitudes when the ladder can express the difference, and (b) the
  panel's day-null is **ladder coarseness, not instrument blindness**.
  Mutation-proven: set `D = 1 + e` (constant equals derived) → they agree → the
  test reddens.
- **Instrument-fires-at-all (width anchor):** Derived vs Free on the panel →
  `width_changed` for ~every cross-people holder, proving the instrument sees the
  penalty. This anchors sensitivity on the width channel (where the effect is
  large); the day channel is anchored by the fine-ladder control above.
- **Anti-vacuity:** the discriminating denominator is asserted `> 0`; a readout
  over an empty population would report a meaningless zero.

## 6. Implementation approach

- **Task 0** — the substrate probe (done first; grounds §3/§4).
- **Task 1** — a penalty-parameterized traced walk in
  `windows/hearsay/src/traced.rs` (the measurement sibling): add
  `PenaltyModel { Free, Derived, ConstantDenominator(f64) }` and a
  `traced_variants_with_penalty(..)` taking it; `crossing_info` reads the model
  instead of `walk.policy.crossing`. The existing
  `traced_variants_about_accumulating` becomes a thin wrapper mapping
  `Crossing::Free → Free`, `Crossing::ContactWeighted → Derived`, so
  `tests/traced_walk.rs`'s agreement battery still pins Derived == shipped. The
  **shipped `Crossing` enum and `derive.rs` are untouched** — the penalty feeds
  the width-first ordering key, so the constant arm must be *re-walked*, not
  post-processed (multiplicative accumulation propagates an early penalty
  difference through every later step), and adding an `f64` to the pinned,
  save-format-adjacent `Crossing` enum would break its `Ord`/`Eq` derives and
  move the `Transmission` label.
- **Task 2** — the **fine-ladder day-channel control**
  (`windows/hearsay/tests/cupel_readout.rs` or a sibling), a fast hand-built
  fixture proving the instrument's day channel discriminates Derived from
  Constant when the ladder can express the difference (§5), mutation-proven
  (`D = 1 + e` → agree → red). This is the mechanism-isolation control and is
  cheap; it is built before the heavy readout so the readout's day-null has a
  live-instrument witness beside it.
- **Task 3** — the heavy readout battery (`cupel_readout.rs`), `#[ignore]`d
  (NOT a census lab metric — registering one runs on ~2000 census worlds
  forever; four campaigns learned this). It re-derives `D` (both panel-wide and
  per-world estimators) from the derived arm on its own tree, runs the three
  arms (Free / Constant / Derived) through the belief-delta instrument over the
  panel, and reports: the day tail (primary, expected ≤ 5%), the route tail over
  the 15 discriminating + 38 multi-crossing holders (co-primary, exact count,
  examined individually), and the width tail (mechanism). It asserts the frozen
  §4 bands and the §5 negative control (`negative_tail == 0`, mutation-proven by
  the controller personally) and anti-vacuity (discriminating denominator > 0).

## 7. Non-goals

- **Not** a change to the shipped model — no arm here ships; `Crossing` gains no
  variant. The Constant arm is a measurement control, not a candidate model.
- **Not** the question of a per-people-pair *authored lookup* penalty (varies by
  pair identity, ignores edges) — a distinct 0021 question, captured as a
  registry row.
- **Not** a penalty derived from a *different* world fact (niche overlap,
  population) — captured as a registry row.
- **Not** a census metric, and **not** the Palimpsest unit-erratum freeze
  (untouched).

## 8. Decisions & risks

- **Substrate-collision risk — `campaign/the-ell`** (a live unmerged epoch on
  another host retyping `Fact.day` and making the history bake emit days) moves
  `domains/history` + `windows/worldgen`, which is this campaign's substrate. If
  it merges mid-campaign every frozen number goes stale (the Undertow
  collision). Mitigation: board notice posted; never absorb mid-measurement;
  finish the readout, then absorb once and re-derive at close.
- Ledgered decisions #2–#5 (constant form, headline channel, library shape) are
  promoted here from `.superpowers/sdd/decision-ledger.md`.
- Deliver as a heavy battery with re-derivable asserted numbers (Touchstone
  precedent), not a census metric.

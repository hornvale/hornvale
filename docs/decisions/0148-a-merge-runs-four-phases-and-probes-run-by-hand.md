# 0148. A merge runs four phases, and a probe runs by hand

**Status:** Accepted (2026-08-19) · **Decider:** Nathan · **Amends:**
[0139](0139-main-advances-only-through-the-lock.md)'s phase list ·
**Relates:** [0132](0132-three-gates-named-for-the-campaign-moment.md),
[0133](0133-nontrivial-checks-run-in-one-serial-lane.md)

In the context of a merge costing about an hour of the one serial box, of which
four fifths went to two sets whose guarantees move at campaign cadence rather
than per-merge, we decided that **the merge queue runs `artifacts outboard gate
clients` and nothing else**, and that three exploratory batteries move to the
existing `probe:` tier so no gate runs them at all.

## The measurement

Mean phase wall time across today's chamber runs on the canonical box:

```text
heavy        1965.0 s   (21 runs)      gate         342.6 s   (32 runs)
seam-guard   1017.5 s   (21 runs)      clients      248.6 s   (26 runs)
                                       artifacts     99.4 s   (34 runs)
                                       outboard      30.6 s   (34 runs)
```

Six phases ≈ 3704 s (62 min). Four ≈ 721 s (12 min). `seam-guard` and `heavy`
are **80.5% of a merge's wall time**.

## The ruling

1. **`merge_phases` is `artifacts outboard gate clients`.** A merge and a stage
   gate now run the same phases and differ only in the push — which was always
   the design ("the stage gate is this script with the push turned off") and is
   now true of the phase list too.
2. **`seam-guard` and `heavy` keep their `campaign`-rung rows and their entry
   points** (`make seam-guard`, `make heavy-remote REF=<full-sha>`). Those are
   now the only things that run them.
3. **Three batteries move `heavy:` → `probe:`**: `the_fare_calibration.rs` (4),
   `repose_exposure.rs` (8), `probe_contact_substrate.rs` (6). They are
   exploratory readouts; running them on a cadence nobody reads was paying for
   an answer nobody had asked for.

## What this costs, stated rather than implied

0139's guarantee is unchanged in KIND and weaker in DEGREE. Every commit on
`origin/main` is still the tip of a tree gated **as itself** — the merge product
is still built and still tested before it is pushed. It is now gated by four
phases rather than six, and **nothing runs `seam-guard` or `heavy`
automatically any more**. A seam that loses its last assertion, or a heavy-tier
regression, will now be found by whoever next runs those commands rather than by
the merge that introduced it. That is the trade, and it is worth naming because
a reader six months out would otherwise reconstruct it from a phase list and
assume the old cover still held.

## `probe:` was already a convention, and this ratifies it

**It was not invented here — it was found here, after an attempt to invent it.**
Eight distinct `probe:` reason strings already existed, all sitting in
`heavy_tier.rs`'s `EXPECTED_UNTOKENISED` roster as untokenised one-offs. The
first draft of this change added a canonical `PROBE_CANONICAL` string and forced
every probe to use it verbatim, mirroring `heavy:`. That was wrong and the
existing practice was right: heavy tests are homogeneous, so one canonical
string loses nothing; probes are heterogeneous, and their reasons carry what the
probe does and what it costs (`"probe: builds all 64 the-ford-probe worlds at
the canonical grid (17.5 s measured, --release); run by hand"`). A canonical
string would have deleted that information in the name of consistency.

So `probe:` becomes a token class — filtered out of the untokenised roster,
which drops from 37 entries to 29 — with **no canonical string**. The
convention is the shape: say what it does, say the measured cost if you know
it, say `run by hand`.

## The scatter-sweep pin spans both tiers

`internally_parallel_heavy_tests()` derives `.config/nextest.toml`'s
scatter-sweep class by finding tests that parallelise their own sweep across
every core. It keyed on `heavy:`, so moving `the_fares_*` to `probe:` would have
dropped two of its three batteries — the pin would still have named them, and
nothing would have said so. It now spans `heavy:` and `probe:` alike, because
the property it selects belongs to the test, not to the set that invokes it: a
probe run by hand saturates forty cores exactly as a heavy one did.

# CLAUDE.md — working in `windows/lab/`

The lab is Hornvale's measurement instrument: it runs **studies** (data) that
compute **metrics** (code) over generated worlds, producing the censuses the
book publishes and the calibration evidence. Read the root `CLAUDE.md`
"Process" section first.

## Studies are data, metrics are code (decision 0011)

- A **study** is a JSON file in `studies/` (seeds × pins × which metrics to
  read). It carries no logic. Adding or changing a study is a data edit.
- A **metric** is a Rust `Metric { name, doc, summary, extract }` entry in the
  `registry()` in `metrics.rs`. There are ~150+ of them. The registry is
  drift-checked — a metric's name/doc/output is a published contract.
- Studies **preregister their hypotheses** (decision 0016); the
  `preregistration_guard` test enforces that a study can't be quietly edited
  to match a result. Don't disable an ignored study with a result-quieting
  reason ("flaky", "TODO") — the guard rejects exactly that.

## nextest runs each test in its own PROCESS

This is the single most surprising fact for optimizing lab tests. `cargo
nextest` (decision 0040, the gate runner) isolates each test in a separate
process, so **a global cache / `OnceLock` fixture never persists across
tests** — every test that builds a world builds it from scratch. You cannot
speed the suite by memoizing world construction across tests. The levers that
*do* exist: build to the shallowest sufficient `BuildDepth` (see
`windows/worldgen/CLAUDE.md`), and the world-gen speedups in the kernel.

## `timings.rs` measures the BUILD, not the world (decision 0088)

The Laboratory is the measurement window, so the suite's own clock lives here:
`timings.rs` parses nextest's `libtest-json-plus` durations, folds/hysteresizes
the committed per-host baseline, and computes both alarms. `cli/` is bin-only,
so its tests cannot host shared code — that is why this module is in `lab` and
not next to `cli/tests/timings_alarm.rs`, which is only the failing-test
surface. Two consequences when editing it:

- **The wall-clock ban does not reach it, and the tags say so.** Durations are
  tagged `bare-ok(diagnostic-value)` — measurements *of* the implementation,
  never of the world. Do not copy that class into anything a world can see. A
  `pub fn` here returning `String` or `Result<_, String>` needs
  `bare-ok(prose: return)`; four consecutive tasks forgot it during execution,
  so assume the tag rather than the vigilance.
- **The pure functions are the contract.** `fold_below_floor`,
  `apply_hysteresis`, `suite_shift`, `per_test_shifts` and the enforcement
  polarity are unit-tested directly, not only through the stage gate's full
  run. The contention gate was shipped *inverted* once and passed
  spec-compliance review; the fix was to make its polarity a pure function
  with a test that fails on re-inversion. Keep new decisions in that shape.

## Censuses regenerate on the canonical box, not on AWS (0063, supersedes 0046; host enforced by 0079)

> **"Locally" here means *not AWS*, not "on your machine".** 0063 retired the
> spot box; 0079 then enforced *which* box, because the machines disagree by
> one unit on ~0.1% of discrete-count metrics. `census-run.sh` fails closed on
> the hostname, so from anywhere but the canonical box the run is refused —
> dispatch it there instead (root `CLAUDE.md` carries the recipe). Measured
> 2026-08-09: 776–921 s, not the ~7 min below.

- The everyday gate still stays fast by skipping censuses: `regenerate-
  artifacts.sh` runs them only under `HV_CENSUS=1` (a plain `make rebaseline`
  skips them). But since [The Local Census](../../book/src/chronicle/the-local-census.md)
  the census is cheap — the all-metric per-world cost fell ~285 → ~8 CPU-s
  (the metric + genesis-naming paths stopped re-sculpting terrain) — so the
  full ~2000-world census regenerates **locally in ~7 min** on the 40-core box.
- The sanctioned refresh is therefore **`scripts/census-run.sh`**, run once
  per campaign at the pre-merge close, keeping the census fixtures
  (`book/src/laboratory/generated/*/rows.csv`) **current with main** — not
  lagging. Use the wrapper, not `HV_CENSUS=1 bash
  scripts/regenerate-artifacts.sh`: since decision
  [0081](../../docs/decisions/0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md)
  every entry point serializes against other heavy runs on the box, but only
  `census-run.sh` also records the run in `docs/timings.md`.
  `scripts/census-run.sh status` says whether one is running. `make regen-remote` (AWS) is retired to
  abandoned — this box is the single canonical platform (decision 0063; AWS
  differs on ~0.1% of discrete-count metrics, so it can't be a parallel ref).
- Calibration loads the drift-checked fixture, not a live recompute (decision
  0032).

## Registering a metric is not a local act

Adding one `Metric` to `registry()` has three consequences, in increasing
order of what they cost you.

**1. It reddens every census-reading calibration test** until the fixtures are
refreshed on the canonical host at the campaign's close. They all carry the
identical panic — `rows.csv header does not match study '<X>' schema`
(`runner.rs`) — so a failure of a *different* shape is a real finding, not
this. Two things make it bite:

- **The fixtures are per-study, and more than one study is involved.** A
  refresh that covers only `the-census` can leave a binary red on
  `census-of-the-meeting`. Re-derive which binary reads which study with a
  grep over `windows/lab/tests/suite/*calibration*.rs` (test-binary
  consolidation moved every top-level `tests/*.rs` file one directory
  deeper) — **do not trust a mapping written in a doc, including this one.**
  The mapping recorded when this was first hit had `gathering_calibration` on
  `census-of-the-meeting`; today that binary reads `the-census` and the other
  two read both. It moved without anyone noticing, because nothing checks
  prose.
- **Establish the exact count at the campaign's FIRST task** and carry it into
  every review brief. Reviewers otherwise read a wall of reds as their own
  breakage. Do not carry the *number* forward between campaigns either: it was
  34 when first measured (2026-08-02) and the three binaries hold 46 `#[test]`s
  now.

**2. Nine studies declare `"metrics": "all"`, and there is no way to opt out.**
`study.rs` resolves `MetricSelection::All(_) => Ok(reg)` — the entire registry,
unfiltered — and `Metric` carries `name`/`doc`/`summary`/`domain`/`role`/
`extract` and **no cost or opt-in flag**. So a metric you add for one study
runs on every world of `the-census` (~2000) forever. This is the expensive
half, and the one people miss: the fixture churn above is annoying and
temporary, a slow metric is permanent. The Mire's three candidates each needed
a ~3.5 s per-world computation, which would have added roughly two hours to
every census refresh; they were not registered.

**2b. The blast radius is wider than "nine studies", and the widest part has no
drift check.** Registering a metric changes the schema of every `"all"` study,
which means it restages **any fixture any campaign has ever frozen against the
registry** — not just the census goldens. The Hearsay added exactly one metric
and staled The Gnomon's injection fixtures under
`windows/lab/tests/fixtures/injection/`, which are **deliberately absent from
`docs/generated-paths.txt`** (see that directory's README and
`scripts/gnomon-injection.sh`'s header) and therefore covered by no drift check
and untouched by `make rebaseline`. Nothing caught it except their reader test
going red in a full stage-gate run — after the census refresh had already run.

So the check before you register: `grep -rl 'rows.csv\|schema.json'
windows/lab/tests/fixtures/` and ask which of those have their own host-pinned
authoring path. Each one is a second refresh you owe, on the canonical box, in
the same sitting as the census — and each has a different script.

**3. Census cost lives in SWEEPING the network, not building or querying it.**
Measured across The Rill/The Millrace, on lefford: 93% of an 11.2× cost blowup
was one function (`lab_band_transects`) computed **three times**, by three
metrics reading three fields of one identical sweep. The fix was a private
per-world `OnceCell<Option<…>>` on `TerrainView` — 3.01×, with peak RSS
*falling* 2.2%, so no space/time trade. (`OnceCell` not `OnceLock`
deliberately: `!Sync` stops a filled view crossing worker threads. The
`Option` is inside so that "swept, found no channels" is a *filled* cell.)
Three measured nulls, recorded so a future pass doesn't spend its first day
rediscovering them: building the 16× network costs nothing (7.94 → 7.88
CPU-s/world), the per-room `rill_reading` path is a non-event (0.42 → 0.45),
and census RSS tracks **concurrency** at ~260 MB per concurrent world — so a
large figure on a 40-core box is workers, not a leak.

If you are adding a metric that reads a swept structure, check whether an
existing metric already sweeps it before adding a second sweep.

## `metrics.rs` is large and splittable

~5.4k lines. Clean seams: `views.rs` (the `WorldView`→`FullView` build-rung
chain), `phonotactics.rs` (the validator), `homophony.rs`, and the `registry()`
broken by its existing `// ---` family headers (Ground/Words/Branches/Tone/
BIO). The per-species metric literals are copy-paste (only a species string
changes) — a `per_species_metric` helper would collapse dozens. Worth doing;
same merge-hot caveat as worldgen's `lib.rs`.

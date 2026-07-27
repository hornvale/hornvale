# The Convalescence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split the population health metric's chronic measure in two — keep
`chronicity` as a diagnostic and add a new per-creature `stuck` (a long distress
run that *never ended*) as the bug alarm — so the null control asserts the
conjunction The Temperament §8 actually specifies instead of only its first
conjunct.

**Architecture:** One field on one struct in one window crate. `HealthReport`
gains `stuck: f64`, computed in the same single pass over each affect trace that
already computes `chronicity` — the run counter is untouched; only what happens
to a run *still open at the trace end* is newly surfaced. The null control's
`assert_eq!(chronicity, 0.0)` moves to `stuck`, and `chronicity` becomes a
reported-but-unbounded diagnostic, exactly as `prevalence` was demoted by
the-living-community.

**Tech Stack:** Rust 2024, `cargo nextest`, `windows/lab` (crate
`hornvale-lab`). No new dependencies, no new files in `src/`.

## Global Constraints

Copied from the spec (`docs/superpowers/specs/2026-07-26-the-convalescence-design.md`)
and `CLAUDE.md`. Every task's requirements implicitly include this section.

- **THE §7 CONSTRAINT — read this first.** This work was occasioned by a
  different campaign that this metric's red null control had blocked. That makes
  it structurally identical to seed-shopping unless held to a higher standard.
  **Justify every decision from The Temperament §8**
  (`docs/superpowers/specs/2026-07-19-the-temperament-design.md`, lines 182–210
  and 259–264) **and validate against `windows/lab/src/synthetic.rs`'s own
  injected scenarios.** Do **not** read, cite, name, or test against
  the-action-clock — not its branch, not its spec, not its numbers. It appears in
  no test, is no success criterion, and its branch is never consulted. If the fix
  cannot stand on §8 plus `synthetic.rs` alone, it is not a fix and must not
  land.
- **Do not make the run counter cause-aware** (spec §2). §8 defines chronicity in
  terms of *labels* (`helpless/frustrated ≥ N ticks`), not causes, so
  cause-agnostic counting is the specified behaviour. Making it cause-aware to
  turn a red test green is exactly what decision 0016 forbids.
- **Do not change `CHRONIC_TICKS`** (spec §8, Out). Its value is not implicated.
- **Do not loosen or delete any assertion to make a suite pass.** `chronicity` is
  *demoted* by an argument from §8, not by a number that was inconvenient. If
  something unexpected goes red, stop and report the measured value — decision
  0016.
- **No `HashMap`/`HashSet`; no wall-clock time.** `BTreeMap`/`BTreeSet`/`Vec`
  only (enforced by `clippy.toml`).
- **Every public item, field and variant gets a one-line doc comment**
  (`#![warn(missing_docs)]` workspace-wide).
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.**
  Adding `stuck` means extending `HealthReport`'s existing tag line *and*
  regenerating the committed report (`docs/audits/type-audit-report.md`) — it is
  drift-checked in CI.
- **`cargo fmt` is the final step before every commit.** A skipped fmt gate is
  this project's most common review finding.
- Run cost-ordered: `cargo fmt` + `clippy` first, then the scoped test, and the
  full `make gate` only at the end. Never re-run a suite to grep a second line —
  `tee` to a file and grep the file.

---

### Task 0: Record the pre-change baseline — **DONE, 2026-07-26**

Run before the plan was written, so any later red is attributable to this
campaign rather than inherited.

```
$ cargo nextest run -p hornvale-lab --test health_calibration     # at 57c31bcb
     Summary [ 256.697s] 15 tests run: 15 passed (1 slow), 0 skipped
```

The two null-control tests are the slow ones (36.6 s for seed 42;
**256.7 s** for the five-seed sweep — this file is the longest battery in the
repo). Spec §6's premise holds: **the null control passes on `main` as it
stands.** Every later count in this plan is against this 15.

---

### Task 1: `stuck` — the conjunction, per creature

**Files:**
- Modify: `windows/lab/src/health.rs:163-266` (the `HealthReport` struct and
  `health_report`)
- Test: `windows/lab/tests/health_calibration.rs` (four new constructed-trace
  tests; two existing tests gain a `stuck` assertion)

**Interfaces:**
- Consumes: `health_report(&[AffectTrace]) -> HealthReport`,
  `CHRONIC_TICKS = 8`, and the test helper
  `trace(&[AffectLabel]) -> AffectTrace` (already in the test file at line 44 —
  it tags every affect with `object: Some(DriveKind::Thirst)`).
- Produces: `HealthReport.stuck: f64` — the fraction of creatures whose distress
  run of at least `CHRONIC_TICKS` never ended. Task 2 asserts on this field.

**The design in one sentence** (spec §4): a distress episode has a **length** and
a **fate**; `chronicity` reads length alone, and `stuck` reads the conjunction
*long AND never ended* — which is §8's bug signal, evaluated per creature so one
stuck creature among nine recovering ones cannot be masked (spec §3).

- [ ] **Step 1: Write the four failing tests**

Append these to `windows/lab/tests/health_calibration.rs`, after
`by_species_separates_a_stricken_people_from_a_healthy_one` (i.e. before the
`// --- END-TO-END:` banner at line 196), so the constructed-trace tier stays
together:

```rust
#[test]
fn a_long_episode_that_recovers_is_a_hard_patch_not_a_bug() {
    // THE DISCRIMINATOR (The Temperament §8): "a spike that RECOVERS (short
    // half-life) is a novel/extreme world event the creatures adapt to —
    // legitimate; a spike that PERSISTS (no recovery, elevated chronicity) is a
    // bug." Two independent distress rhythms can weld into one long run —
    // `HHHH F HHHH`, nine consecutive distress ticks — and then RETURN TO
    // HEALTH. That is a hard patch in a varied world, not a broken sim: it is
    // long (so `chronicity` sees it, as a diagnostic) but it ended (so `stuck`,
    // the alarm, stays silent).
    use AffectLabel::*;
    let mut labels = vec![Content, Content];
    labels.extend([
        Helpless, Helpless, Helpless, Helpless, Frustrated, Helpless, Helpless,
        Helpless, Helpless,
    ]);
    labels.extend([Content, Content, Content]);
    let r = health_report(&[trace(&labels)]);
    assert_eq!(
        r.chronicity, 1.0,
        "the welded run is 9 ticks, over the 8-tick threshold: the DIAGNOSTIC sees it"
    );
    assert_eq!(
        r.stuck, 0.0,
        "but it recovered — a legitimate hard patch, not the bug signal: {r:?}"
    );
    assert_eq!(
        r.recovery_ticks,
        Some(9.0),
        "and its length is the recovery half-life: {r:?}"
    );
}

#[test]
fn one_stuck_creature_among_nine_recovering_ones_still_alarms() {
    // THE MASKING CASE (spec §3): `chronicity` is per-creature but
    // `recovery_ticks` is a POPULATION aggregate, so conjoining the two
    // published numbers at population scope fails in the dangerous direction —
    // this population would read chronicity 1.0 with recovery Some(9.0), i.e.
    // "long distress that recovers → not a bug," and the one creature that
    // never recovers would be MASKED. A bug alarm may be noisy; it may not be
    // silent. `stuck` evaluates the conjunction PER CREATURE, so it fires.
    use AffectLabel::*;
    let mut stuck_labels = vec![Content, Content];
    stuck_labels.extend(std::iter::repeat_n(Helpless, 12)); // never ends
    let mut population = vec![trace(&stuck_labels)];
    for _ in 0..9 {
        let mut recovering = vec![Content, Content];
        recovering.extend(std::iter::repeat_n(Helpless, 9)); // long, but ends
        recovering.extend([Content, Content, Content]);
        population.push(trace(&recovering));
    }
    let r = health_report(&population);
    assert_eq!(
        r.stuck,
        1.0 / 10.0,
        "the one never-recovering creature alarms: {r:?}"
    );
    assert_eq!(
        r.chronicity, 1.0,
        "all ten carry a long run, so chronicity cannot discriminate: {r:?}"
    );
    assert!(
        r.recovery_ticks.is_some(),
        "and nine of them recovered — which at POPULATION scope would have read \
         healthy, masking the stuck one: {r:?}"
    );
}

#[test]
fn a_short_run_still_open_at_the_trace_end_does_not_alarm() {
    // RIGHT-CENSORING, deliberately not an alarm (spec §4): a 3-tick run still
    // open when the trace ends might have recovered one tick later — that is
    // undecidable from the trace, so only LONG-and-open alarms. This
    // asymmetry is intentional; do not "fix" it.
    use AffectLabel::*;
    let r = health_report(&[trace(&[Content, Content, Lost, Lost, Lost])]);
    assert_eq!(
        r.stuck, 0.0,
        "short and still open is censored, not a bug signal: {r:?}"
    );
    assert_eq!(r.chronicity, 0.0, "and it is not even long: {r:?}");
    assert_eq!(
        r.recovery_ticks, None,
        "the run never ended, so nothing recovered: {r:?}"
    );
}

#[test]
fn a_long_run_still_open_at_the_trace_end_is_the_alarm() {
    // THE BUG SIGNAL as §8 states it, on the reduction alone: long AND never
    // ended. The counterpart of the censoring test above — the only difference
    // between them is the run's LENGTH, which is what makes the threshold, not
    // the openness, the discriminator in the open column.
    use AffectLabel::*;
    let mut labels = vec![Content, Content];
    labels.extend(std::iter::repeat_n(Lost, 10));
    let r = health_report(&[trace(&labels)]);
    assert_eq!(r.stuck, 1.0, "long and never ended: the alarm fires: {r:?}");
    assert_eq!(r.chronicity, 1.0, "and the diagnostic agrees: {r:?}");
    assert_eq!(
        r.recovery_ticks, None,
        "a never-ending spike has no recovery half-life: {r:?}"
    );
}
```

Then extend the two existing tests that already describe the alarm, so the field
is asserted everywhere the concept is named:

In `an_unsatisfiable_need_persists` (line 155), after the `chronicity` assertion,
add:

```rust
    assert_eq!(
        r.stuck, 1.0,
        "and it never recovered, so the ALARM fires too (§8's conjunction)"
    );
```

In `an_injected_spike_recovers` (line 133), after the `chronicity` assertion,
add:

```rust
    assert_eq!(r.stuck, 0.0, "a recovered 3-tick spike is not the alarm");
```

- [ ] **Step 2: Run the new tests to verify they fail**

```bash
cargo nextest run -p hornvale-lab --test health_calibration \
  -E 'test(/a_long_episode_that_recovers|one_stuck_creature_among|still_open_at_the_trace_end/)' 2>&1 | tail -30
```

Expected: **compile error**, not a test failure —
`error[E0609]: no field 'stuck' on type 'HealthReport'`, several times. That
compile error IS the red for this step: the field does not exist yet.

- [ ] **Step 3: Add the field to `HealthReport`**

In `windows/lab/src/health.rs`, replace the `chronicity` doc comment and add
`stuck` immediately after it (keeping the pair adjacent, since they read
together), and extend the `type-audit:` tag line above the struct:

```rust
/// The population health family (spec §8) — distress epidemiology over a set of
/// affect traces. Every fraction is in `0.0..=1.0`; `recovery_ticks` is the
/// mean length of a distress spike that DID recover (shorter = more resilient),
/// `None` when there were no recovered spikes.
/// type-audit: bare-ok(ratio: prevalence), bare-ok(ratio: chronicity), bare-ok(ratio: stuck), bare-ok(count: recovery_ticks), bare-ok(ratio: by_cause), bare-ok(ratio: by_species)
#[derive(Clone, Debug, PartialEq)]
pub struct HealthReport {
    /// Fraction of creature-ticks in distress (instantaneous prevalence).
    pub prevalence: f64,
    /// Fraction of creatures with a distress run of at least `CHRONIC_TICKS` —
    /// a DIAGNOSTIC, not the alarm. A long run that RECOVERED is a hard patch
    /// in a varied world, which The Temperament §8 calls legitimate; see
    /// `stuck` for the bug signal.
    pub chronicity: f64,
    /// Fraction of creatures with a distress run of at least `CHRONIC_TICKS`
    /// **that never ended** — THE BUG ALARM: §8's conjunction ("elevated
    /// chronicity, no recovery"), evaluated per creature so one stuck creature
    /// among many recovering ones cannot be masked by a population aggregate.
    pub stuck: f64,
    /// Mean length (ticks) of a distress spike that recovered; `None` if none
    /// did (a healthy world with no spikes, or one where every spike persisted).
    pub recovery_ticks: Option<f64>,
    /// Distress-tick fraction attributed to each drive (by-cause).
    pub by_cause: BTreeMap<String, f64>,
    /// Distress prevalence per species (by-species).
    pub by_species: BTreeMap<String, f64>,
}
```

- [ ] **Step 4: Compute it in the existing single pass**

Still in `windows/lab/src/health.rs`, inside `health_report`:

Add the accumulator beside `chronic_creatures` (currently line 188):

```rust
    let mut chronic_creatures = 0usize;
    let mut stuck_creatures = 0usize;
```

Replace the end-of-trace block (currently lines 231–235) with:

```rust
        // A run still open at the end never recovered. If it is also LONG, this
        // creature is STUCK — the conjunction §8 names as the bug signal ("no
        // recovery, elevated chronicity"), read per creature. A run open at the
        // end is necessarily the last one, so this single check catches every
        // never-ended run.
        //
        // A SHORT open run is deliberately NOT an alarm: it might have
        // recovered one tick after the trace ended, which is right-censoring
        // and undecidable from the trace. Only long-and-open alarms; the
        // asymmetry is intentional (spec §4).
        if run >= CHRONIC_TICKS {
            stuck_creatures += 1;
        }
        if chronic {
            chronic_creatures += 1;
        }
```

And add the field to the returned struct (currently line 259):

```rust
    HealthReport {
        prevalence: frac(distress_ticks, total_ticks),
        chronicity: frac(chronic_creatures, traces.len().max(1)),
        stuck: frac(stuck_creatures, traces.len().max(1)),
        recovery_ticks,
        by_cause,
        by_species,
    }
```

Finally update the module doc's family list (line 8) so the header names the new
member:

```rust
//! (not one number): prevalence, chronicity, stuck (the alarm), recovery-rate,
//! by-cause, and by-species. Searching (normal seeking) is excluded — only the
```

- [ ] **Step 5: Run the constructed-trace tier to verify it passes**

The end-to-end tests in this file are slow; scope to the fast tier first:

```bash
cargo nextest run -p hornvale-lab --test health_calibration \
  -E 'test(/a_long_episode|one_stuck_creature|still_open_at_the_trace_end|an_injected_spike|an_unsatisfiable_need|searching_is_not_distress|by_species_separates_a_stricken_people_from_a_healthy_one/)' 2>&1 | tail -20
```

Expected: **PASS**, 8 tests. If `a_long_episode_that_recovers_is_a_hard_patch_not_a_bug`
fails on `recovery_ticks`, report the measured value — do not adjust the
expectation without saying what it actually was.

- [ ] **Step 6: fmt, clippy, type-audit**

```bash
cargo fmt
cargo clippy -p hornvale-lab --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff --stat docs/audits/type-audit-report.md
```

Expected: clippy clean; `check` passes (the new field is tagged); the report diff
shows the `lab` row moving from `48 | 0 | 2 | 50` to `49 | 0 | 2 | 51` and one
new tagged-item row. If `check` fails with an untagged-primitive error naming
`stuck`, the tag line edit in Step 3 was missed.

- [ ] **Step 7: Commit**

```bash
git add windows/lab/src/health.rs windows/lab/tests/health_calibration.rs docs/audits/type-audit-report.md
git commit -m "feat(lab): stuck — a long distress run that never ended (§8's conjunction)

The Temperament §8 states the bug signal twice as a CONJUNCTION — 'a spike that
persists (no recovery, elevated chronicity) is a bug' — with recovery as the
explicit discriminator between a hard world and a broken sim. \`chronicity\`
reads only the first conjunct. \`stuck\` reads both, per creature: a run of at
least CHRONIC_TICKS that never ended.

Per creature, not per population, because \`recovery_ticks\` is an aggregate:
one stuck creature among nine recovering ones would read chronicity high and
recovery Some(..), and a population-scope conjunction would call that healthy
(the masking test covers exactly this).

Short-and-still-open is right-censored and deliberately does not alarm.

The run counter is unchanged: §8 defines chronicity over LABELS, not causes."
```

---

### Task 2: Move the alarm — the null control asserts `stuck`

**Files:**
- Modify: `windows/lab/tests/health_calibration.rs:67-114` (the two null-control
  tests) and `:205-230`, `:367-392` (two end-to-end assertions)

**Interfaces:**
- Consumes: `HealthReport.stuck` from Task 1;
  `simulate_world(&World) -> Vec<AffectTrace>`;
  `stranded_from_known_water()` and `a_heat_wave_that_passes()` from
  `hornvale_lab::synthetic` (both already imported in this file).
- Produces: the re-baselined five-seed health family, printed per seed, to be
  quoted in the commit message and the chronicle.

- [ ] **Step 1: Move the seed-42 null control's bound to `stuck`**

In `the_null_control_reads_no_chronic_distress`, replace the assertion at line 92
and extend the comment block that precedes it. Replace:

```rust
    assert_eq!(a.chronicity, 0.0, "healthy world: no one chronically stuck");
```

with:

```rust
    assert_eq!(
        a.stuck, 0.0,
        "healthy world: no one is STUCK (a long distress run that never \
         recovered): {a:?}"
    );
```

and append this paragraph to the comment block above (after the sentence ending
"…is no longer bounded here.", line 88):

```rust
    // Re-derived again at The Convalescence: the bound moved one step further
    // along the same argument. §8 states the bug signal as a CONJUNCTION —
    // "a spike that persists (NO RECOVERY, elevated chronicity) is a bug",
    // where a spike that recovers is a legitimate novel world event — and this
    // assertion checked only the first conjunct, so it fired on episodes that
    // RECOVERED. Two independent distress rhythms in one creature can weld
    // into a single long run (`HHHH F HHHH`) that then returns to health;
    // under §8 that is a hard patch in a varied world, not a broken sim. The
    // alarm is therefore `stuck` — long AND never ended, evaluated per
    // creature (a population-scope conjunction would let one genuinely stuck
    // creature hide behind nine recovering ones). `chronicity` is still
    // computed and still reported, as a DIAGNOSTIC; it is no longer bounded
    // here, exactly as `prevalence` was demoted above.
```

- [ ] **Step 2: Move the seed-sweep bound to `stuck` and print the family**

Replace the body of `the_null_control_holds_across_a_seed_sweep` (lines 100–114)
with:

```rust
#[test]
fn the_null_control_holds_across_a_seed_sweep() {
    // Over a small sweep of real worlds, no population reads STUCK distress —
    // the zero is not a seed-42 accident. (Genuine blocked-distress needs a
    // creature boxed in or knowing-but-blocked, which condensed on-water
    // settlements avoid; a healthy world stays healthy.) The bug alarm is armed
    // precisely because this stays quiet.
    //
    // `chronicity` is reported per seed, not bounded (The Convalescence): a
    // long distress run that RECOVERS is a hard patch in a varied world, which
    // The Temperament §8 calls legitimate. The alarm is the conjunction — long
    // AND never recovered — which is `stuck`.
    let mut report = Vec::new();
    for seed in [0u64, 1, 2, 7, 42] {
        let r = health_report(&simulate_world(&world(seed)));
        report.push(format!(
            "seed {seed}: stuck {:.4} chronicity {:.4} prevalence {:.4} recovery {:?}",
            r.stuck, r.chronicity, r.prevalence, r.recovery_ticks
        ));
        assert_eq!(
            r.stuck, 0.0,
            "seed {seed} shows STUCK distress (the alarm fired): {r:?}"
        );
    }
    eprintln!("the-convalescence health-family baseline, per seed:");
    for line in &report {
        eprintln!("  {line}");
    }
}
```

(`eprintln!`-ing a per-seed measurement from a calibration test is the existing
repo idiom — see `windows/lab/tests/the_dial.rs:261` and
`windows/lab/tests/the_explanations.rs:110`.)

- [ ] **Step 3: Assert the alarm on `synthetic.rs`'s own injected scenarios**

This is spec §6's first acceptance criterion — the alarm must still fire on the
scenario the metric exists to catch — and its counterpart.

In `a_stranded_creature_is_scored_chronic_end_to_end` (line 205), after the
`chronicity` assertion, add:

```rust
    assert_eq!(
        r.stuck, 1.0,
        "and it never recovers, so the ALARM fires — splitting the measure must \
         not silence the scenario the metric exists to catch: {r:?}"
    );
```

In `a_passing_heat_wave_is_scored_a_recovered_spike_end_to_end` (line 367), after
the `chronicity` assertion, add:

```rust
    assert_eq!(
        r.stuck, 0.0,
        "a wave that breaks never alarms: {r:?}"
    );
```

- [ ] **Step 4: fmt, clippy, then run the whole battery**

```bash
cargo fmt
cargo clippy -p hornvale-lab --all-targets -- -D warnings
cargo nextest run -p hornvale-lab --test health_calibration --no-fail-fast \
  --success-output final 2>&1 | tee /tmp/hv-convalescence-after.txt
```

Expected: **PASS**, 19 tests (the 15 of Task 0's baseline + 4 new). ~4.5 min,
almost all of it the five-seed sweep (256.7 s at baseline); foreground, long
timeout, do not poll in a loop. `--success-output final` is what surfaces the
per-seed `eprintln!` from a passing test.

- [ ] **Step 5: Record the re-baseline**

```bash
grep -A6 "the-convalescence health-family baseline" /tmp/hv-convalescence-after.txt
```

Copy the five printed lines verbatim into the task report — they are the spec's
"the five-seed health family is re-baselined and recorded" criterion, and Task 3
quotes them in the chronicle. If any seed reads `stuck > 0`, STOP: that is a real
alarm on a natural world, and it is an investigation, not a number to adjust
(decision 0016).

- [ ] **Step 6: Commit**

```bash
git add windows/lab/tests/health_calibration.rs
git commit -m "test(lab): the health null control asserts stuck, not chronicity

§8's bug signal is 'no recovery, elevated chronicity' — a conjunction. The null
control asserted only the first conjunct, so it fired on distress episodes that
RECOVERED, which §8 explicitly calls legitimate (a novel world event the
creatures adapt to). The bound moves to \`stuck\`; \`chronicity\` is still
computed, still reported per seed, no longer bounded — the same demotion
the-living-community applied to \`prevalence\`, one step further along the same
argument.

The alarm is proven still live on the harness's own injected scenarios: the
stranded creature (never recovers) reads stuck 1.0; the heat wave that breaks
reads stuck 0.0.

Five-seed re-baseline (seeds 0/1/2/7/42), stuck 0.0 throughout:
<PASTE THE FIVE eprintln LINES FROM STEP 5 HERE>"
```

---

### Task 3: Close — the book, the retrospective, the decision log

**Files:**
- Create: `book/src/chronicle/the-convalescence.md`
- Create: `docs/retrospectives/the-convalescence.md`
- Create: `docs/decisions/00NN-chronicity-is-a-diagnostic-the-alarm-is-stuck.md`
  (**NN = the next free number after absorbing `main`** — `ls docs/decisions/`
  and take the next; do not guess, another campaign may have landed one)
- Modify: `book/src/SUMMARY.md` (register the chronicle entry after
  `The Margin`)
- Modify: `docs/decisions/README.md` (the index row)
- Modify: `book/src/chronicle/the-temperament.md:48-56` (freshness — its prose
  describes the alarm)
- Modify: `book/src/frontier/idea-registry.md` (the `PSY-7` row's
  "chronicity/recovery" phrase — freshness)

**Interfaces:**
- Consumes: the five-seed re-baseline lines recorded in Task 2 Step 5.
- Produces: the campaign's Definition-of-Done artifacts (CLAUDE.md: chronicle
  entry + freshness sweep + retrospective).

- [ ] **Step 1: Absorb `main` and pick the decision number**

```bash
git fetch origin
git merge origin/main
git diff the-convalescence...origin/main --stat | tail -5   # what actually came in
ls docs/decisions/ | tail -3
```

If the merge conflicts, resolve it and re-run Task 2 Step 4 before continuing —
`cargo` reads the working tree, so a hand-resolved merge is not proven by an
earlier green. Take the next free decision number from the `ls`.

- [ ] **Step 2: Write the decision entry**

Follow the shape of `docs/decisions/0078-thresholded-classification-artifacts-are-platform-local.md`.
Content, in the log's own voice:

- **Decision:** In the population health family, `chronicity` is a *diagnostic*;
  the bug alarm is `stuck` — a distress run of at least `CHRONIC_TICKS` that
  never ended, evaluated per creature.
- **Why:** The Temperament §8 states the bug signal as a conjunction ("a spike
  that persists — no recovery, elevated chronicity — is a bug") and names
  recovery as the discriminator between a hard world and a broken sim. A bound on
  `chronicity` alone fires on recovered episodes, which §8 calls legitimate.
- **Why per creature:** `recovery_ticks` is a population aggregate; conjoining it
  with `chronicity` at population scope lets one never-recovering creature hide
  behind nine recovering ones. A bug alarm may be noisy; it may not be silent.
- **Why short-and-open does not alarm:** right-censoring — a short run still open
  at the trace end might have recovered a tick later, which is undecidable from
  the trace. The asymmetry is deliberate.
- **What this does not change:** the run counter stays cause-agnostic (§8 defines
  chronicity over labels, not causes); `CHRONIC_TICKS` is untouched.
- **Precedent it extends:** the-living-community's demotion of `prevalence` from
  bound to diagnostic, for the same reason and citing the same spec — which left
  only a test comment, no decision entry.
- **Why this one earns an entry where that one did not** (the reason the entry
  exists at all, and the part a later reader most needs): that demotion was
  occasioned by ordinary drift; this one was occasioned by a campaign the red
  control had *blocked*. A demotion under those circumstances is exactly the
  shape of rationalized seed-shopping, so the record must be greppable from
  `docs/decisions/` — where a process-integrity audit ("did this project ever
  loosen a bound to unblock a campaign?") actually looks — and not only from a
  comment in the test, which such an audit never sees. The entry must therefore
  record the *discipline* (justified from §8, validated on the metric's own
  synthetic scenarios, the occasioning campaign never consulted, the unblocking
  verified last and separately) alongside the metric semantics. Generalized: a
  bound demotion occasioned by a blocked campaign owes a decision entry; one
  occasioned by ordinary drift does not.
- **Consequences:** a future reader asking "why isn't `chronicity` bounded?" has
  an answer here rather than relitigating it; re-bounding `chronicity` is a
  supersede, not an edit.

Add the index row to `docs/decisions/README.md` in the existing format.

- [ ] **Step 3: Write the chronicle entry**

`book/src/chronicle/the-convalescence.md`, in the book's established altitude
(technical, mathematical, comprehensible without the code — see
`book/src/chronicle/the-wilding.md` for the register). It should carry:

- The instrument that scored the world had drifted from its own definition. A
  regression alarm is only as good as the quantity it reads.
- §8 defined the bug signal as a conjunction — persistence *and* no recovery —
  and named recovery as what separates a hard world from a broken sim. The
  control asserted the first half.
- The consequence: a creature can carry two independent distress rhythms, and a
  one-tick phase shift can weld a four-tick block, a single frustrated tick, and
  another four-tick block into one nine-tick run that then returns to health. The
  alarm fired; nothing was broken.
- Why the obvious fix (conjoin the two published numbers) is worse: one is per
  creature, the other a population mean, and the mismatch fails silently rather
  than noisily — the failure direction a bug alarm may never take.
- The 2×2 that resolves it: length × fate. A blip; a hard patch; the bug signal;
  and a censored cell that is undecidable and deliberately silent.
- The five-seed re-baseline (quote the recorded lines) — `stuck` reads zero on
  every healthy world, so the alarm is armed by staying quiet, and `chronicity`
  is now reported for what it is.

Register it in `book/src/SUMMARY.md` on the line after `The Margin`:

```markdown
- [The Convalescence](./chronicle/the-convalescence.md)
```

- [ ] **Step 4: Freshness sweep**

The book may never lag merged reality (CLAUDE.md, Definition of Done).

1. `book/src/chronicle/the-temperament.md` around lines 48–56 describes the
   metric as "prevalence of distress now, chronicity of the persistently stuck,
   the half-life of a spike that recovers" and calls it "a regression alarm,
   armed precisely by reading zero on every healthy world." Update it to name the
   member that actually reads zero, without rewriting the campaign's own history
   — a sentence noting that the alarm was later sharpened to the conjunction, and
   a link to `the-convalescence.md`.
2. `book/src/frontier/idea-registry.md`, the `PSY-7` row: the phrase "reduced
   across a population into a self-scoring health metric (chronicity/recovery)"
   becomes the current family, with a short The Convalescence clause in the row's
   established bold-campaign-name style and the chronicle added to its source
   column.
3. Check the Confidence Gradient for a bet this campaign moved:
   `grep -n "alarm\|self-scor\|health" book/src/open-questions.md`. If a bet
   names this metric, re-score it (decision 0030). If none does, say so
   explicitly in the task report — "no bet moved" is a finding, not a skip.
4. `mdbook build book` — expected: no warnings about the new file or dead links.

- [ ] **Step 5: Write the retrospective**

`docs/retrospectives/the-convalescence.md`, one page, **process lessons not
product** (decision 0020). The candidates this campaign actually produced:

- The finding came from reading the *originating spec*, not the failing test. The
  test's own comment stated the conjunction correctly while its assertion did
  not — a comment and its assertion drifting apart is a specific, greppable
  failure mode.
- A red control that blocks a campaign is the highest-risk moment for
  rationalized seed-shopping. The discipline that made this landable was
  structural, not attitudinal: justify from the defining spec, validate against
  the metric's own synthetic scenarios, never consult the blocked campaign's
  numbers, verify the unblocking last and separately.
- The obvious fix was wrong in a way only a scope check caught (per-creature vs
  population aggregate). "These two published numbers agree" is not a
  composition rule.
- Whatever else execution surfaced.

- [ ] **Step 6: fmt and the full commit gate**

```bash
cargo fmt
make gate 2>&1 | tee /tmp/hv-convalescence-gate.txt; tail -30 /tmp/hv-convalescence-gate.txt
```

Expected: **PASS** (~4 min plus the health battery). The full gate is required
here, not `gate-fast`: this campaign changed a `pub` boundary, so the type-audit
and the committed report are in scope (and a scoped gate would miss them).

- [ ] **Step 7: Commit**

```bash
git add book/src/chronicle/the-convalescence.md book/src/SUMMARY.md \
        book/src/chronicle/the-temperament.md book/src/frontier/idea-registry.md \
        docs/retrospectives/the-convalescence.md docs/decisions/
git commit -m "docs(the-convalescence): close — chronicle, retrospective, decision 00NN

The alarm is the conjunction §8 named, and \`chronicity\` is a diagnostic. The
book's Temperament chapter and the PSY-7 registry row are swept to match."
```

- [ ] **Step 8: Verify the unblocking last, and separately**

Spec §7: that another campaign is unblocked afterwards is a *consequence*, never
a goal. It is verified only now, after the fix has stood on §8 and
`synthetic.rs` alone, and it is **not** a success criterion — if it turned out
not to unblock anything, nothing in this campaign would change.

Report to Nathan at the G6 stop; do not check out or consult any other branch as
part of this plan.

---

## Self-review

**Spec coverage.** §1 finding → Task 1's `a_long_episode_that_recovers…` test
plants the worked case. §2 (no cause-awareness) → Global Constraints, and the run
counter is untouched by every diff in Task 1. §3 masking hole → Task 1's
`one_stuck_creature_among_nine_recovering_ones_still_alarms`. §4 design (the 2×2,
the censored cell) → Task 1 Steps 3–4 plus the two open-run tests. §4.1 precedent
→ cited in the Task 2 comment, the commit message, and the decision entry. §5
blast radius → no study/CSV/CLI touched by any task; the type-audit report is the
one generated artifact in scope, handled in Task 1 Step 6. §6 acceptance, all six
bullets → alarm-still-fires (Task 2 Step 3, `stranded_from_known_water`),
alarm-silent-on-recovering-patch (Task 1), masking (Task 1), censoring (Task 1),
null control green (Task 2 Step 4), re-baseline recorded (Task 2 Step 5). §7 →
Global Constraints and Task 3 Step 8. §8 scope → the Out list appears in Global
Constraints as prohibitions.

**Placeholders.** One deliberate fill-in each in Task 2 Step 6 and Task 3 Step 7
(the measured baseline lines; the decision number), both with an explicit
instruction for where the value comes from. No "add error handling", no "similar
to Task N", no undefined types.

**Type consistency.** `stuck: f64` is declared once (Task 1 Step 3) and read as
`r.stuck` / `a.stuck` everywhere after. `frac(stuck_creatures,
traces.len().max(1))` matches the existing `chronicity` denominator idiom
verbatim. `health_report`, `simulate_world`, `trace`, `stranded_from_known_water`
and `a_heat_wave_that_passes` all keep their existing signatures — no call site
changes shape.

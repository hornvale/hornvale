//! Ordered parallel seed sweeps for the heavy-tier calibration batteries.
//!
//! `cargo nextest` runs each test in its own PROCESS, so it parallelizes
//! *across* tests and can do nothing for a test that is itself one long
//! serial `for seed in 1..=N` loop. The preregistered readouts in
//! `the_mire_calibration.rs` and `the_fare_calibration.rs` were exactly that
//! shape, and between them set the whole heavy tier's wall clock (~6100 s
//! each on the 40-core canonical box, where the next slowest test is 537 s
//! and the tier as a whole ran at `cpu_ratio 1.80` — thirty-eight cores
//! idle).
//!
//! This is the same pattern `windows/lab/src/runner.rs`'s `run_pin_set`
//! already uses for every byte-identical census, lifted into a helper the
//! test binaries share. **The load-bearing property is that output order is
//! seed order, never completion order:** each seed's result lands in its own
//! slot, indexed by position in the seed list, and the slots are read back in
//! order on the main thread. The thread count therefore affects speed and
//! nothing else.
//!
//! A panic in a worker is re-raised on the main thread (the runner's
//! `.expect("a study worker thread panicked")` model), so `build_sample`'s
//! deliberate loud panic on a seed that fails to build still fails the test
//! rather than silently shrinking a preregistered population.

/// Environment override for the worker count, honoured by [`map_seeds`].
///
/// Two uses, both deliberate. Setting it to `1` reproduces the old serial
/// loop exactly — that is how these readouts were shown byte-identical to
/// their pre-parallel selves, and it stays available so the check can be
/// repeated against any future change. And a small value caps peak memory on
/// a box that cannot hold one live world per core: each worker holds a full
/// `WorldSample` (two `SubstrateField`s over ~41k cells × a year of days,
/// plus the climate and the cost/graph fields derived from it).
const THREADS_ENV: &str = "HV_SEED_SWEEP_THREADS";

/// How many workers [`map_seeds`] spawns for `count` seeds: the machine's
/// available parallelism, overridable by [`THREADS_ENV`], never more than
/// there are seeds and never less than one.
///
/// **Fails fast on a malformed override**, rather than falling back to the
/// default. This is the one variable whose purpose is reproducing a
/// byte-identity proof: a typo that silently restored full parallelism, or a
/// `0` that silently meant serial, would make the check quietly measure
/// something other than what the operator asked for — the failure mode a
/// verification knob can least afford.
fn thread_budget(count: usize) -> usize {
    let requested = match std::env::var(THREADS_ENV) {
        Err(_) => std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1),
        Ok(raw) => parse_threads(&raw).unwrap_or_else(|| {
            panic!(
                "{THREADS_ENV}={raw:?} is not a positive integer. It sets the seed sweep's \
                 worker count; `1` reproduces the serial loop exactly. Unset it for the \
                 machine's available parallelism — it is never inferred from a malformed \
                 value, because this is the knob a byte-identity proof is driven by."
            )
        }),
    };
    requested.clamp(1, count.max(1))
}

/// The accepted spellings of [`THREADS_ENV`], as a pure function so the
/// reject-vs-accept boundary is unit-testable without mutating the process
/// environment (which would race the other tests in the same binary).
/// `None` means "malformed — refuse", never "use the default".
fn parse_threads(raw: &str) -> Option<usize> {
    raw.trim().parse::<usize>().ok().filter(|&n| n > 0)
}

/// The override's accept/reject boundary. `0` and a typo must both be
/// REFUSALS, not silent fallbacks — a knob that exists to drive a
/// byte-identity proof may not quietly measure something else.
#[test]
fn the_thread_override_refuses_zero_and_nonsense() {
    assert_eq!(parse_threads("8"), Some(8));
    assert_eq!(parse_threads("  4\n"), Some(4));
    assert_eq!(parse_threads("1"), Some(1));
    assert_eq!(parse_threads("0"), None);
    assert_eq!(parse_threads("eight"), None);
    assert_eq!(parse_threads("-2"), None);
    assert_eq!(parse_threads(""), None);
}

/// Run `f` once per seed across the available CPUs, returning the results in
/// **seed order**.
///
/// Byte-identical to `seeds.into_iter().map(f).collect()` for any pure `f`:
/// every world is a pure function of its seed, and results are reassembled by
/// the seed's position in the list rather than by which worker finished
/// first. Each worker owns a contiguous seed range and writes to no shared
/// state, exactly as `run_pin_set` does.
///
/// Anything `f` prints (per-seed progress, say) is inherently interleaved —
/// only the returned values carry the ordering guarantee.
pub fn map_seeds<T, F>(seeds: impl IntoIterator<Item = u64>, f: F) -> Vec<T>
where
    T: Send,
    F: Fn(u64) -> T + Sync,
{
    let seeds: Vec<u64> = seeds.into_iter().collect();
    let threads = thread_budget(seeds.len());
    map_seeds_on(threads, seeds, f)
}

/// [`map_seeds`] with the worker count supplied rather than discovered — the
/// seam the ordering tests below drive, so they need no environment
/// mutation (which would race the other tests in the same binary under
/// `cargo test`).
fn map_seeds_on<T, F>(threads: usize, seeds: Vec<u64>, f: F) -> Vec<T>
where
    T: Send,
    F: Fn(u64) -> T + Sync,
{
    let count = seeds.len();
    if count == 0 {
        return Vec::new();
    }
    if threads <= 1 {
        return seeds.into_iter().map(f).collect();
    }

    // One result slot per seed position, filled by worker threads and read
    // back in position order on the main thread — so output order never
    // depends on scheduling. Each thread owns a contiguous range (no shared
    // writes).
    let mut slots: Vec<Option<T>> = (0..count).map(|_| None).collect();
    let chunk = count.div_ceil(threads);
    let f = &f;
    let seeds = &seeds;

    std::thread::scope(|scope| {
        let mut handles = Vec::with_capacity(threads);
        for t in 0..threads {
            let lo = t * chunk;
            if lo >= count {
                break;
            }
            let hi = ((t + 1) * chunk).min(count);
            handles
                .push(scope.spawn(move || (lo..hi).map(|i| (i, f(seeds[i]))).collect::<Vec<_>>()));
        }
        for handle in handles {
            for (i, value) in handle.join().expect("a seed worker thread panicked") {
                slots[i] = Some(value);
            }
        }
    });

    slots
        .into_iter()
        .map(|slot| slot.expect("every seed produced a result"))
        .collect()
}

/// The ordering guarantee, asserted directly rather than argued: a sweep
/// whose per-seed work takes wildly different amounts of time (so completion
/// order is certainly not seed order) still returns its results in seed
/// order, and visits every seed exactly once.
#[test]
fn map_seeds_returns_results_in_seed_order() {
    // Descending sleeps: the low seeds finish last within their chunk, so a
    // completion-ordered collection would come back scrambled.
    let out = map_seeds_on(8, (1..=64u64).collect(), |seed| {
        std::thread::sleep(std::time::Duration::from_millis(64 - seed));
        seed * 10
    });
    let expected: Vec<u64> = (1..=64u64).map(|s| s * 10).collect();
    assert_eq!(out, expected);
}

/// A serial sweep and a parallel one agree, at every worker count from 1 to
/// 9 over a seed list that divides evenly into none of them — the
/// equivalence the readouts' verification rests on, in miniature, with the
/// chunking arithmetic's off-by-one exercised at each width.
#[test]
fn every_worker_count_agrees_with_the_serial_sweep() {
    let expected: Vec<u64> = (1..=23u64).map(|s| s * s + 7).collect();
    for threads in 1..=9 {
        let got = map_seeds_on(threads, (1..=23u64).collect(), |seed| seed * seed + 7);
        assert_eq!(got, expected, "worker count {threads} disagreed");
    }
}

/// An empty sweep is empty, and a panicking worker fails the sweep rather
/// than being swallowed — the property that keeps `build_sample`'s loud
/// panic on an unbuildable seed from silently shrinking a preregistered
/// population.
#[test]
fn a_worker_panic_propagates_and_an_empty_sweep_is_empty() {
    let empty: Vec<u64> = map_seeds_on(4, Vec::new(), |seed| seed);
    assert!(empty.is_empty());

    let outcome = std::panic::catch_unwind(|| {
        map_seeds_on(4, (1..=8u64).collect(), |seed| {
            assert_ne!(seed, 5, "deliberate worker panic");
            seed
        })
    });
    assert!(
        outcome.is_err(),
        "a panicking worker did not fail the sweep"
    );
}

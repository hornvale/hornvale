//! The disk glue, exercised through the REAL entry points — M9.
//!
//! `state_dir`'s resolution rule and `PhaseTimings`' file format are each unit-
//! tested purely (`state_dir::resolve` over three borrowed strings, and
//! `load_from`/`save_to` over an explicit path), and that is deliberate: no unit
//! test in this crate touches the environment or the real state directory. But
//! it left a seam nothing crossed. Nothing called `state_dir::state_dir`,
//! `state_path`, `ensure`, `PhaseTimings::{load, save, path}`, or `Frame::new`
//! at all — so the *composition* of the pure parts (does `state_dir()` read the
//! three variables in the documented order? does `PhaseTimings::path()` join the
//! documented file name onto it?) was unverified, and either could have been
//! wrong while every unit test stayed green.
//!
//! The `HORNVALE_GAME_STATE_DIR` override exists precisely so this test can
//! exist. It points at a temporary directory, so the real
//! `$XDG_CACHE_HOME`/`$HOME` are never written even though the real functions
//! are the ones under test.
//!
//! # Why one test function, and why the lock
//!
//! `make game-check` drives these through plain `cargo`, not nextest, so these
//! integration tests are THREADS OF ONE PROCESS — there is no process-per-test
//! isolation here, and the environment is process-global. Two tests mutating
//! `HORNVALE_GAME_STATE_DIR` concurrently would flake, and `std::env::set_var`
//! is `unsafe` in edition 2024 for exactly that reason. So the scenario is one
//! test function holding one mutex, sequenced by hand: it is a single narrative
//! anyway (write a baseline, read it back, then read it back through the frame),
//! and splitting it into three would buy nothing but three chances to race.

use hornvale_game::overture::{Frame, Phase, PhaseTimings, View};
use hornvale_game::state_dir::{self, STATE_DIR_ENV};
use hornvale_game_core::Grid;
use hornvale_kernel::World;
use hornvale_worldgen::{BuildDepth, RungArtifacts};
use std::path::{Path, PathBuf};
use std::time::Duration;

/// Serialises every mutation of the process-global environment in this binary.
static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

/// Set `HORNVALE_GAME_STATE_DIR` to `value`, returning what it was.
///
/// # Safety
/// The caller must hold [`ENV_LOCK`] for the whole window in which the value is
/// set, and must restore the previous value before releasing it.
unsafe fn swap_state_dir(value: Option<&std::ffi::OsStr>) -> Option<std::ffi::OsString> {
    let previous = std::env::var_os(STATE_DIR_ENV);
    unsafe {
        match value {
            Some(v) => std::env::set_var(STATE_DIR_ENV, v),
            None => std::env::remove_var(STATE_DIR_ENV),
        }
    }
    previous
}

/// Run `f` with `HORNVALE_GAME_STATE_DIR` pointed at a fresh temporary
/// directory, restoring whatever it was afterwards.
///
/// Restores rather than merely unsetting, so a developer who has the variable
/// set in their own shell is not silently left without it for the rest of the
/// run.
fn with_state_dir<T>(tag: &str, f: impl FnOnce(&Path) -> T) -> T {
    let _guard = ENV_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    let dir = std::env::temp_dir().join(format!(
        "hornvale-overture-statedir-{}-{tag}",
        std::process::id()
    ));
    std::fs::remove_dir_all(&dir).ok();
    // SAFETY: `ENV_LOCK` is held for the whole body, and this binary's only
    // environment mutations go through this function and its sibling below.
    let previous = unsafe { swap_state_dir(Some(dir.as_os_str())) };

    /// Restores the previous `HORNVALE_GAME_STATE_DIR` on drop, including on
    /// unwind — `swap_state_dir`'s own `# Safety` clause requires the caller
    /// to restore the previous value before releasing `ENV_LOCK`, and a bare
    /// post-call restore skips that if `f` panics, leaking the temporary
    /// directory into every later test that shares this process.
    struct Restore(Option<std::ffi::OsString>);
    impl Drop for Restore {
        fn drop(&mut self) {
            // SAFETY: same window `swap_state_dir`'s caller contract names —
            // `ENV_LOCK` (held by `_guard`, still in scope here since drop
            // order is last-declared-first-dropped) covers this call too.
            unsafe { swap_state_dir(self.0.as_deref()) };
        }
    }
    let _restore = Restore(previous);

    let out = f(&dir);
    std::fs::remove_dir_all(&dir).ok();
    out
}

/// A view that never speaks, so `Frame::new` can be exercised without a world.
struct Mute;

impl View for Mute {
    fn name(&self) -> &'static str {
        "mute"
    }
    fn can_speak(&self, _rung: BuildDepth) -> bool {
        false
    }
    fn render(
        &mut self,
        _world: &World,
        _rung: BuildDepth,
        _artifacts: RungArtifacts<'_>,
        w: u16,
        _h: u16,
    ) -> Grid {
        Grid::new(w.max(1), 1)
    }
}

#[test]
fn the_real_disk_glue_round_trips_through_the_env_override() {
    with_state_dir("roundtrip", |dir| {
        // 1. `state_dir()` honours the override — the three-variable ordering in
        //    the real function, not `resolve`'s pure twin.
        assert_eq!(state_dir::state_dir().as_deref(), Some(dir));

        // 2. `state_path` composes onto it, and `PhaseTimings::path()` names the
        //    documented file. Asserting the EXACT path is what makes this
        //    discriminate: a `path()` returning the bare directory, or the wrong
        //    file name, would still let save and load round-trip with each other
        //    while writing somewhere nobody documented.
        let expected: PathBuf = dir.join("overture-timings.tsv");
        assert_eq!(
            state_dir::state_path("overture-timings.tsv").as_deref(),
            Some(expected.as_path())
        );
        assert_eq!(PhaseTimings::path().as_deref(), Some(expected.as_path()));

        // 3. Nothing on disk yet, so `load()` is the first-ever-run state — and
        //    it must be that because the FILE is absent, not because the override
        //    is broken (step 1 already proved it is not).
        assert!(!dir.exists(), "nothing should have created the dir yet");
        assert!(PhaseTimings::load().is_empty());

        // 4. `ensure()` creates it.
        let ensured = state_dir::ensure().expect("create the state dir");
        assert_eq!(ensured.as_deref(), Some(dir));
        assert!(dir.is_dir());

        // 5. `save()` writes through the real path and reports that it did.
        let mut written = PhaseTimings::empty();
        written.set(Phase::Land, Duration::from_millis(202));
        written.set(Phase::Peoples, Duration::from_millis(1_840));
        assert!(
            written.save().expect("save"),
            "save reported nowhere to write"
        );
        assert!(expected.is_file(), "save() wrote somewhere else");

        // 6. `load()` reads it back, and the bytes are the documented format.
        assert_eq!(PhaseTimings::load(), written);
        assert_eq!(
            std::fs::read_to_string(&expected).unwrap(),
            "land\t202\npeoples\t1840\n"
        );

        // 7. And `Frame::new` — the only caller of `PhaseTimings::load` on the
        //    shipped path — picks that baseline up. This is the whole point of the
        //    chain: a first-ever run draws no bar and a second run does, and until
        //    now nothing had exercised the disk half of that at all.
        let frame = Frame::new(vec![Box::new(Mute)]);
        assert_eq!(
            frame.baseline().get(Phase::Peoples),
            Some(1_840),
            "Frame::new did not read the baseline off disk"
        );

        // Non-vacuity: with the file removed, the same call yields nothing — so
        // the assertion above is about the file, not about `Frame::new`
        // fabricating a value.
        std::fs::remove_file(&expected).unwrap();
        let fresh = Frame::new(vec![Box::new(Mute)]);
        assert_eq!(fresh.baseline().get(Phase::Peoples), None);
    });
}

#[test]
fn a_relative_override_is_ignored_by_the_real_resolver_too() {
    // The pure `resolve` already pins the fall-through. This checks the REAL
    // function reads the variable in that case rather than, say, trusting its
    // override blindly. It deliberately does not assert where resolution lands,
    // because that depends on the developer's own `$XDG_CACHE_HOME`/`$HOME`.
    let _guard = ENV_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    // SAFETY: `ENV_LOCK` is held across both calls.
    let previous = unsafe { swap_state_dir(Some(std::ffi::OsStr::new("a-relative-name"))) };
    let got = state_dir::state_dir();
    unsafe { swap_state_dir(previous.as_deref()) };

    assert_ne!(
        got.as_deref(),
        Some(Path::new("a-relative-name")),
        "a relative override was taken verbatim"
    );
    if let Some(path) = got {
        assert!(
            path.is_absolute(),
            "resolution yielded a relative path: {path:?}"
        );
    }
}

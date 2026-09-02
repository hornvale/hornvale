//! Per-turn work counters (The Rack, spec §3.5).
//!
//! Before this campaign's later tasks land, `Session::snapshot` and
//! `Session::needs` are `&self` reads that still re-fold every present
//! body's drives and ledger position from the ledger on every call — at
//! seed 42's flagship that is one fold per present body, 67 of them, plus a
//! ledger position fold per body in `colocated_npcs` and `narrate_motion`,
//! plus a fresh shadowcast per `Session::sighting` call. This module is the
//! INSTRUMENT, landing before the change (Task 4) that makes most of that
//! work zero, so the budget tests it backs
//! (`windows/vessel/tests/suite/turn_budget.rs`) have a measured positive
//! control rather than an assumed one.

/// Per-turn work counters (The Rack, spec §3.5): deterministic on every box,
/// reset by [`crate::Session::handle`] for every non-empty verb line.
/// Interior-mutable fields — each waived below as the Rust
/// interior-mutability type, not this codebase's mesh-vertex sense — because
/// the reads that do the work, `snapshot` and `needs`, are `&self`.
/// type-audit: bare-ok(count: affect_folds), bare-ok(count: position_folds), bare-ok(count: shadowcasts), bare-ok(count: plan_searches), bare-ok(count: bodies_scanned)
#[derive(Debug, Default)]
pub struct TurnWork {
    /// How many times a body's drive state was folded from the ledger this
    /// turn (`affect_of_memo_occupied` calls in `Session::snapshot` and
    /// `Session::needs`).
    pub affect_folds: std::cell::Cell<u32>, // lexicon: std::cell::Cell interior-mutability field, not the mesh sense
    /// How many times a body's ledger position was folded this turn
    /// (`Session::position` plus the direct `agent_position` call sites in
    /// `wait`, `narrate_motion` and `colocated_npcs`).
    pub position_folds: std::cell::Cell<u32>, // lexicon: std::cell::Cell interior-mutability field, not the mesh sense
    /// How many fresh shadowcasts `Session::sighting` derived this turn.
    pub shadowcasts: std::cell::Cell<u32>, // lexicon: std::cell::Cell interior-mutability field, not the mesh sense
    /// `HomeNavCache::searches()` as of the most recent [`Self::reset`] —
    /// the baseline [`Self::read`]'s own `plan_searches` is measured
    /// against, since the cache itself is monotone and never resets.
    pub plan_searches: std::cell::Cell<u64>, // lexicon: std::cell::Cell interior-mutability field, not the mesh sense
    /// How many bodies `Session::colocated_npcs`'s loop iterated this turn.
    pub bodies_scanned: std::cell::Cell<u32>, // lexicon: std::cell::Cell interior-mutability field, not the mesh sense
}

/// A copy of [`TurnWork`]'s counters as plain numbers, for a caller to read
/// after [`crate::Session::handle`] or [`crate::Session::snapshot`] without
/// touching the interior-mutable fields directly.
/// type-audit: bare-ok(count: affect_folds), bare-ok(count: position_folds), bare-ok(count: shadowcasts), bare-ok(count: plan_searches), bare-ok(count: bodies_scanned)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TurnWorkRead {
    /// See [`TurnWork::affect_folds`].
    pub affect_folds: u32,
    /// See [`TurnWork::position_folds`].
    pub position_folds: u32,
    /// See [`TurnWork::shadowcasts`].
    pub shadowcasts: u32,
    /// See [`TurnWork::plan_searches`].
    pub plan_searches: u64,
    /// See [`TurnWork::bodies_scanned`].
    pub bodies_scanned: u32,
}

impl TurnWork {
    /// Zero every fold/shadowcast/scan counter and anchor `plan_searches`
    /// against `searches_now` — the moment this turn's work starts being
    /// counted, since the underlying cache is monotone and never resets on
    /// its own.
    /// type-audit: bare-ok(count: searches_now)
    pub fn reset(&self, searches_now: u64) {
        self.affect_folds.set(0);
        self.position_folds.set(0);
        self.shadowcasts.set(0);
        self.plan_searches.set(searches_now);
        self.bodies_scanned.set(0);
    }

    /// The counters as of `searches_now`, with `plan_searches` folded down
    /// to the span of real searches since the last [`Self::reset`] rather
    /// than the cache's own lifetime total.
    /// type-audit: bare-ok(count: searches_now)
    pub fn read(&self, searches_now: u64) -> TurnWorkRead {
        TurnWorkRead {
            affect_folds: self.affect_folds.get(),
            position_folds: self.position_folds.get(),
            shadowcasts: self.shadowcasts.get(),
            plan_searches: searches_now.saturating_sub(self.plan_searches.get()),
            bodies_scanned: self.bodies_scanned.get(),
        }
    }

    /// Increment `affect_folds` by 1. One small `bump_*` method per counter
    /// (rather than a single helper taking the field by reference) so no
    /// signature here has to spell out the interior-mutable field's own
    /// type — that stays confined to the struct's field declarations above,
    /// each waived on its own line. Not `pub`: only this crate's own
    /// instrumented call sites bump a counter, never an outside caller.
    pub(crate) fn bump_affect_folds(&self) {
        self.affect_folds.set(self.affect_folds.get() + 1);
    }

    /// Increment `position_folds` by 1. See [`Self::bump_affect_folds`].
    pub(crate) fn bump_position_folds(&self) {
        self.position_folds.set(self.position_folds.get() + 1);
    }

    /// Increment `shadowcasts` by 1. See [`Self::bump_affect_folds`].
    pub(crate) fn bump_shadowcasts(&self) {
        self.shadowcasts.set(self.shadowcasts.get() + 1);
    }

    /// Increment `bodies_scanned` by 1. See [`Self::bump_affect_folds`].
    pub(crate) fn bump_bodies_scanned(&self) {
        self.bodies_scanned.set(self.bodies_scanned.get() + 1);
    }
}

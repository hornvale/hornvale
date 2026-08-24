//! An incremental fold over a ledger prefix — a derived value that ADVANCES
//! with the log rather than being invalidated by it.
//!
//! # Why this is not [`crate::derived::Derived`]
//!
//! `Derived` with [`crate::derived::Validity::Ledger`] is a MEMO: an entry goes
//! stale once a fact touching a watched `DepKey` commits after the entry's
//! recorded position, and a stale entry is evicted on read and recomputed from
//! scratch. That is the right shape for a value read far more often than its
//! dependencies change.
//!
//! It is the wrong shape — structurally, not marginally — for a fold whose
//! dependency is touched on **every** tick. The motivating case is
//! `windows/vessel`'s drive stack: a fold over one agent's `agent-at` history
//! watches `(that agent, agent-at)`, and the tick commits exactly that every
//! time the agent moves. So the entry would be stale every tick, every read
//! would be a miss, and the recomputation would be the whole-history walk the
//! cache existed to avoid — a cache with a structurally guaranteed 100% miss
//! rate.
//!
//! **Invalidation and accumulation are different operations, not two policies
//! over one operation.** That is why this is a sibling module rather than a
//! third [`crate::derived::Validity`] variant: there is nowhere in `Derived` to
//! put an update function, and adding one would change what the type is.
//!
//! # The state IS the fold
//!
//! [`LedgerFold`] is implemented by the accumulated state itself, not by a
//! marker type with an associated `State`. That keeps [`Folded`] free of a
//! `PhantomData`, and keeps a tenant's code reading as "my state, plus how it
//! absorbs a fact" rather than as two types that must be kept in step.
//!
//! # The obligations a tenant owes
//!
//! - **Commit order.** [`Folded::absorb_at`] takes the position the caller
//!   believes it is at and asserts it matches. Absorbing one fact twice, and
//!   skipping one, are the two bugs this primitive can have, and both are
//!   otherwise silent — so the guard is an `assert!`, not a `debug_assert!`.
//! - **FOLD equals SCAN.** Advancing incrementally must equal folding the same
//!   prefix from scratch. The primitive cannot prove this of a tenant's
//!   `absorb` — one that read a clock, or that depended on how many facts
//!   arrived in a batch, would break it — so every tenant owes the property
//!   test. [`Folded::rebuild`] exists to make writing it cheap. **The oracle
//!   must reach the state through a path independent of the one under test.**
//!   [`Folded::rebuild`] itself is implemented by calling [`Folded::advance_to`],
//!   so a test that only ever drives production usage through `advance_to` and
//!   then compares it against `rebuild` is comparing `advance_to` against
//!   itself: a bug confined to `advance_to` is applied identically to both
//!   sides and cancels. Pair [`Folded::absorb_at`]'s per-fact path against
//!   `rebuild` instead — see the sibling tests in `kernel/tests/suite/fold.rs`
//!   for a worked example of the vacuous comparison and the independent one
//!   side by side.
//! - **Determinism.** A state holding a `HashMap` would put an unstable
//!   iteration order under a byte-identity guarantee. `clippy.toml`'s
//!   `disallowed-types` already refuses that workspace-wide; it is restated
//!   here because a fold state is exactly where someone would reach for one.
//!
//! # What this does NOT do
//!
//! It does not go backwards. A fold is a one-directional accumulation, so a
//! query about an earlier position is served by [`Folded::rebuild_upto`], which
//! is O(position). A tenant with a natural checkpoint — a reset event returning
//! the state to a known value, such as a `drank` fact zeroing a thirst
//! integral — can do far better by pairing [`Folded::resume`] with a narrowed
//! range. That choice is the tenant's, because only the tenant knows where its
//! checkpoints are.

use crate::ledger::{Fact, Ledger};

/// A value accumulated by absorbing a ledger's facts in commit order.
///
/// Implemented by the state itself. `PartialEq` and `Debug` are required
/// because every tenant owes a FOLD-equals-SCAN property test, and that test
/// needs to compare two states and print them when they differ.
pub trait LedgerFold: Sized + PartialEq + core::fmt::Debug {
    /// The state before any fact has been absorbed — the fold's identity
    /// element.
    fn empty() -> Self;

    /// Absorb one fact. Called once per fact, in commit order.
    ///
    /// Must be a pure function of `(self, fact)`. In particular it must not
    /// depend on how many facts arrive in a batch, or on anything outside the
    /// ledger, or FOLD equals SCAN does not hold.
    fn absorb(&mut self, fact: &Fact);
}

/// A [`LedgerFold`] state together with the ledger position it is valid at.
#[derive(Debug, PartialEq)]
pub struct Folded<S: LedgerFold> {
    state: S,
    position: u64,
}

impl<S: LedgerFold> Folded<S> {
    /// An empty fold, valid at position 0.
    pub fn new() -> Self {
        Folded {
            state: S::empty(),
            position: 0,
        }
    }

    /// Resume from a state known to be valid at `position` — the checkpoint
    /// seam. The caller asserts that `state` is exactly what folding the
    /// ledger's first `position` facts would produce; nothing here can check
    /// that, which is why it is the tenant's obligation and not this type's.
    /// type-audit: bare-ok(count: position)
    pub fn resume(state: S, position: u64) -> Self {
        Folded { state, position }
    }

    /// The accumulated state, valid as of [`Self::position`].
    pub fn state(&self) -> &S {
        &self.state
    }

    /// The number of facts absorbed — equivalently, the length of the ledger
    /// prefix this state folds.
    /// type-audit: bare-ok(count: return)
    pub fn position(&self) -> u64 {
        self.position
    }

    /// Absorb the fact at `position`, which must be the position this fold is
    /// currently at.
    ///
    /// The O(1) door, for a caller already holding the fact it is about to
    /// commit — which is the tick's situation: the drive systems return their
    /// facts before the ledger appends them, so nothing needs to scan.
    ///
    /// # Panics
    ///
    /// If `position` is not [`Self::position`]. Absorbing twice and skipping
    /// are the only two bugs available here and both corrupt the state
    /// silently, so this is an `assert!` in every build, not a
    /// `debug_assert!`.
    /// type-audit: bare-ok(count: position)
    pub fn absorb_at(&mut self, position: u64, fact: &Fact) {
        assert_eq!(
            position, self.position,
            "a fold must absorb each fact exactly once, in commit order"
        );
        self.state.absorb(fact);
        self.position += 1;
    }

    /// Absorb every fact `ledger` has gained since [`Self::position`].
    ///
    /// The catch-up door, for a caller holding a ledger rather than the
    /// individual facts. A ledger shorter than this fold's position absorbs
    /// nothing: an append-only log cannot shrink, so that means the caller
    /// passed a different ledger, and folding nothing is a better failure than
    /// folding a prefix of the wrong log.
    pub fn advance_to(&mut self, ledger: &Ledger) {
        for fact in ledger.iter().skip(self.position as usize) {
            self.state.absorb(fact);
            self.position += 1;
        }
    }

    /// Fold `ledger` from scratch — the SCAN half of FOLD equals SCAN, and the
    /// reference every tenant's property test compares against.
    pub fn rebuild(ledger: &Ledger) -> Self {
        let mut folded = Self::new();
        folded.advance_to(ledger);
        folded
    }

    /// Fold only the ledger's first `position` facts — the general answer to a
    /// query about an earlier position, at O(position).
    ///
    /// A `position` beyond the ledger's length is not treated as a caller
    /// error the way [`Self::absorb_at`]'s mismatch is: `take` silently folds
    /// however many facts actually exist and stops there. Silence is the
    /// right answer here, unlike `absorb_at`'s loud assert, because the two
    /// guards are protecting against different things. `absorb_at`'s position
    /// argument is the caller's claim about *this fold's own state* — a
    /// mismatch means the caller skipped or repeated a fact, which is always a
    /// bug. `rebuild_upto`'s `position` argument is a claim about *the
    /// ledger* — "fold up to here" — and an append-only ledger can validly be
    /// shorter than a query asked of it (the facts have not been committed
    /// yet). Truncating is therefore a legitimate answer, not a corrupted one,
    /// and it is never silent in the way that matters: the returned
    /// [`Self::position`] honestly reports how far the fold actually got, so a
    /// caller that cares can compare it against the `position` it asked for.
    /// type-audit: bare-ok(count: position)
    pub fn rebuild_upto(ledger: &Ledger, position: u64) -> Self {
        let mut folded = Self::new();
        for fact in ledger.iter().take(position as usize) {
            folded.state.absorb(fact);
            folded.position += 1;
        }
        folded
    }
}

impl<S: LedgerFold> Default for Folded<S> {
    fn default() -> Self {
        Self::new()
    }
}

//! The topological relation vocabulary — the Region Connection Calculus
//! (RCC-8), borrowed rather than invented (spec §5). Its eight relations are
//! JOINTLY EXHAUSTIVE and PAIRWISE DISJOINT over region pairs: for any two
//! anchors exactly one holds, which is what structurally prevents the catalogue
//! sprawl a hand-rolled vocabulary invites — a partition cannot be padded.

/// One of the eight RCC-8 relations between two anchor regions. Exactly one
/// holds for any ordered pair (JEPD).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rcc8 {
    /// Disconnected — no contact. The PRIVACY primitive (spec §5).
    Dc,
    /// Externally connected — touching, no shared interior (adjacency).
    Ec,
    /// Partial overlap — shared interior, neither contains the other.
    Po,
    /// Tangential proper part — inside and touching the boundary.
    Tpp,
    /// The converse of [`Rcc8::Tpp`] — tangentially contains.
    TppI,
    /// Non-tangential proper part — strictly inside (the containment case).
    Ntpp,
    /// The converse of [`Rcc8::Ntpp`] — strictly contains.
    NtppI,
    /// Equal — the same region.
    Eq,
}

impl Rcc8 {
    /// Every relation, in a fixed order (the JEPD partition, enumerated).
    pub const ALL: [Rcc8; 8] = [
        Rcc8::Dc,
        Rcc8::Ec,
        Rcc8::Po,
        Rcc8::Tpp,
        Rcc8::TppI,
        Rcc8::Ntpp,
        Rcc8::NtppI,
        Rcc8::Eq,
    ];
}

/// The converse: `converse(r)` holds of `(b, a)` exactly when `r` holds of
/// `(a, b)`. An involution.
pub fn converse(r: Rcc8) -> Rcc8 {
    match r {
        Rcc8::Tpp => Rcc8::TppI,
        Rcc8::TppI => Rcc8::Tpp,
        Rcc8::Ntpp => Rcc8::NtppI,
        Rcc8::NtppI => Rcc8::Ntpp,
        // DC, EC, PO and EQ are symmetric — their own converse.
        other => other,
    }
}

/// Whether `r` is symmetric — equivalently, whether it is its own converse.
/// Derived from [`converse`] rather than declared a second time, so the two can
/// never drift.
/// type-audit: bare-ok(flag: return)
pub fn is_symmetric(r: Rcc8) -> bool {
    converse(r) == r
}

/// Whether `r` is transitive. Only the containment relations and equality are.
/// type-audit: bare-ok(flag: return)
pub fn is_transitive(r: Rcc8) -> bool {
    matches!(
        r,
        Rcc8::Ntpp | Rcc8::NtppI | Rcc8::Tpp | Rcc8::TppI | Rcc8::Eq
    )
}

/// The composition table: the relations that may hold between `a` and `c` given
/// `a ρ b` and `b σ c`.
///
/// v1 implements only the entries it USES — containment transitivity and the
/// identity — and returns the UNIVERSAL set for the rest. That is a correct
/// over-approximation (sound but imprecise), deliberately chosen over a
/// plausible-looking guess: an entry taken from the published table is knowledge,
/// an entry invented here would be a bug wearing knowledge's clothes. Filling in
/// the remaining entries is reserved.
pub fn compose(a: Rcc8, b: Rcc8) -> std::collections::BTreeSet<Rcc8> {
    let one = |r: Rcc8| [r].into_iter().collect();
    match (a, b) {
        // EQ is the identity of composition, on both sides.
        (Rcc8::Eq, r) | (r, Rcc8::Eq) => one(r),
        // Containment is transitive (published RCC-8).
        (Rcc8::Ntpp, Rcc8::Ntpp) => one(Rcc8::Ntpp),
        (Rcc8::NtppI, Rcc8::NtppI) => one(Rcc8::NtppI),
        // Not yet taken from the table: every relation remains possible.
        _ => Rcc8::ALL.into_iter().collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn converse_is_an_involution_and_pairs_the_inverses() {
        for r in Rcc8::ALL {
            assert_eq!(
                converse(converse(r)),
                r,
                "converse is its own inverse: {r:?}"
            );
        }
        assert_eq!(converse(Rcc8::Ntpp), Rcc8::NtppI);
        assert_eq!(converse(Rcc8::Tpp), Rcc8::TppI);
        // The symmetric relations are their own converse.
        for r in [Rcc8::Dc, Rcc8::Ec, Rcc8::Po, Rcc8::Eq] {
            assert_eq!(converse(r), r, "{r:?} is symmetric");
        }
    }

    #[test]
    fn symmetry_declarations_agree_with_converse() {
        // ONE SOURCE OF TRUTH: a relation is symmetric exactly when it is its
        // own converse. The declaration must not drift from the table.
        for r in Rcc8::ALL {
            assert_eq!(
                is_symmetric(r),
                converse(r) == r,
                "symmetry declaration disagrees with converse for {r:?}"
            );
        }
    }

    #[test]
    fn containment_composes_transitively() {
        // The one composition v1 actually uses: a hearth inside an alcove inside
        // a hall is inside the hall. Published RCC-8: NTPP ∘ NTPP = {NTPP}.
        assert_eq!(
            compose(Rcc8::Ntpp, Rcc8::Ntpp),
            [Rcc8::Ntpp]
                .into_iter()
                .collect::<std::collections::BTreeSet<_>>()
        );
        assert!(is_transitive(Rcc8::Ntpp));
        // EQ is the identity of composition.
        for r in Rcc8::ALL {
            assert_eq!(
                compose(Rcc8::Eq, r),
                [r].into_iter().collect::<std::collections::BTreeSet<_>>(),
                "EQ ∘ {r:?} = {{{r:?}}}"
            );
        }
    }

    #[test]
    fn unimplemented_compositions_return_the_universal_set_not_a_guess() {
        // Soundness over precision: an entry we have not taken from the
        // published table returns EVERY relation (a correct over-approximation),
        // never a plausible-looking single answer.
        let all: std::collections::BTreeSet<Rcc8> = Rcc8::ALL.into_iter().collect();
        assert_eq!(compose(Rcc8::Po, Rcc8::Ec), all);
    }
}

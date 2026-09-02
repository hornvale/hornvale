//! The epistemic grade a claim carries: how its holder came to hold it.
//!
//! This is the *fact* of provenance. Whether a tongue can grammatically
//! express it is a separate, language-domain question — see
//! `domains/language`'s `Evidential` and the frontier row
//! `KNOW-evidential-invisibility`, which turns on exactly that split.

/// How a holder came to hold a claim. Ordered by epistemic strength, and
/// transmission only ever moves DOWN it (see [`Provenance::on_transmission`]).
/// type-audit: bare-ok(identifier-text)
/// placement: deliberate(epistemic fact vs grammatical category — sharing
/// variant names is a coincidence of English; hearsay design 2026-08-13 §3.2)
/// shape(49883f)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Provenance {
    /// The holder was present at the event.
    Witnessed,
    /// The holder was told, by someone who held it on some other grade.
    Taught,
    /// The holder derived it from other claims rather than being told.
    Inferred,
}

impl Provenance {
    /// The grade a hearer receives when a holder of `self` tells them.
    ///
    /// `Witnessed` becomes `Taught`: being told that someone saw a thing is
    /// not seeing it. The reverse transition does not exist, and that
    /// anti-symmetry is what makes a rumour decay across retellings instead
    /// of strengthening — see the frontier row `SOC-reputation-provenance`.
    pub fn on_transmission(self) -> Provenance {
        match self {
            Provenance::Witnessed => Provenance::Taught,
            other => other,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn witnessing_downgrades_to_taught_when_retold() {
        assert_eq!(Provenance::Witnessed.on_transmission(), Provenance::Taught);
    }

    #[test]
    fn retelling_a_taught_claim_stays_taught() {
        assert_eq!(Provenance::Taught.on_transmission(), Provenance::Taught);
    }

    #[test]
    fn retelling_an_inference_stays_inferred() {
        assert_eq!(Provenance::Inferred.on_transmission(), Provenance::Inferred);
    }

    #[test]
    fn no_grade_ever_transmits_up_to_witnessed() {
        for g in [
            Provenance::Witnessed,
            Provenance::Taught,
            Provenance::Inferred,
        ] {
            let after = g.on_transmission();
            assert!(
                after != Provenance::Witnessed || g == Provenance::Witnessed,
                "{g:?} transmitted UP to Witnessed; hearsay is never testimony"
            );
        }
    }
}

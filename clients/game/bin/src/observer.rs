//! The terminal as an observer (spec §4.2). RED STATE: implementation not
//! yet written; this file exists only to confirm the tests fail to compile
//! before Step 3.

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::color::{BANDS, Illuminant, Reflectance, standard_observer};

    fn a_signal() -> hornvale_kernel::color::Signal {
        let r = Reflectance::new([0.5; BANDS]).expect("in range");
        let i = Illuminant::new([1.0; BANDS]).expect("in range");
        standard_observer().sense(&r, &i)
    }

    /// FIRES WHEN: NO_COLOR stops meaning "emit no colour". The glyph is
    /// what carries elevation, so a None here must never be treated as an
    /// error by the caller.
    #[test]
    fn the_none_observer_renders_no_colour_at_all() {
        let t = TerminalObserver::new(ColorDepth::None);
        assert_eq!(t.render(&a_signal()), None);
    }

    /// The non-vacuity arm: a truecolor terminal must render SOMETHING, or
    /// the assertion above would hold for a render() that always returns
    /// None.
    #[test]
    fn a_truecolor_observer_renders_a_triple() {
        let t = TerminalObserver::new(ColorDepth::TrueColor);
        assert!(t.render(&a_signal()).is_some());
    }

    /// FIRES WHEN: a reduced palette stops reducing. 16-colour output must
    /// land on one of the 16, which we check by counting distinct outputs
    /// over a spread of signals rather than by naming a palette entry.
    #[test]
    fn the_sixteen_colour_observer_emits_at_most_sixteen_distinct_triples() {
        let t = TerminalObserver::new(ColorDepth::Palette16);
        let mut seen = std::collections::BTreeSet::new();
        for k in 0..64u32 {
            let v = f64::from(k) / 64.0;
            let r = Reflectance::new([v; BANDS]).expect("in range");
            let i = Illuminant::new([1.0; BANDS]).expect("in range");
            let s = standard_observer().sense(&r, &i);
            if let Some(rgb) = t.render(&s) {
                seen.insert(rgb);
            }
        }
        assert!(
            seen.len() <= 16,
            "16-colour observer emitted {} distinct triples",
            seen.len()
        );
        assert!(seen.len() > 1, "it must still discriminate; got {}", seen.len());
    }
}

//! The terminal as an observer (spec §4.2).
//!
//! `NO_COLOR`, a 16-colour terminal and a truecolor one are not three
//! special cases — they are three observers over one pipeline, exactly as
//! `windows/vessel/src/eyes.rs` resolves an observer per creature. Decision
//! 0389 says nothing a reader must trust may live only in colour; here the
//! glyph carries elevation, so `None` is a legal render and never an error.

use hornvale_kernel::color::{Illuminant, Observer, Reflectance, Signal, standard_observer};

/// What this terminal can display.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorDepth {
    /// 24-bit colour.
    TrueColor,
    /// The 256-colour cube.
    Palette256,
    /// The eight base ANSI colours (2 levels per channel = 2^3 = 8). Named
    /// for the ANSI 16-colour palette this terminal class supports, but the
    /// bright variants are unused — the quantization only reaches the base
    /// eight.
    Palette16,
    /// No colour at all (`NO_COLOR`, or a dumb terminal).
    None,
}

/// The observer this terminal is, and the quantization it owns.
pub struct TerminalObserver {
    depth: ColorDepth,
    observer: Observer,
}

impl TerminalObserver {
    /// Build the observer for a given display depth.
    pub fn new(depth: ColorDepth) -> Self {
        Self {
            depth,
            observer: standard_observer(),
        }
    }

    /// The depth this observer was built for.
    pub fn depth(&self) -> ColorDepth {
        self.depth
    }

    /// Collapse a signal to what this terminal can show. `None` means this
    /// terminal shows no colour; the caller still emits the glyph.
    pub fn render(&self, signal: &Signal) -> Option<[u8; 3]> {
        let full = self.observer.to_srgb(signal)?;
        self.show(full)
    }

    /// The whole collapse, from the ground's own curve and the light falling
    /// on it to what this terminal can show — [`Observer::sense`] followed by
    /// [`Self::render`], through this observer's OWN
    /// [`standard_observer`] rather than a second one built at the call site.
    ///
    /// **Named because the plate calls it ~1,920 times a frame** (The Wash,
    /// Task 6). Spelling it `standard_observer().sense(r, i)` in `plate.rs`
    /// would build a fresh [`Observer`] — four spectra — per drawn tile, to
    /// reach the one this struct already holds.
    pub fn observe(&self, reflectance: &Reflectance, illuminant: &Illuminant) -> Option<[u8; 3]> {
        self.render(&self.observer.sense(reflectance, illuminant))
    }

    /// Put an ALREADY-sRGB colour through this terminal's own depth — the
    /// half of [`Self::render`] below the observer.
    ///
    /// **Why this exists (The Wash, Task 6).** The plate's water classes
    /// (`plate::OCEAN_COLOR`, `plate::SALT_BASIN_COLOR`) are invented
    /// client-side palette claims, not spectra: there is no reflectance to
    /// sense for the surface of an ocean, whose facet reflectance describes
    /// the GROUND cover under it. Routing them through here rather than
    /// emitting them raw keeps ONE rule — "the observer decides what colour
    /// reaches this terminal" — instead of two, so a `ColorDepth::None`
    /// terminal emits no colour on ANY terrain tile, which is what spec H5
    /// actually claims.
    pub fn show(&self, rgb: [u8; 3]) -> Option<[u8; 3]> {
        match self.depth {
            ColorDepth::None => None,
            ColorDepth::TrueColor => Some(rgb),
            ColorDepth::Palette256 => Some(quantize_channels(rgb, 6)),
            ColorDepth::Palette16 => Some(quantize_channels(rgb, 2)),
        }
    }
}

/// The observer THIS terminal is, from the one `NO_COLOR` answer the client
/// already resolves ([`crate::plate::colour_allowed`]).
///
/// **The mapping is deliberately two-valued, and that is a statement about
/// what the client can currently probe, not about what the pipeline
/// supports.** [`ColorDepth`] carries four rungs and
/// [`TerminalObserver::render`] honours all four; nothing in this client
/// probes `COLORTERM` or the terminfo database, so the only honest answer
/// available is the one `NO_COLOR` gives. Adding a real depth probe changes
/// this function and nothing else.
pub fn terminal_observer(colour_allowed: bool) -> TerminalObserver {
    TerminalObserver::new(if colour_allowed {
        ColorDepth::TrueColor
    } else {
        ColorDepth::None
    })
}

/// Snap each channel to `levels` evenly spaced values. `levels` is the count
/// per channel, so 2 gives 8 combinations and 6 gives 216 — the classic
/// 6x6x6 cube.
fn quantize_channels(rgb: [u8; 3], levels: u32) -> [u8; 3] {
    let n = levels.max(2) - 1;
    let mut out = [0u8; 3];
    for (i, c) in rgb.iter().enumerate() {
        let step = f64::from(*c) / 255.0 * f64::from(n);
        let snapped = step.round() / f64::from(n) * 255.0;
        out[i] = snapped as u8;
    }
    out
}

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
        assert!(
            seen.len() > 1,
            "it must still discriminate; got {}",
            seen.len()
        );
    }
}

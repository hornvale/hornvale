//! The cell buffer: the one artifact every backend renders and no backend
//! owns.

use std::collections::BTreeMap;

/// How much a thing commands the character's notice right now.
///
/// The brief's most important channel, because it is the only one surviving
/// monochrome, a 16-colour terminal and colour-blindness intact. Note it runs
/// OPPOSITE to durability: `here` lasts one turn and is Bold, `remembered`
/// lasts forever and is Dim. Bold means perishable, not important.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Weight {
    /// Drawn from memory: lighter stroke.
    Dim,
    /// Inked, confident, present tense.
    #[default]
    Normal,
    /// Fresh ink; the most present thing on the page.
    Bold,
}

/// What a thing IS. `Plain` is the floor; `Rgb` carries substance off the
/// wire. **Foreground = cover, background = substrate**: if a future
/// campaign adds background colour (`Ink::Duo { fg, bg }`), fg carries what
/// grows/sits on a cell and bg the material under it — both claims of
/// substance per CLIENT-four-channels, never identity or attention.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Ink {
    /// The default ink.
    #[default]
    Plain,
    /// A truecolor foreground claim carried off the wire.
    Rgb([u8; 3]),
}

impl Ink {
    /// The pure decision behind [`Ink::from_wire`]: with colour disallowed,
    /// or with no colour claimed, the ink is [`Ink::Plain`] — absence is
    /// legible, never faked as black. Tests that need the coloured path to
    /// be hermetic call this directly instead of touching the environment.
    pub fn resolve(color: Option<[u8; 3]>, colour_allowed: bool) -> Ink {
        if !colour_allowed || color.is_none() {
            return Ink::Plain;
        }
        color.map(Ink::Rgb).unwrap_or(Ink::Plain)
    }

    /// Resolve a wire colour claim to ink. A non-empty `NO_COLOR` (the
    /// reader declined colour) and a `None` claim both yield
    /// [`Ink::Plain`]; see [`Ink::resolve`] for the pure decision.
    pub fn from_wire(color: Option<[u8; 3]>) -> Ink {
        Self::resolve(
            color,
            std::env::var_os("NO_COLOR").is_none_or(|v| v.is_empty()),
        )
    }
}

/// The snapshot channel a drawn cell traces back to — the executable form of
/// the brief's "trace listing: every visible datum on the composed screen,
/// and which channel from the inventory it came from." For every variant
/// except [`Source::Look`], [`Source::Typed`], and [`Source::Echo`],
/// "traces back to" is true *by construction*: this crate parsed the value
/// off a `Snapshot` field before ever drawing it, so the provenance claim is
/// one the crate can verify against its own parse.
///
/// Every drawn cell must name one of these. The brief's original two
/// categories were **derived from world state** (every variant below except
/// [`Source::Chrome`]) and **declared inert** ([`Source::Chrome`], and
/// nothing else) — that was a complete accounting when the client only ever
/// showed prose sent back from the sim. Task 2 (The Stylus) gave the client
/// a THIRD category, because the client became typeable: text authored at
/// this terminal, never on the wire — `bin` owns it and this crate cannot
/// verify it. [`Source::Typed`] (the live, unsent buffer) and
/// [`Source::Echo`] (the most recently submitted line) are its two members;
/// see each variant's own doc for what distinguishes them. [`Source::Look`]
/// sits in the world-derived category by claim, not by construction — it is
/// a third variant whose provenance this crate cannot itself check; see its
/// doc for what that means and the caller discipline it rests on.
///
/// There is deliberately **no `Social` variant**. `hornvale-game-core`'s own
/// schema mirror omits the `social` channel entirely (see `schema.rs`'s
/// module doc), so no cell in this crate can ever be attributed to it — the
/// redaction is enforced by the type system here, restating the guarantee
/// `tests/schema.rs` already proves at the parse layer. Do not add a
/// `Social` variant "for completeness"; its absence is the mechanism.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Source {
    /// No draw path has claimed this cell yet. A drawn cell (`glyph.is_some()`)
    /// left at this value is a datum nobody can justify — see
    /// `tests/provenance.rs`'s `every_drawn_cell_names_its_channel`.
    #[default]
    Unattributed,
    /// The walk-band chart (`scene/surrounds/v2`, drawn by `chart.rs`).
    Chart,
    /// The chamber-band floor plan (`vessel/plan/v1`, drawn by `plan.rs`).
    Plan,
    /// The entry's wrapped narration prose (`Narration::prose`, drawn by
    /// `entry.rs`), including its truncation marker when the prose overflows
    /// — the marker is an honest signal about that same channel, not an
    /// invented one.
    Prose,
    /// The endpaper identity strip (`SelfChannel`, plus `day`/`turn`, drawn
    /// by `endpaper.rs`).
    Identity,
    /// Declared-inert decoration: rules, gutters, margins, and the entry's
    /// `>` command-line prompt (`entry.rs`). Never a dumping ground for a
    /// cell whose real channel was merely inconvenient to name — the prompt
    /// qualifies honestly: `PROMPT_GLYPH` is a hardcoded constant, not
    /// derived from any wire field, so it is UI chrome, not a datum.
    ///
    /// **There is deliberately no `WaysOn` variant, on its second deletion
    /// for two different reasons.** Task 9's review first deleted it (it had
    /// been assigned to this same prompt cell) as a false provenance claim:
    /// there was no `ways_on` field on `vessel/session/v2`, so the category
    /// named a channel the wire never separately emitted. Task 9b restored
    /// it on better information — `sensed.room.exits` really is on the wire
    /// — and gave it its own always-visible row (`ways.rs`), re-deriving the
    /// walk band's compass filter and the chamber band's `at + 1 < of`
    /// path-graph invariant from the mirrored fields rather than reading
    /// them off the sim. That row shipped a real bug: underground, it
    /// disagreed with the room's own prose, because the derivation read
    /// `sensed.room.exits` (the outdoor locale, unchanged by stepping
    /// indoors) while the prose was built by the sim, which knows which band
    /// it is in. The Quire (task 9d) deleted it a second time, for the true
    /// reason: the sim already states "Ways on: …" in `Narration::prose`,
    /// correctly in every band, one line above where the row used to sit —
    /// so the row's entire job was to reproduce a conclusion the sim had
    /// already reached, which is the defect this crate exists to avoid
    /// regardless of whether the reproduction happens to agree today. If a
    /// future need requires ways-on as a separately addressable UI element,
    /// that need should be met by parsing the sim's authoritative sentence,
    /// not by re-deriving it from lower-level fields a second time.
    Chrome,
    /// The map strip beneath the plate (`strip.rs`): the resolved feature
    /// name under the free-roaming cursor. Deliberately **not**
    /// [`Source::Chrome`] — the strip's content, once a name is resolved
    /// (The Portolan's Task 3), is drawn from world state the same as the
    /// chart or the floor plan is, so lumping it into `Chrome` would be the
    /// same false-provenance mistake the deleted `WaysOn` variant made in
    /// reverse: `Chrome` is reserved for genuinely inert decoration (rules,
    /// gutters, margins, the entry's own prompt), never a catch-all for
    /// content whose real channel was merely inconvenient to name.
    ///
    /// **This is not a snapshot channel, and — like [`Source::Typed`] and
    /// [`Source::Echo`], its two siblings in that respect — its provenance
    /// is not verifiable by construction.** `Chart`, `Plan`, `Prose`, and
    /// `Identity` each trace to a field this crate itself parsed off
    /// `Snapshot` — the crate can point at the exact struct field that
    /// justifies the label. `Look`'s text does not: it traces to
    /// `resolve_at` in `windows/worldgen`, answered by a `CellFeatureIndex`
    /// over `domains/terrain` and queried live by `bin` (Task 3) — it is
    /// not carried on `vessel/session/v2` at all, so there is no `Snapshot`
    /// field for this crate to check the label against. Structurally, the
    /// string handed to `strip::draw` is as opaque to `hornvale-game-core`
    /// as `Chrome`'s hardcoded glyph: an input this crate cannot itself
    /// verify. So `Look`'s honesty is a **caller discipline**, not a
    /// crate-enforced guarantee — `bin` must only ever pass a genuinely
    /// resolved feature name into the strip parameter, never a hardcoded
    /// hint or placeholder, or the label becomes exactly the false claim
    /// `Chrome` is reserved to avoid.
    Look,
    /// The player's own unsent keystrokes: the command line's buffer text,
    /// drawn by `entry.rs` after the `>` prompt. **Deliberately not
    /// [`Source::Chrome`]**, even though both live on the same command row
    /// — `Chrome` is reserved for genuinely inert decoration, and this text
    /// is neither inert (it changes on every keystroke the player types)
    /// nor a hardcoded constant the way `PROMPT_GLYPH` is. **Also
    /// deliberately not [`Source::Prose`]**: the buffer has not been sent to
    /// `Session::handle` yet, so it is not narration the sim produced —
    /// attributing it to `Prose` would claim a wire provenance nothing on
    /// `vessel/session/v2` backs. It is one of the two members of the third
    /// category the enum's own doc now names — text authored at this
    /// terminal, never on the wire — and is distinguished from its sibling
    /// [`Source::Echo`] by not yet having been sent: the buffer is live,
    /// unsent, and mutates on every keystroke, where `Echo` is a fixed
    /// record of a line already submitted.
    Typed,
    /// The most recently SUBMITTED line, echoed above the command row so
    /// the page reads ask-then-answer (spec §6, The Stylus Task 3).
    /// **Deliberately not [`Source::Typed`]**: unlike the live buffer, this
    /// text HAS already been sent to `Session::handle`, so `Typed`'s own
    /// "not sent yet" reasoning no longer holds once a line is echoed.
    /// **Also deliberately not [`Source::Prose`]**: it is the player's own
    /// composed line, not narration the sim produced — attributing it to
    /// `Prose` would claim a wire provenance nothing on `vessel/session/v2`
    /// backs (the echo is `bin`'s own record of what it sent, held
    /// alongside `Driver`, never a field read off a `Snapshot`).
    Echo,
}

/// One character cell. A tile is a drop-in replacement for exactly one of
/// these: same box, same metrics, same position (the brief's cell law).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cell {
    /// The character drawn, or `None` for unmarked paper.
    pub glyph: Option<char>,
    /// Attention.
    pub weight: Weight,
    /// Substance.
    pub ink: Ink,
    /// Which snapshot channel this cell traces back to.
    pub source: Source,
}

impl Default for Cell {
    fn default() -> Self {
        Cell {
            glyph: None,
            weight: Weight::Normal,
            ink: Ink::Plain,
            source: Source::Unattributed,
        }
    }
}

impl Cell {
    /// A drawn cell, attributed to the snapshot channel it came from.
    pub fn glyph(glyph: char, weight: Weight, source: Source) -> Cell {
        Cell {
            glyph: Some(glyph),
            weight,
            ink: Ink::Plain,
            source,
        }
    }

    /// A cell with a colour claim resolved through [`Ink::from_wire`].
    pub fn inked(glyph: char, weight: Weight, source: Source, color: Option<[u8; 3]>) -> Cell {
        Cell {
            glyph: Some(glyph),
            weight,
            ink: Ink::from_wire(color),
            source,
        }
    }

    /// Unmarked paper — never known, never drawn. Not a space, and not black.
    pub fn is_blank(&self) -> bool {
        self.glyph.is_none()
    }
}

/// A dense character grid. `Vec`, not a map — dense-index storage, matching
/// the kernel's discipline even though this tree is not bound by it.
#[derive(Debug, Clone)]
pub struct Grid {
    width: u16,
    height: u16,
    cells: Vec<Cell>,
}

impl Grid {
    /// A fresh grid of unmarked paper: every cell is `Cell::default()`
    /// (`glyph: None`), not a drawn space.
    pub fn new(width: u16, height: u16) -> Grid {
        let count = width as usize * height as usize;
        Grid {
            width,
            height,
            cells: vec![Cell::default(); count],
        }
    }

    /// The grid's width in cells.
    pub fn width(&self) -> u16 {
        self.width
    }

    /// The grid's height in cells.
    pub fn height(&self) -> u16 {
        self.height
    }

    /// The dense index for `(x, y)`, if it lies within bounds.
    fn index(&self, x: u16, y: u16) -> Option<usize> {
        if x < self.width && y < self.height {
            Some(y as usize * self.width as usize + x as usize)
        } else {
            None
        }
    }

    /// Write `cell` at `(x, y)`. Out-of-bounds writes are silently refused —
    /// never wrapped into range, which would be a layout bug wearing a
    /// bounds check's clothes.
    pub fn set(&mut self, x: u16, y: u16, cell: Cell) {
        if let Some(i) = self.index(x, y) {
            self.cells[i] = cell;
        }
    }

    /// Read the cell at `(x, y)`, or `None` if out of bounds.
    pub fn get(&self, x: u16, y: u16) -> Option<&Cell> {
        self.index(x, y).map(|i| &self.cells[i])
    }

    /// Render the grid as plain text: unmarked paper becomes `' '` (a
    /// terminal has nothing else to write), rows are newline-separated, and
    /// there is no trailing newline.
    pub fn to_plain_text(&self) -> String {
        let mut out = String::with_capacity((self.width as usize + 1) * self.height as usize);
        for y in 0..self.height {
            if y > 0 {
                out.push('\n');
            }
            for x in 0..self.width {
                let ch = self.get(x, y).and_then(|c| c.glyph).unwrap_or(' ');
                out.push(ch);
            }
        }
        out
    }

    /// Render the grid's weight channel, ignoring glyph and ink entirely:
    /// `B` (Bold), `N` (Normal), `d` (Dim), `.` (unwritten). This is how the
    /// monochrome acceptance test reads — legible with colour switched off.
    pub fn to_weight_map(&self) -> String {
        let mut out = String::with_capacity((self.width as usize + 1) * self.height as usize);
        for y in 0..self.height {
            if y > 0 {
                out.push('\n');
            }
            for x in 0..self.width {
                let ch = match self.get(x, y) {
                    Some(c) if c.is_blank() => '.',
                    Some(c) => match c.weight {
                        Weight::Bold => 'B',
                        Weight::Normal => 'N',
                        Weight::Dim => 'd',
                    },
                    None => '.',
                };
                out.push(ch);
            }
        }
        out
    }

    /// The provenance census: how many *drawn* cells (`glyph.is_some()`)
    /// trace to each [`Source`]. Unmarked paper (a cell that was never
    /// written at all) is not counted here — a gutter that is simply never
    /// drawn into needs no source, the same discipline `Cell::is_blank`
    /// already treats as "not a space, and not black". A `BTreeMap`, not a
    /// `HashMap`, matching the kernel's deterministic-ordering discipline
    /// even though this tree is not bound by it.
    pub fn provenance(&self) -> BTreeMap<Source, usize> {
        let mut counts = BTreeMap::new();
        for cell in &self.cells {
            if !cell.is_blank() {
                *counts.entry(cell.source).or_insert(0) += 1;
            }
        }
        counts
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Absent colour means "no colour claimed here", never black — the
    /// producer's own rule (windows/vessel/src/session.rs `tint`).
    #[test]
    fn absent_wire_colour_is_plain_ink() {
        assert_eq!(Ink::from_wire(None), Ink::Plain);
    }

    /// NO_COLOR maps every Rgb to Plain at cell-build time, so the buffer
    /// itself is monochrome and degradation is observable, not silent.
    #[test]
    fn no_color_env_forces_plain_ink() {
        // SAFETY: tests run process-per-test under nextest; no other test
        // reads this var concurrently.
        unsafe { std::env::set_var("NO_COLOR", "1") };
        assert_eq!(Ink::from_wire(Some([36, 36, 1])), Ink::Plain);
        unsafe { std::env::remove_var("NO_COLOR") };
        assert_eq!(Ink::from_wire(Some([36, 36, 1])), Ink::Rgb([36, 36, 1]));
    }
}

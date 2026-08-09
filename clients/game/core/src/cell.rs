//! The cell buffer: the one artifact every backend renders and no backend
//! owns.

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

/// What a thing IS. Monochrome for this campaign; colour is deferred.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Ink {
    /// The default ink.
    #[default]
    Plain,
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
}

impl Default for Cell {
    fn default() -> Self {
        Cell {
            glyph: None,
            weight: Weight::Normal,
            ink: Ink::Plain,
        }
    }
}

impl Cell {
    /// A drawn cell.
    pub fn glyph(glyph: char, weight: Weight) -> Cell {
        Cell {
            glyph: Some(glyph),
            weight,
            ink: Ink::Plain,
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
}

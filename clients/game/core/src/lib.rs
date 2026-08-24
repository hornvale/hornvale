#![warn(missing_docs)]
//! Hornvale's game client, renderer half: `vessel/session/v2` to a
//! character grid. This crate does not depend on any hornvale crate.

pub mod cell;
pub mod chart;
pub mod endpaper;
pub mod entry;
pub mod lexicon;
pub mod plan;
pub mod schema;
pub mod spread;
pub mod strip;
pub use cell::*;
pub use lexicon::*;
pub use schema::*;

impl Snapshot {
    /// Parse an emitted `vessel/session/v2` document.
    pub fn parse(json: &str) -> Result<Snapshot, serde_json::Error> {
        serde_json::from_str(json)
    }
}

/// The minimum grid width the spread renders at. "Monochrome at 80×24 is
/// the floor" — The Quire's register: if it only works larger, it is
/// wrong, so [`render`] refuses anything smaller rather than silently
/// degrading.
pub const MIN_WIDTH: u16 = 80;

/// The minimum grid height the spread renders at. See [`MIN_WIDTH`].
pub const MIN_HEIGHT: u16 = 24;

/// Everything that can keep [`render`] from producing a [`Grid`].
#[derive(Debug)]
pub enum Error {
    /// `json` was not a valid `vessel/session/v2` document.
    Parse(serde_json::Error),
    /// The requested grid was smaller than the monochrome floor
    /// ([`MIN_WIDTH`] by [`MIN_HEIGHT`]).
    TooSmall {
        /// The width that was requested.
        w: u16,
        /// The height that was requested.
        h: u16,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse(e) => write!(f, "failed to parse vessel/session/v2: {e}"),
            Error::TooSmall { w, h } => write!(
                f,
                "grid {w}x{h} is smaller than the {MIN_WIDTH}x{MIN_HEIGHT} floor"
            ),
        }
    }
}

impl std::error::Error for Error {}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Error {
        Error::Parse(e)
    }
}

/// Which pane a key press is addressed to.
///
/// Three modes. [`Focus::Walk`] is the default and the player's primary:
/// arrows and `<`/`>` are movement commands sent to the session, any other
/// printable key bounces to the CLI and types itself — that routing will
/// live in the binary's driver (Tasks 2–3); this enum only names the
/// states. [`Focus::Cli`] is the command line; [`Focus::Map`] drives the
/// map cursor. `Esc` cycles Walk↔Cli and returns Map→Walk (the transition
/// also lives in the binary's driver).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Focus {
    /// Movement commands go straight to the session; printable keys bounce
    /// to the CLI and type themselves.
    #[default]
    Walk,
    /// The command line is listening. Text is the default destination.
    Cli,
    /// The map is listening: arrows drive the map cursor, `-`/`+`/`=` zoom.
    Map,
}

/// The command line's contents, as the renderer sees them.
///
/// A borrowed view, never ownership: the buffer lives in the binary
/// (`hornvale_game::line::Line`) and is handed over for drawing, the same
/// way [`render_with`]'s `strip` already is. This crate has no dependency
/// on any hornvale crate and gains none here — a `&str` and an index are
/// the whole contract.
#[derive(Debug, Clone, Copy, Default)]
pub struct CommandLine<'a> {
    /// What has been typed so far.
    pub text: &'a str,
    /// The insertion point, as a CHARACTER offset into `text` — never a
    /// byte offset, so a multi-byte glyph cannot split it.
    pub caret: usize,
}

/// A free-roaming cursor's screen position, in grid cells.
///
/// **The cursor is not ink.** It is the terminal's own hardware cursor,
/// which occupies no character cell at all — "ornament may never occupy a
/// cell that carries information" holds absolutely for the cursor rather
/// than by argument, because there is no cell to occupy in the first
/// place. [`render_with`] therefore never draws it onto the [`Grid`]; it
/// only reports where the terminal should place its own cursor, as the
/// second element of its return tuple.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cursor {
    /// Column.
    pub x: u16,
    /// Row.
    pub y: u16,
}

/// Render `json` into a `w`-by-`h` character grid, plus the **screen**
/// position the terminal cursor should sit at (see [`Cursor`]).
///
/// A terminal has exactly one hardware cursor, so its position is where
/// [`Focus`] becomes visible rather than any ink on the [`Grid`] (§2.1
/// forbids ornament occupying an informative cell): with [`Focus::Cli`],
/// the returned position is the command line's own caret, computed by
/// [`entry::draw`] from `line`; with [`Focus::Map`], it is `map_cursor`
/// verbatim, reported back exactly as `render_with`'s old `cursor`
/// parameter used to be — never drawn onto the grid either way. With
/// [`Focus::Walk`], neither is consulted — it claims no cursor at all, so
/// `None` comes back; otherwise exactly one of the two is consulted per
/// call, the other's input simply unused for that turn, not merged or
/// overridden.
///
/// `strip`, when `Some`, is drawn as the map strip beneath the plate
/// (see `strip::draw`); the row it occupies is reserved either way (see
/// `spread`'s module doc), so a caller that starts passing `Some` never
/// resizes the plate a second time.
///
/// `echo`, when `Some`, is the most recently SUBMITTED line, drawn above the
/// command row (see `entry::draw`'s doc for the ask-then-answer layout and
/// the reservation discipline the row it occupies follows). `hint`, when
/// `Some`, is the pending tab-completion ambiguity, drawn beneath the
/// command row (Task 9, The Lexicon; see [`entry::Hint`] for the weight
/// channel — typed stem bold, suggested remainder normal, a perishable-free
/// surface).
///
/// **This is the row The Portolan's Task 2 costs the plate.** At exactly
/// 80×24, the plate's content height was 21 rows before this row was
/// reserved and is 20 after — the number the task's own report is required
/// to state explicitly, because the 80×24 floor below is unweakened by
/// either figure: `render_with` still refuses anything smaller than
/// [`MIN_WIDTH`]×[`MIN_HEIGHT`], strip present or not.
///
/// Fails if `json` does not parse, or if the requested grid is smaller
/// than the monochrome floor ([`MIN_WIDTH`] by [`MIN_HEIGHT`]).
#[allow(clippy::too_many_arguments)] // `hint` (Task 9) pushed this to 9, mirroring `entry::draw`'s own allow — see that function's doc for why splitting the parameters would hide more than it clarifies
pub fn render_with(
    json: &str,
    w: u16,
    h: u16,
    focus: Focus,
    map_cursor: Option<Cursor>,
    line: CommandLine<'_>,
    strip: Option<&str>,
    echo: Option<&str>,
    hint: Option<&entry::Hint<'_>>,
) -> Result<(Grid, Option<(u16, u16)>), Error> {
    if w < MIN_WIDTH || h < MIN_HEIGHT {
        return Err(Error::TooSmall { w, h });
    }
    let snapshot = Snapshot::parse(json)?;
    let (grid, caret) = spread::compose(&snapshot, w, h, strip, focus, line, echo, hint);
    // ONE hardware cursor, so its location IS the focus indicator: with
    // `Focus::Cli` it is the caret in the entry pane; with `Focus::Map`,
    // the map cursor on the plate — never both, though a mode may claim no
    // cursor at all (`Focus::Map` without a map cursor, or `Focus::Walk`,
    // reports none). See `Focus`.
    let cursor = match focus {
        Focus::Cli => caret,
        Focus::Map => map_cursor.map(|c| (c.x, c.y)),
        // Walk drives neither pane's cursor: the buffer is not being edited
        // (printable keys bounce to the CLI), so the caret is not reported;
        // the map cursor is not moving either. Deliberately NEITHER — the
        // "never both" rule above is about ambiguity between the two pane
        // cursors, and Walk claims no cursor at all.
        Focus::Walk => None,
    };
    Ok((grid, cursor))
}

/// Render `json` — an emitted `vessel/session/v2` document — into a
/// `w`-by-`h` character grid: the plate left, the entry right, the
/// endpaper strip below (see [`spread::compose`]). Fails if `json` does
/// not parse, or if the requested grid is smaller than the monochrome
/// floor ([`MIN_WIDTH`] by [`MIN_HEIGHT`]).
///
/// Delegates to [`render_with`] with the CLI focused, no map cursor, an
/// empty command line and no map strip — kept as its own entry point
/// because existing callers (`tests/`, the terminal binary) depend on this
/// exact signature. It drops the returned cursor position entirely, so the
/// default focus/empty-line choice is unobservable through this function;
/// it exists only to satisfy `render_with`'s parameters.
pub fn render(json: &str, w: u16, h: u16) -> Result<Grid, Error> {
    render_with(
        json,
        w,
        h,
        Focus::Cli,
        None,
        CommandLine::default(),
        None,
        None,
        None,
    )
    .map(|(grid, _)| grid)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Walk is the DEFAULT focus — the game starts walking, not typing
    /// (The Stride design, "Mode model").
    #[test]
    fn walk_is_the_default_focus() {
        assert_eq!(Focus::default(), Focus::Walk);
    }

    /// `render` of a document that is not even valid JSON must surface
    /// `Error::Parse`, not panic and not `Error::TooSmall`.
    #[test]
    fn render_of_invalid_json_is_a_parse_error() {
        match render("not json at all", MIN_WIDTH, MIN_HEIGHT) {
            Err(Error::Parse(_)) => {}
            other => panic!("expected Error::Parse, got {other:?}"),
        }
    }

    /// A width one short of the floor is refused, and the error carries
    /// the exact `w`/`h` that was requested (not the floor, and not some
    /// clamped value) — the whole reason `TooSmall` is a struct variant
    /// rather than a unit one.
    #[test]
    fn render_one_column_short_of_the_floor_is_too_small() {
        let got = render("{}", MIN_WIDTH - 1, MIN_HEIGHT);
        assert!(matches!(
            got,
            Err(Error::TooSmall {
                w,
                h
            }) if w == MIN_WIDTH - 1 && h == MIN_HEIGHT
        ));
    }

    /// Symmetric case on the height axis, so a bug that only checked one
    /// dimension would still be caught.
    #[test]
    fn render_one_row_short_of_the_floor_is_too_small() {
        let got = render("{}", MIN_WIDTH, MIN_HEIGHT - 1);
        assert!(matches!(
            got,
            Err(Error::TooSmall {
                w,
                h
            }) if w == MIN_WIDTH && h == MIN_HEIGHT - 1
        ));
    }

    /// `TooSmall` is checked before the document is even parsed — an
    /// invalid document at an undersized grid must still report the size
    /// problem, not a parse problem, so a caller sees the more actionable
    /// error first.
    #[test]
    fn too_small_is_reported_even_for_invalid_json() {
        let got = render("not json", MIN_WIDTH - 1, MIN_HEIGHT);
        assert!(matches!(got, Err(Error::TooSmall { .. })));
    }

    /// A committed `vessel/session/v2` fixture, reused rather than minted —
    /// the same file `spread.rs`'s own tests and `tests/provenance.rs`
    /// already read.
    fn fixture_json() -> String {
        include_str!("../tests/fixtures/session-seed-42-turn-0.json").to_string()
    }

    /// H3: the floor holds. At exactly 80x24, with the strip added, the
    /// spread still renders — and 79x24 is still REFUSED rather than
    /// degraded. This is the constraint the campaign inherits and may not
    /// weaken, so it is asserted in both directions.
    #[test]
    fn the_eighty_by_twentyfour_floor_survives_the_strip() {
        let json = fixture_json();
        let (grid, _) = render_with(
            &json,
            80,
            24,
            Focus::Cli,
            None,
            CommandLine::default(),
            Some("Vngashngatva"),
            None,
            None,
        )
        .expect("renders at the floor");
        assert_eq!(grid.width(), 80);
        assert_eq!(grid.height(), 24);
        assert!(
            matches!(
                render_with(
                    &json,
                    79,
                    24,
                    Focus::Cli,
                    None,
                    CommandLine::default(),
                    None,
                    None,
                    None
                ),
                Err(Error::TooSmall { .. })
            ),
            "79 columns must still be refused, not degraded"
        );
    }

    /// The map cursor is NOT ink. It must occupy no grid cell —
    /// `render_with` reports a position for the terminal to place its own
    /// cursor at, and the grid is byte-identical with and without one.
    /// Exercised with the map focused, since a map cursor is only ever
    /// reported in [`Focus::Map`] (see
    /// `the_caret_is_not_reported_when_the_map_is_focused` below for the
    /// entry pane's own half of this same dispatch).
    #[test]
    fn the_cursor_occupies_no_cell() {
        let json = fixture_json();
        let (plain, none_at) = render_with(
            &json,
            80,
            24,
            Focus::Map,
            None,
            CommandLine::default(),
            None,
            None,
            None,
        )
        .expect("renders");
        let (with, some_at) = render_with(
            &json,
            80,
            24,
            Focus::Map,
            Some(Cursor { x: 3, y: 4 }),
            CommandLine::default(),
            None,
            None,
            None,
        )
        .expect("renders");
        assert!(none_at.is_none());
        assert_eq!(
            some_at,
            Some((3, 4)),
            "the cursor position is reported, not drawn"
        );
        for y in 0..24 {
            for x in 0..80 {
                assert_eq!(
                    plain.get(x, y),
                    with.get(x, y),
                    "cursor inked cell ({x},{y})"
                );
            }
        }
    }

    /// `render_with`'s OWN version of `entry`'s same-named test: this one
    /// exercises the dispatch in `render_with` itself (`match focus { Cli
    /// => caret, Map => map_cursor }`), not `entry::draw` in isolation. A
    /// populated command line and a set map cursor both go in; with the map
    /// focused, only the map cursor may come back out. Step 8's mutation
    /// (neutralising that match to always return `caret`) is caught here,
    /// not by `entry`'s test of the same name, which never calls
    /// `render_with` at all.
    #[test]
    fn the_caret_is_not_reported_when_the_map_is_focused() {
        let json = fixture_json();
        let (_, at) = render_with(
            &json,
            80,
            24,
            Focus::Map,
            Some(Cursor { x: 3, y: 4 }),
            CommandLine {
                text: "look",
                caret: 4,
            },
            None,
            None,
            None,
        )
        .expect("renders");
        assert_eq!(
            at,
            Some((3, 4)),
            "with the map focused, render_with must report the MAP cursor, \
             never the entry pane's caret"
        );
    }

    /// Walk claims NO cursor: even with a populated command line and a set
    /// map cursor both present in the arguments, `render_with` must return
    /// `None` for the cursor position — neither pane's cursor is reported
    /// while the player is walking. Pins against a later regression that
    /// leaks the entry caret (or the map cursor) under the default focus.
    #[test]
    fn walk_reports_no_cursor_at_all() {
        let json = fixture_json();
        let (_, at) = render_with(
            &json,
            80,
            24,
            Focus::Walk,
            Some(Cursor { x: 3, y: 4 }),
            CommandLine {
                text: "look",
                caret: 4,
            },
            None,
            None,
            None,
        )
        .expect("renders");
        assert_eq!(
            at, None,
            "Walk claims no cursor; render_with must report None, never \
             either pane's cursor"
        );
    }
}

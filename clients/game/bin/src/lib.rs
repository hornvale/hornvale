#![warn(missing_docs)]
//! Hornvale's game client, driver half: links `hornvale-vessel` and
//! `hornvale-game-core`, so the two never need to meet in the same crate.
//! See `driver`'s module doc for the containment rule this exists to hold.

pub mod boot;
// The Overture, Task 8: the world cache and its three-layer validity
// protocol. Named separately from `overture` (not `overture::cache`)
// because it has nothing to do with rendering — it is a save/load seam next
// to `state_dir`, the module it shares its on-disk home with.
pub mod cache;
pub mod discovery;
pub mod driver;
pub mod history;
pub mod input;
pub mod line;
pub mod mercator;
// The Overture, Task 3: the startup frame and the `View` contract its four
// views are written against. Named `overture::Frame` deliberately, and it is
// NOT `mercator::Frame` — see that module's doc for the distinction.
pub mod overture;
pub mod plate;
pub mod state_dir;
pub mod term;
pub mod tiles;

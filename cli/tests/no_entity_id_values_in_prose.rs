//! An `EntityId` may be stored, compared and looked up. It may not be read
//! for its value in a derived-prose path (The Salt, spec D7).
//!
//! A `clippy.toml` `disallowed-methods` entry is not available here:
//! `NonZeroU64::get` is far too general to ban workspace-wide, and
//! `disallowed-methods` is one lint with one on/off switch per scope
//! (decision 0092), so an `#[allow]` would silence the platform-libm ban
//! too. A scoped source scan is the backstop instead -- the shape
//! `cli/tests/architecture.rs` already uses.
//!
//! **Why these particular shapes.** The plan's first draft banned bare
//! `u64::from(`, which fires on the legitimate `CellId` conversions this
//! same code performs (`u64::from(c.site.0)`, `u64::from(p.site.0)`,
//! `u64::from(own.site.0)` and the byte-fold `u64::from(*b)` in
//! `record.rs`). Every one of those goes through a COMPOUND path
//! (`.site.0`, two dots) or has no `.0` suffix at all (`*b`,
//! `c.peak_population`); an `EntityId` conversion in this codebase is
//! always a single bare `<ident>.0` (`u64::from(occupation.0)`, the shape
//! `founder_of` carried pre-Salt). Requiring exactly one dot immediately
//! before the closing paren separates the two without a per-line allowlist.
//! The one single-dot survivor that is NOT an entity (`u64::from(c.0)` in
//! `layer_key`'s `Genesis` arm) carries its own `// salt-allow`, and so does
//! the one legitimate bare `.get()` (`descent.rs`'s `StdDays` generation
//! length) -- two markers total, both on lines that really are the
//! exception, not sprinkled defensively.
//!
//! **Coverage check against the four pre-Salt originals** (the shapes this
//! backstop exists to keep shut, `git show <rev>~1:<path>`):
//! - `632fbe5d~1` `flesh_seed`: `entity.0.get().to_string()` -- caught by
//!   `.0.get()`.
//! - `141fe2cd~1` `layer_key`: `e.get()` -- caught by bare `.get()`.
//! - `73b61868~1` `founder_of`: `u64::from(occupation.0)` -- caught by the
//!   single-dot `u64::from(<ident>.0)` shape.
//! - `530a5286~1` `conquest_victim`: `min_by_key(|e| e.0.get())` -- caught
//!   by `.0.get()`.

use std::path::Path;

/// The files whose output is derived PROSE, where an id's value must not be
/// read. Deliberately narrow: the ledger, the vessel session surface and the
/// scene exports legitimately carry id values.
const PROSE_PATHS: &[&str] = &[
    "domains/history/src/record.rs",
    "domains/history/src/flesh.rs",
    "windows/almanac/src/history.rs",
    "windows/worldgen/src/descent.rs",
];

/// Unwraps a `NonZeroU64` id straight off its inner field -- the shape
/// `flesh_seed` and `conquest_victim` used pre-Salt. Zero legitimate uses in
/// these four files today.
const DOT_ZERO_GET: &str = ".0.get()";

/// A bare, no-argument `.get()` call -- the shape `layer_key`'s `From` arm
/// used pre-Salt (`EntityId::get`). The one legitimate use left in these
/// files (a `StdDays::get()` generation-length read in `descent.rs`) carries
/// its own `// salt-allow`.
const BARE_GET: &str = ".get()";

/// Every `u64::from(...)` call's argument on `line`, trimmed. Assumes no
/// nested parens inside the argument, true of every call in these four
/// files today.
fn u64_from_args(line: &str) -> Vec<&str> {
    const PREFIX: &str = "u64::from(";
    let mut args = Vec::new();
    let mut rest = line;
    while let Some(start) = rest.find(PREFIX) {
        let after = &rest[start + PREFIX.len()..];
        match after.find(')') {
            Some(end) => {
                args.push(after[..end].trim());
                rest = &after[end + 1..];
            }
            None => break,
        }
    }
    args
}

/// Whether a `u64::from(arg)` argument is the single-level `<ident>.0` shape
/// an `EntityId` conversion takes. A `CellId` reached through a compound
/// path (`p.site.0`, two dots) or a non-`.0` field (`c.peak_population`)
/// does not match.
fn looks_like_entity_conversion(arg: &str) -> bool {
    arg.ends_with(".0") && arg.matches('.').count() == 1
}

#[test]
fn derived_prose_never_reads_an_entity_id_for_its_value() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root");
    let mut findings = Vec::new();
    for rel in PROSE_PATHS {
        let src = std::fs::read_to_string(root.join(rel)).unwrap_or_else(|e| panic!("{rel}: {e}"));
        for (n, line) in src.lines().enumerate() {
            let code = line.trim_start();
            if code.starts_with("//") || code.starts_with("///") {
                continue;
            }
            if code.contains("salt-allow") {
                continue;
            }
            if code.contains(DOT_ZERO_GET) {
                findings.push(format!("{rel}:{}: {} [{DOT_ZERO_GET}]", n + 1, code.trim()));
                continue;
            }
            if code.contains(BARE_GET) {
                findings.push(format!("{rel}:{}: {} [{BARE_GET}]", n + 1, code.trim()));
                continue;
            }
            for arg in u64_from_args(code) {
                if looks_like_entity_conversion(arg) {
                    findings.push(format!(
                        "{rel}:{}: {} [u64::from({arg})]",
                        n + 1,
                        code.trim()
                    ));
                }
            }
        }
    }
    assert!(
        findings.is_empty(),
        "an EntityId's value is read in a derived-prose path:\n  {}\n\
         If the use is legitimate, append a `// salt-allow: <reason>` \
         comment on the SAME line.",
        findings.join("\n  ")
    );
}

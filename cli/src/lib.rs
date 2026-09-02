//! The hornvale CLI as a library: every command module, reachable by tests.
//!
//! `main.rs` is the binary that dispatches into these. The split exists so a
//! command's logic can be tested directly instead of only through a built
//! binary — `cli/` is the thin command surface, and this is what makes that
//! description true of the crate and not merely of its intent.
#![warn(missing_docs)]

pub mod attest;
pub mod audio;
pub mod concepts;
pub mod dictionary;
pub mod phonology;
pub mod proto;
pub mod provision;
pub mod repl;
pub mod streams;
pub mod systems;
pub mod tropes;

/// Read the value following `flag` in `args`, if present.
///
/// Hoisted out of `main.rs` because `audio.rs` reaches it as
/// `crate::flag_value`, and a module moving into the library cannot reach a
/// helper that stayed in the binary.
///
/// **This returns the next token unconditionally**, so it is correct only for
/// flags that take a value. `cmd_concepts` takes `--manifest` with no value;
/// see `cmd_tropes`'s mode scan for what that asymmetry has already cost.
/// type-audit: bare-ok(identifier-text: args), bare-ok(identifier-text: flag), bare-ok(identifier-text: return)
pub fn flag_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1))
        .map(String::as_str)
}

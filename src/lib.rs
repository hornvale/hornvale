//! A library for roguelikes, MUDs, and other text adventures.
//!
//! I don't really anticipate anyone using this library, as it's very specific
//! and intensely opinionated. Nevertheless, I'm making it public in case
//! someone finds it useful or interesting.

// Linting.
#![deny(rustdoc::bare_urls)]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::invalid_codeblock_attributes)]
#![deny(rustdoc::invalid_rust_codeblocks)]
#![deny(rustdoc::missing_crate_level_docs)]
#![deny(rustdoc::unescaped_backticks)]
#![deny(ambiguous_glob_imports)]
#![deny(ambiguous_glob_reexports)]
#![deny(bare_trait_objects)]
#![deny(const_item_mutation)]
#![deny(explicit_outlives_requirements)]
#![deny(let_underscore_drop)]
#![deny(meta_variable_misuse)]
#![deny(missing_copy_implementations)]
#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(non_ascii_idents)]
#![deny(single_use_lifetimes)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unreachable_pub)]
#![deny(unsafe_code)]
#![deny(unused_crate_dependencies)]
#![deny(unused_extern_crates)]
#![deny(unused_import_braces)]
#![deny(unused_lifetimes)]
#![deny(unused_qualifications)]
#![deny(variant_size_differences)]
#![allow(unused_macros)]
#![allow(async_fn_in_trait)]

/// The prelude for the _Hornvale_ library.
///
/// This prelude is similar to the standard library's prelude in that you'll
/// almost always want to have these items available. It includes a number of
/// traits, types, and functions that will be exposed to the _Hornvale_ binary,
/// examples, benches, and integration tests.
pub mod prelude {
  pub use hornvale_astronomy::prelude::*;

  /// Hello, world!
  pub fn hello() {
    println!("Hello, world!");
  }
}

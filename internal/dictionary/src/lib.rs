//! # Dictionary
//!
//! This module provides dictionary functionality for the _Hornvale_ library.
//! It is used to store and retrieve words, phrases, and other language-related
//! data.
//!

/// Keywords.
pub mod keyword;
/// Parts of speech.
pub mod part_of_speech;

/// The prelude.
pub mod prelude {
  pub use crate::keyword::dictionary::KeywordDictionary;
  pub use crate::part_of_speech::{dictionary::PartOfSpeechDictionary, PartOfSpeech};
}

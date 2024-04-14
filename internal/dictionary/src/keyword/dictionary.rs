use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead, Result as IoResult};
use std::path::Path;

/// A dictionary with a list of keywords.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct KeywordDictionary {
  /// The map of keyword types to their keywords.
  pub map: HashMap<String, HashSet<String>>,
}

impl KeywordDictionary {
  /// Creates a new dictionary.
  pub fn new() -> Self {
    let map = HashMap::new();
    Self { map }
  }

  /// Adds a keyword to the dictionary.
  pub fn add_keyword(&mut self, keyword_type: &str, keyword: &str) {
    let entry = self.map.entry(keyword_type.to_string()).or_default();
    if !entry.contains(keyword) {
      entry.insert(keyword.to_string());
    }
  }

  /// Retrieves the keywords for a keyword type.
  pub fn get_keywords(&self, keyword_type: &str) -> Option<&HashSet<String>> {
    self.map.get(keyword_type)
  }

  /// Determine if the dictionary contains a keyword type.
  pub fn contains_keyword_type(&self, keyword_type: &str) -> bool {
    self.map.contains_key(keyword_type)
  }

  /// Determine if the dictionary contains a keyword.
  pub fn contains_keyword(&self, keyword_type: &str, keyword: &str) -> bool {
    if let Some(keywords) = self.map.get(keyword_type) {
      keywords.contains(keyword)
    } else {
      false
    }
  }

  /// Removes a keyword type from the dictionary.
  pub fn remove_keyword_type(&mut self, keyword_type: &str) {
    self.map.remove(keyword_type);
  }

  /// Removes a keyword from the dictionary.
  pub fn remove_keyword(&mut self, keyword_type: &str, keyword: &str) {
    if let Some(keywords) = self.map.get_mut(keyword_type) {
      keywords.remove(keyword);
    }
  }

  /// Clears the dictionary.
  pub fn clear(&mut self) {
    self.map.clear();
  }

  /// Returns the number of words in the dictionary.
  pub fn len(&self) -> usize {
    self.map.len()
  }

  /// Returns true if the dictionary is empty.
  pub fn is_empty(&self) -> bool {
    self.map.is_empty()
  }

  /// Determines whether a word might be a keyword.
  pub fn fits(&self, word: &str, keyword_type: &str) -> bool {
    match self.get_keywords(keyword_type) {
      Some(keywords) => keywords.contains(word),
      None => false,
    }
  }

  /// Loads words from a file into the dictionary as a given part of speech.
  pub fn load_words_from_file(&mut self, path: &Path, keyword_type: &str) -> IoResult<()> {
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
      let word = line?;
      self.add_keyword(keyword_type, &word);
    }

    Ok(())
  }
}

macro_rules! read_into_dictionary {
  ($dictionary:ident, $path:expr, $keyword_type:expr) => {
    let words = include_str!($path);
    for word in words.lines() {
      if word.is_empty() {
        continue;
      }
      $dictionary.add_keyword($keyword_type, word);
    }
  };
}

fn load_default_dictionary() -> KeywordDictionary {
  let mut dictionary = KeywordDictionary::new();
  read_into_dictionary!(dictionary, "../../data/keyword/dictionary/directions.txt", "direction");
  dictionary
}

impl Default for KeywordDictionary {
  fn default() -> Self {
    load_default_dictionary()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_add_word() {
    init();
    let mut dictionary = KeywordDictionary::new();
    dictionary.add_keyword("directions", "north");
    assert_eq!(dictionary.len(), 1);
  }

  #[test]
  fn test_get_words() {
    init();
    let mut dictionary = KeywordDictionary::new();
    dictionary.add_keyword("directions", "north");
    dictionary.add_keyword("directions", "south");
    dictionary.add_keyword("directions", "east");
    dictionary.add_keyword("directions", "west");
    assert_eq!(dictionary.get_keywords("directions").unwrap().len(), 4);
  }

  #[test]
  fn test_contains_word() {
    init();
    let mut dictionary = KeywordDictionary::new();
    dictionary.add_keyword("directions", "north");
    assert!(dictionary.contains_keyword("directions", "north"));
    assert!(!dictionary.contains_keyword("directions", "south"));
  }

  #[test]
  fn test_remove_word() {
    init();
    let mut dictionary = KeywordDictionary::new();
    dictionary.add_keyword("directions", "north");
    dictionary.remove_keyword("directions", "north");
    assert_eq!(dictionary.len(), 1);
    assert_eq!(dictionary.get_keywords("directions").unwrap().len(), 0);
  }

  #[test]
  fn test_clear() {
    init();
    let mut dictionary = KeywordDictionary::new();
    dictionary.add_keyword("directions", "north");
    dictionary.clear();
    assert_eq!(dictionary.len(), 0);
  }

  #[test]
  fn test_is_empty() {
    init();
    let dictionary = KeywordDictionary::new();
    assert!(dictionary.is_empty());
  }

  #[test]
  fn test_is_not_empty() {
    init();
    let mut dictionary = KeywordDictionary::new();
    dictionary.add_keyword("directions", "north");
    assert!(!dictionary.is_empty());
  }

  #[test]
  fn test_is_direction() {
    init();
    let mut dictionary = KeywordDictionary::new();
    dictionary.add_keyword("directions", "north");
    assert!(dictionary.fits("north", "directions"));
    assert!(!dictionary.fits("south", "directions"));
  }

  #[test]
  fn test_default_dictionary() {
    init();
    let dictionary = KeywordDictionary::default();
    assert_eq!(dictionary.len(), 1);
    assert_eq!(dictionary.get_keywords("direction").unwrap().len(), 19);
  }
}

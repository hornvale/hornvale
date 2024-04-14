use super::PartOfSpeech;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// A dictionary mapping words to their parts of speech.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct PartOfSpeechDictionary {
  /// The map of words to their parts of speech.
  pub map: HashMap<String, HashSet<PartOfSpeech>>,
}

impl PartOfSpeechDictionary {
  /// Creates a new dictionary.
  pub fn new() -> Self {
    Self { map: HashMap::new() }
  }

  /// Adds a word to the dictionary.
  pub fn add_word(&mut self, word: String, part_of_speech: PartOfSpeech) {
    let entry = self.map.entry(word).or_default();
    entry.insert(part_of_speech);
  }

  /// Retrieves the parts of speech for a word.
  pub fn get_parts_of_speech(&self, word: &str) -> Option<&HashSet<PartOfSpeech>> {
    self.map.get(word)
  }

  /// Determine if the dictionary contains a word.
  pub fn contains_word(&self, word: &str) -> bool {
    self.map.contains_key(word)
  }

  /// Removes a word from the dictionary.
  pub fn remove_word(&mut self, word: &str) {
    self.map.remove(word);
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

  /// Determines whether a word might be a part of speech.
  pub fn fits(&self, word: &str, part_of_speech: PartOfSpeech) -> bool {
    match self.get_parts_of_speech(word) {
      Some(parts_of_speech) => parts_of_speech.contains(&part_of_speech),
      None => false,
    }
  }

  /// Determines whether a word is a noun.
  pub fn is_noun(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Noun)
  }

  /// Determines whether a word is a pronoun.
  pub fn is_pronoun(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Pronoun)
  }

  /// Determines whether a word is a verb.
  pub fn is_verb(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Verb)
  }

  /// Determines whether a word is an adjective.
  pub fn is_adjective(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Adjective)
  }

  /// Determines whether a word is an adverb.
  pub fn is_adverb(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Adverb)
  }

  /// Determines whether a word is a preposition.
  pub fn is_preposition(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Preposition)
  }

  /// Determines whether a word is a conjunction.
  pub fn is_conjunction(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Conjunction)
  }

  /// Determines whether a word is an interjection.
  pub fn is_interjection(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Interjection)
  }

  /// Determines whether a word is a determiner.
  pub fn is_determiner(&self, word: &str) -> bool {
    self.fits(word, PartOfSpeech::Determiner)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_add_word() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat".to_string(), PartOfSpeech::Noun);
    assert_eq!(dictionary.len(), 1);
  }

  #[test]
  fn test_get_parts_of_speech() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat".to_string(), PartOfSpeech::Noun);
    let parts_of_speech = dictionary.get_parts_of_speech("cat");
    assert_eq!(parts_of_speech, Some(&HashSet::from([PartOfSpeech::Noun])));
  }

  #[test]
  fn test_contains_word() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat".to_string(), PartOfSpeech::Noun);
    assert!(dictionary.contains_word("cat"));
    assert!(!dictionary.contains_word("dog"));
  }

  #[test]
  fn test_remove_word() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat".to_string(), PartOfSpeech::Noun);
    dictionary.remove_word("cat");
    assert_eq!(dictionary.len(), 0);
  }

  #[test]
  fn test_clear() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat".to_string(), PartOfSpeech::Noun);
    dictionary.clear();
    assert_eq!(dictionary.len(), 0);
  }

  #[test]
  fn test_is_noun() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat".to_string(), PartOfSpeech::Noun);
    assert!(dictionary.is_noun("cat"));
    assert!(!dictionary.is_noun("dog"));
  }

  #[test]
  fn test_is_pronoun() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("I".to_string(), PartOfSpeech::Pronoun);
    assert!(dictionary.is_pronoun("I"));
    assert!(!dictionary.is_pronoun("you"));
  }

  #[test]
  fn test_is_verb() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("run".to_string(), PartOfSpeech::Verb);
    assert!(dictionary.is_verb("run"));
    assert!(!dictionary.is_verb("walk"));
  }

  #[test]
  fn test_is_adjective() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("big".to_string(), PartOfSpeech::Adjective);
    assert!(dictionary.is_adjective("big"));
    assert!(!dictionary.is_adjective("small"));
  }

  #[test]
  fn test_is_adverb() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("quickly".to_string(), PartOfSpeech::Adverb);
    assert!(dictionary.is_adverb("quickly"));
    assert!(!dictionary.is_adverb("slowly"));
  }

  #[test]
  fn test_is_preposition() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("in".to_string(), PartOfSpeech::Preposition);
    assert!(dictionary.is_preposition("in"));
    assert!(!dictionary.is_preposition("on"));
  }

  #[test]
  fn test_is_conjunction() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("and".to_string(), PartOfSpeech::Conjunction);
    assert!(dictionary.is_conjunction("and"));
    assert!(!dictionary.is_conjunction("but"));
  }

  #[test]
  fn test_is_interjection() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("wow".to_string(), PartOfSpeech::Interjection);
    assert!(dictionary.is_interjection("wow"));
    assert!(!dictionary.is_interjection("ouch"));
  }

  #[test]
  fn test_is_determiner() {
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("the".to_string(), PartOfSpeech::Determiner);
    assert!(dictionary.is_determiner("the"));
    assert!(!dictionary.is_determiner("a"));
  }
}

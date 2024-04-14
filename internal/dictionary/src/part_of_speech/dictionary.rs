use super::PartOfSpeech;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead, Result as IoResult};
use std::path::Path;

/// A dictionary mapping words to their parts of speech.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PartOfSpeechDictionary {
  /// The map of words to their parts of speech.
  pub map: HashMap<String, HashSet<PartOfSpeech>>,
  /// A list of multi-word expressions.
  pub multi_word_expressions: Vec<String>,
}

impl PartOfSpeechDictionary {
  /// Creates a new dictionary.
  pub fn new() -> Self {
    let map = HashMap::new();
    let multi_word_expressions = Vec::new();
    Self {
      map,
      multi_word_expressions,
    }
  }

  /// Adds a word to the dictionary.
  pub fn add_word(&mut self, word: &str, part_of_speech: PartOfSpeech) {
    let entry = self.map.entry(word.to_string()).or_default();
    if !entry.contains(&part_of_speech) {
      entry.insert(part_of_speech);
    }
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

  /// Loads words from a file into the dictionary as a given part of speech.
  pub fn load_words_from_file(&mut self, path: &Path, pos: PartOfSpeech) -> IoResult<()> {
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
      let word = line?;
      self.add_word(&word, pos);
    }

    Ok(())
  }
}

macro_rules! read_into_dictionary {
  ($dictionary:ident, $path:expr, $pos:expr) => {
    let words = include_str!($path);
    for word in words.lines() {
      if word.is_empty() {
        continue;
      }
      if word.contains(' ') {
        $dictionary.multi_word_expressions.push(word.to_string());
        $dictionary.add_word(&word.to_string().replace(' ', "_"), $pos);
      } else {
        $dictionary.add_word(word, $pos);
      }
    }
  };
}

fn load_default_dictionary() -> PartOfSpeechDictionary {
  let mut dictionary = PartOfSpeechDictionary::new();
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/adjectives.txt",
    PartOfSpeech::Adjective
  );
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/adverbs.txt",
    PartOfSpeech::Adverb
  );
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/conjunctions.txt",
    PartOfSpeech::Conjunction
  );
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/determiners.txt",
    PartOfSpeech::Determiner
  );
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/interjections.txt",
    PartOfSpeech::Interjection
  );
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/nouns.txt",
    PartOfSpeech::Noun
  );
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/prepositions.txt",
    PartOfSpeech::Preposition
  );
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/pronouns.txt",
    PartOfSpeech::Pronoun
  );
  read_into_dictionary!(
    dictionary,
    "../../data/part_of_speech/dictionary/verbs.txt",
    PartOfSpeech::Verb
  );
  dictionary
}

impl Default for PartOfSpeechDictionary {
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
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat", PartOfSpeech::Noun);
    assert_eq!(dictionary.len(), 1);
  }

  #[test]
  fn test_get_parts_of_speech() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat", PartOfSpeech::Noun);
    let parts_of_speech = dictionary.get_parts_of_speech("cat");
    assert_eq!(parts_of_speech, Some(&HashSet::from([PartOfSpeech::Noun])));
  }

  #[test]
  fn test_contains_word() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat", PartOfSpeech::Noun);
    assert!(dictionary.contains_word("cat"));
    assert!(!dictionary.contains_word("dog"));
  }

  #[test]
  fn test_remove_word() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat", PartOfSpeech::Noun);
    dictionary.remove_word("cat");
    assert_eq!(dictionary.len(), 0);
  }

  #[test]
  fn test_clear() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat", PartOfSpeech::Noun);
    dictionary.clear();
    assert_eq!(dictionary.len(), 0);
  }

  #[test]
  fn test_is_noun() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("cat", PartOfSpeech::Noun);
    assert!(dictionary.is_noun("cat"));
    assert!(!dictionary.is_noun("dog"));
  }

  #[test]
  fn test_is_pronoun() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("I", PartOfSpeech::Pronoun);
    assert!(dictionary.is_pronoun("I"));
    assert!(!dictionary.is_pronoun("you"));
  }

  #[test]
  fn test_is_verb() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("run", PartOfSpeech::Verb);
    assert!(dictionary.is_verb("run"));
    assert!(!dictionary.is_verb("walk"));
  }

  #[test]
  fn test_is_adjective() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("big", PartOfSpeech::Adjective);
    assert!(dictionary.is_adjective("big"));
    assert!(!dictionary.is_adjective("small"));
  }

  #[test]
  fn test_is_adverb() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("quickly", PartOfSpeech::Adverb);
    assert!(dictionary.is_adverb("quickly"));
    assert!(!dictionary.is_adverb("slowly"));
  }

  #[test]
  fn test_is_preposition() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("in", PartOfSpeech::Preposition);
    assert!(dictionary.is_preposition("in"));
    assert!(!dictionary.is_preposition("on"));
  }

  #[test]
  fn test_is_conjunction() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("and", PartOfSpeech::Conjunction);
    assert!(dictionary.is_conjunction("and"));
    assert!(!dictionary.is_conjunction("but"));
  }

  #[test]
  fn test_is_interjection() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("wow", PartOfSpeech::Interjection);
    assert!(dictionary.is_interjection("wow"));
    assert!(!dictionary.is_interjection("ouch"));
  }

  #[test]
  fn test_is_determiner() {
    init();
    let mut dictionary = PartOfSpeechDictionary::new();
    dictionary.add_word("the", PartOfSpeech::Determiner);
    assert!(dictionary.is_determiner("the"));
    assert!(!dictionary.is_determiner("a"));
  }

  #[test]
  fn test_default() {
    init();
    let dictionary = PartOfSpeechDictionary::default();
    assert_gt!(dictionary.len(), 0);
  }
}

use crate::prelude::*;
use hornvale_core::prelude::*;

impl TryFrom<&str> for TokenKind {
  type Error = ();

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    let adverb = Adverb::try_from(string);
    let article = Article::try_from(string);
    let boolean = Boolean::try_from(string);
    let character = Character::try_from(string.chars().next().unwrap());
    let conjunction = Conjunction::try_from(string);
    let determiner = Determiner::try_from(string);
    let direction = Direction::try_from(string);
    let magic_word = MagicWord::try_from(string);
    let preposition = Preposition::try_from(string);
    let pronoun = Pronoun::try_from(string);
    let question_word = QuestionWord::try_from(string);
    let yes_no = YesNo::try_from(string);
    match string {
      _string if adverb.is_ok() && direction.is_ok() && preposition.is_ok() => Ok(TokenKind::UndeterminedModifier(
        UndeterminedModifier::AdverbOrDirectionOrPreposition,
      )),
      _string if adverb.is_ok() && direction.is_ok() => {
        Ok(TokenKind::UndeterminedModifier(UndeterminedModifier::AdverbOrDirection))
      },
      _string if adverb.is_ok() && preposition.is_ok() => Ok(TokenKind::UndeterminedModifier(
        UndeterminedModifier::AdverbOrPreposition,
      )),
      _string if direction.is_ok() && preposition.is_ok() => Ok(TokenKind::UndeterminedModifier(
        UndeterminedModifier::DirectionOrPreposition,
      )),
      _string if adverb.is_ok() => Ok(TokenKind::Adverb(adverb.unwrap())),
      _string if article.is_ok() => Ok(TokenKind::Article(article.unwrap())),
      _string if boolean.is_ok() => Ok(TokenKind::Boolean(boolean.unwrap())),
      _string if character.is_ok() => Ok(TokenKind::Character(character.unwrap())),
      _string if conjunction.is_ok() => Ok(TokenKind::Conjunction(conjunction.unwrap())),
      _string if determiner.is_ok() => Ok(TokenKind::Determiner(determiner.unwrap())),
      _string if direction.is_ok() => Ok(TokenKind::Direction(direction.unwrap())),
      _string if magic_word.is_ok() => Ok(TokenKind::MagicWord(magic_word.unwrap())),
      _string if preposition.is_ok() => Ok(TokenKind::Preposition(preposition.unwrap())),
      _string if pronoun.is_ok() => Ok(TokenKind::Pronoun(pronoun.unwrap())),
      _string if question_word.is_ok() => Ok(TokenKind::QuestionWord(question_word.unwrap())),
      _string if yes_no.is_ok() => Ok(TokenKind::YesNo(yes_no.unwrap())),
      _ => Err(()),
    }
  }
}

use super::*;
use hornvale_test_utilities::prelude::*;
use strum::IntoEnumIterator;

#[test]
fn test_is_punctuation() {
  init();
  assert!(TokenKind::Comma.is_punctuation());
  for kind in TokenKind::iter() {
    if kind != TokenKind::Comma {
      assert!(!kind.is_punctuation());
    }
  }
}

#[test]
fn test_is_special_character() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::Comma | TokenKind::SingleQuote => {
        assert!(!kind.is_special_character(), "{:?} is special", kind);
      },
      kind if !kind.is_single_character() => {
        assert!(!kind.is_special_character(), "{:?} is special", kind);
      },
      _ => {
        assert!(kind.is_special_character(), "{:?} is not special", kind);
      },
    }
  }
}

#[test]
fn test_is_direction() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::North
      | TokenKind::Northeast
      | TokenKind::East
      | TokenKind::Southeast
      | TokenKind::South
      | TokenKind::Southwest
      | TokenKind::West
      | TokenKind::Northwest
      | TokenKind::Up
      | TokenKind::Down
      | TokenKind::In
      | TokenKind::Out => {
        assert!(kind.is_direction(), "{:?} is not a direction", kind);
      },
      _ => {
        assert!(!kind.is_direction(), "{:?} is a direction", kind);
      },
    }
  }
}

#[test]
fn test_is_preposition() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::About
      | TokenKind::Above
      | TokenKind::Across
      | TokenKind::Against
      | TokenKind::Along
      | TokenKind::Among
      | TokenKind::As
      | TokenKind::At
      | TokenKind::Before
      | TokenKind::Behind
      | TokenKind::Below
      | TokenKind::Beside
      | TokenKind::Between
      | TokenKind::Beyond
      | TokenKind::By
      | TokenKind::For
      | TokenKind::From
      | TokenKind::In
      | TokenKind::Into
      | TokenKind::Of
      | TokenKind::Off
      | TokenKind::On
      | TokenKind::Out
      | TokenKind::Over
      | TokenKind::To
      | TokenKind::Toward
      | TokenKind::Under
      | TokenKind::Upon
      | TokenKind::With
      | TokenKind::Without => {
        assert!(kind.is_preposition(), "{:?} is not a preposition", kind);
      },
      _ => {
        assert!(!kind.is_preposition(), "{:?} is a preposition", kind);
      },
    }
  }
}

#[test]
fn test_is_adverb() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::Around
      | TokenKind::Down
      | TokenKind::Here
      | TokenKind::In
      | TokenKind::Off
      | TokenKind::On
      | TokenKind::Out
      | TokenKind::Up => {
        assert!(kind.is_adverb(), "{:?} is not an adverb", kind);
      },
      _ => {
        assert!(!kind.is_adverb(), "{:?} is an adverb", kind);
      },
    }
  }
}

#[test]
fn test_is_demonstrative_determiner() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::This | TokenKind::That | TokenKind::These | TokenKind::Those => {
        assert!(
          kind.is_demonstrative_determiner(),
          "{:?} is not a demonstrative determiner",
          kind
        );
      },
      _ => {
        assert!(
          !kind.is_demonstrative_determiner(),
          "{:?} is a demonstrative determiner",
          kind
        );
      },
    }
  }
}

#[test]
fn test_is_distributive_determiner() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::All
      | TokenKind::Any
      | TokenKind::Each
      | TokenKind::Either
      | TokenKind::Every
      | TokenKind::Neither
      | TokenKind::Some => {
        assert!(
          kind.is_distributive_determiner(),
          "{:?} is not a distributive determiner",
          kind
        );
      },
      _ => {
        assert!(
          !kind.is_distributive_determiner(),
          "{:?} is a distributive determiner",
          kind
        );
      },
    }
  }
}

#[test]
fn test_is_possessive_determiner() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::My
      | TokenKind::Your
      | TokenKind::His
      | TokenKind::Her(HerToken::PossessiveDeterminer)
      | TokenKind::Its
      | TokenKind::Our
      | TokenKind::Their
      | TokenKind::NounPossessiveDeterminer => {
        assert!(
          kind.is_possessive_determiner(),
          "{:?} is not a possessive determiner",
          kind
        );
      },
      _ => {
        assert!(
          !kind.is_possessive_determiner(),
          "{:?} is a possessive determiner",
          kind
        );
      },
    }
  }
}

#[test]
fn test_is_pronoun() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::Me
      | TokenKind::You
      | TokenKind::Him
      | TokenKind::Her(HerToken::Pronoun)
      | TokenKind::It
      | TokenKind::Them => {
        assert!(kind.is_pronoun(), "{:?} is not a pronoun", kind);
      },
      _ => {
        assert!(!kind.is_pronoun(), "{:?} is a pronoun", kind);
      },
    }
  }
}

#[test]
fn test_is_article() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::A | TokenKind::The => {
        assert!(kind.is_article(), "{:?} is not an article", kind);
      },
      _ => {
        assert!(!kind.is_article(), "{:?} is an article", kind);
      },
    }
  }
}

#[test]
fn test_is_conjunction() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::And | TokenKind::Or | TokenKind::But => {
        assert!(kind.is_conjunction(), "{:?} is not a conjunction", kind);
      },
      _ => {
        assert!(!kind.is_conjunction(), "{:?} is a conjunction", kind);
      },
    }
  }
}

#[test]
fn test_is_verb() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::North
      | TokenKind::Northeast
      | TokenKind::East
      | TokenKind::Southeast
      | TokenKind::South
      | TokenKind::Southwest
      | TokenKind::West
      | TokenKind::Northwest
      | TokenKind::Up
      | TokenKind::Down
      | TokenKind::In
      | TokenKind::Out
      | TokenKind::Verb => {
        assert!(kind.is_verb(), "{:?} is not a verb", kind);
      },
      _ => {
        assert!(!kind.is_verb(), "{:?} is a verb", kind);
      },
    }
  }
}

#[test]
fn test_is_noun() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::North
      | TokenKind::Northeast
      | TokenKind::East
      | TokenKind::Southeast
      | TokenKind::South
      | TokenKind::Southwest
      | TokenKind::West
      | TokenKind::Northwest
      | TokenKind::Up
      | TokenKind::Down
      | TokenKind::In
      | TokenKind::Out
      | TokenKind::All
      | TokenKind::Noun
      | TokenKind::Him
      | TokenKind::Me
      | TokenKind::Her(HerToken::Pronoun)
      | TokenKind::It
      | TokenKind::Them
      | TokenKind::You
      | TokenKind::DirectObject
      | TokenKind::IndirectObject => {
        assert!(kind.is_noun(), "{:?} is not a noun", kind);
      },
      _ => {
        assert!(!kind.is_noun(), "{:?} is a noun", kind);
      },
    }
  }
}

#[test]
fn test_could_be_noun() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::North
      | TokenKind::Northeast
      | TokenKind::East
      | TokenKind::Southeast
      | TokenKind::South
      | TokenKind::Southwest
      | TokenKind::West
      | TokenKind::Northwest
      | TokenKind::Up
      | TokenKind::Down
      | TokenKind::In
      | TokenKind::Out
      | TokenKind::All
      | TokenKind::Noun
      | TokenKind::Him
      | TokenKind::Me
      | TokenKind::Her(HerToken::Pronoun)
      | TokenKind::It
      | TokenKind::Them
      | TokenKind::You
      | TokenKind::Word(WordToken::Unclassified | WordToken::Noun | WordToken::Ambiguous)
      | TokenKind::DirectObject
      | TokenKind::IndirectObject => {
        assert!(kind.could_be_noun(), "{:?} is not a noun", kind);
      },
      _ => {
        assert!(!kind.could_be_noun(), "{:?} is a noun", kind);
      },
    }
  }
}

#[test]
fn test_could_be_adjective() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::Word(WordToken::Unclassified | WordToken::Adjective | WordToken::Ambiguous) => {
        assert!(kind.could_be_adjective(), "{:?} is not an adjective", kind);
      },
      _ => {
        assert!(!kind.could_be_adjective(), "{:?} is an adjective", kind);
      },
    }
  }
}

#[test]
fn test_could_be_verb() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::North
      | TokenKind::Northeast
      | TokenKind::East
      | TokenKind::Southeast
      | TokenKind::South
      | TokenKind::Southwest
      | TokenKind::West
      | TokenKind::Northwest
      | TokenKind::Up
      | TokenKind::Down
      | TokenKind::In
      | TokenKind::Out
      | TokenKind::Verb
      | TokenKind::Word(WordToken::Unclassified | WordToken::Verb | WordToken::Ambiguous) => {
        assert!(kind.could_be_verb(), "{:?} is not a verb", kind);
      },
      _ => {
        assert!(!kind.could_be_verb(), "{:?} is a verb", kind);
      },
    }
  }
}

#[test]
fn test_can_follow_adjective() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::Comma
      | TokenKind::Adjective
      | TokenKind::Noun
      | TokenKind::DirectObject
      | TokenKind::IndirectObject
      | TokenKind::NounPossessiveDeterminer => {
        assert!(kind.can_follow_adjective(), "{:?} cannot follow an adjective", kind);
      },
      _ => {
        assert!(!kind.can_follow_adjective(), "{:?} can follow an adjective", kind);
      },
    }
  }
}

#[test]
fn test_is_yes_no() {
  init();
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::Yes | TokenKind::No => {
        assert!(kind.is_yes_no(), "{:?} is not a yes/no token", kind);
      },
      _ => {
        assert!(!kind.is_yes_no(), "{:?} is a yes/no token", kind);
      },
    }
  }
}

#[test]
fn test_as_bool() {
  init();
  assert_eq!(TokenKind::Yes.as_bool(), Some(true));
  assert_eq!(TokenKind::No.as_bool(), Some(false));
  for kind in TokenKind::iter() {
    match kind {
      TokenKind::Yes => {
        assert_eq!(kind.as_bool(), Some(true));
      },
      TokenKind::No => {
        assert_eq!(kind.as_bool(), Some(false));
      },
      _ => {
        assert_eq!(kind.as_bool(), None);
      },
    }
  }
}

use crate::prelude::*;
use crate::token::kind::character::Character;

#[cfg(test)]
pub mod tests;

/// A simple scanner for breaking input into tokens.
#[derive(Clone, Debug)]
pub struct Scanner {
  /// The input to scan.
  pub input: String,
  /// Where the current token started.
  pub start: usize,
  /// The current position in the input.
  pub current: usize,
  /// The tokens that have been scanned so far.
  pub tokens: Vec<Token>,
}

// There are many opportunities for optimization here, but we'll keep it
// simple for now. Once it is implemented and working, I'll benchmark it and
// see if there are any bottlenecks that need to be addressed. Currently,
// there are a lot of string allocations, etc.

impl Scanner {
  /// Create a new scanner.
  pub fn new(input: &str) -> Self {
    let input = input.to_string();
    let start = 0;
    let current = 0;
    let tokens = Vec::new();
    Self {
      input,
      start,
      current,
      tokens,
    }
  }

  /// Reset the scanner.
  pub fn reset(&mut self) {
    self.start = 0;
    self.current = 0;
    self.tokens.clear();
  }

  /// Scan the input into tokens.
  pub fn scan_tokens(&mut self) -> Result<Vec<Token>, ParserError> {
    while !self.is_at_end() {
      self.start = self.current;
      let token = self.scan_token()?;
      self.tokens.push(token);
    }
    self.tokens.push(Token {
      kind: TokenKind::EndOfInput,
      lexeme: "".to_string(),
    });
    // Filter out articles.
    self.tokens.retain(|token| !token.kind.is_article());
    // Optimization: take the vector instead of cloning it.
    Ok(self.tokens.clone())
  }

  /// Scan the next token.
  pub fn scan_token(&mut self) -> Result<Token, ParserError> {
    self.skip_whitespace()?;
    self.start = self.current;
    if self.is_at_end() {
      return Ok(self.make_token(TokenKind::EndOfInput));
    }
    let character = self.advance()?;
    use Character::*;
    use MagicWord::*;
    let result = match character {
      ',' => self.make_character_token(Comma),
      '.' => self.make_character_token(Period),
      ';' => self.make_character_token(Semicolon),
      '`' => self.make_character_token(Backtick),
      '!' => self.match_magic_word(BangWord, Bang)?,
      '?' => self.match_magic_word(QuestionWord, Question)?,
      '@' => self.match_magic_word(AtSignWord, AtSign)?,
      '#' => self.match_magic_word(HashWord, Hash)?,
      '$' => self.match_magic_word(DollarWord, Dollar)?,
      '%' => self.match_magic_word(PercentWord, Percent)?,
      '^' => self.match_magic_word(CaretWord, Caret)?,
      '&' => self.match_magic_word(AmpersandWord, Ampersand)?,
      '*' => self.match_magic_word(AsteriskWord, Asterisk)?,
      '/' => self.match_magic_word(ForwardSlashWord, ForwardSlash)?,
      '\\' => self.match_magic_word(BackSlashWord, BackSlash)?,
      '(' => self.match_magic_word(LeftParenthesisWord, LeftParenthesis)?,
      ')' => self.match_magic_word(RightParenthesisWord, RightParenthesis)?,
      '[' => self.match_magic_word(LeftSquareBracketWord, LeftSquareBracket)?,
      ']' => self.match_magic_word(RightSquareBracketWord, RightSquareBracket)?,
      '{' => self.match_magic_word(LeftCurlyBraceWord, LeftCurlyBrace)?,
      '}' => self.match_magic_word(RightCurlyBraceWord, RightCurlyBrace)?,
      '<' => self.match_magic_word(LessThanWord, LessThan)?,
      '>' => self.match_magic_word(GreaterThanWord, GreaterThan)?,
      '=' => self.match_magic_word(EqualsWord, Equals)?,
      '+' => self.match_magic_word(PlusWord, Plus)?,
      '-' => self.match_magic_word(MinusWord, Minus)?,
      '|' => self.match_magic_word(PipeWord, Pipe)?,
      ':' => self.match_magic_word(ColonWord, Colon)?,
      '_' => self.match_magic_word(UnderscoreWord, Underscore)?,
      '~' => self.match_magic_word(TildeWord, Tilde)?,
      '"' | '\'' => self.match_string(character)?,
      char if char.is_ascii_digit() => self.match_number_or_ordinal()?,
      char if char.is_ascii_alphabetic() => self.match_word()?,
      _ => return Err(ParserError::UnexpectedCharacter(character)),
    };
    Ok(result)
  }

  /// Advance one character through the source and return it.
  pub fn advance(&mut self) -> Result<char, ParserError> {
    let position = self.current;
    self.current += 1;
    self
      .input
      .chars()
      .nth(position)
      .ok_or(ParserError::CharacterOutOfBounds)
  }

  /// Create a token based on a single-character token type.
  pub fn make_character_token(&self, kind: Character) -> Token {
    let lexeme = self.get_lexeme();
    Token {
      kind: TokenKind::Character(kind),
      lexeme,
    }
  }

  /// Create a token based on a token type.
  pub fn make_token(&self, kind: TokenKind) -> Token {
    let lexeme = self.get_lexeme();
    Token { kind, lexeme }
  }

  /// Try to match and create a token out of a number or ordinal.
  pub fn match_number_or_ordinal(&mut self) -> Result<Token, ParserError> {
    // Keep going as long as we have digits; this will handle both numbers and
    // ordinals.
    while self.peek().is_ascii_digit() {
      self.advance()?;
    }
    // We can encounter either a letter or whitespace after a number, so we
    // need to check for both.
    if self.peek().is_ascii_alphabetic() {
      // If it's a letter, assume it's an ordinal, and just keep going.
      while self.peek().is_ascii_alphabetic() {
        self.advance()?;
      }
      let result = self.make_token(TokenKind::Ordinal);
      Ok(result)
    } else {
      // If it's not a letter, it's a number.
      let result = self.make_token(TokenKind::NumberLiteral);
      Ok(result)
    }
  }

  /// Try to match and create a token out of a string.
  pub fn match_string(&mut self, start_char: char) -> Result<Token, ParserError> {
    // We can begin a string with either a single or double quote.
    // We'll use the start_char to determine which one we're looking for.
    while self.peek() != start_char && !self.is_at_end() {
      self.advance()?;
    }
    let result = if self.is_at_end() {
      return Err(ParserError::UnterminatedStringLiteral);
    } else {
      // Consume the closing quote.
      self.advance()?;
      self.make_token(TokenKind::StringLiteral)
    };
    Ok(result)
  }

  /// Try to match and create a word beginning with a special character.
  pub fn match_magic_word(&mut self, kind: MagicWord, fallback: Character) -> Result<Token, ParserError> {
    if self.peek() == ' ' {
      return Ok(self.make_token(TokenKind::Character(fallback)));
    }
    while self.peek().is_ascii_alphanumeric() {
      self.advance()?;
    }
    let result = self.make_token(TokenKind::MagicWord(kind));
    Ok(result)
  }

  /// Try to match and create a token out of a word.
  pub fn match_word(&mut self) -> Result<Token, ParserError> {
    while self.is_word_char(self.peek()) {
      self.advance()?;
    }
    let value = self.get_lexeme();
    let kind = match TokenKind::try_from(value.as_str()) {
      Ok(kind) => kind,
      Err(_) if value.find('\'').is_some() => TokenKind::Determiner(Determiner::NounPossessive),
      Err(_) if value.find('-').is_some() => TokenKind::Adjective,
      Err(_) => TokenKind::Word,
    };
    let result = self.make_token(kind);
    Ok(result)
  }

  /// Is the character a word character?
  ///
  /// We allow compound adjectives and possessives, so we need to allow for
  /// hyphens and apostrophes.
  pub fn is_word_char(&self, char: char) -> bool {
    char.is_alphabetic() || char == '-' || char == '\''
  }

  /// Get the lexeme from the start and current position.
  pub fn get_lexeme(&self) -> String {
    self.input[self.start..self.current].to_string()
  }

  /// Are we at the end of the input?
  pub fn is_at_end(&self) -> bool {
    self.current >= self.input.len()
  }

  /// Skip all the whitespace!
  pub fn skip_whitespace(&mut self) -> Result<(), ParserError> {
    while self.peek().is_ascii_whitespace() {
      self.advance()?;
    }
    Ok(())
  }

  /// Peek at the current character, but don't advance.
  pub fn peek(&self) -> char {
    self.peek_at_offset(0)
  }

  /// Peek at the next character.
  pub fn peek_next(&self) -> char {
    self.peek_at_offset(1)
  }

  /// Peek at a character at a specified offset.
  pub fn peek_at_offset(&self, offset: usize) -> char {
    match self.current + offset >= self.input.len() {
      true => '\0',
      false => self.input.chars().nth(self.current + offset).unwrap(),
    }
  }
}

use crate::error::ParserError;
use crate::prelude::{Token, TokenKind};
use derivative::Derivative;
use hecs::{Entity, With, World};
use hornvale_command::prelude::*;
use hornvale_world::prelude::*;

#[cfg(test)]
pub mod tests;

/// The parser, a simple top-down recursive descent parser.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Parser<'world> {
  /// A list of tokens to parse.
  pub tokens: Vec<Token>,
  /// The actor attempting the command.
  pub actor: Entity,
  /// The world to parse in.
  #[derivative(Debug = "ignore")]
  pub world: &'world mut World,
  /// The current index.
  pub current: usize,
  /// The verb, if any.
  pub verb: Option<Token>,
  /// The verb modifier, if any.
  pub modifier: Option<Token>,
  /// The direct object, if any.
  pub direct_object: Option<Token>,
  /// The indirect object, if any.
  pub indirect_object: Option<Token>,
  /// The command function, if any.
  pub function: Option<CommandFunction>,
  /// The command context.
  pub context: Option<CommandContext>,
}

impl<'world> Parser<'world> {
  /// Create a new parser.
  pub fn new(tokens: &[Token], actor: Entity, world: &'world mut World) -> Self {
    let tokens = tokens.to_vec();
    let current = 0;
    let verb = None;
    let modifier = None;
    let direct_object = None;
    let indirect_object = None;
    let function = None;
    let context = None;
    Self {
      tokens,
      actor,
      world,
      current,
      verb,
      modifier,
      direct_object,
      indirect_object,
      function,
      context,
    }
  }

  /// Parse a command and its context from the input.
  ///
  /// This is the main entry point for the parser.
  ///
  /// input → command | magic_word
  pub fn parse(&mut self) -> Result<(&CommandFunction, CommandContext), ParserError> {
    self.assert_non_empty()?;
    if self.match_magic_word()? {
      // None of this is implemented yet.
      self.consume_magic_word()?;
      self.bind_magic_word()?;
    } else {
      self.parse_command()?;
      self.bind_command()?;
      self.bind_context()?;
    }
    Ok((&self.function.as_ref().unwrap(), self.context.take().unwrap()))
  }

  /// Parse a command from the input.
  ///
  /// command → verb-phrase (object-phrase)?
  pub fn parse_command(&mut self) -> Result<(), ParserError> {
    self.consume_verb().map_err(|_| ParserError::NoVerb)?;
    match self.peek() {
      Some(token) if token.kind == TokenKind::Here => {
        self.modifier = self.peek();
        self.direct_object = self.peek();
        self.advance()?;
      },
      Some(token) if token.kind.is_adverb() => {
        self.modifier = self.peek();
        self.advance()?;
      },
      Some(token) if token.kind.is_direction() => {
        self.modifier = self.peek();
        self.direct_object = self.peek();
        self.advance()?;
      },
      Some(token) if token.kind.is_preposition() => {
        self.modifier = self.peek();
        self.advance()?;
      },
      Some(token) if token.kind.is_noun() => {
        self.consume_direct_object()?;
        self.advance()?;
      },
      _ => {},
    }
    Ok(())
  }

  /// Match current token.
  pub fn match_token(&mut self, kind: TokenKind) -> Result<bool, ParserError> {
    if !self.check_token(kind) {
      return Ok(false);
    }
    self.advance()?;
    Ok(true)
  }

  /// Match a magic word.
  pub fn match_magic_word(&mut self) -> Result<bool, ParserError> {
    self.peek().map(|t| t.kind.is_magic_word()).ok_or(ParserError::NoInput)
  }

  /// Consume the verb.
  pub fn consume_verb(&mut self) -> Result<(), ParserError> {
    if self.check_token(TokenKind::Verb) {
      self.verb = self.peek();
      self.advance()?;
      Ok(())
    } else {
      Err(ParserError::NoVerb)
    }
  }

  /// Consume the direct object.
  pub fn consume_direct_object(&mut self) -> Result<(), ParserError> {
    if self.check_token(TokenKind::DirectObject) {
      self.direct_object = self.peek();
      Ok(())
    } else {
      Err(ParserError::CouldNotConsumeDirectObject(
        self.peek().map(|t| t.lexeme.clone()).unwrap_or_default(),
      ))
    }
  }

  /// Consume the current token, throwing an error if it is not the expected kind.
  pub fn consume_token(&mut self, expected: TokenKind, message: &str) -> Result<(), ParserError> {
    let current_kind = self.peek().map(|t| t.kind).unwrap_or(TokenKind::EndOfInput);
    if current_kind == expected {
      self.advance()?;
      Ok(())
    } else {
      Err(ParserError::UnexpectedToken(
        expected,
        current_kind,
        message.to_string(),
      ))
    }
  }

  /// Consume a magic word.
  pub fn consume_magic_word(&mut self) -> Result<(), ParserError> {
    if self.match_magic_word()? {
      self.consume_token(self.peek().unwrap().kind, "Expected a magic word")
    } else {
      Err(ParserError::NoInput)
    }
  }

  /// Bind the magic word.
  pub fn bind_magic_word(&mut self) -> Result<(), ParserError> {
    // self.function = Some(CommandFunction::MagicWord);
    Ok(())
  }

  /// Bind the command.
  ///
  /// This is where the parser retrieves the command registry from the World,
  /// gets the form of the command, and binds the command function to the
  /// parser.
  pub fn bind_command(&mut self) -> Result<(), ParserError> {
    let registry = self
      .world
      .query_mut::<&mut CommandRegistry>()
      .into_iter()
      .next()
      .unwrap()
      .1;
    let name = self.verb.as_ref().unwrap().lexeme.as_str();
    if !registry.has_command(name) {
      return Err(ParserError::UnknownCommand(name.to_string()));
    }
    let form = self
      .modifier
      .as_ref()
      .map(|t| t.kind.try_into().unwrap_or(CommandForm::Default))
      .unwrap_or(CommandForm::Default);
    if !registry.has_form(name, &form) {
      let forms = registry.get_forms(name).unwrap();
      return Err(ParserError::UnknownCommandForm(
        name.to_string(),
        form.to_string(),
        forms.iter().map(|f| f.to_string()).collect(),
      ));
    }
    let command = registry
      .get(name, &form)
      .ok_or(ParserError::UnknownCommand(name.to_string()))?;
    self.function = Some(*command);
    Ok(())
  }

  /// Bind the context.
  #[allow(clippy::field_reassign_with_default)]
  pub fn bind_context(&mut self) -> Result<(), ParserError> {
    let mut context = self.context.take().unwrap_or_default();
    context.raw = self
      .tokens
      .iter()
      .map(|t| t.lexeme.clone())
      .collect::<Vec<_>>()
      .join(" ")
      .trim()
      .to_string();
    context.verb = if let Some(verb) = &self.verb {
      verb.lexeme.clone()
    } else {
      String::new()
    };
    if let Some(direct_object) = &self.direct_object {
      match direct_object {
        direct_object if direct_object.kind.is_direction() => {
          context.direct_object = Some(direct_object.try_into().unwrap());
        },
        direct_object if direct_object.kind == TokenKind::Here => {
          context.direct_object = Some(self.bind_here()?);
        },
        _ => {
          println!("Direct object: {:?}", direct_object);
        },
      }
    }
    context.form = if let Some(modifier) = &self.modifier {
      modifier.kind.try_into().unwrap_or(CommandForm::Default)
    } else {
      CommandForm::Default
    };
    self.context = Some(context);
    Ok(())
  }

  /// Bind the Here token.
  pub fn bind_here(&mut self) -> Result<CommandArgument, ParserError> {
    let actor_info = {
      let query_result = self.world.query_one::<(&Region, &Room)>(self.actor);
      let mut query = query_result.unwrap(); // `query` is now a longer-lived value
      let (region, room) = query.get().unwrap();
      (*region, *room)
    };

    let room_entity = {
      self
        .world
        .query::<With<(&Region, &Room), &IsARoom>>()
        .into_iter()
        .find(|(_, (&reg, &rm))| {
          let (region, room) = actor_info;
          region == reg && room == rm
        })
        .unwrap()
        .0
    };
    Ok(CommandArgument::Entity(room_entity))
  }

  /// Check kind of current token.
  pub fn check_token(&self, kind: TokenKind) -> bool {
    self.peek().is_some() && self.peek().unwrap().kind == kind
  }

  /// Advance to the next token.
  pub fn advance(&mut self) -> Result<(), ParserError> {
    if !self.is_at_end() {
      self.current += 1;
    }
    Ok(())
  }

  /// Assert that there is input to parse.
  pub fn assert_non_empty(&self) -> Result<(), ParserError> {
    if self.tokens.is_empty() {
      return Err(ParserError::NoInput);
    }
    if self.is_at_end() {
      return Err(ParserError::NoInput);
    }
    Ok(())
  }

  /// Check if at end of input.
  pub fn is_at_end(&self) -> bool {
    self.check_token(TokenKind::EndOfInput)
  }

  /// Peek at the current token.
  pub fn peek(&self) -> Option<Token> {
    self.tokens.get(self.current).cloned()
  }

  /// Get the previous token.
  pub fn previous(&self) -> Option<Token> {
    self.tokens.get(self.current - 1).cloned()
  }

  /// Peek at the next token.
  pub fn peek_next(&self) -> Option<Token> {
    self.tokens.get(self.current + 1).cloned()
  }
}

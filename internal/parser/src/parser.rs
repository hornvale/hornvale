use crate::error::ParserError;
use crate::prelude::{IsParserProduct, MagicWordToken, ParserProductKind, QuestionToken, Token, TokenKind, VerbToken};
use derivative::Derivative;
use hecs::{Entity, With, World};
use hornvale_command::prelude::*;
use hornvale_core::prelude::*;
use hornvale_world::prelude::*;

#[cfg(test)]
pub mod tests;

/// The parser, a simple top-down recursive descent parser.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Parser<'world> {
  /// The world to parse in.
  #[derivative(Debug = "ignore")]
  pub world: &'world mut World,
  /// The command registry to use.
  #[derivative(Debug = "ignore")]
  pub command_registry: &'world CommandRegistry,
  /// A list of tokens to parse.
  pub tokens: Vec<Token>,
  /// The actor attempting the command.
  pub actor: Entity,
  /// The current index.
  pub current: usize,
  /// The verb token.
  pub verb: Option<VerbToken>,
  /// The command syntax.
  pub syntax: Option<CommandSyntax>,
}

impl<'world> Parser<'world> {
  /// Create a new parser.
  pub fn new(
    world: &'world mut World,
    command_registry: &'world CommandRegistry,
    tokens: &[Token],
    actor: Entity,
  ) -> Self {
    let tokens = tokens.to_vec();
    let current = 0;
    let verb = None;
    let syntax = None;
    Self {
      world,
      command_registry,
      tokens,
      actor,
      current,
      verb,
      syntax,
    }
  }

  /// Parse a command and its context from the input.
  ///
  /// This is the main entry point for the parser.
  ///
  /// input → command
  pub fn parse(&mut self) -> Result<(), ParserError> {
    self.assert_non_empty()?;
    self.parse_command()?;
    Ok(self.entity)
  }

  /// Parse a command from the input.
  ///
  /// command → verb-phrase (direct-object-phrase)? (indirect-object-phrase)?
  pub fn parse_command(&mut self) -> Result<(), ParserError> {
    self.consume_verb().map_err(|_| ParserError::NoVerb)?;
    self.syntax = Some(CommandSyntax::default());
    self.parse_direct_object_phrase()?;
    self.parse_indirect_object_phrase()?;
    Ok(())
  }

  /// Match current token.
  pub fn match_token_kind(&mut self, kind: TokenKind) -> Result<bool, ParserError> {
    if !self.check_token(kind) {
      return Ok(false);
    }
    Ok(true)
  }

  /// Try to match a token by passing a condition.
  pub fn match_token_condition<F>(&mut self, condition: F) -> Result<bool, ParserError>
  where
    F: Fn(TokenKind) -> bool,
  {
    if !condition(self.peek().map(|t| t.kind).unwrap_or(TokenKind::EndOfInput)) {
      return Ok(false);
    }
    Ok(true)
  }

  /// Parse the direct object phrase, if any.
  pub fn parse_direct_object_phrase(&mut self) -> Result<(), ParserError> {
    if self.match_modifier()? {
      self.consume_direct_object_modifier()?;
    } else if self.match_direct_object()? {
      self.consume_direct_object()?;
    }
    Ok(())
  }

  /// Match the direct object modifier.
  pub fn match_modifier(&mut self) -> Result<bool, ParserError> {
    self.match_token_condition(|kind| kind.is_modifier())
  }

  /// Consume the direct object modifier.
  pub fn consume_direct_object_modifier(&mut self) -> Result<(), ParserError> {
    self.consume_modifier()?;
    self.syntax.as_mut().unwrap().direct_object_modifier = self
      .previous()
      .map(|t| t.kind.try_into().unwrap_or(CommandModifier::None));
    let modifier = self.peek().unwrap().clone();
    self.consume_token(modifier.kind, "Expected a direct object modifier.")?;
    self.consume_direct_object()?;
    Ok(())
  }

  /// Parse the indirect object phrase, if any.

  /// Consume the verb.
  pub fn consume_verb(&mut self) -> Result<(), ParserError> {
    if self.check_token(TokenKind::Verb) {
      self.verb = self.peek().cloned();
      self.advance()?;
      Ok(())
    } else {
      Err(ParserError::NoVerb)
    }
  }

  /// Consume the direct object.
  pub fn consume_direct_object(&mut self) -> Result<(), ParserError> {
    if self.check_token(TokenKind::DirectObject) {
      let direct_object = self.peek().unwrap().clone();
      self.advance()?;
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

  //  /// Bind the magic word.
  //  pub fn bind_magic_word(&mut self) -> Result<(), ParserError> {
  //    // self.function = Some(CommandFunction::MagicWord);
  //    Ok(())
  //  }
  //
  //  /// Bind the command.
  //  ///
  //  /// This is where the parser retrieves the command registry from the World,
  //  /// gets the form of the command, and binds the command function to the
  //  /// parser.
  //  pub fn bind_command(&mut self) -> Result<(), ParserError> {
  //    let name = self.verb.as_ref().unwrap().lexeme.as_str();
  //    if !self.registry.has_command(name) {
  //      return Err(ParserError::UnknownCommand(name.to_string()));
  //    }
  //    let form = self
  //      .modifier
  //      .as_ref()
  //      .map(|t| t.kind.try_into().unwrap_or(CommandForm::Default))
  //      .unwrap_or(CommandForm::Default);
  //    if !registry.has_form(name, &form) {
  //      let forms = registry.get_forms(name).unwrap();
  //      return Err(ParserError::UnknownCommandForm(
  //        name.to_string(),
  //        form.to_string(),
  //        forms.iter().map(|f| f.to_string()).collect(),
  //      ));
  //    }
  //    let command = registry
  //      .get(name, &form)
  //      .ok_or(ParserError::UnknownCommand(name.to_string()))?;
  //    self.function = Some(*command);
  //    Ok(())
  //  }
  //
  //  /// Bind the context.
  //  #[allow(clippy::field_reassign_with_default)]
  //  pub fn bind_context(&mut self) -> Result<(), ParserError> {
  //    let mut context = self.context.take().unwrap_or_default();
  //    context.raw = self
  //      .tokens
  //      .iter()
  //      .map(|t| t.lexeme.clone())
  //      .collect::<Vec<_>>()
  //      .join(" ")
  //      .trim()
  //      .to_string();
  //    context.verb = if let Some(verb) = &self.verb {
  //      verb.lexeme.clone()
  //    } else {
  //      String::new()
  //    };
  //    if let Some(direct_object) = &self.direct_object {
  //      match direct_object {
  //        direct_object if direct_object.kind.is_direction() => {
  //          context.direct_object = Some(direct_object.try_into().unwrap());
  //        },
  //        direct_object if direct_object.kind == TokenKind::Here => {
  //          context.direct_object = Some(self.bind_here()?);
  //        },
  //        _ => {
  //          println!("Direct object: {:?}", direct_object);
  //        },
  //      }
  //    }
  //    context.form = if let Some(modifier) = &self.modifier {
  //      modifier.kind.try_into().unwrap_or(CommandForm::Default)
  //    } else {
  //      CommandForm::Default
  //    };
  //    context.actor = Some(self.actor);
  //    self.context = Some(context);
  //    Ok(())
  //  }

  /// Consume a Here token.
  pub fn consume_here(&mut self) -> Result<(), ParserError> {
    let room = self.world.get_room_entity_containing_entity(self.actor)?;
    self.world.insert(self.entity, (ParserProductKind::Here, room))?;
    Ok(())
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

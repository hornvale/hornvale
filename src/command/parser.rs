use crate::command::error::CommandError;
use crate::command::prelude::*;
use crate::world::prelude::*;
use derivative::Derivative;
use hecs::{Entity, World};

/// Arguments that will be passed to a command.
pub type CommandArgs = (Entity, Option<Entity>, Option<Entity>);

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
  /// The verb token.
  pub verb_token: Option<String>,
  /// The arity of the command (AFAICT).
  pub arity: Option<CommandArity>,
  /// The direct object modifier, if any.
  pub direct_object_modifier: Option<CommandModifier>,
  /// The indirect object modifier, if any.
  pub indirect_object_modifier: Option<CommandModifier>,
  /// The direct object, if any.
  pub direct_object: Option<Entity>,
  /// The indirect object, if any.
  pub indirect_object: Option<Entity>,
  /// The command function, if any.
  pub function: Option<CommandFunction>,
}

impl<'world> Parser<'world> {
  /// Create a new parser.
  pub fn new(tokens: &[Token], actor: Entity, world: &'world mut World) -> Self {
    let tokens = tokens.to_vec();
    let current = 0;
    let verb_token = None;
    let arity = None;
    let direct_object_modifier = None;
    let indirect_object_modifier = None;
    let direct_object = None;
    let indirect_object = None;
    let function = None;
    Self {
      tokens,
      actor,
      world,
      current,
      verb_token,
      arity,
      direct_object_modifier,
      indirect_object_modifier,
      direct_object,
      indirect_object,
      function,
    }
  }

  /// Parse a command and its context from the input.
  ///
  /// This is the main entry point for the parser.
  ///
  /// input → command
  pub fn parse(&mut self) -> Result<(&CommandFunction, CommandArgs), CommandError> {
    self.assert_non_empty()?;
    self.parse_command()?;
    self.bind_command()?;
    Ok((
      &self.function.as_ref().unwrap(),
      (self.actor, self.direct_object, self.indirect_object),
    ))
  }

  /// Parse a command from the input.
  ///
  /// command → verb-phrase (object-phrase)?
  pub fn parse_command(&mut self) -> Result<(), CommandError> {
    self.consume_verb().map_err(|_| CommandError::NoVerb)?;
    println! {"Verb: {:?}", self.verb_token};
    match self.peek() {
      Some(token) if token.kind == TokenKind::CommandModifier(CommandModifier::Here) => {
        println!("Here!");
        self.direct_object_modifier = Some(CommandModifier::Here);
        // self.direct_object = Some(self.bind_here()?);
        self.advance()?;
      },
      Some(token) if token.kind.is_direction() => {
        self.direct_object = self.bind_direction(self.peek().unwrap())?;
        self.advance()?;
      },
      _ => {
        println!("WAT: {:?}", self.peek());
      },
    }
    Ok(())
  }

  /// Match current token.
  pub fn match_token(&mut self, kind: TokenKind) -> Result<bool, CommandError> {
    if !self.check_token(kind) {
      return Ok(false);
    }
    self.advance()?;
    Ok(true)
  }

  /// Match a magic word.
  pub fn match_magic_word(&mut self) -> Result<bool, CommandError> {
    self.peek().map(|t| t.kind.is_magic_word()).ok_or(CommandError::NoInput)
  }

  /// Consume the verb.
  pub fn consume_verb(&mut self) -> Result<(), CommandError> {
    if self.check_token(TokenKind::Word(Word::Verb)) {
      self.verb_token = self.peek().map(|t| t.lexeme.clone());
      self.advance()?;
      Ok(())
    } else {
      Err(CommandError::NoVerb)
    }
  }

  /// Consume the current token, throwing an error if it is not the expected kind.
  pub fn consume_token(&mut self, expected: TokenKind, message: &str) -> Result<(), CommandError> {
    let current_kind = self.peek().map(|t| t.kind).unwrap_or(TokenKind::EndOfInput);
    if current_kind == expected {
      self.advance()?;
      Ok(())
    } else {
      Err(CommandError::UnexpectedToken(
        expected,
        current_kind,
        message.to_string(),
      ))
    }
  }

  /// Consume a magic word.
  pub fn consume_magic_word(&mut self) -> Result<(), CommandError> {
    if self.match_magic_word()? {
      self.consume_token(self.peek().unwrap().kind, "Expected a magic word")
    } else {
      Err(CommandError::NoInput)
    }
  }

  /// Bind the magic word.
  pub fn bind_magic_word(&mut self) -> Result<(), CommandError> {
    // self.function = Some(CommandFunction::MagicWord);
    Ok(())
  }

  /// Bind the command.
  ///
  /// This is where the parser retrieves the command registry from the World,
  /// gets the form of the command, and binds the command function to the
  /// parser.
  pub fn bind_command(&mut self) -> Result<(), CommandError> {
    let registry = self
      .world
      .query_mut::<&mut CommandRegistry>()
      .into_iter()
      .next()
      .unwrap()
      .1;
    let name = self.verb_token.as_ref().unwrap();
    if !registry.has_command(name) {
      return Err(CommandError::UnknownCommand(name.to_string()));
    }
    let syntax = CommandSyntax {
      arity: self.arity.unwrap_or(CommandArity::Nullary),
      direct_object_modifier: self.direct_object_modifier,
      indirect_object_modifier: self.indirect_object_modifier,
    };
    if !registry.has_syntax(name, &syntax) {
      let forms = registry.get_syntaxes(name).unwrap();
      return Err(CommandError::UnknownCommandModifier(
        name.to_string(),
        syntax.to_string(),
        forms.iter().map(|f| f.to_string()).collect(),
      ));
    }
    let command = registry
      .get(name, &syntax)
      .ok_or(CommandError::UnknownCommand(name.to_string()))?;
    self.function = Some(*command);
    Ok(())
  }

  /// Bind the Here token.
  pub fn bind_here(&mut self) -> Result<Entity, CommandError> {
    let room_entity = self.world.get_room_entity_containing_entity(self.actor)?;
    Ok(room_entity)
  }

  /// Bind a specified direction.
  pub fn bind_direction(&mut self, token: Token) -> Result<Option<Entity>, CommandError> {
    self.arity = match self.arity {
      Some(CommandArity::Nullary) => Some(CommandArity::Unary),
      Some(CommandArity::Unary) => Some(CommandArity::Binary),
      Some(CommandArity::Binary) => Some(CommandArity::Binary),
      None => Some(CommandArity::Unary),
    };
    let direction = match token.kind {
      TokenKind::Direction(direction) => direction,
      _ => return Err(CommandError::UnknownError),
    };
    let direction = PassageDirection(direction);
    let (region, room) = self.world.get_region_and_room_containing_entity(self.actor)?;
    let passage = self
      .world
      .get_room_passage_entity_in_direction(&region, &room, &direction)?;
    Ok(Some(passage))
  }

  /// Check kind of current token.
  pub fn check_token(&self, kind: TokenKind) -> bool {
    self.peek().is_some() && self.peek().unwrap().kind == kind
  }

  /// Advance to the next token.
  pub fn advance(&mut self) -> Result<(), CommandError> {
    if !self.is_at_end() {
      self.current += 1;
    }
    Ok(())
  }

  /// Assert that there is input to parse.
  pub fn assert_non_empty(&self) -> Result<(), CommandError> {
    if self.tokens.is_empty() {
      return Err(CommandError::NoInput);
    }
    if self.is_at_end() {
      return Err(CommandError::NoInput);
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

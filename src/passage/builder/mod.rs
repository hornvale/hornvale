use crate::entity_id::PassageId;
use crate::entity_id::RoomId;
use crate::passage::Passage;
use crate::passage::PassageDirection;
use crate::passage::PassageType;

/// The `PassageBuilder` struct.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Builder {
  /// The `PassageId`.
  pub id: Option<PassageId>,
  /// The `PassageType`.
  pub passage_type: Option<PassageType>,
  /// The `PassageDirection`.
  pub direction: Option<PassageDirection>,
  /// The `Passage`'s name.
  pub name: Option<String>,
  /// The `Passage`'s description.
  pub description: Option<String>,
  /// The `Passage`'s origin.
  pub origin: Option<RoomId>,
  /// The `Passage`'s destination.
  pub destination: Option<RoomId>,
}

impl Builder {
  /// Creates a new `PassageBuilder`.
  pub fn new() -> Self {
    Self::default()
  }

  /// Sets the `PassageId`.
  pub fn id(mut self, id: &PassageId) -> Self {
    self.id = Some(id.clone());
    self
  }

  /// Sets the `PassageType`.
  pub fn passage_type(mut self, passage_type: &PassageType) -> Self {
    self.passage_type = Some(passage_type.clone());
    self
  }

  /// Sets the `PassageDirection`.
  pub fn direction(mut self, direction: &PassageDirection) -> Self {
    self.direction = Some(direction.clone());
    self
  }

  /// Sets the `Passage`'s name.
  pub fn name(mut self, name: &str) -> Self {
    self.name = Some(name.to_string());
    self
  }

  /// Sets the `Passage`'s description.
  pub fn description(mut self, description: &str) -> Self {
    self.description = Some(description.to_string());
    self
  }

  /// Sets the `Passage`'s origin.
  pub fn origin(mut self, origin: &RoomId) -> Self {
    self.origin = Some(origin.clone());
    self
  }

  /// Sets the `Passage`'s destination.
  pub fn destination(mut self, destination: &RoomId) -> Self {
    self.destination = Some(destination.clone());
    self
  }

  /// Builds the `Passage`.
  pub fn build(self) -> Passage {
    Passage::new(
      self.id.unwrap_or_default(),
      self.passage_type.unwrap_or_default(),
      self.direction.unwrap_or_default(),
      self.name.unwrap_or_else(|| "Default Passage".to_string()),
      self
        .description
        .unwrap_or_else(|| "This is the default passage.".to_string()),
      self.origin.unwrap_or_default(),
      self.destination.unwrap_or_default(),
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_passage_builder_new() {
    init();
    let passage_builder = Builder::new();
    assert_eq!(passage_builder.id, None);
    assert_eq!(passage_builder.passage_type, None);
    assert_eq!(passage_builder.direction, None);
    assert_eq!(passage_builder.name, None);
    assert_eq!(passage_builder.description, None);
    assert_eq!(passage_builder.origin, None);
    assert_eq!(passage_builder.destination, None);
  }

  #[test]
  fn test_passage_builder_id() {
    init();
    let passage_builder = Builder::new().id(&PassageId::default());
    assert_ne!(passage_builder.id, Some(PassageId::default()));
  }

  #[test]
  fn test_passage_builder_type() {
    init();
    let passage_builder = Builder::new().passage_type(&PassageType::default());
    assert_eq!(passage_builder.passage_type, Some(PassageType::default()));
  }

  #[test]
  fn test_passage_builder_direction() {
    init();
    let passage_builder = Builder::new().direction(&PassageDirection::default());
    assert_eq!(passage_builder.direction, Some(PassageDirection::default()));
  }

  #[test]
  fn test_passage_builder_name() {
    init();
    let passage_builder = Builder::new().name("Test Passage");
    assert_eq!(passage_builder.name, Some("Test Passage".to_string()));
  }

  #[test]
  fn test_passage_builder_description() {
    init();
    let passage_builder = Builder::new().description("This is a test passage.");
    assert_eq!(passage_builder.description, Some("This is a test passage.".to_string()));
  }

  #[test]
  fn test_passage_builder_origin() {
    init();
    let passage_builder = Builder::new().origin(&RoomId::default());
    assert_ne!(passage_builder.origin, Some(RoomId::default()));
  }

  #[test]
  fn test_passage_builder_destination() {
    init();
    let passage_builder = Builder::new().destination(&RoomId::default());
    assert_ne!(passage_builder.destination, Some(RoomId::default()));
  }

  #[test]
  fn test_passage_builder_build() {
    init();
    let passage = Builder::new().build();
    assert_ne!(passage.id, PassageId::default());
    assert_eq!(passage.passage_type, PassageType::default());
    assert_eq!(passage.direction, PassageDirection::default());
    assert_eq!(passage.name, "Default Passage");
    assert_eq!(passage.description, "This is the default passage.");
    assert_ne!(passage.origin, RoomId::default());
    assert_ne!(passage.destination, RoomId::default());
  }
}

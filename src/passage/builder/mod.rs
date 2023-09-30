use crate::entity_id::PassageId;
use crate::entity_id::RoomId;
use crate::passage::PassageType;

/// The `PassageBuilder` struct.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Builder {
  /// The `PassageId`.
  id: Option<PassageId>,
  /// The `PassageType`.
  r#type: Option<PassageType>,
  /// The `Passage`'s name.
  name: Option<String>,
  /// The `Passage`'s description.
  description: Option<String>,
  /// The `Passage`'s origin.
  origin: Option<RoomId>,
  /// The `Passage`'s destination.
  destination: Option<RoomId>,
}

impl Builder {

  /// Creates a new `PassageBuilder`.
  pub fn new() -> Self {
    Self::default()
  }

  /// Sets the `PassageId`.
  pub fn id(mut self, id: PassageId) -> Self {
    self.id = Some(id);
    self
  }

  /// Sets the `PassageType`.
  pub fn r#type(mut self, r#type: PassageType) -> Self {
    self.r#type = Some(r#type);
    self
  }

  /// Sets the `Passage`'s name.
  pub fn name(mut self, name: String) -> Self {
    self.name = Some(name);
    self
  }

  /// Sets the `Passage`'s description.
  pub fn description(mut self, description: String) -> Self {
    self.description = Some(description);
    self
  }

  /// Sets the `Passage`'s origin.
  pub fn origin(mut self, origin: RoomId) -> Self {
    self.origin = Some(origin);
    self
  }

  /// Sets the `Passage`'s destination.
  pub fn destination(mut self, destination: RoomId) -> Self {
    self.destination = Some(destination);
    self
  }

  /// Builds the `Passage`.
  pub fn build(self) -> crate::passage::Passage {
    crate::passage::Passage::new(
      self.id.unwrap_or_default(),
      self.r#type.unwrap_or_default(),
      self.name.unwrap_or_default(),
      self.description.unwrap_or_default(),
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
    assert_eq!(passage_builder.r#type, None);
    assert_eq!(passage_builder.name, None);
    assert_eq!(passage_builder.description, None);
    assert_eq!(passage_builder.origin, None);
    assert_eq!(passage_builder.destination, None);
  }

  #[test]
  fn test_passage_builder_id() {
    init();
    let passage_builder = Builder::new().id(PassageId::default());
    assert_eq!(passage_builder.id, Some(PassageId::default()));
  }

  #[test]
  fn test_passage_builder_type() {
    init();
    let passage_builder = Builder::new().r#type(PassageType::default());
    assert_eq!(passage_builder.r#type, Some(PassageType::default()));
  }

  #[test]
  fn test_passage_builder_name() {
    init();
    let passage_builder = Builder::new().name("Test Passage".to_string());
    assert_eq!(passage_builder.name, Some("Test Passage".to_string()));
  }

  #[test]
  fn test_passage_builder_description() {
    init();
    let passage_builder = Builder::new().description("This is a test passage.".to_string());
    assert_eq!(passage_builder.description, Some("This is a test passage.".to_string()));
  }

  #[test]
  fn test_passage_builder_origin() {
    init();
    let passage_builder = Builder::new().origin(RoomId::default());
    assert_eq!(passage_builder.origin, Some(RoomId::default()));
  }

  #[test]
  fn test_passage_builder_destination() {
    init();
    let passage_builder = Builder::new().destination(RoomId::default());
    assert_eq!(passage_builder.destination, Some(RoomId::default()));
  }

  #[test]
  fn test_passage_builder_build() {
    init();
    let passage = Builder::new().build();
    assert_eq!(passage.id, PassageId::default());
    assert_eq!(passage.r#type, PassageType::default());
    assert_eq!(passage.name, "Default Passage");
    assert_eq!(passage.description, "This is the default passage.");
    assert_eq!(passage.origin, RoomId::default());
    assert_eq!(passage.destination, RoomId::default());
  }
}

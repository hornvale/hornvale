use crate::entity_id::RoomId;
use crate::passage::Passage;
use crate::passage::PassageBuilder;
use crate::passage::PassageId;
use crate::passage::PassageType;

/// The `PassageBidirectionalBuilder` struct.
///
/// This wraps the `PassageBuilder` struct and builds bidirectional `Passage`s.
#[derive(Debug, Default)]
pub struct BidirectionalBuilder {
  /// The first `PassageBuilder`.
  pub builder1: PassageBuilder,
  /// The second `PassageBuilder`.
  pub builder2: PassageBuilder,
}

impl BidirectionalBuilder {
  /// Creates a new `PassageBidirectionalBuilder`.
  pub fn new() -> Self {
    Self {
      builder1: PassageBuilder::new(),
      builder2: PassageBuilder::new(),
    }
  }

  pub fn origin(&mut self, origin: &RoomId) -> &mut Self {
    self.builder1.origin = Some(origin.clone());
    self.builder2.destination = Some(origin.clone());
    self
  }

  pub fn destination(&mut self, destination: &RoomId) -> &mut Self {
    self.builder1.destination = Some(destination.clone());
    self.builder2.origin = Some(destination.clone());
    self
  }

  pub fn passage_type(&mut self, passage_type: &PassageType) -> &mut Self {
    self.builder1.passage_type = Some(passage_type.clone());
    self.builder2.passage_type = Some(passage_type.clone());
    self
  }

  pub fn id(&mut self, id: &PassageId) -> &mut Self {
    self.builder1.id = Some(id.clone());
    self.builder2.id = Some(id.clone());
    self
  }

  pub fn name(&mut self, name: &str) -> &mut Self {
    self.builder1.name = Some(name.to_string());
    self.builder2.name = Some(name.to_string());
    self
  }

  pub fn description(&mut self, description: &str) -> &mut Self {
    self.builder1.description = Some(description.to_string());
    self.builder2.description = Some(description.to_string());
    self
  }

  pub fn build(&self) -> (Passage, Passage) {
    (self.builder1.clone().build(), self.builder2.clone().build())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_bidirectional_builder() {
    let origin = RoomId::new();
    let destination = RoomId::new();
    let (passage1, passage2) = BidirectionalBuilder::new()
      .origin(&origin)
      .destination(&destination)
      .passage_type(&PassageType::default())
      .id(&PassageId::default())
      .name("Test Passage")
      .description("This is a test passage.")
      .build();
    assert_eq!(passage1.origin, origin);
    assert_eq!(passage1.destination, destination);
    assert_eq!(passage2.origin, destination);
    assert_eq!(passage2.destination, origin);
  }
}

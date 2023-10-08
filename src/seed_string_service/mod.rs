use uuid::Uuid;

/// The `SeedString` service.
#[derive(Clone, Debug)]
pub struct SeedStringService {
  /// The seed string.
  seed_string: String,
}

impl SeedStringService {
  /// Creates a new `SeedString`.
  pub fn new(seed_string: &str) -> Self {
    Self {
      seed_string: seed_string.to_string(),
    }
  }

  /// Returns the seed string.
  pub fn get_seed_string(&self) -> &str {
    &self.seed_string
  }

  /// Sets the seed string.
  pub fn set_seed_string(&mut self, seed_string: &str) {
    self.seed_string = seed_string.to_string();
  }

  /// Generate a new seed string.
  pub fn generate_seed_string(&mut self) {
    self.seed_string = Uuid::new_v4().to_string();
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    let seed_string = SeedStringService::new("");
    assert_eq!(seed_string.get_seed_string(), "");
  }

  #[test]
  fn test_set_seed_string() {
    let mut seed_string = SeedStringService::new("");
    seed_string.set_seed_string("test");
    assert_eq!(seed_string.get_seed_string(), "test");
  }

  #[test]
  fn test_generate_seed_string() {
    let mut seed_string = SeedStringService::new("");
    seed_string.generate_seed_string();
    assert_eq!(seed_string.get_seed_string().len(), 36);
  }
}

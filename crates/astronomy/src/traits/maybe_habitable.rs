use crate::error::AstronomyError;

/// The `MaybeHabitable` trait, used to determine if something is habitable.
pub trait MaybeHabitable {
  /// Check if the object is habitable.
  fn check_habitability(&self) -> Result<(), AstronomyError>;
  /// Determine if the object is habitable.
  fn is_habitable(&self) -> bool {
    self.check_habitability().is_ok()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_is_habitable() {
    init();
    struct Test {
      input: Result<(), AstronomyError>,
      expected: bool,
    }

    let tests = vec![
      Test {
        input: Ok(()),
        expected: true,
      },
      Test {
        input: Err(AstronomyError::UnknownError),
        expected: false,
      },
    ];

    impl MaybeHabitable for Test {
      fn check_habitability(&self) -> Result<(), AstronomyError> {
        self.input
      }
    }

    for test in tests {
      let result = test.check_habitability().is_ok();
      assert_eq!(result, test.expected);
    }
  }
}

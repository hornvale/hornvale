use crate::prelude::InputError;
use crate::prelude::InputSource;
use std::fs::File;
use std::io::{BufRead, BufReader, StdinLock};

/// An input source that reads from a generic reader.
#[derive(Debug)]
pub struct GenericSource<R> {
  reader: R,
}

impl<R: BufRead> GenericSource<R> {
  /// Create a new GenericSource with the given reader.
  pub fn new(reader: R) -> Self {
    Self { reader }
  }
}

impl<R: BufRead> InputSource for GenericSource<R> {
  fn fetch_input(&mut self) -> Result<String, InputError> {
    let mut input = String::new();
    self.reader.read_line(&mut input)?;
    Ok(input.trim().to_string())
  }
}

/// A type alias for a GenericSource that reads from standard input.
pub type StdinSource = GenericSource<StdinLock<'static>>;

impl Default for StdinSource {
  fn default() -> Self {
    Self::new(std::io::stdin().lock())
  }
}

/// A type alias for a GenericSource that reads lines from a file.
pub type FileSource = GenericSource<BufReader<File>>;

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::*;
  use std::io::BufReader;
  use std::io::Write;
  use tempfile::NamedTempFile;

  #[test]
  fn test_fetch_input() {
    init();
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "test").unwrap();
    file.flush().unwrap();

    let file = file.reopen().unwrap();
    let file_source = GenericSource::new(BufReader::new(file));
    let mut input_source = file_source;
    let input = input_source.fetch_input().unwrap();
    assert_eq!(input, "test");
  }
}

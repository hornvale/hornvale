use crate::prelude::{OutputError, OutputSink};
use std::fs::File;
use std::io::{BufWriter, Stderr, Stdout, Write};

/// An output sink that writes to a generic writer.
#[derive(Debug)]
pub struct GenericSink<W> {
  writer: W,
}

impl<W: Write> GenericSink<W> {
  /// Create a new GenericSink with the given writer.
  pub fn new(writer: W) -> Self {
    Self { writer }
  }
}

impl<W: Write> OutputSink for GenericSink<W> {
  fn send_output(&mut self, output: String) -> Result<(), OutputError> {
    writeln!(self.writer, "{}", output)?;
    Ok(())
  }
}

/// A type alias for a GenericSink that writes to standard output.
pub type StdoutSink = GenericSink<Stdout>;

/// A type alias for a GenericSink that writes to standard error.
pub type StderrSink = GenericSink<Stderr>;

/// A type alias for a GenericSink that writes lines to a file.
pub type FileSink = GenericSink<BufWriter<File>>;

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;
  use std::io::BufWriter;
  use std::io::Write;
  use tempfile::NamedTempFile;

  #[test]
  fn test_send_output() {
    init();
    let file = NamedTempFile::new().unwrap();
    let mut sink = GenericSink::new(BufWriter::new(file.reopen().unwrap()));
    sink.send_output("test".to_string()).unwrap();
    sink.send_output("test2".to_string()).unwrap();
    sink.writer.flush().unwrap();
    let contents = std::fs::read_to_string(file.path()).unwrap();
    assert_eq!(contents, "test\ntest2\n");
  }

  #[test]
  fn test_send_output_stdout() {
    init();
    let mut sink = StdoutSink::new(std::io::stdout());
    sink.send_output("test".to_string()).unwrap();
  }

  #[test]
  fn test_send_output_stderr() {
    init();
    let mut sink = StderrSink::new(std::io::stderr());
    sink.send_output("test".to_string()).unwrap();
  }

  #[test]
  fn test_send_output_file() {
    init();
    let file = NamedTempFile::new().unwrap();
    let mut sink = FileSink::new(BufWriter::new(file.reopen().unwrap()));
    sink.send_output("test".to_string()).unwrap();
    sink.send_output("test2".to_string()).unwrap();
    sink.writer.flush().unwrap();
    let contents = std::fs::read_to_string(file.path()).unwrap();
    assert_eq!(contents, "test\ntest2\n");
  }
}

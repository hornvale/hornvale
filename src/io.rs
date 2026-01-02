//! I/O abstraction for the world VM.
//!
//! All input/output goes through the `WorldIO` trait, enabling:
//! - Testing with mock I/O
//! - Network play
//! - Replay verification

use std::io::{self, BufRead, Write};

/// Abstraction for world I/O operations.
///
/// Implementations handle reading input and writing output.
/// This allows the world to be run with different I/O backends
/// (terminal, test harness, network, etc.)
pub trait WorldIO {
    /// Print a message to the output.
    fn print(&mut self, message: &str);

    /// Print a message followed by a newline.
    fn println(&mut self, message: &str) {
        self.print(message);
        self.print("\n");
    }

    /// Read a line of input.
    ///
    /// Returns `None` if input is exhausted (EOF).
    fn read_line(&mut self) -> Option<String>;

    /// Display a prompt and read input.
    fn prompt(&mut self, prompt: &str) -> Option<String> {
        self.print(prompt);
        self.read_line()
    }
}

/// Standard I/O implementation using stdin/stdout.
pub struct StdIO {
    stdin: io::Stdin,
    stdout: io::Stdout,
}

impl StdIO {
    /// Create a new StdIO instance.
    pub fn new() -> Self {
        Self {
            stdin: io::stdin(),
            stdout: io::stdout(),
        }
    }
}

impl Default for StdIO {
    fn default() -> Self {
        Self::new()
    }
}

impl WorldIO for StdIO {
    fn print(&mut self, message: &str) {
        let _ = write!(self.stdout, "{message}");
        let _ = self.stdout.flush();
    }

    fn read_line(&mut self) -> Option<String> {
        let mut line = String::new();
        match self.stdin.lock().read_line(&mut line) {
            Ok(0) => None, // EOF
            Ok(_) => {
                // Remove trailing newline
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Some(line)
            }
            Err(_) => None,
        }
    }
}

/// Test I/O implementation with predefined input and captured output.
#[cfg(test)]
pub struct TestIO {
    input: Vec<String>,
    input_index: usize,
    pub output: String,
}

#[cfg(test)]
impl TestIO {
    /// Create a TestIO with the given input lines.
    pub fn new(input: Vec<&str>) -> Self {
        Self {
            input: input.into_iter().map(String::from).collect(),
            input_index: 0,
            output: String::new(),
        }
    }
}

#[cfg(test)]
impl WorldIO for TestIO {
    fn print(&mut self, message: &str) {
        self.output.push_str(message);
    }

    fn read_line(&mut self) -> Option<String> {
        if self.input_index < self.input.len() {
            let line = self.input[self.input_index].clone();
            self.input_index += 1;
            Some(line)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_test_io() {
        let mut io = TestIO::new(vec!["hello", "world"]);

        io.println("Output line");
        assert_eq!(io.output, "Output line\n");

        assert_eq!(io.read_line(), Some("hello".to_string()));
        assert_eq!(io.read_line(), Some("world".to_string()));
        assert_eq!(io.read_line(), None);
    }
}

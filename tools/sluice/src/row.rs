//! One queue row. The TSV is the durable format and this type is its only
//! parser; see the plan's Global Constraints for the column order.

/// A single request in the queue.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Row {
    /// RFC3339 stamp written when the row was added.
    pub when: String,
    /// `req-<sha12>-<stamp>`, the row's exact identity.
    pub id: String,
    /// The branch the request names.
    pub branch: String,
    /// The full 40-character ref.
    pub sha: String,
    /// One of the closed state vocabulary.
    pub state: String,
    /// One of `merge`, `stage`, `census`.
    pub kind: String,
    /// Free text, already sanitised.
    pub note: String,
}

impl Row {
    /// Parse one TSV line. TOTAL — never returns `None` for a short line.
    ///
    /// The shell it replaces PADS a malformed row out to seven fields and
    /// keeps it; measured 2026-09-05 by feeding `set-state` a three-field
    /// line and watching it survive as `TRUNCATED\tonly\tthree\t\t\tmerge\t`.
    /// Dropping such a line here would make the next `write_rows` delete it
    /// permanently, silently losing a request — which the plan's own Global
    /// Constraints forbid (the format does not change) and which is the exact
    /// opposite of a queue whose first duty is durability.
    pub fn parse(line: &str) -> Option<Row> {
        let f: Vec<&str> = line.split('\t').collect();
        let g = |i: usize| f.get(i).copied().unwrap_or("");
        let kind = if g(5).is_empty() { "merge" } else { g(5) };
        Some(Row {
            when: g(0).to_string(),
            id: g(1).to_string(),
            branch: g(2).to_string(),
            sha: g(3).to_string(),
            state: g(4).to_string(),
            kind: kind.to_string(),
            note: g(6).to_string(),
        })
    }

    /// Render back to one TSV line, no trailing newline.
    pub fn render(&self) -> String {
        format!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}",
            self.when, self.id, self.branch, self.sha, self.state, self.kind, self.note
        )
    }
}

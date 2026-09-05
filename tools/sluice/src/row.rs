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
    /// Parse one TSV line. Returns `None` if it has fewer than six fields.
    pub fn parse(line: &str) -> Option<Row> {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() < 6 {
            return None;
        }
        let kind = if f[5].is_empty() { "merge" } else { f[5] };
        Some(Row {
            when: f[0].to_string(),
            id: f[1].to_string(),
            branch: f[2].to_string(),
            sha: f[3].to_string(),
            state: f[4].to_string(),
            kind: kind.to_string(),
            note: f.get(6).copied().unwrap_or("").to_string(),
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

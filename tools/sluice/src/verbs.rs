//! The queue's verbs. Each takes its `Store` explicitly.

use crate::store::Store;

/// Why `set_state` refused.
#[derive(Debug)]
pub enum SetStateError {
    /// The state is outside the closed vocabulary.
    BadState,
    /// No row carries that id. NOTHING was changed.
    NoSuchRow,
    /// The store could not be read or written.
    Io(std::io::Error),
}

/// Tab, CR and LF each become one space. Strip, never reject: a tab shifts
/// every later field, and a newline is read as a whole separate row by the
/// next reader and re-emitted forever.
pub fn sanitize_note(s: &str) -> String {
    s.replace(['\t', '\r', '\n'], " ")
}

/// The closed state vocabulary.
pub fn validate_state(s: &str) -> bool {
    matches!(
        s,
        "queued" | "running" | "held" | "landed" | "reported" | "superseded" | "dropped"
    )
}

/// The closed kind vocabulary.
pub fn validate_kind(s: &str) -> bool {
    matches!(s, "merge" | "stage" | "census")
}

/// Set one row's state, and its note when `note` is `Some` and non-empty.
pub fn set_state(
    store: &Store,
    id: &str,
    state: &str,
    note: Option<&str>,
) -> Result<(), SetStateError> {
    if !validate_state(state) {
        return Err(SetStateError::BadState);
    }
    // THE LOCK IS NOT OPTIONAL HERE. `scripts/sluice-queue.sh`'s `set-state`
    // takes `with_lock` before its rewrite; an earlier draft of this plan
    // dropped it, which would have let an unlocked set-state race a locked
    // claim through the same read-modify-write and the same temp path.
    let _guard = store.lock().map_err(SetStateError::Io)?;
    let mut rows = store.read_rows().map_err(SetStateError::Io)?;
    let mut matched = false;
    for r in rows.iter_mut() {
        if r.id == id {
            matched = true;
            r.state = state.to_string();
            if let Some(n) = note {
                let n = sanitize_note(n);
                if !n.is_empty() {
                    r.note = n;
                }
            }
        }
    }
    if !matched {
        return Err(SetStateError::NoSuchRow);
    }
    store.write_rows(&rows).map_err(SetStateError::Io)
}

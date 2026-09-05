//! The queue's verbs. Each takes its `Store` explicitly.

use crate::row::Row;
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

/// Why `claim` refused.
#[derive(Debug)]
pub enum ClaimError {
    /// A row for that ref exists but is not `queued` — somebody else has it.
    /// Exit code 4.
    HeldByAnother,
    /// No row at all for that ref. Exit code 5; an ad hoc run, not a race.
    NoSuchRow,
    /// The store could not be read or written.
    Io(std::io::Error),
}

/// Atomically select a `queued` row and mark it `running`, returning it.
///
/// Selecting and marking happen under ONE lock acquisition, which is the whole
/// point: the shell version released its lock between `next` and `set-state`,
/// so two dispatchers both saw an unclaimed row and one merge ran twice.
///
/// With `sha`, claims the row for that ref (an executor, which knows its ref
/// and not its id). Without, claims the first queued row (a dispatcher).
pub fn claim(
    store: &Store,
    sha: Option<&str>,
    note: Option<&str>,
) -> Result<Option<Row>, ClaimError> {
    let _guard = store.lock().map_err(ClaimError::Io)?;
    let mut rows = store.read_rows().map_err(ClaimError::Io)?;

    // A SHA THAT IS ALREADY RUNNING IS NOT CLAIMABLE UNDER ANY ROW.
    //
    // The interlock this verb exists to provide is per-ROW, and that is one
    // row short of the guarantee callers actually need. Two rows can name the
    // SAME sha — a resubmission under a second branch name, or a stage request
    // and a merge request for one ref — and coalescing does not merge them,
    // because it is scoped to branch AND kind. With one of them `running` and
    // the other `queued`, the loop below finds the queued one, marks it, and
    // the dispatcher launches a SECOND chamber run against a ref already being
    // merged. That is the 2026-09-04 duplicate by a third road: not `next` +
    // `set-state` across two transactions, and not an unclaimed `req-` run,
    // but two legitimate rows for one commit.
    //
    // Observed 2026-09-05: two rows for e8692ba5bb50, one running and one
    // queued, cleared by hand before a drain could take the second.
    //
    // Refusing here rather than in the dispatcher is deliberate — this is the
    // only place that both holds the lock and sees every row, so it is the only
    // place the answer cannot be stale by the time it is used.
    // THERE IS NO SEPARATE BY-SHA REFUSAL HERE, AND THAT IS LOAD-BEARING
    // RATHER THAN AN OMISSION. One was written first and mutation testing
    // showed it unfalsifiable: with the skip below in place, a by-sha claim
    // whose twin is running finds no selectable row, and `seen_sha` is true,
    // so the existing `HeldByAnother` arm already answers it. Removing the
    // early return left all sixteen tests green; removing the skip kills them.
    // Do not add it back — it would be a second derivation of one answer, with
    // nothing able to tell you when the two stopped agreeing.
    let running_shas: Vec<String> = rows
        .iter()
        .filter(|r| r.state == "running")
        .map(|r| r.sha.clone())
        .collect();

    let mut seen_sha = false;
    let mut idx = None;
    for (i, r) in rows.iter().enumerate() {
        if let Some(want) = sha
            && r.sha == want
        {
            seen_sha = true;
        }
        // The no-sha (dispatcher) form skips a queued row whose sha is already
        // running and keeps looking, rather than refusing: the queue may hold
        // other, unrelated work it should get on with.
        if idx.is_none()
            && r.state == "queued"
            && !running_shas.iter().any(|s| *s == r.sha)
            && sha.map(|w| r.sha == w).unwrap_or(true)
        {
            idx = Some(i);
        }
    }
    let Some(i) = idx else {
        if sha.is_some() && seen_sha {
            return Err(ClaimError::HeldByAnother);
        }
        if sha.is_some() {
            return Err(ClaimError::NoSuchRow);
        }
        return Ok(None);
    };
    rows[i].state = "running".to_string();
    if let Some(n) = note {
        let n = sanitize_note(n);
        if !n.is_empty() {
            rows[i].note = n;
        }
    }
    let claimed = rows[i].clone();
    store.write_rows(&rows).map_err(ClaimError::Io)?;
    Ok(Some(claimed))
}

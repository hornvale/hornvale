use sluice::row::Row;

#[test]
fn a_row_round_trips_through_render_and_parse() {
    let line = "2026-09-05T00:00:00Z\treq-abc-1\tcampaign/x\tdeadbeef\tqueued\tmerge\ta note";
    let r = Row::parse(line).expect("parses");
    assert_eq!(r.state, "queued");
    assert_eq!(r.note, "a note");
    assert_eq!(r.render(), line);
}

#[test]
fn an_empty_kind_normalises_to_merge() {
    let line = "2026-09-05T00:00:00Z\treq-abc-1\tcampaign/x\tdeadbeef\tqueued\t\t";
    assert_eq!(Row::parse(line).expect("parses").kind, "merge");
}

use sluice::store::Store;
use std::path::PathBuf;

fn scratch(name: &str) -> PathBuf {
    let d = std::env::temp_dir().join(format!("sluice-test-{}-{}", name, std::process::id()));
    let _ = std::fs::remove_dir_all(&d);
    d
}

#[test]
fn rows_survive_a_write_then_read() {
    let s = Store::new(scratch("roundtrip")).expect("store");
    let r = Row::parse("2026-09-05T00:00:00Z\treq-a-1\tb\tsha\tqueued\tmerge\t").expect("row");
    s.write_rows(std::slice::from_ref(&r)).expect("write");
    assert_eq!(s.read_rows().expect("read"), vec![r]);
}

#[test]
fn a_missing_queue_file_reads_as_empty_not_an_error() {
    let s = Store::new(scratch("missing")).expect("store");
    assert!(s.read_rows().expect("read").is_empty());
}

#[test]
fn a_short_line_is_padded_and_kept_never_dropped() {
    let r = Row::parse("TRUNCATED\tonly\tthree").expect("kept, not dropped");
    assert_eq!(r.when, "TRUNCATED");
    assert_eq!(r.state, "");
    assert_eq!(r.kind, "merge");
}

#[test]
fn a_long_line_keeps_everything_past_the_seventh_field() {
    // Fix round 2, Important F4: a bare `split('\t')` handed an 8+-field
    // line straight to `f.get(6)`, keeping only the 7th piece and silently
    // dropping everything after it. `splitn(7, ...)` must fold every tab
    // from the 7th onward into `note`, exactly as bash's own
    // `read -r ... rnote` absorbs the remainder — so the whole line,
    // including its embedded tab, round-trips through parse -> render
    // unchanged.
    let line = "2026-09-05T00:00:00Z\treq-abc-1\tcampaign/x\tdeadbeef\tqueued\tmerge\ta note\tan eighth field";
    let r = Row::parse(line).expect("parses");
    assert_eq!(r.note, "a note\tan eighth field");
    assert_eq!(r.render(), line);
}

use sluice::verbs::{SetStateError, sanitize_note, set_state};

#[test]
fn a_note_loses_tabs_and_newlines_to_single_spaces() {
    assert_eq!(sanitize_note("a\tb\nc\rd"), "a b c d");
}

#[test]
fn set_state_changes_exactly_the_named_row() {
    let s = Store::new(scratch("setstate")).expect("store");
    s.write_rows(&[
        Row::parse("w\treq-a\tb1\tsha1\tqueued\tmerge\t").unwrap(),
        Row::parse("w\treq-b\tb2\tsha2\tqueued\tmerge\t").unwrap(),
    ])
    .unwrap();
    set_state(&s, "req-b", "running", Some("mine")).expect("ok");
    let rows = s.read_rows().unwrap();
    assert_eq!(rows[0].state, "queued");
    assert_eq!(rows[1].state, "running");
    assert_eq!(rows[1].note, "mine");
}

#[test]
fn an_unmatched_id_is_a_refusal_and_changes_nothing() {
    let s = Store::new(scratch("nomatch")).expect("store");
    let before = vec![Row::parse("w\treq-a\tb\tsha\tqueued\tmerge\t").unwrap()];
    s.write_rows(&before).unwrap();
    let e = set_state(&s, "req-typo", "held", None).expect_err("must refuse");
    assert!(matches!(e, SetStateError::NoSuchRow));
    assert_eq!(s.read_rows().unwrap(), before);
}

use sluice::verbs::{ClaimError, claim};

#[test]
fn claim_marks_the_row_running_in_the_same_call_that_selects_it() {
    let s = Store::new(scratch("claim1")).expect("store");
    s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t").unwrap()])
        .unwrap();
    let got = claim(&s, None, Some("taken")).expect("ok").expect("a row");
    assert_eq!(got.id, "req-a");
    assert_eq!(s.read_rows().unwrap()[0].state, "running");
}

#[test]
fn claim_by_sha_on_a_held_row_refuses_and_changes_nothing() {
    let s = Store::new(scratch("claim4")).expect("store");
    let before = vec![Row::parse("w\treq-a\tb\tsha1\trunning\tmerge\t").unwrap()];
    s.write_rows(&before).unwrap();
    let e = claim(&s, Some("sha1"), None).expect_err("must refuse");
    assert!(matches!(e, ClaimError::HeldByAnother));
    assert_eq!(s.read_rows().unwrap(), before);
}

#[test]
fn claim_by_sha_for_an_absent_ref_is_a_different_answer() {
    let s = Store::new(scratch("claim5")).expect("store");
    s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t").unwrap()])
        .unwrap();
    let e = claim(&s, Some("nope"), None).expect_err("must refuse");
    assert!(matches!(e, ClaimError::NoSuchRow));
}

#[test]
fn racing_claimants_produce_exactly_one_winner_per_queued_row() {
    // 32 rounds x 32 threads = 1024 claim attempts, but never more than 32
    // file descriptors at once. Concurrency is BOUNDED on purpose: this suite
    // gates every merge once Task 4 lands, and macOS defaults `ulimit -n` to
    // 256, so a 1000-thread version would fail on a developer's laptop for a
    // reason that has nothing to do with the property under test. A flaky test
    // in the gate blocks everyone; that happened on 2026-09-05 and once is
    // enough. 32 concurrent is ample: the shell equivalent of this mutation
    // produced 12 winners out of 12.
    for round in 0..32 {
        let s = Store::new(scratch(&format!("claimrace-{round}"))).expect("store");
        s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t").unwrap()])
            .unwrap();
        let winners = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let dir = s.queue_path().parent().expect("dir").to_path_buf();
        let mut hs = Vec::new();
        for _ in 0..32 {
            let d = dir.clone();
            let w = winners.clone();
            hs.push(std::thread::spawn(move || {
                let s = Store::new(d).expect("store");
                if let Ok(Some(_)) = claim(&s, None, None) {
                    w.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                }
            }));
        }
        for h in hs {
            h.join().expect("thread");
        }
        assert_eq!(
            winners.load(std::sync::atomic::Ordering::SeqCst),
            1,
            "round {round}: more than one claimant won the same row"
        );
    }
}

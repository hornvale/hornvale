use sluice::row::Row;

#[test]
fn a_row_round_trips_through_render_and_parse() {
    let line = "2026-09-05T00:00:00Z\treq-abc-1\tcampaign/x\tdeadbeef\tqueued\tmerge\ta note";
    let r = Row::parse(line);
    assert_eq!(r.state, "queued");
    assert_eq!(r.note, "a note");
    assert_eq!(r.render(), line);
}

#[test]
fn an_empty_kind_normalises_to_merge() {
    let line = "2026-09-05T00:00:00Z\treq-abc-1\tcampaign/x\tdeadbeef\tqueued\t\t";
    assert_eq!(Row::parse(line).kind, "merge");
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
    let r = Row::parse("2026-09-05T00:00:00Z\treq-a-1\tb\tsha\tqueued\tmerge\t");
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
    let r = Row::parse("TRUNCATED\tonly\tthree");
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
    let r = Row::parse(line);
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
        Row::parse("w\treq-a\tb1\tsha1\tqueued\tmerge\t"),
        Row::parse("w\treq-b\tb2\tsha2\tqueued\tmerge\t"),
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
    let before = vec![Row::parse("w\treq-a\tb\tsha\tqueued\tmerge\t")];
    s.write_rows(&before).unwrap();
    let e = set_state(&s, "req-typo", "held", None).expect_err("must refuse");
    assert!(matches!(e, SetStateError::NoSuchRow));
    assert_eq!(s.read_rows().unwrap(), before);
}

use sluice::verbs::{ClaimError, claim};

#[test]
fn claim_marks_the_row_running_in_the_same_call_that_selects_it() {
    let s = Store::new(scratch("claim1")).expect("store");
    s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t")])
        .unwrap();
    let got = claim(&s, None, Some("taken")).expect("ok").expect("a row");
    assert_eq!(got.id, "req-a");
    assert_eq!(s.read_rows().unwrap()[0].state, "running");
}

#[test]
fn claim_by_sha_on_a_held_row_refuses_and_changes_nothing() {
    let s = Store::new(scratch("claim4")).expect("store");
    let before = vec![Row::parse("w\treq-a\tb\tsha1\trunning\tmerge\t")];
    s.write_rows(&before).unwrap();
    let e = claim(&s, Some("sha1"), None).expect_err("must refuse");
    assert!(matches!(e, ClaimError::HeldByAnother));
    assert_eq!(s.read_rows().unwrap(), before);
}

#[test]
fn claim_by_sha_for_an_absent_ref_is_a_different_answer() {
    let s = Store::new(scratch("claim5")).expect("store");
    s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t")])
        .unwrap();
    let e = claim(&s, Some("nope"), None).expect_err("must refuse");
    assert!(matches!(e, ClaimError::NoSuchRow));
}

#[test]
fn racing_claimants_produce_exactly_one_winner_per_queued_row() {
    // 1000 ROUNDS x 8 THREADS. The spec asks for ">=1000 iterations without a
    // double-claim", and an earlier 32x32 shape read that as 1024 claim
    // ATTEMPTS — but the property under test is "at most one winner per
    // contested row", and a round is one trial of it. 32 rounds was 32 trials,
    // 3.1% of what was asked for. The rounds are the axis that was too small,
    // so the rounds are what moved.
    //
    // CONCURRENCY STAYS BOUNDED, and is now SMALLER on purpose. It was capped
    // at 32 because macOS defaults `ulimit -n` to 256 and a 1000-thread
    // version would fail on a developer's laptop for a reason that has nothing
    // to do with the property; a flaky test in the gate blocks everyone (it
    // happened on 2026-09-05 and once is enough). Widening rounds 31x while
    // holding threads at 32 would have cost 31x the thread spawns too, so the
    // width came down to keep the whole test cheap. 8 concurrent claimants is
    // still real contention — the shell equivalent of this mutation produced
    // 12 winners out of 12 — and 1000 independent trials buy far more than a
    // wider single trial does.
    //
    // MEASURED at 1000x8 on lefford, debug profile: 1.68 / 2.31 / 2.33 / 2.36 s
    // over four runs — comfortably under the 5 s a single test may cost here,
    // since this suite runs in `outboard` on every stage gate and every merge.
    // The whole 13-test suite was 0.11 s before this change, so the 1000 trials
    // are now essentially the suite's entire cost. That is a deliberate trade:
    // this is the property the queue exists for.
    for round in 0..1000 {
        let s = Store::new(scratch(&format!("claimrace-{round}"))).expect("store");
        s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t")])
            .unwrap();
        let winners = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let dir = s.queue_path().parent().expect("dir").to_path_buf();
        let mut hs = Vec::new();
        for _ in 0..8 {
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

#[test]
fn a_second_row_for_a_running_sha_is_not_claimable_by_sha() {
    // TWO ROWS, ONE COMMIT. Coalescing is scoped to branch AND kind, so a
    // resubmission under a second branch name — or a stage request beside a
    // merge request — leaves two live rows naming one sha. With the first
    // running, claiming the second by sha would launch a concurrent chamber
    // run against a ref already being merged.
    let s = Store::new(scratch("claim_dup_sha")).expect("store");
    let before = vec![
        Row::parse("w\treq-a\tcampaign/x\tsha1\trunning\tmerge\t"),
        Row::parse("w\treq-b\tcampaign/y\tsha1\tqueued\tmerge\t"),
    ];
    s.write_rows(&before).unwrap();
    let e = claim(&s, Some("sha1"), None).expect_err("must refuse");
    assert!(matches!(e, ClaimError::HeldByAnother));
    assert_eq!(s.read_rows().unwrap(), before, "refusal must write nothing");
}

#[test]
fn the_dispatcher_skips_a_running_shas_twin_and_takes_the_next_real_row() {
    // The no-sha form must not merely refuse: the queue may hold unrelated
    // work. It skips the twin and gets on with the next row. Without this the
    // guard above would convert a duplicate into a stalled queue, which is a
    // different failure rather than a fixed one.
    let s = Store::new(scratch("claim_dup_skip")).expect("store");
    s.write_rows(&[
        Row::parse("w\treq-a\tcampaign/x\tsha1\trunning\tmerge\t"),
        Row::parse("w\treq-b\tcampaign/y\tsha1\tqueued\tmerge\t"),
        Row::parse("w\treq-c\tcampaign/z\tsha2\tqueued\tmerge\t"),
    ])
    .unwrap();
    let got = claim(&s, None, None).expect("ok").expect("a row");
    assert_eq!(
        got.id, "req-c",
        "took the twin instead of the next real row"
    );
    let rows = s.read_rows().unwrap();
    assert_eq!(rows[1].state, "queued", "the twin must be left alone");
    assert_eq!(rows[2].state, "running");
}

#[test]
fn a_queued_twin_is_claimable_once_its_sibling_is_no_longer_running() {
    // ANTI-VACUITY. A guard that refused a duplicated sha forever would pass
    // both tests above and permanently strand every resubmission. Once the
    // sibling reaches a terminal state the twin is ordinary queued work.
    let s = Store::new(scratch("claim_dup_release")).expect("store");
    s.write_rows(&[
        Row::parse("w\treq-a\tcampaign/x\tsha1\tlanded\tmerge\t"),
        Row::parse("w\treq-b\tcampaign/y\tsha1\tqueued\tmerge\t"),
    ])
    .unwrap();
    let got = claim(&s, Some("sha1"), None).expect("ok").expect("a row");
    assert_eq!(got.id, "req-b");
    assert_eq!(s.read_rows().unwrap()[1].state, "running");
}

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

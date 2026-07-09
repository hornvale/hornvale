//! The orrery CLI: single-frame print and deterministic .cast animation.
use std::process::Command;

fn bin() -> Command {
    Command::new(env!("CARGO_BIN_EXE_hornvale"))
}

#[test]
fn orrery_frame_and_cast_are_deterministic() {
    let dir = std::env::temp_dir().join(format!("hv-orrery-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let world = dir.join("w.json");
    assert!(
        bin()
            .args(["new", "--seed", "42", "--out", world.to_str().unwrap()])
            .status()
            .unwrap()
            .success()
    );

    // Single frame: deterministic, contains the star.
    let one = bin()
        .args(["orrery", "--world", world.to_str().unwrap(), "--day", "10"])
        .output()
        .unwrap();
    assert!(one.status.success());
    let frame = String::from_utf8(one.stdout).unwrap();
    assert!(frame.contains('★'), "frame has the star");

    // Animated: N frames for 0..10 step 2 => 5 events; deterministic bytes.
    let cast = dir.join("a.cast");
    let run = || {
        assert!(
            bin()
                .args([
                    "orrery",
                    "--world",
                    world.to_str().unwrap(),
                    "--day",
                    "0..10",
                    "--step",
                    "2",
                    "--cast",
                    cast.to_str().unwrap()
                ])
                .status()
                .unwrap()
                .success()
        );
        std::fs::read(&cast).unwrap()
    };
    let first = run();
    assert_eq!(first, run(), "the .cast is byte-deterministic");
    let text = String::from_utf8(first).unwrap();
    let events = text.lines().skip(1).count();
    assert_eq!(events, 5, "0..10 step 2 => 5 frames");
    // Frames carry CRLF line endings (JSON-escaped as \r\n) so an
    // asciinema-player replay returns to column 0 each row.
    assert!(
        text.contains("\\r\\n"),
        "cast frames must use CRLF line endings for the raw-terminal replay"
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

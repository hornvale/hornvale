//! The Cairn CLI.

use board::git::Repo;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let repo = Repo::new(std::env::current_dir().expect("cwd"));
    match args.get(1).map(String::as_str) {
        Some("read") => {
            // Task 4 fills this in; for now prove the seam is wired.
            let _ = &repo;
            println!();
        }
        _ => {
            eprintln!("usage: board <post|read|render|digest|retract|reap>");
            std::process::exit(2);
        }
    }
}

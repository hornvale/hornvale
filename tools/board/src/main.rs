//! The Cairn CLI. Positional parsing only, `tools/digest`'s pattern.

use board::git::Repo;
use board::live::LiveContext;
use board::post::Post;
use board::relevance::{Cursor, Displayed, changed_paths, unseen};
use board::render::{RenderOptions, live_posts, render};
use board::store::Board;
use std::collections::BTreeSet;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let repo = Repo::new(std::env::current_dir().expect("cwd"));
    let board = Board::new(repo.clone());

    match args.get(1).map(String::as_str) {
        // board post <kind> <by> [key=value ...]
        Some("post") => {
            let (Some(kind), Some(by)) = (args.get(2), args.get(3)) else {
                eprintln!("usage: board post <kind> <by> [key=value ...]");
                std::process::exit(2);
            };
            let mut post = Post::new(kind, by);
            for pair in args.iter().skip(4) {
                let Some((k, v)) = pair.split_once('=') else {
                    eprintln!("board: expected key=value, got {pair:?}");
                    std::process::exit(2);
                };
                // Numbers and booleans keep their JSON type; everything else
                // is a string.
                let value = serde_json::from_str(v)
                    .unwrap_or_else(|_| serde_json::Value::String(v.to_string()));
                post = post.with(k, value);
            }
            match board.append(&post) {
                Ok(id) => println!("{id}"),
                Err(e) => {
                    eprintln!("board: {e}");
                    std::process::exit(1);
                }
            }
        }
        // board render  — the ambient session-start view (unseen + relevant, capped)
        // board read    — the full on-demand view; marks NOTHING seen
        Some(cmd @ ("render" | "read")) => {
            // I2: a real git failure here must not be indistinguishable from
            // an empty board. `posts_at_tip()` only ever errs on a genuine
            // git failure (an absent board reads as `Ok(vec![])`), so
            // swallowing it into a default would silently print nothing on
            // exit 0 for `read` -- this project's worst failure mode.
            let posts = match board.posts_at_tip() {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("board: could not read the board: {e}");
                    if cmd == "read" {
                        std::process::exit(1);
                    }
                    return; // render is ambient; warn and stay quiet (D7)
                }
            };
            let ctx = match LiveContext::probe(&repo, &posts) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("board: {e}");
                    return; // never break a session (D7)
                }
            };
            let live = live_posts(&posts, &ctx);

            if cmd == "read" {
                // `full()` has no real cap (its post budget is effectively
                // unbounded), so nothing is ever elided here.
                print!("{}", render(&live, 0, &RenderOptions::full()));
                return; // an explicit full read must not advance the cursor
            }

            let opts = RenderOptions::session_start();

            // Ambient path. The `Displayed` that is capped here is the SAME
            // value both `render` draws its text from and `cursor.record`
            // marks seen, by construction: `Displayed::cap` can only shrink
            // an already-filtered `Displayed`, so there is no way for the
            // recorded set to include a post whose content never made it
            // into `render`'s output. That equivalence is what Task 5's
            // `Displayed` closed for the wrong-`BTreeSet` bug, and what
            // `cap` extends to close it for a render's own line budget too
            // (C1) -- routing both `render` and `record` through the same
            // capped value is what keeps it closed.
            let Ok(cursor) = Cursor::open(&repo) else {
                // No cursor: still render, but show everything live and
                // relevant rather than silently nothing.
                let changed = changed_paths(&repo).unwrap_or_default();
                let all: BTreeSet<String> = live.iter().map(|s| s.id.clone()).collect();
                let displayed = Displayed::filter(&live, &all, &changed);
                let (shown, elided) = displayed.cap(opts.post_budget());
                print!("{}", render(shown.posts(), elided, &opts));
                return;
            };
            let fresh = match unseen(&board, &cursor) {
                Ok(f) => f,
                Err(e) => {
                    // Could not determine what is unseen. Say so and render
                    // nothing rather than advancing the cursor over posts we
                    // never showed -- that is the permanent-loss path.
                    eprintln!(
                        "board: cannot determine unread posts, not advancing the cursor: {e}"
                    );
                    return;
                }
            };
            let changed = changed_paths(&repo).unwrap_or_default();
            let displayed = Displayed::filter(&live, &fresh, &changed);
            let (shown, elided) = displayed.cap(opts.post_budget());
            print!("{}", render(shown.posts(), elided, &opts));
            if let Err(e) = cursor.record(&board, &shown) {
                eprintln!("board: could not record the read cursor: {e}");
            }
        }
        _ => {
            eprintln!("usage: board <post|read|render|digest|retract|reap>");
            std::process::exit(2);
        }
    }
}

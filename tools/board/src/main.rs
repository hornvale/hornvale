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
            let posts = board.posts_at_tip().unwrap_or_default();
            let ctx = match LiveContext::probe(&repo, &posts) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("board: {e}");
                    return; // never break a session (D7)
                }
            };
            let live = live_posts(&posts, &ctx);

            if cmd == "read" {
                print!("{}", render(&live, &RenderOptions::full()));
                return; // an explicit full read must not advance the cursor
            }

            // Ambient path. The cursor is advanced from the SAME `Displayed`
            // that produced the text, so a post can never be marked seen
            // without having been shown -- that equivalence is the bug Task
            // 5 closed, and routing the write through `Displayed` is what
            // keeps it closed.
            let Ok(cursor) = Cursor::open(&repo) else {
                // No cursor: still render, but show everything live and
                // relevant rather than silently nothing.
                let changed = changed_paths(&repo).unwrap_or_default();
                let all: BTreeSet<String> = live.iter().map(|s| s.id.clone()).collect();
                let displayed = Displayed::filter(&live, &all, &changed);
                print!(
                    "{}",
                    render(displayed.posts(), &RenderOptions::session_start())
                );
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
            print!(
                "{}",
                render(displayed.posts(), &RenderOptions::session_start())
            );
            if let Err(e) = cursor.record(&board, &displayed) {
                eprintln!("board: could not record the read cursor: {e}");
            }
        }
        _ => {
            eprintln!("usage: board <post|read|render|digest|retract|reap>");
            std::process::exit(2);
        }
    }
}

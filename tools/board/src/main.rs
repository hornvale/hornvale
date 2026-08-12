//! The Cairn CLI. Positional parsing only, `tools/digest`'s pattern.

use board::git::Repo;
use board::live::{LiveContext, current_host};
use board::post::Post;
use board::relevance::{Cursor, Displayed, changed_paths, unseen};
use board::render::{RenderOptions, live_posts, peer_status, render, with_peer_header};
use board::store::{Board, ReapPlan};
use board::sync::{peer_ages, peer_content_ages, sync};
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
            // A convention-numeric field that did not parse as a number is
            // still a legal post (D12: the tool validates only `kind` and
            // `by`), but its decay check will be skipped at read time --
            // `ttl_s=900s` is an immortal claim. Warn at the moment the typo
            // is made, which is the only moment anyone can cheaply fix it;
            // `live.rs` warns again each time the consequence is drawn.
            for field in post.non_numeric_convention_fields() {
                eprintln!(
                    "board: WARNING `{field}` is not a number, so its decay check will be \
                     skipped on every read -- a claim with a non-numeric ttl_s never expires. \
                     Numbers must be bare (ttl_s=900, not ttl_s=900s)."
                );
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
            // B6: ships WITH the render, not after it -- a `notice` carries
            // no `ttl_s`, so once its authoring host goes quiet, sync age is
            // the only local signal left that its content might be stale.
            // Same wall clock the liveness probe already read, so this
            // never disagrees with the render it accompanies about "now".
            // Two signals (see `peer_status`'s doc): how stale OUR VIEW of
            // each peer is, and how long since that peer actually posted --
            // the second is the one a frozen/retired peer's staleness
            // actually needs, since a host that syncs regularly reports the
            // first as fresh forever regardless of the peer's own silence.
            let peer_header = peer_status(
                &peer_ages(&repo, ctx.now_unix),
                &peer_content_ages(&repo, ctx.now_unix),
            );

            if cmd == "read" {
                // `full()` has no real cap (its post budget is effectively
                // unbounded), so nothing is ever elided here.
                let body = render(&live, 0, &RenderOptions::full());
                print!("{}", with_peer_header(&peer_header, body));
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
                let body = render(shown.posts(), elided, &opts);
                print!("{}", with_peer_header(&peer_header, body));
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
            let body = render(shown.posts(), elided, &opts);
            print!("{}", with_peer_header(&peer_header, body));
            if let Err(e) = cursor.record(&board, &shown) {
                eprintln!("board: could not record the read cursor: {e}");
            }
        }
        // board reap — compact away posts no longer live from the tip tree.
        //
        // The one operation with PERMANENT consequences, so it is the one
        // that refuses to guess. Two properties, both structural rather than
        // conventional:
        //
        //  1. It reads the board ONCE, into a `TipSnapshot`, and `ReapPlan`
        //     probes that snapshot. So the post set the probe ran over, the
        //     post set the predicate judges, and the tip the CAS is
        //     baselined on are the same three things — there is no window in
        //     which a `claim` appended after the probe can be judged
        //     pid-dead (it was never `ps`-probed) and dropped forever.
        //  2. It FAILS LOUD on the read. An `unwrap_or_default()` here would
        //     turn a git failure into an empty post set, which probes as
        //     "nothing is live" — the maximally destructive reading, in the
        //     only operation that cannot be undone by running again. Refuse
        //     to reap instead.
        Some("reap") => {
            let snapshot = match board.snapshot() {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("board: refusing to reap, cannot read the board: {e}");
                    std::process::exit(1);
                }
            };
            let Some(snapshot) = snapshot else {
                println!("reaped 0"); // no board yet; nothing to compact
                return;
            };
            match ReapPlan::probe(&repo, &snapshot).and_then(|plan| board.reap(&plan)) {
                Ok(n) => println!("reaped {n}"),
                Err(e) => {
                    eprintln!("board: {e}");
                    std::process::exit(1);
                }
            }
        }
        // board retract <by> <post-id> — appends a new post; never rewrites
        // or deletes anything. Retraction is data, not an operation on the
        // store.
        Some("retract") => {
            let (Some(by), Some(id)) = (args.get(2), args.get(3)) else {
                eprintln!("usage: board retract <by> <post-id>");
                std::process::exit(2);
            };
            let post = Post::new("retract", by).with("post", serde_json::Value::String(id.clone()));
            match board.append(&post) {
                Ok(new_id) => println!("{new_id}"),
                Err(e) => {
                    eprintln!("board: {e}");
                    std::process::exit(1);
                }
            }
        }
        // board redact <by> <post-id> — B8/D10: appends a `redact` control
        // post, then evicts the named post from the TIP tree. History keeps
        // it (D13) and the digest suppresses its body while still reporting
        // the act; see `Board::redact`'s doc comment for why this is a
        // read-time judgment rather than a (prohibited, and measured not to
        // work) history rewrite.
        Some("redact") => {
            let (Some(by), Some(id)) = (args.get(2), args.get(3)) else {
                eprintln!("usage: board redact <by> <post-id>");
                std::process::exit(2);
            };
            match board.redact(by, id) {
                Ok(new_id) => println!("{new_id}"),
                Err(e) => {
                    eprintln!("board: {e}");
                    std::process::exit(1);
                }
            }
        }
        // board digest [days] — the human read seam, over history (D14).
        // Never advances the read cursor and never writes to the board: it
        // is a rendering of history, not a new fact about it.
        Some("digest") => {
            let days: u64 = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(14);
            // Wall clock: the digest's window is measured in real days, and
            // this tool lives outside the workspace's no-wall-clock rule
            // (see the crate's Global Constraints) — this is the sanctioned
            // call site for it in this crate, alongside `live.rs`'s probe.
            #[allow(clippy::disallowed_types)]
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0);
            match board::digest::history(&board, days, now) {
                Ok(posts) => print!("{}", board::digest::digest(&posts)),
                Err(e) => {
                    eprintln!("board: {e}");
                    std::process::exit(1);
                }
            }
        }
        // board sync [remote] — The Beacon: publish this host's log to
        // `remote` (default `origin`), then fetch every peer's.
        //
        // NEVER FAILS THE PROCESS (B6): the local append this session cares
        // about has already succeeded by the time sync ever runs, so an
        // unreachable remote degrades to exactly the single-box behaviour
        // that shipped before this campaign. Both halves are reported on
        // whatever channel fits their outcome; the exit code stays 0
        // either way, or `make board-sync` would look broken merely
        // because the network is down.
        Some("sync") => {
            let remote = args.get(2).map(String::as_str).unwrap_or("origin");
            // Same host `sync` itself resolves internally (`current_host()`
            // is the crate's one function for this, by design -- see its
            // doc comment on why two independent copies must never exist);
            // read again here only to name the slot in this print, not to
            // decide anything.
            let host = current_host();
            let report = sync(&repo, remote);
            match report.pushed {
                Ok(()) => println!("board: pushed to {remote} (refs/hornvale/hosts/{host})"),
                Err(e) => eprintln!("board: push to {remote} failed: {e}"),
            }
            match report.fetched {
                Ok(peers) => println!(
                    "board: fetched from {remote} ({} peer mirror{})",
                    peers.len(),
                    if peers.len() == 1 { "" } else { "s" }
                ),
                Err(e) => eprintln!("board: fetch from {remote} failed: {e}"),
            }
        }
        _ => {
            eprintln!("usage: board <post|read|render|digest|retract|redact|reap|sync>");
            std::process::exit(2);
        }
    }
}

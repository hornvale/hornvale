# The Planetarium client boundary qualification

Task 8 landed its local implementation as
`be764f4d8824858b08d9bc47b6be061904ded7d8`, based on
`2dd219cef400c0a4cdb770f83f5cae533ca68c32`, in
`/Users/nathan/.config/superpowers/worktrees/hornvale/the-planetarium` on
`campaign/the-planetarium`. The root commit hook passed 4,563 selected tests
across four chunks in a 42.895 s gate. This is local commit qualification;
canonical clients-phase qualification remains a later queue result.

The metadata guard follows resolved IDs and normal/build/dev paths under all
features. Synthetic tests first failed with a no-op guard (45 failing cases),
then passed six tests including 36 forbidden-direction/kind/path combinations,
allowed app/source edges, optional and renamed edges, transitive paths, Bevy
subcrates, path-based simulation identity and incomplete metadata.

A real source-manifest mutation asserted the unique `[dev-dependencies]` target,
inserted `leaked_view = { package = "hornvale-bevy-view", path = "../bevy" }`,
and refreshed the disposable lock resolution with Cargo. The actual locked guard
returned exit 1 and `forbidden reachability`, naming source→view and
source→view→Bevy plus its subcrates. The experiment's `finally` restored the
manifest and lock byte-for-byte; assertions confirmed restoration and the fresh
guard returned exit 0. Thus a resolver error was not mistaken for boundary
coverage, and a missing mutation target could not produce a false witness.

The root integration test first failed because `visual-check-run` was absent.
After integration, nine architecture tests passed; a combined architecture,
lane-roster and test-binary-ratchet run passed 15 tests. No new compiled test
binary was added. Forty-one documentation tests passed, and new local links were
checked directly. Only the decision-index generator changed an artifact (three
new rows); a temporary delta render was identical to the committed report.

`make visual-check` exposed the nested virtual-manifest formatting trap: from
root, `cargo fmt --manifest-path clients/visual/Cargo.toml` returned `Failed to
find targets` (0.077 s). Explicit package selection repaired that run: the full
CPU gate passed in 47.578 s, with 72 Rust tests and six Python tests. The final
recipe uses `cd clients/visual && cargo +1.96.1 fmt --check`; a verbose successful
probe confirmed exactly the same three client crates' targets, without formatting
native path dependencies or maintaining a second member roster. Clippy, tests
and the production guard retain explicit Rust 1.96.1 and locked resolution.

[Bevy's pinned Linux requirements](https://github.com/bevyengine/bevy/blob/v0.19.1/docs/linux_dependencies.md)
were inspected against this workspace's X11 feature set (no audio, gamepad or
Wayland). The recorded lefford preflight found Rust 1.96.1 and development
libraries x11/xcursor/xi/xrandr/xkbcommon at 1.8.4/1.2.1/1.8/1.5.2/1.5.0.
No installation was performed. Installed prerequisites do not establish a build;
the later canonical clients phase must compile and test the new workspace.

The [Task 7 GPU package](package.md) retains its original capture revision and
hashes. This CPU integration does not relabel that package or replace required
moving visual review, GPU/runtime qualification, final performance work or G6.

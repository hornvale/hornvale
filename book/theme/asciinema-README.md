# Vendored: asciinema-player v3.8.0

Downloaded from the pinned GitHub release:

- `asciinema-player.min.js` — <https://github.com/asciinema/asciinema-player/releases/download/v3.8.0/asciinema-player.min.js>
- `asciinema-player.css` — <https://github.com/asciinema/asciinema-player/releases/download/v3.8.0/asciinema-player.css>

Note: the v3.8.0 release ships a minified JS bundle but an **unminified** CSS
file only (no `asciinema-player.min.css` asset exists upstream). We vendor
the CSS file under its upstream name, `asciinema-player.css`, and reference
that exact filename in `book.toml`'s `additional-css`.

These are static assets committed to the repository, not Cargo dependencies.
To upgrade: download the same two files from a newer pinned release tag and
replace them here, updating this file's version note and the URLs above.

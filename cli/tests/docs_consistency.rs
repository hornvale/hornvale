//! Drift checks binding the knowledge architecture together: the idea
//! registry and frontier essays (published as the book's Frontier part),
//! and the docs map must not silently diverge. Enforces that every
//! cross-link resolves, every frontier section is indexed in the Contents
//! ToC, and registry IDs stay unique. The architecture these checks defend
//! is described in `docs/README.md` and
//! `docs/CLAUDE.md`; this file makes the discipline executable, the same way
//! `architecture.rs` makes the layering rules executable.

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

/// The repository root: `cli/tests/` lives in the `cli` crate, whose manifest
/// dir is `<root>/cli`, so the root is its parent.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli crate should sit under the repo root")
        .to_path_buf()
}

fn read(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| panic!("reading {}: {e}", path.display()))
}

/// The GitHub-flavored heading anchor (a github-slugger approximation):
/// lowercase, drop every character that is not alphanumeric, space, hyphen,
/// or underscore, then map spaces to hyphens. Removed runs leave repeated
/// hyphens — e.g. " — " becomes "--" — which matches GitHub's own output and
/// the anchors the docs link to.
fn slug(header: &str) -> String {
    header
        .trim()
        .to_lowercase()
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == ' ' || *c == '-' || *c == '_')
        .collect::<String>()
        .replace(' ', "-")
}

/// `(level, trimmed text)` for every ATX heading (`#`..`######`) in `content`.
fn headings(content: &str) -> Vec<(usize, String)> {
    content
        .lines()
        .filter_map(|line| {
            let hashes = line.chars().take_while(|c| *c == '#').count();
            let rest = &line[hashes..];
            if (1..=6).contains(&hashes) && rest.starts_with(' ') {
                Some((hashes, rest.trim().to_string()))
            } else {
                None
            }
        })
        .collect()
}

/// Every heading anchor a file exposes, at any level.
fn anchors(content: &str) -> BTreeSet<String> {
    headings(content)
        .into_iter()
        .map(|(_, t)| slug(&t))
        .collect()
}

/// The URL part of every inline `[text](url)` link in `content`. URLs contain
/// no nested parentheses in these docs, so a scan to the first `)` suffices.
fn link_urls(content: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut rest = content;
    while let Some(open) = rest.find("](") {
        let after = &rest[open + 2..];
        if let Some(close) = after.find(')') {
            out.push(after[..close].to_string());
            rest = &after[close + 1..];
        } else {
            break;
        }
    }
    out
}

/// Links into this repo written as GitHub blob URLs (the published frontier
/// part links to decisions and specs, which live outside the book) are
/// mapped back to repo-relative paths and validated like relative links.
const REPO_BLOB_PREFIX: &str = "https://github.com/hornvale/hornvale/blob/main/";

/// Validate every link in `file`, pushing a message per broken one into
/// `errors`. A link resolves if its path exists (relative to the file's
/// directory; an empty path means the file itself; a `REPO_BLOB_PREFIX` URL
/// is relative to the repo root), and, when it carries a `#fragment` into a
/// Markdown file, if that fragment names a real heading.
fn check_links(file: &Path, errors: &mut Vec<String>) {
    let content = read(file);
    let dir = file.parent().expect("doc file should have a parent");
    for url in link_urls(&content) {
        let (rel, base) = if let Some(repo_rel) = url.strip_prefix(REPO_BLOB_PREFIX) {
            (repo_rel.to_string(), repo_root())
        } else if url.starts_with("http://") || url.starts_with("https://") {
            continue;
        } else {
            (url.clone(), dir.to_path_buf())
        };
        let (path_part, fragment) = match rel.split_once('#') {
            Some((p, f)) => (p, Some(f.to_string())),
            None => (rel.as_str(), None),
        };
        let target = if path_part.is_empty() {
            file.to_path_buf()
        } else {
            base.join(path_part)
        };
        if !target.exists() {
            errors.push(format!(
                "{}: link `{url}` points at a missing path",
                file.display()
            ));
            continue;
        }
        if let Some(fragment) = fragment
            && target.extension().and_then(|e| e.to_str()) == Some("md")
            && !anchors(&read(&target)).contains(&fragment)
        {
            errors.push(format!(
                "{}: link `{url}` names an anchor that no heading produces",
                file.display()
            ));
        }
    }
}

/// The links appearing inside the frontier's `## Contents` block, up to the
/// next `---` rule.
fn toc_anchor_targets(frontier: &str) -> BTreeSet<String> {
    let start = frontier
        .find("## Contents")
        .expect("frontier should have a Contents section");
    let region = &frontier[start..];
    let end = region.find("\n---").unwrap_or(region.len());
    link_urls(&region[..end])
        .into_iter()
        .filter_map(|u| u.strip_prefix('#').map(str::to_string))
        .collect()
}

#[test]
fn every_frontier_section_is_listed_in_the_contents() {
    let frontier = read(&repo_root().join("book/src/frontier/frontier.md"));
    let toc = toc_anchor_targets(&frontier);
    let mut missing = Vec::new();
    for (level, text) in headings(&frontier) {
        if level != 2 || text == "Contents" {
            continue;
        }
        let anchor = slug(&text);
        if !toc.contains(&anchor) {
            missing.push(format!("`{text}` (#{anchor})"));
        }
    }
    assert!(
        missing.is_empty(),
        "frontier sections absent from the Contents ToC (add a bullet, or the \
         registry pointer will drift):\n  {}",
        missing.join("\n  ")
    );
}

#[test]
fn registry_ids_are_unique() {
    let registry = read(&repo_root().join("book/src/frontier/idea-registry.md"));
    let mut seen = BTreeSet::new();
    let mut dupes = Vec::new();
    for line in registry.lines() {
        let Some(rest) = line.strip_prefix("| ") else {
            continue;
        };
        let cell = rest.split('|').next().unwrap_or("").trim();
        // An ID is a category prefix, a hyphen, and either a number with an
        // optional sub-letter (MAP-9, MAP-9a, LANG-1 — the frozen numbered
        // era) or a lowercase slug (SKY-eclipse-seasons — decision
        // `0026-slugs-not-numbers`). Anything else is a header or separator
        // cell.
        let looks_like_id = cell.split_once('-').is_some_and(|(pre, post)| {
            let numbered = post.starts_with(|c: char| c.is_ascii_digit())
                && post
                    .trim_end_matches(|c: char| c.is_ascii_lowercase())
                    .chars()
                    .all(|c| c.is_ascii_digit());
            let slug = post.starts_with(|c: char| c.is_ascii_lowercase())
                && post
                    .chars()
                    .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-');
            !pre.is_empty()
                && pre.chars().all(|c| c.is_ascii_uppercase())
                && !post.is_empty()
                && (numbered || slug)
        });
        if looks_like_id && !seen.insert(cell.to_string()) {
            dupes.push(cell.to_string());
        }
    }
    assert!(
        dupes.is_empty(),
        "duplicate registry IDs (IDs are permanent and unique):\n  {}",
        dupes.join("\n  ")
    );
}

#[test]
fn all_knowledge_doc_links_resolve() {
    let root = repo_root();
    let mut errors = Vec::new();
    for rel in [
        "docs/README.md",
        "book/src/frontier/frontier.md",
        "book/src/frontier/idea-registry.md",
    ] {
        check_links(&root.join(rel), &mut errors);
    }
    assert!(
        errors.is_empty(),
        "broken cross-links in the knowledge docs:\n  {}",
        errors.join("\n  ")
    );
}

#[test]
fn the_confidence_gradient_links_resolve() {
    // The Confidence Gradient (`book/src/open-questions.md`) is a re-scored map,
    // not a generated artifact — nothing regenerates it, so a renamed chronicle
    // or gallery target would rot its evidence pointers silently. Link-check it
    // so at least that failure is loud (decision
    // 0030; DoD prompts the
    // re-scoring itself, which no test can prove).
    let mut errors = Vec::new();
    check_links(&repo_root().join("book/src/open-questions.md"), &mut errors);
    assert!(
        errors.is_empty(),
        "broken links in the Confidence Gradient (re-score the chapter, don't \
         edit the check):\n  {}",
        errors.join("\n  ")
    );
}

/// The set of category prefixes the idea registry actually uses (`EXP`, `MAP`,
/// `BIO`, …), parsed from the ID column so the book lint auto-adapts when a new
/// prefix is coined rather than hard-coding a list that rots.
fn registry_id_prefixes() -> BTreeSet<String> {
    let registry = read(&repo_root().join("book/src/frontier/idea-registry.md"));
    let mut prefixes = BTreeSet::new();
    for line in registry.lines() {
        let Some(rest) = line.strip_prefix("| ") else {
            continue;
        };
        let cell = rest.split('|').next().unwrap_or("").trim();
        if let Some((pre, post)) = cell.split_once('-')
            && !pre.is_empty()
            && pre.chars().all(|c| c.is_ascii_uppercase())
            && post.starts_with(|c: char| c.is_ascii_digit())
        {
            prefixes.insert(pre.to_string());
        }
    }
    prefixes
}

/// The first registry ID (`EXP-3`, `MAP-9a`) appearing in `text` as a whole
/// token, or `None`. Restricting the scan to known registry prefixes avoids
/// false positives on prose like `CC-BY-4.0` or `UTF-8`.
fn find_registry_id(text: &str, prefixes: &BTreeSet<String>) -> Option<String> {
    let bytes = text.as_bytes();
    for prefix in prefixes {
        let pat = format!("{prefix}-");
        let mut start = 0;
        while let Some(pos) = text[start..].find(&pat) {
            let idx = start + pos;
            let prev_ok = idx == 0 || !bytes[idx - 1].is_ascii_alphanumeric();
            let after = idx + pat.len();
            let next_is_digit = bytes.get(after).is_some_and(u8::is_ascii_digit);
            if prev_ok && next_is_digit {
                let mut end = after;
                while end < bytes.len()
                    && (bytes[end].is_ascii_digit() || bytes[end].is_ascii_lowercase())
                {
                    end += 1;
                }
                return Some(text[idx..end].to_string());
            }
            start = idx + pat.len();
        }
    }
    None
}

/// Collect every `.md` file under `dir`, recursively.
fn md_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).unwrap_or_else(|e| panic!("reading {}: {e}", dir.display())) {
        let path = entry.unwrap().path();
        if path.is_dir() {
            md_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "md") {
            out.push(path);
        }
    }
}

/// The merged-reality parts of the book must not cite the idea registry:
/// The Frontier part is the registry's published home (decision
/// 0031), and every other part describes
/// merged reality — the recurring failure where a chronicle or domain
/// chapter cited a registry ID (`EXP-3`) or leaked engineering-process
/// vocabulary was caught by review twice before this check existed.
/// No registry ID may appear anywhere in the book; a tight set of
/// unambiguous engineering terms may not appear in the world-prose chapters
/// (chronicle, domain chapters). The set is deliberately small —
/// `task`/`plan`/`gate`/`commit`/`code review` are legitimate English and
/// are NOT banned, to avoid false positives; this guards the clear leaks,
/// not every conceivable slip.
#[test]
fn the_book_carries_no_registry_ids_or_process_vocabulary() {
    let root = repo_root();
    let prefixes = registry_id_prefixes();
    let mut md = Vec::new();
    md_files(&root.join("book/src"), &mut md);

    // Never occur in world-prose; unambiguous engineering terms.
    const PROSE_ONLY_BANNED: [&str; 5] = [
        "subagent",
        "pull request",
        "merge conflict",
        "worktree",
        "git commit",
    ];

    let mut errors = Vec::new();
    for path in &md {
        let text = read(path);
        let rel = path.strip_prefix(&root).unwrap_or(path);
        // The Frontier part IS the registry; the ban guards everything else.
        let in_frontier_part = rel.starts_with("book/src/frontier");
        if !in_frontier_part && let Some(id) = find_registry_id(&text, &prefixes) {
            errors.push(format!(
                "{}: registry ID `{id}` — only The Frontier part may cite the registry",
                rel.display()
            ));
        }
        let in_world_prose =
            rel.starts_with("book/src/chronicle") || rel.starts_with("book/src/domains");
        if in_world_prose {
            let lower = text.to_lowercase();
            for term in PROSE_ONLY_BANNED {
                if lower.contains(term) {
                    errors.push(format!(
                        "{}: process vocabulary `{term}` in a world-prose chapter",
                        rel.display()
                    ));
                }
            }
        }
    }
    assert!(
        errors.is_empty(),
        "a merged-reality part of the book cited the idea registry or leaked \
         engineering-process vocabulary (only the marked Frontier part is \
         speculative — docs/CLAUDE.md):\n  {}",
        errors.join("\n  ")
    );
}

/// Recursively collect `.rs` and `.sh` files under `dir`, skipping build
/// output and hidden directories (the source-side companion to [`md_files`]).
fn source_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries {
        let path = entry.expect("dir entry").path();
        let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if path.is_dir() {
            if name != "target" && !name.starts_with('.') {
                source_files(&path, out);
            }
        } else if path.extension().is_some_and(|e| e == "rs" || e == "sh") {
            out.push(path);
        }
    }
}

/// Check one cite token against the decision records. `None` means fine —
/// either it resolves, or it is prose rather than a cite: a 4-digit token
/// must match a `NNNN-*.md` record; a lowercase token with ≥ 2 hyphens and
/// at least one letter must match a `<slug>.md` record or a numbered
/// record's slug tail ("decision log" has no hyphen, "4-digit" has one,
/// "2026-07-05" has no letter — none is a cite; the letter rule keeps the
/// maximal-munch token grabber from dressing dates and numeric ranges up as
/// slugs).
fn cite_error(token: &str, numbers: &BTreeSet<String>, slugs: &BTreeSet<String>) -> Option<String> {
    if token.len() == 4 && token.chars().all(|c| c.is_ascii_digit()) {
        return (!numbers.contains(token))
            .then(|| format!("no docs/decisions/{token}-*.md record"));
    }
    let is_slug_shaped = token.matches('-').count() >= 2
        && !token.starts_with('-')
        && !token.ends_with('-')
        && token.chars().any(|c| c.is_ascii_lowercase())
        && token
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-');
    if is_slug_shaped && !slugs.contains(token) {
        return Some(format!("no docs/decisions/{token}.md record"));
    }
    None
}

/// Strip one leading comment marker (`///`, `//!`, `//`, `#`) and the space
/// after it, so a cite wrapped across comment lines reads as continuous text
/// once the lines are joined (the linter-side twin of the doctor's sed).
fn strip_comment_marker(line: &str) -> &str {
    let trimmed = line.trim_start();
    for marker in ["///", "//!", "//", "#"] {
        if let Some(rest) = trimmed.strip_prefix(marker) {
            return rest.strip_prefix(' ').unwrap_or(rest);
        }
    }
    line
}

/// A file's comment-marker-stripped, line-joined text, with a parallel map
/// from each byte of the joined text to its 1-indexed source line — so a
/// match in the joined text still reports a real line number.
fn joined_with_line_map(content: &str) -> (String, Vec<usize>) {
    let mut text = String::new();
    let mut line_of = Vec::new();
    for (idx, line) in content.lines().enumerate() {
        if idx > 0 {
            text.push(' ');
            line_of.push(idx);
        }
        let stripped = strip_comment_marker(line);
        text.push_str(stripped);
        line_of.resize(text.len(), idx + 1);
    }
    (text, line_of)
}

/// Scan one file's content for `decision <token>` / `decisions <token>` /
/// `ADR <token>` cites and return the errors for tokens that resolve to no
/// decision record, each prefixed `<rel>:<line>`. Scans the line-joined view
/// so a cite whose keyword ends one comment line and whose token starts the
/// next is still checked.
fn cite_errors_in(
    content: &str,
    rel: &str,
    numbers: &BTreeSet<String>,
    slugs: &BTreeSet<String>,
) -> Vec<String> {
    let (text, line_of) = joined_with_line_map(content);
    let mut found = Vec::new();
    for keyword in ["decision ", "decisions ", "ADR "] {
        let mut from = 0;
        while let Some(pos) = text[from..].find(keyword) {
            let at = from + pos;
            let after = &text[at + keyword.len()..];
            let token: String = after
                .trim_start_matches('`')
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '-')
                .collect();
            if let Some(err) = cite_error(&token, numbers, slugs) {
                let line = line_of[at];
                found.push((
                    line,
                    format!("{rel}:{line}: cite `{keyword}{token}` — {err}"),
                ));
            }
            from = at + keyword.len();
        }
    }
    found.sort();
    found.into_iter().map(|(_, msg)| msg).collect()
}

/// Every decision citation in the Rust and shell sources resolves to a
/// record in `docs/decisions/` — the decision-log half of the knowledge-base
/// drift linters. Forms: `decision 0014` / `decisions 0002` / `ADR 0016`
/// (numeric) and `decision <slug>` (hyphenated slug, optionally backticked).
#[test]
fn decision_cites_in_sources_resolve() {
    let root = repo_root();
    let mut numbers = BTreeSet::new();
    let mut slugs = BTreeSet::new();
    for entry in fs::read_dir(root.join("docs/decisions")).expect("decisions dir") {
        let name = entry.expect("dir entry").file_name();
        let name = name.to_string_lossy();
        let Some(stem) = name.strip_suffix(".md") else {
            continue;
        };
        let numbered = stem.len() > 5
            && stem.as_bytes()[4] == b'-'
            && stem[..4].chars().all(|c| c.is_ascii_digit());
        if numbered {
            numbers.insert(stem[..4].to_string());
            slugs.insert(stem[5..].to_string());
            // The full stem (`0026-slugs-not-numbers`) is a citable form
            // too — it is the record's actual filename.
            slugs.insert(stem.to_string());
        } else {
            slugs.insert(stem.to_string());
        }
    }

    let mut files = Vec::new();
    for dir in ["kernel", "domains", "windows", "cli", "tools", "scripts"] {
        source_files(&root.join(dir), &mut files);
    }
    files.sort();

    let mut errors = Vec::new();
    for file in &files {
        let content = read(file);
        let rel = file
            .strip_prefix(&root)
            .unwrap_or(file)
            .display()
            .to_string();
        errors.extend(cite_errors_in(&content, &rel, &numbers, &slugs));
    }
    assert!(
        errors.is_empty(),
        "decision cites that resolve to no record (fix the cite, or add the \
         missing record to docs/decisions/):\n  {}",
        errors.join("\n  ")
    );
}

#[test]
fn cite_error_resolves_the_known_forms() {
    let numbers: BTreeSet<String> = ["0016".to_string()].into();
    let slugs: BTreeSet<String> = [
        "calibration-loads-the-census-fixture".to_string(),
        "slugs-not-numbers".to_string(),
        "0016-slugs-not-numbers".to_string(),
    ]
    .into();
    // Resolvable numeric and slug cites.
    assert_eq!(cite_error("0016", &numbers, &slugs), None);
    assert_eq!(
        cite_error("calibration-loads-the-census-fixture", &numbers, &slugs),
        None
    );
    // A numbered record's slug tail resolves too, and so does its full stem.
    assert_eq!(cite_error("slugs-not-numbers", &numbers, &slugs), None);
    assert_eq!(cite_error("0016-slugs-not-numbers", &numbers, &slugs), None);
    // Unresolvable cites are errors.
    assert!(cite_error("0999", &numbers, &slugs).is_some());
    assert!(cite_error("no-such-decision-here", &numbers, &slugs).is_some());
    // Prose, not cites: hyphen-free words and short hyphenations.
    assert_eq!(cite_error("log", &numbers, &slugs), None);
    assert_eq!(cite_error("point", &numbers, &slugs), None);
    assert_eq!(cite_error("4-digit", &numbers, &slugs), None);
    assert_eq!(cite_error("", &numbers, &slugs), None);
    // Maximal-munch numerics: the token grabber eats digits and hyphens, so
    // a date or a numeric range after the keyword munches into a token with
    // enough hyphens to look slug-shaped. A slug names words — no letters,
    // no cite.
    assert_eq!(cite_error("2026-07-05", &numbers, &slugs), None);
    assert_eq!(cite_error("0002-0005-0007", &numbers, &slugs), None);
}

#[test]
fn cite_errors_in_catches_line_wrapped_cites() {
    let numbers: BTreeSet<String> = ["0016".to_string()].into();
    let slugs: BTreeSet<String> = ["slugs-not-numbers".to_string()].into();
    // A cite wrapped across comment lines: the keyword ends one line, the
    // token starts the next. A wrapped cite of a real record stays silent...
    let good = "// as ratified (decision\n// `slugs-not-numbers`), the log wins\n";
    assert_eq!(
        cite_errors_in(good, "src/lib.rs", &numbers, &slugs),
        Vec::<String>::new()
    );
    // ...and a wrapped cite of a missing record is an error, reported at the
    // keyword's line.
    let bad = "fn f() {}\n// see decision\n// `no-such-decision-here` for why\n";
    let errors = cite_errors_in(bad, "src/lib.rs", &numbers, &slugs);
    assert_eq!(
        errors.len(),
        1,
        "wrapped cite should be scanned: {errors:?}"
    );
    assert!(
        errors[0].starts_with("src/lib.rs:2:"),
        "line of the keyword: {errors:?}"
    );
    assert!(errors[0].contains("no-such-decision-here"), "{errors:?}");
}

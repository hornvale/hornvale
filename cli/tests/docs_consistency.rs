//! Drift checks binding the knowledge architecture together: the idea
//! registry, the frontier essays, and the docs map must not silently
//! diverge. Enforces that every cross-link resolves, every frontier section
//! is indexed in the Contents ToC, and registry IDs stay unique. The
//! architecture these checks defend is described in `docs/README.md` and
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

/// Validate every link in `file`, pushing a message per broken one into
/// `errors`. A link resolves if its path exists (relative to the file's
/// directory; an empty path means the file itself), and, when it carries a
/// `#fragment` into a Markdown file, if that fragment names a real heading.
fn check_links(file: &Path, errors: &mut Vec<String>) {
    let content = read(file);
    let dir = file.parent().expect("doc file should have a parent");
    for url in link_urls(&content) {
        if url.starts_with("http://") || url.starts_with("https://") {
            continue;
        }
        let (path_part, fragment) = match url.split_once('#') {
            Some((p, f)) => (p, Some(f)),
            None => (url.as_str(), None),
        };
        let target = if path_part.is_empty() {
            file.to_path_buf()
        } else {
            dir.join(path_part)
        };
        if !target.exists() {
            errors.push(format!(
                "{}: link `{url}` points at a missing path",
                file.display()
            ));
            continue;
        }
        if let Some(fragment) = fragment {
            if target.extension().and_then(|e| e.to_str()) == Some("md")
                && !anchors(&read(&target)).contains(fragment)
            {
                errors.push(format!(
                    "{}: link `{url}` names an anchor that no heading produces",
                    file.display()
                ));
            }
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
    let frontier = read(&repo_root().join("docs/vision/frontier.md"));
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
    let registry = read(&repo_root().join("docs/vision/idea-registry.md"));
    let mut seen = BTreeSet::new();
    let mut dupes = Vec::new();
    for line in registry.lines() {
        let Some(rest) = line.strip_prefix("| ") else {
            continue;
        };
        let cell = rest.split('|').next().unwrap_or("").trim();
        // An ID is a category prefix, a hyphen, a number, and an optional
        // sub-letter: MAP-9, MAP-9a, LANG-1. Anything else is a header or
        // separator cell.
        let looks_like_id = cell.split_once('-').is_some_and(|(pre, post)| {
            !pre.is_empty()
                && pre.chars().all(|c| c.is_ascii_uppercase())
                && !post.is_empty()
                && post
                    .trim_end_matches(|c: char| c.is_ascii_lowercase())
                    .chars()
                    .all(|c| c.is_ascii_digit())
                && post.starts_with(|c: char| c.is_ascii_digit())
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
        "docs/vision/frontier.md",
        "docs/vision/idea-registry.md",
    ] {
        check_links(&root.join(rel), &mut errors);
    }
    assert!(
        errors.is_empty(),
        "broken cross-links in the knowledge docs:\n  {}",
        errors.join("\n  ")
    );
}

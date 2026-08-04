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

/// One parsed row of the idea registry's tables. `cells` counts the pieces the
/// line splits into on unescaped pipes — a well-formed five-column row splits
/// into seven (an empty piece before the leading `|` and after the trailing
/// one).
struct RegistryRow {
    /// 1-based line number in `idea-registry.md`, for error messages.
    line: usize,
    /// The ID cell (`MAP-7`, `SKY-eclipse-seasons`).
    id: String,
    /// The Idea cell — the prose the length cap applies to.
    idea: String,
    /// The Status cell.
    status: String,
    /// The Where cell — the pointer to where the idea is argued.
    where_cell: String,
    /// Pieces the line splits into on *unescaped* pipes; 7 when well-formed.
    cells: usize,
}

/// A sentinel standing in for `\|` while splitting, so an escaped pipe (which
/// GFM renders as a literal `|` inside a cell) never counts as a separator.
/// Restored before any cell is returned, so lengths and text stay faithful.
const ESCAPED_PIPE: char = '\u{1}';

/// True when `cell` is a registry ID: a category prefix, a hyphen, and either a
/// number with an optional sub-letter (`MAP-9`, `MAP-9a` — the frozen numbered
/// era) or a lowercase slug (`SKY-eclipse-seasons` — decision
/// `0026-slugs-not-numbers`). Anything else is a header or separator cell.
fn looks_like_registry_id(cell: &str) -> bool {
    cell.split_once('-').is_some_and(|(pre, post)| {
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
    })
}

/// Parse `text` as the idea registry, returning one entry per ID-bearing table
/// row. Header and separator rows are skipped.
fn parse_registry(text: &str) -> Vec<RegistryRow> {
    let mut rows = Vec::new();
    for (idx, line) in text.lines().enumerate() {
        if !line.starts_with("| ") {
            continue;
        }
        let masked = line.replace("\\|", &ESCAPED_PIPE.to_string());
        let pieces: Vec<String> = masked
            .split('|')
            .map(|p| p.replace(ESCAPED_PIPE, "\\|").trim().to_string())
            .collect();
        // pieces[0] is the empty text before the leading `|`.
        let Some(id) = pieces.get(1) else { continue };
        if !looks_like_registry_id(id) {
            continue;
        }
        let at = |i: usize| pieces.get(i).cloned().unwrap_or_default();
        rows.push(RegistryRow {
            line: idx + 1,
            id: id.clone(),
            idea: at(2),
            status: at(3),
            where_cell: at(5),
            cells: pieces.len(),
        });
    }
    rows
}

/// Every ID-bearing row of the idea registry.
fn registry_rows() -> Vec<RegistryRow> {
    parse_registry(&read(
        &repo_root().join("book/src/frontier/idea-registry.md"),
    ))
}

#[test]
fn an_escaped_pipe_is_not_a_column_separator() {
    // The trap this parser exists to avoid: a naive split on '|' counts the
    // escaped pipes inside a code span as separators and reports a well-formed
    // row as broken. Both rows below are five-column rows; only the second is
    // malformed.
    let ok = "| MAP-1 | uses `a \\| b` in prose | raw | med | [x](y.md) |";
    let broken = "| MAP-2 | uses `a | b` unescaped | raw | med | [x](y.md) |";
    let rows = parse_registry(&format!("{ok}\n{broken}\n"));
    assert_eq!(rows.len(), 2, "both rows should parse as registry rows");
    assert_eq!(rows[0].cells, 7, "escaped pipes must not split the cell");
    assert_eq!(
        rows[0].idea, "uses `a \\| b` in prose",
        "the escape must survive parsing intact"
    );
    assert_eq!(rows[1].cells, 8, "a bare pipe must split the cell");
}

#[test]
fn registry_rows_have_five_columns() {
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| r.cells != 7)
        .map(|r| {
            format!(
                "{}:{} ({} columns, expected 5) — escape bare `|` in prose as `\\|`",
                r.id,
                r.line,
                r.cells - 2
            )
        })
        .collect();
    assert!(
        offenders.is_empty(),
        "malformed registry rows — mdbook truncates these to five cells, \
         shifting the columns left and DROPPING the Where pointer from the \
         published page:\n  {}",
        offenders.join("\n  ")
    );
}

/// The closed status vocabulary, per `idea-registry.md`'s "How to read a row".
/// Unlike the category prefixes — which `registry_id_prefixes` derives from the
/// file so a newly coined category adapts automatically — this list is
/// deliberately hard-coded. The category vocabulary is open; the status
/// vocabulary is closed, and leaving it open by omission is what let
/// `registered` and three prose-filled Status cells into the file.
const REGISTRY_STATUSES: [&str; 6] = [
    "raw",
    "elaborated",
    "spec'd",
    "shipped",
    "ratified",
    "rejected",
];

/// Reduce a Status cell to its bare token: strip `**` emphasis, a trailing
/// `→ <status>` transition, and any trailing parenthetical (`ratified (0009)`,
/// `shipped (field half)`).
fn normalize_status(cell: &str) -> String {
    let mut s = cell.replace('*', "");
    if let Some((head, _)) = s.split_once('→') {
        s = head.to_string();
    }
    if let Some((head, _)) = s.split_once('(') {
        s = head.to_string();
    }
    s.trim().to_string()
}

#[test]
fn status_normalization_handles_the_documented_forms() {
    assert_eq!(normalize_status("shipped"), "shipped");
    assert_eq!(normalize_status("**shipped**"), "shipped");
    assert_eq!(normalize_status("ratified (0009)"), "ratified");
    assert_eq!(normalize_status("shipped (field half)"), "shipped");
    assert_eq!(normalize_status("rejected → ratified"), "rejected");
    assert_eq!(normalize_status("registered"), "registered"); // not a status
}

#[test]
fn registry_statuses_use_the_closed_vocabulary() {
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| !REGISTRY_STATUSES.contains(&normalize_status(&r.status).as_str()))
        .map(|r| {
            format!(
                "{}:{} — status {:?}",
                r.id,
                r.line,
                r.status.chars().take(60).collect::<String>()
            )
        })
        .collect();
    assert!(
        offenders.is_empty(),
        "registry rows whose Status is outside the closed vocabulary \
         {REGISTRY_STATUSES:?}:\n  {}",
        offenders.join("\n  ")
    );
}

/// The Idea-cell budget, in characters. A row is a shelf-mark: what the idea
/// is, and a pointer to where it is argued. The argument lives in
/// `frontier.md`; the campaign narrative lives in the chronicle the Where cell
/// links. Sibling norm: `docs/decisions/README.md` — "keep each record short …
/// if it needs a page, it is probably a spec".
///
/// The cap is on the Idea cell only. The Where column carries full GitHub blob
/// URLs by mandate (`book/src/frontier/CLAUDE.md`), and taxing a row for
/// carrying pointers is backwards.
const REGISTRY_IDEA_CAP: usize = 600;

/// Rows over `REGISTRY_IDEA_CAP` on the day the cap landed. **Append-never:**
/// entries may be removed as rows are compacted, never added. A new row over
/// the cap is a failure, not a fixture edit — that ratchet is the whole
/// mechanism (the pattern is the type audit's `pending(wave-N)`, decision
/// 0028).
fn registry_length_waivers() -> BTreeSet<&'static str> {
    include_str!("fixtures/registry-length-waivers.txt")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect()
}

#[test]
fn registry_idea_cells_are_within_budget() {
    let waived = registry_length_waivers();
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| r.idea.chars().count() > REGISTRY_IDEA_CAP)
        .filter(|r| !waived.contains(r.id.as_str()))
        .map(|r| format!("{}:{} — {} chars", r.id, r.line, r.idea.chars().count()))
        .collect();
    assert!(
        offenders.is_empty(),
        "registry Idea cells over {REGISTRY_IDEA_CAP} chars. A row is an index \
         entry, not an essay — compact it (the prose is redundant with the \
         chronicle the Where cell links), relocate it (move the argument to a \
         frontier.md section and flip `raw` → `elaborated`), or trim it:\n  {}",
        offenders.join("\n  ")
    );
}

#[test]
fn the_waiver_list_only_shrinks() {
    let waived = registry_length_waivers();
    let rows = registry_rows();
    let ids: BTreeSet<&str> = rows.iter().map(|r| r.id.as_str()).collect();

    let unknown: Vec<&str> = waived
        .iter()
        .filter(|w| !ids.contains(*w))
        .copied()
        .collect();
    assert!(
        unknown.is_empty(),
        "waived IDs absent from the registry — the waiver list is append-never \
         and rows are permanent, so this means a typo or a renamed ID:\n  {}",
        unknown.join("\n  ")
    );

    let compacted: Vec<&str> = waived
        .iter()
        .filter(|w| {
            rows.iter()
                .find(|r| r.id.as_str() == **w)
                .is_some_and(|r| r.idea.chars().count() <= REGISTRY_IDEA_CAP)
        })
        .copied()
        .collect();
    assert!(
        compacted.is_empty(),
        "these rows are now within budget — remove them from \
         fixtures/registry-length-waivers.txt so the ratchet holds:\n  {}",
        compacted.join("\n  ")
    );
}

/// The numbered registry IDs that existed when decision
/// `0026-slugs-not-numbers`'s freeze was finally applied to registry rows.
/// Append-never: an ID may leave this list only by leaving the registry, which
/// never happens (rows are permanent). A *new* numbered ID fails.
fn frozen_numbered_ids() -> BTreeSet<&'static str> {
    include_str!("fixtures/registry-numbered-ids.txt")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect()
}

#[test]
fn no_new_numbered_registry_ids() {
    let frozen = frozen_numbered_ids();
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| {
            r.id.split_once('-')
                .is_some_and(|(_, post)| post.starts_with(|c: char| c.is_ascii_digit()))
        })
        .filter(|r| !frozen.contains(r.id.as_str()))
        .map(|r| format!("{}:{}", r.id, r.line))
        .collect();
    assert!(
        offenders.is_empty(),
        "new numbered registry IDs — decision `0026-slugs-not-numbers` requires \
         category+slug for new rows (`LANG-exonyms`, not `LANG-6`); the \
         numbered era is frozen, not extended:\n  {}",
        offenders.join("\n  ")
    );
}

#[test]
fn every_registry_row_carries_a_pointer() {
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| r.where_cell.is_empty() || r.where_cell == "—")
        .map(|r| format!("{}:{}", r.id, r.line))
        .collect();
    assert!(
        offenders.is_empty(),
        "registry rows with an empty Where cell — a row is a pointer; without \
         one there is nothing to point at:\n  {}",
        offenders.join("\n  ")
    );
}

#[test]
fn registry_ids_are_unique() {
    let mut seen = BTreeSet::new();
    let mut dupes = Vec::new();
    for row in registry_rows() {
        if !seen.insert(row.id.clone()) {
            dupes.push(format!("{}:{}", row.id, row.line));
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
    registry_rows()
        .iter()
        .filter_map(|r| r.id.split_once('-'))
        .filter(|(_, post)| post.starts_with(|c: char| c.is_ascii_digit()))
        .map(|(pre, _)| pre.to_string())
        .collect()
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

/// The history gallery page names a cell in hand-authored prose *and* renders
/// that cell's `history` output in a fenced block. The two must agree, and the
/// block must not be empty.
///
/// They silently disagreed once. The Sundering's moving-sea epoch emptied the
/// then-pinned cell 36918 — correctly, since the epoch moved the sea and that
/// cell stopped being a settleable clearing — while the paragraph above it went
/// on describing a bugbear lineage that returned five times over two centuries.
/// The artifact drift check passed the whole time, because the *generated* half
/// was current; nothing gated the hand-authored half. That is the mirror of the
/// usual freshness bug, and this is the cheap guard for it.
#[test]
fn the_history_page_prose_names_the_cell_it_renders() {
    let page = read(&repo_root().join("book/src/gallery/history-seed-42.md"));

    // The prose cites "cell N"; the rendered block heads with "The clearing at cell N".
    let rendered = page
        .lines()
        .find_map(|l| l.strip_prefix("The clearing at cell "))
        .map(|c| c.trim().to_string())
        .expect("the rendered block heads with 'The clearing at cell N'");

    let cited = page
        .lines()
        .filter(|l| !l.starts_with("The clearing at cell "))
        .find_map(|l| {
            l.split("cell ").nth(1).and_then(|rest| {
                let n: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                (!n.is_empty()).then_some(n)
            })
        })
        .expect("the framing prose cites a cell id");

    assert_eq!(
        cited, rendered,
        "history-seed-42.md prose cites cell {cited} but renders cell {rendered} — \
         the hand-authored half has gone stale against the generated half"
    );

    assert!(
        !page.contains("Nothing ever settled here"),
        "the history showcase page renders an EMPTY column — it is the showcase \
         for stratigraphy and is telling readers the feature did nothing. \
         Repoint `history_site` in scripts/regenerate-artifacts.sh at a cell with \
         a real column and rewrite the framing paragraph to match it."
    );

    // Matching cell ids is necessary but nowhere near sufficient, and this
    // test learned that the hard way one commit after it was written. The
    // Tithe's accumulation term re-baselined the deep-history bake while this
    // page's paragraph was being authored against the pre-Tithe bake; cell
    // 28414 kept its id and its twelve layers, so both checks above passed,
    // while the prose went on naming hobgoblins and kobolds at a cell now
    // held by bugbears, over centuries it no longer spans.
    //
    // So also check the two classes of claim that are mechanically
    // checkable: every people the prose names, and every year it cites, must
    // actually appear in the rendered block. Narrative claims (how many
    // souls, what ended them) still are not covered — but those are the ones
    // an author re-reads, and these are the ones that rot silently.
    let (prose, block) = page
        .split_once("```text")
        .expect("the page has a fenced render block");

    // The Generalist added a sixth people (human); appended so this loop
    // still covers every settling people if the hand-authored prose is ever
    // edited to name one (Fix round 1, Finding 1's shape, caught by a
    // follow-up grep rather than a live failure — the guard below only
    // checks names the prose actually contains, so this was dormant, not
    // red).
    for people in ["bugbear", "hobgoblin", "kobold", "goblin", "gnoll", "human"] {
        if prose.to_lowercase().contains(people) {
            assert!(
                block.to_lowercase().contains(people),
                "history-seed-42.md prose names {people}s, but no {people} appears \
                 in the rendered column — the hand-authored half has gone stale \
                 against the generated half"
            );
        }
    }

    for year in prose.split("the year ").skip(1).map(|rest| {
        rest.chars()
            .take_while(char::is_ascii_digit)
            .collect::<String>()
    }) {
        assert!(
            !year.is_empty() && block.contains(&format!("year {year}")),
            "history-seed-42.md prose cites the year {year}, which the rendered \
             column never reports — the hand-authored half has gone stale against \
             the generated half"
        );
    }
}

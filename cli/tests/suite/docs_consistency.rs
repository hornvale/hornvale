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

/// True when `cell` is a Markdown table separator segment — the `------` or
/// `:---:` pieces of the rule line under a header row. These are built only
/// from hyphens, colons, and spaces, which no real cell (ID or otherwise) is.
fn looks_like_table_separator(cell: &str) -> bool {
    !cell.is_empty() && cell.chars().all(|c| c == '-' || c == ':' || c == ' ')
}

/// **Property: every `| `-prefixed line in the idea registry is either a
/// table header, a table separator, or a row `parse_registry` actually
/// parses as an ID row. Direction enforced: a line that is data (not header,
/// not separator) but whose first cell fails `looks_like_registry_id` is a
/// failure here, not a silent skip.**
///
/// `parse_registry` treats "first cell doesn't look like an ID" as "not a
/// data row" and moves on — the right call for a header or separator line,
/// and exactly the wrong one for a data row whose ID merely has an
/// unanticipated shape. That gap is how `CLIENT-22-glyphs-rejected` evaded
/// every other guard in this file (length cap, status vocabulary, five-column
/// shape, Where-link validation): its post-hyphen segment started with a
/// digit, `looks_like_registry_id` said no, and the row vanished rather than
/// failing. This test closes the class rather than the instance — it does
/// not touch `looks_like_registry_id`'s matching rules, so a future ID with a
/// different unanticipated shape still gets caught here instead of vanishing
/// again.
#[test]
fn every_registry_table_row_is_a_parseable_id_row() {
    let text = read(&repo_root().join("book/src/frontier/idea-registry.md"));
    let offenders: Vec<String> = text
        .lines()
        .enumerate()
        .filter_map(|(idx, line)| {
            if !line.starts_with("| ") {
                return None;
            }
            let masked = line.replace("\\|", &ESCAPED_PIPE.to_string());
            let pieces: Vec<String> = masked
                .split('|')
                .map(|p| p.replace(ESCAPED_PIPE, "\\|").trim().to_string())
                .collect();
            let id = pieces.get(1)?;
            if id == "ID" || looks_like_table_separator(id) || looks_like_registry_id(id) {
                return None;
            }
            Some(format!(
                "{}: first cell {:?} did not parse as an ID",
                idx + 1,
                id
            ))
        })
        .collect();
    assert!(
        offenders.is_empty(),
        "idea-registry.md table rows that are neither a header, a separator, \
         nor a parseable ID — every other check in this file (`registry_rows`) \
         is BLIND to a row like this, because `parse_registry` silently drops \
         it instead of failing. Rename the ID so `looks_like_registry_id` \
         accepts it, or if the ID shape itself should widen, do that \
         deliberately and explain why — do not leave the row unparsed:\n  {}",
        offenders.join("\n  ")
    );
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
///
/// The vocabulary was opened exactly once, deliberately, by decision 0131
/// (`refuted`, distinct from `rejected`: a measurement rather than a
/// decision). It is closed again at seven — 0131 is not a precedent for an
/// eighth.
const REGISTRY_STATUSES: [&str; 7] = [
    "raw",
    "elaborated",
    "spec'd",
    "shipped",
    "ratified",
    "rejected",
    "refuted",
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

#[test]
fn refuted_is_an_admissible_status() {
    assert!(REGISTRY_STATUSES.contains(&"refuted"));
}

/// A `refuted` row must cite the campaign or decision that refuted it.
/// This is stricter than any other status carries, and deliberately: an
/// uncited refutation is an assertion with no way to check it, which is the
/// exact defect PROC-project-epistemology names.
#[test]
fn every_refuted_row_cites_its_evidence() {
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| normalize_status(&r.status) == "refuted")
        .filter(|r| !r.status.contains('('))
        .map(|r| r.id.clone())
        .collect();
    assert!(
        offenders.is_empty(),
        "refuted rows must cite what refuted them, e.g. `refuted (The Mire)`: {offenders:?}"
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
    include_str!("../fixtures/registry-length-waivers.txt")
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
    include_str!("../fixtures/registry-numbered-ids.txt")
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

/// No two decision records share a leading number.
///
/// **What this defends.** A decision number is the project's citation handle:
/// prose, chronicles, registry rows, `#[ignore]` reasons and `CLAUDE.md` all
/// refer to decisions as bare four-digit numbers. Decisions are append-only, so
/// a number is supposed to name exactly one ratified choice forever. Two
/// records sharing one makes every such citation ambiguous, and the log stops
/// being the durable, grep-able home `docs/decisions/README.md` promises.
///
/// **Why the defect is invisible without this check.** It is minted by two
/// campaigns running in parallel, each claiming the next free number against
/// the `main` it branched from, and it survives every mechanism that would
/// normally catch a collision:
///
/// - **git raises no conflict.** The two records are *different files* —
///   `0134-a-partition-statistic-….md` and `0134-a-capability-corpus-….md` —
///   so a merge keeps both, silently, with no marker to resolve.
/// - **the digest renders one line per file**, so a duplicate number appears
///   in `docs/digest/decisions-in-force.md` as an ordinary extra bullet. It
///   reads as a normal index, sorted and plausible, and nothing about it looks
///   wrong to a human skimming for drift.
/// - **the link checks above pass**, because both filenames exist and every
///   cross-link resolves. Reference integrity is not the property at issue.
///
/// This fired for real: The Ballast and The Compendium both authored an 0134
/// (2026-08-15). Nothing in the repository objected; it was caught by a human
/// reading the two branches side by side at a merge gate. The later campaign
/// renumbered — append-only means the earlier claim wins — and added this test
/// so the next collision is caught by the suite instead.
#[test]
fn decision_numbers_are_unique() {
    let mut seen: BTreeSet<u32> = BTreeSet::new();
    let mut dupes = Vec::new();
    for (number, name) in decision_records() {
        if !seen.insert(number) {
            dupes.push(format!("{number:04} claimed again by {name}"));
        }
    }
    assert!(
        dupes.is_empty(),
        "duplicate decision numbers (a decision number is a permanent, unique \
         citation handle; two records sharing one makes every bare-number \
         citation ambiguous). Renumber the record that was ratified LATER — \
         decisions are append-only, so the earlier claim keeps the number — and \
         sweep every citation of it:\n  {}",
        dupes.join("\n  ")
    );
}

/// A decision record's `# NNNN.` title matches the number in its filename.
///
/// The renumber this file's sibling check exists for is a two-part edit: the
/// filename and the title inside it. Moving one without the other leaves a
/// record whose own first line disagrees with how everything cites it, which
/// no link check can see — the file resolves either way.
#[test]
fn a_decision_records_title_matches_its_filename() {
    let dir = repo_root().join("docs/decisions");
    let mut offenders = Vec::new();
    for (number, name) in decision_records() {
        let text = read(&dir.join(&name));
        let first = text.lines().next().unwrap_or_default();
        let expected = format!("# {number:04}.");
        if !first.starts_with(&expected) {
            offenders.push(format!(
                "{name}: title reads {first:?}, expected {expected:?}"
            ));
        }
    }
    assert!(
        offenders.is_empty(),
        "decision records whose title number disagrees with their filename:\n  {}",
        offenders.join("\n  ")
    );
}

/// A `Decision block: NNNN-MMMM` reservation as it is actually written into a
/// spec or plan's committed prose
/// (`docs/superpowers/{specs,plans}/*.md`) — **not** the reservation ledger.
/// `scripts/decision-block.sh`'s `blocks.tsv` lives under `$HOME`, on the
/// canonical box, reachable only over ssh, and has never been committed
/// (Task 7, The Attestation, verified via `git log --all -- '*blocks.tsv'`
/// returning nothing). This is the closest thing to it a checkout can see:
/// what a campaign told the repository, in its own words, it had reserved.
struct DeclaredDecisionBlock {
    /// The campaign a spec/plan filename names — see [`campaign_slug`]. A
    /// spec and its own plan restate one reservation, not two, so this is
    /// what distinguishes "the same campaign said it twice" from "two
    /// campaigns collided".
    campaign: String,
    /// Path relative to the repo root, for error messages.
    file: String,
    start: u32,
    end: u32,
}

/// Every maximal run of ASCII digits in `s` that is exactly four digits long,
/// in order, parsed as a number. Runs of other lengths (a year, a lone
/// "main ceiling NNNN" aside past the two numbers we want) are skipped
/// rather than mis-parsed.
fn four_digit_runs(s: &str) -> Vec<u32> {
    let bytes = s.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i].is_ascii_digit() {
            let start = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i - start == 4 {
                out.push(s[start..i].parse::<u32>().expect("4 ascii digits"));
            }
        } else {
            i += 1;
        }
    }
    out
}

/// Parse a `Decision block: NNNN-MMMM` declaration out of one line, if
/// present.
///
/// The convention is committed prose, not a fixed field — a census of every
/// occurrence in `docs/superpowers/{specs,plans}/*.md` (2026-08-30) found
/// four independent variations: the colon inside or outside the bold run
/// (`**Decision block:**` vs `**Decision block**:`), a hyphen or an en dash
/// between the numbers, a doubled bold around the numbers themselves
/// (`**0436–0445**`), and a trailing `(main ceiling NNNN at reservation)`
/// aside that itself contains a four-digit number. Rather than encode each
/// shape, this takes the first two four-digit numbers following the phrase
/// "decision block" (case-insensitive) on the line and ignores everything
/// else — which is why the ceiling aside is harmless (it is the *third*
/// four-digit run, never consulted) and why a line that only *mentions* the
/// phrase with no numbers attached (prose pointing at `make decision-block`)
/// correctly yields `None` rather than a bogus match.
fn parse_decision_block_line(line: &str) -> Option<(u32, u32)> {
    let lower = line.to_ascii_lowercase();
    let anchor = lower.find("decision block")?;
    let after = line.get(anchor + "decision block".len()..)?;
    let runs = four_digit_runs(after);
    (runs.len() >= 2).then(|| (runs[0], runs[1]))
}

/// The campaign a spec/plan filename names: strip the `YYYY-MM-DD-` prefix
/// every file in both directories carries (11 bytes), then strip a spec's
/// trailing `-design` — `2026-08-26-the-quadrat-design.md` and
/// `2026-08-26-the-quadrat.md` (its plan) both name `the-quadrat`.
fn campaign_slug(filename: &str) -> String {
    let stem = filename.strip_suffix(".md").unwrap_or(filename);
    let after_date = stem.get(11..).unwrap_or(stem);
    after_date
        .strip_suffix("-design")
        .unwrap_or(after_date)
        .to_string()
}

/// Every `Decision block: NNNN-MMMM` declaration committed anywhere in
/// `docs/superpowers/specs/` or `docs/superpowers/plans/`, one per file (the
/// first matching line — a campaign declares its reservation once, at the
/// top), in directory-then-filename order.
fn decision_block_declarations() -> Vec<DeclaredDecisionBlock> {
    let root = repo_root();
    let mut out = Vec::new();
    for dir in ["docs/superpowers/specs", "docs/superpowers/plans"] {
        let full_dir = root.join(dir);
        let mut names: Vec<String> = fs::read_dir(&full_dir)
            .unwrap_or_else(|e| panic!("reading {}: {e}", full_dir.display()))
            .map(|e| {
                e.expect("dir entry")
                    .file_name()
                    .to_string_lossy()
                    .into_owned()
            })
            .filter(|n| n.ends_with(".md"))
            .collect();
        names.sort();
        for name in names {
            let text = read(&full_dir.join(&name));
            if let Some((start, end)) = text.lines().find_map(parse_decision_block_line) {
                out.push(DeclaredDecisionBlock {
                    campaign: campaign_slug(&name),
                    file: format!("{dir}/{name}"),
                    start,
                    end,
                });
            }
        }
    }
    out
}

/// The number of `Decision block` declarations
/// [`decision_block_declarations`] found in a full census taken 2026-08-30
/// (Task 7, The Attestation). This is the ratchet half of the check below,
/// and it exists for a specific reason: the disjointness assertion can only
/// object to what the extractor actually sees, so a future spec that
/// rephrases the convention past what [`parse_decision_block_line`]
/// recognizes would make disjointness go quiet, not red — coverage silently
/// shrinking while the check keeps reporting green, which is exactly the
/// failure this campaign exists to name ("an absence has no row" — spec
/// `2026-08-29-the-attestation-design.md` §2, "The thesis"). The count only
/// grows — a campaign is never un-drafted — so a drop always means the
/// parser stopped recognizing a real declaration, never that fewer campaigns
/// reserved blocks.
const EXPECTED_DECISION_BLOCK_DECLARATIONS: usize = 19;

#[test]
fn decision_block_declaration_count_has_not_dropped() {
    let found = decision_block_declarations().len();
    assert!(
        found >= EXPECTED_DECISION_BLOCK_DECLARATIONS,
        "found {found} `Decision block` declarations in \
         docs/superpowers/{{specs,plans}}/*.md, expected at least \
         {EXPECTED_DECISION_BLOCK_DECLARATIONS}. This list only grows, so a \
         drop means `parse_decision_block_line` stopped recognizing a real \
         declaration — a rephrased header, an unfamiliar dash, a moved colon \
         — not that a campaign un-reserved a block. Read the file that \
         dropped out and widen the parser; if the count instead rose, raise \
         this constant to match."
    );
}

/// Known, already-resolved overlaps between two campaigns' declared
/// `Decision block` reservations, append-never — the same shape as
/// `registry_length_waivers` above. A pair earns a place here only when the
/// collision already happened and the record of it is the ORIGINAL
/// reservation each campaign actually made, so correcting either header
/// would misstate history rather than fix a stale value.
///
/// The one entry: The Scarf reserved `0286-0295` and used only `0286`. The
/// Quadrat's spec, drafted a day later, independently reserved the identical
/// `0286-0295` — `docs/retrospectives/the-quadrat.md`: "a sibling campaign's
/// spec reserved the same range, landed first, and took 0286 ... nothing
/// detected the double reservation at either drafting, and nothing would
/// have — the specs were written a day apart and neither read the other's
/// header." The Quadrat shifted its own usage to `0287-0295` once the
/// collision surfaced, but its spec header still records what it actually
/// reserved, which is what this test reads. A new pair must never be added
/// here — the check exists to make sure this is the last one.
fn known_decision_block_overlaps() -> BTreeSet<(String, String)> {
    [("the-quadrat", "the-scarf")]
        .into_iter()
        .map(|(a, b)| overlap_key(a, b))
        .collect()
}

/// A pair of campaign slugs, ordered so `(a, b)` and `(b, a)` produce the
/// same key regardless of which side the caller happened to name first.
fn overlap_key(a: &str, b: &str) -> (String, String) {
    if a <= b {
        (a.to_string(), b.to_string())
    } else {
        (b.to_string(), a.to_string())
    }
}

/// No two DIFFERENT campaigns' committed `Decision block` declarations
/// overlap, apart from the one pair already known and waived above.
///
/// **What this defends, and what it deliberately does not.** Spec §5a
/// records that no check asks whether a decision record's number falls
/// inside the block its author actually reserved — that reservation lives in
/// `scripts/decision-block.sh`'s ledger, on the canonical box, readable only
/// over ssh, which a workspace test must not do (Task 7 report verified this
/// directly: `HV_BLOCK_DIR` defaults under `$HOME`, both `make
/// decision-blocks` and `decision-block-request.sh` reach it only by `ssh`,
/// and `blocks.tsv` has never been committed under any name). This check is a
/// narrower, repo-only relative: instead of comparing a record against the
/// real reservation, it compares every campaign's own DECLARATION of its
/// reservation — the `Decision block: NNNN-MMMM` line a drafted spec (and
/// often its plan) already commits — against every other campaign's.
///
/// **It is a strict subset of the specced check, never a replacement for
/// it.** It is blind to a record minted with no declared block at all —
/// `campaign/the-stride` minting `0160` inside `campaign/the-burr`'s reserved
/// `0156-0165` (2026-08-19): the-stride's spec and plan declare no block
/// whatsoever, so there is nothing here to compare it against. That is
/// exactly the shape that most needs the live ledger, and if that ledger is
/// ever made committable — a separate, infrastructure-scope decision — the
/// originally specced per-record check is still owed.
///
/// **What it does catch, and it already has, once.** See
/// `known_decision_block_overlaps` above: The Scarf and The Quadrat both
/// committed `Decision block: 0286–0295` in their own spec headers, drafted a
/// day apart, and per `docs/retrospectives/the-quadrat.md` "nothing detected
/// the double reservation at either drafting, and nothing would have ...
/// neither read the other's header." This test reads both. Run against
/// either commit once both headers existed, it would have failed before
/// either campaign minted a single decision record, instead of the collision
/// being caught by the queue operator by hand after `0286` was already
/// taken.
///
/// **A weaker, honestly-hedged claim about The Overture.** The Overture's
/// spec committed `Decision block: 0357–0366` (verified directly: `git show
/// 570ef81d4:docs/superpowers/specs/2026-08-28-the-overture-design.md`),
/// which nothing had reserved and which by close collided with two other
/// campaigns' real reservations; it was corrected to `0436–0445` before the
/// branch closed. Whether this test would have caught it live depends on
/// merge-ancestry timing this record does not resolve — specifically,
/// whether the colliding campaigns' own spec commits had already reached the
/// tree the Overture branch was gating against at 11:24 that morning. Left
/// as a plausible hedge, not upgraded to a claim.
#[test]
fn decision_blocks_do_not_overlap_across_campaigns() {
    let declared = decision_block_declarations();
    let waived = known_decision_block_overlaps();
    let mut offenders = Vec::new();
    let mut seen_waived: BTreeSet<(String, String)> = BTreeSet::new();

    for (i, a) in declared.iter().enumerate() {
        for b in declared.iter().skip(i + 1) {
            if a.campaign == b.campaign {
                continue; // a campaign's own spec and plan restate one reservation
            }
            if a.start > b.end || b.start > a.end {
                continue; // disjoint
            }
            let key = overlap_key(&a.campaign, &b.campaign);
            if waived.contains(&key) {
                seen_waived.insert(key);
                continue;
            }
            offenders.push(format!(
                "{} ({}: {:04}-{:04}) overlaps {} ({}: {:04}-{:04})",
                a.campaign, a.file, a.start, a.end, b.campaign, b.file, b.start, b.end
            ));
        }
    }

    assert!(
        offenders.is_empty(),
        "two campaigns' committed `Decision block` declarations overlap — the \
         same defect that collided The Scarf and The Quadrat on 0286-0295 \
         (docs/retrospectives/the-quadrat.md). Read the other campaign's \
         header before drafting a new one, and if this fires anyway, whoever \
         committed second must reserve and declare a fresh range:\n  {}",
        offenders.join("\n  ")
    );

    let stale_waivers: Vec<String> = waived
        .iter()
        .filter(|pair| !seen_waived.contains(*pair))
        .map(|(a, b)| format!("{a}/{b}"))
        .collect();
    assert!(
        stale_waivers.is_empty(),
        "waived decision-block overlaps that no longer overlap in the \
         current declarations — remove from `known_decision_block_overlaps` \
         so the waiver list stays honest:\n  {}",
        stale_waivers.join("\n  ")
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
fn the_history_page_prose_names_the_vertex_it_renders() {
    let page = read(&repo_root().join("book/src/gallery/history-seed-42.md"));

    // The prose cites "vertex N"; the rendered block heads with "The clearing at vertex N".
    let rendered = page
        .lines()
        .find_map(|l| l.strip_prefix("The clearing at vertex "))
        .map(|c| c.trim().to_string())
        .expect("the rendered block heads with 'The clearing at vertex N'");

    let cited = page
        .lines()
        .filter(|l| !l.starts_with("The clearing at vertex "))
        .find_map(|l| {
            l.split("vertex ").nth(1).and_then(|rest| {
                let n: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                (!n.is_empty()).then_some(n)
            })
        })
        .expect("the framing prose cites a vertex id");

    assert_eq!(
        cited, rendered,
        "history-seed-42.md prose cites vertex {cited} but renders vertex {rendered} — \
         the framing prose in scripts/regenerate-artifacts.sh (the `history_site` \
         block, ~lines 219-243) has gone stale against the rendered column. Fix \
         the printf literals there, not this generated .md file — it is \
         overwritten wholesale on the next `make rebaseline`."
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
    //
    // The Delvers (C2c) appends the dwarf family's three, taking the settling
    // roster to nine. **This list is authored and the roster is not**, so it
    // goes stale silently and in the safe-looking direction: a missing people
    // makes the guard check LESS, never fail, which is why the note above
    // records the last omission as "dormant, not red". Anyone adding a
    // settling kind must append it here.
    //
    // The Radiation (C2d) appends the elf family's six, taking the settling
    // roster to fifteen. It was omitted for the whole campaign and found at
    // the close, dormant exactly as this comment predicts: the page's prose
    // named kobolds throughout, so no elf name was ever there to check. The
    // omission cost nothing this time and the mechanism that hides it is
    // unchanged — a list authored beside a roster that is not.
    for people in [
        "bugbear",
        "hobgoblin",
        "kobold",
        "goblin",
        "gnoll",
        "human",
        "desert-dwarf",
        "gully-dwarf",
        "hill-dwarf",
        "desert-elf",
        "drow",
        "high-elf",
        "sea-elf",
        "snow-elf",
        "wood-elf",
    ] {
        if prose.to_lowercase().contains(people) {
            assert!(
                block.to_lowercase().contains(people),
                "history-seed-42.md prose names {people}s, but no {people} appears \
                 in the rendered column — fix the framing prose in \
                 scripts/regenerate-artifacts.sh (the `history_site` block, \
                 ~lines 219-243), not this generated .md file, which is \
                 overwritten wholesale on the next `make rebaseline`."
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
             column never reports — fix the framing prose in \
             scripts/regenerate-artifacts.sh (the `history_site` block, \
             ~lines 219-243), not this generated .md file, which is \
             overwritten wholesale on the next `make rebaseline`."
        );
    }
}

/// Every numbered decision record in `docs/decisions/`, as `(number,
/// filename)`, ascending.
///
/// Filenames are `NNNN-slug.md`; `README.md` and any other unnumbered file is
/// skipped, using the same "numbered stem" shape as
/// `decision_cites_in_sources_resolve`.
///
/// This keeps the filename alongside the number because the duplicate check
/// has to be able to *name* the colliding record — "0134 is claimed twice" is
/// not actionable, "0134 claimed again by `0134-a-capability-corpus-….md`" is.
/// It is the one parse of this directory; `decision_numbers` is a view over it,
/// so the two checks can never disagree about what counts as a record.
fn decision_records() -> Vec<(u32, String)> {
    let dir = repo_root().join("docs/decisions");
    let mut records: Vec<(u32, String)> = fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("reading {}: {e}", dir.display()))
        .map(|e| e.expect("dir entry").file_name())
        .filter_map(|name| {
            let name = name.to_string_lossy();
            let stem = name.strip_suffix(".md")?;
            // `NNNN-slug`: four digits, a hyphen, then a non-empty slug.
            if stem.len() > 5 && stem.as_bytes()[4] == b'-' {
                Some((stem[..4].parse::<u32>().ok()?, name.into_owned()))
            } else {
                None
            }
        })
        .collect();
    records.sort();
    records
}

/// The numbered decision records in `docs/decisions/`, ascending.
fn decision_numbers() -> Vec<u32> {
    decision_records().into_iter().map(|(n, _)| n).collect()
}

/// The decision log **starts at 0001**. Contiguity above that is no longer
/// asserted, and the reason is a change in what "legitimate" means rather
/// than a relaxation of standards.
///
/// **What this test used to assert, and why that stopped working.** It
/// required a contiguous run with no holes, on the stated premise that "no
/// legitimate operation removes a number". That premise held while decisions
/// were minted rarely and usually on `main` — the premise decision 0043
/// itself relied on when it returned the log to numbers. It no longer holds:
/// the log went from 43 records at 0043's ratification (2026-07-13) to 141
/// in 36 days, about 2.7 a day, minted by parallel campaigns off `main`.
///
/// Under that load a hole is a NORMAL outcome — a campaign renumbers away
/// from a collision, or withdraws a record it drafted — so the check could no
/// longer tell a lost record from a legitimate gap. It did not become wrong;
/// it lost its discriminating power, which is the same thing as being unable
/// to answer the question it was asked.
///
/// **And it actively caused the collisions the uniqueness guard exists to
/// catch.** `docs/retrospectives/the-sluice.md` filed this before either
/// guard shipped: "The no-gaps and no-collision invariants on
/// `docs/decisions/` are mutually exclusive under parallel campaigns, and the
/// gap check pushes an author into the collision the uniqueness check exists
/// to catch." Contiguity made the next-free number the ONLY committable one,
/// which is exactly the number another campaign is most likely to hold. Both
/// guards shipped anyway, a month apart, each creating what the other caught.
///
/// It also SERIALIZED the queue, which nothing had priced. A campaign
/// numbering above a held campaign could not commit at all — observed
/// 2026-08-18, when a campaign with its close written was blocked behind a
/// campaign that was itself blocked on unrelated heavy failures. Decision
/// 0043 knowingly accepted a COORDINATION cost ("sessions minting a decision
/// off main must again confirm the next free number at merge"); the gap check
/// converted that into a hard block, a change in kind that was never
/// re-derived against 0043.
///
/// **What is kept, and why the start is different.** A log beginning at
/// `0002` still means record 0001 was lost or misnamed, and no parallel-
/// campaign workflow produces that legitimately — so this half retains the
/// discriminating power the contiguity half lost, at no coordination cost.
/// Duplicates remain guarded by `decision_numbers_are_unique`, which catches
/// the failure that actually corrupts a citation handle.
///
/// The original incident stays worth knowing: absorbing The Radiation into
/// The Grain collided on `0120`, a mechanical shift moved the unmerged
/// records four places instead of one, and `0121`-`0123` opened. Under the
/// rule here that botched renumber would leave a legible hole rather than a
/// red gate — the cost accepted for unblocking parallel campaigns.
#[test]
fn the_decision_log_starts_at_0001() {
    let numbers = decision_numbers();
    assert!(
        !numbers.is_empty(),
        "no numbered records found in docs/decisions/ — the decision log \
         cannot be empty, so this is a broken path or a changed filename \
         convention, not a real state"
    );

    let first = *numbers.first().expect("non-empty");
    assert_eq!(
        first, 1,
        "the decision log starts at {first:04}, not 0001 — record 0001 is \
         missing. The log is append-only and records are superseded rather \
         than deleted, so a missing first record means it was lost or \
         misnamed, never retired."
    );
}

/// Suffixes a spec file's stem may carry beyond a plan's own slug. A plan is
/// named `<date>-<slug>.md`; its spec is almost always
/// `<date>-<slug>-design.md`, with `-metaplan`, `-brief` and
/// `-question-space` covering the handful of historical exceptions observed
/// in `docs/superpowers/specs/` (`2026-08-07-the-journal-brief.md`,
/// `2026-07-07-year-2-metaplan-design.md`'s siblings, and
/// `2026-08-11-the-ford-stage-2-question-space.md`). The empty string covers
/// the rare case where spec and plan share the identical stem.
const SPEC_SUFFIXES: &[&str] = &["", "-design", "-metaplan", "-brief", "-question-space"];

/// Every `.md` file stem directly inside `dir`.
fn file_stems(dir: &Path) -> BTreeSet<String> {
    fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("reading {}: {e}", dir.display()))
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "md"))
        .filter_map(|path| path.file_stem().map(|s| s.to_string_lossy().into_owned()))
        .collect()
}

/// Every campaign slug (a plan's filename stem, e.g. `2026-08-30-the-cartulary`
/// — the same string a ledger for it would be named after) that has both a
/// spec and a plan today, matched by exact slug: the spec's stem must equal
/// the plan's stem plus one of `SPEC_SUFFIXES`.
///
/// # What this cannot see
///
/// The match requires spec and plan to share the exact same date-plus-slug
/// stem. Real historical campaigns violate that in ways this function does
/// not chase: a spec written a day before its plan
/// (`2026-08-27-the-precedence-design.md` / `2026-08-28-the-precedence.md`),
/// a plan that inserts a word the spec's slug does not carry
/// (`2026-08-20-the-deed-design.md` / `2026-08-20-the-deed-state.md`), or an
/// umbrella spec covering several separately-dated sub-campaign plans
/// (`campaign-2-the-sky-design.md` covering `campaign-2a-genesis.md`,
/// `campaign-2b-sky-debut.md`, ...). Those campaigns are invisible to this
/// function and so never appear in its output at all — not flagged missing a
/// ledger, not carried in the exemption list, simply outside what this check
/// evaluates. A fuzzier matcher could reclaim some of them; it would also
/// risk a false pairing nobody could verify by eye. This function trades
/// recall for a rule any reader can check against the two directories
/// directly.
fn campaigns_with_spec_and_plan() -> BTreeSet<String> {
    let root = repo_root();
    let spec_stems = file_stems(&root.join("docs/superpowers/specs"));
    file_stems(&root.join("docs/superpowers/plans"))
        .into_iter()
        .filter(|plan_stem| {
            SPEC_SUFFIXES
                .iter()
                .any(|suffix| spec_stems.contains(&format!("{plan_stem}{suffix}")))
        })
        .collect()
}

/// Whether `docs/superpowers/ledgers/<slug>.md` exists and holds more than
/// whitespace. Resolved **by name from the slug** — never by listing the
/// ledgers directory — so `docs/superpowers/ledgers/README.md` is invisible
/// to this check by construction rather than by an exclusion rule someone
/// has to maintain (`docs/superpowers/ledgers/README.md` states the same
/// resolution rule from the other side).
fn ledger_exists_and_is_nonempty(slug: &str) -> bool {
    let path = repo_root()
        .join("docs/superpowers/ledgers")
        .join(format!("{slug}.md"));
    fs::read_to_string(&path).is_ok_and(|s| !s.trim().is_empty())
}

/// Campaign slugs exempted from `every_campaign_with_a_spec_and_a_plan_has_a_ledger`
/// because they predate the ledger convention (The Cartulary, 2026-08-30).
/// **Append-never in the shrinking direction only**: an entry is removed
/// once its campaign gains a ledger, never added — a new campaign cannot
/// exempt itself, the same ratchet `registry_length_waivers` enforces for
/// the registry Idea-column length budget.
fn ledger_exempt_campaigns() -> BTreeSet<&'static str> {
    include_str!("../fixtures/ledger-exempt-campaigns.txt")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect()
}

/// A campaign with a spec and a plan also has a ledger.
///
/// # Direction this check enforces
///
/// spec-and-plan implies ledger. It is blind to a ledger with no campaign
/// (harmless), and blind to every campaign in
/// `cli/tests/fixtures/ledger-exempt-campaigns.txt` — the 239 that predate
/// this convention. That list may only SHRINK: a campaign gaining a ledger
/// drops out of it, and a new campaign cannot add itself.
///
/// # What it cannot see, stated because a check that does not say so reads
/// as total
///
/// It sees that a ledger file exists and is non-empty. It cannot see whether
/// the contents are honest, whether they are complete, or whether they were
/// written as the campaign ran rather than backfilled in one sitting at
/// close. Those are the properties that actually matter and none of them is
/// mechanically checkable — the same three-valued honesty `tropes check` and
/// type-audit's `waiver(...)` carry.
#[test]
fn every_campaign_with_a_spec_and_a_plan_has_a_ledger() {
    let exempt = ledger_exempt_campaigns();
    let missing: Vec<String> = campaigns_with_spec_and_plan()
        .into_iter()
        .filter(|slug| !exempt.contains(slug.as_str()))
        .filter(|slug| !ledger_exists_and_is_nonempty(slug))
        .collect();
    assert!(
        missing.is_empty(),
        "campaigns with a spec and a plan but no ledger at \
         docs/superpowers/ledgers/. A campaign's rulings, deferred minors and \
         parked findings belong in a committed file — scratch dies with the \
         worktree, which has cost this project five recorded losses:\n  {}",
        missing.join("\n  ")
    );
}

/// The equivalent of `the_waiver_list_only_shrinks`, for the ledger
/// exemption list. Both halves apply here exactly as they do there: an
/// exempted slug that never had both a spec and a plan is a typo or a
/// fabrication (append-never means nothing should ever need to be added,
/// so a slug outside today's population could not have arrived
/// legitimately), and an exempted slug whose campaign now carries a
/// non-empty ledger should have been removed rather than left to ride
/// along unused.
#[test]
fn the_ledger_exemption_list_only_shrinks() {
    let exempt = ledger_exempt_campaigns();
    let population = campaigns_with_spec_and_plan();
    let population_refs: BTreeSet<&str> = population.iter().map(String::as_str).collect();

    let unknown: Vec<&str> = exempt
        .iter()
        .filter(|slug| !population_refs.contains(*slug))
        .copied()
        .collect();
    assert!(
        unknown.is_empty(),
        "exempted slugs with no matching spec-and-plan pair today — the \
         exemption list is append-never and its population is fixed, so a \
         slug outside that population means a typo, a renamed campaign, or a \
         fabricated entry:\n  {}",
        unknown.join("\n  ")
    );

    let now_ledgered: Vec<&str> = exempt
        .iter()
        .filter(|slug| ledger_exists_and_is_nonempty(slug))
        .copied()
        .collect();
    assert!(
        now_ledgered.is_empty(),
        "these campaigns now have a ledger — remove them from \
         fixtures/ledger-exempt-campaigns.txt so the ratchet holds:\n  {}",
        now_ledgered.join("\n  ")
    );
}

//! The walk-band depth is stated **once** in this repository, and this file is
//! what keeps that true.
//!
//! `hornvale_locale::walk_depth` is the definition. `hornvale_vessel::
//! walk_depth` is a `pub use` of it, not a second definition. Every other site
//! — both production `--depth` defaults in `cli/src/main.rs`, the scene
//! goldens' observer, `baseline_band`/`real_band`, the illumination probe, the
//! locale conservation and surface-mixture scans — **calls** the function.
//!
//! ## Why this file is a completeness scan and not a two-copy diff
//!
//! Its first version compared exactly two copies — `windows/vessel/src/agent.
//! rs` and `windows/locale/tests/suite/water_reading.rs` — and its own failure
//! message claimed that a second copy "belongs in this test's roster, not
//! outside it". **The roster was 2 of 16.** The other fourteen restated
//! `globe_level() + 6` and were invisible to the very check written to catch
//! them, including `hornvale locale`'s and `hornvale surrounds`' production
//! `--depth` defaults, which had fallen a whole band behind. Nothing went red,
//! because the stale sites were consistent with **each other** — and the scene
//! goldens would have been rebaselined at the wrong band by the epoch task and
//! then drift-checked green forever.
//!
//! A roster that reads as total and is not is worse than no roster, so the
//! rule is inverted: instead of listing the copies to compare, this scans the
//! whole tree and **fails when a `globe_level()` offset appears anywhere the
//! roster does not account for**. Sixteen copies existed because restating was
//! easy; the answer is fewer copies, not sixteen corrected ones, and this is
//! the check that keeps a seventeenth from accreting.
//!
//! Decision 0456's Consequences ask a check to state what it is blind to.
//! This one is blind five ways, all deliberate and all disclosed:
//!
//! 1. **It cannot tell whether a site that CALLS `walk_depth` wanted the walk
//!    band.** It proves only that nobody restated the arithmetic. A site that
//!    genuinely wants some other depth and calls this function anyway is a
//!    reader's error to catch.
//! 2. **An absolute depth literal is invisible to the offset scan.** A bare
//!    `13` needs no `globe_level()` to be wrong.
//!    [`absolute_walk_depth_constants_track_the_walk_band`] closes this for
//!    the NAMED constants its roster lists, and
//!    [`every_walk_constant_tracks_the_walk_band`] closes it unconditionally
//!    for constants named exactly `WALK`; a fresh inline literal, and a
//!    differently-named constant nobody rostered, are still not covered.
//!
//!    **The Pavement, Task 11, is why the second of those exists.** The
//!    absolute roster is OPT-IN, and nothing forces a new depth constant into
//!    it — so when the walk band moved to `globe_level + 7`, **eleven**
//!    `const WALK: u32 = 12;` fixtures across `windows/vessel` went stale at
//!    once and every one of them was invisible to both arms: they carry no
//!    `globe_level()` offset for the first arm to see, and nobody had put them
//!    in the roster the second arm reads. That is precisely the blindness this
//!    list disclosed, arriving in practice, and at a count nobody had guessed
//!    (the campaign brief that found it named ONE file).
//!
//!    A wider name-based arm — every `const *_DEPTH` / `*_RUNG` — was
//!    considered and NOT built, because in this repository those names mostly
//!    mean something else: `CLAUSE_EMBED_MAX_DEPTH`, `MAX_PARTITION_DEPTH`,
//!    `MEMORY_DEPTH`, `CASCADE_DEPTH_CAP`, `MAIN_LINE_DEPTH_CEILING`,
//!    `WALK_BYTES_BUDGET`, `WALK_STEPS`, `WALK_LEN`. Eight false positives
//!    against ten true ones is a roster of exceptions, not a check. `WALK`
//!    alone has no such collision — every one of its eleven definitions means
//!    the walk band — so that is the arm that got built, and the wider one
//!    stays disclosed here rather than implied.
//! 3. **Prose in a rostered file may drift up to its recorded count.** The
//!    count is a ratchet, not an exact pin: lowering the number in
//!    [`ROSTER`] is always allowed and is the direction of travel.
//! 4. **It reads `.rs` only.** A book chapter, a spec or a decision record may
//!    say `globe_level() + 6` freely, and should — those are historical
//!    records of what was true when written.
//! 5. **It skips its own source**, mirroring `cli/tests/suite/lexicon_guard.
//!    rs`: a file that must write the pattern in its own prose and failure
//!    messages cannot also be scanned by itself.
//!
//! Precedent for a `cli`-crate test reading files elsewhere in the repo as
//! plain text, and for a per-file ratcheted roster:
//! `cli/tests/suite/lexicon_guard.rs`, `cli/tests/suite/docs_consistency.rs`,
//! `cli/tests/suite/client_band_coverage.rs`.

use std::fs;
use std::path::{Path, PathBuf};

/// The one file allowed to state the arithmetic, in code.
const THE_DEFINITION: &str = "windows/locale/src/lib.rs";

/// What a rostered file is allowed to do with a `globe_level()` offset.
enum Role {
    /// The single definition: exactly one CODE occurrence, and it must agree
    /// with what the function returns at runtime.
    Definition,
    /// A site that deliberately uses an offset OTHER than the walk band, with
    /// the reason it is not the walk band. **This list is empty**, and that is
    /// a finding rather than an oversight — see
    /// [`the_independent_list_is_empty_on_purpose`].
    #[allow(dead_code)]
    Independent {
        /// Why this site is not asking for the walk band.
        reason: &'static str,
    },
    /// Comment prose that MENTIONS an offset — history, or an explanation of
    /// what a line used to say. Occurrences must all be on comment lines, and
    /// there may be no more than `max` of them.
    Prose {
        /// The recorded count. Lowering it is always allowed.
        max: usize,
        /// What the prose is for.
        reason: &'static str,
    },
}

/// Every file allowed to contain a `globe_level()` offset at all. A file
/// absent from this list may contain none.
fn roster() -> Vec<(&'static str, Role)> {
    vec![
        (THE_DEFINITION, Role::Definition),
        (
            "windows/vessel/src/agent.rs",
            Role::Prose {
                max: 1,
                reason: "the re-export's doc explains that sixteen sites restated \
                         `globe_level() + 6` rather than calling the function, which is \
                         why the definition moved out of this crate",
            },
        ),
        (
            "windows/locale/tests/suite/water_reading.rs",
            Role::Prose {
                max: 2,
                reason: "the local alias's doc records that it used to be a genuine second \
                         definition, and that `hornvale locale`'s own `--depth` default was a \
                         separate hardcoded copy that had fallen a band behind",
            },
        ),
        (
            "windows/locale/tests/suite/surface_mixture.rs",
            Role::Prose {
                max: 1,
                reason: "the comment records that its own parenthetical used to restate the \
                         offset, which is how this site fell a band behind",
            },
        ),
    ]
}

/// Named constants that state a walk depth ABSOLUTELY, which the offset scan
/// cannot see (blindness 2 above). Each must equal the live walk depth at the
/// canonical globe level — or declare itself stale, with an owner.
enum Absolute {
    /// Must equal `walk_depth` at the canonical globe level.
    Tracks,
    /// Known stale, pinned at its CURRENT wrong value with the task that owns
    /// the fix. Shaped like `tools/seam-guard`'s `expect(survives: …)` on
    /// purpose: a one-directional acknowledgement can only ever be satisfied,
    /// so it rots. This one fails the moment the value changes AT ALL —
    /// whether somebody fixes it (delete the declaration) or it drifts further.
    ///
    /// **The list is empty**, for the same reason [`Role::Independent`]'s is:
    /// the one declaration this variant ever carried (the game client's
    /// `BAND_B_RUNG`) was fixed by The Pavement's Task 8 and its row deleted,
    /// which is the declaration working as designed. The variant stays so the
    /// next campaign that must ship a knowingly-stale constant can declare it
    /// instead of leaving it unwatched.
    #[allow(dead_code)]
    StaleAt {
        /// The wrong value it currently holds.
        value: u32,
        /// Why it has not been fixed here, and who owns it.
        reason: &'static str,
    },
}

fn absolute_roster() -> Vec<(&'static str, &'static str, Absolute)> {
    vec![
        (
            "domains/terrain/tests/suite/rill_probe.rs",
            "WALK_DEPTH",
            Absolute::Tracks,
        ),
        (
            "windows/locale/tests/suite/surface_mixture.rs",
            "DEPTH",
            Absolute::Tracks,
        ),
        // FIXED, and the `StaleAt` declaration deleted with it (The Pavement,
        // Task 8): the browser client's finest zoom rung now equals the live
        // walk depth. Its ~80 readers all read the constant, so the move was
        // the one line plus its doc — and the silence that let it drift a
        // whole band with a green client gate is closed on the client's own
        // side too, by `clients/game/bin/tests/walk_band_agreement.rs`, which
        // asks `walk_depth` itself rather than restating the arithmetic. This
        // row stays as `Tracks` even so: that test runs only under `make
        // game-check`, which no workspace gate can see.
        (
            "clients/game/bin/src/plate.rs",
            "BAND_B_RUNG",
            Absolute::Tracks,
        ),
    ]
}

/// The repository root: `cli/tests/` lives in the `cli` crate, whose manifest
/// dir is `<root>/cli`, so the root is its parent.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli crate should sit under the repo root")
        .to_path_buf()
}

/// Every crate tree this scan covers — discovered rather than listed, so a new
/// crate is covered the day it is added. Mirrors `lexicon_guard.rs`'s
/// `covered_dirs`, `clients/` and `tools/` included: the offset can be wrong
/// outside the workspace too.
fn covered_dirs(root: &Path) -> Vec<PathBuf> {
    let mut dirs = vec![root.join("kernel"), root.join("cli")];
    for layer in ["domains", "windows", "clients", "tools"] {
        let Ok(entries) = fs::read_dir(root.join(layer)) else {
            continue;
        };
        let mut crates: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
        crates.sort();
        for c in crates {
            if c.is_dir() {
                dirs.push(c);
            }
        }
    }
    dirs
}

fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
    paths.sort();
    for path in paths {
        if path
            .file_name()
            .is_some_and(|n| n == "target" || n == ".git")
        {
            continue;
        }
        if path.is_dir() {
            rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// One `globe_level() <op> <n>` occurrence.
struct Hit {
    path: String,
    line_no: usize,
    line: String,
    is_comment: bool,
    offset: i64,
}

/// Find `globe_level()` followed by `+`/`-` and an integer, std-only (no regex
/// crate: the dependency allowlist is `ALLOWED_EXTERNAL`).
fn offset_in(line: &str) -> Option<i64> {
    let idx = line.find("globe_level()")?;
    let rest = line[idx + "globe_level()".len()..].trim_start();
    let (sign, rest) = match rest.strip_prefix('+') {
        Some(r) => (1i64, r),
        None => (-1i64, rest.strip_prefix('-')?),
    };
    let digits: String = rest
        .trim_start()
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .collect();
    if digits.is_empty() {
        return None;
    }
    digits.parse::<i64>().ok().map(|n| sign * n)
}

fn all_hits() -> Vec<Hit> {
    let root = repo_root();
    let mut hits = Vec::new();
    for dir in covered_dirs(&root) {
        let mut files = Vec::new();
        rs_files(&dir, &mut files);
        for file in files {
            let rel = file
                .strip_prefix(&root)
                .unwrap_or(&file)
                .to_string_lossy()
                .replace('\\', "/");
            // Blindness 5: this file writes the pattern in its own prose and
            // failure messages, so it cannot scan itself.
            if rel.ends_with("walk_depth_agreement.rs") {
                continue;
            }
            let Ok(text) = fs::read_to_string(&file) else {
                continue;
            };
            for (i, line) in text.lines().enumerate() {
                if let Some(offset) = offset_in(line) {
                    hits.push(Hit {
                        path: rel.clone(),
                        line_no: i + 1,
                        line: line.trim().to_string(),
                        is_comment: line.trim_start().starts_with("//"),
                        offset,
                    });
                }
            }
        }
    }
    hits
}

/// A real `LocaleContext` from the committed seed-42 fixture. `walk_depth`
/// reads only `globe_level()` and has no settlement dependency; the fixture
/// supplies the built world's mandatory sky facts without paying for genesis.
fn context() -> hornvale_locale::LocaleContext {
    let world = hornvale_worldgen::fixture::seed_42_world();
    hornvale_locale::LocaleContext::build(&world).expect("seed 42 builds a context")
}

#[test]
fn the_offset_is_stated_exactly_once_in_the_repository() {
    let ros = roster();
    let hits = all_hits();
    let mut problems: Vec<String> = Vec::new();

    for h in &hits {
        let Some((_, role)) = ros.iter().find(|(p, _)| *p == h.path) else {
            problems.push(format!(
                "{}:{}: a `globe_level()` offset outside the roster — {}\n    \
                 If this site wants THE WALK BAND, call `hornvale_locale::walk_depth(ctx)` \
                 instead of restating the arithmetic. If it deliberately wants a different \
                 depth, add it to this file's roster as `Role::Independent` WITH the reason.",
                h.path, h.line_no, h.line
            ));
            continue;
        };
        match role {
            Role::Definition => {
                if h.is_comment {
                    problems.push(format!(
                        "{}:{}: the definition file's offset must be CODE, not prose — {}",
                        h.path, h.line_no, h.line
                    ));
                }
            }
            Role::Independent { reason } => {
                if reason.trim().is_empty() {
                    problems.push(format!("{}: an Independent site needs a reason", h.path));
                }
            }
            Role::Prose { reason, .. } => {
                if reason.trim().is_empty() {
                    problems.push(format!("{}: a Prose entry needs a reason", h.path));
                }
                if !h.is_comment {
                    problems.push(format!(
                        "{}:{}: this file is rostered for PROSE only, but this is code — {}\n    \
                         Call `hornvale_locale::walk_depth(ctx)`.",
                        h.path, h.line_no, h.line
                    ));
                }
            }
        }
    }

    // The definition holds exactly one code occurrence.
    let def_code = hits
        .iter()
        .filter(|h| h.path == THE_DEFINITION && !h.is_comment)
        .count();
    if def_code != 1 {
        problems.push(format!(
            "{THE_DEFINITION} must state the offset exactly once in code; found {def_code}"
        ));
    }

    // Prose ratchet, per rostered file.
    for (path, role) in &ros {
        if let Role::Prose { max, .. } = role {
            let n = hits.iter().filter(|h| &h.path == path).count();
            if n > *max {
                problems.push(format!(
                    "{path}: rostered for at most {max} prose mention(s), found {n}. \
                     Adding the words is how sixteen copies happened; if the new mention is \
                     genuinely history, raise the number here deliberately."
                ));
            }
        }
    }

    assert!(
        problems.is_empty(),
        "the walk-band offset is restated outside its one definition:\n  {}",
        problems.join("\n  ")
    );
}

#[test]
fn the_independent_list_is_empty_on_purpose() {
    // The Pavement read all sixteen restatements one at a time and asked of
    // each: is this asking for THE WALK BAND, or for depth 12 specifically as
    // an independent choice? Every one of the sixteen was the walk band —
    // production `--depth` defaults, the gallery observer at the flagship
    // settlement's own room ("the same place a possession mints its agent"),
    // `baseline_band`/`real_band` at `WALK_BAND_RADIUS`, the illumination
    // probe's bands, and two locale scans whose own comments said "the walking
    // depth this crate uses everywhere else". None wanted an independent
    // depth. So the list is empty, and this test says so out loud rather than
    // leaving an empty `vec![]` that reads as "nobody looked yet".
    let n = roster()
        .iter()
        .filter(|(_, r)| matches!(r, Role::Independent { .. }))
        .count();
    assert_eq!(
        n, 0,
        "an Independent site has been added — good, but check its reason says why it is \
         NOT the walk band, and update this test's comment with the triage"
    );
}

#[test]
fn the_definition_matches_what_the_function_returns() {
    let ctx = context();
    let hits = all_hits();
    let def = hits
        .iter()
        .find(|h| h.path == THE_DEFINITION && !h.is_comment)
        .expect("the definition states an offset in code");
    let scraped = u32::try_from(def.offset).expect("the walk-band offset is positive");
    assert_eq!(
        hornvale_locale::walk_depth(&ctx),
        ctx.globe_level() + scraped,
        "the running `walk_depth` disagrees with the line scraped from {THE_DEFINITION}:{} — \
         the expression has changed under the text this file compares, which is exactly the \
         gap a text-only check cannot see",
        def.line_no
    );
}

#[test]
fn the_definitions_prose_agrees_with_its_code() {
    const WORDS: [(&str, i64); 10] = [
        ("zero", 0),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ];
    let text = fs::read_to_string(repo_root().join(THE_DEFINITION)).expect("source is utf8");
    // Anchored on "the canonical grid" deliberately: `water_reading.rs` also
    // says "three levels below WALK DEPTH", about an offset FROM this band
    // rather than about the band itself, and an unanchored pattern picked
    // those up (measured: it found [7, 3, 3] instead of [7]).
    let mut found: Vec<i64> = Vec::new();
    for line in text.lines() {
        let t = line.trim();
        if !t.starts_with("///") {
            continue;
        }
        for (word, n) in WORDS {
            if t.contains(&format!("{word} levels below the canonical grid"))
                || t.contains(&format!(
                    "{word} refinement levels below the canonical grid"
                ))
            {
                found.push(n);
            }
        }
    }
    assert_eq!(
        found.len(),
        1,
        "{THE_DEFINITION} must spell the offset in words exactly once \
         (\"<word> [refinement] levels below the canonical grid\"); found {found:?}"
    );
    let def_offset = all_hits()
        .into_iter()
        .find(|h| h.path == THE_DEFINITION && !h.is_comment)
        .expect("the definition states an offset in code")
        .offset;
    assert_eq!(
        found[0], def_offset,
        "{THE_DEFINITION}: the doc comment says {} levels below the grid and the code says \
         {def_offset}. The prose is the half a reader believes.",
        found[0]
    );
}

#[test]
fn the_vessel_reexport_is_the_same_rule_not_a_second_definition() {
    let ctx = context();
    // Same value...
    assert_eq!(
        hornvale_vessel::walk_depth(&ctx),
        hornvale_locale::walk_depth(&ctx),
        "`hornvale_vessel::walk_depth` has diverged from `hornvale_locale::walk_depth`"
    );
    // ...and, more to the point, `agent.rs` states no offset in code at all,
    // so it CANNOT diverge. A `pub use` is what makes the equality above
    // trivially true; this is the assertion that keeps it a `pub use`.
    let agent = "windows/vessel/src/agent.rs";
    let code = all_hits()
        .into_iter()
        .filter(|h| h.path == agent && !h.is_comment)
        .collect::<Vec<_>>();
    assert!(
        code.is_empty(),
        "{agent} has grown a walk-depth expression of its own; it must stay a `pub use` of \
         `hornvale_locale::walk_depth` — a definition here is unreachable from \
         `windows/locale` and `windows/scene`, which is what produced sixteen restatements. \
         Found: {:?}",
        code.iter()
            .map(|h| format!("{}:{}", h.path, h.line_no))
            .collect::<Vec<_>>()
    );
}

#[test]
fn absolute_walk_depth_constants_track_the_walk_band() {
    let ctx = context();
    let live = hornvale_locale::walk_depth(&ctx);
    let root = repo_root();
    let mut problems: Vec<String> = Vec::new();

    for (path, name, role) in absolute_roster() {
        let text = match fs::read_to_string(root.join(path)) {
            Ok(t) => t,
            Err(e) => {
                problems.push(format!(
                    "{path}: unreadable ({e}) — fix or drop the roster row"
                ));
                continue;
            }
        };
        // `const NAME: u32 = N;` or `const NAME: usize = N;` — the two shapes
        // the roster's entries use. A depth is a count either way.
        let mut values: Vec<u32> = Vec::new();
        for ty in ["u32", "usize"] {
            let needle = format!("const {name}: {ty} = ");
            for line in text.lines() {
                if let Some(i) = line.find(&needle) {
                    let rest = &line[i + needle.len()..];
                    let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                    if let Ok(v) = digits.parse::<u32>() {
                        values.push(v);
                    }
                }
            }
        }
        if values.len() != 1 {
            problems.push(format!(
                "{path}: expected exactly one `const {name}: u32|usize = N;`, found {values:?}"
            ));
            continue;
        }
        let value = values[0];
        match role {
            Absolute::Tracks => {
                if value != live {
                    problems.push(format!(
                        "{path}: {name} is {value} but the walk band at globe level {} is \
                         {live}. This constant restates the walk depth absolutely, so it must \
                         move with `hornvale_locale::walk_depth`.",
                        ctx.globe_level()
                    ));
                }
            }
            Absolute::StaleAt {
                value: declared,
                reason,
            } => {
                if reason.trim().is_empty() {
                    problems.push(format!("{path}: a StaleAt declaration needs a reason"));
                }
                if value != declared {
                    problems.push(format!(
                        "{path}: {name} is {value}; this roster declares it STALE AT \
                         {declared}. If it was fixed to {live}, delete the `StaleAt` row (the \
                         declaration has served its purpose). If it moved to something else, \
                         say what and why."
                    ));
                }
            }
        }
    }

    assert!(
        problems.is_empty(),
        "an absolute walk-depth constant disagrees with the walk band:\n  {}",
        problems.join("\n  ")
    );
}

/// Prose anywhere in the tree that states the band's depth in words, or as a
/// power of four. Invisible to
/// [`the_offset_is_stated_exactly_once_in_the_repository`], which only sees
/// `globe_level()` expressions.
///
/// This closes a real gap, not a hypothetical one. The Pavement's fix round
/// found four stale claims in the half of the record a reader actually
/// believes, none of which any offset scan could see:
/// `windows/scene/src/surrounds.rs` telling a reader that "a chart at walk
/// depth sits six refinement levels below the canonical grid ... 4^6 rooms
/// share one grid vertex"; `windows/locale/src/lib.rs` saying the same in two
/// places
/// ("`4^6 = 4096` rooms in that vertex", "all 4^6 rooms inside it"); and
/// `windows/locale/tests/suite/surface_mixture.rs` citing "the same 'six
/// refinement levels below the canonical grid' convention" to justify its own
/// hardcoded `DEPTH`.
///
/// Deliberately a VALUE check, not a location check: prose may say this
/// anywhere and as often as it likes, as long as it says the true number.
/// **A sentence recording what the band USED to be must therefore not use
/// these exact phrasings** — write "until The Pavement a chart sat one level
/// coarser" or state the change without the phrase. That is a real constraint
/// on how history is written here, and it is the price of the check being a
/// value check; the alternative is a per-file roster, which is what
/// [`ROSTER`]-style location lists already showed can silently go stale.
#[test]
fn no_prose_states_a_stale_band_depth() {
    const WORDS: [(&str, u32); 10] = [
        ("zero", 0),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ];
    let ctx = context();
    let live = hornvale_locale::walk_depth(&ctx) - ctx.globe_level();
    let root = repo_root();
    let mut problems: Vec<String> = Vec::new();

    for dir in covered_dirs(&root) {
        let mut files = Vec::new();
        rs_files(&dir, &mut files);
        for file in files {
            let rel = file
                .strip_prefix(&root)
                .unwrap_or(&file)
                .to_string_lossy()
                .replace('\\', "/");
            // Blindness 5: this file writes the phrases in its own prose.
            if rel.ends_with("walk_depth_agreement.rs") {
                continue;
            }
            let Ok(text) = fs::read_to_string(&file) else {
                continue;
            };
            for (i, line) in text.lines().enumerate() {
                for (word, n) in WORDS {
                    let hit = line.contains(&format!("{word} levels below the canonical grid"))
                        || line.contains(&format!(
                            "{word} refinement levels below the canonical grid"
                        ));
                    if hit && n != live {
                        problems.push(format!(
                            "{rel}:{}: prose says {n} levels below the canonical grid; the \
                             walk band is {live}. — {}",
                            i + 1,
                            line.trim()
                        ));
                    }
                }
                // `4^N rooms` / `4^N = M rooms`, the other way this claim gets
                // written. Two narrowings, both from measured false positives:
                // the line must also say "rooms", because
                // `domains/terrain/src/lib.rs` writes the icosphere's VERTEX
                // count as `10 × 4^6 + 2` and that 6 is a subdivision level
                // with nothing to do with the walk band; and a parametric
                // `4^depth_below_grid` carries no literal, so it is not matched
                // at all.
                for n in 0..10u32 {
                    if line.contains(&format!("4^{n}")) && line.contains("rooms") && n != live {
                        problems.push(format!(
                            "{rel}:{}: prose says 4^{n} rooms share one grid vertex; the walk \
                             band is {live} levels below it, so it is 4^{live}. — {}",
                            i + 1,
                            line.trim()
                        ));
                    }
                }
            }
        }
    }

    assert!(
        problems.is_empty(),
        "prose states a stale walk-band depth — the half of the record a reader believes:\n  {}",
        problems.join("\n  ")
    );
}

/// **Every constant named exactly `WALK` equals the live walk depth.**
///
/// Unconditional and name-based: no roster, nothing to opt into, and nothing
/// to forget. This is the arm blind spot 2 above was missing — the absolute
/// roster covers only what someone remembered to list, which is how eleven
/// `const WALK: u32 = 12;` fixtures went stale together at The Pavement while
/// both existing arms stayed green.
///
/// It is deliberately narrow. `WALK` is the one depth name in this repository
/// with no other meaning; the module doc above records the eight `*_DEPTH` /
/// `*_RUNG` constants that DO mean other things and why widening the pattern
/// to reach them would produce a roster of exceptions rather than a check.
///
/// Skips its own source for the same reason the offset scan does: this file
/// must write the pattern in its own prose to explain it.
#[test]
fn every_walk_constant_tracks_the_walk_band() {
    let ctx = context();
    let live = hornvale_locale::walk_depth(&ctx);
    let root = repo_root();
    let mut files = Vec::new();
    for dir in covered_dirs(&root) {
        rs_files(&dir, &mut files);
    }
    files.sort();

    let mut problems: Vec<String> = Vec::new();
    let mut found = 0usize;
    for file in &files {
        let rel = file
            .strip_prefix(&root)
            .unwrap_or(file)
            .to_string_lossy()
            .replace('\\', "/");
        if rel.ends_with("walk_depth_agreement.rs") {
            continue;
        }
        let Ok(text) = fs::read_to_string(file) else {
            continue;
        };
        for (n, line) in text.lines().enumerate() {
            let Some(i) = line.find("const WALK: ") else {
                continue;
            };
            let rest = &line[i + "const WALK: ".len()..];
            let Some(eq) = rest.find("= ") else { continue };
            let digits: String = rest[eq + 2..]
                .chars()
                .take_while(|c| c.is_ascii_digit())
                .collect();
            let Ok(value) = digits.parse::<u32>() else {
                continue;
            };
            found += 1;
            if value != live {
                problems.push(format!("{rel}:{}: WALK = {value}, live = {live}", n + 1));
            }
        }
    }

    assert!(
        found > 0,
        "no `const WALK` was found anywhere in the tree — this check has stopped \
         checking anything, which is worse than a red one"
    );
    assert!(
        problems.is_empty(),
        "a constant named WALK does not equal the live walk depth ({live}). It is \
         the walk band by name and there is no other meaning of `WALK` in this \
         repository, so this is a stale fixture rather than an independent \
         depth — set it, or rename the constant to say what else it means:\n  {}",
        problems.join("\n  ")
    );
}

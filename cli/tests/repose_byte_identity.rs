//! **C0 is a pure read.** The Repose adds a hazard field, a derived volcano
//! identity, a drawn event stream and a knownness stock, and none of them may
//! move a byte of any world that already exists: nothing here draws from an
//! existing stream, and nothing here commits a fact. That is a claim about the
//! whole campaign, and this file is what makes it checkable.
//!
//! **Direction of the check.** It catches this campaign's code *reaching a
//! shipped output path* — a new draw taken from an existing stream, a name
//! minted on a path some other name already walks, a rendering that started
//! mentioning a mountain. It cannot catch a change that moves an output none of
//! the probes below covers; the probes are three shipped surfaces, not a proof
//! of absence.
//!
//! **Why this file lives in `cli/`, not `windows/worldgen/`.** The plan sited
//! it at `windows/worldgen/tests/repose_byte_identity.rs`, and a test there can
//! reach exactly one of the three surfaces: `hornvale-worldgen` depends on
//! `hornvale-almanac` but not on `hornvale-scene`, and `hornvale-scene` depends
//! on `hornvale-worldgen`, so the scene probe would need a dev-dependency
//! cycle. `cli/` is the crate that depends on everything, which is why every
//! other workspace-wide invariant is already asserted from here
//! (`cli/CLAUDE.md`). The three surfaces belong to one claim and are therefore
//! in one file.
//!
//! **The world-JSON probe deliberately duplicates `lens_purity.rs`.** That test
//! guards the same fixture in the always-running tier, which is strictly
//! stronger than anything here. It is repeated below because a reader auditing
//! "did The Repose move a byte?" should find all three surfaces in one place,
//! and because its marginal cost is a string comparison against a world the
//! scene probe has already built.
//!
//! **These are NOT heavy-tier, and measurement is why.** The plan tagged all
//! three with the canonical `heavy:` ignore reason on the assumption that a
//! live-worldgen battery costs minutes. Measured on `MacBookPro` at this
//! campaign's HEAD, under the workspace-wide optimized dev profile (decision
//! 0113): **1.00 s** (almanac), **2.05 s** (scene), **1.66 s** (world JSON) —
//! 4.71 s serial, 2.14 s in parallel, for three seed-42 builds. Deferring
//! 2 s of work into a tier `make gate` never runs would have bought nothing
//! and cost the guarantee: `cli/CLAUDE.md` records that the heavy tier is
//! invisible on `main` too, and a byte-identity guard nobody runs is the
//! weakest form of one. This is the campaign's third measured plan amendment,
//! after Task 6's ("the recovery batteries are not heavy, and measurement says
//! so").
//!
//! **Each probe below has been observed RED** (Task 8's positive controls; an
//! empty diff needs one). What that exercise also established is that the
//! three are *not* interchangeable, and the almanac is the odd one out:
//!
//! | mutation | world JSON | scene | almanac |
//! |---|---|---|---|
//! | `ARC_EDIFICE_DECAY_CELLS` 1.5 → 1.6 | RED | RED | green |
//! | settlement name draw 2–3 → 2–4 syllables | RED | green | green |
//! | a sentence prepended to the almanac render | green | green | RED |
//!
//! The almanac is a **summary** document — aggregate statistics and a
//! fifteen-row roster of chief settlements — so it is nearly blind to a
//! per-cell terrain move and to a name change that lands late in the ledger,
//! and it is the *only* one of the three that sees a rendering change. Keep
//! all three: they cover different halves, and no single one of them is the
//! byte-identity claim.
//!
//! Test fixture (decision 0092): builds worlds through the public entry points
//! exactly as `cli/src/main.rs` does.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, almanac_context, build_world};

/// The repository root: the parent of this crate's manifest dir (`cli/`).
/// Filesystem-based, not git-based — the heavy tier runs the suite in an
/// rsync'd tree that is not a git repository (`heavy_tier.rs` does the same).
fn repo_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Seed 42 under the tier-0 constant sun — the world
/// `hornvale new --seed 42 --sky constant` writes, which is the world
/// `book/src/gallery/almanac-seed-42.md` is rendered from.
///
/// The CLI additionally calls `streams::stamp`, which only sets
/// `World::derived_under` (a metadata map, never a fact), so it cannot reach
/// any rendering. Omitted here because `cli` has no library target and the
/// stamp is not reachable from an integration test.
fn constant_sun_world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Constant,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds under the constant sun")
}

/// Seed 42 under the generated sky — the default world, and the one both
/// `cli/tests/fixtures/world-seed-42.json` and
/// `book/src/gallery/scene-tiles-seed-42.json` are taken from.
fn generated_sky_world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds under the generated sky")
}

/// Compare a rendered artifact against its committed bytes, naming the first
/// divergence.
///
/// Deliberately **not** `hornvale_kernel::golden::assert_golden`. Two of these
/// fixtures are owned by `scripts/regenerate-artifacts.sh`, so giving them a
/// second writer through `REBASELINE=1` would let a divergence between the
/// in-process render and the CLI's be accepted silently — which is the one
/// thing this file exists to notice.
fn assert_committed_bytes(relative: &str, actual: &str) {
    let path = repo_root().join(relative);
    let expected = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("committed artifact {relative} is readable: {e}"));
    if expected == actual {
        return;
    }
    let at = expected
        .char_indices()
        .zip(actual.char_indices())
        .find(|((_, a), (_, b))| a != b)
        .map(|((i, _), _)| i)
        .unwrap_or_else(|| expected.len().min(actual.len()));
    let window = |s: &str| {
        let lo = at.saturating_sub(80);
        let hi = (at + 80).min(s.len());
        s.get(lo..hi).unwrap_or(s).to_string()
    };
    panic!(
        "THE REPOSE MOVED A SHIPPED ARTIFACT: {relative}\n\
         C0 is a pure read (spec §5.1) — it draws from no existing stream and \
         commits no fact — so this must be byte-identical. A red here is a leak, \
         not a rebaseline: find the geohazard code that reached this rendering.\n\
         first divergence at byte {at} (expected {} bytes, got {})\n\
         expected: …{}…\n  actual: …{}…",
        expected.len(),
        actual.len(),
        window(&expected),
        window(actual)
    );
}

/// The seed-42 almanac — the oldest rendered surface in the repo, and the one a
/// stray fact or a reseeded name would move first.
#[test]
fn seed_42_almanac_is_unmoved_by_the_repose() {
    let world = constant_sun_world();
    let ctx = almanac_context(&world).expect("seed 42 renders an almanac");
    assert_committed_bytes(
        "book/src/gallery/almanac-seed-42.md",
        &hornvale_almanac::render(&ctx),
    );
}

/// The seed-42 scene JSON — a cross-repo contract (decision 0055), so a move
/// here is not merely a drift but a broken external client.
///
/// `hornvale scene tiles` prints with `println!`, so the committed file carries
/// the trailing newline the renderer does not.
#[test]
fn seed_42_scene_output_is_unmoved_by_the_repose() {
    let world = generated_sky_world();
    let scene = hornvale_scene::tiles_scene(&world, 256).expect("seed 42 renders scene/tiles/v1");
    assert_committed_bytes(
        "book/src/gallery/scene-tiles-seed-42.json",
        &format!("{}\n", hornvale_scene::scene_json(&scene)),
    );
}

/// The seed-42 world JSON — world identity itself. See this file's module doc
/// for why the duplication with `lens_purity.rs` is deliberate.
#[test]
fn seed_42_world_json_is_unmoved_by_the_repose() {
    assert_committed_bytes(
        "cli/tests/fixtures/world-seed-42.json",
        &generated_sky_world().to_json(),
    );
}

/// The vocabulary a geohazard would arrive under, if one ever reached a
/// rendered artifact — hazard-*consequence* words: an eruption event, the
/// knownness stock a character's exposure to one would build. This is what
/// "C0 commits nothing and renders nothing" means in bytes, for the halves of
/// The Repose no later campaign has surfaced.
///
/// **`"volcano"` moved out here (The Gazetteer, 2026-08-18), exactly the
/// remedy this test's own failure message names**: the Gazetteer campaign
/// ships volcanoes as a fifth individuated feature *class* with names
/// (`book/src/gallery/gazetteer-seed-42.md`), a toponymy surface, not a
/// hazard-consequence one — no eruption event, no knownness stock, no
/// walk-scale perception reaches any rendered artifact through it, only a
/// class label and a people's word for a place. `"eruption"` and
/// `"knownness"` remain absent at zero occurrences, so the check still has no
/// baseline to drift against for the half that IS still unshipped. Chosen
/// over `recurrence`, `landform` or `seismic`: the first two already occur in
/// shipped prose (`book/src/domesday/terrain.md`, the census schema), so a
/// guard on them would be satisfied by text neither campaign wrote.
const GEOHAZARD_VOCABULARY: [&str; 2] = ["eruption", "knownness"];

/// The rendered-world artifact trees, from `docs/generated-paths.txt`, minus
/// the two entries that are reports about the *source* rather than renderings
/// of a *world*: `docs/audits/` (the type-audit report names every new `pub`
/// boundary, and this campaign added several) and `docs/digest/` (the decision
/// index and delta report describe the repo). `book/src/reference/` is out for
/// the same reason — the stream manifest publishes the new labels by design.
const RENDERED_WORLD_TREES: [&str; 4] = [
    "book/src/gallery/",
    "book/src/laboratory/",
    "book/src/domesday/",
    "clients/game/core/tests/fixtures/",
];

/// Collect every file under `dir`, skipping dot-directories.
fn collect_files(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    for entry in std::fs::read_dir(dir).expect("directory is readable") {
        let entry = entry.expect("directory entry is readable");
        let path = entry.path();
        if entry.file_name().to_string_lossy().starts_with('.') {
            continue;
        }
        if path.is_dir() {
            collect_files(&path, out);
        } else {
            out.push(path);
        }
    }
}

/// No rendered world mentions a geohazard. This is the cheap, always-running
/// half of the byte-identity claim: the three probes above pin *specific*
/// artifacts exactly, and this one sweeps *every* rendered artifact for the
/// one thing C0 promises never to say.
#[test]
fn no_rendered_artifact_names_a_geohazard() {
    let root = repo_root();
    let mut files = Vec::new();
    for tree in RENDERED_WORLD_TREES {
        collect_files(&root.join(tree), &mut files);
    }
    files.sort();
    assert!(
        files.len() > 50,
        "expected the rendered-artifact trees to hold many files; found {} — \
         the path list has probably drifted from docs/generated-paths.txt",
        files.len()
    );

    let mut hits = Vec::new();
    for path in &files {
        // Binary artifacts (the PNG maps) are skipped: they cannot carry a word.
        let Ok(text) = std::fs::read_to_string(path) else {
            continue;
        };
        let lower = text.to_lowercase();
        for word in GEOHAZARD_VOCABULARY {
            if lower.contains(word) {
                hits.push(format!(
                    "{} names {word:?}",
                    path.strip_prefix(&root).unwrap_or(path).display()
                ));
            }
        }
    }
    assert!(
        hits.is_empty(),
        "a rendered world artifact names a geohazard — C0 renders nothing \
         (spec §2.2: no consequence facts, no walk-scale perception). If a later \
         campaign deliberately surfaces one, move the word out of \
         GEOHAZARD_VOCABULARY in the same commit and say so in the chronicle.\n{}",
        hits.join("\n")
    );
}

//! The failure black-box (TOOL-failure-black-box): when a swept-seed test
//! fails, capture the failing world and a repro command to disk instead of
//! leaving the developer with only a seed number in a panic message.

use crate::runner::resolve_roster;
use crate::study::Study;
use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use std::io;
use std::path::{Path, PathBuf};

/// Directory (relative to the workspace root) that recorded failures land
/// under. Not a new gitignore entry: the top-level `target` rule already
/// covers it.
const FAILURES_DIR: &str = "target/failures";

/// The workspace root, derived from this crate's compile-time manifest
/// directory (`windows/lab`, two levels below the root) rather than the
/// process's current directory — stable regardless of where the caller (a
/// test binary, whose CWD is the crate directory, not the workspace root)
/// happens to run from.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent() // windows/
        .and_then(Path::parent) // workspace root
        .expect("hornvale-lab lives two directories below the workspace root")
        .to_path_buf()
}

/// Find `pin_set_label`'s pin set in `study`, parsed exactly as
/// [`crate::runner::run`] parses it (`Study::pin_sets_parsed`) — never a
/// second copy of pin-string parsing. `Err` names the study and label when
/// no such pin set exists.
fn find_pin_set(study: &Study, pin_set_label: &str) -> io::Result<(SkyPins, Option<String>)> {
    let pin_sets = study.pin_sets_parsed().map_err(io::Error::other)?;
    pin_sets
        .into_iter()
        .find(|(label, _, _)| label == pin_set_label)
        .map(|(_, pins, roster)| (pins, roster))
        .ok_or_else(|| {
            io::Error::other(format!(
                "no pin set '{pin_set_label}' in study '{}'",
                study.name
            ))
        })
}

/// Render the equivalent `hornvale new` repro command for `seed` under
/// `pin_set_label`'s raw (unparsed) pin strings — "key=value" pins map 1:1
/// onto `--key value` CLI flags (`cli::parse_sky_args`'s convention, which
/// folds every sky flag through the same `key=value` pin-string syntax).
/// `hornvale new` always builds the shipped default roster, so a pin set
/// naming a different roster gets a trailing comment naming the gap rather
/// than a flag that doesn't exist.
fn repro_command(study: &Study, seed: u64, pin_set_label: &str) -> String {
    let pin_set = study.pin_sets.iter().find(|ps| ps.label == pin_set_label);
    let mut command = format!("cargo run -p hornvale -- new --seed {seed}");
    if let Some(pin_set) = pin_set {
        for pin in &pin_set.pins {
            if let Some((key, value)) = pin.split_once('=') {
                command.push_str(&format!(" --{key} {value}"));
            }
        }
    }
    command.push_str(" --out world.json\n");
    if let Some(roster) = pin_set.and_then(|ps| ps.roster.as_deref())
        && roster != "default"
    {
        command.push_str(&format!(
            "# NOTE: pin set '{pin_set_label}' builds roster '{roster}'; `hornvale new` \
             always builds the shipped default roster, so this command will not \
             reproduce that roster.\n"
        ));
    }
    command
}

/// Rebuild the world that a study's `(seed, pin_set_label)` produced —
/// reusing the runner's own pin-parsing (`Study::pin_sets_parsed`) and
/// roster resolution (`runner::resolve_roster`), never a second copy of
/// that logic — and write it to disk for postmortem debugging:
/// `target/failures/<study-name>-seed<seed>-<pin-set>.json` (quantized JSON,
/// [`hornvale_kernel::World`]'s normal save format — the ledger's quantizing
/// boundary already ran at commit time, so `World::save` needs no extra
/// step) plus a sibling `.repro.txt` naming the equivalent `hornvale new`
/// command. Returns the path to the written world JSON.
///
/// Best-effort by design: every caller that invokes this from a test's
/// failure path (see `fixture_staleness.rs`, `branches_family_calibration.rs`)
/// must treat any `Err` as informational and never let it mask the real
/// assertion failure that triggered the call.
/// type-audit: bare-ok(constructor-edge: seed), bare-ok(identifier-text: pin_set_label)
pub fn record_failure(study: &Study, seed: u64, pin_set_label: &str) -> io::Result<PathBuf> {
    let (pins, roster_name) = find_pin_set(study, pin_set_label)?;
    let roster = resolve_roster(roster_name.as_deref()).map_err(io::Error::other)?;
    let view =
        crate::WorldView::build_with_roster(Seed(seed), &pins, roster).map_err(io::Error::other)?;

    let dir = workspace_root().join(FAILURES_DIR);
    std::fs::create_dir_all(&dir)?;
    let base = format!("{}-seed{seed}-{pin_set_label}", study.name);
    let world_path = dir.join(format!("{base}.json"));
    let repro_path = dir.join(format!("{base}.repro.txt"));

    view.world.save(&world_path)?;
    std::fs::write(&repro_path, repro_command(study, seed, pin_set_label))?;

    Ok(world_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::study::{MetricSelection, PinSet, Seeds};
    use hornvale_kernel::World;

    fn tiny_study() -> Study {
        Study {
            name: "blackbox-test".to_string(),
            description: "unit test study".to_string(),
            seeds: Seeds { from: 0, count: 1 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec![],
                roster: None,
            }],
            metrics: MetricSelection::All("all".to_string()),
        }
    }

    #[test]
    fn record_failure_writes_a_world_that_round_trips_to_the_directly_built_one() {
        let study = tiny_study();
        let world_path = record_failure(&study, 7, "default").expect("record_failure succeeds");
        let recorded = World::load(&world_path).expect("recorded world parses");

        let direct = crate::WorldView::build(Seed(7), &SkyPins::default())
            .expect("direct build succeeds")
            .world;

        // World has no PartialEq (its ledger holds floats); compare via the
        // deterministic save format instead (`world_json_is_deterministic`
        // already pins that two builds of the same seed produce identical
        // JSON).
        assert_eq!(
            recorded.to_json(),
            direct.to_json(),
            "the recorded world must equal a world built directly for the same seed + pins"
        );

        let repro_path = world_path.with_extension("repro.txt");
        let repro = std::fs::read_to_string(&repro_path).expect("repro file exists");
        assert!(
            repro.contains("cargo run -p hornvale -- new --seed 7"),
            "repro command must name the seed: {repro}"
        );

        // Clean up: this test writes into the shared workspace
        // target/failures/ directory, not a private temp dir.
        let _ = std::fs::remove_file(&world_path);
        let _ = std::fs::remove_file(&repro_path);
    }

    #[test]
    fn record_failure_reports_an_unknown_pin_set_without_panicking() {
        let study = tiny_study();
        let err = record_failure(&study, 0, "no-such-set").expect_err("unknown pin set errors");
        assert!(
            err.to_string().contains("no-such-set"),
            "error must name the missing pin set: {err}"
        );
    }
}

//! The hornvale CLI: create worlds, render almanacs, interrogate via REPL.
#![warn(missing_docs)]

mod audio;
mod concepts;
mod dictionary;
mod phonology;
mod proto;
mod repl;
mod streams;

use hornvale_astronomy::{SkyPins, parse_pin};
use hornvale_kernel::{Seed, World};
use hornvale_worldgen as world_builder;
use std::process::ExitCode;

const SKY_FLAGS: &str =
    "  [--sky constant|generated]               sky provider (default: generated)
  [--moons N|MIN+K]                        pin the moon count, exact or graded
  [--rotation normal|locked]               pin the rotation regime
  [--day-hours F]                          pin the solar day length, in standard hours
  [--obliquity none|F]                     pin axial tilt in degrees
  [--year-days F]                          pin the year length, in local days
  [--neighbor blue-giant|red-giant|white-dwarf|orange-giant|red-dwarf|sun-like]
                                            force one showpiece neighbor star
";

const USAGE: &str = "\
usage:
  hornvale new --seed <N> [--out <PATH>] [sky flags]
                                            create a world (default out: world.json)
  hornvale scout [sky flags] [--from-seed N] [--limit K] [--max-scan M]
                                            scan seeds for ones satisfying the pins
  hornvale almanac [--world <PATH>]        render the almanac (default: world.json)
  hornvale repl [--world <PATH>]           interrogate a world interactively
  hornvale map [--world <PATH>] [--out <PNG>] render the elevation map (markdown to stdout)
  hornvale biome-map [--world <PATH>] [--out <PNG>] render the biome map (markdown to stdout)
  hornvale paleo-map [--world <PATH>] [--out <PNG>] render the deep-time strata map (markdown to stdout)
  hornvale settlement-map [--world <PATH>] [--out <PNG>] render the settlement map (markdown to stdout)
  hornvale star-chart [--world <PATH>] [--out <PNG>] render the star chart (markdown to stdout)
  hornvale orrery [--world <PATH>] [--day <D>] [--glyphs unicode|emoji]   print one orrery frame (ANSI)
  hornvale orrery [--world <PATH>] --day <A..B> [--step <k>] [--fps <f>] [--glyphs unicode|emoji] --cast <OUT>   animate to a .cast
  hornvale scene tiles [--world <PATH>] [--width <N>] emit scene/tiles/v1 JSON to stdout
  hornvale scene system [--world <PATH>]              emit scene/system/v1 JSON to stdout
  hornvale concepts                        dump the concept registry as markdown
  hornvale streams                         dump the stream manifest as markdown
  hornvale phonology                       dump per-species phonology as markdown
  hornvale dictionary [--world <PATH>]     dump per-species dictionary as markdown
  hornvale proto                           dump proto-goblinoid's inventory/phonotactics/proto-root table as markdown
  hornvale voice [--out <DIR>]             author missing phonology audio clips (espeak-ng + ffmpeg; default out: book/src/audio)
  hornvale lab run <PATH>                  run a batch study, publishing CSV + book artifacts
  hornvale lab diff <STUDY> <OLD_CSV> <NEW_CSV>  report which census metrics moved between two rows.csv snapshots
  hornvale lab list-metrics                list every metric in the lab's registry

sky flags (shared by new and scout):
";

const TERRAIN_FLAGS: &str = "\
  [--plates N]                             pin the plate count (2-64)
  [--ocean-fraction F]                     pin the target ocean fraction (0.05-0.95)
  [--supercontinent true|false]            cluster the continents into one landmass
";

const SETTLEMENT_FLAGS: &str = "\
  [--min-suitability F]                    pin the settlement placement floor (0-1); each species' single
                                           best (founder) cell bypasses it, so no floor drops a placed
                                           species below one settlement
  --species <NAME>  place only this species (default: all known species)
";

fn usage() -> String {
    format!(
        "{USAGE}{SKY_FLAGS}\nterrain flags (new only):\n{TERRAIN_FLAGS}\nsettlement flags (new only):\n{SETTLEMENT_FLAGS}"
    )
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = match args.first().map(String::as_str) {
        Some("new") => cmd_new(&args),
        Some("scout") => cmd_scout(&args),
        Some("almanac") => cmd_almanac(&args),
        Some("repl") => cmd_repl(&args),
        Some("map") => cmd_map(&args),
        Some("biome-map") => cmd_biome_map(&args),
        Some("paleo-map") => cmd_paleo_map(&args),
        Some("settlement-map") => cmd_settlement_map(&args),
        Some("star-chart") => cmd_star_chart(&args),
        Some("orrery") => cmd_orrery(&args),
        Some("scene") => cmd_scene(&args),
        Some("concepts") => cmd_concepts(),
        Some("streams") => cmd_streams(),
        Some("phonology") => cmd_phonology(),
        Some("dictionary") => cmd_dictionary(&args),
        Some("proto") => cmd_proto(),
        Some("voice") => audio::cmd_voice(&args),
        Some("lab") => cmd_lab(&args),
        Some("help") | None => {
            print!("{}", usage());
            Ok(())
        }
        Some(other) => Err(format!("unknown command '{other}'\n{}", usage())),
    };
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("error: {message}");
            ExitCode::FAILURE
        }
    }
}

/// Value of `--flag` in args, if present.
fn flag_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1))
        .map(String::as_str)
}

/// Parse the sky-related flags shared by `new` and `scout` into pins plus a
/// sky choice. One parser: every flag becomes a `key=value` pin string and
/// folds through `astronomy::parse_pin`, so pin-string syntax never drifts
/// from flag syntax. Default sky is `Generated` (spec §8).
fn parse_sky_args(args: &[String]) -> Result<(SkyPins, world_builder::SkyChoice), String> {
    let sky = match flag_value(args, "--sky") {
        None | Some("generated") => world_builder::SkyChoice::Generated,
        Some("constant") => world_builder::SkyChoice::Constant,
        Some(other) => return Err(format!("--sky: unknown value '{other}'")),
    };

    let mut pins = SkyPins::default();
    for (flag, key) in [
        ("--moons", "moons"),
        ("--rotation", "rotation"),
        ("--day-hours", "day-hours"),
        ("--obliquity", "obliquity"),
        ("--year-days", "year-days"),
        ("--neighbor", "neighbor"),
    ] {
        if let Some(value) = flag_value(args, flag) {
            parse_pin(&format!("{key}={value}"), &mut pins)?;
        }
    }
    Ok((pins, sky))
}

/// Parse the terrain flags into pins. One parser: every flag becomes a
/// `key=value` pin string and folds through `hornvale_terrain::parse_pin`,
/// so pin-string syntax never drifts from flag syntax.
fn parse_terrain_args(args: &[String]) -> Result<hornvale_terrain::TerrainPins, String> {
    let mut pins = hornvale_terrain::TerrainPins::default();
    for (flag, key) in [
        ("--plates", "plates"),
        ("--ocean-fraction", "ocean-fraction"),
        ("--supercontinent", "supercontinent"),
    ] {
        if let Some(value) = flag_value(args, flag) {
            hornvale_terrain::parse_pin(&format!("{key}={value}"), &mut pins)?;
        }
    }
    Ok(pins)
}

/// Parse the settlement flags into pins. One parser: every flag becomes a
/// `key=value` pin string and folds through `world_builder`'s settlement pin
/// parser, so pin-string syntax never drifts from flag syntax (mirrors
/// `parse_terrain_args`).
fn parse_settlement_args(args: &[String]) -> Result<world_builder::SettlementPins, String> {
    let mut pins = world_builder::SettlementPins::default();
    for (flag, key) in [
        ("--min-suitability", "min-suitability"),
        ("--species", "species"),
    ] {
        if let Some(value) = flag_value(args, flag) {
            world_builder::settlement_pins::parse_pin(&format!("{key}={value}"), &mut pins)?;
        }
    }
    Ok(pins)
}

fn cmd_new(args: &[String]) -> Result<(), String> {
    let seed: u64 = flag_value(args, "--seed")
        .ok_or("new requires --seed <N>")?
        .parse()
        .map_err(|e| format!("--seed must be a u64: {e}"))?;
    let out = flag_value(args, "--out").unwrap_or("world.json");
    let (pins, sky) = parse_sky_args(args)?;
    let terrain_pins = parse_terrain_args(args)?;
    let settlement_pins = parse_settlement_args(args)?;
    let world = world_builder::build_world(Seed(seed), &pins, sky, &terrain_pins, &settlement_pins)
        .map_err(|e| e.to_string())?;
    world
        .save(std::path::Path::new(out))
        .map_err(|e| format!("saving {out}: {e}"))?;
    let village = hornvale_settlement::village_info(&world)
        .map(|v| v.name)
        .unwrap_or_else(|| "no settlement".to_string());
    println!(
        "world of seed {seed} written to {out} ({} facts; village: {village})",
        world.ledger.len()
    );
    Ok(())
}

/// Deterministically scan seeds upward from `--from-seed` (default 0) for
/// ones whose generated sky satisfies the pins, printing up to `--limit`
/// (default 5) matches and scanning at most `--max-scan` (default 10000)
/// seeds. Read-only: never writes a world.
fn cmd_scout(args: &[String]) -> Result<(), String> {
    let (pins, _sky) = parse_sky_args(args)?;
    let from: u64 = flag_value(args, "--from-seed")
        .unwrap_or("0")
        .parse()
        .map_err(|e| format!("--from-seed: {e}"))?;
    let limit: u64 = flag_value(args, "--limit")
        .unwrap_or("5")
        .parse()
        .map_err(|e| format!("--limit: {e}"))?;
    let max_scan: u64 = flag_value(args, "--max-scan")
        .unwrap_or("10000")
        .parse()
        .map_err(|e| format!("--max-scan: {e}"))?;
    let mut found = 0u64;
    let mut scanned = 0u64;
    for seed in from..from.saturating_add(max_scan) {
        scanned += 1;
        if let Ok(outcome) = hornvale_astronomy::generate(hornvale_kernel::Seed(seed), &pins) {
            let system = &outcome.system;
            let day = match system.anchor.rotation {
                hornvale_astronomy::Rotation::Spinning { day } => {
                    format!("{:.1}h day", day.get() * 24.0)
                }
                hornvale_astronomy::Rotation::Locked => "tidally locked".to_string(),
            };
            println!(
                "seed {seed}: {} moons, {day}, year {:.1} std days, {} neighbors",
                system.moons.len(),
                system.anchor.year.get(),
                system.neighbors.len()
            );
            found += 1;
            if found == limit {
                break;
            }
        }
    }
    println!("scanned {scanned} seeds, found {found}");
    Ok(())
}

/// Load a world from `--world` (default world.json).
fn load_world(args: &[String]) -> Result<World, String> {
    let path = flag_value(args, "--world").unwrap_or("world.json");
    World::load(std::path::Path::new(path)).map_err(|e| format!("loading {path}: {e}"))
}

fn cmd_almanac(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let ctx = world_builder::almanac_context(&world).map_err(|e| e.to_string())?;
    print!("{}", hornvale_almanac::render(&ctx));
    Ok(())
}

fn cmd_repl(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    repl::run(&world, stdin.lock(), stdout.lock()).map_err(|e| e.to_string())
}

/// Render the world's elevation map: a markdown page (title, land lines,
/// ASCII map) to stdout and, with `--out`, the PNG image to disk. Both are
/// deterministic; CI drift-checks the committed copies.
/// Appended after every emitted map/chart PNG reference. The raster's exact
/// bytes are a *platform-local render*: pixel colors come from per-cell
/// classifications (biome, ocean) thresholded on transcendental-derived
/// floats, which the host math library computes to the last ULP differently
/// on different platforms. So the PNGs are not cross-platform byte-checked
/// (see the determinism decision on quantization); the markdown page's text
/// and ASCII map above remain deterministic. Canonical numeric state
/// (world.json, censuses, ephemeris) stays byte-identical everywhere.
const PLATFORM_LOCAL_RENDER_NOTE: &str = "> Rendered view — this raster's exact bytes are platform-local (pixel colors \
depend on the host math library) and are not cross-platform byte-checked; the \
page above is deterministic.\n\n";

fn cmd_map(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let terrain = world_builder::terrain_of(&world).map_err(|e| e.to_string())?;
    let mut doc = format!("# The Land of Seed {}\n\n", world.seed.0);
    for line in world_builder::land_lines(&world).map_err(|e| e.to_string())? {
        doc.push_str(&format!("{line}\n"));
    }
    doc.push_str("\n```text\n");
    doc.push_str(&hornvale_terrain::render::elevation_ascii(
        terrain.geosphere(),
        terrain.globe(),
    ));
    doc.push_str("```\n\n");
    if let Some(out) = flag_value(args, "--out") {
        let png = hornvale_terrain::render::elevation_png(
            terrain.geosphere(),
            terrain.globe(),
            world.seed,
        );
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
        let name = std::path::Path::new(out)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(out);
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
        doc.push_str(PLATFORM_LOCAL_RENDER_NOTE);
    }
    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    print!("{doc}");
    Ok(())
}

/// Render the world's biome map: a markdown page (title, biome/land lines,
/// ASCII biome map) to stdout and, with `--out`, the PNG image to disk. Both
/// are deterministic; CI drift-checks the committed copies.
fn cmd_biome_map(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let climate = world_builder::climate_of(&world).map_err(|e| e.to_string())?;
    let mut doc = format!("# The Biomes of Seed {}\n\n", world.seed.0);
    for line in world_builder::biome_lines(&world).map_err(|e| e.to_string())? {
        doc.push_str(&format!("{line}\n"));
    }
    doc.push_str("\n```text\n");
    doc.push_str(&hornvale_climate::render::biome_ascii(
        climate.geosphere(),
        &climate.biome_map(),
    ));
    doc.push_str("```\n\n");
    if let Some(out) = flag_value(args, "--out") {
        let png = hornvale_climate::render::biome_png(climate.geosphere(), &climate.biome_map());
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
        let name = std::path::Path::new(out)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(out);
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
        doc.push_str(PLATFORM_LOCAL_RENDER_NOTE);
    }
    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    print!("{doc}");
    Ok(())
}

/// Render the world's deep-time strata: a markdown page (title, deep-time
/// lines, ASCII strata map) to stdout and, with `--out`, the PNG to disk. Both
/// deterministic; CI drift-checks the committed copies.
fn cmd_paleo_map(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let terrain = world_builder::terrain_of(&world).map_err(|e| e.to_string())?;
    let record = world_builder::paleoclimate_of(&world).map_err(|e| e.to_string())?;
    let mut doc = format!("# The Deep Time of Seed {}\n\n", world.seed.0);
    doc.push_str(&format!(
        "Glacial maximum at day {:.0}; {:.0}% of the land lay under ice.\n\n",
        record.glacial_maximum_day,
        record.max_ice_fraction * 100.0
    ));
    doc.push_str("Legend: `#` ice envelope, `*` refugium, `~` fossil shoreline.\n\n");
    doc.push_str("```text\n");
    doc.push_str(&hornvale_paleoclimate::render::paleo_ascii(
        terrain.geosphere(),
        &record,
    ));
    doc.push_str("```\n\n");
    if let Some(out) = flag_value(args, "--out") {
        let png = hornvale_paleoclimate::render::paleo_png(terrain.geosphere(), &record);
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
        let name = std::path::Path::new(out)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(out);
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
        doc.push_str(PLATFORM_LOCAL_RENDER_NOTE);
    }
    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    print!("{doc}");
    Ok(())
}

/// Latitude/longitude of a place, read from settlement's facts (`Number`
/// values on `LATITUDE`/`LONGITUDE`). `None` if either is missing.
fn place_latlon(world: &World, id: hornvale_kernel::EntityId) -> Option<(f64, f64)> {
    let lat = match world.ledger.value_of(id, hornvale_settlement::LATITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => return None,
    };
    let lon = match world.ledger.value_of(id, hornvale_settlement::LONGITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => return None,
    };
    Some((lat, lon))
}

/// Render the world's settlement map: a markdown page (title, settlement
/// lines, ASCII overlay) to stdout and, with `--out`, the biome raster
/// overlaid with settlement marks to disk, as PNG. Both are deterministic.
fn cmd_settlement_map(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let climate = world_builder::climate_of(&world).map_err(|e| e.to_string())?;
    let places = hornvale_terrain::places(&world);
    let sites: Vec<(f64, f64)> = places
        .iter()
        .filter_map(|p| place_latlon(&world, p.id))
        .collect();
    let flagship =
        hornvale_settlement::village_info(&world).and_then(|v| place_latlon(&world, v.id));
    let mut doc = format!("# The Peoples of Seed {}\n\n", world.seed.0);
    for line in world_builder::settlement_lines(&world).map_err(|e| e.to_string())? {
        doc.push_str(&format!("{line}\n"));
    }
    doc.push_str("\n```text\n");
    doc.push_str(&hornvale_settlement::render::settlement_ascii(
        &sites, flagship,
    ));
    doc.push_str("```\n\n");
    if let Some(out) = flag_value(args, "--out") {
        let pixels =
            hornvale_climate::render::biome_pixels(climate.geosphere(), &climate.biome_map());
        let png = hornvale_settlement::render::overlay_png(&pixels, &sites, flagship);
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
        let name = std::path::Path::new(out)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(out);
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
        doc.push_str(PLATFORM_LOCAL_RENDER_NOTE);
    }
    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    print!("{doc}");
    Ok(())
}

/// Render the world's star chart: a markdown page (title, sun line, ASCII
/// chart, star legend, moon phase strips) to stdout and, with `--out`, the
/// planisphere PNG to disk. Both are deterministic; CI drift-checks the
/// committed copies. Errors on a world with no generated sky.
fn cmd_star_chart(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let sky = world_builder::sky_of(&world).map_err(|e| e.to_string())?;
    let Some(system) = sky.system() else {
        return Err("this world has no generated sky; no chart to draw".to_string());
    };
    let calendar = sky
        .calendar()
        .expect("a generated sky always has a calendar");
    let mut doc = format!("# The Night Sky of Seed {}\n\n", world.seed.0);
    doc.push_str(&format!("The sun is a {}.\n\n", system.star.class_name));
    doc.push_str("```text\n");
    doc.push_str(&hornvale_astronomy::render::chart_ascii(&system.neighbors));
    doc.push_str("```\n\n");
    for (index, n) in system.neighbors.iter().enumerate() {
        let name = hornvale_astronomy::class_name(n.class);
        let article = if name.starts_with(['a', 'e', 'i', 'o', 'u']) {
            "An"
        } else {
            "A"
        };
        doc.push_str(&format!(
            "{}. {} {} — {}, {:.1} light-years away, apparent brightness {:.3}.\n",
            index + 1,
            article,
            name,
            n.color,
            n.distance.get(),
            n.apparent_brightness
        ));
    }
    doc.push('\n');
    for line in hornvale_astronomy::render::moon_lines(&system.moons, calendar) {
        doc.push_str(&format!("{line}\n"));
    }
    doc.push('\n');
    if let Some(out) = flag_value(args, "--out") {
        let png = hornvale_astronomy::render::chart_png(&system.neighbors);
        std::fs::write(out, png).map_err(|e| format!("writing {out}: {e}"))?;
        let name = std::path::Path::new(out)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(out);
        doc.push_str(&format!("![Full-color render](./{name})\n\n"));
        doc.push_str(PLATFORM_LOCAL_RENDER_NOTE);
    }
    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    print!("{doc}");
    Ok(())
}

/// Render the orrery: one ANSI frame to stdout, or — with `--day A..B --cast
/// OUT` — a deterministic asciinema-v2 animation of the system over that span
/// (the moons cycling, the world orbiting, the sky drifting). Errors on a
/// world with no generated sky.
fn cmd_orrery(args: &[String]) -> Result<(), String> {
    use hornvale_astronomy::StdDays;
    use hornvale_astronomy::render::{GlyphSet, ORRERY_HEIGHT, orrery_ansi, orrery_cols};
    let world = load_world(args)?;
    let sky = world_builder::sky_of(&world).map_err(|e| e.to_string())?;
    let Some(system) = sky.system() else {
        return Err("this world has no generated sky; no orrery to draw".to_string());
    };
    let calendar = sky
        .calendar()
        .expect("a generated sky always has a calendar");
    let glyphs = match flag_value(args, "--glyphs").unwrap_or("unicode") {
        "unicode" => GlyphSet::Unicode,
        "emoji" => GlyphSet::Emoji,
        other => {
            return Err(format!(
                "unknown --glyphs '{other}'; expected 'unicode' or 'emoji'"
            ));
        }
    };
    let day_arg = flag_value(args, "--day").unwrap_or("0");
    let mk = |d: f64| StdDays::new(d).map_err(|e| e.to_string());

    if let Some(cast_path) = flag_value(args, "--cast") {
        let (a, b) = day_arg
            .split_once("..")
            .ok_or("--cast needs a --day range, e.g. --day 0..365")?;
        let (a, b): (f64, f64) = (
            a.parse().map_err(|_| "bad --day start")?,
            b.parse().map_err(|_| "bad --day end")?,
        );
        let step: f64 = flag_value(args, "--step")
            .unwrap_or("1")
            .parse()
            .map_err(|_| "bad --step")?;
        let fps: f64 = flag_value(args, "--fps")
            .unwrap_or("6")
            .parse()
            .map_err(|_| "bad --fps")?;
        if step <= 0.0 || b <= a || fps <= 0.0 {
            return Err("--day A..B must have B>A, --step>0, --fps>0".to_string());
        }
        let mut frames = Vec::new();
        let mut d = a;
        while d < b {
            // Hide the cursor (`?25l`), clear + home, then the frame, so playback
            // redraws in place with no cursor block parked below the grid. The
            // frame uses CRLF line endings: an asciinema-player replay feeds the
            // raw bytes through a terminal emulator, where a bare LF moves down
            // but not to column 0 — so full-width rows would smear. (The
            // single-frame stdout path needs neither: the tty shows its own
            // cursor and its cooked-mode ONLCR translates LF→CRLF.)
            frames.push(format!(
                "\u{1b}[?25l\u{1b}[2J\u{1b}[H{}",
                orrery_ansi(system, calendar, mk(d)?, glyphs).replace('\n', "\r\n")
            ));
            d += step;
        }
        // Synthetic frame timing: the interval is 1/fps, so --fps sets playback
        // speed (default 6 fps ≈ a slow, watchable ~12 s for a 73-frame year).
        // The value is synthetic; only its ratio to real time matters.
        let dt = 1.0 / fps;
        let cast = hornvale_kernel::asciinema_v2(
            orrery_cols(glyphs) as u16,
            ORRERY_HEIGHT as u16,
            dt,
            &frames,
        );
        std::fs::write(cast_path, cast).map_err(|e| format!("writing {cast_path}: {e}"))?;
        println!("wrote {} frames to {cast_path}", frames.len());
        Ok(())
    } else {
        let t = mk(day_arg.parse().map_err(|_| "bad --day")?)?;
        print!("{}", orrery_ansi(system, calendar, t, glyphs));
        Ok(())
    }
}

fn cmd_concepts() -> Result<(), String> {
    // Generated, not constant: the registry is identical either way (every
    // predicate is registered up front), but this exercises the fuller
    // pipeline as a bonus smoke test of the sky genesis wiring.
    let world = world_builder::build_world(
        Seed(0),
        &SkyPins::default(),
        world_builder::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &world_builder::SettlementPins::default(),
    )
    .map_err(|e| e.to_string())?;
    print!("{}", concepts::render_concepts(&world.registry));
    Ok(())
}

fn cmd_streams() -> Result<(), String> {
    print!("{}", streams::render_streams());
    Ok(())
}

fn cmd_phonology() -> Result<(), String> {
    print!("{}", phonology::render_phonology());
    Ok(())
}

/// Dump the loaded world's per-species dictionary as markdown (default
/// world: world.json, like `almanac`).
fn cmd_dictionary(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    print!("{}", dictionary::render_dictionary(&world)?);
    Ok(())
}

/// Dump proto-goblinoid's reference page (inventory, phonotactics, and
/// proto-root table) — a pure function of the reference seed, so unlike
/// `dictionary` this takes no `--world`.
fn cmd_proto() -> Result<(), String> {
    print!("{}", proto::render_proto()?);
    Ok(())
}

/// Dispatch `lab` subcommands: `run <PATH>`, `diff <STUDY> <OLD_CSV> <NEW_CSV>`,
/// and `list-metrics`.
fn cmd_lab(args: &[String]) -> Result<(), String> {
    match args.get(1).map(String::as_str) {
        Some("run") => cmd_lab_run(args),
        Some("diff") => cmd_lab_diff(args),
        Some("list-metrics") => cmd_lab_list_metrics(),
        Some(other) => Err(format!("lab: unknown subcommand '{other}'\n{}", usage())),
        None => Err(format!(
            "lab: requires a subcommand (run <PATH>|diff <STUDY> <OLD_CSV> <NEW_CSV>|list-metrics)\n{}",
            usage()
        )),
    }
}

/// Run a study end to end: load, run, write the CSV to `lab-out/`, publish
/// the summary and charts into the book, and report a one-line tally.
fn cmd_lab_run(args: &[String]) -> Result<(), String> {
    let path = args.get(2).ok_or("lab run requires <PATH>")?;
    let study = hornvale_lab::load_study(std::path::Path::new(path)).map_err(|e| e.to_string())?;
    let result = hornvale_lab::run(&study).map_err(|e| e.to_string())?;
    hornvale_lab::write_csv(&result, std::path::Path::new("lab-out")).map_err(|e| e.to_string())?;
    let written = hornvale_lab::publish(
        &result,
        std::path::Path::new("book/src/laboratory/generated"),
    )
    .map_err(|e| e.to_string())?;
    let refusals = result.rows.iter().filter(|r| r.refusal.is_some()).count();
    println!(
        "study {}: {} rows, {} refusals; summary + {} charts published.",
        result.study.name,
        result.rows.len(),
        refusals,
        written.len() - 1
    );
    Ok(())
}

/// Diff two `rows.csv` snapshots of one study: which metric moved, and by
/// how much. Old/new are paths to CSV files (typically `git show
/// HEAD:<...>/rows.csv` output vs the working tree's copy — `make lab-diff
/// STUDY=<name>` wraps exactly that).
fn cmd_lab_diff(args: &[String]) -> Result<(), String> {
    let (Some(study_path), Some(old_path), Some(new_path)) =
        (args.get(2), args.get(3), args.get(4))
    else {
        return Err(format!(
            "lab diff requires <STUDY> <OLD_CSV> <NEW_CSV>\n{}",
            usage()
        ));
    };
    let study =
        hornvale_lab::load_study(std::path::Path::new(study_path)).map_err(|e| e.to_string())?;
    let old_csv = std::fs::read_to_string(old_path).map_err(|e| format!("read {old_path}: {e}"))?;
    let new_csv = std::fs::read_to_string(new_path).map_err(|e| format!("read {new_path}: {e}"))?;
    print!(
        "{}",
        hornvale_lab::render_diff(&study, &old_csv, &new_csv).map_err(|e| e.to_string())?
    );
    Ok(())
}

fn cmd_lab_list_metrics() -> Result<(), String> {
    print!("{}", hornvale_lab::render_metric_list());
    Ok(())
}

/// Emit a scene description as JSON on stdout: `scene tiles` renders the
/// cartographic tile lattice (scene/tiles/v1), and `scene system` renders the
/// system's orbital elements for the orrery (scene/system/v1). Deterministic;
/// CI drift-checks the committed example scene.
fn cmd_scene(args: &[String]) -> Result<(), String> {
    match args.get(1).map(String::as_str) {
        Some("tiles") => {
            let world = load_world(args)?;
            let width = match flag_value(args, "--width") {
                Some(raw) => raw
                    .parse::<u32>()
                    .map_err(|e| format!("--width must be a u32: {e}"))?,
                None => 256,
            };
            let scene = hornvale_scene::tiles_scene(&world, width).map_err(|e| e.to_string())?;
            println!("{}", hornvale_scene::scene_json(&scene));
            Ok(())
        }
        Some("system") => {
            let world = load_world(args)?;
            let scene = hornvale_scene::system_scene(&world).map_err(|e| e.to_string())?;
            println!("{}", hornvale_scene::system_json(&scene));
            Ok(())
        }
        Some(other) => Err(format!(
            "unknown scene kind '{other}'; known kinds: tiles, system"
        )),
        None => Err("scene needs a kind; known kinds: tiles, system".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::{MoonsPin, NeighborClass, RotationPin};

    fn args(flags: &[&str]) -> Vec<String> {
        flags.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn default_sky_is_generated_with_no_pins() {
        let (pins, sky) = parse_sky_args(&args(&[])).unwrap();
        assert_eq!(sky, world_builder::SkyChoice::Generated);
        assert_eq!(pins, SkyPins::default());
    }

    #[test]
    fn terrain_flags_fold_into_pins() {
        let a = args(&[
            "new",
            "--plates",
            "12",
            "--ocean-fraction",
            "0.7",
            "--supercontinent",
            "true",
        ]);
        let pins = parse_terrain_args(&a).unwrap();
        assert_eq!(pins.plates, Some(12));
        assert_eq!(pins.ocean_fraction, Some(0.7));
        assert_eq!(pins.supercontinent, Some(true));
        assert_eq!(
            parse_terrain_args(&args(&["new"])).unwrap(),
            hornvale_terrain::TerrainPins::default()
        );
    }

    #[test]
    fn settlement_flags_fold_into_pins() {
        let a = args(&["new", "--min-suitability", "0.5"]);
        let pins = parse_settlement_args(&a).unwrap();
        assert_eq!(pins.min_suitability, Some(0.5));
        assert_eq!(
            parse_settlement_args(&args(&["new"])).unwrap(),
            world_builder::SettlementPins::default()
        );
    }

    #[test]
    fn sky_flag_selects_constant() {
        let (_, sky) = parse_sky_args(&args(&["--sky", "constant"])).unwrap();
        assert_eq!(sky, world_builder::SkyChoice::Constant);
    }

    #[test]
    fn sky_flag_selects_generated_explicitly() {
        let (_, sky) = parse_sky_args(&args(&["--sky", "generated"])).unwrap();
        assert_eq!(sky, world_builder::SkyChoice::Generated);
    }

    #[test]
    fn sky_flag_rejects_unknown_value() {
        let err = parse_sky_args(&args(&["--sky", "bogus"])).unwrap_err();
        assert!(err.contains("--sky"), "unexpected error text: {err}");
    }

    #[test]
    fn moons_flag_parses_exact_count() {
        let (pins, _) = parse_sky_args(&args(&["--moons", "2"])).unwrap();
        assert_eq!(pins.moons, Some(MoonsPin::exact(2).unwrap()));
    }

    #[test]
    fn moons_flag_parses_graded_pin() {
        let (pins, _) = parse_sky_args(&args(&["--moons", "2+1"])).unwrap();
        assert_eq!(pins.moons, Some(MoonsPin::graded(2, 1).unwrap()));
    }

    #[test]
    fn rotation_flag_parses() {
        let (pins, _) = parse_sky_args(&args(&["--rotation", "locked"])).unwrap();
        assert_eq!(pins.rotation, Some(RotationPin::Locked));
    }

    #[test]
    fn day_hours_flag_sets_a_period_hours_rotation() {
        let (pins, _) = parse_sky_args(&args(&["--day-hours", "30"])).unwrap();
        assert_eq!(pins.rotation, Some(RotationPin::PeriodHours(30.0)));
    }

    #[test]
    fn obliquity_flag_parses_a_number() {
        let (pins, _) = parse_sky_args(&args(&["--obliquity", "7.5"])).unwrap();
        assert_eq!(pins.obliquity.unwrap().get(), 7.5);
    }

    #[test]
    fn obliquity_flag_parses_none() {
        let (pins, _) = parse_sky_args(&args(&["--obliquity", "none"])).unwrap();
        assert_eq!(pins.obliquity.unwrap().get(), 0.0);
    }

    #[test]
    fn year_days_flag_parses() {
        let (pins, _) = parse_sky_args(&args(&["--year-days", "300"])).unwrap();
        assert_eq!(pins.year_local_days.unwrap().get(), 300.0);
    }

    #[test]
    fn neighbor_flag_parses() {
        let (pins, _) = parse_sky_args(&args(&["--neighbor", "blue-giant"])).unwrap();
        assert_eq!(pins.neighbor, Some(NeighborClass::BlueGiant));
    }

    #[test]
    fn all_flags_combine_into_one_pin_set() {
        let (pins, sky) = parse_sky_args(&args(&[
            "--sky",
            "generated",
            "--moons",
            "1+2",
            "--obliquity",
            "12.5",
            "--neighbor",
            "red-giant",
        ]))
        .unwrap();
        assert_eq!(sky, world_builder::SkyChoice::Generated);
        assert_eq!(pins.moons, Some(MoonsPin::graded(1, 2).unwrap()));
        assert_eq!(pins.obliquity.unwrap().get(), 12.5);
        assert_eq!(pins.neighbor, Some(NeighborClass::RedGiant));
    }

    #[test]
    fn bad_moons_value_yields_parse_error_text() {
        let err = parse_sky_args(&args(&["--moons", "abc"])).unwrap_err();
        assert!(err.contains("moons"), "unexpected error text: {err}");
    }

    #[test]
    fn moons_exceeding_the_legal_maximum_yields_constructor_error_text() {
        let err = parse_sky_args(&args(&["--moons", "4"])).unwrap_err();
        assert!(
            err.contains("legal maximum"),
            "unexpected error text: {err}"
        );
    }

    #[test]
    fn bad_rotation_value_yields_parse_error_text() {
        let err = parse_sky_args(&args(&["--rotation", "sideways"])).unwrap_err();
        assert!(err.contains("rotation"), "unexpected error text: {err}");
    }

    #[test]
    fn bad_obliquity_value_yields_constructor_error_text() {
        let err = parse_sky_args(&args(&["--obliquity", "-1"])).unwrap_err();
        assert!(!err.is_empty());
    }

    #[test]
    fn bad_neighbor_value_yields_parse_error_text() {
        let err = parse_sky_args(&args(&["--neighbor", "green-dwarf"])).unwrap_err();
        assert!(err.contains("neighbor"), "unexpected error text: {err}");
    }

    #[test]
    fn usage_mentions_scout() {
        assert!(USAGE.contains("scout"));
    }

    #[test]
    fn usage_mentions_lab() {
        assert!(USAGE.contains("lab run"));
        assert!(USAGE.contains("lab diff"));
        assert!(USAGE.contains("list-metrics"));
    }

    #[test]
    fn usage_mentions_biome_map() {
        assert!(USAGE.contains("biome-map"));
    }

    #[test]
    fn usage_mentions_settlement_map() {
        assert!(USAGE.contains("settlement-map"));
    }

    #[test]
    fn usage_mentions_paleo_map() {
        assert!(USAGE.contains("paleo-map"));
    }

    #[test]
    fn usage_mentions_star_chart() {
        assert!(USAGE.contains("star-chart"));
    }

    #[test]
    fn star_chart_on_a_constant_sun_world_is_a_loud_error() {
        use hornvale_kernel::Seed;
        use world_builder::{SettlementPins, SkyChoice, build_world};
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let path = std::env::temp_dir().join(format!(
            "hornvale-star-chart-test-{}.json",
            std::process::id()
        ));
        world.save(&path).unwrap();
        let err =
            cmd_star_chart(&args(&["star-chart", "--world", path.to_str().unwrap()])).unwrap_err();
        std::fs::remove_file(&path).ok();
        assert!(
            err.contains("no generated sky"),
            "unexpected error text: {err}"
        );
    }

    #[test]
    fn lab_run_unknown_path_is_an_error() {
        let err = cmd_lab(&args(&["lab", "run", "studies/does-not-exist.study.json"])).unwrap_err();
        assert!(!err.is_empty());
    }

    #[test]
    fn lab_unknown_subcommand_is_an_error() {
        let err = cmd_lab(&args(&["lab", "bogus"])).unwrap_err();
        assert!(err.contains("bogus"));
    }

    #[test]
    fn lab_with_no_subcommand_is_an_error() {
        let err = cmd_lab(&args(&["lab"])).unwrap_err();
        assert!(err.contains("subcommand"));
    }

    #[test]
    fn list_metrics_output_contains_belief_kind() {
        let output = hornvale_lab::render_metric_list();
        assert!(output.contains("belief-kind"));
    }

    #[test]
    fn usage_mentions_sky_flags() {
        let full = usage();
        assert!(full.contains("--sky"));
        assert!(full.contains("--moons"));
        assert!(full.contains("--neighbor"));
    }

    #[test]
    fn usage_mentions_min_suitability() {
        assert!(usage().contains("--min-suitability"));
    }

    #[test]
    fn usage_mentions_dictionary() {
        assert!(USAGE.contains("dictionary"));
    }

    #[test]
    fn usage_mentions_proto() {
        assert!(USAGE.contains("proto"));
    }

    #[test]
    fn scene_with_no_subcommand_is_an_error() {
        let err = cmd_scene(&args(&["scene"])).unwrap_err();
        assert!(err.contains("tiles"), "should name the known kinds: {err}");
    }

    #[test]
    fn scene_unknown_kind_is_an_error() {
        let err = cmd_scene(&args(&["scene", "dioramas"])).unwrap_err();
        assert!(err.contains("tiles"));
    }

    #[test]
    fn usage_mentions_scene() {
        assert!(usage().contains("scene tiles"));
    }

    fn test_generated_world() -> World {
        world_builder::build_world(
            Seed(42),
            &Default::default(),
            world_builder::SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .unwrap()
    }

    #[test]
    fn scene_system_emits_the_schema() {
        let json = hornvale_scene::system_json(
            &hornvale_scene::system_scene(&test_generated_world()).unwrap(),
        );
        assert!(json.contains("\"scene/system/v1\""));
        assert!(json.contains("\"moons\""));
    }

    #[test]
    fn scene_unknown_kind_names_system() {
        let err = cmd_scene(&args(&["scene", "dioramas"])).unwrap_err();
        assert!(
            err.contains("system"),
            "known kinds must include system: {err}"
        );
    }
}

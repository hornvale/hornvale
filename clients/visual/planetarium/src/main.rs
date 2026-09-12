#![allow(
    clippy::disallowed_types,
    reason = "Instant measures startup from main entry, never source time"
)]
use planetarium::shots::FilmDefinition;
use std::{error::Error, path::PathBuf};
fn main() {
    let started = std::time::Instant::now();
    if let Err(e) = run(started) {
        eprintln!("planetarium: {e}");
        std::process::exit(1);
    }
}
fn run(started: std::time::Instant) -> Result<(), Box<dyn Error>> {
    let mut args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        let executable = std::env::current_exe()?;
        let launch = executable
            .parent()
            .and_then(|p| p.parent())
            .ok_or("missing app bundle")?
            .join("Resources/launch.json");
        if launch.exists() {
            let configured: Vec<String> = serde_json::from_slice(&std::fs::read(launch)?)?;
            args.extend(configured);
        }
    }
    let arg = |name: &str| -> Result<String, Box<dyn Error>> {
        let i = args
            .iter()
            .position(|a| a == name)
            .ok_or_else(|| format!("missing {name}"))?;
        args.get(i + 1)
            .cloned()
            .ok_or_else(|| format!("missing value for {name}").into())
    };
    if args.get(1).map(String::as_str) == Some("verify") {
        planetarium::package::verify_package(&PathBuf::from(arg("--out")?))?;
        println!("VERIFIED study package");
        return Ok(());
    }
    let world = PathBuf::from(arg("--world")?);
    let revision = arg("--revision")?;
    let film: FilmDefinition = serde_json::from_slice(&std::fs::read(arg("--film")?)?)?;
    match args.get(1).map(String::as_str) {
        Some("inspect") => planetarium::live::run(world, revision, film, arg("--record").ok().map(PathBuf::from), arg("--benchmark-out").ok().map(PathBuf::from), started),
        Some("review") => {
            let stride = arg("--stride").unwrap_or_else(|_| "5".into()).parse()?;
            let width = arg("--width").unwrap_or_else(|_| "1920".into()).parse()?;
            planetarium::review::run(world, revision, film, PathBuf::from(arg("--output")?), stride, width)
        }
        Some("qualify") => planetarium::review::qualify(world, revision, film, PathBuf::from(arg("--output")?)),
        Some("capture") => {
            let limit = if args.iter().any(|v| v == "--limit") {
                Some(arg("--limit")?.parse()?)
            } else {
                None
            };
            planetarium::capture::run(world, revision, film, PathBuf::from(arg("--out")?), limit)
        }
        _ => Err("usage: planetarium inspect|review|capture --world PATH --revision SHA --film PATH [--out NEW_DIRECTORY --limit FRAMES]".into()),
    }
}

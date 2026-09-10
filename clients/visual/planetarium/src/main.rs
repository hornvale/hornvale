use planetarium::shots::FilmDefinition;
use std::{error::Error, path::PathBuf};
fn main() {
    if let Err(e) = run() {
        eprintln!("planetarium: {e}");
        std::process::exit(1);
    }
}
fn run() -> Result<(), Box<dyn Error>> {
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
    let world = PathBuf::from(arg("--world")?);
    let revision = arg("--revision")?;
    let film: FilmDefinition = serde_json::from_slice(&std::fs::read(arg("--film")?)?)?;
    match args.get(1).map(String::as_str){Some("inspect")=>planetarium::live::run(world,revision,film,arg("--record").ok().map(PathBuf::from)),Some("review")=>{let stride=arg("--stride").unwrap_or_else(|_|"5".into()).parse()?;let width=arg("--width").unwrap_or_else(|_|"1920".into()).parse()?;planetarium::review::run(world,revision,film,PathBuf::from(arg("--output")?),stride,width)},_=>Err("usage: planetarium inspect|review --world PATH --revision SHA --film PATH [--output NEW_DIRECTORY --stride 5 --width 1920]".into())}
}

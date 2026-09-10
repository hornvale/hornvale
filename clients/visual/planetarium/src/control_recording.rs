//! Optional acknowledged screenshots of real interactive controls, not film export.
use bevy::{
    prelude::*,
    render::view::screenshot::{Screenshot, ScreenshotCaptured},
};
use std::path::PathBuf;
#[derive(Resource)]
pub struct ControlRecording {
    directory: PathBuf,
    enabled: bool,
    pending: bool,
    next: u32,
    last_seconds: f64,
    pub error: Option<String>,
}
impl ControlRecording {
    pub fn new(
        directory: PathBuf,
        film: &crate::shots::FilmDefinition,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        std::fs::create_dir(&directory)?;
        use sha2::{Digest, Sha256};
        let executable = std::env::current_exe()?;
        let status = std::process::Command::new("git")
            .args(["status", "--porcelain"])
            .output()?;
        std::fs::write(
            directory.join("session.json"),
            serde_json::to_vec_pretty(
                &serde_json::json!({"schema":"planetarium/interactive-control-session/v1","film":film,"executable":executable,"executable_sha256":format!("{:x}",Sha256::digest(std::fs::read(&executable)?)),"working_tree_status":String::from_utf8_lossy(&status.stdout),"method":"acknowledged live window screenshots; state records are request-time annotations, not export-frame identity; input provenance is recorded by controller"}),
            )?,
        )?;
        Ok(Self {
            directory,
            enabled: false,
            pending: false,
            next: 0,
            last_seconds: -1.,
            error: None,
        })
    }
    pub fn toggle(&mut self) {
        self.enabled = !self.enabled;
        println!(
            "control recording={} pending={}",
            self.enabled, self.pending
        );
    }
}
pub fn sample(world: &mut World, seconds: f64, record: serde_json::Value) {
    let Some(mut state) = world.get_resource_mut::<ControlRecording>() else {
        return;
    };
    if !state.enabled
        || state.pending
        || state.error.is_some()
        || seconds - state.last_seconds < 0.1
    {
        return;
    }
    state.pending = true;
    state.last_seconds = seconds;
    let index = state.next;
    state.next += 1;
    let directory = state.directory.clone();
    world.spawn(Screenshot::primary_window()).observe(move |event:On<ScreenshotCaptured>,mut state:ResMut<ControlRecording>|{
        let result=(||->Result<(),String>{let image=event.image.clone().try_into_dynamic().map_err(|e|e.to_string())?;image.to_rgb8().save(directory.join(format!("control-{index:05}.png"))).map_err(|e|e.to_string())?;let bytes=serde_json::to_vec_pretty(&serde_json::json!({"schema":"planetarium/interactive-control-frame/v1","seconds":seconds,"state_at_request":record,"width":image.width(),"height":image.height()})).map_err(|e|e.to_string())?;std::fs::write(directory.join(format!("control-{index:05}.json")),bytes).map_err(|e|e.to_string())?;Ok(())})();
        state.pending=false;state.error=result.err();if let Some(e)=&state.error {eprintln!("control recording failed: {e}");}else{println!("control recording acknowledged frame={index}");}
    });
}

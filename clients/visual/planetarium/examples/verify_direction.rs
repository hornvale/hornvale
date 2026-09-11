//! Real-source seek qualification without GPU or per-frame world construction.
use hornvale_bevy_view::ObservationMirror;
use planetarium::{
    bridge::Bridge,
    live::positions,
    shots::{FilmDefinition, sample_caption, sample_shot},
};
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let world = std::env::args().nth(1).ok_or("provide saved world path")?;
    let film: FilmDefinition = serde_json::from_str(include_str!("../films/pilot.json"))?;
    let (mut source, initial) = Bridge::open(world.into(), film.binding.source_revision.clone())?;
    let mut mirror = ObservationMirror::new(&initial)?;
    film.validate(&mirror.initial().binding)?;
    let mut expected = None;
    for frame in (0..300).chain((0..300).rev()).chain([150]) {
        let query = mirror.request(film.clock().tick_at(frame)?)?;
        if !mirror.accept(&source.observe(query)?)? {
            return Err("exact reply rejected".into());
        }
        let result = serde_json::json!({"astronomy":mirror.current().unwrap().astronomy,"camera":sample_shot(&film,frame,&positions(&mirror))?,"caption":sample_caption(&film,frame)?});
        if frame == 150 {
            if let Some(ref prior) = expected {
                if prior != &result {
                    return Err("frame150 differs after forward/reverse/direct sampling".into());
                }
            } else {
                expected = Some(result);
            }
        }
    }
    println!(
        "{}",
        serde_json::to_string_pretty(
            &serde_json::json!({"schema":"planetarium/direction-seek-check/v1","binding":film.binding,"queries":source.diagnostics().queries,"frame150_equal_after_forward_reverse_direct":true,"frame150":expected})
        )?
    );
    Ok(())
}

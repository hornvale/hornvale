use planetarium::shots::{FilmDefinition, sample_caption, sample_shot};
use std::collections::BTreeMap;
fn film() -> FilmDefinition {
    serde_json::from_str(include_str!("../../films/pilot.json")).unwrap()
}
fn positions() -> BTreeMap<String, [f64; 3]> {
    BTreeMap::from([
        ("anchor".into(), [0.; 3]),
        ("moon:0".into(), [-279000., -132000., 9000.]),
    ])
}
/// claim: structural(every authored frame has exactly one shot; no seed sweep)
#[test]
fn every_frame_has_exactly_one_shot_and_cuts_are_declared() {
    let f = film();
    f.validate(&f.binding).unwrap();
    for frame in 0..300 {
        assert_eq!(
            f.shots
                .iter()
                .filter(|s| s.start_frame <= frame && frame < s.end_frame)
                .count(),
            1
        );
    }
    assert_eq!(sample_caption(&f, 89).unwrap(), "A world in motion");
    assert_eq!(sample_caption(&f, 90).unwrap(), "Turning into the light");
    assert_eq!(
        sample_caption(&f, 210).unwrap(),
        "Moons keep their own time"
    );
    assert!(sample_caption(&f, 300).is_err());
}
#[test]
fn rejects_gap_overlap_binding_and_unknown_target() {
    let f = film();
    for end in [89, 91] {
        let mut bad = f.clone();
        bad.shots[0].end_frame = end;
        assert!(bad.validate(&f.binding).is_err());
    }
    let mut b = f.binding.clone();
    b.source_revision = "0".repeat(40);
    assert!(f.validate(&b).is_err());
    let mut bad = f.clone();
    bad.shots[1].target_body_id = "missing".into();
    assert!(sample_shot(&bad, 150, &positions()).is_err());
}
#[test]
fn direct_frame_150_equals_sequential_and_reverse_camera_and_caption() {
    let f = film();
    let p = positions();
    let direct = sample_shot(&f, 150, &p).unwrap();
    let text = sample_caption(&f, 150).unwrap();
    for i in (0..300).chain((0..300).rev()) {
        let _ = sample_shot(&f, i, &p).unwrap();
    }
    assert_eq!(
        serde_json::to_string(&direct).unwrap(),
        serde_json::to_string(&sample_shot(&f, 150, &p).unwrap()).unwrap()
    );
    assert_eq!(text, sample_caption(&f, 150).unwrap());
}
#[test]
fn rejects_incompatible_dimensions_rate_count_and_interval() {
    let f = film();
    for i in 0..5 {
        let mut bad = f.clone();
        match i {
            0 => bad.width = 0,
            1 => bad.fps = 0,
            2 => bad.frames = 299,
            3 => bad.end_ticks = 999999,
            _ => bad.shots[0].focus_distance_km = 0.,
        };
        assert!(bad.validate(&f.binding).is_err());
    }
}

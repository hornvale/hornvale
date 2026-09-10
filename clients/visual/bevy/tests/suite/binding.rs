use hornvale_bevy_view::ObservationMirror;
const INITIAL: &str = include_str!("../fixtures/initial.json");
#[test]
fn accepts_entire_matching_snapshot() {
    let mut mirror = ObservationMirror::new(INITIAL).unwrap();
    let q: serde_json::Value = serde_json::from_str(&mirror.request(0).unwrap()).unwrap();
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    reply["request_id"] = q["request_id"].clone();
    assert!(mirror.accept(&reply.to_string()).unwrap());
    assert_eq!(mirror.current_ticks(), Some(0));
}
#[test]
fn mismatched_binding_cannot_change_current_snapshot() {
    let mut mirror = ObservationMirror::new(INITIAL).unwrap();
    mirror.request(0).unwrap();
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    reply["binding"]["source_id"] = serde_json::json!("other");
    assert!(mirror.accept(&reply.to_string()).is_err());
    assert_eq!(mirror.current_ticks(), None);
}
#[test]
fn reset_and_obsolete_replies_do_not_mix_world_state() {
    let mut mirror = ObservationMirror::new(INITIAL).unwrap();
    mirror.request(0).unwrap();
    mirror.request(1).unwrap();
    assert!(
        !mirror
            .accept(include_str!("../fixtures/reply.json"))
            .unwrap()
    );
    mirror.reset(INITIAL).unwrap();
    assert_eq!(mirror.current_ticks(), None);
    assert!(
        !mirror
            .accept(include_str!("../fixtures/reply.json"))
            .unwrap()
    );
}

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

fn binary() -> (String, serde_json::Value) {
    let mut initial: serde_json::Value = serde_json::from_str(INITIAL).unwrap();
    initial["system"]["stellar"]["topology"] = serde_json::json!("wide-binary");
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    let mut star = reply["astronomy"]["bodies"][1].clone();
    star["id"] = serde_json::json!("star:1");
    reply["astronomy"]["bodies"]
        .as_array_mut()
        .unwrap()
        .push(star);
    let mut light = reply["astronomy"]["lights"][0].clone();
    light["star_id"] = serde_json::json!("star:1");
    reply["astronomy"]["lights"]
        .as_array_mut()
        .unwrap()
        .push(light);
    (initial.to_string(), reply)
}
#[test]
fn missing_binary_light_cannot_replace_complete_snapshot() {
    let (initial, mut reply) = binary();
    let mut mirror = ObservationMirror::new(&initial).unwrap();
    mirror.request(0).unwrap();
    mirror.accept(&reply.to_string()).unwrap();
    let saved = serde_json::to_string(mirror.current().unwrap()).unwrap();
    for missing in 0..2 {
        let mut incomplete = reply.clone();
        incomplete["astronomy"]["lights"]
            .as_array_mut()
            .unwrap()
            .remove(missing);
        let request: serde_json::Value =
            serde_json::from_str(&mirror.request(60).unwrap()).unwrap();
        incomplete["request_id"] = request["request_id"].clone();
        incomplete["ticks"] = serde_json::json!(60);
        incomplete["astronomy"]["ticks"] = serde_json::json!(60);
        assert!(mirror.accept(&incomplete.to_string()).is_err());
        assert_eq!(mirror.current_ticks(), Some(0));
        assert_eq!(
            serde_json::to_string(mirror.current().unwrap()).unwrap(),
            saved
        );
    }
    reply["astronomy"]["lights"].as_array_mut().unwrap().pop();
    let mut fresh = ObservationMirror::new(&initial).unwrap();
    fresh.request(0).unwrap();
    assert!(fresh.accept(&reply.to_string()).is_err());
    assert_eq!(fresh.current_ticks(), None);
}
#[test]
fn first_binary_reply_requires_native_catalog_star_ids() {
    let (initial, mut reply) = binary();
    reply["astronomy"]["bodies"]
        .as_array_mut()
        .unwrap()
        .last_mut()
        .unwrap()["id"] = serde_json::json!("star:99");
    reply["astronomy"]["lights"][1]["star_id"] = serde_json::json!("star:99");
    let mut mirror = ObservationMirror::new(&initial).unwrap();
    mirror.request(0).unwrap();
    assert!(mirror.accept(&reply.to_string()).is_err());
}
#[test]
fn excessive_combined_radius_and_relief_is_rejected_atomically() {
    let mut initial: serde_json::Value = serde_json::from_str(INITIAL).unwrap();
    initial["tiles"]["sea_level_m"] = serde_json::json!(-1e9);
    initial["tiles"]["elevation_m"] = serde_json::json!(vec![1e9; 128]);
    let mut mirror = ObservationMirror::new(&initial.to_string()).unwrap();
    mirror.request(0).unwrap();
    assert!(
        mirror
            .accept(include_str!("../fixtures/reply.json"))
            .is_err()
    );
    assert_eq!(mirror.current_ticks(), None);
}

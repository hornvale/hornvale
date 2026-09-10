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
    initial["system"]["stellar"]["companion"] = serde_json::json!({"star": initial["system"]["stellar"]["primary"], "orbit": {"semi_major_axis_au":1.0,"period_days":365.0,"phase_offset":0.0}});
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

#[test]
fn old_binding_reply_after_reset_is_obsolete_but_current_conflict_is_error() {
    let mut m = ObservationMirror::new(INITIAL).unwrap();
    m.request(0).unwrap();
    let old = include_str!("../fixtures/reply.json");
    let mut initial: serde_json::Value = serde_json::from_str(INITIAL).unwrap();
    initial["binding"]["scope_id"] = serde_json::json!("replacement-scope");
    m.reset(&initial.to_string()).unwrap();
    m.request(1).unwrap();
    assert!(!m.accept(old).unwrap());
    let mut current: serde_json::Value = serde_json::from_str(old).unwrap();
    current["request_id"] = serde_json::json!(1);
    assert!(m.accept(&current.to_string()).is_err());
    assert_eq!(m.current_ticks(), None);
}
#[test]
fn newest_reply_stays_displayed_and_malformed_old_data_is_rejected() {
    let mut m = ObservationMirror::new(INITIAL).unwrap();
    m.request(0).unwrap();
    m.request(60).unwrap();
    let mut b: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    b["request_id"] = serde_json::json!(1);
    b["ticks"] = serde_json::json!(60);
    b["astronomy"]["ticks"] = serde_json::json!(60);
    assert!(m.accept(&b.to_string()).unwrap());
    assert!(!m.accept(include_str!("../fixtures/reply.json")).unwrap());
    assert_eq!(m.current_ticks(), Some(60));
    let mut bad = b.clone();
    bad["astronomy"]["bodies"][0]["radius_km"] = serde_json::json!(-1);
    assert!(m.accept(&bad.to_string()).is_err());
    b["ticks"] = serde_json::json!(61);
    b["astronomy"]["ticks"] = serde_json::json!(61);
    assert!(m.accept(&b.to_string()).is_err());
    assert_eq!(m.current_ticks(), Some(60));
}
#[test]
fn reset_to_valid_source_without_optional_bodies() {
    let mut m = ObservationMirror::new(INITIAL).unwrap();
    m.request(0).unwrap();
    m.accept(include_str!("../fixtures/reply.json")).unwrap();
    let mut initial: serde_json::Value = serde_json::from_str(INITIAL).unwrap();
    initial["system"]["moons"] = serde_json::json!([]);
    initial["system"]["wanderers"] = serde_json::json!([]);
    initial["moons"]["moons"] = serde_json::json!([]);
    initial["binding"]["world_sha256"] = serde_json::json!("f".repeat(64));
    m.reset(&initial.to_string()).unwrap();
    assert_eq!(m.current_ticks(), None);
    let q: serde_json::Value = serde_json::from_str(&m.request(0).unwrap()).unwrap();
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    reply["binding"] = initial["binding"].clone();
    reply["request_id"] = q["request_id"].clone();
    reply["astronomy"]["bodies"]
        .as_array_mut()
        .unwrap()
        .retain(|b| b["kind"] == "anchor" || b["kind"] == "star");
    assert!(m.accept(&reply.to_string()).unwrap());
    assert_eq!(m.current().unwrap().astronomy.bodies.len(), 2);
}
#[test]
fn first_reply_requires_native_wanderer_indices() {
    let mut m = ObservationMirror::new(INITIAL).unwrap();
    m.request(0).unwrap();
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    let body = reply["astronomy"]["bodies"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|b| b["kind"] == "wanderer")
        .unwrap();
    body["id"] = serde_json::json!("wanderer:99");
    assert!(m.accept(&reply.to_string()).is_err());
    assert_eq!(m.current_ticks(), None);
}
#[test]
fn committed_conflict_errors_while_newer_request_remains_pending() {
    let mut mirror = ObservationMirror::new(INITIAL).unwrap();
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    mirror.request(-1).unwrap();
    let old = {
        let mut old = reply.clone();
        old["ticks"] = serde_json::json!(-1);
        old["astronomy"]["ticks"] = serde_json::json!(-1);
        old.to_string()
    };
    mirror.request(0).unwrap();
    reply["request_id"] = serde_json::json!(1);
    mirror.accept(&reply.to_string()).unwrap();
    let committed = serde_json::to_value(mirror.current().unwrap()).unwrap();
    mirror.request(60).unwrap();
    assert!(!mirror.accept(&reply.to_string()).unwrap());
    assert!(!mirror.accept(&old).unwrap());
    reply["ticks"] = serde_json::json!(1);
    reply["astronomy"]["ticks"] = serde_json::json!(1);
    assert!(mirror.accept(&reply.to_string()).is_err());
    assert_eq!(
        serde_json::to_value(mirror.current().unwrap()).unwrap(),
        committed
    );
    assert_eq!(mirror.pending_ticks(), Some(60));
}

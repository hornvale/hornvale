use hornvale_bevy_view::documents;
#[test]
fn native_initial_is_accepted() {
    assert!(documents::initial(include_str!("../fixtures/initial.json")).is_ok());
}
#[test]
fn native_reply_with_explicit_nulls_is_accepted() {
    assert!(documents::reply(include_str!("../fixtures/reply.json")).is_ok());
}
#[test]
fn rejects_missing_initial_fields() {
    let mut v: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/initial.json")).unwrap();
    for key in ["system", "moons", "tiles", "binding", "ticks_per_std_day"] {
        let saved = v.as_object_mut().unwrap().remove(key).unwrap();
        assert!(documents::initial(&v.to_string()).is_err(), "{key}");
        v[key] = saved;
    }
}
#[test]
fn rejects_schema_frame_radius_and_duplicate_ids() {
    let original: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    for (pointer, value) in [
        ("/schema", serde_json::json!("bad")),
        ("/astronomy/frame", serde_json::json!("metres")),
        ("/astronomy/bodies/0/radius_km", serde_json::json!(-1)),
        ("/astronomy/bodies/1/id", serde_json::json!("anchor")),
    ] {
        let mut v = original.clone();
        *v.pointer_mut(pointer).unwrap() = value;
        assert!(documents::reply(&v.to_string()).is_err(), "{pointer}");
    }
}

#[test]
fn declared_binary_topology_discriminants_are_accepted() {
    let original: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/initial.json")).unwrap();
    for topology in ["single", "wide-binary", "close-binary"] {
        let mut v = original.clone();
        v["system"]["stellar"]["topology"] = serde_json::json!(topology);
        assert!(documents::initial(&v.to_string()).is_ok());
    }
}
#[test]
fn rejects_missing_null_fields_and_nonfinite_json_numbers() {
    let mut v: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    v["astronomy"]["bodies"][1]
        .as_object_mut()
        .unwrap()
        .remove("radius_km");
    assert!(documents::reply(&v.to_string()).is_err());
    assert!(
        documents::reply(&include_str!("../fixtures/reply.json").replace("7126.4059", "1e999"))
            .is_err()
    );
}
#[test]
fn rejects_tile_array_and_unit_corruption() {
    let mut v: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/initial.json")).unwrap();
    v["tiles"]["elevation_m"].as_array_mut().unwrap().pop();
    assert!(documents::initial(&v.to_string()).is_err());
    let mut v: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/initial.json")).unwrap();
    v["tiles"]["schema"] = serde_json::json!("scene/tiles-feet/v1");
    assert!(documents::initial(&v.to_string()).is_err());
}

#[test]
fn rejects_finite_overflow_sized_physical_geometry() {
    let initial: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/initial.json")).unwrap();
    for pointer in [
        "/moons/moons/0/radius_km",
        "/tiles/elevation_m/0",
        "/tiles/sea_level_m",
    ] {
        let mut bad = initial.clone();
        *bad.pointer_mut(pointer).unwrap() = serde_json::json!(1e300);
        assert!(documents::initial(&bad.to_string()).is_err(), "{pointer}");
    }
    let reply: serde_json::Value =
        serde_json::from_str(include_str!("../fixtures/reply.json")).unwrap();
    for index in [0, 2] {
        let mut bad = reply.clone();
        bad["astronomy"]["bodies"][index]["radius_km"] = serde_json::json!(1e300);
        assert!(documents::reply(&bad.to_string()).is_err(), "body {index}");
    }
}

use hornvale_bevy_view::{
    astronomy::{lighting, surface},
    bevy::mesh::VertexAttributeValues,
    documents,
};
#[test]
fn globe_uses_source_north_and_sea_reference_without_relief_gain() {
    let mut initial = documents::initial(include_str!("../fixtures/initial.json")).unwrap();
    initial.tiles.ocean.fill(false);
    initial
        .tiles
        .elevation_m
        .fill(initial.tiles.sea_level_m + 2000.0);
    let mesh = surface::globe_mesh(&initial.tiles, 7000.0, 1000.0);
    let Some(VertexAttributeValues::Float32x3(points)) =
        mesh.attribute(hornvale_bevy_view::bevy::mesh::Mesh::ATTRIBUTE_POSITION)
    else {
        panic!("positions")
    };
    assert!((points[0][2] - 7.002).abs() < 1e-6);
    let middle =
        (initial.tiles.height / 2 * (initial.tiles.width + 1) + initial.tiles.width / 2) as usize;
    assert!((points[middle][0] - 7.002).abs() < 1e-6);
    assert!(points[middle][1].abs() < 1e-6 && points[middle][2].abs() < 1e-6);
}
#[test]
fn point_flux_respects_distance_and_render_scale() {
    for scale in [1.0, 1000.0] {
        let intensity = f64::from(lighting::point_intensity(0.7, scale).unwrap());
        let distance = 149_597_870.7 / scale;
        let flux = intensity / (4.0 * std::f64::consts::PI * distance * distance);
        assert!((flux - 127_000.0 * 0.7).abs() < 0.01);
    }
}

fn icy_coast(sea_ice: bool) -> documents::Tiles {
    let mut t = documents::initial(include_str!("../fixtures/initial.json"))
        .unwrap()
        .tiles;
    t.snow_fraction.fill(1.0);
    t.t_mean_c.fill(-25.0);
    t.moisture.fill(0.5);
    let ice = t.biome_legend.iter().position(|b| b == "ice").unwrap();
    let ocean = t
        .biome_legend
        .iter()
        .position(|b| b == if sea_ice { "sea-ice" } else { "epipelagic" })
        .unwrap();
    for i in 0..t.elevation_m.len() {
        let water = i % (t.width as usize) < t.width as usize / 2;
        t.ocean[i] = water;
        t.biome[i] = if water { ocean } else { ice };
        t.elevation_m[i] = t.sea_level_m + if water { -1000.0 } else { 200.0 };
    }
    t
}
#[test]
fn icy_land_and_icy_ocean_do_not_gain_a_dark_reconstruction_seam() {
    let texture = surface::anchor_texture(&icy_coast(true));
    let pixels = texture.data.as_ref().unwrap();
    for x in 900..=1200 {
        assert!(pixels[(512 * 2048 + x) * 4] > 150, "dark seam at x={x}");
    }
}
#[test]
fn adjacent_icy_land_does_not_create_sea_ice_over_open_water() {
    let texture = surface::anchor_texture(&icy_coast(false));
    let pixels = texture.data.as_ref().unwrap();
    assert!(pixels[(512 * 2048 + 512) * 4] < 30);
}
#[test]
fn source_longitude_seam_and_poles_reconstruct_continuously() {
    let t = icy_coast(true);
    assert_eq!(
        surface::sample(&t, &t.elevation_m, 0.0, 0.5),
        surface::sample(&t, &t.elevation_m, 1.0, 0.5)
    );
    assert_eq!(
        surface::sample(&t, &t.elevation_m, 0.25, -1.0),
        surface::sample(&t, &t.elevation_m, 0.25, 0.0)
    );
}

#[test]
fn physical_geometry_limits_include_scaled_radius_and_relief() {
    use hornvale_bevy_view::coordinates::render_radius;
    assert!(render_radius(0.001, 0.0, 1000.0).is_ok());
    assert!(render_radius(999_920.0, 80.0, 1000.0).is_ok());
    for (radius, relief, scale) in [
        (1e300, 0.0, 1000.0),
        (7000.0, 1e300, 1000.0),
        (999_921.0, 80.0, 1000.0),
        (0.0001, 0.0, 1000.0),
        (7000.0, 0.0, 1e-300),
        (7000.0, 0.0, 1e300),
    ] {
        assert!(render_radius(radius, relief, scale).is_err());
    }
}

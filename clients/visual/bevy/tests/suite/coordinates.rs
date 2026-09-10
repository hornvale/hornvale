#![allow(
    clippy::disallowed_methods,
    reason = "screen-projection reference math is client-side and must not import the simulation kernel"
)]
use hornvale_bevy_view::coordinates::render_position;
#[test]
fn subtracts_before_converting_to_float() {
    let near = [1000.125, 240.25, -4.5];
    let origin = [1e12, 1e12, 1e12];
    let far = std::array::from_fn(|i| origin[i] + near[i]);
    assert_eq!(
        render_position(near, [0.; 3], 1000.).unwrap(),
        render_position(far, origin, 1000.).unwrap()
    );
}
#[test]
fn bounds_projection_error_to_quarter_pixel_at_4k() {
    for origin in [[0.; 3], [1e12, -1e12, 1e12]] {
        for distance in [8000., 20000., 307744., 1e7] {
            for offset in [1599., 7126., -2274.] {
                let relative = [offset, offset * 0.4, distance];
                let p = std::array::from_fn(|i| origin[i] + relative[i]);
                let actual = render_position(p, origin, 1000.).unwrap();
                let focal = 2160.0 / (2.0 * (0.005_f64 / 2.0).tan());
                let expected = (p[0] - origin[0]) / (p[2] - origin[2]) * focal;
                let projected = f64::from(actual[0]) / f64::from(actual[2]) * focal;
                assert!(
                    (expected - projected).abs() < 0.25,
                    "{expected} {projected}"
                );
            }
        }
    }
}
#[test]
fn refuses_invalid_scale_and_range() {
    for scale in [0., -1., f64::NAN] {
        assert!(render_position([0.; 3], [0.; 3], scale).is_err());
    }
    assert!(render_position([f64::INFINITY; 3], [0.; 3], 1.).is_err());
    assert!(render_position([1e15; 3], [0.; 3], 1000.).is_err());
}

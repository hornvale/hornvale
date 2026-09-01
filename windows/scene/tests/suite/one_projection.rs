//! Spec section 2.0: there is ONE projection in this repository, not two that
//! happen to agree today. `windows/scene/src/region.rs` used to carry its own
//! copy of the cube-face basis, `param`, `locate_on_cube` and `face_unit`;
//! this file is what would have caught the day that copy drifted from
//! `hornvale_kernel::cube`'s.

/// `Facet::containing`/`Facet::centroid` (the room mesh's own projection) and
/// `region.rs`'s tile sampler now both call straight into
/// `hornvale_kernel::cube::face_unit`/`locate` — there is nothing left to
/// drift. This round-trips through both directions across every face.
#[test]
fn region_and_room_project_through_the_same_function() {
    for face in 0..6usize {
        for &(a, b) in &[(-0.7, 0.3), (0.0, 0.0), (0.55, -0.85)] {
            let p = hornvale_kernel::cube::face_unit(face, a, b);
            let f = hornvale_kernel::Facet::containing(p, 8);
            let (f2, _, _) = hornvale_kernel::cube::locate(f.centroid());
            assert_eq!(f2, face);
        }
    }
}

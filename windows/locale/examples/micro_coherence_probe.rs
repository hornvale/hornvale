//! THROWAWAY DIAGNOSTIC (delete me) — what resolution is the terrain grid
//! actually at, and is anything finer than it?

use hornvale_kernel::Geosphere;

fn main() {
    for level in 4..=7 {
        let g = Geosphere::new(level);
        let n = g.vertex_count();
        // Vertices around a great circle: the linear measure. A quasi-uniform
        // sphere of N points has ~sqrt(N * pi) around a great circle
        // (N points over 4*pi steradians -> spacing sqrt(4*pi/N) -> 2*pi/spacing).
        let around =
            (2.0 * std::f64::consts::PI / (4.0 * std::f64::consts::PI / n as f64).sqrt()).round();
        let spacing_km = 40_075.0 / around;
        println!(
            "level {level}: {n:>7} vertices total  |  ~{around:.0} around a great circle  |  ~{spacing_km:.0} km spacing"
        );
    }
    println!(
        "\nhornvale_terrain::GLOBE_LEVEL = {}",
        hornvale_terrain::GLOBE_LEVEL
    );
}

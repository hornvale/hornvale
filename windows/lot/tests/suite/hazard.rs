//! H-M1 and H-M2 (spec §8): the Siler hazard's calibration band and monotonicity.
use hornvale_lot::hazard::{Hazard, death_age, e0, q_before, survival_table};

fn human_like() -> Hazard {
    Hazard {
        lifespan_years: 60.0,
        strife: 0.0,
    }
}

/// H-M1: a 60-year-lifespan people at zero strife reads e0 in [28, 36] years
/// and died-before-15 in [0.35, 0.45] — the pre-modern human band.
#[test]
fn calibration_band_holds_at_the_human_anchor() {
    let h = human_like();
    let e = e0(&h);
    assert!((28.0..=36.0).contains(&e), "e0 = {e}");
    let q15 = q_before(&h, 15.0);
    assert!((0.35..=0.45).contains(&q15), "q15 = {q15}");
}

/// H-M2: e0 rises with lifespan and falls with strife, on a grid.
#[test]
fn e0_is_monotone_in_lifespan_and_strife() {
    let mut last = 0.0;
    for l in [20.0, 40.0, 60.0, 120.0, 300.0] {
        let e = e0(&Hazard {
            lifespan_years: l,
            strife: 0.0,
        });
        assert!(e > last, "e0({l}) = {e} not above {last}");
        last = e;
    }
    let mut last = f64::INFINITY;
    for strife_level in [0.0, 0.25, 0.5, 1.0] {
        let e = e0(&Hazard {
            lifespan_years: 60.0,
            strife: strife_level,
        });
        assert!(e < last, "e0(strife {strife_level}) = {e} not below {last}");
        last = e;
    }
}

#[test]
fn survival_starts_at_one_and_is_nonincreasing() {
    let t = survival_table(&human_like());
    assert_eq!(t[0], 1.0);
    for w in t.windows(2) {
        assert!(w[1] <= w[0]);
    }
    assert!(*t.last().unwrap() < 1e-3, "S(2L) = {}", t.last().unwrap());
}

#[test]
fn death_age_is_the_inverse_of_survival() {
    let h = human_like();
    let t = survival_table(&h);
    for u in [0.9, 0.5, 0.1, 0.01] {
        let a = death_age(&h, u);
        let i = a.floor() as usize;
        assert!(
            t[i] >= u - 1e-9 && t[(i + 1).min(t.len() - 1)] <= u + 1e-9,
            "u={u} a={a}"
        );
    }
    assert_eq!(death_age(&h, 1.0), 0.0);
}

/// H-M4: attributing the hazard must not move the mortality envelope.
#[test]
fn attribution_preserves_hm2_survival_bytes() {
    let mut digest = 0xcbf2_9ce4_8422_2325_u64;
    for lifespan_years in [20.0, 40.0, 60.0, 120.0, 300.0] {
        for strife in [0.0, 0.25, 0.5, 1.0] {
            let hazard = Hazard {
                lifespan_years,
                strife,
            };
            for value in survival_table(&hazard)
                .into_iter()
                .chain([e0(&hazard), q_before(&hazard, 15.0)])
            {
                for byte in value.to_bits().to_le_bytes() {
                    digest ^= u64::from(byte);
                    digest = digest.wrapping_mul(0x0000_0100_0000_01b3);
                }
            }
        }
    }
    assert_eq!(
        digest, 0xc684_461c_0d83_9d3a,
        "the frozen H-M2 mortality envelope moved"
    );
}

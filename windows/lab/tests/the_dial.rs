//! The bet, preregistered (spec §6) — AS AMENDED after T3's partial
//! unblinding (decision ledger #13; spec §6 addendum): the original C1
//! ordered the poles on the distortion AGGREGATE, and T3 measured that
//! clause false at seeds 1–6 (a shipped voice's lossless salience reorder
//! outweighs the pathological pole's total loss — order divergence is
//! distinctiveness, not destruction). The amended criteria return to
//! LANG-41's named axes; no threshold was relaxed.
//! C1a loss-monotonicity: loss_fraction(null)=0 < loss_fraction(shipped)
//!    < loss_fraction(pathological)=1, every measured multi-people world.
//! C1b recoverability ordering: recoverability(null)=1;
//!    recoverability(pathological) <= 0.25.
//! C1c the null pole is uncanny: pairwise distinctiveness of null-filter
//!    voices == 0.0 exactly.
//! C2 band: shipped mean distinctiveness >= 0.05 on at least one measured
//!    multi-people world; per world, shipped recoverability >=
//!    pathological recoverability + 0.25.
//! C3 non-vacuity: mean pairwise DISTINCTIVENESS > 0 on every measured
//!    multi-people world. (Distinctiveness, not distortion-variance:
//!    two accounts can differ in stance VALUES while their distortion
//!    magnitudes tie — seed 1 does exactly this, honestly. Variance stays
//!    the census population number, read against param spread; it is not
//!    the standing criterion.)
//! A C1/C3 failure FALSIFIES the bet (report at close; taste fallback —
//! do NOT tune these thresholds to pass); a C2 failure alone is
//! calibration work on the derived strengths.

use hornvale_language::{
    Account, Disposition, account_of, distinctiveness, identity_params, recoverability,
};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, accounts_of, build_world, chorus_ground, pathological_params,
};

/// Build a world with the shipped four-people component set, generated sky,
/// default terrain/settlement pins — the shared pattern every neighboring
/// worldgen integration test (`exposure.rs`, `species_worlds.rs`,
/// `chorus_params.rs`) uses.
fn generated(seed: u64) -> hornvale_kernel::World {
    build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// The fraction of `account`'s entries that did NOT survive unchanged
/// (`Lost` or `Substituted`), over `n = entries.len()`. Deliberately not a
/// shipped helper (the brief) — derived here from the account's own
/// entries, the same computation `distortion`'s loss component performs
/// internally.
fn loss_fraction(account: &Account) -> f64 {
    let n = account.entries.len();
    if n == 0 {
        return 0.0;
    }
    let lost = account
        .entries
        .iter()
        .filter(|e| e.disposition != Disposition::Kept)
        .count();
    lost as f64 / n as f64
}

/// The mean of every pairwise `distinctiveness()` value over `accounts`
/// (each pair compared once). Panics if `accounts.len() < 2` — callers
/// gate on a multi-people world themselves.
fn mean_pairwise_distinctiveness(accounts: &[&Account]) -> f64 {
    let mut pairs = Vec::new();
    for i in 0..accounts.len() {
        for j in (i + 1)..accounts.len() {
            pairs.push(distinctiveness(accounts[i], accounts[j]));
        }
    }
    assert!(
        !pairs.is_empty(),
        "mean_pairwise_distinctiveness needs at least 2 accounts"
    );
    pairs.iter().sum::<f64>() / pairs.len() as f64
}

/// The known-groups standing test — the program's biggest bet (LANG-41,
/// spec §6, as amended by decision ledger #13). A failure here is a
/// campaign FINDING, not a bug: STOP and report the measured values rather
/// than tuning any threshold.
#[test]
fn the_dial_separates_the_poles() {
    const MEASURED_SEEDS: [u64; 4] = [1, 2, 3, 42];

    let mut multi_people_worlds = 0usize;
    let mut band_achieved = false;
    // Per-seed measured values, reported verbatim (eprintln, the
    // terminator_acceptance.rs precedent) so a preregistered result is
    // never narrated instead of measured.
    let mut report: Vec<String> = Vec::new();

    for seed in MEASURED_SEEDS {
        let world = generated(seed);
        let voices = accounts_of(&world);
        if voices.len() < 2 {
            // Skip: the multi-people criteria (C1c/C2/C3) need at least two
            // placed cultures to compare. C1a/C1b (single-voice pole
            // ordering) still require a multi-people world per the brief's
            // "every measured multi-people world" framing, so the skip
            // applies uniformly.
            report.push(format!(
                "seed {seed}: SKIPPED ({} placed culture(s), need >= 2)",
                voices.len()
            ));
            continue;
        }
        multi_people_worlds += 1;

        let ground = chorus_ground(&world);
        let null_params = identity_params();
        let pathological = pathological_params();
        let null_account = account_of(&ground, &null_params);
        let pathological_account = account_of(&ground, &pathological);

        // C1a (null/pathological poles) + C1b.
        let loss_null = loss_fraction(&null_account);
        let loss_pathological = loss_fraction(&pathological_account);
        assert_eq!(
            loss_null, 0.0,
            "seed {seed}: the null filter must keep every fact (loss_fraction == 0)"
        );
        assert_eq!(
            loss_pathological, 1.0,
            "seed {seed}: the pathological pole must lose or substitute every fact \
             (loss_fraction == 1)"
        );
        let recoverability_null = recoverability(&null_account);
        let recoverability_pathological = recoverability(&pathological_account);
        assert_eq!(
            recoverability_null, 1.0,
            "seed {seed}: the null filter must be perfectly recoverable"
        );
        assert!(
            recoverability_pathological <= 0.25,
            "seed {seed}: the pathological pole's recoverability must be <= 0.25, got {recoverability_pathological}"
        );

        // C1a per shipped voice: strict loss ordering against both poles.
        for voice in &voices {
            let loss_shipped = loss_fraction(&voice.account);
            assert!(
                loss_null < loss_shipped,
                "seed {seed}, {}: loss_fraction(shipped)={loss_shipped} must exceed \
                 loss_fraction(null)={loss_null}",
                voice.kind
            );
            assert!(
                loss_shipped < loss_pathological,
                "seed {seed}, {}: loss_fraction(shipped)={loss_shipped} must be less than \
                 loss_fraction(pathological)={loss_pathological}",
                voice.kind
            );
        }

        // C1c: the null pole is uncanny — every placed culture's account
        // under the null filter is pairwise indistinguishable.
        let null_accounts: Vec<Account> = voices
            .iter()
            .map(|_| account_of(&ground, &null_params))
            .collect();
        for i in 0..null_accounts.len() {
            for j in (i + 1)..null_accounts.len() {
                let d = distinctiveness(&null_accounts[i], &null_accounts[j]);
                assert_eq!(
                    d, 0.0,
                    "seed {seed}: null-filter voices {} and {} must be exactly \
                     indistinguishable, got distinctiveness {d}",
                    voices[i].kind, voices[j].kind
                );
            }
        }

        // C2: per-world shipped-vs-pathological recoverability margin.
        let mean_shipped_recoverability = voices
            .iter()
            .map(|v| recoverability(&v.account))
            .sum::<f64>()
            / voices.len() as f64;
        assert!(
            mean_shipped_recoverability >= recoverability_pathological + 0.25,
            "seed {seed}: mean shipped recoverability {mean_shipped_recoverability} must be \
             >= pathological recoverability {recoverability_pathological} + 0.25"
        );

        // C2 (band) / C3 (non-vacuity): mean pairwise distinctiveness among
        // the shipped voices' real accounts.
        let shipped_accounts: Vec<&Account> = voices.iter().map(|v| &v.account).collect();
        let mean_distinctiveness = mean_pairwise_distinctiveness(&shipped_accounts);
        assert!(
            mean_distinctiveness > 0.0,
            "seed {seed}: mean pairwise shipped distinctiveness must be > 0 (non-vacuity), \
             got {mean_distinctiveness}"
        );
        if mean_distinctiveness >= 0.05 {
            band_achieved = true;
        }

        let shipped_losses: Vec<f64> = voices.iter().map(|v| loss_fraction(&v.account)).collect();
        let shipped_recoverabilities: Vec<f64> =
            voices.iter().map(|v| recoverability(&v.account)).collect();
        report.push(format!(
            "seed {seed}: voices={:?} loss_null={loss_null} loss_shipped={shipped_losses:?} \
             loss_pathological={loss_pathological} recoverability_null={recoverability_null} \
             recoverability_shipped={shipped_recoverabilities:?} \
             recoverability_pathological={recoverability_pathological} \
             mean_shipped_recoverability={mean_shipped_recoverability} \
             null_pairwise_distinctiveness=0.0 (C1c, verified exact) \
             mean_shipped_distinctiveness={mean_distinctiveness} \
             band_0.05_cleared={}",
            voices.iter().map(|v| v.kind.clone()).collect::<Vec<_>>(),
            mean_distinctiveness >= 0.05,
        ));
    }

    eprintln!("the-dial known-groups measurement, per seed:");
    for line in &report {
        eprintln!("  {line}");
    }

    // The Concordance anti-vacuity lesson: a criterion over "measured
    // multi-people worlds" is meaningless if none survive the skip.
    assert!(
        multi_people_worlds >= 2,
        "at least two of the measured seeds must place >= 2 cultures, got {multi_people_worlds}"
    );

    // C2's band: at least one measured multi-people world must clear 0.05
    // mean pairwise distinctiveness.
    assert!(
        band_achieved,
        "at least one measured multi-people world must clear the 0.05 mean \
         pairwise distinctiveness band"
    );
}

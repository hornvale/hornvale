//! Emergent social structure: which caste/role strata a settlement grows,
//! from its subsistence, surplus (fertility × water), population scale, and
//! threat. A lean forager camp is nearly egalitarian; a rich, populous farm
//! town differentiates into a full ladder. The role vocabulary stays goblin;
//! what varies is which rungs exist (replacing the tier-0 fixed ladder).

use crate::subsistence::Subsistence;

/// The environmental pressures that shape a settlement's structure.
/// type-audit: bare-ok(ratio: surplus), bare-ok(count: population), bare-ok(ratio: threat)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EnvSummary {
    /// The settlement's subsistence mode.
    pub subsistence: Subsistence,
    /// Surplus in `[0, 1]` (fertility × water availability).
    pub surplus: f64,
    /// Population.
    pub population: u32,
    /// Threat in `[0, 1]` (tectonic unrest / frontier pressure).
    pub threat: f64,
}

/// Role words a species uses for the rungs (pre-language stopgap; The
/// Tongues deletes this). `worker_override: None` = the subsistence word.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct RoleVocabulary {
    /// Worker-rung override ("digger"); `None` = subsistence worker word.
    pub worker_override: Option<String>,
    /// Warrior-rung word.
    pub warrior: String,
    /// Artisan-rung word.
    pub artisan: String,
    /// Shaman-rung word.
    pub shaman: String,
    /// Top-rung word.
    pub top: String,
}

impl Default for RoleVocabulary {
    fn default() -> Self {
        RoleVocabulary {
            worker_override: None,
            warrior: "warrior".to_string(),
            artisan: "artisan".to_string(),
            shaman: "shaman".to_string(),
            top: "chief".to_string(),
        }
    }
}

/// The psychology a settlement's people bring to structure formation,
/// assembled at the composition root from the species domain (culture never
/// imports species). `Default` is the goblin baseline: every modulation
/// below is the identity function at it.
/// type-audit: bare-ok(ratio: threat_response), bare-ok(ratio: time_horizon), bare-ok(flag: communal), bare-ok(flag: rank_status)
#[derive(Clone, Debug, PartialEq)]
pub struct PsychSummary {
    /// Flee 0 ↔ stand 1; 0.5 baseline.
    pub threat_response: f64,
    /// Planning depth; 0.5 baseline.
    pub time_horizon: f64,
    /// Communal (true) suppresses singular chieftainship.
    pub communal: bool,
    /// Rank-based status (true) is what makes the slave rung possible.
    pub rank_status: bool,
    /// The species' role words.
    pub vocabulary: RoleVocabulary,
}

impl Default for PsychSummary {
    fn default() -> Self {
        PsychSummary {
            threat_response: 0.5,
            time_horizon: 0.5,
            communal: false,
            rank_status: true,
            vocabulary: RoleVocabulary::default(),
        }
    }
}

/// The ordered role list (lowest → highest) a settlement grows, from its
/// environment modulated by its people's psychology (spec §5). Every
/// modulation is identity at the goblin baseline.
/// type-audit: bare-ok(identifier-text)
pub fn structure(env: &EnvSummary, psych: &PsychSummary) -> Vec<String> {
    let invest = 1.5 - psych.time_horizon; // 1.0 at baseline
    let mut roles: Vec<String> = Vec::new();
    if psych.rank_status && env.surplus > 0.6 && env.population > 300 {
        roles.push("slave".to_string());
    }
    roles.push(match &psych.vocabulary.worker_override {
        Some(w) => w.clone(),
        None => env.subsistence.worker().to_string(),
    });
    if env.threat > 0.4 * (1.5 - psych.threat_response) {
        roles.push(psych.vocabulary.warrior.clone());
    }
    if env.surplus > 0.6 * invest && f64::from(env.population) > 200.0 * invest {
        roles.push(psych.vocabulary.artisan.clone());
    }
    if env.surplus > 0.4 {
        roles.push(psych.vocabulary.shaman.clone());
    }
    roles.push(psych.vocabulary.top.clone());
    roles
}

#[cfg(test)]
mod tests {
    use super::*;

    fn env(sub: Subsistence, surplus: f64, pop: u32, threat: f64) -> EnvSummary {
        EnvSummary {
            subsistence: sub,
            surplus,
            population: pop,
            threat,
        }
    }

    #[test]
    fn lean_forager_camp_is_nearly_egalitarian() {
        let s = structure(
            &env(Subsistence::Foraging, 0.1, 30, 0.1),
            &PsychSummary::default(),
        );
        assert_eq!(s, vec!["forager".to_string(), "chief".to_string()]);
    }

    #[test]
    fn rich_populous_farm_town_is_stratified() {
        let s = structure(
            &env(Subsistence::Farming, 0.8, 500, 0.1),
            &PsychSummary::default(),
        );
        assert_eq!(
            s,
            vec!["slave", "farmer", "artisan", "shaman", "chief"]
                .into_iter()
                .map(String::from)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn a_frontier_settlement_grows_warriors() {
        let s = structure(
            &env(Subsistence::Herding, 0.3, 120, 0.7),
            &PsychSummary::default(),
        );
        assert!(s.contains(&"warrior".to_string()));
        assert!(
            !s.contains(&"shaman".to_string()),
            "low surplus: no priest caste"
        );
        assert_eq!(s.first().unwrap(), "herder");
        assert_eq!(s.last().unwrap(), "chief");
    }

    #[test]
    fn structure_always_has_a_worker_and_a_chief() {
        for sub in [
            Subsistence::Farming,
            Subsistence::Herding,
            Subsistence::Fishing,
            Subsistence::Foraging,
        ] {
            let s = structure(&env(sub, 0.0, 10, 0.0), &PsychSummary::default());
            assert_eq!(s.len(), 2);
            assert_eq!(s[0], sub.worker());
            assert_eq!(s[1], "chief");
        }
    }

    #[test]
    fn baseline_psych_reproduces_the_pre_species_table_exactly() {
        // The identity property (spec §2.2): at the goblin baseline the
        // modulated table equals the original on a dense grid.
        let base = PsychSummary::default();
        for sub in [
            Subsistence::Farming,
            Subsistence::Herding,
            Subsistence::Fishing,
            Subsistence::Foraging,
        ] {
            for surplus10 in 0..=10 {
                for pop in [10u32, 150, 201, 250, 301, 500] {
                    for threat10 in 0..=10 {
                        let env = EnvSummary {
                            subsistence: sub,
                            surplus: f64::from(surplus10) / 10.0,
                            population: pop,
                            threat: f64::from(threat10) / 10.0,
                        };
                        let expected = original_structure_for_test(&env);
                        assert_eq!(structure(&env, &base), expected, "diverged at {env:?}");
                    }
                }
            }
        }
    }

    /// The pre-species table, frozen verbatim as the identity oracle.
    fn original_structure_for_test(env: &EnvSummary) -> Vec<String> {
        let mut roles: Vec<String> = Vec::new();
        if env.surplus > 0.6 && env.population > 300 {
            roles.push("slave".to_string());
        }
        roles.push(env.subsistence.worker().to_string());
        if env.threat > 0.4 {
            roles.push("warrior".to_string());
        }
        if env.surplus > 0.6 && env.population > 200 {
            roles.push("artisan".to_string());
        }
        if env.surplus > 0.4 {
            roles.push("shaman".to_string());
        }
        roles.push("chief".to_string());
        roles
    }

    #[test]
    fn communal_knowledge_peoples_never_enslave_and_top_out_collectively() {
        let kobold = PsychSummary {
            threat_response: 0.8,
            time_horizon: 0.8,
            communal: true,
            rank_status: false,
            vocabulary: RoleVocabulary {
                worker_override: Some("digger".to_string()),
                warrior: "warden".to_string(),
                artisan: "shaper".to_string(),
                shaman: "keeper".to_string(),
                top: "elders".to_string(),
            },
        };
        let env = EnvSummary {
            subsistence: Subsistence::Farming,
            surplus: 0.9,
            population: 1000,
            threat: 0.5,
        };
        let s = structure(&env, &kobold);
        assert!(!s.contains(&"slave".to_string()));
        assert_eq!(s.first().map(String::as_str), Some("digger"));
        assert_eq!(s.last().map(String::as_str), Some("elders"));
        assert!(s.contains(&"warden".to_string()), "0.5 > 0.4·0.7 = 0.28");
        assert!(
            s.contains(&"shaper".to_string()),
            "long horizon lowers the artisan gates"
        );
    }
}

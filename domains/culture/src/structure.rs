//! Emergent social structure: which caste/role strata a settlement grows,
//! from its subsistence, surplus (fertility × water), population scale, and
//! threat. A lean forager camp is nearly egalitarian; a rich, populous farm
//! town differentiates into a full ladder. The role vocabulary stays goblin;
//! what varies is which rungs exist (replacing the tier-0 fixed ladder).

use crate::subsistence::Subsistence;

/// The environmental pressures that shape a settlement's structure.
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

/// The ordered role list (lowest → highest) a settlement grows. `chief` and
/// the subsistence worker always appear; `slave`/`artisan`/`shaman`/`warrior`
/// appear as surplus, scale, and threat cross thresholds.
pub fn structure(env: &EnvSummary) -> Vec<String> {
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
        let s = structure(&env(Subsistence::Foraging, 0.1, 30, 0.1));
        assert_eq!(s, vec!["forager".to_string(), "chief".to_string()]);
    }

    #[test]
    fn rich_populous_farm_town_is_stratified() {
        let s = structure(&env(Subsistence::Farming, 0.8, 500, 0.1));
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
        let s = structure(&env(Subsistence::Herding, 0.3, 120, 0.7));
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
            let s = structure(&env(sub, 0.0, 10, 0.0));
            assert_eq!(s.len(), 2);
            assert_eq!(s[0], sub.worker());
            assert_eq!(s[1], "chief");
        }
    }
}

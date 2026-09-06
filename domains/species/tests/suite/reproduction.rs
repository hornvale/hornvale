//! The Grammar, Task 1: the species-owned reproductive-affordance contract.
//!
//! These are synthetic contract profiles, not reproductive canon for any
//! shipped kind. The tables at the end make every closed-enum state an
//! explicit coverage decision.

use hornvale_species::{
    AssistanceCapability, CompatibilityContext, CompatibilityOutcome, CompatibilityRule,
    DevelopmentSite, DevelopmentalTiming, GuardStatus, MaterialCompatibility,
    ReproductiveAffordances, ReproductiveOperation, ReproductiveProfile, ReproductiveRole,
    SupportMode, TransitionCapability, compatibility, possible_pathways,
};

fn pairborn() -> ReproductiveAffordances {
    use ReproductiveOperation::{Grow, Join, Make, Release, Support};
    affordances(
        vec![Make, Join, Grow, Support, Release],
        vec![DevelopmentSite::Body],
        vec![SupportMode::Pair],
        vec![
            ReproductiveRole::MaterialProducer,
            ReproductiveRole::MaterialContributor,
            ReproductiveRole::DevelopmentCarrier,
            ReproductiveRole::DevelopmentSupporter,
        ],
        vec![],
    )
}

fn ready_context() -> CompatibilityContext {
    CompatibilityContext {
        available_roles: vec![
            ReproductiveRole::MaterialProducer,
            ReproductiveRole::MaterialContributor,
            ReproductiveRole::DevelopmentCarrier,
            ReproductiveRole::DevelopmentSupporter,
            ReproductiveRole::Host,
            ReproductiveRole::Builder,
        ],
        development_sites: vec![
            DevelopmentSite::Body,
            DevelopmentSite::Egg,
            DevelopmentSite::BroodStructure,
            DevelopmentSite::Colony,
            DevelopmentSite::Host,
            DevelopmentSite::Environment,
            DevelopmentSite::Workshop,
        ],
        support_modes: vec![
            SupportMode::Individual,
            SupportMode::Pair,
            SupportMode::Group,
            SupportMode::Host,
            SupportMode::Environment,
            SupportMode::Artificial,
        ],
        timing: DevelopmentalTiming::Ready,
        environment: GuardStatus::Satisfied,
        resources: GuardStatus::Satisfied,
        assistance: vec![],
        first_to_second: CompatibilityRule {
            material: MaterialCompatibility::Fertile,
            required_assistance: vec![],
        },
        second_to_first: CompatibilityRule {
            material: MaterialCompatibility::Fertile,
            required_assistance: vec![],
        },
    }
}

#[test]
fn pairborn_composes_one_complete_pathway() {
    use ReproductiveOperation::{Grow, Join, Make, Release, Support};
    let paths = possible_pathways(&pairborn(), &ready_context());
    assert_eq!(paths.len(), 1);
    assert_eq!(paths[0].operations, [Make, Join, Grow, Support, Release]);
    assert_eq!(paths[0].development_site, DevelopmentSite::Body);
    assert_eq!(paths[0].support_mode, Some(SupportMode::Pair));
}

#[test]
fn copy_group_host_and_manufacturing_compose_from_capabilities() {
    use ReproductiveOperation::{Build, Convert, Copy, Grow, Join, Make, Release, Support};
    use ReproductiveRole::{
        Builder, DevelopmentCarrier, DevelopmentSupporter, Host, MaterialContributor,
        MaterialProducer,
    };
    let cases = [
        (
            vec![Copy, Grow, Support, Release],
            DevelopmentSite::Body,
            vec![SupportMode::Individual],
            vec![MaterialProducer, DevelopmentCarrier, DevelopmentSupporter],
        ),
        (
            vec![Make, Join, Grow, Support, Release],
            DevelopmentSite::BroodStructure,
            vec![SupportMode::Group],
            vec![MaterialProducer, MaterialContributor, DevelopmentSupporter],
        ),
        (
            vec![Make, Convert, Grow, Release],
            DevelopmentSite::Host,
            vec![],
            vec![MaterialProducer, Host],
        ),
        (
            vec![Build],
            DevelopmentSite::Workshop,
            vec![],
            vec![Builder],
        ),
    ];
    for (operations, site, support, roles) in cases {
        let aff = affordances(
            operations.clone(),
            vec![site],
            support.clone(),
            roles,
            vec![],
        );
        let paths = possible_pathways(&aff, &ready_context());
        assert_eq!(paths.len(), 1, "{aff:?}");
        assert_eq!(paths[0].operations, operations);
        assert_eq!(paths[0].development_site, site);
        assert_eq!(paths[0].support_mode, support.first().copied());
    }
}

#[test]
fn each_natural_transition_is_an_explicit_timing_prerequisite() {
    use ReproductiveOperation::{Change, Grow, Join, Make, Release, Support};
    for transition in [
        TransitionCapability::Maturation,
        TransitionCapability::Metamorphosis,
        TransitionCapability::Seasonal,
        TransitionCapability::SequentialRole,
    ] {
        let base = pairborn();
        let aff = affordances(
            vec![Make, Join, Grow, Support, Release, Change],
            base.development_sites().to_vec(),
            base.support_modes().to_vec(),
            base.roles().to_vec(),
            vec![transition],
        );
        let mut context = ready_context();
        context.timing = DevelopmentalTiming::After(transition);
        let paths = possible_pathways(&aff, &context);
        assert_eq!(
            paths[0].operations,
            [Change, Make, Join, Grow, Support, Release]
        );
        assert_eq!(paths[0].transition, Some(transition));
        assert!(possible_pathways(&base, &context).is_empty());
        assert_eq!(
            possible_pathways(&aff, &ready_context())[0].transition,
            None
        );
    }
}

#[test]
fn every_development_guard_can_block_an_otherwise_possible_pathway() {
    let ready = ready_context();
    let mut blocked = vec![];
    let mut context = ready.clone();
    context.available_roles.clear();
    blocked.push(context);
    let mut context = ready.clone();
    context.development_sites.clear();
    blocked.push(context);
    let mut context = ready.clone();
    context.support_modes.clear();
    blocked.push(context);
    let mut context = ready.clone();
    context.timing = DevelopmentalTiming::Unavailable;
    blocked.push(context);
    let mut context = ready.clone();
    context.environment = GuardStatus::Unsatisfied;
    blocked.push(context);
    let mut context = ready.clone();
    context.resources = GuardStatus::Unsatisfied;
    blocked.push(context);
    for context in blocked {
        assert!(
            possible_pathways(&pairborn(), &context).is_empty(),
            "{context:?}"
        );
        assert_eq!(
            compatibility(&pairborn(), &pairborn(), &context)
                .first_to_second
                .outcome,
            CompatibilityOutcome::Impossible
        );
    }
    assert_eq!(possible_pathways(&pairborn(), &ready).len(), 1);
}

#[test]
fn missing_body_roles_cannot_be_supplied_by_a_context_flag() {
    let base = pairborn();
    for missing in base.roles() {
        let roles = base
            .roles()
            .iter()
            .copied()
            .filter(|role| role != missing)
            .collect();
        let aff = affordances(
            base.operations().to_vec(),
            base.development_sites().to_vec(),
            base.support_modes().to_vec(),
            roles,
            vec![],
        );
        assert!(
            possible_pathways(&aff, &ready_context()).is_empty(),
            "{missing:?}"
        );
    }
}

#[test]
fn empty_invalid_and_incomplete_affordances_produce_no_pathways() {
    use ReproductiveOperation::{Grow, Join, Make, Support};
    let base = pairborn();
    let cases = [
        ReproductiveAffordances::empty(),
        affordances(
            vec![Make, Join, Grow, Support],
            base.development_sites().to_vec(),
            base.support_modes().to_vec(),
            base.roles().to_vec(),
            vec![],
        ),
        affordances(vec![Make, Make], vec![], vec![], vec![], vec![]),
    ];
    for aff in cases {
        assert!(possible_pathways(&aff, &ready_context()).is_empty());
    }
}

#[test]
fn alternatives_have_stable_initiation_site_and_support_order() {
    use ReproductiveOperation::{Copy, Grow, Join, Make, Release, Support};
    let base = pairborn();
    let aff = affordances(
        vec![Release, Support, Grow, Copy, Join, Make],
        vec![DevelopmentSite::Egg, DevelopmentSite::Body],
        vec![SupportMode::Group, SupportMode::Pair],
        base.roles().to_vec(),
        vec![],
    );
    let paths = possible_pathways(&aff, &ready_context());
    let signatures: Vec<_> = paths
        .iter()
        .map(|p| (p.operations[0], p.development_site, p.support_mode))
        .collect();
    assert_eq!(
        signatures,
        vec![
            (Make, DevelopmentSite::Egg, Some(SupportMode::Group)),
            (Make, DevelopmentSite::Egg, Some(SupportMode::Pair)),
            (Make, DevelopmentSite::Body, Some(SupportMode::Group)),
            (Make, DevelopmentSite::Body, Some(SupportMode::Pair)),
            (Copy, DevelopmentSite::Egg, Some(SupportMode::Group)),
            (Copy, DevelopmentSite::Egg, Some(SupportMode::Pair)),
            (Copy, DevelopmentSite::Body, Some(SupportMode::Group)),
            (Copy, DevelopmentSite::Body, Some(SupportMode::Pair)),
        ]
    );
    assert_eq!(possible_pathways(&aff, &ready_context()), paths);
}

#[test]
fn material_relations_distinguish_fertility_sterility_instability_and_impossibility() {
    let cases = [
        (
            MaterialCompatibility::Fertile,
            CompatibilityOutcome::Fertile,
        ),
        (
            MaterialCompatibility::ViableButSterile,
            CompatibilityOutcome::ViableButSterile,
        ),
        (
            MaterialCompatibility::Unstable,
            CompatibilityOutcome::Unstable,
        ),
        (
            MaterialCompatibility::Incompatible,
            CompatibilityOutcome::Impossible,
        ),
    ];
    for (material, expected) in cases {
        let mut context = ready_context();
        context.first_to_second.material = material;
        context.second_to_first.material = material;
        let relation = compatibility(&pairborn(), &pairborn(), &context);
        assert_eq!(relation.first_to_second.outcome, expected);
        assert_eq!(relation.first_to_second, relation.second_to_first);
    }
}

#[test]
fn one_way_material_relation_does_not_infer_reverse_compatibility() {
    let mut context = ready_context();
    context.second_to_first.material = MaterialCompatibility::Incompatible;
    let relation = compatibility(&pairborn(), &pairborn(), &context);
    assert_eq!(
        relation.first_to_second.outcome,
        CompatibilityOutcome::Fertile
    );
    assert_eq!(
        relation.second_to_first.outcome,
        CompatibilityOutcome::Impossible
    );
    assert!(relation.second_to_first.pathways.is_empty());
}

#[test]
fn parental_direction_preserves_specialized_producer_and_developer_roles() {
    use ReproductiveOperation::{Grow, Join, Make, Release, Support};
    let donor = affordances(
        vec![Make],
        vec![],
        vec![],
        vec![
            ReproductiveRole::MaterialProducer,
            ReproductiveRole::MaterialContributor,
        ],
        vec![],
    );
    let receiver = affordances(
        vec![Join, Grow, Support, Release],
        vec![DevelopmentSite::Body],
        vec![SupportMode::Pair],
        vec![
            ReproductiveRole::MaterialContributor,
            ReproductiveRole::DevelopmentCarrier,
            ReproductiveRole::DevelopmentSupporter,
        ],
        vec![],
    );
    let relation = compatibility(&donor, &receiver, &ready_context());
    assert_eq!(
        relation.first_to_second.outcome,
        CompatibilityOutcome::Fertile
    );
    assert_eq!(
        relation.first_to_second.pathways[0].operations,
        [Make, Join, Grow, Support, Release]
    );
    assert_eq!(
        relation.second_to_first.outcome,
        CompatibilityOutcome::Impossible
    );
    let swapped = compatibility(&receiver, &donor, &ready_context());
    assert_eq!(swapped.second_to_first, relation.first_to_second);
}

#[test]
fn assistance_is_required_observable_and_never_repairs_missing_body_operations() {
    for capability in [
        AssistanceCapability::Developmental,
        AssistanceCapability::Magic,
    ] {
        let mut context = ready_context();
        context.first_to_second.required_assistance = vec![capability];
        let absent = compatibility(&pairborn(), &pairborn(), &context);
        assert_eq!(
            absent.first_to_second.outcome,
            CompatibilityOutcome::Impossible
        );
        assert_eq!(absent.first_to_second.missing_assistance, [capability]);
        context.assistance.push(capability);
        let present = compatibility(&pairborn(), &pairborn(), &context);
        assert_eq!(
            present.first_to_second.outcome,
            match capability {
                AssistanceCapability::Developmental =>
                    CompatibilityOutcome::Assisted(MaterialCompatibility::Fertile),
                AssistanceCapability::Magic =>
                    CompatibilityOutcome::MagicOnly(MaterialCompatibility::Fertile),
            }
        );
        assert_eq!(present.first_to_second.required_assistance, [capability]);
        assert!(present.first_to_second.missing_assistance.is_empty());
        assert_eq!(
            present.second_to_first.outcome,
            CompatibilityOutcome::Fertile
        );
        assert_eq!(
            compatibility(&ReproductiveAffordances::empty(), &pairborn(), &context)
                .first_to_second
                .outcome,
            CompatibilityOutcome::Impossible
        );
    }
}

#[test]
fn magic_capability_does_not_substitute_for_developmental_assistance() {
    let mut context = ready_context();
    context.first_to_second.required_assistance = vec![
        AssistanceCapability::Developmental,
        AssistanceCapability::Magic,
    ];
    context.assistance = vec![AssistanceCapability::Magic];
    let relation = compatibility(&pairborn(), &pairborn(), &context);
    assert_eq!(
        relation.first_to_second.outcome,
        CompatibilityOutcome::Impossible
    );
    assert_eq!(
        relation.first_to_second.missing_assistance,
        [AssistanceCapability::Developmental]
    );
}

#[test]
fn copy_only_profiles_do_not_become_hybrids_when_material_is_declared_fertile() {
    let aff = affordances(
        vec![
            ReproductiveOperation::Copy,
            ReproductiveOperation::Grow,
            ReproductiveOperation::Release,
        ],
        vec![DevelopmentSite::Environment],
        vec![],
        vec![ReproductiveRole::MaterialProducer],
        vec![],
    );
    assert_eq!(possible_pathways(&aff, &ready_context()).len(), 1);
    assert_eq!(
        compatibility(&aff, &aff, &ready_context())
            .first_to_second
            .outcome,
        CompatibilityOutcome::Impossible
    );
}

#[test]
fn possibility_profile_needs_no_event_or_frequency_and_typicality_cannot_filter_it() {
    #[derive(Debug, PartialEq, Eq)]
    enum TypicalityInput {
        Unspecified,
        Seasonal,
    }
    let paths = possible_pathways(&pairborn(), &ready_context());
    let mut profile = ReproductiveProfile {
        pathways: paths.clone(),
        typicality: TypicalityInput::Unspecified,
    };
    profile.typicality = TypicalityInput::Seasonal;
    assert_eq!(profile.pathways, paths);
    assert_eq!(profile.typicality, TypicalityInput::Seasonal);
}

fn affordances(
    operations: Vec<ReproductiveOperation>,
    development_sites: Vec<DevelopmentSite>,
    support_modes: Vec<SupportMode>,
    roles: Vec<ReproductiveRole>,
    transitions: Vec<TransitionCapability>,
) -> ReproductiveAffordances {
    ReproductiveAffordances::new(
        operations,
        development_sites,
        support_modes,
        roles,
        transitions,
    )
}

#[test]
fn no_reproductive_pathway_is_a_valid_affordance() {
    let profile = ReproductiveAffordances::empty();

    assert_eq!(profile.validate(), Ok(()));
    assert!(profile.operations().is_empty());
    assert!(profile.development_sites().is_empty());
    assert!(profile.support_modes().is_empty());
    assert!(profile.roles().is_empty());
    assert!(profile.transitions().is_empty());
}

#[test]
fn operation_capabilities_preserve_authored_order_and_can_offer_multiple_pathways() {
    use ReproductiveOperation::{Build, Change, Convert, Copy, Grow, Join, Make, Release, Support};

    let operations = vec![
        Make, Join, Grow, Support, Release, Copy, Change, Build, Convert,
    ];
    let profile = affordances(
        operations.clone(),
        vec![DevelopmentSite::Body, DevelopmentSite::Workshop],
        vec![SupportMode::Individual],
        vec![
            ReproductiveRole::MaterialProducer,
            ReproductiveRole::MaterialContributor,
            ReproductiveRole::DevelopmentCarrier,
            ReproductiveRole::DevelopmentSupporter,
            ReproductiveRole::Host,
            ReproductiveRole::Builder,
        ],
        vec![TransitionCapability::SequentialRole],
    );

    assert_eq!(profile.validate(), Ok(()));
    assert_eq!(profile.operations(), operations);
    assert!(profile.allows(Make));
    assert!(profile.allows(Join));
    assert!(profile.allows(Copy));
}

#[test]
fn every_populated_accessor_preserves_authored_order() {
    let development_sites = vec![DevelopmentSite::Workshop, DevelopmentSite::Body];
    let support_modes = vec![SupportMode::Group, SupportMode::Individual];
    let roles = vec![ReproductiveRole::Builder, ReproductiveRole::Host];

    let profile = affordances(
        vec![ReproductiveOperation::Grow, ReproductiveOperation::Support],
        development_sites.clone(),
        support_modes.clone(),
        roles.clone(),
        vec![
            TransitionCapability::Seasonal,
            TransitionCapability::Maturation,
        ],
    );

    assert_eq!(profile.development_sites(), development_sites);
    assert_eq!(profile.support_modes(), support_modes);
    assert_eq!(profile.roles(), roles);
}

#[test]
fn site_and_support_are_explicit_not_inferred_from_a_readable_category() {
    use ReproductiveOperation::{Grow, Release, Support};

    // Both profiles have the operations commonly summarized as
    // "live-bearing". Their actual site and support requirements differ and
    // are carried by the contract rather than inferred from that label.
    let body_supported = affordances(
        vec![Grow, Support, Release],
        vec![DevelopmentSite::Body],
        vec![SupportMode::Individual],
        vec![
            ReproductiveRole::DevelopmentCarrier,
            ReproductiveRole::DevelopmentSupporter,
        ],
        vec![],
    );
    let host_supported = affordances(
        vec![Grow, Support, Release],
        vec![DevelopmentSite::Host],
        vec![SupportMode::Host],
        vec![ReproductiveRole::Host],
        vec![],
    );

    assert_eq!(body_supported.operations(), host_supported.operations());
    assert_ne!(
        body_supported.development_sites(),
        host_supported.development_sites()
    );
    assert_ne!(
        body_supported.support_modes(),
        host_supported.support_modes()
    );
}

#[test]
fn natural_transition_capability_has_no_social_gender_or_identity_input() {
    let profile = affordances(
        vec![ReproductiveOperation::Change],
        vec![],
        vec![],
        vec![ReproductiveRole::MaterialContributor],
        vec![
            TransitionCapability::Maturation,
            TransitionCapability::Metamorphosis,
            TransitionCapability::Seasonal,
            TransitionCapability::SequentialRole,
        ],
    );

    assert_eq!(profile.validate(), Ok(()));
    assert_eq!(
        profile.transitions(),
        [
            TransitionCapability::Maturation,
            TransitionCapability::Metamorphosis,
            TransitionCapability::Seasonal,
            TransitionCapability::SequentialRole,
        ]
    );
}

#[test]
fn grow_requires_an_explicit_development_site() {
    let profile = affordances(
        vec![ReproductiveOperation::Grow],
        vec![],
        vec![],
        vec![],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("grow requires at least one development site")
    );
}

#[test]
fn build_requires_an_explicit_development_site() {
    let profile = affordances(
        vec![ReproductiveOperation::Build],
        vec![],
        vec![],
        vec![ReproductiveRole::Builder],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("build requires at least one development site")
    );
}

#[test]
fn development_sites_require_a_development_operation() {
    let profile = affordances(
        vec![ReproductiveOperation::Make],
        vec![DevelopmentSite::Egg],
        vec![],
        vec![ReproductiveRole::MaterialProducer],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("development sites require the grow or build operation")
    );
}

#[test]
fn support_requires_an_explicit_support_mode() {
    let profile = affordances(
        vec![ReproductiveOperation::Support],
        vec![],
        vec![],
        vec![],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("support requires at least one support mode")
    );
}

#[test]
fn support_modes_require_the_support_operation() {
    let profile = affordances(
        vec![ReproductiveOperation::Make],
        vec![],
        vec![SupportMode::Group],
        vec![ReproductiveRole::MaterialProducer],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("support modes require the support operation")
    );
}

#[test]
fn change_requires_an_explicit_transition_capability() {
    let profile = affordances(
        vec![ReproductiveOperation::Change],
        vec![],
        vec![],
        vec![ReproductiveRole::MaterialContributor],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("change requires at least one transition capability")
    );
}

#[test]
fn transition_capabilities_require_the_change_operation() {
    let profile = affordances(
        vec![ReproductiveOperation::Make],
        vec![],
        vec![],
        vec![ReproductiveRole::MaterialProducer],
        vec![TransitionCapability::Metamorphosis],
    );

    assert_eq!(
        profile.validate(),
        Err("transition capabilities require the change operation")
    );
}

#[test]
fn reproductive_roles_require_a_declared_operation() {
    let profile = affordances(
        vec![],
        vec![],
        vec![],
        vec![ReproductiveRole::MaterialProducer],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("reproductive roles require at least one operation")
    );
}

#[test]
fn duplicate_capabilities_are_rejected_instead_of_obscuring_authored_order() {
    let profile = affordances(
        vec![ReproductiveOperation::Copy, ReproductiveOperation::Copy],
        vec![],
        vec![],
        vec![],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("reproductive operations must not contain duplicates")
    );
}

#[test]
fn duplicate_development_sites_are_rejected() {
    let profile = affordances(
        vec![ReproductiveOperation::Make],
        vec![DevelopmentSite::Egg, DevelopmentSite::Egg],
        vec![],
        vec![],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("development sites must not contain duplicates")
    );
}

#[test]
fn duplicate_support_modes_are_rejected() {
    let profile = affordances(
        vec![ReproductiveOperation::Make],
        vec![],
        vec![SupportMode::Group, SupportMode::Group],
        vec![],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("support modes must not contain duplicates")
    );
}

#[test]
fn duplicate_reproductive_roles_are_rejected() {
    let profile = affordances(
        vec![],
        vec![],
        vec![],
        vec![ReproductiveRole::Builder, ReproductiveRole::Builder],
        vec![],
    );

    assert_eq!(
        profile.validate(),
        Err("reproductive roles must not contain duplicates")
    );
}

#[test]
fn duplicate_transition_capabilities_are_rejected() {
    let profile = affordances(
        vec![ReproductiveOperation::Make],
        vec![],
        vec![],
        vec![],
        vec![
            TransitionCapability::Seasonal,
            TransitionCapability::Seasonal,
        ],
    );

    assert_eq!(
        profile.validate(),
        Err("transition capabilities must not contain duplicates")
    );
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Rung {
    /// Exercised by a behavior test above.
    Exercised,
    /// Publicly declared for a future grammar branch; listed here on purpose.
    Declared,
}

fn operation_state(operation: ReproductiveOperation) -> (&'static str, Rung) {
    use ReproductiveOperation::{Build, Change, Convert, Copy, Grow, Join, Make, Release, Support};
    match operation {
        Make => ("make", Rung::Exercised),
        Join => ("join", Rung::Exercised),
        Grow => ("grow", Rung::Exercised),
        Support => ("support", Rung::Exercised),
        Release => ("release", Rung::Exercised),
        Copy => ("copy", Rung::Exercised),
        Change => ("change", Rung::Exercised),
        Build => ("build", Rung::Exercised),
        Convert => ("convert", Rung::Exercised),
    }
}

fn development_site_state(site: DevelopmentSite) -> (&'static str, Rung) {
    use DevelopmentSite::{Body, BroodStructure, Colony, Egg, Environment, Host, Workshop};
    match site {
        Body => ("body", Rung::Exercised),
        Egg => ("egg", Rung::Declared),
        BroodStructure => ("brood-structure", Rung::Declared),
        Colony => ("colony", Rung::Declared),
        Host => ("host", Rung::Exercised),
        Environment => ("environment", Rung::Declared),
        Workshop => ("workshop", Rung::Exercised),
    }
}

fn support_mode_state(mode: SupportMode) -> (&'static str, Rung) {
    use SupportMode::{Artificial, Environment, Group, Host, Individual, Pair};
    match mode {
        Individual => ("individual", Rung::Exercised),
        Pair => ("pair", Rung::Declared),
        Group => ("group", Rung::Declared),
        Host => ("host", Rung::Exercised),
        Environment => ("environment", Rung::Declared),
        Artificial => ("artificial", Rung::Declared),
    }
}

fn reproductive_role_state(role: ReproductiveRole) -> (&'static str, Rung) {
    use ReproductiveRole::{
        Builder, DevelopmentCarrier, DevelopmentSupporter, Host, MaterialContributor,
        MaterialProducer,
    };
    match role {
        MaterialProducer => ("material-producer", Rung::Exercised),
        MaterialContributor => ("material-contributor", Rung::Exercised),
        DevelopmentCarrier => ("development-carrier", Rung::Exercised),
        DevelopmentSupporter => ("development-supporter", Rung::Exercised),
        Host => ("host", Rung::Exercised),
        Builder => ("builder", Rung::Exercised),
    }
}

fn transition_state(capability: TransitionCapability) -> (&'static str, Rung) {
    use TransitionCapability::{Maturation, Metamorphosis, Seasonal, SequentialRole};
    match capability {
        Maturation => ("maturation", Rung::Exercised),
        Metamorphosis => ("metamorphosis", Rung::Exercised),
        Seasonal => ("seasonal", Rung::Exercised),
        SequentialRole => ("sequential-role", Rung::Exercised),
    }
}

#[test]
fn every_declared_enum_state_has_an_explicit_coverage_rung() {
    use DevelopmentSite::{Body, BroodStructure, Colony, Egg, Environment, Host, Workshop};
    use ReproductiveOperation::{Build, Change, Convert, Copy, Grow, Join, Make, Release, Support};
    use ReproductiveRole::{
        Builder, DevelopmentCarrier, DevelopmentSupporter, Host as HostRole, MaterialContributor,
        MaterialProducer,
    };
    use SupportMode::{
        Artificial, Environment as EnvironmentalSupport, Group, Host as HostSupport, Individual,
        Pair,
    };
    use TransitionCapability::{Maturation, Metamorphosis, Seasonal, SequentialRole};

    let operations = [
        Make, Join, Grow, Support, Release, Copy, Change, Build, Convert,
    ]
    .map(operation_state);
    let sites = [
        Body,
        Egg,
        BroodStructure,
        Colony,
        Host,
        Environment,
        Workshop,
    ]
    .map(development_site_state);
    let support_modes = [
        Individual,
        Pair,
        Group,
        HostSupport,
        EnvironmentalSupport,
        Artificial,
    ]
    .map(support_mode_state);
    let roles = [
        MaterialProducer,
        MaterialContributor,
        DevelopmentCarrier,
        DevelopmentSupporter,
        HostRole,
        Builder,
    ]
    .map(reproductive_role_state);
    let transitions = [Maturation, Metamorphosis, Seasonal, SequentialRole].map(transition_state);

    assert_eq!(operations.len(), 9);
    assert_eq!(sites.len(), 7);
    assert_eq!(support_modes.len(), 6);
    assert_eq!(roles.len(), 6);
    assert_eq!(transitions.len(), 4);
    assert!(
        operations
            .iter()
            .chain(sites.iter())
            .chain(support_modes.iter())
            .chain(roles.iter())
            .chain(transitions.iter())
            .all(|(name, _)| !name.is_empty())
    );
}

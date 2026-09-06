//! The Grammar, Task 1: the species-owned reproductive-affordance contract.
//!
//! These are synthetic contract profiles, not reproductive canon for any
//! shipped kind. The tables at the end make every closed-enum state an
//! explicit coverage decision.

use hornvale_species::{
    DevelopmentSite, ReproductiveAffordances, ReproductiveOperation, ReproductiveRole, SupportMode,
    TransitionCapability,
};

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

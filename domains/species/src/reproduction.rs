//! Species-owned reproductive affordances.
//!
//! This module is the narrow projection between a future detailed body plan
//! and the reproductive grammar. It exposes capabilities and requirements,
//! not anatomy, social gender, personal identity, typicality, or realized
//! reproductive history.

/// A causal operation a body plan can participate in.
///
/// These are grammar primitives, not readable reproductive categories. A
/// category such as live-bearing is derived later from several operations and
/// explicit development/support requirements.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReproductiveOperation {
    /// Create reproductive material, a bud, a spore, or a template.
    Make,
    /// Combine compatible reproductive inputs.
    Join,
    /// Develop an organism from an input.
    Grow,
    /// Carry, nourish, brood, or otherwise sustain development.
    Support,
    /// Separate offspring from a parent, host, or group.
    Release,
    /// Replicate without combining distinct inputs.
    Copy,
    /// Move an organism between developmental or reproductive states.
    Change,
    /// Manufacture a body.
    Build,
    /// Use another organism as developmental substrate.
    Convert,
}

/// A site at which a new organism can develop.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DevelopmentSite {
    /// Within or on a supporting body.
    Body,
    /// Within a separately released egg or equivalent capsule.
    Egg,
    /// Within a constructed nest, brood chamber, or equivalent structure.
    BroodStructure,
    /// Within a shared colonial body or matrix.
    Colony,
    /// Within or on another organism used as a host.
    Host,
    /// Directly in a suitable environment without a dedicated carrier.
    Environment,
    /// Within a workshop or other manufacturing site.
    Workshop,
}

/// The source of support required while an organism develops.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SupportMode {
    /// Support supplied by one body acting alone.
    Individual,
    /// Support jointly supplied by a pair.
    Pair,
    /// Support supplied by a group or colony.
    Group,
    /// Support supplied by a host organism.
    Host,
    /// Support supplied by ambient environmental conditions.
    Environment,
    /// Support supplied by an artificial process or apparatus.
    Artificial,
}

/// A body-level role that may be required by a reproductive pathway.
///
/// Roles name causal contributions only. They carry no social gender,
/// identity, presentation, kinship, or institutional meaning.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReproductiveRole {
    /// Produces reproductive material, a bud, a spore, or a template.
    MaterialProducer,
    /// Contributes material that can join with another compatible input.
    MaterialContributor,
    /// Carries or contains a developing organism.
    DevelopmentCarrier,
    /// Sustains development without necessarily carrying it.
    DevelopmentSupporter,
    /// Serves as another organism's developmental substrate.
    Host,
    /// Manufactures a body.
    Builder,
}

/// A natural developmental or reproductive transition a body can undergo.
///
/// Future magical transitions may act on a later realization-layer seam;
/// this enum describes natural body-plan capability only.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransitionCapability {
    /// Mature from one developmental state into another.
    Maturation,
    /// Undergo a body-plan metamorphosis.
    Metamorphosis,
    /// Change state in response to a season or recurring environment.
    Seasonal,
    /// Move naturally between reproductive roles over a lifetime.
    SequentialRole,
}

/// The ordered reproductive capabilities projected from a species body plan.
///
/// Collection order is authored and preserved exactly. Consumers may inspect
/// the projection through accessors, but cannot reach the future detailed body
/// plan behind it. Empty collections are a valid profile for a non-reproducing
/// or externally manufactured kind.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReproductiveAffordances {
    operations: Vec<ReproductiveOperation>,
    development_sites: Vec<DevelopmentSite>,
    support_modes: Vec<SupportMode>,
    roles: Vec<ReproductiveRole>,
    transitions: Vec<TransitionCapability>,
}

impl ReproductiveAffordances {
    /// Construct an affordance projection while preserving authored order.
    ///
    /// Construction does not hide invalid authoring. Call [`Self::validate`]
    /// at an authoring or composition boundary to receive a descriptive
    /// contradiction error.
    pub fn new(
        operations: Vec<ReproductiveOperation>,
        development_sites: Vec<DevelopmentSite>,
        support_modes: Vec<SupportMode>,
        roles: Vec<ReproductiveRole>,
        transitions: Vec<TransitionCapability>,
    ) -> Self {
        Self {
            operations,
            development_sites,
            support_modes,
            roles,
            transitions,
        }
    }

    /// Construct the valid empty profile used by a non-reproducing or
    /// externally manufactured kind.
    pub fn empty() -> Self {
        Self::new(Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new())
    }

    /// The causal operations this body plan permits, in authored order.
    pub fn operations(&self) -> &[ReproductiveOperation] {
        &self.operations
    }

    /// Whether this body plan permits `operation`.
    /// type-audit: bare-ok(flag: return)
    pub fn allows(&self, operation: ReproductiveOperation) -> bool {
        self.operations.contains(&operation)
    }

    /// The possible development sites, in authored order.
    pub fn development_sites(&self) -> &[DevelopmentSite] {
        &self.development_sites
    }

    /// The available support modes, in authored order.
    pub fn support_modes(&self) -> &[SupportMode] {
        &self.support_modes
    }

    /// The body-level reproductive roles, in authored order.
    pub fn roles(&self) -> &[ReproductiveRole] {
        &self.roles
    }

    /// The natural transition capabilities, in authored order.
    pub fn transitions(&self) -> &[TransitionCapability] {
        &self.transitions
    }

    /// Validate direct contradictions in the affordance projection.
    ///
    /// This deliberately does not compose pathways or decide compatibility;
    /// those belong to the reproductive grammar. It only checks uniqueness
    /// and metadata that is meaningless without its corresponding operation.
    /// type-audit: bare-ok(prose: return)
    pub fn validate(&self) -> Result<(), &'static str> {
        if has_duplicates(&self.operations) {
            return Err("reproductive operations must not contain duplicates");
        }
        if has_duplicates(&self.development_sites) {
            return Err("development sites must not contain duplicates");
        }
        if has_duplicates(&self.support_modes) {
            return Err("support modes must not contain duplicates");
        }
        if has_duplicates(&self.roles) {
            return Err("reproductive roles must not contain duplicates");
        }
        if has_duplicates(&self.transitions) {
            return Err("transition capabilities must not contain duplicates");
        }

        let grows = self.allows(ReproductiveOperation::Grow);
        let builds = self.allows(ReproductiveOperation::Build);
        if grows && self.development_sites.is_empty() {
            return Err("grow requires at least one development site");
        }
        if builds && self.development_sites.is_empty() {
            return Err("build requires at least one development site");
        }
        if !self.development_sites.is_empty() && !grows && !builds {
            return Err("development sites require the grow or build operation");
        }

        let supports = self.allows(ReproductiveOperation::Support);
        if supports && self.support_modes.is_empty() {
            return Err("support requires at least one support mode");
        }
        if !self.support_modes.is_empty() && !supports {
            return Err("support modes require the support operation");
        }

        let changes = self.allows(ReproductiveOperation::Change);
        if changes && self.transitions.is_empty() {
            return Err("change requires at least one transition capability");
        }
        if !self.transitions.is_empty() && !changes {
            return Err("transition capabilities require the change operation");
        }

        if self.operations.is_empty() && !self.roles.is_empty() {
            return Err("reproductive roles require at least one operation");
        }

        Ok(())
    }
}

fn has_duplicates<T: PartialEq>(values: &[T]) -> bool {
    values
        .iter()
        .enumerate()
        .any(|(index, value)| values[..index].contains(value))
}

/// Whether an externally evaluated environmental or resource guard is met.
/// The grammar neither invents costs nor models the surrounding environment.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GuardStatus {
    /// The caller has established the requirement is met.
    Satisfied,
    /// The requirement is not met (including unknown conditions).
    Unsatisfied,
}

/// Developmental timing of the body that will develop the offspring.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DevelopmentalTiming {
    /// No prerequisite body transition is needed.
    Ready,
    /// A declared natural transition is required before reproduction.
    After(TransitionCapability),
    /// No suitable developmental timing is available.
    Unavailable,
}

/// An explicit external assistance capability, never a grammar operation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssistanceCapability {
    /// A developmental intervention supplied outside the body.
    Developmental,
    /// A future magical intervention. No magic is performed by this module.
    Magic,
}

/// Declared compatibility of inherited material for one parental direction.
/// Task 1 does not encode material identity; callers supply this evidence,
/// independently of body-operation availability. No name-pair lookup occurs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MaterialCompatibility {
    /// Viable offspring capable of reproduction.
    Fertile,
    /// Viable offspring without reproductive fertility.
    ViableButSterile,
    /// Unstable development or reduced survival, with no invented rate.
    Unstable,
    /// No compatible inherited-material combination is declared.
    Incompatible,
}

/// Material and assistance requirements for one ordered parental pairing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompatibilityRule {
    /// Material potential if all body and development guards are met.
    pub material: MaterialCompatibility,
    /// All these capabilities are required; magic is not a substitute for
    /// developmental assistance. Author order is preserved in the result.
    pub required_assistance: Vec<AssistanceCapability>,
}

/// Explicit conditions of a possibility query, without events or frequencies.
///
/// Available roles describe a prospective participating population, not named
/// individuals. Each body must also declare the role it contributes. Sites
/// and support modes constrain authored alternatives; resource sufficiency
/// means the caller has checked the costs for the queried alternatives.
/// Conditions apply to the developing body in each direction. Query again
/// with different conditions to inspect another environment or timing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompatibilityContext {
    /// Causal roles available among prospective participants.
    pub available_roles: Vec<ReproductiveRole>,
    /// Sites available in the prospective environment.
    pub development_sites: Vec<DevelopmentSite>,
    /// Available forms of developmental support.
    pub support_modes: Vec<SupportMode>,
    /// Timing or prerequisite natural change of the developing body.
    pub timing: DevelopmentalTiming,
    /// Whether the environmental requirements are satisfied.
    pub environment: GuardStatus,
    /// Whether the resource costs are covered; no quantities are invented.
    pub resources: GuardStatus,
    /// Hypothetically available assistance, not a record of interventions.
    pub assistance: Vec<AssistanceCapability>,
    /// First argument supplies material; second supplies development.
    pub first_to_second: CompatibilityRule,
    /// Second argument supplies material; first supplies development.
    pub second_to_first: CompatibilityRule,
}

/// A complete permitted composition, not a realized birth or transition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReproductivePathway {
    /// Causal composition order. Support overlaps growth; this is not an
    /// event schedule. A prerequisite change precedes reproductive operations.
    pub operations: Vec<ReproductiveOperation>,
    /// The selected development site.
    pub development_site: DevelopmentSite,
    /// Selected support, or none when no support operation is required.
    pub support_mode: Option<SupportMode>,
    /// Natural transition required by developmental timing, if any.
    pub transition: Option<TransitionCapability>,
}

/// Possibilities and caller-owned typicality, kept as distinct values.
///
/// `T` is the later substrate's input type. The grammar never interprets it,
/// manufactures frequencies, or requires a realized event to form a profile.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReproductiveProfile<T> {
    /// What the grammar permits under the queried conditions.
    pub pathways: Vec<ReproductivePathway>,
    /// Separate authored frequency/condition data for later realization.
    pub typicality: T,
}

/// Evaluated outcome in one parental direction; never a social category.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompatibilityOutcome {
    /// Viable and fertile without required external assistance.
    Fertile,
    /// Viable but sterile without required external assistance.
    ViableButSterile,
    /// Unstable or low-survival; no numerical survival rate is implied.
    Unstable,
    /// Requires available developmental assistance; retains material potential.
    Assisted(MaterialCompatibility),
    /// Requires available future magic; retains material potential. This
    /// reports a conditional possibility, never successful execution of magic.
    MagicOnly(MaterialCompatibility),
    /// Material, body, development, or assistance requirements are unmet.
    Impossible,
}

/// One directed result, retaining assistance requirements even when blocked.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectionalCompatibility {
    /// Outcome after all guards have been checked.
    pub outcome: CompatibilityOutcome,
    /// Complete joined pathways available in this direction.
    pub pathways: Vec<ReproductivePathway>,
    /// Explicit assistance requirements, in declared order.
    pub required_assistance: Vec<AssistanceCapability>,
    /// Required capabilities absent from the context, in declared order.
    pub missing_assistance: Vec<AssistanceCapability>,
}

/// Both parental directions, evaluated independently. No symmetry is assumed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompatibilityRelation {
    /// First argument contributes material; second develops the offspring.
    pub first_to_second: DirectionalCompatibility,
    /// Second argument contributes material; first develops the offspring.
    pub second_to_first: DirectionalCompatibility,
}

/// Compose complete pathways permitted by the body's operations and context.
/// Initiations are ordered join, copy, convert, build, followed by authored
/// site and support order. Join here is structural potential; the two-parent
/// material and assistance guards are evaluated by [`compatibility`].
/// Invalid affordances fail closed; use [`ReproductiveAffordances::validate`]
/// at the authoring boundary for diagnostic errors.
pub fn possible_pathways(
    affordances: &ReproductiveAffordances,
    context: &CompatibilityContext,
) -> Vec<ReproductivePathway> {
    compose(affordances, context, InputSource::Own)
}

/// Evaluate material donation and development in both parental directions.
pub fn compatibility(
    first: &ReproductiveAffordances,
    second: &ReproductiveAffordances,
    context: &CompatibilityContext,
) -> CompatibilityRelation {
    CompatibilityRelation {
        first_to_second: directed_compatibility(first, second, context, &context.first_to_second),
        second_to_first: directed_compatibility(second, first, context, &context.second_to_first),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum InputSource {
    Own,
    Donated,
}

fn role_available(
    affordances: &ReproductiveAffordances,
    context: &CompatibilityContext,
    role: ReproductiveRole,
) -> bool {
    affordances.roles().contains(&role) && context.available_roles.contains(&role)
}

fn compose(
    affordances: &ReproductiveAffordances,
    context: &CompatibilityContext,
    input: InputSource,
) -> Vec<ReproductivePathway> {
    use ReproductiveOperation::{Build, Change, Convert, Copy, Grow, Join, Make, Release, Support};
    use ReproductiveRole::{
        Builder, DevelopmentCarrier, DevelopmentSupporter, Host, MaterialContributor,
        MaterialProducer,
    };

    if affordances.validate().is_err()
        || context.environment != GuardStatus::Satisfied
        || context.resources != GuardStatus::Satisfied
    {
        return Vec::new();
    }
    let transition = match context.timing {
        DevelopmentalTiming::Ready => None,
        DevelopmentalTiming::After(transition)
            if affordances.transitions().contains(&transition) =>
        {
            Some(transition)
        }
        DevelopmentalTiming::After(_) | DevelopmentalTiming::Unavailable => return Vec::new(),
    };
    let role = |role| role_available(affordances, context, role);
    let makes = affordances.allows(Make) && role(MaterialProducer);
    let mut paths = Vec::new();
    for initiation in [Join, Copy, Convert, Build] {
        if !affordances.allows(initiation) || (input == InputSource::Donated && initiation != Join)
        {
            continue;
        }
        let initiates = match initiation {
            Join => (makes || input == InputSource::Donated) && role(MaterialContributor),
            Copy => role(MaterialProducer),
            Convert => makes && role(Host),
            Build => role(Builder),
            _ => unreachable!("only initiation operations are enumerated"),
        };
        if !initiates
            || (initiation != Build && (!affordances.allows(Grow) || !affordances.allows(Release)))
        {
            continue;
        }
        for &site in affordances.development_sites() {
            if !context.development_sites.contains(&site)
                || (site == DevelopmentSite::Body && !role(DevelopmentCarrier))
                || (site == DevelopmentSite::Host && !role(Host))
                || (initiation == Convert && site != DevelopmentSite::Host)
            {
                continue;
            }
            let support_modes: Vec<_> = if affordances.allows(Support) {
                affordances
                    .support_modes()
                    .iter()
                    .copied()
                    .map(Some)
                    .collect()
            } else {
                vec![None]
            };
            for support in support_modes {
                if let Some(mode) = support {
                    let supported = match mode {
                        SupportMode::Individual | SupportMode::Pair | SupportMode::Group => {
                            role(DevelopmentSupporter)
                        }
                        SupportMode::Host => role(Host),
                        SupportMode::Environment | SupportMode::Artificial => true,
                    };
                    if !context.support_modes.contains(&mode) || !supported {
                        continue;
                    }
                }
                let mut operations = Vec::new();
                if transition.is_some() {
                    operations.push(Change);
                }
                if matches!(initiation, Join | Convert) {
                    operations.push(Make);
                }
                operations.push(initiation);
                if affordances.allows(Grow) {
                    operations.push(Grow);
                }
                if support.is_some() {
                    operations.push(Support);
                }
                if affordances.allows(Release) {
                    operations.push(Release);
                }
                paths.push(ReproductivePathway {
                    operations,
                    development_site: site,
                    support_mode: support,
                    transition,
                });
            }
        }
    }
    paths
}

fn directed_compatibility(
    donor: &ReproductiveAffordances,
    developer: &ReproductiveAffordances,
    context: &CompatibilityContext,
    rule: &CompatibilityRule,
) -> DirectionalCompatibility {
    let missing_assistance: Vec<_> = rule
        .required_assistance
        .iter()
        .copied()
        .filter(|capability| !context.assistance.contains(capability))
        .collect();
    let can_donate = donor.validate().is_ok()
        && donor.allows(ReproductiveOperation::Make)
        && role_available(donor, context, ReproductiveRole::MaterialProducer)
        && role_available(donor, context, ReproductiveRole::MaterialContributor);
    let pathways = if can_donate
        && rule.material != MaterialCompatibility::Incompatible
        && missing_assistance.is_empty()
    {
        compose(developer, context, InputSource::Donated)
    } else {
        Vec::new()
    };
    let outcome = if pathways.is_empty() {
        CompatibilityOutcome::Impossible
    } else if rule
        .required_assistance
        .contains(&AssistanceCapability::Magic)
    {
        CompatibilityOutcome::MagicOnly(rule.material)
    } else if rule
        .required_assistance
        .contains(&AssistanceCapability::Developmental)
    {
        CompatibilityOutcome::Assisted(rule.material)
    } else {
        match rule.material {
            MaterialCompatibility::Fertile => CompatibilityOutcome::Fertile,
            MaterialCompatibility::ViableButSterile => CompatibilityOutcome::ViableButSterile,
            MaterialCompatibility::Unstable => CompatibilityOutcome::Unstable,
            MaterialCompatibility::Incompatible => CompatibilityOutcome::Impossible,
        }
    };
    DirectionalCompatibility {
        outcome,
        pathways,
        required_assistance: rule.required_assistance.clone(),
        missing_assistance,
    }
}

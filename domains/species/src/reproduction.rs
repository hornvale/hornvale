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

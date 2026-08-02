//! The anthroponymic schema (The Namesake, spec §3.2): what a personal name
//! is made of.
//!
//! A name is an **ordered list of elements**, and each element is a *source*
//! (where its material comes from) paired with an *author* (who conferred
//! it). Those two axes together span the human anthroponymic record —
//! patronymics, clan names, occupational and toponymic bynames, birth-order
//! and generation names, deed-names, teknonyms — and the ergonomic subset of
//! the speculative-fiction record, including the true name, which is simply
//! the element no one authored.
//!
//! This module is plain data and kernel-only. It never learns which people a
//! name belongs to or what its relations resolve to; the composition root
//! derives the pattern and supplies the rendered material, the same
//! discipline [`crate::MorphOptions`] and [`crate::SiteConcepts`] follow.

/// Where a name element's material comes from.
///
/// The variant order is a **save-format contract** where a pattern is
/// serialized by index; add new variants at the end. Deriving `Ord` makes
/// that order an **ordering contract** too — reordering variants silently
/// changes how patterns sort, not just how they serialize.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ElementSource {
    /// Drawn phonology: the given name, a bare stem in the culture's own
    /// sound system.
    Stem,
    /// Compounded concepts from the culture's lexicon — a descriptive
    /// byname.
    Gloss(GlossBasis),
    /// A walk to another entity: the patronymic, the clan name, the
    /// toponymic.
    Relation(Cite),
    /// A position in a sequence: Roman `Quintus`, Balinese `Wayan`, a
    /// Chinese generation character.
    Index(IndexBasis),
    /// An event from the world's committed history — the deed-name.
    Deed,
}

/// What a descriptive byname describes.
///
/// Variant order is a save-format contract (see [`ElementSource`]) and,
/// since `Ord` is derived, an ordering contract as well.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum GlossBasis {
    /// What the bearer does: Smith, Müller, Kovács.
    Trade,
    /// What the bearer is like: Erik the Red, Æthelred the Unready.
    Bearing,
    /// Where the bearer is from, as a quality rather than a named place.
    Origin,
}

/// Which relation a `Relation` element walks to.
///
/// Variant order is a save-format contract (see [`ElementSource`]) and,
/// since `Ord` is derived, an ordering contract as well.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Cite {
    /// The bearer's forebear: the patronymic (Jónsdóttir, ibn, -ovich).
    Parent,
    /// The root of the bearer's descent chain: the clan name (Mac-, Ó-).
    Clan,
    /// The community the bearer belongs to.
    Community,
    /// A named place: the nisba, `da Vinci`.
    Place,
    /// The deity the bearer's community holds foremost — a theophoric name.
    Deity,
    /// The bearer's teacher rather than their parent: the transmission
    /// lineage, the anthroponymic twin of mentorship-distance drift.
    Mentor,
    /// The bearer's **child** — teknonymy, Arabic *Abu Bakr*, "father of
    /// Bakr". Assignment flows backward: the parent is named for the child,
    /// so this element cannot exist until the child does.
    Child,
}

/// Which sequence an `Index` element counts along.
///
/// Variant order is a save-format contract (see [`ElementSource`]) and,
/// since `Ord` is derived, an ordering contract as well.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IndexBasis {
    /// Position among siblings.
    BirthOrder,
    /// Depth in the descent chain.
    Generation,
}

/// Who conferred an element.
///
/// The load-bearing axis: authorship determines whether an element can be
/// revoked, who may confer another, and whether it can be *false*. Variant
/// order is a save-format contract (see [`ElementSource`]) and, since `Ord`
/// is derived, an ordering contract as well.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Author {
    /// Given at birth by family.
    Kin,
    /// Conferred by the community, typically at coming of age.
    Community,
    /// Conferred by those who saw the deed.
    Witnesses,
    /// Conferred by an office: regnal, monastic, caste.
    Institution,
    /// Assumed by the bearer — the only author that can lie.
    Selfward,
    /// Applied from outside and not revocable by the bearer: the exonym, the
    /// epithet, the slur.
    Outsiders,
    /// Conferred by nobody. The name is discovered rather than given — the
    /// true name.
    Inherent,
}

impl Author {
    /// Whether the bearer may drop an element this author conferred.
    ///
    /// Only a self-assumed element is freely revocable. A kin- or
    /// institution-given element is revocable by *that* author, not by the
    /// bearer, which is why they return `false` here.
    /// type-audit: bare-ok(flag: return)
    pub fn revocable_by_bearer(self) -> bool {
        matches!(self, Author::Selfward)
    }
}

/// One element of a personal name.
/// type-audit: bare-ok(count: conferred)
#[derive(Clone, Debug, PartialEq)]
pub struct NameElement {
    /// Where this element's material comes from.
    pub source: ElementSource,
    /// Who conferred it.
    pub author: Author,
    /// The standard year it was conferred; `None` for conferred-at-birth.
    pub conferred: Option<f64>,
}

/// A figure's full name: every element they have accrued, in cultural order.
///
/// The *full* name is rarely what is uttered — see
/// [`crate::anthroponym::render`], which returns the shortest prefix that
/// disambiguates at the scope of the utterance.
#[derive(Clone, Debug, PartialEq)]
pub struct PersonName {
    /// The elements, in the order this culture speaks them.
    pub elements: Vec<NameElement>,
}

/// A culture's naming rule: which elements, from which sources, by which
/// authors, in what order.
///
/// Derived from the culture's `SocietyVector` at the composition root, never
/// authored per-culture — the anti-lookup-table discipline (decision 0021).
#[derive(Clone, Debug, PartialEq)]
pub struct NamePattern {
    /// The elements this culture's names carry, in order.
    pub elements: Vec<(ElementSource, Author)>,
}

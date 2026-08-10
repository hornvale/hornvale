//! World assembly: seed + registry + ledger. A saved world IS a seed
//! plus a ledger (Constitution §2.3); providers are stateless in tier 0
//! and reconstructed at load by the application.

use crate::ledger::Ledger;
use crate::registry::ConceptRegistry;
use crate::seed::Seed;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// The core name predicate, registered by `World::new` for every world.
/// type-audit: bare-ok(identifier-text)
pub const NAME: &str = "name";

/// The classification predicate: `(entity, is-a, <class-label>)`. Functional —
/// an entity has one immutable concept-class (celestial classes: planet, star,
/// moon). Object is a `Value::Text` class label. Distinct from [`INSTANCE_OF`]:
/// `is-a` is a fixed concept-class (contradiction-checked); `instance-of` is a
/// mutable roster-kind (latest-wins). C2 decides their long-term relationship.
/// type-audit: bare-ok(identifier-text)
pub const IS_A: &str = "is-a";

/// The kind an entity is an instance of (object: `Value::Text` kind label).
/// NON-functional: a kind can change over sim time (awakened beast, corpse,
/// lich); each change is a new day-stamped fact and the CURRENT kind is the
/// latest one (`Ledger::kind_of`). Kind references serialize as labels,
/// never positions (metaplan §7).
/// type-audit: bare-ok(identifier-text)
pub const INSTANCE_OF: &str = "instance-of";

/// The glossed meaning of an entity's generated name (functional, Text):
/// what a settlement's or a deity's name-gloss says the name compounds over.
/// Kernel-core (not domain-owned, ecs-c6 T3): it names a fact about the
/// pairing of a generated name with the site facts it compounds over, which
/// no single domain crate owns — a settlement's name-gloss and a deity's
/// name-gloss are the same predicate on disjoint subjects, written by
/// different composition-root stages. Registering it here (rather than in
/// `windows/worldgen`, where it previously lived) lets `single_writer_check`
/// exempt it by [`KERNEL_CORE_PREDICATES`] instead of flagging a false
/// same-predicate conflict between disjoint-subject writers.
/// type-audit: bare-ok(identifier-text)
pub const NAME_GLOSS: &str = "name-gloss";

/// Every predicate `World::new` registers as shared kernel-core
/// infrastructure — writable by more than one system on disjoint subjects,
/// so [`crate::schedule::CapabilitySchema::single_writer_check`] exempts
/// exactly this set rather than flagging a false conflict (ecs-c6 T3, spec
/// §7). Kept in lockstep with `World::new`'s `register_predicate` calls by
/// construction (this literally lists their subjects).
/// type-audit: bare-ok(identifier-text)
pub const KERNEL_CORE_PREDICATES: &[&str] = &[NAME, INSTANCE_OF, NAME_GLOSS];

/// A world is a seed plus everything ever observed about it, plus the record
/// of which versioned seed-derivation labels it was derived under
/// ([`World::derived_under`]).
/// type-audit: bare-ok(identifier-text: derived_under)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct World {
    /// The seed that generated this world.
    pub seed: Seed,
    /// The registry of all concepts (predicates, phenomenon kinds, etc.) in this world.
    pub registry: ConceptRegistry,
    /// The ledger of all facts committed to this world.
    pub ledger: Ledger,
    /// Which versioned seed-derivation labels this world was derived under, as
    /// `label-without-version -> version` (e.g. `room/furnishing -> v1`).
    /// Written by the composition root at save time, because only it can see
    /// every crate's labels (`cli::streams::stamp`); the kernel cannot, since
    /// `hornvale-vessel`'s labels live downstream of it.
    ///
    /// **Metadata about derivation, not derived content.** It exists so a
    /// reload after an epoch can say WHAT moved rather than silently
    /// rearranging someone's memory of a place (Rose Window Amendment 1
    /// §1a.5, which asked for that consequence to be stated rather than
    /// discovered). It commits no fact and mints no entity — deliberately:
    /// entity ids are minted sequentially, so a stamp entity at genesis would
    /// shift every id after it and move every artifact in the project that
    /// names one, for the sake of metadata.
    ///
    /// The keys drop the `/vN` segment so that a bump is a *value* change on a
    /// stable key, which is what lets a diff name the label that moved instead
    /// of reporting one key vanishing and another appearing. Unversioned labels
    /// are absent: they are structural and must never move (decision 0073), so
    /// recording one would add a row that can never differ.
    ///
    /// Empty on any world saved before stamping existed, which is itself the
    /// honest answer for such a world — hence `#[serde(default)]`. An empty
    /// stamp makes no claim, and a diff against it must therefore report
    /// nothing moved rather than reporting everything moved.
    #[serde(default)]
    pub derived_under: std::collections::BTreeMap<String, String>,
}

impl World {
    /// Create an empty world and register kernel-core concepts.
    /// Domains register their own concepts at wiring time.
    pub fn new(seed: Seed) -> World {
        let mut registry = ConceptRegistry::default();
        registry
            .register_predicate(NAME, true, "canonical name of an entity")
            .expect("core concept registration cannot conflict in an empty registry");
        registry
            .register_predicate(IS_A, true, "the class an entity belongs to")
            .expect("core concept registration cannot conflict in an empty registry");
        registry
            .register_predicate(
                INSTANCE_OF,
                false,
                "the kind an entity is an instance of; the latest fact is its current kind",
            )
            .expect("core concept registration cannot conflict in an empty registry");
        registry
            .register_predicate(
                NAME_GLOSS,
                true,
                "the glossed meaning of an entity's generated name",
            )
            .expect("core concept registration cannot conflict in an empty registry");
        World {
            seed,
            registry,
            ledger: Ledger::default(),
            // Unstamped: only the composition root can see every crate's
            // labels, so only it may fill this in (`cli::streams::stamp`).
            derived_under: std::collections::BTreeMap::new(),
        }
    }

    /// Deterministic pretty JSON. This string is the save format and the
    /// determinism-test currency: same world → same bytes.
    /// type-audit: bare-ok(artifact)
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("World serialization cannot fail")
    }

    /// Deserialize a world from JSON.
    ///
    /// There is no load-time identity check here. An entity id is derived
    /// from its `Lineage` (parent, role, ordinal), and lineage is not
    /// persisted — `Ledger::minted` is `#[serde(skip)]` deliberately, so the
    /// save format never widens to carry it. That leaves nothing independent
    /// in a save to verify a fact's subject against: a loaded ledger's
    /// `minted` set is rebuilt FROM the very facts a validity check would
    /// compare it to, which makes such a check trivially true for any
    /// well-formed JSON (an earlier version of this function had exactly
    /// that vacuous check; it was deleted with the-signet Task 2's review
    /// round because a guard that cannot fail reads as protection it does
    /// not provide). The real guard is at mint time, not load time:
    /// `Ledger::mint_entity` panics on a lineage collision, and that guard
    /// demonstrably survives a save/load round trip (see
    /// `Ledger`'s `ledger_serializes_roundtrip_including_minting_state`
    /// test) because `ensure_index` repopulates `minted` from the loaded
    /// facts before the first post-load mint.
    /// type-audit: bare-ok(artifact)
    pub fn from_json(json: &str) -> Result<World, serde_json::Error> {
        let world: World = serde_json::from_str(json)?;
        Ok(world)
    }

    /// Save this world to a JSON file.
    pub fn save(&self, path: &Path) -> std::io::Result<()> {
        std::fs::write(path, self.to_json())
    }

    /// Load a world from a JSON file.
    pub fn load(path: &Path) -> std::io::Result<World> {
        let json = std::fs::read_to_string(path)?;
        World::from_json(&json).map_err(std::io::Error::other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{Fact, Lineage, Value};

    #[test]
    fn new_world_registers_core_concepts() {
        let w = World::new(Seed(42));
        assert!(w.registry.predicate("name").unwrap().functional);
    }

    #[test]
    fn name_constant_is_the_registered_core_predicate() {
        let w = World::new(Seed(1));
        assert!(w.registry.predicate(crate::world::NAME).unwrap().functional);
    }

    #[test]
    fn world_json_roundtrips() {
        let mut w = World::new(Seed(42));
        let e = w.ledger.mint_entity(Lineage {
            parent: None,
            role: "world-roundtrip-subject",
            ordinal: 0,
        });
        w.ledger
            .commit(
                Fact {
                    subject: e,
                    predicate: "name".to_string(),
                    object: Value::Text("Zaggrak".to_string()),
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &w.registry,
            )
            .unwrap();
        let w2 = World::from_json(&w.to_json()).unwrap();
        assert_eq!(w2.seed, Seed(42));
        assert_eq!(w2.ledger.len(), 1);
        assert_eq!(
            w2.ledger.value_of(e, "name"),
            Some(&Value::Text("Zaggrak".to_string()))
        );
    }

    #[test]
    fn world_json_is_deterministic() {
        assert_eq!(World::new(Seed(9)).to_json(), World::new(Seed(9)).to_json());
    }

    #[test]
    fn next_entity_corruption_no_longer_invalidates_a_world() {
        // The campaign's whole point again, seen from the save-format side:
        // `next_entity` used to be the load-time corruption oracle (a fact
        // referencing an id past the counter meant a forged save). Ids are
        // now lineage-derived rather than counter positions, so `next_entity`
        // is a bare accession count. `World::from_json` no longer runs ANY
        // identity check (there is nothing left in the save to check it
        // against — see the doc comment on `from_json`), so mangling
        // `next_entity` must not stop a world from loading, and the real
        // guard — the mint-time collision panic — must still fire correctly
        // afterward.
        let mut w = World::new(Seed(42));
        let lineage = Lineage {
            parent: None,
            role: "corruption-probe-subject",
            ordinal: 0,
        };
        let e = w.ledger.mint_entity(lineage);
        w.ledger
            .commit(
                Fact {
                    subject: e,
                    predicate: "name".to_string(),
                    object: Value::Text("Zaggrak".to_string()),
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &w.registry,
            )
            .unwrap();
        let json = w.to_json();
        // `next_entity` is now a bare accession count (1, after one mint) —
        // NOT the entity id, which is lineage-derived and huge. Mangle the
        // count itself, not `e`'s value.
        let corrupt = json.replacen(
            &format!("\"next_entity\": {}", w.ledger.entity_count()),
            "\"next_entity\": 999",
            1,
        );
        assert_ne!(json, corrupt, "test setup must actually mangle the json");
        let loaded = World::from_json(&corrupt);
        assert!(
            loaded.is_ok(),
            "a mangled next_entity must not invalidate a world: {loaded:?}"
        );
        let mut loaded = loaded.unwrap();
        // And minting still guards correctly after such a load: re-minting the
        // lineage that produced `e` must still collide, proving `minted` was
        // repopulated from the loaded facts rather than trusting the mangled
        // counter.
        let again = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            loaded.ledger.mint_entity(lineage)
        }));
        assert!(
            again.is_err(),
            "a lineage already present in the loaded facts must still collide"
        );
    }

    #[test]
    fn every_world_registers_is_a() {
        let w = World::new(Seed(1));
        assert!(
            w.registry.predicate("is-a").is_some(),
            "is-a must be registered"
        );
        assert!(
            w.registry.predicate("is-a").unwrap().functional,
            "is-a is functional"
        );
    }

    /// Exactly what `serde_json::to_string(&World::new(Seed(42)))` emitted
    /// before `derived_under` existed — captured from a run of the real
    /// serializer on the commit before this one, not hand-written. A
    /// plausible-but-wrong fixture would test nothing.
    const PRE_STAMP_WORLD_JSON: &str = r#"{"seed":42,"registry":{"predicates":{"instance-of":{"name":"instance-of","functional":false,"doc":"the kind an entity is an instance of; the latest fact is its current kind"},"is-a":{"name":"is-a","functional":true,"doc":"the class an entity belongs to"},"name":{"name":"name","functional":true,"doc":"canonical name of an entity"},"name-gloss":{"name":"name-gloss","functional":true,"doc":"the glossed meaning of an entity's generated name"}},"phenomenon_kinds":{},"concepts":{}},"ledger":{"facts":[],"next_entity":0}}"#;

    #[test]
    fn a_world_saved_before_stamps_existed_still_loads() {
        // `#[serde(default)]`, asserted rather than assumed: a world.json
        // written before this campaign must not fail to parse, and an absent
        // stamp is itself informative — that world predates stamping.
        let w = World::from_json(PRE_STAMP_WORLD_JSON);
        assert!(w.is_ok(), "an unstamped world must still load: {w:?}");
        let w = w.unwrap();
        assert_eq!(w.seed, Seed(42));
        assert!(
            w.derived_under.is_empty(),
            "an unstamped world's stamp makes no claim"
        );
    }

    #[test]
    fn the_pre_stamp_fixture_matches_the_real_shape_minus_the_stamp() {
        // Guards the fixture above against becoming a fossil: if the serialized
        // shape of a fresh world changes for any OTHER reason, this fails and
        // the fixture gets re-captured rather than silently testing a shape
        // nothing emits any more.
        let real = serde_json::to_string(&World::new(Seed(42))).unwrap();
        let expected = format!(
            "{},\"derived_under\":{{}}}}",
            PRE_STAMP_WORLD_JSON.strip_suffix('}').unwrap()
        );
        assert_eq!(real, expected);
    }

    #[test]
    fn the_stamp_round_trips_through_json() {
        let mut w = World::new(Seed(42));
        w.derived_under
            .insert("room/furnishing".to_string(), "v1".to_string());
        w.derived_under
            .insert("room/layout/rectilinear".to_string(), "v1".to_string());
        let back = World::from_json(&w.to_json()).unwrap();
        assert_eq!(back.derived_under, w.derived_under);
    }

    #[test]
    fn world_saves_and_loads_from_disk() {
        let dir = std::env::temp_dir().join(format!("hornvale-kernel-test-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("world.json");
        let w = World::new(Seed(42));
        w.save(&path).unwrap();
        let w2 = World::load(&path).unwrap();
        assert_eq!(w2.seed, Seed(42));
        std::fs::remove_dir_all(&dir).unwrap();
    }
}

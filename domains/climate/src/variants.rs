//! Biome **variants** (The Stratum §3.2): the named sub-types of a formation.
//! A savanna is grass or wooded; a temperate forest is old growth, a damp
//! hollow, a gap, or deadfall. The variant is the facet that gives a place its
//! particular character — and, since The Toponym, the one a settlement can be
//! named for.
//!
//! The vocabulary lives here rather than in `windows/locale` because
//! `windows/worldgen` names settlements and `locale` already depends on
//! `worldgen`; the reverse edge would be a cycle. It belongs here on its own
//! terms too: realm, formation and stratum are all climate's, and variant is a
//! facet of the same expression.
//!
//! **The pool below preserves the order and weights the prose pool has always
//! had.** Several entries may share a variant, so the draw is unchanged and
//! every descriptor renders exactly as it did — the epoch this table carries
//! is confined to settlement names.

use crate::facets::{Formation, Stratum};

/// A named sub-type of a formation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Variant {
    /// A sand sea of dunes.
    Erg,
    /// A dry lake bed of salt and cracked clay.
    Playa,
    /// A stony desert pavement of bare rock.
    Hamada,
    /// A desert floor of wind-swept gravel.
    Reg,
    /// Mature forest, closed above and open beneath.
    OldGrowth,
    /// A shaded, wet fold in the forest floor.
    DampHollow,
    /// A break in the canopy where light reaches the ground.
    ForestGap,
    /// Fallen timber going back to moss and lichen.
    MossyDeadfall,
    /// A stand of northern conifers.
    BorealStand,
    /// Waterlogged peat ground in the boreal forest.
    Muskeg,
    /// Ground recovering from fire.
    Burn,
    /// Ground churned and patterned by freezing.
    FrostHeave,
    /// A field of frost-shattered boulders.
    Felsenmeer,
    /// Ground swept bare by wind.
    WindScour,
    /// Open grassland, unbroken by trees.
    GrassSward,
    /// Grassland with scattered trees.
    WoodedGrassland,
    /// Tall closed-canopy tropical forest.
    ClosedCanopy,
    /// Tropical forest tangled with climbing vines.
    LianaForest,
    /// Forest following a watercourse.
    GalleryForest,
    /// An unbroken field of snow.
    Snowfield,
    /// Ice split by crevasses.
    CrevasseField,
    /// Ice swept bare and carved by wind.
    ScouredIce,
    /// Dry scrub of thorned shrubs.
    ThornScrub,
    /// Hard-leaved drought-adapted scrub.
    SclerophyllScrub,
    /// Scrub regrowing after fire.
    FireScrub,
    /// Sea ice buckled into a ridge.
    PressureRidge,
    /// A channel of open water through sea ice.
    IceLead,
    /// Ice floes driven over one another.
    RaftedFloe,
    /// A pool of meltwater on sea ice.
    MeltPond,
    /// A massive coral colony standing proud of the reef.
    CoralHead,
    /// The ribbed seaward face of a reef.
    SpurAndGroove,
    /// Broken coral debris behind a reef.
    ReefRubble,
    /// A thicket of branching coral.
    StaghornStand,
    /// The floating canopy of a kelp forest.
    KelpCanopy,
    /// The anchored base of a kelp forest.
    HoldfastTangle,
    /// Seabed grazed bare of kelp.
    UrchinBarren,
    /// A field of hydrothermal chimneys.
    SmokerField,
    /// Vent fauna crowded around hot water.
    TubewormThicket,
    /// Shimmering hot water rising from a vent.
    VentPlume,
    /// Water thick with plankton.
    PlanktonBloom,
    /// Cold nutrient-rich water rising from below.
    ColdUpwelling,
    /// A dense turning mass of fish.
    BaitBall,
    /// Open sunlit water, far from any shore.
    OpenBlue,
    /// A drifting raft of floating weed.
    SargassumDrift,
    /// A shoal moving as one body.
    FishShoal,
    /// Water at the edge of the light.
    TwilightWater,
    /// The daily-rising layer of small sea life.
    ScatteringLayer,
    /// Water below all light.
    LightlessWater,
    /// Organic debris drifting endlessly down.
    MarineSnow,
    /// The flat floor of the deep ocean.
    AbyssalPlain,
    /// Seafloor strewn with mineral nodules.
    NoduleField,
    /// The steep side of an ocean trench.
    TrenchWall,
    /// The deepest floor of an ocean trench.
    TrenchFloor,
}

impl Variant {
    /// The registry key for this variant's concept.
    /// type-audit: bare-ok(identifier-text)
    pub fn concept_name(self) -> &'static str {
        match self {
            Variant::Erg => "erg",
            Variant::Playa => "playa",
            Variant::Hamada => "hamada",
            Variant::Reg => "reg",
            Variant::OldGrowth => "old-growth",
            Variant::DampHollow => "damp-hollow",
            Variant::ForestGap => "forest-gap",
            Variant::MossyDeadfall => "mossy-deadfall",
            Variant::BorealStand => "boreal-stand",
            Variant::Muskeg => "muskeg",
            Variant::Burn => "burn",
            Variant::FrostHeave => "frost-heave",
            Variant::Felsenmeer => "felsenmeer",
            Variant::WindScour => "wind-scour",
            Variant::GrassSward => "grass-sward",
            Variant::WoodedGrassland => "wooded-grassland",
            Variant::ClosedCanopy => "closed-canopy",
            Variant::LianaForest => "liana-forest",
            Variant::GalleryForest => "gallery-forest",
            Variant::Snowfield => "snowfield",
            Variant::CrevasseField => "crevasse-field",
            Variant::ScouredIce => "scoured-ice",
            Variant::ThornScrub => "thorn-scrub",
            Variant::SclerophyllScrub => "sclerophyll-scrub",
            Variant::FireScrub => "fire-scrub",
            Variant::PressureRidge => "pressure-ridge",
            Variant::IceLead => "ice-lead",
            Variant::RaftedFloe => "rafted-floe",
            Variant::MeltPond => "melt-pond",
            Variant::CoralHead => "coral-head",
            Variant::SpurAndGroove => "spur-and-groove",
            Variant::ReefRubble => "reef-rubble",
            Variant::StaghornStand => "staghorn-stand",
            Variant::KelpCanopy => "kelp-canopy",
            Variant::HoldfastTangle => "holdfast-tangle",
            Variant::UrchinBarren => "urchin-barren",
            Variant::SmokerField => "smoker-field",
            Variant::TubewormThicket => "tubeworm-thicket",
            Variant::VentPlume => "vent-plume",
            Variant::PlanktonBloom => "plankton-bloom",
            Variant::ColdUpwelling => "cold-upwelling",
            Variant::BaitBall => "bait-ball",
            Variant::OpenBlue => "open-blue",
            Variant::SargassumDrift => "sargassum-drift",
            Variant::FishShoal => "fish-shoal",
            Variant::TwilightWater => "twilight-water",
            Variant::ScatteringLayer => "scattering-layer",
            Variant::LightlessWater => "lightless-water",
            Variant::MarineSnow => "marine-snow",
            Variant::AbyssalPlain => "abyssal-plain",
            Variant::NoduleField => "nodule-field",
            Variant::TrenchWall => "trench-wall",
            Variant::TrenchFloor => "trench-floor",
        }
    }

    /// Every variant, in declaration order.
    pub fn catalog() -> &'static [Variant] {
        &[
            Variant::Erg,
            Variant::Playa,
            Variant::Hamada,
            Variant::Reg,
            Variant::OldGrowth,
            Variant::DampHollow,
            Variant::ForestGap,
            Variant::MossyDeadfall,
            Variant::BorealStand,
            Variant::Muskeg,
            Variant::Burn,
            Variant::FrostHeave,
            Variant::Felsenmeer,
            Variant::WindScour,
            Variant::GrassSward,
            Variant::WoodedGrassland,
            Variant::ClosedCanopy,
            Variant::LianaForest,
            Variant::GalleryForest,
            Variant::Snowfield,
            Variant::CrevasseField,
            Variant::ScouredIce,
            Variant::ThornScrub,
            Variant::SclerophyllScrub,
            Variant::FireScrub,
            Variant::PressureRidge,
            Variant::IceLead,
            Variant::RaftedFloe,
            Variant::MeltPond,
            Variant::CoralHead,
            Variant::SpurAndGroove,
            Variant::ReefRubble,
            Variant::StaghornStand,
            Variant::KelpCanopy,
            Variant::HoldfastTangle,
            Variant::UrchinBarren,
            Variant::SmokerField,
            Variant::TubewormThicket,
            Variant::VentPlume,
            Variant::PlanktonBloom,
            Variant::ColdUpwelling,
            Variant::BaitBall,
            Variant::OpenBlue,
            Variant::SargassumDrift,
            Variant::FishShoal,
            Variant::TwilightWater,
            Variant::ScatteringLayer,
            Variant::LightlessWater,
            Variant::MarineSnow,
            Variant::AbyssalPlain,
            Variant::NoduleField,
            Variant::TrenchWall,
            Variant::TrenchFloor,
        ]
    }
}

/// One weighted entry: how likely, which variant, and the prose that renders
/// it.
/// type-audit: bare-ok(ratio: weight), bare-ok(prose: prose)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct VariantEntry {
    /// Draw weight, relative within its pool.
    pub weight: f64,
    /// The named sub-type this entry is an instance of.
    pub variant: Variant,
    /// The prose a room renders for it.
    pub prose: &'static str,
}

/// The substrate a cell's ground is made of, as the variant pool distinguishes
/// it. Mirrors `locale`'s own substrate classes; passed in so this table can
/// live below the window that computes it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroundKind {
    /// Rock and soil — the mundane default.
    Ordinary,
    /// Wind-worked sand.
    Sand,
    /// Evaporite salt/gypsum crust.
    Evaporite,
    /// Bare volcanic basalt.
    Basaltic,
    /// Volcanic ash drifts.
    Ashen,
}

/// The weighted variant pool for a formation, at a stratum, on a ground kind.
/// Order and weights are the prose pool's own, unchanged.
pub fn variant_pool(
    formation: Formation,
    stratum: Stratum,
    ground: GroundKind,
) -> &'static [VariantEntry] {
    match (formation, ground) {
        (Formation::Desert, GroundKind::Sand) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::Erg,
                prose: "erg dunes",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::Erg,
                prose: "a nabkha field",
            },
        ],
        (Formation::Desert, GroundKind::Evaporite) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::Playa,
                prose: "a cracked playa",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::Playa,
                prose: "a salt pan",
            },
        ],
        (Formation::Desert, GroundKind::Basaltic) => &[VariantEntry {
            weight: 3.0,
            variant: Variant::Hamada,
            prose: "a hamada of bare rock",
        }],
        (Formation::Desert, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::Reg,
                prose: "a reg of wind-swept gravel",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::Reg,
                prose: "a yardang field",
            },
        ],
        (Formation::TemperateForest | Formation::TemperateRainforest, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::OldGrowth,
                prose: "old-growth timber",
            },
            VariantEntry {
                weight: 3.0,
                variant: Variant::OldGrowth,
                prose: "dense understory",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::DampHollow,
                prose: "a mossy hollow",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::ForestGap,
                prose: "a windthrow gap",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::DampHollow,
                prose: "a fern-choked draw",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::MossyDeadfall,
                prose: "a lichen-hung grove",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::MossyDeadfall,
                prose: "a deadfall tangle",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::ForestGap,
                prose: "a shaft of clear light",
            },
        ],
        (Formation::Taiga, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::BorealStand,
                prose: "a boreal stand",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::Muskeg,
                prose: "a peat hollow",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::Burn,
                prose: "a burnt snag",
            },
        ],
        (Formation::Tundra | Formation::Alpine, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::FrostHeave,
                prose: "frost-heaved ground",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::Felsenmeer,
                prose: "a boulder field",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::WindScour,
                prose: "wind scour",
            },
        ],
        (Formation::Savanna | Formation::TemperateGrassland, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::GrassSward,
                prose: "open sward",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::WoodedGrassland,
                prose: "a scattered copse",
            },
        ],
        (Formation::TropicalRainforest | Formation::TropicalSeasonalForest, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::ClosedCanopy,
                prose: "buttressed canopy",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::LianaForest,
                prose: "a liana tangle",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::GalleryForest,
                prose: "a stream gully",
            },
        ],
        (Formation::Ice, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::Snowfield,
                prose: "a snowfield",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::CrevasseField,
                prose: "a crevasse field",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::ScouredIce,
                prose: "wind-carved sastrugi",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::ScouredIce,
                prose: "blue ice, swept bare",
            },
        ],
        (Formation::Shrubland, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::ThornScrub,
                prose: "thorn scrub",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::SclerophyllScrub,
                prose: "a chaparral slope",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::SclerophyllScrub,
                prose: "matorral, low and grey",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::FireScrub,
                prose: "a burnt-over thicket",
            },
        ],
        (Formation::SeaIce, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::PressureRidge,
                prose: "a pressure ridge",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::IceLead,
                prose: "a lead of open water",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::RaftedFloe,
                prose: "rafted floe",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::MeltPond,
                prose: "a melt pond",
            },
        ],
        (Formation::Reef, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::CoralHead,
                prose: "a coral head",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::SpurAndGroove,
                prose: "a spur-and-groove channel",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::ReefRubble,
                prose: "a rubble apron",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::StaghornStand,
                prose: "a stand of staghorn",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::CoralHead,
                prose: "a bommie standing alone",
            },
        ],
        (Formation::KelpForest, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::KelpCanopy,
                prose: "a kelp canopy",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::HoldfastTangle,
                prose: "a holdfast tangle",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::KelpCanopy,
                prose: "a stipe forest",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::UrchinBarren,
                prose: "an urchin barren, grazed bare",
            },
        ],
        (Formation::Vent, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::SmokerField,
                prose: "a black smoker",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::SmokerField,
                prose: "a chimney field",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::TubewormThicket,
                prose: "a tubeworm thicket",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::VentPlume,
                prose: "a shimmering haze of hot water",
            },
        ],
        (Formation::Upwelling, _) => &[
            VariantEntry {
                weight: 3.0,
                variant: Variant::PlanktonBloom,
                prose: "a plankton bloom",
            },
            VariantEntry {
                weight: 2.0,
                variant: Variant::ColdUpwelling,
                prose: "cold water rising",
            },
            VariantEntry {
                weight: 1.0,
                variant: Variant::BaitBall,
                prose: "a bait ball, turning",
            },
        ],
        (Formation::OpenWater, _) => match stratum {
            Stratum::Epipelagic | Stratum::Surface => &[
                VariantEntry {
                    weight: 3.0,
                    variant: Variant::OpenBlue,
                    prose: "open blue water",
                },
                VariantEntry {
                    weight: 2.0,
                    variant: Variant::SargassumDrift,
                    prose: "a drifting sargassum mat",
                },
                VariantEntry {
                    weight: 1.0,
                    variant: Variant::FishShoal,
                    prose: "a shoal turning as one",
                },
            ],
            Stratum::Mesopelagic => &[
                VariantEntry {
                    weight: 3.0,
                    variant: Variant::TwilightWater,
                    prose: "the twilight water",
                },
                VariantEntry {
                    weight: 2.0,
                    variant: Variant::ScatteringLayer,
                    prose: "a scattering layer, rising",
                },
            ],
            Stratum::Bathypelagic => &[
                VariantEntry {
                    weight: 3.0,
                    variant: Variant::LightlessWater,
                    prose: "the lightless water",
                },
                VariantEntry {
                    weight: 2.0,
                    variant: Variant::MarineSnow,
                    prose: "marine snow, drifting down",
                },
            ],
            Stratum::Abyssal => &[
                VariantEntry {
                    weight: 3.0,
                    variant: Variant::AbyssalPlain,
                    prose: "the abyssal plain",
                },
                VariantEntry {
                    weight: 2.0,
                    variant: Variant::NoduleField,
                    prose: "a field of manganese nodules",
                },
            ],
            Stratum::Hadal => &[
                VariantEntry {
                    weight: 3.0,
                    variant: Variant::TrenchWall,
                    prose: "the trench wall",
                },
                VariantEntry {
                    weight: 2.0,
                    variant: Variant::TrenchFloor,
                    prose: "the trench floor",
                },
            ],
        },
    }
}

//! The almanac window: render a world as a one-page document. Windows may
//! depend on domains (they present them); domains never depend on windows.
#![warn(missing_docs)]

use hornvale_astronomy::SkyReport;
use hornvale_climate::ClimateReport;
use hornvale_kernel::Phenomenon;
use hornvale_religion::Belief;
use hornvale_terrain::PlaceInfo;

/// One species' flagship settlement, rendered as its own block under The
/// People. Replaces the old single `village`/`culture_lines` pair now that
/// worlds hold more than one species.
/// type-audit: bare-ok(identifier-text: species), bare-ok(identifier-text: noun), bare-ok(identifier-text: name), bare-ok(count: population), bare-ok(prose: culture_lines)
pub struct PeopleBlock {
    /// The species name ("goblin", "kobold").
    pub species: String,
    /// The species' settlement noun ("village", "warren").
    pub noun: String,
    /// The flagship settlement's name.
    pub name: String,
    /// The flagship settlement's population.
    pub population: u32,
    /// The flagship's emergent culture: its subsistence mode and a one-line
    /// role-structure summary.
    pub culture_lines: Vec<String>,
}

/// One belief, paired with its rendered tenet — the content→render seam's
/// output (spec §6). The composition root builds a `LineContent` from the
/// belief's structured facts and a species' `VoiceParams` from its
/// psychology vector, then calls `hornvale_language::render_line`; the
/// almanac only ever displays the resulting string, never the structured
/// fields directly.
/// type-audit: bare-ok(prose: tenet)
pub struct BeliefLine {
    /// The belief's structured content (deity, epithet, sentiment, rank).
    pub belief: Belief,
    /// The rendered tenet sentence, voiced by the holding species.
    pub tenet: String,
}

/// One community's pantheon, ready to render: the species and settlement
/// it belongs to, its cult form, and its beliefs in salience order.
/// type-audit: bare-ok(identifier-text: species), bare-ok(identifier-text: noun), bare-ok(identifier-text: settlement), bare-ok(identifier-text: cult_form)
pub struct PantheonBlock {
    /// The species name ("goblin", "kobold"); empty for legacy saves that
    /// predate species facts.
    pub species: String,
    /// The settlement noun ("village", "warren").
    pub noun: String,
    /// The holding settlement's name.
    pub settlement: String,
    /// The pantheon's cult form (`"organized"` or `"folk"`), if recorded.
    pub cult_form: Option<String>,
    /// The pantheon's beliefs, head first, each paired with its rendered
    /// tenet.
    pub beliefs: Vec<BeliefLine>,
}

/// The night instrument's additional lines under **The Sky** (night-sky
/// stage 1/2): the pole star, if a bright star stands within the pole-star
/// radius of either celestial pole at genesis, the heliacal returns of the
/// brightest neighbors, and one line per wandering sibling planet. `None`
/// for constant-sky worlds, which have no neighborhood to describe (see
/// [`AlmanacContext::night_sky_lines`]).
/// type-audit: bare-ok(prose: pole_star), bare-ok(prose: heliacal), bare-ok(prose: wanderers), bare-ok(prose: figures), bare-ok(prose: alignment)
pub struct NightSkyLines {
    /// The pole-star sentence, if one exists at genesis.
    pub pole_star: Option<String>,
    /// Up to three heliacal-return sentences, neighbor-index order.
    pub heliacal: Vec<String>,
    /// One sentence per wandering sibling planet, innermost order
    /// (night-sky stage 2).
    pub wanderers: Vec<String>,
    /// A single figure-count/ecliptic summary line (night-sky stage 3);
    /// empty for skies with no figures at all.
    pub figures: Vec<String>,
    /// The flagship settlement's founding sightline and its drift rate
    /// (The Long Count), when the world has both a sunrise and a
    /// settlement.
    pub alignment: Option<String>,
}

/// Everything the almanac needs, gathered by the composition root.
/// type-audit: bare-ok(constructor-edge: seed), bare-ok(prose: land_lines), bare-ok(prose: biome_lines), bare-ok(prose: ground_lines), bare-ok(prose: deep_time_lines), bare-ok(prose: calendar_lines), bare-ok(prose: night_sky), bare-ok(prose: genesis_notes), bare-ok(prose: settlement_lines)
pub struct AlmanacContext {
    /// The world seed, for the title.
    pub seed: u64,
    /// The sky at genesis.
    pub sky: SkyReport,
    /// The climate at the world's first place.
    pub climate: ClimateReport,
    /// Phenomena salient at the world's first place, salience-ranked.
    pub phenomena: Vec<Phenomenon>,
    /// Known places.
    pub places: Vec<PlaceInfo>,
    /// The tectonic globe's headline lines, from the composition root.
    pub land_lines: Vec<String>,
    /// The globe's biome/habitability headline lines, from the composition root.
    pub biome_lines: Vec<String>,
    /// The ground's headline lines: dominant rock/soil order over land, plus
    /// notable formations (The Ground, spec §3/§4/§6); empty for a landless
    /// world.
    pub ground_lines: Vec<String>,
    /// Deep-time headline lines (the glacial history); empty for worlds with
    /// no glacial past (constant sky, or zero forcing).
    pub deep_time_lines: Vec<String>,
    /// The world's cycles, reader-facing; empty for constant-sky worlds.
    pub calendar_lines: Vec<String>,
    /// The night sky as a sentence; `None` for constant-sky worlds.
    pub night_sky: Option<String>,
    /// The pole-star and heliacal-return lines under **The Sky** (night-sky
    /// stage 1); `None` for constant-sky worlds.
    pub night_sky_lines: Option<NightSkyLines>,
    /// Notes recorded during sky genesis; empty for constant-sky worlds.
    pub genesis_notes: Vec<String>,
    /// Headline lines describing the world's people: how many settlements,
    /// and the flagship's name, population, and biome.
    pub settlement_lines: Vec<String>,
    /// One block per species that placed settlements, registry order
    /// (goblin first). Empty for worlds with no settlements.
    pub peoples: Vec<PeopleBlock>,
    /// One block per species-flagship pantheon, in the order gathered by
    /// the composition root; legacy saves fall back to a single anonymous
    /// block. Empty for worlds with no beliefs.
    pub pantheons: Vec<PantheonBlock>,
}

/// Render one species' life-history line for the almanac (BIO-2, spec §5/§6):
/// its basal metabolism, plus a pace-of-life headline and lifespan/maturity
/// figures when the species has biological traits at all. Suppressed for
/// `Ametabolic` species (constructs, undead), which carry no mass-derived
/// life-history to report — only the metabolic clause renders for those.
/// type-audit: bare-ok(prose: return)
pub fn render_life_history_line(def: &hornvale_species::SpeciesDef) -> String {
    let history = hornvale_species::life_history(def.mass, def.metabolic_class);
    let mut line = format!(
        "The {} run a basal metabolism of {:.0} W",
        def.name, history.basal_metabolic_rate_w
    );
    if let (Some(lifespan), Some(maturity)) = (history.lifespan, history.age_at_maturity) {
        let headline = if history.pace_of_life < 0.33 {
            "fast-lived and prolific"
        } else if history.pace_of_life > 0.66 {
            "slow, long-lived, and sparse"
        } else {
            "moderate-paced"
        };
        line.push_str(&format!(
            "; {headline}, lifespan ~{} yr, matures ~{} yr",
            lifespan.get().round(),
            maturity.get().round()
        ));
    }
    line.push('.');
    line
}

/// Render the one-page world document as markdown. Deterministic: same
/// context, same bytes.
/// type-audit: bare-ok(artifact: return)
pub fn render(ctx: &AlmanacContext) -> String {
    let mut doc = String::new();
    doc.push_str(&format!("# The Almanac of Seed {}\n\n", ctx.seed));

    doc.push_str("## The Sky\n\n");
    doc.push_str(&format!("{}\n\n", ctx.sky.description));
    doc.push_str(&format!(
        "Visible bodies: {}.\n\n",
        ctx.sky.bodies.join(", ")
    ));

    if !ctx.phenomena.is_empty() {
        doc.push_str("Salient phenomena, most attention-demanding first:\n\n");
        for p in &ctx.phenomena {
            doc.push_str(&format!(
                "- [{:.2}] *{}* — {}\n",
                p.salience, p.kind, p.description
            ));
        }
        doc.push('\n');
    }

    if let Some(night_sky) = &ctx.night_sky {
        doc.push_str(&format!("{night_sky}\n\n"));
    }

    if let Some(lines) = &ctx.night_sky_lines {
        if let Some(pole_star) = &lines.pole_star {
            doc.push_str(&format!("{pole_star}\n\n"));
        }
        for line in &lines.heliacal {
            doc.push_str(&format!("{line}\n\n"));
        }
        for line in &lines.wanderers {
            doc.push_str(&format!("{line}\n\n"));
        }
        for line in &lines.figures {
            doc.push_str(&format!("{line}\n\n"));
        }
        if let Some(alignment) = &lines.alignment {
            doc.push_str(&format!("{alignment}\n\n"));
        }
    }

    if !ctx.calendar_lines.is_empty() {
        doc.push_str("## The Calendar\n\n");
        for line in &ctx.calendar_lines {
            doc.push_str(&format!("- {line}\n"));
        }
        doc.push('\n');
        // Nested inside the calendar-lines check deliberately: genesis
        // notes only ever exist alongside a generated calendar (both are
        // empty for constant-sky worlds), so this stays in lockstep with
        // world_builder's calendar_lines/genesis_notes pairing.
        if !ctx.genesis_notes.is_empty() {
            doc.push_str("Notes from genesis:\n\n");
            for note in &ctx.genesis_notes {
                doc.push_str(&format!("- {note}\n"));
            }
            doc.push('\n');
        }
    }

    doc.push_str("## The Land\n\n");
    for line in &ctx.land_lines {
        doc.push_str(&format!("{line}\n"));
    }
    if !ctx.land_lines.is_empty() {
        doc.push('\n');
    }
    for line in &ctx.biome_lines {
        doc.push_str(&format!("{line}\n"));
    }
    if !ctx.biome_lines.is_empty() {
        doc.push('\n');
    }
    if ctx.places.is_empty() {
        doc.push_str("No places are known.\n\n");
    } else {
        for place in &ctx.places {
            doc.push_str(&format!("- **{}** — {}\n", place.name, place.biome));
        }
        doc.push_str(&format!(
            "\n{} ({:.0}°C)\n\n",
            ctx.climate.description, ctx.climate.temperature_c
        ));
    }

    if !ctx.ground_lines.is_empty() {
        doc.push_str("## The Ground\n\n");
        for line in &ctx.ground_lines {
            doc.push_str(&format!("{line}\n"));
        }
        doc.push('\n');
    }

    if !ctx.deep_time_lines.is_empty() {
        doc.push_str("## Deep Time\n\n");
        for line in &ctx.deep_time_lines {
            doc.push_str(&format!("{line}\n"));
        }
        doc.push('\n');
    }

    doc.push_str("## The People\n\n");
    for line in &ctx.settlement_lines {
        doc.push_str(&format!("{line}\n"));
    }
    if !ctx.settlement_lines.is_empty() {
        doc.push('\n');
    }
    if ctx.peoples.is_empty() {
        doc.push_str("No settlements are known.\n\n");
    } else {
        for p in &ctx.peoples {
            doc.push_str(&format!(
                "The {} {} of **{}**, population {}.\n\n",
                p.species, p.noun, p.name, p.population
            ));
            for line in &p.culture_lines {
                doc.push_str(&format!("{line}\n"));
            }
            if !p.culture_lines.is_empty() {
                doc.push('\n');
            }
        }
    }

    doc.push_str("## The Gods\n\n");
    if ctx.pantheons.iter().all(|p| p.beliefs.is_empty()) {
        doc.push_str("No beliefs are recorded.\n\n");
    } else {
        for (i, pantheon) in ctx.pantheons.iter().enumerate() {
            if pantheon.beliefs.is_empty() {
                continue;
            }
            // The first block reproduces the legacy section byte-for-byte
            // (the identity contract); later blocks name their people.
            if i == 0 {
                if let Some(form) = &pantheon.cult_form {
                    let lead = match form.as_str() {
                        "organized" => "An organized priesthood tends a pantheon:",
                        _ => "The people keep a folk pantheon:",
                    };
                    doc.push_str(&format!("{lead}\n\n"));
                }
            } else {
                let lead = match pantheon.cult_form.as_deref() {
                    Some("organized") => format!(
                        "In the {} of **{}**, an organized priesthood tends its own pantheon:",
                        pantheon.noun, pantheon.settlement
                    ),
                    _ => format!(
                        "The {} of **{}** keeps its own folk pantheon:",
                        pantheon.noun, pantheon.settlement
                    ),
                };
                doc.push_str(&format!("{lead}\n\n"));
            }
            for line in &pantheon.beliefs {
                let mark = if line.belief.high_god {
                    " *(who presides)*"
                } else {
                    ""
                };
                // The content→render seam (spec §6): `line.tenet` is
                // `hornvale_language::render_line` applied to the belief's
                // structured content under the holding species' voice.
                doc.push_str(&format!(
                    "> {}{mark}\n>\n> — derived from the phenomenon *{}*\n\n",
                    line.tenet, line.belief.source_kind
                ));
            }
        }
    }

    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    doc
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{EntityId, Venue};
    use hornvale_religion::Sentiment;

    fn sample_context() -> AlmanacContext {
        AlmanacContext {
            seed: 42,
            sky: SkyReport {
                description: "A golden sun hangs fixed at zenith.".to_string(),
                bodies: vec!["the sun".to_string()],
            },
            climate: ClimateReport {
                temperature_c: 18.0,
                description: "Mild and temperate.".to_string(),
            },
            phenomena: vec![Phenomenon {
                kind: "celestial-body".to_string(),
                description: "a golden sun fixed at zenith".to_string(),
                period_days: None,
                salience: 1.0,
                venue: Venue::Ambient,
            }],
            places: vec![PlaceInfo {
                id: EntityId(1),
                name: "the Vale".to_string(),
                biome: "temperate forest".to_string(),
            }],
            land_lines: vec![
                "The globe breaks into 23 plates; the sea claims 63% of its surface.".to_string(),
            ],
            biome_lines: vec![],
            ground_lines: vec![
                "The land is mostly granite, its soils mostly loam.".to_string(),
            ],
            deep_time_lines: vec![
                "The frost retreated; ice advanced over 30% of the land at its greatest."
                    .to_string(),
            ],
            peoples: vec![PeopleBlock {
                species: "goblin".to_string(),
                noun: "village".to_string(),
                name: "Bolnar".to_string(),
                population: 60,
                culture_lines: vec![],
            }],
            pantheons: vec![PantheonBlock {
                species: "goblin".to_string(),
                noun: "village".to_string(),
                settlement: "Bolnar".to_string(),
                cult_form: Some("organized".to_string()),
                beliefs: vec![BeliefLine {
                    belief: Belief {
                        id: EntityId(3),
                        deity: "Ignathar".to_string(),
                        epithet: "the Ever-Flame".to_string(),
                        source_kind: "celestial-body".to_string(),
                        sentiment: Sentiment::Eternal,
                        high_god: true,
                    },
                    tenet: "Ignathar the Ever-Flame is ever: Ignathar the Ever-Flame watches unceasing."
                        .to_string(),
                }],
            }],
            calendar_lines: vec![],
            night_sky: None,
            night_sky_lines: None,
            genesis_notes: vec![],
            settlement_lines: vec![],
        }
    }

    #[test]
    fn render_contains_every_section_and_datum() {
        let doc = render(&sample_context());
        for expected in [
            "# The Almanac of Seed 42",
            "## The Sky",
            "zenith",
            "## The Land",
            "23 plates",
            "the Vale",
            "temperate forest",
            "## The Ground",
            "granite",
            "loam",
            "## Deep Time",
            "## The People",
            "Bolnar",
            "60",
            "## The Gods",
            "Ever-Flame",
            "celestial-body",
            "a golden sun fixed at zenith",
            "[1.00]",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn render_is_deterministic() {
        assert_eq!(render(&sample_context()), render(&sample_context()));
    }

    #[test]
    fn deep_time_section_renders_when_present_and_is_skipped_when_empty() {
        let mut ctx = sample_context();
        ctx.deep_time_lines = vec!["The frost retreated.".to_string()];
        assert!(render(&ctx).contains("## Deep Time"));
        ctx.deep_time_lines = vec![];
        assert!(!render(&ctx).contains("## Deep Time"));
    }

    #[test]
    fn ground_section_names_the_dominant_rock_and_soil_and_is_skipped_when_empty() {
        let mut ctx = sample_context();
        ctx.ground_lines = vec![
            "The land is mostly granite, its soils mostly loam.".to_string(),
            "Notable: karst country, salt flats.".to_string(),
        ];
        let doc = render(&ctx);
        assert!(doc.contains("## The Ground"));
        assert!(doc.contains("mostly granite"));
        assert!(doc.contains("mostly loam"));
        assert!(doc.contains("karst country"));
        assert!(doc.contains("salt flats"));

        ctx.ground_lines = vec![];
        assert!(!render(&ctx).contains("## The Ground"));
    }

    #[test]
    fn ground_section_renders_between_land_and_deep_time() {
        let ctx = AlmanacContext {
            ground_lines: vec!["The land is mostly basalt, its soils mostly andosol.".to_string()],
            deep_time_lines: vec!["The frost retreated.".to_string()],
            ..sample_context()
        };
        let doc = render(&ctx);
        let land_pos = doc.find("## The Land").unwrap();
        let ground_pos = doc.find("## The Ground").unwrap();
        let deep_time_pos = doc.find("## Deep Time").unwrap();
        assert!(land_pos < ground_pos, "Ground must come after Land");
        assert!(
            ground_pos < deep_time_pos,
            "Ground must come before Deep Time"
        );
    }

    #[test]
    fn culture_lines_render_under_the_people_section() {
        let ctx = AlmanacContext {
            peoples: vec![PeopleBlock {
                species: "goblin".to_string(),
                noun: "village".to_string(),
                name: "Bolnar".to_string(),
                population: 60,
                culture_lines: vec![
                    "Bolnar lives by farming.".to_string(),
                    "Its roles, lowest to highest: farmer, chief.".to_string(),
                ],
            }],
            ..sample_context()
        };
        let doc = render(&ctx);
        let people_pos = doc.find("## The People").unwrap();
        let culture_pos = doc.find("Bolnar lives by farming.").unwrap();
        assert!(
            people_pos < culture_pos,
            "culture lines belong under The People"
        );
        assert!(doc.contains("Its roles, lowest to highest: farmer, chief."));
    }

    #[test]
    fn empty_world_renders_honestly() {
        let ctx = AlmanacContext {
            peoples: vec![],
            pantheons: vec![],
            places: vec![],
            ..sample_context()
        };
        let doc = render(&ctx);
        assert!(doc.contains("No settlements are known."));
        assert!(doc.contains("No beliefs are recorded."));
        assert!(!doc.contains("## The Calendar"));
    }

    #[test]
    fn a_lone_goblin_block_is_byte_identical_to_the_legacy_string() {
        let ctx = AlmanacContext {
            peoples: vec![PeopleBlock {
                species: "goblin".to_string(),
                noun: "village".to_string(),
                name: "Grumoknar".to_string(),
                population: 359,
                culture_lines: vec![],
            }],
            ..sample_context()
        };
        let doc = render(&ctx);
        assert!(doc.contains("The goblin village of **Grumoknar**, population 359.\n\n"));
    }

    #[test]
    fn two_species_render_as_separate_blocks_goblin_first() {
        let ctx = AlmanacContext {
            peoples: vec![
                PeopleBlock {
                    species: "goblin".to_string(),
                    noun: "village".to_string(),
                    name: "Grum".to_string(),
                    population: 200,
                    culture_lines: vec![
                        "Grum lives by farming.".to_string(),
                        "Its roles, lowest to highest: farmer, chief.".to_string(),
                    ],
                },
                PeopleBlock {
                    species: "kobold".to_string(),
                    noun: "warren".to_string(),
                    name: "Zikthur".to_string(),
                    population: 80,
                    culture_lines: vec![
                        "Zikthur lives by mining.".to_string(),
                        "Its roles, lowest to highest: digger, elders.".to_string(),
                    ],
                },
            ],
            ..sample_context()
        };
        let doc = render(&ctx);
        assert!(doc.contains("The goblin village of **Grum**, population 200."));
        assert!(doc.contains("The kobold warren of **Zikthur**, population 80."));
        assert!(doc.contains("Grum lives by farming."));
        assert!(doc.contains("Zikthur lives by mining."));

        let goblin_pos = doc.find("The goblin village of **Grum**").unwrap();
        let kobold_pos = doc.find("The kobold warren of **Zikthur**").unwrap();
        assert!(goblin_pos < kobold_pos, "kobold block must follow goblin");

        // Each block's culture lines follow its own settlement line.
        let goblin_culture_pos = doc.find("Grum lives by farming.").unwrap();
        let kobold_culture_pos = doc.find("Zikthur lives by mining.").unwrap();
        assert!(goblin_pos < goblin_culture_pos && goblin_culture_pos < kobold_pos);
        assert!(kobold_pos < kobold_culture_pos);
    }

    #[test]
    fn the_gods_section_renders_a_structured_pantheon() {
        let ctx = AlmanacContext {
            pantheons: vec![PantheonBlock {
                species: "goblin".to_string(),
                noun: "village".to_string(),
                settlement: "Bolnar".to_string(),
                cult_form: Some("organized".to_string()),
                beliefs: vec![
                    BeliefLine {
                        belief: Belief {
                            id: EntityId(3),
                            deity: "Ignathar".to_string(),
                            epithet: "the Ever-Flame".to_string(),
                            source_kind: "celestial-body".to_string(),
                            sentiment: Sentiment::Eternal,
                            high_god: true,
                        },
                        tenet: "Ignathar the Ever-Flame is ever: Ignathar the Ever-Flame watches unceasing."
                            .to_string(),
                    },
                    BeliefLine {
                        belief: Belief {
                            id: EntityId(4),
                            deity: "Meraleth".to_string(),
                            epithet: "the Tidewalker".to_string(),
                            source_kind: "seasonal-cycle".to_string(),
                            sentiment: Sentiment::Cyclic,
                            high_god: false,
                        },
                        tenet: "Meraleth the Tidewalker returns every 29 days.".to_string(),
                    },
                ],
            }],
            ..sample_context()
        };
        let doc = render(&ctx);
        assert!(doc.contains("organized"), "cult form named");
        assert!(doc.contains("Ever-Flame"));
        assert!(doc.contains("Tidewalker"));
        // The high god is distinguished from the lesser deities.
        let gods = doc.split("## The Gods").nth(1).unwrap();
        assert!(
            gods.contains("presides") || gods.contains("high"),
            "high god marked"
        );
    }

    #[test]
    fn a_single_pantheon_renders_exactly_as_before() {
        // Byte-identity: the first block must reproduce the legacy section.
        let ctx = sample_context(); // one goblin pantheon, organized, one high god
        let doc = render(&ctx);
        assert!(doc.contains("## The Gods\n\nAn organized priesthood tends a pantheon:\n\n"));
        assert!(
            !doc.contains("its own"),
            "single-pantheon worlds name no species"
        );
    }

    #[test]
    fn a_second_pantheon_gets_a_species_lead() {
        let mut ctx = sample_context();
        ctx.pantheons.push(PantheonBlock {
            species: "kobold".to_string(),
            noun: "warren".to_string(),
            settlement: "Zikthur".to_string(),
            cult_form: Some("folk".to_string()),
            beliefs: vec![BeliefLine {
                belief: Belief {
                    id: EntityId(99),
                    deity: "Meraleth".to_string(),
                    epithet: "the Tidewalker".to_string(),
                    source_kind: "celestial-body".to_string(),
                    sentiment: Sentiment::Cyclic,
                    high_god: true,
                },
                tenet: "Meraleth the Tidewalker returns every 29 days.".to_string(),
            }],
        });
        let doc = render(&ctx);
        assert!(doc.contains("The warren of **Zikthur** keeps its own folk pantheon:"));
        let gods = doc.split("## The Gods").nth(1).unwrap();
        assert!(
            gods.find("priesthood tends a pantheon").unwrap()
                < gods.find("its own folk pantheon").unwrap(),
            "goblin block renders first, exactly as before"
        );
    }

    #[test]
    fn calendar_section_renders_between_sky_and_land() {
        let ctx = AlmanacContext {
            calendar_lines: vec![
                "The year is 365.2 local days (365.2 standard days).".to_string(),
                "The first moon circles every 29.5 local days — 12.4 months to a year.".to_string(),
            ],
            night_sky: Some("By night: a hard blue-white star that does not wander.".to_string()),
            genesis_notes: vec!["a showpiece neighbor was forced by pin".to_string()],
            ..sample_context()
        };
        let doc = render(&ctx);

        assert!(doc.contains("## The Calendar"));
        let sky_pos = doc.find("## The Sky").unwrap();
        let calendar_pos = doc.find("## The Calendar").unwrap();
        let land_pos = doc.find("## The Land").unwrap();
        assert!(sky_pos < calendar_pos, "Calendar must come after Sky");
        assert!(calendar_pos < land_pos, "Calendar must come before Land");

        assert!(doc.contains("- The year is 365.2 local days (365.2 standard days)."));
        assert!(
            doc.contains("- The first moon circles every 29.5 local days — 12.4 months to a year.")
        );
        assert!(doc.contains("Notes from genesis:"));
        assert!(doc.contains("- a showpiece neighbor was forced by pin"));

        // The night sky reads as part of the Sky section, before Calendar.
        let night_sky_pos = doc
            .find("By night: a hard blue-white star that does not wander.")
            .unwrap();
        assert!(sky_pos < night_sky_pos && night_sky_pos < calendar_pos);
    }

    #[test]
    fn night_sky_lines_render_when_present_and_are_skipped_when_absent() {
        let mut ctx = sample_context();
        ctx.night_sky_lines = Some(NightSkyLines {
            pole_star: Some(
                "A blue-white star stands 3.2° from the north celestial pole; the sky wheels around it."
                    .to_string(),
            ),
            heliacal: vec![
                "The blue-white star returns before dawn at year-phase 0.42, after 70 days of absence."
                    .to_string(),
            ],
            wanderers: vec![
                "A rock wanderer rounds the sun every 224 days — a morning and evening star."
                    .to_string(),
            ],
            figures: vec!["The sky holds 4 figures; 1 stands on the sun's road.".to_string()],
            alignment: None,
        });
        let doc = render(&ctx);
        assert!(doc.contains("A blue-white star stands 3.2° from the north celestial pole"));
        assert!(doc.contains("returns before dawn at year-phase 0.42, after 70 days of absence."));
        assert!(doc.contains("A rock wanderer rounds the sun every 224 days"));
        assert!(doc.contains("The sky holds 4 figures; 1 stands on the sun's road."));

        ctx.night_sky_lines = None;
        let doc = render(&ctx);
        assert!(!doc.contains("celestial pole"));
        assert!(!doc.contains("returns before dawn"));
        assert!(!doc.contains("rounds the sun"));
        assert!(!doc.contains("The sky holds"));
    }

    #[test]
    fn night_sky_lines_render_under_sky_before_calendar() {
        let ctx = AlmanacContext {
            night_sky: Some("By night: a hard blue-white star that does not wander.".to_string()),
            night_sky_lines: Some(NightSkyLines {
                pole_star: Some(
                    "A blue-white star stands 3.2° from the north celestial pole; the sky wheels around it."
                        .to_string(),
                ),
                heliacal: vec![
                    "The blue-white star returns before dawn at year-phase 0.42, after 70 days of absence."
                        .to_string(),
                ],
                wanderers: vec![
                    "A rock wanderer rounds the sun every 224 days — a morning and evening star."
                        .to_string(),
                ],
                figures: vec!["The sky holds 4 figures; 1 stands on the sun's road.".to_string()],
                alignment: None,
            }),
            calendar_lines: vec!["The year is 365.2 local days (365.2 standard days).".to_string()],
            ..sample_context()
        };
        let doc = render(&ctx);

        let sky_pos = doc.find("## The Sky").unwrap();
        let night_sky_pos = doc.find("By night:").unwrap();
        let pole_pos = doc.find("celestial pole").unwrap();
        let heliacal_pos = doc.find("returns before dawn").unwrap();
        let wanderer_pos = doc.find("rounds the sun").unwrap();
        let figures_pos = doc.find("The sky holds").unwrap();
        let calendar_pos = doc.find("## The Calendar").unwrap();

        assert!(sky_pos < night_sky_pos, "Sky heading must come first");
        assert!(
            night_sky_pos < pole_pos,
            "pole-star line follows the existing night-star line"
        );
        assert!(
            pole_pos < heliacal_pos,
            "heliacal lines follow the pole-star line"
        );
        assert!(
            heliacal_pos < wanderer_pos,
            "wanderer lines follow the heliacal lines"
        );
        assert!(
            wanderer_pos < figures_pos,
            "figure lines follow the wanderer lines"
        );
        assert!(
            figures_pos < calendar_pos,
            "the night instrument stays inside The Sky, before The Calendar"
        );
    }

    #[test]
    fn almanac_species_block_shows_life_history() {
        // Model on the registry's real goblin def (BIO-2 Task 5) — the
        // helper is a pure function of `SpeciesDef`, no world needed.
        let registry = hornvale_species::registry();
        let goblin = registry.get("goblin").expect("goblin is in the registry");
        let line = render_life_history_line(goblin);
        assert!(line.contains("lifespan"), "missing lifespan figure: {line}");
        assert!(
            line.contains("fast") || line.contains("slow") || line.contains("moderate"),
            "missing pace headline: {line}"
        );
    }

    #[test]
    fn render_life_history_line_suppresses_the_clause_for_ametabolic_species() {
        use hornvale_kernel::Mass;
        use hornvale_species::MetabolicClass;

        let mut construct = hornvale_species::registry()
            .get("goblin")
            .expect("goblin is in the registry")
            .clone();
        construct.metabolic_class = MetabolicClass::Ametabolic;
        construct.mass = Mass::new(500.0).unwrap();
        let line = render_life_history_line(&construct);
        assert!(
            !line.contains("lifespan"),
            "ametabolic species must suppress the life-history clause: {line}"
        );
    }

    #[test]
    fn the_alignment_line_renders_under_the_sky() {
        let mut ctx = sample_context();
        ctx.night_sky_lines = Some(NightSkyLines {
            pole_star: None,
            heliacal: vec![],
            wanderers: vec![],
            figures: vec![],
            alignment: Some(
                "From the first settlement, the midsummer sun rises at azimuth 63.4°; the sightline drifts 0.21° in a thousand years.".to_string(),
            ),
        });
        let doc = render(&ctx);
        let sky = doc.split("## The Calendar").next().unwrap();
        assert!(sky.contains("the sightline drifts"));
    }
}

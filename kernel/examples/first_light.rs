//! Generate the Gallery's "First Light" artifacts for the project book:
//! a rendering of seed 42's fractal noise field (BMP, dependency-free) and
//! the mini-genesis world document. Deterministic: reruns produce identical
//! bytes, so a changed artifact in review means changed behavior.
//!
//! This duplicates the mini-genesis from `tests/determinism.rs` on purpose;
//! both merge into the almanac window in Campaign 1b.
//!
//! Run from the workspace root: `cargo run -p hornvale-kernel --example first_light`

use hornvale_kernel::{
    EntityId, Fact, Ledger, ObserverContext, PhenomenaSource, Phenomenon, Seed, Value, Venue,
    World, WorldTime, choose_consistent, fbm_2d, observe,
};
use std::io::Write as _;
use std::path::Path;

const SEED: Seed = Seed(42);
const IMAGE_SIZE: u32 = 512;
const OUT_DIR: &str = "book/src/gallery";

fn main() -> std::io::Result<()> {
    let out = Path::new(OUT_DIR);
    std::fs::create_dir_all(out)?;
    write_noise_bmp(&out.join("first-light-seed-42.bmp"))?;
    std::fs::write(out.join("world-seed-42.md"), render_world_document())?;
    println!("first light artifacts written to {OUT_DIR}");
    Ok(())
}

/// Render fbm over seed 42's terrain stream as a 24-bit BMP, mapping the
/// unit interval onto an ink-to-parchment ramp.
fn write_noise_bmp(path: &Path) -> std::io::Result<()> {
    let size = IMAGE_SIZE;
    let row_bytes = size * 3; // 512 * 3 is already a multiple of 4; no padding
    let pixel_bytes = row_bytes * size;
    let file_bytes = 14 + 40 + pixel_bytes;

    let mut bmp = Vec::with_capacity(file_bytes as usize);
    // File header
    bmp.extend_from_slice(b"BM");
    bmp.extend_from_slice(&file_bytes.to_le_bytes());
    bmp.extend_from_slice(&[0; 4]); // reserved
    bmp.extend_from_slice(&54u32.to_le_bytes()); // pixel data offset
    // Info header (BITMAPINFOHEADER)
    bmp.extend_from_slice(&40u32.to_le_bytes());
    bmp.extend_from_slice(&(size as i32).to_le_bytes());
    bmp.extend_from_slice(&(size as i32).to_le_bytes()); // positive: bottom-up
    bmp.extend_from_slice(&1u16.to_le_bytes()); // planes
    bmp.extend_from_slice(&24u16.to_le_bytes()); // bits per pixel
    bmp.extend_from_slice(&[0; 24]); // no compression, defaults

    let terrain = SEED.derive("terrain");
    for row in 0..size {
        // Bottom-up: BMP's first stored row is the image's bottom.
        let y = f64::from(size - 1 - row);
        for col in 0..size {
            let x = f64::from(col);
            let v = fbm_2d(terrain, x / 64.0, y / 64.0, 5);
            let (r, g, b) = ramp(v);
            bmp.extend_from_slice(&[b, g, r]); // BMP stores BGR
        }
    }

    let mut file = std::fs::File::create(path)?;
    file.write_all(&bmp)
}

/// Linear ramp from deep ink (low) to warm parchment (high).
fn ramp(v: f64) -> (u8, u8, u8) {
    let lerp = |lo: f64, hi: f64| (lo + (hi - lo) * v).round() as u8;
    (lerp(24.0, 238.0), lerp(28.0, 230.0), lerp(48.0, 208.0))
}

/// The trivial phenomena source used by the mini-genesis: a sun that never
/// sets. Astronomy proper replaces this in Campaign 1b.
struct MiniSun;

impl PhenomenaSource for MiniSun {
    fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
        vec![Phenomenon {
            kind: "celestial-body".to_string(),
            description: "a golden sun fixed at zenith".to_string(),
            period_days: None,
            salience: 1.0,
            venue: Venue::DaySky,
        }]
    }
}

/// Run the mini-genesis and render the entire resulting world as markdown.
fn render_world_document() -> String {
    let mut world = World::new(SEED);
    world
        .registry
        .register_predicate("revered-phenomenon", true, "what a settlement reveres")
        .expect("fresh registry");
    world
        .registry
        .register_phenomenon_kind("celestial-body", "a body visible in the sky")
        .expect("fresh registry");

    let vale = world.ledger.mint_entity();
    let village = world.ledger.mint_entity();

    let candidates = ["Zaggrak", "Bolnar", "Mokru", "Ishtor"];
    let mut stream = SEED.derive("settlement").derive("name").stream();
    let name_fact = |n: &&str| Fact {
        subject: village,
        predicate: "name".to_string(),
        object: Value::Text((*n).to_string()),
        place: Some(vale),
        day: Some(0.0),
        provenance: "settlement".to_string(),
    };
    let idx = choose_consistent(
        &mut stream,
        &world.ledger,
        &world.registry,
        &candidates,
        name_fact,
    )
    .expect("a name survives an empty ledger");
    world
        .ledger
        .commit(name_fact(&candidates[idx]), &world.registry)
        .expect("name commits");

    let sun = MiniSun;
    let ctx = ObserverContext::at(vale, WorldTime { day: 0.0 });
    let seen = observe(&[&sun], &ctx);
    world
        .ledger
        .commit(
            Fact {
                subject: village,
                predicate: "revered-phenomenon".to_string(),
                object: Value::Text(seen[0].kind.clone()),
                place: Some(vale),
                day: Some(0.0),
                provenance: "religion".to_string(),
            },
            &world.registry,
        )
        .expect("belief commits");

    let village_name = match world.ledger.value_of(village, "name") {
        Some(Value::Text(n)) => n.clone(),
        _ => unreachable!("name was just committed"),
    };

    let mut doc = String::new();
    doc.push_str("### The world of seed 42, in its entirety\n\n");
    doc.push_str(&format!(
        "Under a golden sun fixed at zenith, in an unnamed vale, stands the \
         goblin village of **{village_name}**. Its people revere the most \
         salient phenomenon their sky offers them.\n\n"
    ));
    doc.push_str("The complete fact ledger:\n\n");
    doc.push_str("| Subject | Predicate | Object | Asserted by |\n");
    doc.push_str("|---------|-----------|--------|-------------|\n");
    doc.push_str(&render_fact_rows(&world.ledger, village, &village_name));
    doc.push_str(&format!(
        "\nEntities: {} (the vale), {} (the village). Facts: {}. \
         That is everything. Every rerun of this document produces these \
         exact bytes.\n",
        vale.0,
        village.0,
        world.ledger.len()
    ));
    doc
}

/// Render every fact about the village as markdown table rows.
fn render_fact_rows(ledger: &Ledger, village: EntityId, village_name: &str) -> String {
    ledger
        .facts_about(village)
        .map(|f| {
            let object = match &f.object {
                Value::Text(t) => t.clone(),
                Value::Number(n) => n.to_string(),
                Value::Flag(b) => b.to_string(),
                Value::Entity(e) => format!("entity {}", e.0),
            };
            format!(
                "| {village_name} | {} | {} | {} |\n",
                f.predicate, object, f.provenance
            )
        })
        .collect()
}

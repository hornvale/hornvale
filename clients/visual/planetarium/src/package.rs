//! Study packaging is independent of the renderer and native source transport.
use crate::shots::{FilmDefinition, sample_caption, sample_shot};
use hornvale_bevy_view::{Binding, CameraPose, ObservationMirror};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::{
    collections::BTreeMap,
    fs,
    io::Write,
    path::{Component, Path, PathBuf},
    process::Command,
};

#[derive(Debug)]
pub enum PackageError {
    Io(String),
    Manifest(String),
    Hash(String),
    Encode(String),
}
impl std::fmt::Display for PackageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (kind, text) = match self {
            Self::Io(s) => ("io", s),
            Self::Manifest(s) => ("manifest", s),
            Self::Hash(s) => ("hash", s),
            Self::Encode(s) => ("encode", s),
        };
        write!(f, "{kind}: {text}")
    }
}
impl std::error::Error for PackageError {}
impl From<std::io::Error> for PackageError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e.to_string())
    }
}
impl From<serde_json::Error> for PackageError {
    fn from(e: serde_json::Error) -> Self {
        Self::Manifest(e.to_string())
    }
}
impl From<hornvale_bevy_view::ViewError> for PackageError {
    fn from(e: hornvale_bevy_view::ViewError) -> Self {
        Self::Manifest(e.to_string())
    }
}
pub fn hash(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}
fn require(ok: bool, message: impl Into<String>) -> Result<(), PackageError> {
    if ok {
        Ok(())
    } else {
        Err(PackageError::Manifest(message.into()))
    }
}
/// Reject every symlink component, even one currently pointing inside the root.
fn member(root: &Path, name: &str) -> Result<PathBuf, PackageError> {
    require(!name.is_empty(), "empty package path")?;
    require(
        !fs::symlink_metadata(root)
            .map_err(|e| PackageError::Io(format!("{name}: {e}")))?
            .file_type()
            .is_symlink(),
        "symlink package root",
    )?;
    let mut path = root.to_path_buf();
    for part in Path::new(name).components() {
        let Component::Normal(part) = part else {
            return Err(PackageError::Manifest(format!(
                "unsafe package path {name}"
            )));
        };
        path.push(part);
        let info =
            fs::symlink_metadata(&path).map_err(|e| PackageError::Io(format!("{name}: {e}")))?;
        require(
            !info.file_type().is_symlink(),
            format!("symlink package path {name}"),
        )?;
    }
    require(path.is_file(), format!("not a package file: {name}"))?;
    Ok(path)
}
fn read(root: &Path, name: &str) -> Result<Vec<u8>, PackageError> {
    Ok(fs::read(member(root, name)?)?)
}
fn document<T: serde::de::DeserializeOwned>(root: &Path, name: &str) -> Result<T, PackageError> {
    Ok(serde_json::from_slice(&read(root, name)?)?)
}
fn checked(root: &Path, name: &str, expected: &str) -> Result<Vec<u8>, PackageError> {
    let bytes = read(root, name)?;
    if hash(&bytes) != expected {
        return Err(PackageError::Hash(name.into()));
    }
    Ok(bytes)
}
fn write_new(root: &Path, name: &str, bytes: &[u8]) -> Result<(), PackageError> {
    let mut f = fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(root.join(name))?;
    f.write_all(bytes)?;
    f.sync_all()?;
    Ok(())
}
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationTime {
    pub numerator: u32,
    pub denominator: u32,
}
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FrameRecord {
    pub frame: u32,
    pub presentation_time: PresentationTime,
    pub request_id: u64,
    pub ticks: i64,
    pub camera: CameraPose,
    pub caption: String,
    pub file: String,
    pub png_sha256: String,
    pub observation_file: String,
    pub observation_sha256: String,
    pub capture_seconds: f64,
    pub frame_seconds: f64,
    pub elapsed_seconds: f64,
}
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Manifest {
    pub schema: String,
    pub binding: Binding,
    pub width: u32,
    pub height: u32,
    pub fps: u32,
    pub frames: u32,
    pub files: BTreeMap<String, String>,
    pub provenance: Value,
    pub color: Value,
    pub ffmpeg: String,
    pub ffprobe: String,
    pub source_model_limitations: Value,
}
/// The injected executable paths are also used by CPU failure tests; production
/// always calls the actual named tools. Arguments are never interpreted by a shell.
pub struct EncodeTools<'a> {
    pub ffmpeg: &'a Path,
    pub ffprobe: &'a Path,
}
impl Default for EncodeTools<'_> {
    fn default() -> Self {
        Self {
            ffmpeg: Path::new("ffmpeg"),
            ffprobe: Path::new("ffprobe"),
        }
    }
}
const FILTER: &str = "scale=in_range=full:out_range=limited:out_color_matrix=bt709,format=yuv420p,setparams=range=limited:color_primaries=bt709:color_trc=iec61966-2-1:colorspace=bt709";
fn color() -> Value {
    json!({"delivery":"SDR", "input":"RGB8 sRGB full range PNG", "filter":FILTER,"matrix":"bt709","primaries":"bt709","transfer":"iec61966-2-1","range":"tv","pixel_format":"yuv420p","note":"sRGB transfer preserved; BT.709 matrix, limited-range YCbCr. PNG originals retained; no HDR claim."})
}
fn run(tool: &Path, args: &[&str], root: &Path) -> Result<Vec<u8>, PackageError> {
    let result = Command::new(tool)
        .args(args)
        .current_dir(root)
        .output()
        .map_err(|e| PackageError::Encode(format!("{}: {e}", tool.display())))?;
    if !result.status.success() {
        return Err(PackageError::Encode(format!(
            "{} {}: {}",
            tool.display(),
            result.status,
            String::from_utf8_lossy(&result.stderr)
        )));
    }
    Ok(result.stdout)
}
fn probe(root: &Path, tool: &Path) -> Result<(), PackageError> {
    let bytes = run(
        tool,
        &[
            "-v",
            "error",
            "-count_frames",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=width,height,avg_frame_rate,nb_read_frames,pix_fmt,color_space,color_transfer,color_primaries,color_range",
            "-of",
            "json",
            "study.mp4",
        ],
        root,
    )?;
    let doc: Value = serde_json::from_slice(&bytes)?;
    let streams = doc["streams"]
        .as_array()
        .ok_or_else(|| PackageError::Encode("probe streams missing".into()))?;
    if streams.len() != 1 {
        return Err(PackageError::Encode("probe stream count".into()));
    }
    let s = &streams[0];
    for (key, expected) in [
        ("width", json!(3840)),
        ("height", json!(2160)),
        ("avg_frame_rate", json!("30/1")),
        ("nb_read_frames", json!("300")),
        ("pix_fmt", json!("yuv420p")),
        ("color_space", json!("bt709")),
        ("color_transfer", json!("iec61966-2-1")),
        ("color_primaries", json!("bt709")),
        ("color_range", json!("tv")),
    ] {
        if s[key] != expected {
            return Err(PackageError::Encode(format!(
                "probe {key}: expected {expected}, got {}",
                s[key]
            )));
        }
    }
    Ok(())
}
fn verify_content(root: &Path, marker: bool, tools: &EncodeTools<'_>) -> Result<(), PackageError> {
    require(
        !root.join("FAILED").exists(),
        "failed capture cannot be complete",
    )?;
    let manifest_bytes = read(root, "manifest.json")?;
    if marker {
        let complete = read(root, "COMPLETE")?;
        require(
            complete == hash(&manifest_bytes).as_bytes(),
            "COMPLETE manifest hash mismatch",
        )?;
    }
    let m: Manifest = serde_json::from_slice(&manifest_bytes)?;
    require(m.schema == "visual/study/v1", "unknown manifest schema")?;
    require(
        (m.width, m.height, m.fps, m.frames) == (3840, 2160, 30, 300),
        "unqualified manifest film profile",
    )?;
    for (name, digest) in &m.files {
        checked(root, name, digest)?;
    }
    for name in [
        "source/world.json",
        "source/initial.json",
        "film.json",
        "frames.jsonl",
        "study.mp4",
        "provenance.json",
        "source-files.json",
        "development-source.patch",
        "assets/LibreBaskerville-Regular.ttf",
    ] {
        require(
            m.files.contains_key(name),
            format!("missing manifest member {name}"),
        )?;
    }
    let film: FilmDefinition = document(root, "film.json")?;
    let initial = String::from_utf8(read(root, "source/initial.json")?)
        .map_err(|e| PackageError::Manifest(e.to_string()))?;
    let mut mirror = ObservationMirror::new(&initial)?;
    film.validate(&mirror.initial().binding)?;
    require(m.binding == film.binding, "manifest/film binding mismatch")?;
    require(
        hash(&read(root, "source/world.json")?) == m.binding.world_sha256,
        "world binding mismatch",
    )?;
    let world: Value = document(root, "source/world.json")?;
    require(
        world["seed"].as_u64() == Some(mirror.initial().tiles.seed),
        "world/initial seed mismatch",
    )?;
    let provenance: Value = document(root, "provenance.json")?;
    require(m.provenance == provenance, "manifest provenance mismatch")?;
    require(
        provenance["source_revision"] == m.binding.source_revision,
        "provenance source revision mismatch",
    )?;
    let clean = provenance["rendering_source_tree_clean"]
        .as_bool()
        .ok_or_else(|| PackageError::Manifest("missing clean provenance".into()))?;
    for field in [
        "build_revision",
        "executable_sha256",
        "rustc",
        "os",
        "renderer",
        "gpu",
        "backend",
    ] {
        require(
            provenance[field]
                .as_str()
                .is_some_and(|s| !s.trim().is_empty()),
            format!("missing provenance {field}"),
        )?;
    }
    let hexadecimal = |v: &Value, length| {
        v.as_str().is_some_and(|s| {
            s.len() == length
                && s.bytes()
                    .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
        })
    };
    require(
        hexadecimal(&provenance["build_revision"], 40)
            && hexadecimal(&provenance["executable_sha256"], 64),
        "malformed build identity",
    )?;
    require(
        provenance["source_patch_sha256"] == hash(&read(root, "development-source.patch")?),
        "source patch identity mismatch",
    )?;
    require(
        provenance["history"] == hornvale_bevy_view::HISTORY_RESET_POLICY,
        "history policy mismatch",
    )?;
    require(
        provenance["cosmetic_treatments"]
            .as_array()
            .is_some_and(|a| !a.is_empty()),
        "missing cosmetic treatments",
    )?;
    let inventory: Vec<crate::provenance::SourceFile> = document(root, "source-files.json")?;
    require(
        inventory
            .iter()
            .any(|f| f.path == "clients/visual/planetarium/src/main.rs"),
        "empty or unexpected rendering source inventory",
    )?;
    if clean {
        require(
            provenance["build_tree_clean"] == true
                && provenance["working_tree_status"] == ""
                && provenance["head"] == m.binding.source_revision
                && provenance["build_revision"] == m.binding.source_revision
                && provenance["purpose"] == "clean pinned study",
            "contradictory clean build provenance",
        )?;
    } else {
        require(
            provenance["purpose"] == "dirty or unpinned development study",
            "development provenance must be labeled",
        )?;
    }
    require(
        provenance["font_sha256"] == hash(&read(root, "assets/LibreBaskerville-Regular.ttf")?),
        "font asset identity mismatch",
    )?;
    require(
        serde_json::from_value::<hornvale_bevy_view::camera::ViewSettings>(
            provenance["view_settings"].clone(),
        )? == film.settings
            && provenance["presentation_seed"] == film.presentation_seed,
        "presentation settings mismatch",
    )?;
    require(
        provenance["settings"]["warmup_frames"] == 3
            && provenance["settings"]["frames"] == film.frames
            && provenance["settings"]["width"] == film.width
            && provenance["settings"]["height"] == film.height
            && provenance["settings"]["timeout_seconds"] == 120,
        "capture settings mismatch",
    )?;
    require(m.color == color(), "unqualified color conversion")?;
    let records = String::from_utf8(read(root, "frames.jsonl")?)
        .map_err(|e| PackageError::Manifest(e.to_string()))?;
    let records = records
        .lines()
        .map(serde_json::from_str::<FrameRecord>)
        .collect::<Result<Vec<_>, _>>()?;
    require(records.len() == film.frames as usize, "frame record count")?;
    // Independently decode every video frame to a small RGB witness. This tests
    // lossy video/PNG correspondence, beyond accepting a valid codec and count.
    // The bounded 64x36 witnesses total only ~2 MiB for the qualified film.
    probe(root, tools.ffprobe)?;
    let video_rgb = run(
        tools.ffmpeg,
        &[
            "-v",
            "error",
            "-i",
            "study.mp4",
            "-vf",
            "scale=64:36:flags=area:in_color_matrix=bt709:in_range=limited:out_range=full,format=rgb24",
            "-f",
            "rawvideo",
            "-",
        ],
        root,
    )?;
    const WITNESS_BYTES: usize = 64 * 36 * 3;
    require(
        video_rgb.len() == records.len() * WITNESS_BYTES,
        "decoded video frame count",
    )?;
    for (index, r) in records.iter().enumerate() {
        let index = index as u32;
        require(r.frame == index, format!("frame index/order at {index}"))?;
        require(
            r.presentation_time.numerator == index && r.presentation_time.denominator == film.fps,
            format!("presentation time at {index}"),
        )?;
        require(
            r.ticks == film.clock().tick_at(index)? && r.request_id == u64::from(index),
            format!("exact ticks/request at {index}"),
        )?;
        require(
            r.file == format!("frames/{index:06}.png")
                && r.observation_file == format!("source/observations/{index:06}.json"),
            format!("frame mapping at {index}"),
        )?;
        let observation = checked(root, &r.observation_file, &r.observation_sha256)?;
        mirror.request(r.ticks)?;
        let observation =
            std::str::from_utf8(&observation).map_err(|e| PackageError::Manifest(e.to_string()))?;
        require(
            mirror.accept(observation)?,
            format!("observation rejected at {index}"),
        )?;
        require(
            serde_json::to_value(&r.camera)?
                == serde_json::to_value(sample_shot(
                    &film,
                    index,
                    &crate::live::positions(&mirror),
                )?)?,
            format!("camera at {index}"),
        )?;
        require(
            r.caption == sample_caption(&film, index)?,
            format!("caption at {index}"),
        )?;
        require(
            m.source_model_limitations
                == serde_json::to_value(&mirror.current().unwrap().astronomy.models)?,
            "source model limitations mismatch",
        )?;
        let png = checked(root, &r.file, &r.png_sha256)?;
        let image = image::load_from_memory_with_format(&png, image::ImageFormat::Png)
            .map_err(|e| PackageError::Manifest(format!("PNG {}: {e}", r.file)))?;
        require(
            image.width() == film.width
                && image.height() == film.height
                && image.color() == image::ColorType::Rgb8,
            format!("PNG dimensions/format at {index}"),
        )?;
        let reference = image
            .resize_exact(64, 36, image::imageops::FilterType::Triangle)
            .to_rgb8();
        let decoded =
            &video_rgb[index as usize * WITNESS_BYTES..(index as usize + 1) * WITNESS_BYTES];
        let difference: u64 = reference
            .as_raw()
            .iter()
            .zip(decoded)
            .map(|(a, b)| u64::from(a.abs_diff(*b)))
            .sum();
        require(
            difference as f64 / WITNESS_BYTES as f64 <= 8.0,
            format!("video/PNG correspondence at {index}"),
        )?;
    }
    for (dir, count) in [("frames", 300), ("source/observations", 300)] {
        require(
            fs::read_dir(root.join(dir))?.count() == count,
            format!("unexpected files in {dir}"),
        )?;
    }
    Ok(())
}
/// Public verification always requires the durable completion marker.
pub fn verify_package(directory: &Path) -> Result<(), PackageError> {
    // Check this first so standalone interrupted-output diagnostics are useful.
    read(directory, "COMPLETE")?;
    verify_with_tools(directory, &EncodeTools::default())
}
/// Same independent verifier with explicit external-media executables.
pub fn verify_with_tools(directory: &Path, tools: &EncodeTools<'_>) -> Result<(), PackageError> {
    read(directory, "COMPLETE")?;
    verify_content(directory, true, tools)
}
/// Encode a complete capture, author manifest last, then verify before publishing.
pub fn finish_package(root: &Path) -> Result<(), PackageError> {
    finish_with_tools(root, &EncodeTools::default())
}
pub fn finish_with_tools(root: &Path, tools: &EncodeTools<'_>) -> Result<(), PackageError> {
    require(
        !root.join("COMPLETE").exists()
            && !root.join("manifest.json").exists()
            && !root.join("study.mp4").exists(),
        "package output already exists",
    )?;
    let film: FilmDefinition = document(root, "film.json")?;
    film.validate(&film.binding)?;
    // Validate frame membership BEFORE handing the pattern to another process.
    for index in 0..film.frames {
        member(root, &format!("frames/{index:06}.png"))?;
        member(root, &format!("source/observations/{index:06}.json"))?;
    }
    run(
        tools.ffmpeg,
        &[
            "-nostdin",
            "-v",
            "error",
            "-framerate",
            "30",
            "-start_number",
            "0",
            "-i",
            "frames/%06d.png",
            "-frames:v",
            "300",
            "-vf",
            FILTER,
            "-c:v",
            "libx264",
            "-crf",
            "16",
            "-pix_fmt",
            "yuv420p",
            "-color_range",
            "tv",
            "-colorspace",
            "bt709",
            "-color_primaries",
            "bt709",
            "-color_trc",
            "iec61966-2-1",
            "-movflags",
            "+faststart",
            "-n",
            "study.mp4",
        ],
        root,
    )?;
    member(root, "study.mp4")?;
    probe(root, tools.ffprobe)?;
    let mut files = BTreeMap::new();
    for name in [
        "source/world.json",
        "source/initial.json",
        "film.json",
        "frames.jsonl",
        "study.mp4",
        "provenance.json",
        "source-files.json",
        "development-source.patch",
        "assets/LibreBaskerville-Regular.ttf",
    ] {
        files.insert(name.into(), hash(&read(root, name)?));
    }
    let observation: Value = document(root, "source/observations/000000.json")?;
    let version = |tool| -> Result<String, PackageError> {
        Ok(String::from_utf8_lossy(&run(tool, &["-version"], root)?)
            .lines()
            .next()
            .unwrap_or("")
            .into())
    };
    let manifest = Manifest {
        schema: "visual/study/v1".into(),
        binding: film.binding,
        width: film.width,
        height: film.height,
        fps: film.fps,
        frames: film.frames,
        files,
        provenance: document(root, "provenance.json")?,
        color: color(),
        ffmpeg: version(tools.ffmpeg)?,
        ffprobe: version(tools.ffprobe)?,
        source_model_limitations: observation["astronomy"]["models"].clone(),
    };
    let bytes = serde_json::to_vec_pretty(&manifest)?;
    write_new(root, "manifest.json", &bytes)?;
    verify_content(root, false, tools)?;
    // Never invent a temporary success marker to run verification. Rename the
    // fully synced marker only after exactly the same content checks pass.
    write_new(root, ".COMPLETE.pending", hash(&bytes).as_bytes())?;
    fs::rename(root.join(".COMPLETE.pending"), root.join("COMPLETE"))?;
    if let Err(error) = fs::File::open(root).and_then(|directory| directory.sync_all()) {
        fs::remove_file(root.join("COMPLETE"))?;
        return Err(error.into());
    }
    Ok(())
}

#[test]
fn standalone_verify_requires_completion_not_capture_inputs() {
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_planetarium"))
        .args(["verify", "--out", "/nonexistent-planetarium-package"])
        .output()
        .unwrap();
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(error.contains("COMPLETE"), "{error}");
}

#[test]
fn source_inventory_is_anchored_from_client_subdirectory() {
    let client = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap();
    let evidence = planetarium::provenance::source_state(client).unwrap();
    assert!(
        evidence
            .files
            .iter()
            .any(|f| f.path == "clients/visual/planetarium/src/main.rs")
    );
    assert!(!evidence.files.is_empty());
}

use planetarium::{
    package::{self, EncodeTools, Manifest, hash},
    shots::{FilmDefinition, sample_shot},
};
use serde_json::{Value, json};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::atomic::{AtomicU64, Ordering},
};
static NEXT: AtomicU64 = AtomicU64::new(0);
struct Fixture {
    root: PathBuf,
    probe: PathBuf,
    encoder: PathBuf,
}
impl Drop for Fixture {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.root).unwrap();
    }
}
fn put(root: &Path, name: &str, value: &Value) {
    fs::write(root.join(name), serde_json::to_vec_pretty(value).unwrap()).unwrap();
}
#[cfg(unix)]
fn script(path: &Path, body: &str) {
    use std::os::unix::fs::PermissionsExt;
    fs::write(path, format!("#!/bin/sh\n{body}\n")).unwrap();
    fs::set_permissions(path, fs::Permissions::from_mode(0o700)).unwrap();
}
impl Fixture {
    fn new() -> Self {
        let root = std::env::temp_dir().join(format!(
            "planetarium-package-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::SeqCst)
        ));
        fs::create_dir(&root).unwrap();
        for dir in ["source/observations", "frames", "assets"] {
            fs::create_dir_all(root.join(dir)).unwrap();
        }
        let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let world =
            fs::read(manifest_dir.join("../../../cli/tests/fixtures/world-seed-42.json")).unwrap();
        fs::write(root.join("source/world.json"), world).unwrap();
        let mut initial: Value =
            serde_json::from_str(include_str!("../../../bevy/tests/fixtures/initial.json"))
                .unwrap();
        initial["binding"]["world_sha256"] =
            json!(hash(&fs::read(root.join("source/world.json")).unwrap()));
        put(&root, "source/initial.json", &initial);
        let mut film: FilmDefinition =
            serde_json::from_str(include_str!("../../films/pilot.json")).unwrap();
        film.binding = serde_json::from_value(initial["binding"].clone()).unwrap();
        // A static-tick film still uses the public qualified 4K / 30 / 300 profile.
        film.end_ticks = 0;
        film.validate(&film.binding).unwrap();
        put(&root, "film.json", &serde_json::to_value(&film).unwrap());
        fs::write(
            root.join("assets/LibreBaskerville-Regular.ttf"),
            include_bytes!("../../assets/LibreBaskerville-Regular.ttf"),
        )
        .unwrap();
        put(
            &root,
            "source-files.json",
            &json!([{"path":"clients/visual/planetarium/src/main.rs","sha256":"c".repeat(64)}]),
        );
        fs::write(root.join("development-source.patch"), b"test fixture").unwrap();
        put(
            &root,
            "provenance.json",
            &json!({"history":hornvale_bevy_view::HISTORY_RESET_POLICY,"source_patch_sha256":hash(b"test fixture"),"cosmetic_treatments":["synthetic black PNG test data"],"source_revision":film.binding.source_revision,"rendering_source_tree_clean":false,"purpose":"dirty or unpinned development study","build_revision":"a".repeat(40),"executable_sha256":"b".repeat(64),"rustc":"synthetic CPU fixture","os":"fixture","renderer":"fixture","gpu":"no GPU: synthetic fixture","backend":"none","font_sha256":hash(include_bytes!("../../assets/LibreBaskerville-Regular.ttf")),"view_settings":film.settings,"presentation_seed":42,"settings":{"width":3840,"height":2160,"frames":300,"warmup_frames":3,"timeout_seconds":120}}),
        );
        // Flat compressed full-size PNG, hardlinked only to keep fixture disk tiny.
        image::RgbImage::new(3840, 2160)
            .save(root.join("frames/000000.png"))
            .unwrap();
        let png_hash = hash(&fs::read(root.join("frames/000000.png")).unwrap());
        let mut reply: Value =
            serde_json::from_str(include_str!("../../../bevy/tests/fixtures/reply.json")).unwrap();
        reply["binding"] = initial["binding"].clone();
        let positions = reply["astronomy"]["bodies"]
            .as_array()
            .unwrap()
            .iter()
            .map(|b| {
                (
                    b["id"].as_str().unwrap().to_owned(),
                    serde_json::from_value(b["position_km"].clone()).unwrap(),
                )
            })
            .collect();
        let mut lines = String::new();
        for i in 0..300 {
            let png = format!("frames/{i:06}.png");
            if i > 0 {
                fs::hard_link(root.join("frames/000000.png"), root.join(&png)).unwrap();
            }
            reply["request_id"] = json!(i);
            let obs = format!("source/observations/{i:06}.json");
            put(&root, &obs, &reply);
            let record = json!({"frame":i,"presentation_time":{"numerator":i,"denominator":30},"request_id":i,"ticks":0,"camera":sample_shot(&film,i,&positions).unwrap(),"caption":film.shot(i).unwrap().caption,"file":png,"png_sha256":png_hash,"observation_file":obs,"observation_sha256":hash(&fs::read(root.join(&obs)).unwrap()),"capture_seconds":0.0,"frame_seconds":0.0,"elapsed_seconds":0.0});
            lines.push_str(&record.to_string());
            lines.push('\n');
        }
        fs::write(root.join("frames.jsonl"), lines).unwrap();
        let probe = root.join("probe");
        let encoder = root.join("encoder");
        script(
            &probe,
            "echo '{\"streams\":[{\"width\":3840,\"height\":2160,\"avg_frame_rate\":\"30/1\",\"nb_read_frames\":\"300\",\"pix_fmt\":\"yuv420p\",\"color_space\":\"bt709\",\"color_transfer\":\"iec61966-2-1\",\"color_primaries\":\"bt709\",\"color_range\":\"tv\"}]}'",
        );
        script(
            &encoder,
            "if [ \"$1\" = '-version' ]; then echo 'CPU fixture encoder'; elif [ \"$1\" = '-nostdin' ]; then case \"$*\" in *setparams=range=limited:color_primaries=bt709:color_trc=iec61966-2-1:colorspace=bt709*) echo 'synthetic video' > study.mp4 ;; *) echo 'frame color metadata missing' >&2; exit 8 ;; esac; else dd if=/dev/zero bs=6912 count=300 2>/dev/null; fi",
        );
        Self {
            root,
            probe,
            encoder,
        }
    }
    fn tools(&self) -> EncodeTools<'_> {
        EncodeTools {
            ffmpeg: &self.encoder,
            ffprobe: &self.probe,
        }
    }
    fn finish(&self) {
        package::finish_with_tools(&self.root, &self.tools()).unwrap();
    }
    fn verify(&self) -> Result<(), package::PackageError> {
        package::verify_with_tools(&self.root, &self.tools())
    }
    fn rehash(&self) {
        let mut m: Manifest =
            serde_json::from_slice(&fs::read(self.root.join("manifest.json")).unwrap()).unwrap();
        for (name, digest) in &mut m.files {
            *digest = hash(&fs::read(self.root.join(name)).unwrap());
        }
        let bytes = serde_json::to_vec_pretty(&m).unwrap();
        fs::write(self.root.join("manifest.json"), &bytes).unwrap();
        fs::write(self.root.join("COMPLETE"), hash(&bytes)).unwrap();
    }
    fn records(&self, f: impl FnOnce(&mut Vec<Value>)) {
        let mut rows: Vec<Value> = fs::read_to_string(self.root.join("frames.jsonl"))
            .unwrap()
            .lines()
            .map(|s| serde_json::from_str(s).unwrap())
            .collect();
        f(&mut rows);
        fs::write(
            self.root.join("frames.jsonl"),
            rows.iter().map(|r| format!("{r}\n")).collect::<String>(),
        )
        .unwrap();
        self.rehash();
    }
}
#[test]
fn full_profile_cpu_fixture_and_independent_corruption_checks() {
    let f = Fixture::new();
    assert!(f.verify().unwrap_err().to_string().contains("COMPLETE")); // after frame 299, before encode
    f.finish();
    f.verify().unwrap();
    fs::write(f.root.join("FAILED"), b"terminal source failure").unwrap();
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("failed capture")
    );
    fs::remove_file(f.root.join("FAILED")).unwrap();
    let original_marker = fs::read(f.root.join("COMPLETE")).unwrap();
    fs::remove_file(f.root.join("COMPLETE")).unwrap();
    assert!(f.verify().unwrap_err().to_string().contains("COMPLETE")); // encoded, not published
    fs::write(f.root.join("COMPLETE"), original_marker).unwrap();
    let saved_manifest = fs::read(f.root.join("manifest.json")).unwrap();
    let mut traversing: Manifest = serde_json::from_slice(&saved_manifest).unwrap();
    traversing
        .files
        .insert("../outside.json".into(), "0".repeat(64));
    let bytes = serde_json::to_vec_pretty(&traversing).unwrap();
    fs::write(f.root.join("manifest.json"), &bytes).unwrap();
    fs::write(f.root.join("COMPLETE"), hash(&bytes)).unwrap();
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("unsafe package path")
    );
    fs::write(f.root.join("manifest.json"), &saved_manifest).unwrap();
    fs::write(f.root.join("COMPLETE"), hash(&saved_manifest)).unwrap();
    let records = fs::read(f.root.join("frames.jsonl")).unwrap();
    f.records(|r| r[1]["frame"] = json!(0));
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("frame index/order")
    );
    fs::write(f.root.join("frames.jsonl"), &records).unwrap();
    f.rehash();
    f.records(|r| r[0]["camera"]["eye_km"][0] = json!(123));
    assert!(f.verify().unwrap_err().to_string().contains("camera"));
    fs::write(f.root.join("frames.jsonl"), &records).unwrap();
    f.rehash();
    f.records(|r| r[0]["caption"] = json!("invented"));
    assert!(f.verify().unwrap_err().to_string().contains("caption"));
    fs::write(f.root.join("frames.jsonl"), &records).unwrap();
    f.rehash();
    f.records(|r| r[0]["presentation_time"]["denominator"] = json!(24));
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("presentation time")
    );
    fs::write(f.root.join("frames.jsonl"), &records).unwrap();
    f.rehash();
    let obs = f.root.join("source/observations/000000.json");
    let original = fs::read(&obs).unwrap();
    let mut altered: Value = serde_json::from_slice(&original).unwrap();
    altered["ticks"] = json!(1);
    altered["astronomy"]["ticks"] = json!(1);
    fs::write(&obs, serde_json::to_vec(&altered).unwrap()).unwrap();
    f.records(|r| r[0]["observation_sha256"] = json!(hash(&fs::read(&obs).unwrap())));
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("pending observation")
    );
    fs::write(&obs, original).unwrap();
    fs::write(f.root.join("frames.jsonl"), &records).unwrap();
    f.rehash();
    let world = f.root.join("source/world.json");
    let original = fs::read(&world).unwrap();
    fs::write(&world, b"{}").unwrap();
    f.rehash();
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("world binding")
    );
    fs::write(&world, original).unwrap();
    f.rehash();
    let video = f.root.join("study.mp4");
    let original = fs::read(&video).unwrap();
    fs::write(&video, b"corrupt").unwrap();
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("hash: study.mp4")
    );
    fs::write(&video, original).unwrap();
    let encoder_original = fs::read(&f.encoder).unwrap();
    script(
        &f.encoder,
        "dd if=/dev/zero bs=6912 count=300 2>/dev/null | LC_ALL=C tr '\\000' '\\377'",
    );
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("video/PNG correspondence")
    );
    fs::write(&f.encoder, encoder_original).unwrap();
    let png = f.root.join("frames/000000.png");
    let original = fs::read(&png).unwrap();
    fs::write(&png, b"corrupt").unwrap();
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("hash: frames/000000.png")
    );
    fs::write(&png, original).unwrap();
    fs::remove_file(&png).unwrap();
    assert!(
        f.verify()
            .unwrap_err()
            .to_string()
            .contains("frames/000000.png")
    );
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(f.root.join("frames/000001.png"), &png).unwrap();
        assert!(f.verify().unwrap_err().to_string().contains("symlink"));
    }
}
#[test]
fn failed_encoders_and_probes_never_publish_completion() {
    for mode in [
        "missing",
        "exit",
        "codec",
        "probe",
        "rate",
        "count",
        "traversal",
    ] {
        let f = Fixture::new();
        match mode {
            "missing" => fs::remove_file(&f.encoder).unwrap(),
            "exit" => script(&f.encoder, "exit 7"),
            "codec" => script(&f.encoder, "echo 'Unknown encoder libx264' >&2; exit 1"),
            "probe" => script(&f.probe, "echo '{\"streams\":[{\"width\":1920}]}'"),
            "rate" | "count" => {
                let key = if mode == "rate" {
                    "avg_frame_rate"
                } else {
                    "nb_read_frames"
                };
                let mut data: Value = serde_json::from_str("{\"streams\":[{\"width\":3840,\"height\":2160,\"avg_frame_rate\":\"30/1\",\"nb_read_frames\":\"300\",\"pix_fmt\":\"yuv420p\",\"color_space\":\"bt709\",\"color_transfer\":\"iec61966-2-1\",\"color_primaries\":\"bt709\",\"color_range\":\"tv\"}]}").unwrap();
                data["streams"][0][key] = json!("wrong");
                script(&f.probe, &format!("echo '{data}'"));
            }
            "traversal" => {
                fs::remove_file(f.root.join("frames/000000.png")).unwrap();
                #[cfg(unix)]
                std::os::unix::fs::symlink("/tmp", f.root.join("frames/000000.png")).unwrap();
            }
            _ => unreachable!(),
        }
        let error = package::finish_with_tools(&f.root, &f.tools())
            .unwrap_err()
            .to_string();
        assert!(!f.root.join("COMPLETE").exists(), "{mode}: {error}");
        assert!(
            if mode == "traversal" {
                error.contains("symlink")
            } else {
                error.contains("encode:")
            },
            "{mode}: {error}"
        );
    }
}

#[test]
fn capture_refuses_existing_directory_without_touching_it() {
    let root = std::env::temp_dir().join(format!("planetarium-existing-{}", std::process::id()));
    fs::create_dir(&root).unwrap();
    fs::write(root.join("preserved"), b"interrupted evidence").unwrap();
    let film: FilmDefinition =
        serde_json::from_str(include_str!("../../films/pilot.json")).unwrap();
    let error = planetarium::capture::run(
        PathBuf::from("missing-world"),
        film.binding.source_revision.clone(),
        film,
        root.clone(),
        None,
    )
    .unwrap_err();
    assert!(error.to_string().contains("exist"), "{error}");
    assert_eq!(
        fs::read(root.join("preserved")).unwrap(),
        b"interrupted evidence"
    );
    assert_eq!(fs::read_dir(&root).unwrap().count(), 1);
    fs::remove_dir_all(root).unwrap();
}

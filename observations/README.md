# Observation production

Observation manifests and exported frame packets bind each episode to a seed,
world revision, and source digest. The packets are the reproducible data source;
rendered PNG frames are the authoritative visual output. MP4 films are a derived
local convenience and are never a publication command.

Validate and export an episode into scratch space:

```bash
cargo run -p hornvale -- observations validate \
  --manifest observations/episodes/HV-001.json
cargo run -p hornvale -- observations export \
  --manifest observations/episodes/HV-001.json \
  --out observations/render-output/HV-001/frames
```

After a renderer has written matching `frame-NNN.png` files beside the
`frame-NNN.json` packets, assemble and verify the local package:

```bash
scripts/observation-film.sh \
  --manifest observations/episodes/HV-001.json \
  --frames observations/render-output/HV-001/frames \
  --out observations/render-output/HV-001/package
```

The assembler verifies contiguous packet indices and episode identity before
writing anything. Its SHA-256 sidecar covers the exact manifest, every packet,
every PNG present, and the video when one is assembled. If `ffmpeg` is absent,
verification still succeeds and writes the sidecar without claiming a video.

An approved package consists of the exact manifest, frame checksum sidecar,
video checksum when a video is present, and final caption text reviewed as one
unit. Publication remains manual. No observation command contacts a social
network or treats generated caption drafts as approved copy.

Run `make observation-check` for manifest validation, deterministic export,
fixture byte checks, shell tests, and shell lint. It writes only to a temporary
directory and does not modify committed artifacts.

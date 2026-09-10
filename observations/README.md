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

The Atlas HTML preview is an inspectable review surface, not the authoritative
visual frame sequence. On a host with Firefox's headless screenshot executable,
render the existing self-contained Atlas laptop preview for every validated
packet at its declared `1440×900` viewport:

```bash
scripts/observation-render.sh \
  --manifest observations/episodes/HV-001.json \
  --frames observations/render-output/HV-001/frames
```

The command discovers `firefox` on `PATH` or the macOS Firefox application
bundle; set `HV_OBSERVATION_FIREFOX=/path/to/firefox` to name another local
executable. It refuses rather than emitting a substitute image if no backend
is available. Firefox's pixels are platform-local: compare repeat checksums on
the rendering host, but do not treat cross-platform byte identity as a client
contract.

The resulting `frame-NNN.png` files beside the `frame-NNN.json` packets are
the authoritative local visual output. Only after that raster sequence exists,
assemble and verify the local package:

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

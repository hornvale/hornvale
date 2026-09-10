# Visual clients

This independent Cargo workspace contains the scientific observation source,
the reusable Bevy view, and its first application, Planetarium. It has its own
lockfile and optimized development profiles. Rust 1.96.1 is selected explicitly
by the check commands; Bevy is pinned to 0.19.1 with default features disabled.
The simulation workspace does not depend on Bevy.

| Crate | Owns | Must never reach, including test/build dependencies |
|---|---|---|
| `hornvale-visual-source` (`source/`) | One immutable native world, cached contexts, serialized scientific observations | Bevy, the view, Planetarium |
| `hornvale-bevy-view` (`bevy/`) | Document mirror, identity, coordinates, camera and rendering/capture mechanisms | Simulation crates, the source, Planetarium |
| `planetarium` (`planetarium/`) | Composition, source worker, pilot film, captions, controls and study packaging | Libraries may be used by this application |

Run from the repository root:

```sh
make visual-check
python3 scripts/visual-dependencies.py
python3 scripts/test-visual-dependencies.py
```

`visual-check` times `visual-check-run`: client formatting, locked clippy with
all targets, locked workspace tests, and the resolved dependency guard and its
synthetic tests. Formatting changes directory because a virtual manifest passed
from the root does not select the client packages. The guard runs pinned
`cargo metadata --format-version 1 --locked --all-features`; it follows package
IDs through normal, build and dev edges, including renamed/transitive edges and
all resolved targets. Missing resolution is failure. No GPU or window is started
by these tests. Both invocation and log collection in `clients-check-run` include
this client; the canonical stage/merge clients phase supplies combined coverage.
The root commit gate alone does not qualify the client.

## Using the libraries

Open `Source::open(&world_path, full_revision, source_id)` once, call
`initial_document(tile_width)` once per required resolution, and feed that JSON
to `ObservationMirror::new`. Call the mirror's request method for an
exact tick, send that serialized request to `Source::observe`, and accept the
reply with the mirror before updating presentation. See `source/tests/suite.rs`
and `bevy/tests/suite.rs` for independent consumers that do not import Planetarium.
The source lazily builds terrain once and caches each static resolution;
astronomical requests reuse the initialized native context.

Bindings include source ID, observation scope, world-byte SHA-256 and full source
revision. Requests/replies carry an ID and exact tick. Old replies cannot replace
a newer request; switching the binding discards incompatible scene state. The
view neither loads a world nor fetches a default scientific source. Another
application can provide these serialized documents and its own camera/time
composition without taking Planetarium's film, captions or source worker.

The current source is explicitly `scientific:unrestricted`. A future situated
game must supply its own observer/session-limited producer and permitted display
mirror. Hiding unrestricted facts in a UI is not that boundary. Gameplay commands,
consequences and allowed time controls belong to that future application. No
second renderer or situated graphical game is implemented here.

## Native Linux prerequisites

The [pinned Bevy Linux requirements](https://github.com/bevyengine/bevy/blob/v0.19.1/docs/linux_dependencies.md)
identify the C/C++ toolchain, pkg-config and X11 libraries. This workspace enables
X11, without audio, gamepad or Wayland features. Its lockfile/feature resolution
is the authority for enabled dependencies; missing libraries must fail compilation.
The canonical preflight on 2026-09-10 found Rust 1.96.1 and pkg-config versions
x11 1.8.4, xcursor 1.2.1, xi 1.8, xrandr 1.5.2 and xkbcommon 1.5.0.
That establishes installed development libraries, not a successful Linux build.
The queued clients phase must compile and test this workspace on lefford.
GPU runtime additionally requires a working graphics backend/driver and display;
CPU-gate success does not establish those.

## Running and qualifying Planetarium

The [client guide](../../book/src/clients/planetarium.md) describes controls,
physical/model limits, exact clocks and study commands. The
[evaluated schema](../../book/src/reference/scene-astronomy-at-v1.md) documents the
native authority. The [package evidence](../../docs/audits/the-planetarium/package.md)
names the actual qualified capture revision, hashes and machine.

GPU qualification is explicit: run `planetarium inspect`, then a fresh full
`planetarium capture`, followed by `planetarium verify --out DIR` using the
commands in the client guide. Record the selected GPU/backend, build revision,
toolchain, frame/capture timing, complete package hashes, full moving review and
representative full-resolution/phone-size stills. GPU absence or failed readback
is missing qualification, never a passing skip. Cross-host GPU byte identity is
not promised. The technical verifier does not approve visual quality or publication.

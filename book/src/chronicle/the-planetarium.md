# The Planetarium

**September 2026 · implementation review candidate; final visual acceptance and merge pending**

The Observation Series made a checked route from simulation data to a movie,
but its rasterized text layouts did not deliver the intended visual experience.
The Planetarium builds the missing scene: a native Bevy astronomical observer
whose interactive view and directed film share bodies, lighting, materials and
camera primitives. Its pilot moves from a whole world through its illuminated
surface to a moon, with three short captions and no technical overlay in the film.

The reusable work lives outside the application. A native source library opens
one saved world and evaluates requested instants. A Bevy view library accepts
serialized observations without depending on the simulation. Planetarium composes
them with its film, controls and package verifier. Another application can use
those libraries without adopting this film. A situated game still needs a source
that enforces what its observer is permitted to know.

## The picture has a physical reference

The first prerequisite was a radius. Hornvale had an anchor-world mass but no
physical anchor radius, so assigning a convenient sphere size would have made
all size comparisons cosmetic. The source now exposes a documented Earth-like
mass–radius estimate, restricted to its supported mass interval. It adds no
random draws, stored world field or dynamical feedback. The model's composition
assumption remains visible rather than becoming a claim about a simulated interior.

The native evaluated astronomy document supplies physical positions, illumination
and orientation at exact integer ticks. The renderer does not integrate a second
orbit. Body-size and distance ratios remain physical within each shot; terrain
uses source elevation relative to the sea datum without vertical exaggeration.
Fine surface appearance, cloud shapes, atmospheric treatment, exposure and focus
remain disclosed presentation choices. The film interval has an independent
native eclipse-avoidance witness; the picture does not validate eclipse shadows.

The orbital camera also has an explicit boundary. Testing visible surface points
found that a center-only precision check could pass while near-surface geometry
failed. The supported view stays at least twice the body's outer radius from its
center. A finite grid of 6,342 visible points measured a worst projection error
of 0.101748006 pixels within the qualified range. This is evidence for an orbital
view, not a promise of walkable detail.

## A movie that can be checked

The pilot records 300 frames at 3840×2160 and 30 fps over ten seconds. Each frame
carries its exact observation, camera and caption state; capture waits for assets,
render readiness and acknowledged GPU readback. A package binds its world, film,
source revision, build state, assets, frames and encoded movie. The verifier checks
semantic replay, image hashes and dimensions, the video profile and coarse decoded
video correspondence. Only successful verification writes COMPLETE.

The final clean qualification produced all 300 frames in 137.779 seconds on Apple
M1 Max/Metal; capture through encoding and internal verification took 189.18
seconds. A separate verifier and independent full-frame inspection passed. The
actual ten-second movie was played and reviewed at full and phone size. The final
moon refinement adds subtle, stable cosmetic shading while retaining native
positions, physical scale and the authored cameras.

The 60-second 1080p interactive script measured a 20.390 ms p95 frame interval,
meeting the 33.33 ms target. Its 43.588 ms p99 and 154.991 ms maximum remain
recorded: other campaign builds were active on the desktop. First readiness from
program entry took 3.027 seconds. Two fresh renderers produced identical pixels
for the sixteen repeated frame requests; this is a measured result on this Mac,
not a guarantee for every GPU. Manual mouse-drag delivery through the review tool
remains unproven. Nathan's visual acceptance and merge remain pending.

The [client guide](../clients/planetarium.md) documents operation and limitations.
The [campaign ledger](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/ledgers/2026-09-10-the-planetarium.md)
and [final visual audit](https://github.com/hornvale/hornvale/blob/main/docs/audits/the-planetarium/final-review.md)
keep the measured provenance. Technical completion does not grant publication or
settle whether this is the visual quality the project wants.

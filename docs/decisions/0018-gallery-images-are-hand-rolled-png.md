# 0018. Gallery images are hand-rolled PNG

**Status:** Accepted (2026-07-07) · **Decider:** Nathan

In the context of gallery raster artifacts committed as P6 PPM (plus one
BMP), which browsers and GitHub cannot display inline, facing the
no-new-dependencies rule (decision 0004), we decided that **committed
raster artifacts are PNG emitted by a hand-rolled, pure-std encoder using
stored (uncompressed) deflate blocks and table-driven CRC32**, accepting
files larger than a compressing encoder would produce.

**Context.** PNG's only hard requirements are a CRC32 (≈30 lines) and a
zlib stream, and zlib permits stored deflate blocks — so a valid,
browser-viewable PNG encoder is ≈120 lines of std-only Rust. At gallery
resolutions (256×128) the size penalty is irrelevant, and the output is
deterministic bytes (fixed chunk layout), so CI's drift check holds
unchanged.

**Consequence.** A shared encoder lands in the kernel (or wherever render
utilities converge); domain `*_ppm` renders gain `*_png` siblings; the
committed PPM/BMP gallery artifacts migrate in a chore pass and CI's
"Artifacts are current" command list updates with them. PPM may survive as
an internal intermediate, but what the gallery commits is PNG. Reaching
for the `png`/`image` crates stays off the table (0004).

**See also.** Decision 0004 (no new dependencies); the frontier's
expressive-culture cluster, which already commits to hand-rolled
deterministic PCM for audio on the same reasoning.

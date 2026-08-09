#!/usr/bin/env bash
# hornvale-game-core must never gain a hornvale dependency. The containment
# in The Quire spec section 6 is structural — this asserts the structure has
# not quietly changed.
#
# --all-features: a dependency declared `optional = true` is only resolved
# into `cargo tree`'s output once the feature that activates it is turned
# on, so a plain `cargo tree` call would resolve it as absent and print
# "ok" even with a hornvale crate one manifest line away from being live.
# `--all-features` forces every optional dependency (and every dependency
# gated behind a non-default feature) into the resolve, so this check's
# actual contract is "no hornvale-* crate reachable under ANY feature
# combination this crate could ever be built with" — not merely "not
# reachable in the default build". It does not widen the check along any
# other axis (still host-target only, still this one manifest), and this
# crate carries no [features] table today, so today the two invocations
# resolve identically; the flag is here for the day one is added.
set -euo pipefail
if cargo tree --manifest-path clients/game/core/Cargo.toml --all-features --prefix none \
   | grep -v '^hornvale-game-core ' | grep -q '^hornvale-'; then
  echo "FAIL: hornvale-game-core has gained a hornvale dependency." >&2
  cargo tree --manifest-path clients/game/core/Cargo.toml --all-features >&2
  exit 1
fi
echo "ok: hornvale-game-core has no hornvale dependency"

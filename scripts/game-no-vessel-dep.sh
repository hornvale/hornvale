#!/usr/bin/env bash
# hornvale-game-core must never gain a hornvale dependency. The containment
# in The Quire spec section 6 is structural — this asserts the structure has
# not quietly changed.
set -euo pipefail
if cargo tree --manifest-path clients/game/core/Cargo.toml --prefix none \
   | grep -v '^hornvale-game-core ' | grep -q '^hornvale-'; then
  echo "FAIL: hornvale-game-core has gained a hornvale dependency." >&2
  cargo tree --manifest-path clients/game/core/Cargo.toml >&2
  exit 1
fi
echo "ok: hornvale-game-core has no hornvale dependency"

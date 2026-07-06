# The Stream Manifest

Every random decision in Hornvale flows from the world seed through a chain
of *labeled derivations*. Those labels are permanent save-format contracts:
renaming one silently re-rolls everything downstream of it in every world
(the Bolnar/Gruugish lesson). This manifest is the complete registry of the
labels in use — generated, drift-checked in CI, and reviewed at each
campaign close alongside the concept registry. Deliberate regeneration of a
stream is an **epoch bump** (`settlement/name/v2`), never a rename.

{{#include stream-manifest-generated.md}}

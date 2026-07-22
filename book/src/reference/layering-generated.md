<!-- GENERATED FILE — do not edit. Emitted and drift-checked by cli/tests/architecture.rs (the layering enforcer); accept a deliberate graph change with REBASELINE=1 (make rebaseline-goldens). -->

```text
kernel  →  domains/*  →  windows/*  →  cli
```

| crate | layer | workspace dependencies | dev/build-only extras |
|---|---|---|---|
| hornvale-kernel | kernel | — | — |
| hornvale-astronomy | domains | hornvale-kernel | — |
| hornvale-climate | domains | hornvale-kernel | — |
| hornvale-culture | domains | hornvale-kernel | — |
| hornvale-demography | domains | hornvale-kernel | — |
| hornvale-history | domains | hornvale-kernel | — |
| hornvale-language | domains | hornvale-kernel | — |
| hornvale-paleoclimate | domains | hornvale-kernel | — |
| hornvale-religion | domains | hornvale-kernel | — |
| hornvale-settlement | domains | hornvale-kernel | — |
| hornvale-species | domains | hornvale-kernel | — |
| hornvale-terrain | domains | hornvale-kernel | — |
| hornvale-topology | domains | hornvale-kernel | — |
| hornvale-almanac | windows | hornvale-astronomy, hornvale-climate, hornvale-culture, hornvale-history, hornvale-kernel, hornvale-religion, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-topology | — |
| hornvale-book | windows | hornvale-astronomy, hornvale-kernel, hornvale-language, hornvale-worldgen | hornvale-terrain |
| hornvale-chronicle | windows | hornvale-kernel | — |
| hornvale-explain | windows | hornvale-astronomy, hornvale-kernel | — |
| hornvale-historiography | windows | hornvale-kernel | — |
| hornvale-lab | windows | hornvale-astronomy, hornvale-climate, hornvale-culture, hornvale-demography, hornvale-kernel, hornvale-language, hornvale-locale, hornvale-religion, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-vessel, hornvale-worldgen | — |
| hornvale-locale | windows | hornvale-climate, hornvale-kernel, hornvale-terrain, hornvale-worldgen | — |
| hornvale-scene | windows | hornvale-astronomy, hornvale-climate, hornvale-kernel, hornvale-settlement, hornvale-terrain, hornvale-worldgen | — |
| hornvale-vessel | windows | hornvale-astronomy, hornvale-book, hornvale-historiography, hornvale-kernel, hornvale-language, hornvale-locale, hornvale-settlement, hornvale-species, hornvale-worldgen | hornvale-terrain |
| hornvale-worldgen | windows | hornvale-almanac, hornvale-astronomy, hornvale-climate, hornvale-culture, hornvale-demography, hornvale-history, hornvale-kernel, hornvale-language, hornvale-paleoclimate, hornvale-religion, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-topology | — |
| hornvale | cli | hornvale-almanac, hornvale-astronomy, hornvale-book, hornvale-chronicle, hornvale-climate, hornvale-culture, hornvale-explain, hornvale-historiography, hornvale-kernel, hornvale-lab, hornvale-language, hornvale-locale, hornvale-paleoclimate, hornvale-religion, hornvale-scene, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-vessel, hornvale-worldgen | — |

<!-- GENERATED FILE — do not edit. Emitted and drift-checked by cli/tests/architecture.rs (the layering enforcer); accept a deliberate graph change with REBASELINE=1 (make rebaseline-goldens). -->

```text
kernel  →  domains/*  →  windows/*  →  cli
```

| crate | layer | workspace dependencies | dev/build-only extras |
|---|---|---|---|
| hornvale-kernel | kernel | — | — |
| hornvale-alchemy | domains | hornvale-kernel | — |
| hornvale-astronomy | domains | hornvale-kernel | — |
| hornvale-climate | domains | hornvale-kernel | — |
| hornvale-culture | domains | hornvale-kernel | — |
| hornvale-demography | domains | hornvale-kernel | — |
| hornvale-history | domains | hornvale-kernel | — |
| hornvale-language | domains | hornvale-kernel | — |
| hornvale-paleoclimate | domains | hornvale-kernel | — |
| hornvale-person | domains | hornvale-kernel | — |
| hornvale-religion | domains | hornvale-kernel | — |
| hornvale-settlement | domains | hornvale-kernel | — |
| hornvale-species | domains | hornvale-kernel | — |
| hornvale-terrain | domains | hornvale-kernel | — |
| hornvale-thing | domains | hornvale-kernel | — |
| hornvale-topology | domains | hornvale-kernel | — |
| hornvale-almanac | windows | hornvale-astronomy, hornvale-climate, hornvale-culture, hornvale-history, hornvale-kernel, hornvale-language, hornvale-person, hornvale-religion, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-topology | hornvale-worldgen |
| hornvale-book | windows | hornvale-astronomy, hornvale-climate, hornvale-kernel, hornvale-language, hornvale-terrain, hornvale-worldgen | — |
| hornvale-chronicle | windows | hornvale-kernel | — |
| hornvale-explain | windows | hornvale-almanac, hornvale-astronomy, hornvale-kernel, hornvale-language, hornvale-terrain | — |
| hornvale-hearsay | windows | hornvale-astronomy, hornvale-history, hornvale-kernel | hornvale-species, hornvale-terrain, hornvale-worldgen |
| hornvale-historiography | windows | hornvale-kernel | — |
| hornvale-lab | windows | hornvale-astronomy, hornvale-climate, hornvale-culture, hornvale-demography, hornvale-hearsay, hornvale-history, hornvale-kernel, hornvale-language, hornvale-locale, hornvale-lot, hornvale-religion, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-topology, hornvale-vessel, hornvale-worldgen | — |
| hornvale-locale | windows | hornvale-climate, hornvale-kernel, hornvale-terrain, hornvale-worldgen | — |
| hornvale-lot | windows | hornvale-almanac, hornvale-astronomy, hornvale-culture, hornvale-hearsay, hornvale-history, hornvale-kernel, hornvale-language, hornvale-religion, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-worldgen | — |
| hornvale-scene | windows | hornvale-astronomy, hornvale-climate, hornvale-kernel, hornvale-locale, hornvale-settlement, hornvale-terrain, hornvale-worldgen | — |
| hornvale-sentiment | windows | hornvale-demography, hornvale-kernel, hornvale-language, hornvale-species | — |
| hornvale-vessel | windows | hornvale-astronomy, hornvale-book, hornvale-climate, hornvale-historiography, hornvale-history, hornvale-kernel, hornvale-language, hornvale-locale, hornvale-person, hornvale-religion, hornvale-scene, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-thing, hornvale-worldgen | — |
| hornvale-worldgen | windows | hornvale-alchemy, hornvale-almanac, hornvale-astronomy, hornvale-climate, hornvale-culture, hornvale-demography, hornvale-explain, hornvale-history, hornvale-kernel, hornvale-language, hornvale-paleoclimate, hornvale-person, hornvale-religion, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-thing, hornvale-topology | — |
| hornvale | cli | hornvale-almanac, hornvale-astronomy, hornvale-book, hornvale-chronicle, hornvale-climate, hornvale-culture, hornvale-explain, hornvale-historiography, hornvale-kernel, hornvale-lab, hornvale-language, hornvale-locale, hornvale-lot, hornvale-paleoclimate, hornvale-religion, hornvale-scene, hornvale-sentiment, hornvale-settlement, hornvale-species, hornvale-terrain, hornvale-thing, hornvale-vessel, hornvale-worldgen | — |

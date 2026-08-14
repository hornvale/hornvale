<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# The Domesday

A generated survey of the committed census: what Hornvale's worlds actually produce, and where the instrument finds them wanting.

1000 worlds, 222 metrics across 12 domains.

## Domains

| domain | metrics | weaknesses | |
|---|---|---|---|
| Astronomy | 25 | 5 | [page](./astronomy.md) |
| Terrain | 19 | 1 | [page](./terrain.md) |
| Climate | 6 | 9 | [page](./climate.md) |
| Hydrology | 17 | 13 | [page](./hydrology.md) |
| Biology | 14 | 34 | [page](./biology.md) |
| Settlement | 20 | 10 | [page](./settlement.md) |
| Demography | 7 | 7 | [page](./demography.md) |
| Society | 5 | 3 | [page](./society.md) |
| Religion | 21 | 12 | [page](./religion.md) |
| Language | 48 | 35 | [page](./language.md) |
| Naming | 17 | 9 | [page](./naming.md) |
| History | 23 | 24 | [page](./history.md) |

## Crate coverage (D8)

A `domains/` crate no census metric measures at all is a gap in the world, not a per-metric finding, so it has no domain page of its own.

- `alchemy`: no census metric measures any quantity the `alchemy` crate produces
- `paleoclimate`: no census metric measures any quantity the `paleoclimate` crate produces
- `person`: no census metric measures any quantity the `person` crate produces

## Findings by detector

Raw firing counts, not distinct metrics: D2's hits are a subset of D4's by construction (a frozen metric's median trivially equals its min and max), and D3 and D4 also overlap. Each domain page groups its own findings by metric so no reader counts the same metric twice. A detector that found nothing still gets a row, reading `0`: silence here would mean both *this detector does not exist* and *this detector ran and every claim it checks held*, and those two must not share a channel.

| detector | findings |
|---|---|
| D1 | 29 |
| D2 | 40 |
| D3 | 13 |
| D4 | 53 |
| D5 direction | 0 |
| D5 strength | 20 |
| D5 unmeasurable | 6 |
| D6 | 1 |
| D7 | 0 |
| D8 | 3 |

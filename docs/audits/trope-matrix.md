<!-- GENERATED FILE — do not edit. Regenerate with `hornvale tropes matrix`. -->

# The trope matrix

This measures reach against *these* catalogues. It is not a verdict on the
world, and it scores **representability only** — whether an agent could plan
or recognise a situation is not measured here.

Neither is it a ranking of the catalogues against each other. Each is an
instrument carrying a declared bias (ADR 0095), so a column is a reading
taken through that bias and nothing more. The finding a single column cannot
carry is where the instruments **disagree** — which is what the demand table
below is for.

## Columns

All columns resolve against one registry of 335 tokens, built once per run,
so a difference between columns is a difference between catalogues and never
between two worlds.

| Corpus | Stageable | Inapplicable | Report |
|---|---|---|---|
| `polti-1895` | 0 of 36 | 1 | [trope-coverage-polti-1895.md](./trope-coverage-polti-1895.md) |
| `tvtropes-2012` | 0 of 409 | 62 | [trope-coverage-tvtropes-2012.md](./trope-coverage-tvtropes-2012.md) |

- `polti-1895` — Georges Polti, Les 36 situations dramatiques (1895). French
dramaturgical taxonomy of European theatre. An instrument with known bias,
not a standard: coverage measures reach against this catalogue only. Role
vocabulary for the decomposition is Greimas' actantial model (subject,
object, sender, receiver, helper, opponent).
- `tvtropes-2012` — TVTropes, wget rip May-June 2012, CC BY-SA 3.0.
Selection: 409 character tropes that the wiki both dissected on a
PlayingWith page and filed in one of seven character indexes - the wiki's
editorial judgment, not the compiler's. The 2012 rip holds 71.6% of the
tropes a 2021 third-party extraction lists, so this measures a 2012
vocabulary. Requirements were mapped onto the bundle vocabulary by a model
reading wiki prose blind, not authored by a domain expert. An instrument
with known bias, not a standard. 20 entries require capabilities that too
few judges requested for them to enter the vocabulary, under 12 distinct
names; those requirements name an undeclared bundle and are unsatisfiable by
construction, so 19 of the 20 are permanently blocked and the remaining one
is excluded before its requirements are expanded. This is deliberate and not
a defect.

## Demand

Every bundle either catalogue requires (52), with the share of that
catalogue's situations requiring it. Shares are counted over the corpora
themselves — a bundle's numerator is the situations requiring it, the
denominator is the whole catalogue — and are not read back out of the
rendered columns.

**12 of those 52 are marked †**: no catalogue here *declares* the bundle its
situations ask for. A dangling reference is not a capability — it expands to
itself, matches no registry token, and blocks its situation by construction.
Such a row exists because a catalogue asked, and carries the mark because
asking is all it can do; two of them may be near-synonyms without being two
demands.

**Gap** is the difference between the highest and lowest share, in
percentage points, and is what the table is sorted by, descending. Rows
sharing a Gap are ordered by their unrounded spread — recomputable from the
counts in each cell — and then by bundle name.

Every column above reads 0 stageable, so nothing in this table is a score.
It says what each catalogue asks the world for, and the catalogues do not
agree.

| Bundle | `polti-1895` | `tvtropes-2012` | Gap |
|---|---|---|---|
| `bundle:agent-knowledge` | 19% (7/36) | 53% (217/409) | 34 |
| `bundle:intent` | 50% (18/36) | 25% (103/409) | 25 |
| `bundle:identity-and-recognition` | 6% (2/36) | 29% (118/409) | 23 |
| `bundle:consanguineal-kin` | 33% (12/36) | 12% (50/409) | 21 |
| `bundle:norm-and-transgression` | 22% (8/36) | 38% (157/409) | 16 |
| `bundle:reputation-and-dishonour` | 14% (5/36) | 30% (122/409) | 16 |
| `bundle:erotic-desire` | 28% (10/36) | 13% (52/409) | 15 |
| `bundle:felt-affect` | 44% (16/36) | 30% (124/409) | 14 |
| `bundle:speech-act` | 14% (5/36) | 27% (110/409) | 13 |
| `bundle:self-judgement` | 17% (6/36) | 5% (20/409) | 12 |
| `bundle:witnessing` | 17% (6/36) | 10% (39/409) | 7 |
| `bundle:impaired-reason` | 3% (1/36) | 10% (40/409) | 7 |
| `bundle:traversable-geography` | 11% (4/36) | 4% (17/409) | 7 |
| `bundle:interpersonal-violence` | 25% (9/36) | 32% (129/409) | 7 |
| `bundle:relinquishment` | 11% (4/36) | 5% (21/409) | 6 |
| `bundle:conspiracy` | 8% (3/36) | 2% (10/409) | 6 |
| `bundle:environmental-adversity` | 6% (2/36) | 0% (0/409) | 6 |
| `bundle:community-subordination` | 6% (2/36) | 0% (1/409) | 6 |
| `bundle:named-pantheon` | 6% (2/36) | 1% (4/409) | 5 |
| `bundle:act-chronology` | 11% (4/36) | 16% (64/409) | 5 |
| `bundle:judgement-and-sanction` | 11% (4/36) | 7% (29/409) | 4 |
| `bundle:community-downfall` | 6% (2/36) | 2% (9/409) | 4 |
| `bundle:sworn-obligation` | 6% (2/36) | 2% (10/409) | 4 |
| `bundle:collective-conflict` | 8% (3/36) | 5% (19/409) | 3 |
| `bundle:community-as-actor` | 8% (3/36) | 5% (19/409) | 3 |
| `bundle:revelation` | 14% (5/36) | 11% (44/409) | 3 |
| `bundle:office-and-authority` | 14% (5/36) | 11% (45/409) | 3 |
| `bundle:community-chronology` | 3% (1/36) | 0% (0/409) | 3 |
| `bundle:succession` | 6% (2/36) | 3% (12/409) | 3 |
| `bundle:celestial-portent` | 3% (1/36) | 0% (2/409) | 3 |
| `bundle:personal-property` | 11% (4/36) | 9% (36/409) | 2 |
| `bundle:flight-and-pursuit` | 6% (2/36) | 4% (15/409) | 2 |
| `bundle:search-and-finding` | 6% (2/36) | 4% (17/409) | 2 |
| `bundle:sustenance-and-appetite` † | 0% (0/36) | 1% (4/409) | 1 |
| `bundle:deception` | 11% (4/36) | 10% (42/409) | 1 |
| `bundle:pair-bond` | 11% (4/36) | 12% (48/409) | 1 |
| `bundle:personal-rank` | 14% (5/36) | 13% (55/409) | 1 |
| `bundle:captivity-and-guardianship` | 6% (2/36) | 5% (21/409) | 1 |
| `bundle:coercive-threat` | 8% (3/36) | 8% (32/409) | 0 |
| `bundle:arcane-power` † | 0% (0/36) | 0% (2/409) | 0 |
| `bundle:food-and-eating` † | 0% (0/36) | 0% (2/409) | 0 |
| `bundle:nonhuman-creatures` † | 0% (0/36) | 0% (2/409) | 0 |
| `bundle:nonhuman-kinds` † | 0% (0/36) | 0% (2/409) | 0 |
| `bundle:preternatural-power` † | 0% (0/36) | 0% (2/409) | 0 |
| `bundle:artificial-agency` † | 0% (0/36) | 0% (1/409) | 0 |
| `bundle:food-and-drink` † | 0% (0/36) | 0% (1/409) | 0 |
| `bundle:game-of-strategy` † | 0% (0/36) | 0% (1/409) | 0 |
| `bundle:healing-and-recovery` † | 0% (0/36) | 0% (1/409) | 0 |
| `bundle:machine-artifact` † | 0% (0/36) | 0% (1/409) | 0 |
| `bundle:temporal-displacement` † | 0% (0/36) | 0% (1/409) | 0 |
| `bundle:divine-agency` | 3% (1/36) | 3% (11/409) | 0 |
| `bundle:individual-persons` | 100% (36/36) | 100% (409/409) | 0 |

## Agreement and fork

Each catalogue's own bundles ranked by share within that catalogue —
descending, ties by name — read down together until they part.

They agree without exception on their first rank:

1. `bundle:individual-persons` — 100% in `polti-1895`, 100% in `tvtropes-2012`

They diverge at rank 2:

- `polti-1895` asks next for `bundle:intent` (50%)
- `tvtropes-2012` asks next for `bundle:agent-knowledge` (53%)

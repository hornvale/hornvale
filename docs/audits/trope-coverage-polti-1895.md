<!-- GENERATED FILE — do not edit. Regenerate with `hornvale tropes --corpus tropes/polti.trope.json report`. -->

# Trope coverage

## Provenance

- **Corpus:** `polti-1895`
- **Source:** Georges Polti, Les 36 situations dramatiques (1895). French dramaturgical taxonomy of European theatre. An instrument with known bias, not a standard: coverage measures reach against this catalogue only. Role vocabulary for the decomposition is Greimas' actantial model (subject, object, sender, receiver, helper, opponent).
- **Frozen:** before first measurement, The Repertoire

This measures reach against *that* catalogue. It is not a verdict on the
world, and it scores **representability only** — whether an agent could plan
or recognise a situation is not measured here.

A low score is the expected reading at this stage: the report is a baseline
taken before the machinery it measures exists. What carries information is
movement between runs, not the absolute number.

**Stageable now means witnessed, not merely named (decisions 0577/0583).** A
situation scores Stageable only when every requirement token resolves, a
tableau is registered under its id, and the tableau stages successfully —
its cast places as entities and its relations commit without contradiction.
The witness's staged-relation predicates and the situation's own required
`predicate:` tokens must be the SAME SET, not merely one a subset of the
other: every predicate the tableau relates by is one the situation requires,
AND every `predicate:` token the situation requires is realized by at least
one staged relation — so neither an extraneous relation nor an uncovered
requirement can pass silently, and a witness with no relations at all can
bind only to a situation that requires none. **Known limits, named as an
open list, not a closed one:** actant ROLE assignment is never checked
(`actants` is prose-valued); a `concept:`/`phenomenon:` requirement is never
realized by a relation at all, since nothing a tableau stages can represent
one, so such a requirement is never actually witnessed even on a Stageable
situation; and the bar is name-level, not aptness-level — a relation naming
a registered predicate counts as realizing it whatever its actual arguments
are.

**A count taken before this gate existed is not comparable to one taken
after it.** Before, a count measured token membership alone; after, it
measures the strictly harder, bound claim above. Migration cost was zero at
the moment this gate was wired (spec §4.2) — no situation here had a witness
to lose — so counts taken today are unchanged from the last pre-witness run,
but that is a fact about today's corpus, not a property that would let a
future reader diff the two eras' counts meaningfully.

## Demand

Stageable 0 of 36 (1 inapplicable).

| Situation | Actants | Outcome |
|---|---|---|
| Supplication (polti-01-supplication) | opponent, receiver, subject | blocked — missing `predicate:compels`, `predicate:threatens`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:holds-office`, `predicate:office-over`, `predicate:office-rank`, `predicate:utterance-force`, `predicate:uttered-to` |
| Deliverance (polti-02-deliverance) | object, opponent, subject | blocked — missing `predicate:guarded-by`, `predicate:held-captive-by`, `predicate:released-by`, `predicate:compels`, `predicate:threatens`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:executioner-of`, `predicate:judged-by`, `predicate:sanctioned-with`, `predicate:verdict` |
| Crime Pursued by Vengeance (polti-03-crime-pursued-by-vengeance) | opponent, subject | blocked — missing `predicate:feels-toward`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:slain-by`, `predicate:norm-holds-in`, `predicate:transgresses`, `predicate:transgression-class` |
| Vengeance Taken for Kindred upon Kindred (polti-04-vengeance-for-kin-upon-kin) | object, opponent, subject | blocked — missing `predicate:feels-toward`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:slain-by`, `predicate:norm-holds-in`, `predicate:transgresses`, `predicate:transgression-class` |
| Pursuit (polti-05-pursuit) | opponent, subject | blocked — missing `predicate:flees-from`, `predicate:pursues`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:executioner-of`, `predicate:judged-by`, `predicate:sanctioned-with`, `predicate:verdict` |
| Disaster (polti-06-disaster) | opponent, sender, subject | blocked — missing `predicate:at-war-with`, `predicate:conflict-outcome`, `predicate:utterance-force`, `predicate:uttered-to` |
| Falling Prey to Cruelty or Misfortune (polti-07-falling-prey-to-cruelty-or-misfortune) | opponent, subject | blocked — missing `predicate:compels`, `predicate:threatens`, `predicate:feels-toward`, `predicate:caste-of-person`, `predicate:outranks`, `predicate:subordinate-to` |
| Revolt (polti-08-revolt) | opponent, subject | blocked — missing `predicate:at-war-with`, `predicate:conflict-outcome`, `predicate:conspires-with`, `predicate:plot-against`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:holds-office`, `predicate:office-over`, `predicate:office-rank`, `predicate:caste-of-person`, `predicate:outranks`, `predicate:subordinate-to`, `predicate:heir-of`, `predicate:office-vacant`, `predicate:succeeded-by` |
| Daring Enterprise (polti-09-daring-enterprise) | helper, object, opponent, subject | blocked — missing `predicate:conspires-with`, `predicate:plot-against`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:slain-by`, `predicate:coveted-by`, `predicate:held-by-person`, `predicate:is-holding` |
| Abduction (polti-10-abduction) | object, opponent, subject | blocked — missing `predicate:guarded-by`, `predicate:held-captive-by`, `predicate:released-by`, `predicate:feels-toward`, `predicate:flees-from`, `predicate:pursues`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:slain-by` |
| The Enigma (polti-11-the-enigma) | object, opponent, subject | blocked — missing `predicate:believes-falsely`, `predicate:ignorant-of`, `predicate:knows-that`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:revealed-to`, `predicate:revealer-of`, `predicate:found-by`, `predicate:seeks`, `predicate:whereabouts-unknown`, `predicate:utterance-force`, `predicate:uttered-to` |
| Obtaining (polti-12-obtaining) | opponent, receiver, subject | blocked — missing `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:holds-office`, `predicate:office-over`, `predicate:office-rank`, `predicate:coveted-by`, `predicate:held-by-person`, `predicate:is-holding`, `predicate:utterance-force`, `predicate:uttered-to` |
| Enmity of Kinsmen (polti-13-enmity-of-kinsmen) | opponent, subject | blocked — missing `predicate:feels-toward`, `predicate:acts-to-bring-about`, `predicate:intends` |
| Rivalry of Kinsmen (polti-14-rivalry-of-kinsmen) | object, opponent, subject | blocked — missing `predicate:desire-requited`, `predicate:desires`, `predicate:feels-toward`, `predicate:coveted-by`, `predicate:held-by-person`, `predicate:is-holding` |
| Murderous Adultery (polti-15-murderous-adultery) | helper, object, subject | blocked — missing `predicate:conspires-with`, `predicate:plot-against`, `predicate:asserted-falsely`, `predicate:deceives`, `predicate:evidence-forged-by`, `predicate:desire-requited`, `predicate:desires`, `predicate:slain-by`, `predicate:norm-holds-in`, `predicate:transgresses`, `predicate:transgression-class`, `predicate:bond-dissolved`, `predicate:bond-recognized-by`, `predicate:bonded-to` |
| Madness (polti-16-madness) | object, subject | blocked — missing `predicate:feels-toward`, `predicate:affliction-of`, `predicate:reason-impaired`, `predicate:slain-by`, `predicate:regrets`, `predicate:self-reproach-for` |
| Fatal Imprudence (polti-17-fatal-imprudence) | object, subject | blocked — missing `predicate:believes-falsely`, `predicate:ignorant-of`, `predicate:knows-that`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:coveted-by`, `predicate:held-by-person`, `predicate:is-holding`, `predicate:regrets`, `predicate:self-reproach-for` |
| Involuntary Crimes of Love (polti-18-involuntary-crimes-of-love) | object, sender, subject | blocked — missing `predicate:believes-falsely`, `predicate:ignorant-of`, `predicate:knows-that`, `predicate:desire-requited`, `predicate:desires`, `predicate:norm-holds-in`, `predicate:transgresses`, `predicate:transgression-class`, `predicate:revealed-to`, `predicate:revealer-of`, `predicate:regrets`, `predicate:self-reproach-for` |
| Slaying of a Kinsman Unrecognized (polti-19-slaying-of-a-kinsman-unrecognized) | object, sender, subject | blocked — missing `predicate:presents-as`, `predicate:recognized-as`, `predicate:unrecognized-by`, `predicate:slain-by`, `predicate:revealed-to`, `predicate:revealer-of`, `predicate:regrets`, `predicate:self-reproach-for` |
| Self-Sacrificing for an Ideal (polti-20-self-sacrifice-for-an-ideal) | object, sender, subject | blocked — missing `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:relinquished-for`, `predicate:relinquishes`, `predicate:sworn-to`, `predicate:vow-binds`, `predicate:vow-broken` |
| Self-Sacrifice for Kindred (polti-21-self-sacrifice-for-kindred) | object, receiver, subject | blocked — missing `predicate:feels-toward`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:relinquished-for`, `predicate:relinquishes` |
| All Sacrificed for a Passion (polti-22-all-sacrificed-for-a-passion) | object, opponent, subject | blocked — missing `predicate:desire-requited`, `predicate:desires`, `predicate:feels-toward`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:relinquished-for`, `predicate:relinquishes`, `predicate:dishonoured-by`, `predicate:standing-of` |
| Necessity of Sacrificing Loved Ones (polti-23-necessity-of-sacrificing-loved-ones) | object, sender, subject | blocked — missing `predicate:feels-toward`, `predicate:slain-by`, `predicate:relinquished-for`, `predicate:relinquishes`, `predicate:sworn-to`, `predicate:vow-binds`, `predicate:vow-broken` |
| Rivalry of Superior and Inferior (polti-24-rivalry-of-superior-and-inferior) | object, opponent, subject | blocked — missing `predicate:desire-requited`, `predicate:desires`, `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:holds-office`, `predicate:office-over`, `predicate:office-rank`, `predicate:caste-of-person`, `predicate:outranks`, `predicate:subordinate-to`, `predicate:dishonoured-by`, `predicate:standing-of` |
| Adultery (polti-25-adultery) | helper, object, subject | blocked — missing `predicate:believes-falsely`, `predicate:ignorant-of`, `predicate:knows-that`, `predicate:asserted-falsely`, `predicate:deceives`, `predicate:evidence-forged-by`, `predicate:desire-requited`, `predicate:desires`, `predicate:norm-holds-in`, `predicate:transgresses`, `predicate:transgression-class`, `predicate:bond-dissolved`, `predicate:bond-recognized-by`, `predicate:bonded-to` |
| Crimes of Love (polti-26-crimes-of-love) | object, subject | blocked — missing `predicate:desire-requited`, `predicate:desires`, `predicate:norm-holds-in`, `predicate:transgresses`, `predicate:transgression-class`, `predicate:dishonoured-by`, `predicate:standing-of` |
| Discovery of the Dishonour of a Loved One (polti-27-discovery-of-the-dishonour-of-a-loved-one) | object, subject | blocked — missing `predicate:believes-falsely`, `predicate:ignorant-of`, `predicate:knows-that`, `predicate:feels-toward`, `predicate:dishonoured-by`, `predicate:standing-of`, `predicate:revealed-to`, `predicate:revealer-of`, `predicate:regrets`, `predicate:self-reproach-for` |
| Obstacles to Love (polti-28-obstacles-to-love) | object, opponent, subject | blocked — missing `predicate:desire-requited`, `predicate:desires`, `predicate:bond-dissolved`, `predicate:bond-recognized-by`, `predicate:bonded-to`, `predicate:caste-of-person`, `predicate:outranks`, `predicate:subordinate-to` |
| An Enemy Loved (polti-29-an-enemy-loved) | object, opponent, subject | blocked — missing `predicate:at-war-with`, `predicate:conflict-outcome`, `predicate:desire-requited`, `predicate:desires`, `predicate:feels-toward` |
| Ambition (polti-30-ambition) | object, opponent, subject | blocked — missing `predicate:acts-to-bring-about`, `predicate:intends`, `predicate:holds-office`, `predicate:office-over`, `predicate:office-rank`, `predicate:caste-of-person`, `predicate:outranks`, `predicate:subordinate-to`, `predicate:dishonoured-by`, `predicate:standing-of`, `predicate:heir-of`, `predicate:office-vacant`, `predicate:succeeded-by` |
| Conflict with a God (polti-31-conflict-with-a-god) | opponent, subject | inapplicable — design:deities-are-mythologizations-not-agents — Hornvale's religion domain derives every deity from a salient phenomenon and commits the provenance (predicate:derived-from-phenomenon); a god is a community's account of the sky or the weather, and the causal chain terminates in astronomy and climate. A mortal contending with an immortal antagonist presupposes a supernatural agent with will, which the world deliberately does not contain (Constitution §1, sim first). Different, not deficient. |
| Mistaken Jealousy (polti-32-mistaken-jealousy) | object, opponent, sender, subject | blocked — missing `predicate:believes-falsely`, `predicate:ignorant-of`, `predicate:knows-that`, `predicate:asserted-falsely`, `predicate:deceives`, `predicate:evidence-forged-by`, `predicate:desire-requited`, `predicate:desires`, `predicate:feels-toward`, `predicate:bond-dissolved`, `predicate:bond-recognized-by`, `predicate:bonded-to` |
| Erroneous Judgement (polti-33-erroneous-judgement) | object, opponent, sender, subject | blocked — missing `predicate:believes-falsely`, `predicate:ignorant-of`, `predicate:knows-that`, `predicate:asserted-falsely`, `predicate:deceives`, `predicate:evidence-forged-by`, `predicate:executioner-of`, `predicate:judged-by`, `predicate:sanctioned-with`, `predicate:verdict`, `predicate:norm-holds-in`, `predicate:transgresses`, `predicate:transgression-class`, `predicate:revealed-to`, `predicate:revealer-of` |
| Remorse (polti-34-remorse) | object, opponent, subject | blocked — missing `predicate:feels-toward`, `predicate:norm-holds-in`, `predicate:transgresses`, `predicate:transgression-class`, `predicate:regrets`, `predicate:self-reproach-for`, `predicate:utterance-force`, `predicate:uttered-to` |
| Recovery of a Lost One (polti-35-recovery-of-a-lost-one) | object, subject | blocked — missing `predicate:feels-toward`, `predicate:presents-as`, `predicate:recognized-as`, `predicate:unrecognized-by`, `predicate:found-by`, `predicate:seeks`, `predicate:whereabouts-unknown` |
| Loss of Loved Ones (polti-36-loss-of-loved-ones) | object, opponent, subject | blocked — missing `predicate:feels-toward`, `predicate:slain-by`, `predicate:executioner-of`, `predicate:judged-by`, `predicate:sanctioned-with`, `predicate:verdict` |

## Leverage

Missing bundles ranked by fan-in over the 35 situations **blocked by a
missing token** — the denominator this section is actually about. The 1
inapplicable situation is excluded from this ranking, but not from the
report: the Supply section below still counts its requirements as demand,
which keeps those tokens off the orphan list. 0 more situations are blocked
by a missing or unbound witness rather than a missing token, excluded from
this ranking for the same reason (no bundle here can resolve one).
Reconciled: the **corpus** column counts all 36 situations, which is 0
stageable + 1 inapplicable + 35 blocked by a missing token + 0 blocked by a
missing or unbound witness.

Fan-in is **not** an unlock count: the closest blocked situation is still
missing 2 bundles, so no single row makes anything stageable on its own.

**1 missing bundle is not ranked below.** `bundle:divine-agency` — required
only by situations that resolve inapplicable, so they contribute no fan-in
and no row. The corpus holds 28 missing bundles against the 27 ranked here;
that is the difference.

| Bundle | Fan-in (blocked) | Corpus | Situations |
|---|---|---|---|
| `bundle:intent` | 17 | 18 | polti-01-supplication, polti-02-deliverance, polti-03-crime-pursued-by-vengeance, polti-04-vengeance-for-kin-upon-kin, polti-05-pursuit, polti-08-revolt, polti-09-daring-enterprise, polti-10-abduction, polti-11-the-enigma, polti-12-obtaining, polti-13-enmity-of-kinsmen, polti-17-fatal-imprudence, polti-20-self-sacrifice-for-an-ideal, polti-21-self-sacrifice-for-kindred, polti-22-all-sacrificed-for-a-passion, polti-24-rivalry-of-superior-and-inferior, polti-30-ambition |
| `bundle:felt-affect` | 16 | 16 | polti-03-crime-pursued-by-vengeance, polti-04-vengeance-for-kin-upon-kin, polti-07-falling-prey-to-cruelty-or-misfortune, polti-10-abduction, polti-13-enmity-of-kinsmen, polti-14-rivalry-of-kinsmen, polti-16-madness, polti-21-self-sacrifice-for-kindred, polti-22-all-sacrificed-for-a-passion, polti-23-necessity-of-sacrificing-loved-ones, polti-27-discovery-of-the-dishonour-of-a-loved-one, polti-29-an-enemy-loved, polti-32-mistaken-jealousy, polti-34-remorse, polti-35-recovery-of-a-lost-one, polti-36-loss-of-loved-ones |
| `bundle:erotic-desire` | 10 | 10 | polti-14-rivalry-of-kinsmen, polti-15-murderous-adultery, polti-18-involuntary-crimes-of-love, polti-22-all-sacrificed-for-a-passion, polti-24-rivalry-of-superior-and-inferior, polti-25-adultery, polti-26-crimes-of-love, polti-28-obstacles-to-love, polti-29-an-enemy-loved, polti-32-mistaken-jealousy |
| `bundle:interpersonal-violence` | 9 | 9 | polti-03-crime-pursued-by-vengeance, polti-04-vengeance-for-kin-upon-kin, polti-09-daring-enterprise, polti-10-abduction, polti-15-murderous-adultery, polti-16-madness, polti-19-slaying-of-a-kinsman-unrecognized, polti-23-necessity-of-sacrificing-loved-ones, polti-36-loss-of-loved-ones |
| `bundle:norm-and-transgression` | 8 | 8 | polti-03-crime-pursued-by-vengeance, polti-04-vengeance-for-kin-upon-kin, polti-15-murderous-adultery, polti-18-involuntary-crimes-of-love, polti-25-adultery, polti-26-crimes-of-love, polti-33-erroneous-judgement, polti-34-remorse |
| `bundle:agent-knowledge` | 7 | 7 | polti-11-the-enigma, polti-17-fatal-imprudence, polti-18-involuntary-crimes-of-love, polti-25-adultery, polti-27-discovery-of-the-dishonour-of-a-loved-one, polti-32-mistaken-jealousy, polti-33-erroneous-judgement |
| `bundle:self-judgement` | 6 | 6 | polti-16-madness, polti-17-fatal-imprudence, polti-18-involuntary-crimes-of-love, polti-19-slaying-of-a-kinsman-unrecognized, polti-27-discovery-of-the-dishonour-of-a-loved-one, polti-34-remorse |
| `bundle:office-and-authority` | 5 | 5 | polti-01-supplication, polti-08-revolt, polti-12-obtaining, polti-24-rivalry-of-superior-and-inferior, polti-30-ambition |
| `bundle:personal-rank` | 5 | 5 | polti-07-falling-prey-to-cruelty-or-misfortune, polti-08-revolt, polti-24-rivalry-of-superior-and-inferior, polti-28-obstacles-to-love, polti-30-ambition |
| `bundle:reputation-and-dishonour` | 5 | 5 | polti-22-all-sacrificed-for-a-passion, polti-24-rivalry-of-superior-and-inferior, polti-26-crimes-of-love, polti-27-discovery-of-the-dishonour-of-a-loved-one, polti-30-ambition |
| `bundle:revelation` | 5 | 5 | polti-11-the-enigma, polti-18-involuntary-crimes-of-love, polti-19-slaying-of-a-kinsman-unrecognized, polti-27-discovery-of-the-dishonour-of-a-loved-one, polti-33-erroneous-judgement |
| `bundle:speech-act` | 5 | 5 | polti-01-supplication, polti-06-disaster, polti-11-the-enigma, polti-12-obtaining, polti-34-remorse |
| `bundle:deception` | 4 | 4 | polti-15-murderous-adultery, polti-25-adultery, polti-32-mistaken-jealousy, polti-33-erroneous-judgement |
| `bundle:judgement-and-sanction` | 4 | 4 | polti-02-deliverance, polti-05-pursuit, polti-33-erroneous-judgement, polti-36-loss-of-loved-ones |
| `bundle:pair-bond` | 4 | 4 | polti-15-murderous-adultery, polti-25-adultery, polti-28-obstacles-to-love, polti-32-mistaken-jealousy |
| `bundle:personal-property` | 4 | 4 | polti-09-daring-enterprise, polti-12-obtaining, polti-14-rivalry-of-kinsmen, polti-17-fatal-imprudence |
| `bundle:relinquishment` | 4 | 4 | polti-20-self-sacrifice-for-an-ideal, polti-21-self-sacrifice-for-kindred, polti-22-all-sacrificed-for-a-passion, polti-23-necessity-of-sacrificing-loved-ones |
| `bundle:coercive-threat` | 3 | 3 | polti-01-supplication, polti-02-deliverance, polti-07-falling-prey-to-cruelty-or-misfortune |
| `bundle:collective-conflict` | 3 | 3 | polti-06-disaster, polti-08-revolt, polti-29-an-enemy-loved |
| `bundle:conspiracy` | 3 | 3 | polti-08-revolt, polti-09-daring-enterprise, polti-15-murderous-adultery |
| `bundle:captivity-and-guardianship` | 2 | 2 | polti-02-deliverance, polti-10-abduction |
| `bundle:flight-and-pursuit` | 2 | 2 | polti-05-pursuit, polti-10-abduction |
| `bundle:identity-and-recognition` | 2 | 2 | polti-19-slaying-of-a-kinsman-unrecognized, polti-35-recovery-of-a-lost-one |
| `bundle:search-and-finding` | 2 | 2 | polti-11-the-enigma, polti-35-recovery-of-a-lost-one |
| `bundle:succession` | 2 | 2 | polti-08-revolt, polti-30-ambition |
| `bundle:sworn-obligation` | 2 | 2 | polti-20-self-sacrifice-for-an-ideal, polti-23-necessity-of-sacrificing-loved-ones |
| `bundle:impaired-reason` | 1 | 1 | polti-16-madness |

## Supply

411 served tokens no situation in this corpus requires.

**Demand-side only.** Spec §4 L2.4 asks for tokens no situation requires
*and no readout consumes*; the second half is not implemented. So this list
includes tokens that readouts do consume — `predicate:is-a` carries the
Book, and the `moon-*` family carries the almanac. Read it as *unrequired by
this catalogue*, not *unused*. Spec D5's Goodhart guard — a rising demand
score beside a rising count of genuinely unconsumed tokens — needs the
missing half before this list can serve it.

- `concept:abyssal` (climate)
- `concept:abyssal-elf-kind` (species)
- `concept:abyssal-plain` (climate)
- `concept:affords-passage` (language)
- `concept:alcove` (thing)
- `concept:alpine` (climate)
- `concept:altar` (thing)
- `concept:anvil` (thing)
- `concept:bait-ball` (climate)
- `concept:barley` (climate)
- `concept:bathypelagic` (climate)
- `concept:bed` (thing)
- `concept:bench` (thing)
- `concept:black-dragon-kind` (species)
- `concept:blood` (language)
- `concept:blue` (language)
- `concept:blue-giant` (astronomy)
- `concept:bone` (language)
- `concept:boreal-stand` (climate)
- `concept:bracken` (thing)
- `concept:brazier` (thing)
- `concept:brown` (language)
- `concept:bugbear-kind` (species)
- `concept:burn` (climate)
- `concept:carrion-crawler-kind` (species)
- `concept:cave-mouth` (thing)
- `concept:chart` (language)
- `concept:closed-canopy` (climate)
- `concept:coast` (terrain)
- `concept:cold` (climate)
- `concept:cold-upwelling` (climate)
- `concept:content` (language)
- `concept:coral-head` (climate)
- `concept:coral-reef` (climate)
- `concept:crevasse-field` (climate)
- `concept:damp-hollow` (climate)
- `concept:dark` (language)
- `concept:day` (language)
- `concept:desert` (climate)
- `concept:desert-dwarf-kind` (species)
- `concept:desert-elf-kind` (species)
- `concept:dire-wolf-kind` (species)
- `concept:door` (thing)
- `concept:drink` (language)
- `concept:drow-kind` (species)
- `concept:duergar-kind` (species)
- `concept:eager` (language)
- `concept:earth` (language)
- `concept:east` (language)
- `concept:eat` (language)
- `concept:eclipse` (astronomy)
- `concept:encloses` (language)
- `concept:epipelagic` (climate)
- `concept:erg` (climate)
- `concept:eye` (language)
- `concept:felsenmeer` (climate)
- `concept:fire` (language)
- `concept:fire-scrub` (climate)
- `concept:fish-shoal` (climate)
- `concept:foot` (language)
- `concept:ford` (terrain)
- `concept:forest-gap` (climate)
- `concept:frost-heave` (climate)
- `concept:frustrated` (language)
- `concept:gallery-forest` (climate)
- `concept:giant-constrictor-snake-kind` (species)
- `concept:giant-crocodile-kind` (species)
- `concept:giant-elk-kind` (species)
- `concept:giant-goat-kind` (species)
- `concept:giant-hyena-kind` (species)
- `concept:giant-octopus-kind` (species)
- `concept:giant-scorpion-kind` (species)
- `concept:giant-squid-kind` (species)
- `concept:gloom` (language)
- `concept:gnoll-kind` (species)
- `concept:goblin-kind` (species)
- `concept:grass-sward` (climate)
- `concept:great` (language)
- `concept:green` (language)
- `concept:ground` (thing)
- `concept:gully-dwarf-kind` (species)
- `concept:hadal-trench` (climate)
- `concept:hamada` (climate)
- `concept:hand` (language)
- `concept:hearth` (settlement)
- `concept:heat` (climate)
- `concept:help` (language)
- `concept:helpless` (language)
- `concept:high` (language)
- `concept:high-elf-kind` (species)
- `concept:high-seat` (thing)
- `concept:hill` (terrain)
- `concept:hill-dwarf-kind` (species)
- `concept:hobgoblin-kind` (species)
- `concept:holdfast-tangle` (climate)
- `concept:holds-liquid` (language)
- `concept:home` (settlement)
- `concept:human-kind` (species)
- `concept:hydrothermal-vent` (climate)
- `concept:ice` (climate)
- `concept:ice-lead` (climate)
- `concept:identify` (language)
- `concept:island` (terrain)
- `concept:kelp-canopy` (climate)
- `concept:kelp-forest` (climate)
- `concept:kelp-tender-kind` (species)
- `concept:key` (thing)
- `concept:kill` (language)
- `concept:killer-whale-kind` (species)
- `concept:know` (language)
- `concept:kobold-kind` (species)
- `concept:kuo-toa-kind` (species)
- `concept:lake` (terrain)
- `concept:ledge` (thing)
- `concept:lens` (language)
- `concept:liana-forest` (climate)
- `concept:light` (language)
- `concept:lightless-water` (climate)
- `concept:little` (language)
- `concept:lockable` (language)
- `concept:log` (thing)
- `concept:look` (language)
- `concept:loom` (thing)
- `concept:lost` (language)
- `concept:low` (language)
- `concept:many` (language)
- `concept:marine-snow` (climate)
- `concept:marsh` (terrain)
- `concept:melt-pond` (climate)
- `concept:merfolk-kind` (species)
- `concept:mesopelagic` (climate)
- `concept:millet` (climate)
- `concept:moon` (astronomy)
- `concept:mossy-deadfall` (climate)
- `concept:mountain` (terrain)
- `concept:mountain-dwarf-kind` (species)
- `concept:mouth` (language)
- `concept:muskeg` (climate)
- `concept:name` (language)
- `concept:new` (language)
- `concept:night` (astronomy)
- `concept:nodule-field` (climate)
- `concept:north` (language)
- `concept:north-east` (language)
- `concept:north-west` (language)
- `concept:old` (language)
- `concept:old-growth` (climate)
- `concept:one` (language)
- `concept:open-blue` (climate)
- `concept:openable` (language)
- `concept:orange-dwarf` (astronomy)
- `concept:orange-giant` (astronomy)
- `concept:otyugh-kind` (species)
- `concept:over` (language)
- `concept:owlbear-kind` (species)
- `concept:plankton-bloom` (climate)
- `concept:playa` (climate)
- `concept:pool` (thing)
- `concept:portable` (language)
- `concept:pressure-ridge` (climate)
- `concept:provoke` (language)
- `concept:radiates-heat` (language)
- `concept:rafted-floe` (climate)
- `concept:rain` (climate)
- `concept:read` (language)
- `concept:recount` (language)
- `concept:red` (language)
- `concept:red-dragon-kind` (species)
- `concept:red-dwarf` (astronomy)
- `concept:red-giant` (astronomy)
- `concept:reef-mason-kind` (species)
- `concept:reef-rubble` (climate)
- `concept:reef-shark-kind` (species)
- `concept:reg` (climate)
- `concept:rest` (language)
- `concept:rhinoceros-kind` (species)
- `concept:rice` (climate)
- `concept:river` (terrain)
- `concept:rushes` (thing)
- `concept:rust-monster-kind` (species)
- `concept:sargassum-drift` (climate)
- `concept:savanna` (climate)
- `concept:scattering-layer` (climate)
- `concept:sclerophyll-scrub` (climate)
- `concept:scoured-ice` (climate)
- `concept:screen` (thing)
- `concept:sea` (terrain)
- `concept:sea-elf-kind` (species)
- `concept:sea-ice` (climate)
- `concept:searching` (language)
- `concept:sense` (language)
- `concept:shadow` (language)
- `concept:shrieker-kind` (species)
- `concept:shrubland` (climate)
- `concept:sleep` (language)
- `concept:smoker-field` (climate)
- `concept:snow` (climate)
- `concept:snow-elf-kind` (species)
- `concept:snowfield` (climate)
- `concept:soothe` (language)
- `concept:south` (language)
- `concept:south-east` (language)
- `concept:south-west` (language)
- `concept:spring` (terrain)
- `concept:spur-and-groove` (climate)
- `concept:staghorn-stand` (climate)
- `concept:star` (astronomy)
- `concept:starlit` (language)
- `concept:stone` (terrain)
- `concept:strongbox` (thing)
- `concept:sun` (astronomy)
- `concept:sun-like-star` (astronomy)
- `concept:supports-rest` (language)
- `concept:survey` (language)
- `concept:svirfneblin-kind` (species)
- `concept:taiga` (climate)
- `concept:temperate-forest` (climate)
- `concept:temperate-grassland` (climate)
- `concept:temperate-rainforest` (climate)
- `concept:the-consumption` (species)
- `concept:the-flux` (species)
- `concept:the-marsh-fever` (species)
- `concept:the-pest` (species)
- `concept:the-pox` (species)
- `concept:think` (language)
- `concept:thorn-scrub` (climate)
- `concept:threshold` (thing)
- `concept:tide` (astronomy)
- `concept:treant-kind` (species)
- `concept:tree` (language)
- `concept:trench-floor` (climate)
- `concept:trench-wall` (climate)
- `concept:triton-kind` (species)
- `concept:tropical-rainforest` (climate)
- `concept:tropical-seasonal-forest` (climate)
- `concept:tuber` (climate)
- `concept:tubeworm-thicket` (climate)
- `concept:tundra` (climate)
- `concept:twig-blight-kind` (species)
- `concept:twilight-water` (climate)
- `concept:two` (language)
- `concept:under` (language)
- `concept:upwelling` (climate)
- `concept:urchin-barren` (climate)
- `concept:valley` (terrain)
- `concept:vent-commensal-kind` (species)
- `concept:vent-plume` (climate)
- `concept:vessel` (thing)
- `concept:vine` (climate)
- `concept:wait` (language)
- `concept:water` (language)
- `concept:west` (language)
- `concept:wheat` (climate)
- `concept:white-dragon-kind` (species)
- `concept:white-dwarf` (astronomy)
- `concept:wind` (language)
- `concept:wind-scour` (climate)
- `concept:wood-elf-kind` (species)
- `concept:wooded-grassland` (climate)
- `concept:woolly-mammoth-kind` (species)
- `concept:write` (language)
- `concept:xorn-kind` (species)
- `concept:yellow` (language)
- `concept:yellow-dwarf` (astronomy)
- `concept:yellow-white-dwarf` (astronomy)
- `phenomenon:ambient`
- `phenomenon:celestial-body`
- `phenomenon:heliacal-setting`
- `phenomenon:rain`
- `phenomenon:seasonal-cycle`
- `phenomenon:snow`
- `phenomenon:tide`
- `predicate:anchor-mass-earth`
- `predicate:anchor-orbit-au`
- `predicate:association`
- `predicate:association-ended`
- `predicate:association-form`
- `predicate:breakup-age`
- `predicate:brightening-per-gyr`
- `predicate:care`
- `predicate:care-ended`
- `predicate:custody`
- `predicate:custody-ended`
- `predicate:day-length-std`
- `predicate:deity-epithet`
- `predicate:deity-epithet-ipa`
- `predicate:deity-name-ipa`
- `predicate:dependency`
- `predicate:dependency-ended`
- `predicate:descent`
- `predicate:descent-ended`
- `predicate:die`
- `predicate:dissolve`
- `predicate:eccentricity-mean`
- `predicate:figure-count`
- `predicate:figure-members`
- `predicate:figure-on-ecliptic`
- `predicate:figure-region`
- `predicate:fossil-shoreline`
- `predicate:founding-solstice-azimuth-degrees`
- `predicate:frost-retreat`
- `predicate:gender-identity`
- `predicate:gender-identity-ended`
- `predicate:gender-recognition`
- `predicate:gender-recognition-ended`
- `predicate:genesis-note`
- `predicate:greenhouse-forcing-k`
- `predicate:hab-zone-inner-au`
- `predicate:hab-zone-outer-au`
- `predicate:highest-elevation-m`
- `predicate:insolation-rel`
- `predicate:instance-of`
- `predicate:is-a`
- `predicate:is-neighbor`
- `predicate:is-occupation`
- `predicate:membership`
- `predicate:membership-ended`
- `predicate:moon-age-gyr`
- `predicate:moon-angular-size-rel`
- `predicate:moon-count`
- `predicate:moon-density`
- `predicate:moon-distance-mm`
- `predicate:moon-formation`
- `predicate:moon-inclination-degrees`
- `predicate:moon-mass-lunar`
- `predicate:moon-node-longitude-degrees`
- `predicate:moon-node-period-days`
- `predicate:moon-period-ratio`
- `predicate:moon-period-std`
- `predicate:moon-tide-rel`
- `predicate:name-gloss`
- `predicate:neighbor-brightness-rel`
- `predicate:neighbor-class`
- `predicate:neighbor-declination-deg`
- `predicate:neighbor-distance-ly`
- `predicate:neighbor-ra-deg`
- `predicate:obliquity-amplitude`
- `predicate:obliquity-degrees`
- `predicate:occ-delve-depth`
- `predicate:occ-founded-from`
- `predicate:occ-function`
- `predicate:occ-notability`
- `predicate:occ-peak`
- `predicate:occ-person-years`
- `predicate:occ-site`
- `predicate:occ-tech`
- `predicate:ocean-fraction`
- `predicate:origin`
- `predicate:origin-ended`
- `predicate:outbreak-deaths`
- `predicate:person-founded`
- `predicate:person-social-provenance`
- `predicate:plate-count`
- `predicate:pole-star-north`
- `predicate:pole-star-south`
- `predicate:recognition`
- `predicate:recognition-ended`
- `predicate:recognition-interpretation`
- `predicate:reproductive-role`
- `predicate:reproductive-role-ended`
- `predicate:residence`
- `predicate:residence-ended`
- `predicate:retrograde-spin`
- `predicate:rifted-from`
- `predicate:scenario-pin`
- `predicate:sea-level-m`
- `predicate:sentiment`
- `predicate:separate`
- `predicate:settlement-pin`
- `predicate:sex-trait`
- `predicate:sex-trait-ended`
- `predicate:sky-provider`
- `predicate:social-role`
- `predicate:social-role-ended`
- `predicate:species-activity-cycle`
- `predicate:species-deliberation-latency`
- `predicate:species-exotic-manner`
- `predicate:species-in-group-radius`
- `predicate:species-labiality`
- `predicate:species-mass-kg`
- `predicate:species-name`
- `predicate:species-night-vision`
- `predicate:species-potency`
- `predicate:species-sibilance`
- `predicate:species-sky-attention`
- `predicate:species-sociality-mode`
- `predicate:species-threat-response`
- `predicate:species-time-horizon`
- `predicate:species-tonality`
- `predicate:species-voice-loudness`
- `predicate:species-voicing`
- `predicate:species-vowel-space`
- `predicate:spreading-rate`
- `predicate:star-age-gyr`
- `predicate:star-class`
- `predicate:star-luminosity-solar`
- `predicate:star-mass-solar`
- `predicate:subsistence`
- `predicate:tenet`
- `predicate:terrain-note`
- `predicate:terrain-pin`
- `predicate:tidally-locked`
- `predicate:transfer`
- `predicate:transfer-ended`
- `predicate:transitioned`
- `predicate:transitioned-ended`
- `predicate:wanderer-class`
- `predicate:wanderer-count`
- `predicate:wanderer-orbit-au`
- `predicate:wanderer-period-std`
- `predicate:year-length-std`

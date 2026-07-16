# The Menagerie

The Niche closed on a confession: the descriptor of a *people* had quietly
become the descriptor of every *entity*. A single struct carried a creature's
mass and niche and magical potency alongside its psychology, its phonology, and
the words for its chieftain — and when the time came to author a dragon or a
fungus, the struct demanded a proto-language from things that would never speak.
This is the inheritance-versus-composition problem that gives entity-component
systems their reason to exist, and this campaign is the first of the program
that answers it: it cuts the seam, and it fills the world behind it with
creatures that are not peoples.

## An entity is the set of components it carries

`SpeciesDef` divides in two. A universal `BiosphereTraits` — mass, metabolic
class, resource niche, condition niche, potency — the component every living
thing has and the only one the packer and the habitat model ever read. And an
`Option<PeopledTraits>` — psychology, perception, articulation, the settlement
noun, the social lexicon — present only for a people that settles and speaks.
Fauna carry `peopled: None`. Nothing computes differently; the traits merely
move to where they belong. Every existing world stayed byte-identical through
the split, because a seam that changes *where* a value lives must never change
*what* it is.

The line the split falls along was already latent in the code: settlement
genesis, phonology, and perception are peopled concerns and now guard on
`peopled.is_some()`; the habitat layer reads only `biosphere` and iterates every
kind. Settlement is peopled-gated; placement is universal. The composition model
gets the border-zone entities a fat struct could never express — a culture with
no body, a deity with sovereignty near one, an awakened beast — for free, as a
data change rather than a new god-struct.

## The menagerie

Behind the seam, the roster grows from four near-identical goblinoids to
sixteen kinds. Twelve canonical creatures — a treant and a twig blight drawing
on light, giant elk and woolly mammoth and giant goat grazing, an otyugh in the
rot, a xorn and a rust monster in the stone, three chromatic dragons and an
owlbear at the top of the food web — each authored biosphere-only, each grounded
in the same discipline the goblinoids were: canonical mass, a resource niche
read from its ecology, a climate tolerance placed against the world's measured
fields, and potency for the mighty. For the first time the world holds entities
that are not peoples, and the machine that renders a settlement never has to ask
a dragon for its castes.

Minting a fauna kind renumbers the peoples' entity ids — genesis assigns ids in
roster order, and a dragon sorts before a bugbear — but every renumber is
deterministic and self-consistent, worlds re-derive from their seed, and the
stable identity that will make even that renumbering irrelevant is the next
campaign's work.

## The cutover, and the shape of what emerged

The Niche had left its differentiated carrying capacity as a shadow: computed,
observed, but not yet the path genesis walked. This campaign completes the
cutover. Settlement genesis now packs the competitive niche-K coexistence stack,
so a settlement is peopled by whichever people locally prevails. On the reference
world the count falls from two hundred seventy-six settlements to sixty-six,
peopled by goblins and hobgoblins with kobolds and bugbears present in the
composition of every one but the majority of none — the two-way biogeography the
habitat model had promised, now the world's committed reality rather than a
reading taken beside it.

The menagerie was meant to carry the differentiation further: distinct resource
niches were supposed to partition space into strongholds, a dragon owning the
cold, a xorn the deep. They do not — and the reason is worth stating plainly,
because it marks the exact edge of what this habitat model can do. Carrying
capacity is `supply × fitness`, but the supply is a single field: one net-
primary-productivity proxy, scaled per species by how much of it that species
takes. A mineral eater and a sun eater draw on the same field at different
magnitudes; they do not draw on different *places*. So a resource niche makes a
species denser or sparser everywhere, never dominant somewhere. Only the climate
term varies across space, and climate alone — The Niche already found this —
differentiates the world about two ways, no matter how many creatures compete in
it. The stronghold a dragon should hold is invisible for a second reason too:
the stack reports density, headcount per cell, and a small-bodied creature packs
more heads into a cell than a mighty one, so the arg-max of density is always
the smallest viable animal, never the apex that commands the most resource.

The strongholds wait, then, on a world whose resources have geography — minerals
in the mountains, prey where the prey is — and on a way to read ecological
dominance as resource captured rather than bodies counted. That is a modeling
campaign of its own, and it is named and set aside rather than half-built here.
What this campaign delivers is the structure that makes it approachable: the
god-struct is gone, an entity is its components, the menagerie exists and is
placed by the climate it can bear, and the seam every later campaign of the
entity-component program will grow behind is cut clean.

# The Gyre

The winds had a shape the water did not. A spinning world's atmosphere
organizes into bands — easterly trades at the equator, westerlies in the
mid-latitudes — and the climate has known that since tier zero. But the
oceans, which the winds drive, sat still: the model card's headline
omission. The Gyre gives the sea its motion — a surface-current field
derived from the winds it already computes — and hands it to the client to
watch flow. It is the second campaign of the Weather Program, and the first
to make the world's fluids *move* on the globe.

## A current from the wind

The model is a single sentence made precise. Surface water does not run with
the wind but at an angle to it, turned by the planet's rotation — to the
right in the northern hemisphere, to the left in the southern (the Ekman
deflection). Rotate the alternating band winds by that hemisphere-signed
angle and the zonal belts resolve into the rotational structure of the
**subtropical gyres**: clockwise in the north, counter-clockwise in the
south, the great wheels the trades and westerlies turn between them. Where a
current meets a coast it cannot flow into the land, so its vector is
projected onto the shore — the flow turns to run *along* the coast, which is
how boundary currents arise. On land it is zero; on a world with no wind
bands — a tidally locked one — there is no field at all.

It is a kinematic sketch, not a circulation solver: a fixed deflection and a
one-pass coastal projection, the same altitude as the banded winds
themselves, computed once over the geosphere like the drainage and the
rain-shadow before it. The full Sverdrup gyre — closed streamlines, western
intensification — and the current's real payoff, its warming of poleward
coasts by carrying heat, are deferred by design. Those are *feedback*: the
current would advect the sea's temperature, and a temperature that feeds back
must be relaxed to a fixed point, which moves the mean the census reads and
so becomes an epoch. The Gyre is deliberately upstream of that line. It
touches no temperature; it is a pure derived layer; every world and every
census is byte-identical to the one before, and the sea now has arrows.

## The level the field climbs

That line — between a field drawn and a field that feeds back — was the
campaign's real design work, and an ideonomy pass drew it. The obvious
question was binary: ship the current as a picture, or ship the coastal
warming it implies. The pass found a hidden third rung between them — a
current that advects a *diagnostic* temperature the biome classifier ignores
— and unmasked it as a trap, the same one The Faces and The Reckoning had
already named: a rendered quantity the model itself does not believe. So the
honest cleave is not the middle but the ends: either the flow is an honest
arrow that claims nothing about heat, or the heat is grounded and the biomes
change. And what triggers the machinery the grounded version needs — the
operator spine, the relaxation solver — is not the *number* of fields but
their *cyclicity*: a loop, not a layer. The Gyre adds layers, not loops, so
it earns none of that apparatus, and the refactor the diurnal campaign had
pencilled in for "the winds campaign" moves on to the feedback campaign,
where the loop finally demands it.

## Watching it turn

The client is where the field stops being data. The orrery reads the two
tangent components off each ocean tile and seeds particles over the water,
sweeping them along the current, fading them as they age and re-seeding them
where they run aground or slow to nothing — the living-globe pattern, in its
first real motion. Its correctness is a producer contract: the
client's tangent frame is the exact inverse of the one the sim emitted in, so
the arrows point where the sim says, and the gyres wheel the way the physics
turns them. The rest is honest eyecandy — client-side particle motion that
never re-enters the world's identity, the felt flow over the deterministic
field beneath. The first pass drew it as a faint scatter of specks a viewer
could barely find; the campaign's own visual check, which the last campaign
had taught to distrust anything unseen, made it a bright legible flood of
streaks. The sea moves now, and it moves the right way.

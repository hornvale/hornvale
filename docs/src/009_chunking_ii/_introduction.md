## Chunking, Pt. II: Chunk Loading and Unloading

[Previously](../008_chunking_i/), we arrived at a method of generating regions as a connected graph that can grow infinitely in any direction, creating the potential for an essentially boundless explorable world.

In this article, I want to achieve a good understanding of how we can implement a system for smoothly and efficiently loading these regions into the application's working memory when needed and unloading them when no longer needed, to realize the goal of an infinite world on very finite hardware. This leads us to the concept of "juice," which is an attempt to model the engagement and interest of the player in a given region, and use that to determine how computing resources are allocated.

The dream here is that the player, traveling through the world, will frequently encounter interesting developing situations, and feel compelled to interact in some way. For instance, a human village might be getting attacked by a tribe of bugbears, using goblins as cannon fodder. This sort of situation should arise from the evolution of the situation within each region and between the regions and their neighbors. The connections between regions, the edges between the vertices of our graph, are not just passages for the player to travel but potential avenues for trade, communication, or invasion.

So we will know if we have this algorithm tuned smoothly if a player wanders into a chunk and feels as though they have arrived "just in the nick of time."  We can fudge this a bit by decreasing the offensive power of attacks when the player isn't present (although not to a degree that it strains belief) and by noting, but simultaneously diminishing, the effective disparities of power within and between regions. Thus, _things tend to happen when the player is around, and not happen so much when they're not around_.

I call this "juice" in the sense of gasoline or energy. The region currently occupied by the player has the highest juice; those neighboring have somewhat less, and those at a higher degree of separation have still less. Juice "lingers" after a player leaves a region; if the player leaves a nasty situation, we don't want to just pause it until they get back. We want them to return and see the aftermath.

When a region's juice is sufficiently low, there's no reason to keep it in memory anymore. Then it can be unloaded.

We can describe _J<sub>current</sub>_, the total current juice of the region, as follows:

- _J<sub>current</sub>_ = _w<sub>proximity</sub>_ ⋅ _J<sub>proximity</sub>_ + _w<sub>linger</sub>_ ⋅ _J<sub>linger</sub>_
  - _w<sub>proximity</sub>_ is some fractional weight for the proximity factor
  - _w<sub>linger</sub>_ is some fractional weight for the lingering factor
  - _J<sub>proximity</sub>_ (_d_) = _J<sub>0</sub>_ ⋅ _e<sup>-λd</sup>_
    - _J<sub>proximity</sub>_ (_d_) is the juice level at distance _d_
    - _J<sub>0</sub>_ is the juice level at the player's location
    - λ is the decay constant, determining how quickly the juice level decreases with distance
    - _d_ is the distance from the player’s current position.
  - _J<sub>linger</sub>_ (_t_) = _L_ ÷ ( 1 + _e<sup>-k(t - t<sub>0</sub>) </sup>_ )
    - _J<sub>linger</sub>_ (_t_) is the juice level at time _t_
    - _L_ is the maximum juice level
    - _k_ is the steepness of the curve, determining how quickly the juice level falls off
    - _t<sub>0</sub>_ is the time at which the player leaves the region, serving as the midpoint of the sigmoid curve

If _J<sub>current</sub>_ falls below some threshold value _J<sub>min</sub>_, the region is declared to be effectively devoid of interest and can be unloaded until such a time that its juice once again exceeds that threshold.

The following visualization simulates these changes to _J<sub>current</sub>_; the character moves between chunks, and chunks are loaded (invisible -> visible) and are unloaded (visible -> invisible).

![Jcurrent changes](./images/juice_simulator.svg)

In addition, we can also monitor a longer-term cumulative trend:

- _J<sub>engagement</sub>_ = _J<sub>engagement</sub>_ + Δ _J_ ⋅ _t_
  - _J<sub>engagement</sub>_ is a cumulative measurement of how much a player has engaged with a region.
  - Δ _J_ ⋅ _t_ is a measure of how much the juice has changed this turn. This will always be at least zero, though it may be rounded down to zero judiciously.

And we can compare the current state to that historic trend:

- _J<sub>decay</sub>_ = _J<sub>engagement</sub>_ ⋅ _e<sup>-λ(t - t<sub>0</sub>)</sup>_
  - _J<sub>decay</sub>_ is an indicator of how much a player has engaged with a region _recently_
  - λ is the decay constant, determining how quickly the juice level decreases with time since last engagement
  - _t<sub>0</sub>_ is the time at which the player last interacted with the region

This can be used as a trigger for "hey, remember me?" type events:

- the player receives a letter/newspaper/warning/threat/bounty hunter from a person in `$region`
- a loose end from the current quest ties off with a loose end from a quest in `$region`
- etc

That's not particularly important right now; we'll deal with other aspects of chunk management moving forward.

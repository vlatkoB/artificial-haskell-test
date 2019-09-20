# artificial-haskell-test

This is a solution for the task given @ https://gist.github.com/pwm/c1fee0dc2613eb9b38384de01fcb86a7

TL;DR
Given a 1D scroll consisting of only '~' and '#' characters, parse it, detect villages anf calc their population. Roll that 1D scroll onto a square spiral starting in the middle and going clockwise to get a 2D world. Form the islands of nearby villages and count the population of those islands. Present the maximum population of an island.


This task has three main parts:
1. Parse initial scroll to find the villages and their population
2. Convert the scroll to a 2D world
3. Find islands and count their population to find maximum population of an island


First one is rather simple, but very slow if executed sequentially. Running it concurrently, on 1 CPU, yields a significant speed improvement. Sequential ~1323 ms, concurrent ~168 ms. Almost 8 times.

Second problem was the most interesting one because it has several solution.
First that came to mind was to use the mutable arrays and peek/poke values. Would be suited for most imperative languages.
That, however, did not seem very exciting (nor functional), so I decided to give it a little twist.

I made template(s) of the world and simply mapped the parsed scroll over it. That enables to use the same code for different worlds, just by specifying a different template. As an example of this approach there is a zig-zag world template.

This is all nice and pretty, but it could use a lot of memory, depending on the scroll size. The whole template has to be in memory so that scroll can be mapped over it. And 2D world is also needed to find out which villages are close neighbours.

So, why not create a function that can calculate the coordinates of a village based on its sequence number. Exactly that is what direct coordination search does. No template at all. Done for SpiralWorld only.
Criterion shows that this approach is about two times faster.

Scroll processing options:

 - `--spiral`      - apply scroll to spiral world template
 - `--zig-zag`     - apply scroll to zig-zag world template
 - `--no-template` - calculate population via spiral world, but without template, direct coordinate calculation
 - `--scroll`      - specify the path of the scroll
 - `--chunk_size`  - if not present, sequential processing is used
                   - if present, specify the size of chunks in Bytes to spread over CPUs, use +RTS -Nx to specify num of cores

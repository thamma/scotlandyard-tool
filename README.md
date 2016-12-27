# scotlandyard-tool

As a friend of mine recently received the boardgame Scotland Yard as a gift, i quickly figured that the problem the players are supposed to solve is very similar to the [pursuit-evasion-game](https://en.wikipedia.org/wiki/Pursuit-evasion).

For entertainments sake i quickly implemented a solver for the problem in haskell - admittedly with a horrible runtime, but who cares with this boardsize? If used in actual gameplay, this will grant a big, unfair advantage to the seekers if MisterX does not play smart at all - which is rarely given when casually playing.

# Run it
To run the tool, simply `cat` the mapfile into the input stream of the executable `scotlandyard`.
You call the program with the arguments `./scotlandyard [start] [transition...]` where `start` is the number of the tile MisterX starts on and `transition` is a sequence of space separated moves. Moves consist of one single character `t`, `b`, `m` or `a` respectively meaning taxi, bus, metro and "any" (which is used if the mean of transport is unknown to the player).

#Map file
Each line in the mapfile consits of the mean of transport, the nodes id and a list of outgoing edges resembled by the target vertex' id.
For example this graph (included as `map2.map`)
![Graph](/images/samplegraph.png)

can be resembled by
```
m 1 2
m 2 1 4
m 4 2
t 1 3
t 2 3
t 3 1 2 4
t 4 3
b 1 4
b 4 1 5
b 5 4
s 2 5
s 5 2
```

and executing `cat map2.map | ./scotlandyard 1 m s a` returns the list of possible states `[2,4]` since starting in state 1, taking the metro, taking the ship and finally taking anything, leaves us being an state 2 or 4. You can find the latest SLY map named `map.map` in the repository.

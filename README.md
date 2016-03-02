# chicken-graphs

Provides basic combinatorial graph functionality for CHICKEN Scheme 4.9+.

## Contributing

This repository is mirrored on both
[Bitbucket](https://bitbucket.org/ThatGeoGuy/chicken-minikanren/) and
[Github](https://github.com/ThatGeoGuy/chicken-miniKanren). To avoid
duplication of effort, please submit issues via Bitbucket. I have enabled
anonymous issue reporting in case you do not wish to create a Bitbucket account
for the sake of uploading an issue.

## History

I developed this library as a simple way to play around with various graph
concepts as I was teaching myself graph theory during my master's. The library
is meant to be flexible, easy to use, and easy to learn from. I believe the
library is more or less complete in terms of graph primitives, although some of
the performance characteristics are annoying due to SRFI-69 hash-tables and how
I store the data. High performance graph functionality will require destructive
operations, which is the majority of my gripe.

I have also considered switching from COOPS, which is CHICKEN Scheme's object
system, similar to CLOS in many ways, to a more low-level approach using
Records. However, some of the cohesion and flexibility between
(multi)(di)graphs within the library would be lost without generic methods.
Nonetheless, if there's any suggestions to improving the library or using it in
an interesting way, please let me know, as I am very interested, at least
academically. :)

## License

Released under the BSD3 license (same as CHICKEN Scheme). See LICENSE for more details.

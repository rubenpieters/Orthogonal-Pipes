# Orthogonal-Pipes

This library contains the orthogonal pipes encoding inspired by Spivey's [Faster Coroutine Pipelines](https://dl.acm.org/citation.cfm?id=3110249).
The encoding in this library also supports request/respond in addition to yield/await.

## Main Use Case

The main use case for this library is pipes containing a huge amount of merges (`>->` or `>+>` in Pipes).
The used encoding is much faster for these use cases.

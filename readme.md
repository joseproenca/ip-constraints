Interactive Partial Constraints
===============================

This project encodes in [Scala](http://www.scala-lang.org) a constraint-based engine for running connectors based on [Reo](http://reo.project.cwi.nl).

The coordination engine operates in rounds, each of which proceeds by collecting constraints from components and the connector that coordinates them, and then solving the constraints. Components perform blocking reads and writes on ports, which are converted into constraints stating that they want to output or input data. A solution to the constraints describes how data flows between the components, after which some reads and writes may succeed. Each round is considered to be an atomic (or synchronous) step. Between rounds the states of the components and connectors may change.

In practice, connectors are specified as the composition of simpler primitive connectors, and each of these primitive connectors specifies:

  * its coordination constraints; and
  
  * how to update its state based on the solution for the last round.

To give a flavour of how to define primitive connectors, a few examples of primitive connectors can be found in [Connectors](code/src/reopp/common/guardedcommands/dataconnectors).

Examples of some more complex connectors built using these primitive ones can be found in [Examples](code/src/reopp/common/examples).


<!-- 
Examples
--------
Examples of success and failure of trying to type families of connectors can be found below.

```scala
if (a) {b}
```
 -->
Primitive connectors with guarded commands
==========================================

A primitive connector describes both its behaviour for the current round, and how to update its state based on the choice for the previous round.
Intuitively, it can be seen as a state machine, allowing only the observation of its current state (stating what steps can be made), and the request to take one of these steps.

The current state (the steps that can be made) are given by a logical formula, specified as a set of _guarded commands_. Each solution for these constraints describes a possible step. The request to take one these steps is made via a method called ```updateConstraints```, which given a solution updates the internal state of a connector.

The template for a primitive connector is as follows:

```scala
class MyConnector(...) extends GCConnector(listOfPorts:List[String], uniqueID:Int) {
  
  /* Internal functionality */
  ...
  
  /** Returns the constraints of the current round. */
  def getConstraints: Formula = ...

  /** Returns the constraints of the current round. */
  override def update(s: OptionSol[GCSolution]) {
    ...
  }
}

```

Formulas are sets of guarded commands, each specified by the following grammar.

```
GCOM      ::= GUARD --> STATEMENT
GUARD     ::= var  |  var :<  pred  |  GUARD /\ GUARD  |  GUARD \/ GUARD
           |  !GUARD  |  GUARD -> GUARD  |  GUARD <-> GUARD  |
STATEMENT ::= var  |  var := var  |  STATEMENT /\ STATEMENT  |  var := (func,vars) 
```

In this case, a predicate ```pred``` is defined as an arbitrary scala function that, given a parameter ```x:Any``` returns a boolean, and a function ```func``` that returns a value instead. Each variable ```var``` is created based on a name (string) and the ```uniqueID``` of the  connector. Finally, ```vars``` represents a variable or a list of variables (type ```List[Var]```). The following example shows how to define a simple connector with two ports and a buffer, which can be empty or full. When empty, it can receive data from its input port, becoming full, and when full it can send its buffered value through the output port. As a twist, it can also receive a value and send it atomically (in the same round) to its output port, after being transformed: if the data is an integer, it will be doubled, otherwise it will be transformed into a string.

```scala
import reopp.common.guardedcommands._
import reopp.common._

class TransfFifo(ins: String, outs: String, var data: Option[Any], uid: Int = 0)
    extends GCConnector(List(ins,outs), uid) {

  val in  = mkVar(ins)
  val out = mkVar(outs)

  val double = Function("double") {
    case i:Int => i*2
    case x     => x.toString
  }
  
  val emptyFifo = Formula(
    out --> (in /\ (out := (double,in)))
  )

  def fullFifo = Formula(
    in --> (out /\ (out := (double,in))),
    (!in /\ out) --> (out := data.get)
  )

  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: OptionSol[GCSolution]) = s match  {
    case SomeSol(sol) =>
      if ((sol hasFlowOn in) && !(sol hasFlowOn out)) {
        // data goes in
        data = Some(sol(out.dataName))
      }
      else if ((sol hasFlowOn out) && !(sol hasFlowOn out)) {
        // data goes out
        data = None
      }
    case _ => {}
  }  	
}
```
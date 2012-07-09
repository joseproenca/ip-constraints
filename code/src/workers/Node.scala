package workers

import common.beh.{Behaviour, Solution, Constraints}
import actors.OutputChannel


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:18
 * To change this template use File | Settings | File Templates.
 */

abstract class Node[S<:Solution, C<:Constraints[S,C]]
    (deployer: OutputChannel[Any]) {



  // abstract method:
  val behaviour: Behaviour[S, C]

  var invConnections: Map[String, Set[Node[S,C]]] = Map() withDefaultValue(Set[Node[S,C]]())

  def getNeighbours(): Iterable[Node[S,C]] = invConnections.values.flatten

  //  var neighbours = List[Node[S,C]]() // order MUST be the same as the order of ends in behaviour
//  var neighbours = Set[OutputChannel[Any]]()

  // shared lock
  val lock:scala.concurrent.Lock = new scala.concurrent.Lock()
  var owner: Option[OutputChannel[Any]] = None

//  // what ends depend on "end" - just a guess to decide when to search for a solution
//  def dependsOn(end: String): Set[String]

  // suggests which ends must have dataflow if "end" has also dataflow
  // - used as an heuristics in the traversal, to know where to traverse first,
  // -                                         and know what to collect before solving constraints
  def guessRequirements(nd:Node[S,C]): Set[Node[S,C]]


  // Auxiliar functions

  def init() {
    //    println("INIT? ["+hashCode()+"] "+behaviour.isProactive)
    if (behaviour.isProactive) deployer ! this
  }



  def connect(other:Node[S,C],ends:Set[(String,String)]) {
    this.behaviour.connections +=
      other -> (for ((myend,otherend) <- ends) yield (myend,otherend,other.behaviour.uid))
    other.behaviour.connections +=
      this -> (for ((myend,otherend) <- ends) yield (otherend,myend,this.behaviour.uid))

    for ((myend,otherend) <- ends) {
//      val myendNodes: Set[Node[S,C]] = this.invConnections(myend)
      val newMyEndNodes:Set[Node[S,C]] = this.invConnections(myend) ++ Set(other)
      val newOtherEndNodes:Set[Node[S,C]] = other.invConnections(otherend) ++ Set(this)
      this.invConnections  += myend -> newMyEndNodes //(this.invConnections(myend) ++ Set(other))
      other.invConnections += otherend -> newOtherEndNodes
    }
//    this.neighbours ::= other
//    other.neighbours ::= this
  }

  def connect(other:Node[S,C],myend:String,otherend:String) {
    this.behaviour.connections +=
      other -> Set((myend,otherend,other.behaviour.uid))
    other.behaviour.connections +=
      this -> Set((otherend,myend,this.behaviour.uid))

    //      val myendNodes: Set[Node[S,C]] = this.invConnections(myend)
    val newMyEndNodes:Set[Node[S,C]] = this.invConnections(myend) ++ Set(other)
    val newOtherEndNodes:Set[Node[S,C]] = other.invConnections(otherend) ++ Set(this)
    this.invConnections  += myend -> newMyEndNodes //(this.invConnections(myend) ++ Set(other))
    other.invConnections += otherend -> newOtherEndNodes
    //    this.neighbours ::= other
//    other.neighbours ::= this
  }


//  def update(s:S) // to be overriden
//  def update(s:S) {
//    behaviour.update(s)
//    init
//  }
}

package workers

import common.beh.{Connector, Solution, Constraints}
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
  val behaviour: Connector[S, C]

  // neighbours to pairs of sync'd ends
  var connections    = Map[Node[S,C],Set[(String,String,Int)]]()
  var invConnections = Map[String, Set[Node[S,C]]]() withDefaultValue(Set[Node[S,C]]())

  var flowconn = Set[(String,Int,String,Int)]()

  def getNeighbours: Iterable[Node[S,C]] = invConnections.values.flatten

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

  def apply(e:String): End[S,C] = new End(this,e)


//  /**
//   * Add to connections from this and the other node, so we know how
//   * to traverse the graph of nodes.
//   * Also add to flow connection, to know how to plug ends to describe the behaviour.
//   * ORDER MATTERS: (mysourceend,othersinkend)
//   * @param other
//   * @param ends
//   */
//  def connect(other:Node[S,C],ends:Set[(String,String)]) {
//    // forward connections
//    this.connections +=
//      other -> (for ((myend,otherend) <- ends) yield (myend,otherend,other.behaviour.uid))
//    other.connections +=
//      this -> (for ((myend,otherend) <- ends) yield (otherend,myend,this.behaviour.uid))
//
//    // backward connections
//    for ((myend,otherend) <- ends) {
////      val myendNodes: Set[Node[S,C]] = this.invConnections(myend)
//      val newMyEndNodes:Set[Node[S,C]] = this.invConnections(myend) ++ Set(other)
//      val newOtherEndNodes:Set[Node[S,C]] = other.invConnections(otherend) ++ Set(this)
//      this.invConnections  += myend -> newMyEndNodes //(this.invConnections(myend) ++ Set(other))
//      other.invConnections += otherend -> newOtherEndNodes
//    }
//
//    // flow connections
//    for ((myend,otherend) <- ends)
//      flowconn += ((myend,behaviour.uid,otherend,other.behaviour.uid))
//
////    this.neighbours ::= other
////    other.neighbours ::= this
//  }

//  /**
//   * Add to connections from this and the other node, so we know how
//   * to traverse the graph of nodes.
//   * Also add to flow connection, to know how to plug ends to describe the behaviour.
//   * ORDER MATTERS: (mysourceend,othersinkend)
//   * @param other
//   * @param myend
//   * @param otherend
//   */
//  def connect(other:Node[S,C],myend:String,otherend:String) {
//    //println("connecting "+otherend+"-->"+myend)
//
//    this.connections +=
//      other -> Set((myend,otherend,other.behaviour.uid))
//    other.connections +=
//      this -> Set((otherend,myend,this.behaviour.uid))
//
//    //      val myendNodes: Set[Node[S,C]] = this.invConnections(myend)
//    val newMyEndNodes:Set[Node[S,C]] = this.invConnections(myend) ++ Set(other)
//    val newOtherEndNodes:Set[Node[S,C]] = other.invConnections(otherend) ++ Set(this)
//    this.invConnections  += myend -> newMyEndNodes //(this.invConnections(myend) ++ Set(other))
//    other.invConnections += otherend -> newOtherEndNodes
//
//    // flow connections
//    flowconn += ((myend,behaviour.uid,otherend,other.behaviour.uid))
//
////    this.neighbours ::= other
////    other.neighbours ::= this
//  }


//  def update(s:S) // to be overriden
//  def update(s:S) {
//    behaviour.update(s)
//    init
//  }
}


/////////////////////////
// Elegant connections //
/////////////////////////

//  private val thisactor = this
class End[S<:Solution, C<:Constraints[S,C]](val n: Node[S,C], val e: String) {
  /**
   * Add to connections from this and the other node, so we know how
   * to traverse the graph of nodes.
   * Also add to flow connection, to know how to plug ends to describe the behaviour.
   * ORDER MATTERS: (mysourceend,othersinkend)
   */
  def <--(e2: End[S,C]) {
    //a.connect(e2.a,e,e2.e)
    //    println("connecting "+otherend+"-"+other.myrank+"-->"+myend+"-"+myrank)
    val me = n
    val other = e2.n
    val otherend = e2.e
    val myend = e


    // better design: expose connections and flowconn only via an interface...
    me.connections +=
      other -> Set((myend,otherend,other.behaviour.uid))
    other.connections +=
      me -> Set((otherend,myend,me.behaviour.uid))

    //      val myendNodes: Set[Node[S,C]] = this.invConnections(myend)
    val newMyEndNodes:Set[Node[S,C]] = me.invConnections(myend) ++ Set(other)
    val newOtherEndNodes:Set[Node[S,C]] = other.invConnections(otherend) ++ Set(me)
    me.invConnections  += myend -> newMyEndNodes //(this.invConnections(myend) ++ Set(other))
    other.invConnections += otherend -> newOtherEndNodes

    // flow connections
    me.flowconn += ((myend,me.behaviour.uid,otherend,other.behaviour.uid))
  }
}

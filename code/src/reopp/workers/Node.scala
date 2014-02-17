package reopp.workers

import reopp.common._
import actors.OutputChannel
import reopp.common.{Connector, CBuilder, Solution, Constraints}


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:18
 * To change this template use File | Settings | File Templates.
 */

abstract class Node[S<:Solution, C<:Constraints[S,C]] {
//    (deployer: OutputChannel[Any]) {

  val uid = hashCode()

  // abstract method. UID of connector should be the same as the node.
  val connector: Connector[S, C]

  // neighbours to pairs of sync'd ends
  protected var connections    = Map[Node[S,C],Set[(String,String)]]()
  protected var invConnections = Map[String, Set[Node[S,C]]]() withDefaultValue(Set[Node[S,C]]())

  /** Represents direction of dataflow - from the right pair to the left pair. */
//  var flowconn = Set[(String,Int,String,Int)]()
  private var sourceEnds = Set[String]()

  /** Adds a new connection between a local end and a remote end. */
  def addConnection(myend:String,otherend:String,other:Node[S,C]) {
    connections +=
      other -> Set((myend,otherend))
//	val newMyEndNodes:Set[Node[S,C]] = invConnections(myend) ++ Set(other)
    invConnections  += myend -> (invConnections(myend) ++ Set(other))
  }
  
  /** Marks a port as being an input port.
   *  Needed for adding the right constraints when connecting nodes. */
  def addSourceEnd(end:String) = sourceEnds += end
  /** Checks if a given port is an input port. */
  def hasSourceEnd(end:String) = sourceEnds contains end
  
  /** Checks if there is a connection to another node. */
  def connectedTo(other:Node[S,C]) =
    connections contains other
  /** Gets all the end names connected to a given node. */
  def getConnectedEndsTo(other:Node[S,C]): Iterable[(String,String)] =
    connections(other)
//    for (e <- connections(other)) yield e._1
  /** Get all the neighbour nodes. */
  def getNeighbours: Iterable[Node[S,C]] = invConnections.values.flatten


//  // what ends depend on "end" - just a guess to decide when to search for a solution
//  def dependsOn(end: String): Set[String]

  // suggests which ends must have dataflow if "end" has also dataflow
  // - used as an heuristics in the traversal, to know where to traverse first,
  // -                                         and know what to collect before solving constraints
  def guessRequirements(nd:Node[S,C]): Set[Node[S,C]]


  // Auxiliar functions

  def canStart(): Boolean = {
//    println("INIT? nd@["+hashCode()+"] "+connector.isProactive)
//    if (connector.isProactive) deployer ! this
    connector.isProactive
  }

  def apply(e:String): End[S,C] = new End(this,e)

  override def toString = 
    //s"nd[${hashCode.toString.substring(5)}]"
    s"{${connector.ends.mkString(".")}}"

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

object Node {
  /**
   * Creates a new node, with a new connector, linked to a deployer (with a strategy).
   * @param deployer the associated [[Deployer]] reference
   * @param deps pairs of dependent port names, used by the hybrid strategy ([[strategies.HybridStrategy]]).
   *    		For each (a,b), if 'a' is not on the border of the region, 'b' cannot be either.
   * @param priority priority port names, used by the hybrid strategy ([[strategies.HybridStrategy]]).
   * 			Every priority port must be connected before searching for a solution. 
   * @param conn function that, given a unique ID (of the node), returns the connector of this node.
   */
  def apply[S<:Solution, C<:Constraints[S,C]]
      (   deps: Iterable[(String,String)],
          priority: Iterable[String],
          conn : Int => Connector[S,C])
      (implicit b:CBuilder[S,C]): Node[S,C] =
    new Node[S,C] {
      //      val uid = this.hashCode()
      val connector = conn(uid)

      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(nd: Node[S, C]) = {
        var res: Set[Node[S,C]] = Set()
        if (!deps.isEmpty && connections.contains(nd)) {
          for ((a,b) <- deps)
            if (invConnections contains a) // if 'a' is connected
              res ++= invConnections(b)	   // then 'b's connections are required
        }
        for (p <- priority)
          res ++= invConnections(p)
        res
      }
    }


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

    // connect the ends
    me.addConnection(myend, otherend, other)
    other.addConnection(otherend, myend, me)

    // flow connections - data flows from other to me.
//    me.flowconn += ((myend,me.connector.uid,otherend,other.connector.uid))
    me.addSourceEnd(myend)
  }
}

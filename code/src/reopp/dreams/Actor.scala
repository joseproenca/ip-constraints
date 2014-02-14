package reopp.dreams

import actors.OutputChannel
import reopp.common._
import scala.Some
import reopp.common.guardedcommands.GCConnector.GCBuilder
import reopp.common.{Solution, Connector, Constraints, CBuilder}

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 13:42
 * To change this template use File | Settings | File Templates.
 */

abstract class Actor[S<:Solution, C<:Constraints[S,C]](implicit noSol:EmptySol[S], b:CBuilder[S,C])
  extends scala.actors.Actor {

  val uid = hashCode()

  var isIdle = false

  // to be overriden:
  val behaviour: Connector[S, C]
  val myrank = this.hashCode() // default ranking
  
//  type Constr = behaviour.constraints.mytype
  type ActorRef = OutputChannel[Any]
  var neighbours = Set[OutputChannel[Any]]()
  var connections    = Map[OutputChannel[Any],Set[(String,String,Int)]]()
  var flowconn = Set[(String,Int,String,Int)]()


//  /**
//   * Add to connections from this and the other actor, so we know how
//   * to traverse the graph of actors.
//   * Also add to flow connection, to know how to plug ends to describe the behaviour.
//   * ORDER MATTERS: (mysourceend,othersinkend)
//   * @param other
//   * @param myend
//   * @param otherend
//   */
//  def connect(other:Actor[S,C],myend:String,otherend:String) {
////    println("connecting "+otherend+"-"+other.myrank+"-->"+myend+"-"+myrank)
//
//    this.connections +=
//      other -> Set((myend,otherend,other.behaviour.uid))
//    other.connections +=
//      this -> Set((otherend,myend,this.behaviour.uid))
//
//    // flow connections
//    flowconn += ((myend,behaviour.uid,otherend,other.behaviour.uid))
////    println("new flowconn: "+flowconn)
//  }



  def init: Nothing = {
    if (behaviour.isProactive) {
      var invited: Map[ActorRef,Int] = Map()
      for (a <- neighbours) {
        a ! RequestBeh(myrank)
        invited += a -> myrank
      }
      stateCommitting(myrank,None,invited,sync(behaviour.getConstraints))
    }
    else stateIdle    
  }


//  def start(implicit nosol: EmptySol[S]) {
//    super.start()
//  }
  
  def act() { stateSuspended(Set()) }

  ////////////////  
  //    IDLE    //
  ////////////////  

  def stateIdle: Nothing = {
    debug("Idle."); isIdle = true; react {
    case Suspend => stateSuspended(Set(sender))
    case RequestBeh(rank:Int) => {
      val children: Set[ActorRef] = neighbours - sender
      if (children.isEmpty) {
    	throw new RuntimeException("Need to fix strange type error below...")
//        sender ! ReplyBeh[C](sync(behaviour.getConstraints))
        stateCommitted
      }
      else {
        // sendRequests(rank,children)
        for (c <- children)
          c ! RequestBeh(rank)
        val invited = Map() ++ (for (c <- children) yield c -> rank)
        stateCommitting(rank,Some(sender),invited,sync(behaviour.getConstraints))
      }         
    }
    case Update(s:S) => stateIdle // when an update comes to a split channel
    case Admin('KILL) => {}
    case _ => stateIdle
  }}
  
  ////////////////  
  // COMMITTING //
  ////////////////  

  def stateCommitting
  (rank:Int, root:Option[ActorRef], invited:Map[ActorRef,Int],temp: C): Nothing = {
    debug("Committting."); isIdle = false; react {
    case RequestBeh(newrank:Int)  => processRequest(newrank,rank,root,invited,temp)
    case StrongerReq(newrank:Int) => processRequest(newrank,rank,root,invited,temp)
    case ReplyBeh(c:C) =>
        updCommitting(rank,root,invited, sync(temp ++ c)) //behaviour.sync(sender,temp ++ c))
    case Busy =>
        updCommitting(rank,root,invited,temp)
    // case Admin...
  }}


  // TODO: TEST CODE!!!
//  private def sync(otherref: OutputChannel[Any], basec: C)(implicit cbuilder: CBuilder[S,C]): C = {
//    println("synching "+myrank+" <-> "+otherref.hashCode())
//    println("connections: "+connections.keys.map(_.hashCode()))
//    if (!(connections contains otherref))
//      basec
//    else {
//      val other = connections(otherref)._1
//      val otherid = other.behaviour.uid
//      val me = behaviour.uid
//      var res = basec
//
//      for ((e1,u1,e2,u2) <- flowconn)
//        if (u2 == otherid) res ++= cbuilder.sync(e1,u1,e2,u2)
//      for ((e2,u2,e1,u1) <- other.flowconn)
//        if (u1 == me)      res ++= cbuilder.sync(e2,u2,e1,u1)
//      println("## my flowconn:    "+flowconn)
//      println("## other flowconn: "+other.flowconn)
//      println("new constraints: "+res)
//      res
//    }
//  }

private def sync(basec: C)(implicit cbuilder: CBuilder[S,C]): C = {
//  println("synching all source ends of "+myrank)
  var res = basec
  for ((e1,u1,e2,u2) <- flowconn)
    res ++= cbuilder.sync(e1,u1,e2,u2)
//  println("new constraints: "+res)
  res
}



  private def processRequest(newrank:Int,rank:Int,root:Option[ActorRef],invited:Map[ActorRef,Int],temp:C): Nothing = {
    assert(invited contains sender)
    val srank = invited(sender)
    
    if (newrank == rank) {
      if (srank < newrank)
        sender ! Busy
      updCommitting(rank,root,invited,temp)
    }
    else if (srank <= newrank && newrank < rank) {
      sender ! StrongerReq(rank)
      stateCommitting(rank,root,invited,temp)
    }
    else if (newrank < srank && srank <= rank)
      stateCommitting(rank,root,invited,temp)
    else // (newrank > rank)
      if (root.isDefined) {
        root.get ! RequestBeh(newrank)
        val invited2 = (invited - sender) + (root.get -> newrank)
        stateCommitting(newrank,Some(sender),invited2,temp)
      }
      else
      updCommitting(newrank,root,invited,temp) //stateCommitting(newrank,Some(sender),invited - sender,temp)
  }
  
  private def updCommitting(rank:Int, root:Option[ActorRef], invited:Map[ActorRef,Int], c:C): Nothing = {
    val invited2 = invited - sender
    if (!invited2.isEmpty)
      stateCommitting(rank,root,invited2,c)
    else if (root.isDefined)
      stateCommitted
    else { // proactive actor
      println("found constraints:\n"+c.toString)
      val sol = c.solve
//      if (sol.isDefined)
//        processSol(sol.get,freshSol = true)
//      else
//        processSol(noSol.sol,freshSol = true)
      processSol(sol,freshSol = true)
    }
  }


  ////////////////  
  // COMMITTED  //
  ////////////////  

  def stateCommitted : Nothing = {
    debug("Committted."); isIdle = false; react {
      case StrongerReq(rank:Int) => {
        sender ! Busy
        stateCommitted
      }
      case ReplySol(s:OptionSol[S]) => processSol(s,freshSol = false)
      //    case ReplyData(d:Any) => processData(d)
      //    case ReplySolData(s:S,d:Any) => processSolData(s,d)
    }
  }

  protected def processSol(sol:OptionSol[S],freshSol:Boolean): Nothing = { //,rcvd: Map[ActorRef,Any]) = act
    if (freshSol) debug("got solution!\n"+sol)
    val interested = if (freshSol) neighbours else neighbours - sender
    for (a <- interested) {
      a ! ReplySol(sol)
      debug("sending solution to "+a.hashCode())
    }
    behaviour.update(sol)
    init
  }


//  private def processData(d:Any,rcvd: Map[ActorRef,Any]) = act
//  private def processSolData(sol:S,d:Any,rcvd: Map[ActorRef,Any]) = act

  ////////////////  
  // SUSPENDED  //
  ////////////////  
  
  def stateSuspended(locks:Set[OutputChannel[Any]]) : Nothing = {
    debug("suspended."); isIdle = true; react {
    case Release => {
      val newlocks = locks - sender
      if (newlocks.isEmpty) init
      else stateSuspended(newlocks)
    }
    case Suspend => stateSuspended(locks + sender)
    case Connect(other:ActorRef) => {
      neighbours += other
      stateSuspended(locks)
    }
  }}



  def apply(e:String) : End[S,C] = new End(this,e)


  /// DEBUG
  val db = true
  private def debug(s:String) { if (db) println("["+hashCode()+"] "+s) }
}

object Actor {
  def apply[S<:Solution, C<:Constraints[S,C]]
  (conn : Int => Connector[S,C])
  (implicit noSol:EmptySol[S], b:CBuilder[S,C]): Actor[S,C] =
    new Actor[S,C]() {
      //      val uid = this.hashCode()
      val behaviour = conn(uid)
    }
}


/////////////////////////
// Elegant connections //
/////////////////////////

//  private val thisactor = this
class End[S<:Solution, C<:Constraints[S,C]](val a: Actor[S,C], val e: String) {
  /**
   * Add to connections from this and the other actor, so we know how
   * to traverse the graph of actors.
   * Also add to flow connection, to know how to plug ends to describe the behaviour.
   * ORDER MATTERS: (mysourceend,othersinkend)
//     * @param other
//     * @param myend
//     * @param otherend
   */
  def <--(e2: End[S,C]) {
    //a.connect(e2.a,e,e2.e)
    //    println("connecting "+otherend+"-"+other.myrank+"-->"+myend+"-"+myrank)
    val me = a
    val other = e2.a
    val otherend = e2.e
    val myend = e

    // better design: expose connections and flowconn only via an interface...
    me.connections +=
      other -> Set((myend,otherend,other.behaviour.uid))
    other.connections +=
      me -> Set((otherend,myend,me.behaviour.uid))

    // flow connections
    me.flowconn += ((myend,me.behaviour.uid,otherend,other.behaviour.uid))
    //    println("new flowconn: "+flowconn)
  }
}

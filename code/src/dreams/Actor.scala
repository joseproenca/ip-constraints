package dreams

import actors.OutputChannel
import common.beh.{EmptySol, Constraints, Connector, Solution}

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 13:42
 * To change this template use File | Settings | File Templates.
 */

abstract class Actor[S<:Solution, C<:Constraints[S,C]](implicit noSol:EmptySol[S])
  extends scala.actors.Actor {

  var isIdle = false

  // to be overriden:
  val behaviour: Connector[S, C]
  val myrank = this.hashCode() // default ranking
  
//  type Constr = behaviour.constraints.mytype
  type ActorRef = OutputChannel[Any]
  var neighbours = Set[OutputChannel[Any]]()


  def init: Nothing = {
    if (behaviour.isProactive) {
      var invited: Map[ActorRef,Int] = Map()
      for (a <- neighbours) {
        a ! RequestBeh(myrank)
        invited += a -> myrank
      }
      stateCommitting(myrank,None,invited,behaviour.getConstraints)
    }
    else stateIdle    
  }


//  def start(implicit nosol: EmptySol[S]) {
//    super.start()
//  }
  
  def act { stateSuspended(Set()) }

  ////////////////  
  //    IDLE    //
  ////////////////  

  def stateIdle: Nothing = {
    debug("Idle."); isIdle = true; react {
    case Suspend => stateSuspended(Set(sender))
    case RequestBeh(rank:Int) => {
      val children: Set[ActorRef] = neighbours - sender
      if (children.isEmpty) {
        sender ! ReplyBeh[C](behaviour.getConstraints)
        stateCommitted
      }
      else {
        // sendRequests(rank,children)
        for (c <- children)
          c ! RequestBeh(rank)
        val invited = Map() ++ (for (c <- children) yield c -> rank)
        stateCommitting(rank,Some(sender),invited,behaviour.getConstraints)
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
  (rank:Int, root:Option[ActorRef], invited:Map[ActorRef,Int],temp: C) : Nothing = {
    debug("Committting."); isIdle = false; react {
    case RequestBeh(newrank:Int)  => processRequest(newrank,rank,root,invited,temp)
    case StrongerReq(newrank:Int) => processRequest(newrank,rank,root,invited,temp)
    case ReplyBeh(c:C) =>
        updCommitting(rank,root,invited,behaviour.sync(sender,temp ++ c))
    case Busy =>
        updCommitting(rank,root,invited,temp)
    // case Admin...
  }}
  
  
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
      val sol = c.solve
      if (sol.isDefined)
        processSol(sol.get,true)
      else
        processSol(noSol.sol,true)
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
      case ReplySol(s:S) => processSol(s,false)
      //    case ReplyData(d:Any) => processData(d)
      //    case ReplySolData(s:S,d:Any) => processSolData(s,d)
    }
  }

  protected def processSol(sol:S,freshSol:Boolean): Nothing = { //,rcvd: Map[ActorRef,Any]) = act
    if (freshSol) debug("got solution!\n"+sol.pretty)
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

  /// DEBUG
  val db = true
  private def debug(s:String) { if (db) println("["+hashCode()+"] "+s) }
}

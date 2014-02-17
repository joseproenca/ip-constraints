package reopp.workers

import reopp.common.Solution
import reopp.common.Constraints
import reopp.workers.strategies.Strategy
import scala.collection.mutable.{Map => MMap,Set => MSet}
import akka.actor.Actor
import akka.actor.ActorRef

class ConflictManager//[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
//      (deployer: ActorRef)
      extends Actor {

  // type alias
  type Nd = AnyRef //Node[S,C]
//  type Wk = Worker[S,C,Str]
  type Worker = ActorRef
  
  private val deployer = context.parent
  
  // State
  private val nodesOf : MMap[Worker,MSet[Nd]] = MMap() // INV: sync with below
  private val ownerOf : MMap[Nd,Worker] = MMap()       // INV: sync with above
  private val prev: MMap[Worker,Worker] = MMap() // INV: acyclic
  private val next: MMap[Worker,Worker] = MMap() // INV: acyclic
  
  /**
   * Messages received.
   * From the workers:
   *  - Claim -> return the node or ask to give up
   *  - Strat -> forward the strategy to the current owner of a node, or ask to continue
   *  - Done -> clear references, and send "Quit" 
   *  - Fail -> clear references, and send "Quit" 
   */
//  def act: Unit = loop ({debug("waiting"); self.react {
  def receive = {
    case Claim(nd:AnyRef) =>
      debugMsg("Claim")
      if (ownerOf contains nd) {
        conflict(nd,sender,ownerOf(nd))          
      }
      else {
        addOwner(sender,nd)
        debug("sending node to sender "+pp(sender))
        sender ! Claimed(nd)
      }
    case StratNd(st:AnyRef,nd:Nd) =>
      debugMsg("StratNd")
       // strategy sent to the new owner of nd IF it has not finished without this strategy.
      // (otherwise the nodes has been updated with this other solution)
      if ((ownerOf contains nd) && smaller(sender,ownerOf(nd))) {
        debug("sending partial strat to the boss of "+pp(ownerOf(nd))+
        		" - "+pp(max(ownerOf(nd))))
        max(ownerOf(nd)) ! Strat(st)
      }
      else debug("Discarding received strategy. "+ownerOf.mkString(","))
      deployer ! WorkerDone
      // will be forgotten (or it was already) by its superiors. 
      sender ! Quit
    case Success =>
      debugMsg("Success")
      val nodes = collectNodes(sender)
      sender ! QuitAndUpdate(nodes)
      // not forgetting yet - only after update is confirmed, to avoid nodes being used
      //   before they are updated.
    case Fail =>
      debugMsg("Fail")
//      val nodes = collectNodes(sender)
//      sender ! QuitAndUpdate(nodes)
      deployer ! WorkerDone
      sender ! Quit // no need to update without a solution -> avoid continuously trying...
      forget(sender)
    case Updated(nds:Iterable[Nd]) =>
      debugMsg("Updated")
      for (n <- nds) deployer ! Task(n) // add tasks to deployer (it will check if proactive) 
      deployer ! WorkerDone // ONLY after sending the nodes (tasks), to avoid quitting earlier.
      forget(sender)
       // PROBLEM: higher workers could exist. If so, ask them to "quitAndUpdate", even if
       //  not quitting -- they are either waiting or quitting (with their solution) --
       //  and forget everything BELOW sender. The rest will be forgotten once the higher ones
       //  confirm "Updated (and quited)".
      quitHigherWorkers(sender)
    case IgnoredClaim(nd:Nd) =>
      debugMsg("Ignored claim - maybe forgetting owner.")
      // everything should be fine, BUT if it was from an old claim from a worker that was
      //  forgotten when the claim was received, then this worker needs to be re-forgotten.
      if ((ownerOf(nd) == sender) ) {
    	  if (!(next contains sender)) { // sender is still the main owner!
	          debug("Fixing wrong claim of a forgotten worker.")
	    	  if (prev contains sender) {    	    
	    	    for (n <- collectNodes(prev(sender))) deployer ! Task(n) 
	    	  }
	    	  forget(sender)
    	  }
    	  else { // bigger owner still needs the claimed node
    	    max(sender) ! Claimed(nd)
    	  }
      }        


    case Status =>
      debug(s"Status:\n + ownerOf: ${ownerOf.mkString(",")}\n | next: ${next.mkString(",")}")
//      exit
  }
  
  private def conflict(nd:Nd,wk1:Worker,wk2:Worker): Unit = {
    if      (smaller(wk1,wk2)) {} // do nothing - wk1 has been asked to give up.
    else if (larger(wk1,wk2)) {} // do nothing - wk2 has been asked to give up.
    else if (wk1 == wk2) {} // do nothing - merging of strategies causes some nodes to be reclaimed.
    else {
      val repr1 = max(wk1)
      val repr2 = min(wk2)
      repr1 ! GiveUp(nd)
      extendOrder(repr1,repr2)
    }
  }
  
  
  //// AUXILIARY state managers ///
  
  /** Adds a owner to the state. */
  private def addOwner(wk:Worker, nd:Nd) {
    ownerOf(nd) = wk
    if (nodesOf contains wk)
      nodesOf(wk) += nd
    else
      nodesOf(wk) = MSet(nd)  
  }  
  /** Returns the last worker in the temporal partial order. */
  private def max(wk:Worker): Worker = next.get(wk) match {
    case Some(w2) => max(w2)
    case _ => wk
  }
  /** Returns the first worker in the temporal partial order. */
  private def min(wk:Worker): Worker = prev.get(wk) match {
    case Some(w2) => min(w2)
    case _ => wk
  }
  /** Checks if the first worker is smaller than the second, if comparable. */
  private def smaller(wk1:Worker,wk2:Worker) : Boolean = prev.get(wk2) match {
    case Some(`wk1`) => true
    case Some(wk3) => smaller(wk1,wk3)
    case _ => false
  } 
  /** Checks if the first worker is greater than the second, if comparable. */
  private def larger(wk1:Worker,wk2:Worker) : Boolean = next.get(wk2) match {
    case Some(`wk1`) => true
    case Some(wk3) => larger(wk1,wk3)
    case _ => false
  } 
  /** Adds a pair to the partial order, ASSUMING the elements were not comparable. */
  private def extendOrder(wk1:Worker,wk2:Worker) {
    debug(s"extended order: $wk1 < $wk2")
    next(wk1) = wk2
    prev(wk2) = wk1
//    checkCycles(wk1,wk2) // DEBUGGING line
  }
  private def checkCycles(wk1:Worker,wk2:Worker) {
    var wk = next.get(wk1)
    while (wk.isDefined) {
      if (wk.get == wk1) throw new RuntimeException(s"FOUND CYCLE when adding $wk1<$wk2." +
    		  s" next: ${next.mkString(",")}")
      wk = next.get(wk.get)
    }
    wk = prev.get(wk1)
    while (wk.isDefined) {
      if (wk.get == wk1) throw new RuntimeException(s"FOUND CYCLE when adding $wk1<$wk2." +
    		  s" prev: ${next.mkString(",")}")
      wk = prev.get(wk.get)
    }
  }
  /** Drops references to a worker and its descendants. */
  private def forget(wk:Worker) {
	debug(s"forgetting: [${pp(wk)}]")
    if (nodesOf contains wk) {
      for (nd <- nodesOf(wk)) {
        ownerOf -= nd
      }
    }
    nodesOf -= wk
//    if (next contains wk)
//      quitHigherWorkers(wk) //ask them to quit and update(confirm), to be forgotten later.
//      throw new RuntimeException(s"forgetting a non-maximum worker $wk!\n"+
//    		  s"exiting.\n + ownerOf: ${ownerOf.mkString(",")}\n | next: ${next.mkString(",")}")
    if (prev contains wk) {
      val p = prev(wk)
      prev -= wk
      next -= p
      forget(p)
    }    
  }
  /** After forgetting wk, if a higher exists it is "killed", and asked to update its nodes.*/
  private def quitHigherWorkers(wk:Worker) {
    val big = max(wk)
    if (big != wk) {
      val nds = collectNodes(big)
      big ! QuitAndUpdate(nds)
    }
  }
  /** Collect all nodes owned by a worker. */
  private def collectNodes(wk:Worker) : Iterable[Nd] = {
    val nds: Set[Nd] = if (nodesOf contains wk) nodesOf(wk).toSet else Set()
    if (prev contains wk)
        nds ++ collectNodes(prev(wk))
      else
        nds
  } 
  /** Debug information */
  private def debug(msg: String) {
//    println("[CM] "+msg)
  }
  private def debugMsg(msg:String) {
//    debug(s" <- ${pp(sender)} $msg")
  }
  private def pp(a:AnyRef): String = "["+a.hashCode.toString.substring(5)+"]"
  
}
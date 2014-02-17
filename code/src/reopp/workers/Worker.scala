package reopp.workers

import actors.OutputChannel
import strategies.Strategy
import reopp.common.{OptionSol, Solution, CBuilder, Constraints}
import reopp.common.NoneSol
import reopp.common.SomeSol
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
//import scala.actors.Actor._


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:36
 * 
 * A worker is responsible for claiming nodes and search for solutions
 * within the claimed nodes, according to some given strategy.
 * A centralised conflict manager controls whether each node can be claimed,
 * and is the only source of communication with the worker.
 * The implicit builder is required to describe the behaviour of connecting
 * ends from different nodes.  
 */

class Worker[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
      (conflictMng: ActorRef, strat: Str)
      (implicit builder: CBuilder[S,C]) extends Actor {

  // type alias
  type Nd = Node[S,C]
  type Wk = Worker[S,C,Str]
  type ActorRef = OutputChannel[Any]

  // NO state (all state in the strategy).  
  

  
//  /** Request the ConflictManager to use a node. */
//  private def claim(node: Nd) {
//    conflictMng ! Claim(node)
//  }
  
  /** Receives requests and process them. */
//  def act(): Nothing = {debug("waiting"); self.react {
  def receive = {
    case Claim(n:Nd) =>
      debug("initial Claim (from deployer)")
      conflictMng ! Claim(n)
    case Claimed(n:Nd) =>
      debugMsg("gotNode "+n)
      strat.register(n)
      checkSolution
    case GiveUp(n:Nd) =>
      debugMsg("giveUp. quitting.")
      conflictMng ! StratNd(strat,n)
      context.become(quitting(NoneSol()))
    case Strat(otherStrat:Str) =>
      debugMsg("gotStrat")
      strat merge otherStrat
      checkSolution
    case QuitAndUpdate(ns:Iterable[Nd]) =>
      debugMsg("Forced to quit in advance!")
      self ! QuitAndUpdate(ns)
      context.become(quitting(NoneSol()))
    case x => bug("main - "+x)
  }
  
  /** ignore every message until quitting. */
//  private def quitting(sol: OptionSol[S]): Nothing = {debug("quitting"); self.react {
  private def quitting(sol: OptionSol[S]): Receive = {
    case Quit =>
      debug("got Quit. Bye!")
      context.stop(self)
    case QuitAndUpdate(ns:Iterable[Nd]) =>
      debug("updating and quitting. Bye!\n"+sol)
      for (n:Nd <- ns) {
        n.connector.update(sol)
//        n.init // start if ready --> NO, otherwise new workers will start before current and unlocked
      }
//      sender ! Updated
      conflictMng ! Updated(ns)
      context.stop(self)
    case Claimed(n:Nd) =>
      debug("informing about ignored claim "+n)
      conflictMng ! IgnoredClaim(n)
//      context.become(quitting(sol))
    case Strat(_) => debug("ignored strategy while quitting.")
    case x => bug("quitting - "+x) //; quitting(sol)}
  }
  
  /** Search for a solution (if possible), and expand later if necessary. */
  private def checkSolution {
    if (strat.canSolve) {
      val sol = strat.solve
      if (sol.isDefined) {
        debug("got solution! quitting.")
        conflictMng ! Success
        context.become(quitting(sol))
        return
      }
      else expand(sol)
    }
    // else (cannot solve or failed to solve)
    expand(NoneSol())
  }
  
  /** Get next nodes, and claim them. Quit if fail to expand. */
  private def expand(sol:OptionSol[S]) {
    val next = strat.nextNodes
    if (next.isEmpty){
      debug("failed to expand. quitting.")
      conflictMng ! Fail
      context.become(quitting(sol))
    }
    else {
      for (nd <- next)
        conflictMng ! Claim(nd)
    }
  }
  
  override def toString = s"[${hashCode.toString.substring(5)}]"



  def debug(msg: String) {
//    println("["+self.hashCode().toString.substring(5)+"] "+msg)
  }
  private def debugMsg(msg:String) = {
//    val other = if (sender == conflictMng) "CM"
//    		    else sender.hashCode().toString.substring(5)
//    debug(s" <- [$other] $msg")
  }

  def bug(x:Any) {
    debug(s"UNEXPECTED message - $x:${x.getClass}")
  }  
  
  def mark(msg: Char) {
//    print(msg)
  }
  
  
  private def debugGraph() {
	for (n <- strat.owned)
      debug(" o "+n.hashCode())
    for (n <- strat.fringe)
      debug(" f "+n.hashCode())
  }

}

object Worker {
//  def apply[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
//      (node:Node[S,C],deployer: OutputChannel[Any],conflictMng: ActorRef, strat:Str)
//      (implicit builder: CBuilder[S,C]) : Worker[S,C,Str] = {
//    val w = new Worker[S,C,Str](conflictMng,strat)
//    w.work(node) // w ! Claim(node)
//    w
//  }
  def props[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
      (conflictMng: ActorRef, strat:Str)
      (implicit builder: CBuilder[S,C]): Props =
      	Props(new Worker[S,C,Str](conflictMng,strat))

}

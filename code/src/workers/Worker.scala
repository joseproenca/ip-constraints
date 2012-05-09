package workers

import actors.OutputChannel
import common.beh.{Constraints, Solution}
import choco.kernel.solver.propagation.listener.SetPropagator

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:36
 * To change this template use File | Settings | File Templates.
 */

class Worker[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
      (deployer: OutputChannel[Any], strat: Str) extends scala.actors.Actor {

  // type alias
  type Nd = Node[S,C]
  type Wk = Worker[S,C,Str]
  type ActorRef = OutputChannel[Any]

  // state
  var inConflict = Set[ActorRef]()       // All conflicts sent
  var pendingConflicts = Set[ActorRef]() // conflict sent, not received
  var pendingWorkers = Set[ActorRef]()   // conflict received, acknowledged, and waiting for their graph
//  var locks = Set[ActorRef](this)      // conflict won: needed to release locks


  def work(node: Node[S,C]): Boolean = {
    debug("starting node - "+node.behaviour.connections.values.head.head._1)
    // ...
    val nodes = strat.initNodes(node)
    val claimed = claim(nodes)
    if (!claimed._1.isEmpty) {
      cleanLocks()
      false
    }
    else {
      start()
      this ! 'GO
      true
    }
  }

  def claim(nodes: Iterable[Nd]): (Iterable[ActorRef],Iterable[Nd]) = {
    var confls = Set[ActorRef]()
    var sampleNodes = Set[Nd]()
    for (n <- nodes) {
      var other: Option[ActorRef] = None
      n.synchronized {
        if (!n.owner.isDefined) {
          n.owner = Some(this)
        }
        else
          other = n.owner
      }
      if (!other.isDefined)
        strat register n
      else
        if (!(confls contains other.get)) {
          confls += other.get
          sampleNodes += n
      }
    }
    (confls,sampleNodes)
  }

  def cleanLocks() {
    for (nd <- strat.owned)
    //      if (nd.owner.isDefined)
    //        if (locks contains nd.owner.get) // maybe unecessary...
      nd.owner = None
  }
  def swapOwner(act: ActorRef) {
    for (nd <- strat.owned)
      nd.owner = Some(act)
  }

  def nextRound() {
    this ! 'GO
    act()
  }

  def success(sol:S) {
    debug("DONE (mb restarting)\n"+sol.pretty)
    for (n <- strat.owned) {
      n.behaviour.update(sol)
      n.init
      n.owner = None
    }
    deployer ! 'DONE
  }

  // will not send if it is already in conflict, and syncrhonises to ensure the receiver is alive.
  def safeSendConfl(nd: Nd) {
    if (nd.owner.isDefined)
      if (inConflict contains nd.owner.get)
        nd.synchronized {
          val act = nd.owner
          if (act.isDefined) {
            act.get ! 'CONFLICT
            pendingConflicts += act.get
          }
        }
  }

  /**
   * Wait for pending requests (traversal or conflict), clean mailbox, and quit.
   * Precond: locks are clean -> no one can send message to "this".
   *     | cleanLocks
   * @param reason message of why it is quiting.
   */
  def quit(reason: String) {
    if (pendingConflicts.isEmpty && pendingWorkers.isEmpty)
      this ! 'QUIT
    loop(react {
      case 'GO   => {}
      case 'CONFLICT =>
        pendingConflicts -= sender
        // if it is the first time it is a conflict, reply NO
        if (!(inConflict contains sender)) {
          sender ! 'NOCONFLICT
        }
        // if it was in conflict and I'm stronger, wait for traversal,
        else if (hashCode() > sender.hashCode()) pendingWorkers += sender
        // if it was in conflict and I'm weaker all is good.
        quit(reason)

      case (other: Str) =>
        strat merge other
        pendingWorkers -= sender
        quit(reason)

      case 'NOCONFLICT => {
        pendingConflicts -= sender
        quit(reason)
      }
      case 'QUIT =>
        debug("quiting - "+reason)
        exit(reason)
    })
  }


  // solve a round
  def act(): Nothing = react {
    case 'GO =>
      debug("go")
      // find a local solution
      if (strat.canSolve) {
        val sol = strat.solve
        if (sol.isDefined) {
          success(sol.get)
          quit("found a solution")
        }
      }
      // find nodes to expand to
      val next = strat.nextNodes
      if (next.isEmpty) {
        cleanLocks()
        quit("no solution, no expansion")
      }
      // claim new nodes
      val claimed = claim(next)
      if (claimed._1.isEmpty)
        nextRound()
      // send conflicts
      else {
        for (othernd <- claimed._2)
          safeSendConfl(othernd)
        inConflict ++= claimed._1
        nextRound()
      }

    case 'CONFLICT =>
      debug("got conflict")
      // check if conflict was also sent before
      if (!(inConflict contains sender)) {
        inConflict += sender
        sender ! 'CONFLICT // SAFE SEND? NO - IT HAS TO EXPECT A REPLY!
      }
      // I'm weaker...
      if (sender.hashCode() > this.hashCode()) {
        swapOwner(sender)
        sender ! strat
        quit("stronger message")
      }
      // I'm stronger! Wait for the new strategy...
      else {
        pendingWorkers += sender
        act()
      }

    case (other: Str) =>
      debug("got graph")
      pendingWorkers -= sender
      strat merge other
      act()

    case 'NOCONFLICT =>
      debug("got noconflict")
      inConflict -= sender
      act()
  }


  def debug(msg: String) {
    println("["+hashCode()+"] "+msg)
  }

}

object Worker {
  def apply[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]](node:Node[S,C],deployer: OutputChannel[Any], strat:Str) {
    val w = new Worker[S,C,Str](deployer,strat)
    w.work(node)
    w
  }
}

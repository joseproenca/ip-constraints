package workers

import actors.OutputChannel
import strategies.Strategy
import common.{Solution, CBuilder, Constraints}

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:36
 * To change this template use File | Settings | File Templates.
 */

class Worker[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
      (deployer: OutputChannel[Any], strat: Str)
      (implicit builder: CBuilder[S,C]) extends scala.actors.Actor {

  // type alias
  type Nd = Node[S,C]
  type Wk = Worker[S,C,Str]
  type ActorRef = OutputChannel[Any]

  // state
  private var inConflict = Set[ActorRef]()       // All conflicts sent
  private var pendingConflicts = Map[ActorRef,Set[Nd]]() // conflict sent, not received
  private var pendingWorkers = Set[ActorRef]()   // conflict received, acknowledged, and waiting for their graph
//  var locks = Set[ActorRef](this)      // conflict won: needed to release locks
  private var paused = false // true if no expansion exists, but it is waiting for conflicts or graphs


  def work(node: Node[S,C]): Boolean = {
    debug("starting node - "+node.connections.values.head.head._1)
    // ...
    val nodes = strat.initNodes(node)
    val claimed = claim(nodes)
    if (!claimed._1.isEmpty) {
      cleanLocks()
      debug("failed to start")
      false
    }
    else {
      start()
      this ! 'GO
      true
    }
  }

  def claim(nodes: Iterable[Nd]): (Iterable[ActorRef],Iterable[Nd]) = {
    // TODO: drop claims from the fringe! - DONE
    var confls = Set[ActorRef]()
    var possibleNodes = Set[Nd]()
    for (n <- nodes) {
      var other: Option[ActorRef] = None
      n.lock.acquire()
      //      n.synchronized {
              if (!n.owner.isDefined) {
                n.owner = Some(this)
              }
              else
                other = n.owner
      //      }
      n.lock.release()


      if (!other.isDefined)
        strat register n
      else
      if (!(confls contains other.get)) {
        confls += other.get
        possibleNodes += n
        // drop claims from the fringe to avoid claiming the same node again and again...
        strat.dropFromFringe(n)  // fringe -= n
      }
    }
    (confls,possibleNodes)
  }

  def cleanLocks() {
    for (nd <- strat.owned)
      nd.owner = None
    //      if (nd.owner.isDefined)
    //        if (locks contains nd.owner.get) // maybe unecessary...
  }
  def swapOwner(act: ActorRef) {
    for (nd <- strat.owned)
      nd.owner = Some(act)
  }

  def nextRound() {
    this ! 'GO
    act()
  }

  def success(sol:Option[S]) {
    debug("DONE\n"+sol)
    for (n <- strat.owned) {
      n.behaviour.update(sol)
      n.owner = None // cleaning locks (before any init)
    }
    for (n <- strat.owned)
      n.init

//    deployer ! 'DONE
  }

  // will not send if it is already in conflict, and syncrhonises to ensure the receiver is alive.
  def safeSendConfl(nd: Nd) {
    // TODO: put everything in the lock, and recheck the claimed actor that could have changed - DONE
    nd.lock.acquire()
    val other = nd.owner
    if (other.isDefined) {
      // case1: owner is already me (changed by weaker worker who wanted to quit)
      // case2: owner is weaker:
      //  case 2.1: conflict already sent and waiting for graph -> do nothing
      //  case 2.2: conflict already sent and graph received -> ERROR (I should have the lock already)
      //  case 2.3: conflict is new (and weaker) -> send conflict , wait for graph
      // case3: owner is stronger:
      //  case 3.1: conflict sent but graph not sent (pendingConflict) -> do nothing (graph sent later)
      //  case 3.2: conflict sent and graph sent (not pendingConflict) -> ERROR (always quit after sending graph)
      //  case 3.3: conflict is new (and stronger) -> send conflict and graph, then quit.
      ///////////////
      // RETHINK: avoid using other.get.hashCode for ranking here,
      // and decide if it is stronger or weaker only when receiving conflicts!
      // case1: owner is me -> do nothing
      // case2: conflict already sent and waiting for graph (pendingWorker) -> do nothing
      // case3: conflict already sent and waiting for their conflict (pendingConflict) -> do nothing
      // case4: conflict already sent, and not waiting for anything (merge complete) -> ERROR (owner should have changed)
      // case5: conflict not sent yet -> send conflict, wait for reply (pendingConflict)
      if (other.get != this) {
        if (inConflict contains other.get) {
          // temporary check - WRONG (could be an old pending conflict that is no longer pending)
//          assert((pendingConflicts contains other.get) || (pendingWorkers contains other.get),"old conflict not pending!")
          if (pendingConflicts contains other.get) {
            pendingConflicts += other.get -> (pendingConflicts(other.get) + nd)
          }
        }
        else {
          debug("sent conflict! - "+other.get.hashCode())
          other.get ! 'CONFLICT
          inConflict += other.get
          pendingConflicts += other.get -> Set(nd)
        }
      }
      nd.lock.release()
    }


//    val owner = nd.owner
//    if (owner.isDefined)
//      if (!(inConflict contains owner.get)) {
//        nd.lock.acquire()
////        nd.synchronized {
//          val act = nd.owner // this tile sync...
//          if (act.isDefined)
//            if (act.get.hashCode() != hashCode()) {
//              debug("sent conflict! - "+act.get.hashCode())
//              act.get ! 'CONFLICT
//              pendingConflicts += act.get
//            }
////        }
//         nd.lock.release()
//      }
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
    else debug("WAITING!! ["+reason+"] - conflicts: "+pendingConflicts.size+", missing workers: "+pendingWorkers.size)
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
//        strat merge other
        // need to free other nodes as well
        for (nd <- other.owned)
          nd.owner = None
        pendingWorkers -= sender
        quit(reason)

      case 'NOCONFLICT => {
        pendingConflicts -= sender
        quit(reason)
      }
      case 'QUIT =>
        debug("quiting - "+reason)
//        println("quiting - "+reason)
        if (reason.startsWith("found"))
          deployer ! 'SOLVED
        else
          deployer ! 'DONE
        exit(reason)
    })
  }


  // solve a round
  def act(): Nothing = react {
    case 'GO => gotGo

    case 'CONFLICT => gotConflict

    case (other: Str) => gotGraph(other)

    case 'NOCONFLICT => gotNoConflict
  }


  private def gotGo {
    debug("go")
    // find a local solution
    if (strat.canSolve) {
      debug("solving...")
      mark('.')
      val sol = strat.solve
      if (sol.isDefined) {
        success(sol)
        quit("found a solution")
      }
    }
    // find nodes to expand to
    debug("expanding...")
    val next = strat.nextNodes
    if (next.isEmpty) {
      // Wait for strategies or conflicts that can unstuck the expansion
      if (!(pendingConflicts.isEmpty && pendingWorkers.isEmpty)) {
        debug("waiting for pending (Conflicts/Workers) - "+pendingConflicts.keys.map(_.hashCode()).mkString(", ")+" / "
                                        +pendingWorkers.map(_.hashCode()).mkString(", "))
        paused = true
        act()
      }
      cleanLocks()
      for (n <- strat.owned)
        println(" - "+n.hashCode())
      for (n <- strat.fringe)
        println(" * "+n.hashCode())
      quit("no solution, no expansion")
    }
    // claim new nodes
    debug("claiming...")
    val claimed = claim(next)
    if (claimed._1.isEmpty)
      nextRound()
    // send conflicts
    else {
      debug("in conflict... - "+claimed._2.mkString("[",",","]"))
      for (othernd <- claimed._2)
        safeSendConfl(othernd)
      //        inConflict ++= claimed._1
      nextRound()
    }
  }

  private def gotConflict {
    debug("got conflict")
    val newConflict = !(inConflict contains sender)
    pendingConflicts -= sender
    // check if conflict was also sent before
    if (newConflict) {
      inConflict += sender
      sender ! 'CONFLICT // SAFE SEND? NO - IT HAS TO EXPECT A REPLY!
    }
    // I'm weaker...
    if (sender.hashCode() > this.hashCode()) {
      swapOwner(sender)
      rebuildFringeFromPending()
      sender ! strat
      mark('*')
      quit("stronger message")
    }
    // I'm stronger! Wait for the new strategy...
    else {
//      if (newConflict)
      pendingWorkers += sender
      act()
    }
  }

  private def rebuildFringeFromPending() {
    strat.restore2fringe
//    for (ns <- pendingConflicts.values; nd <- ns)
//      if (!(strat.owned contains nd)) strat add2fringe nd
  }

  private def gotGraph(other: Str) {
    debug("got graph")
    pendingWorkers -= sender
    strat merge other
    if (paused) {
      paused = false
      nextRound()
    }
    act()
  }

  private def gotNoConflict {
    // TODO: if a conflict is cancelled, the nodes need to be re-added to the fringe! - DONE
    debug("got noconflict")
    val oldPaused = paused
    if (pendingConflicts contains sender) {
      debug("## found pending conflicts")
      for (nd <- pendingConflicts(sender)) {
        if (!(strat.owned contains nd)) {
          debug("## re-added node to fringe: "+nd.hashCode())
          strat restore2fringe nd
          paused = false
        }
      }
      if (paused != oldPaused) { // if it changed the fringe, and turned off pending
        pendingConflicts -= sender
        nextRound()
      }
//      if (!(strat.owned contains pendingConflicts(sender))) {
//      }
      else {
        debug("## strategy already owned node... "+pendingConflicts(sender).hashCode())
        pendingConflicts -= sender
      }
    }
    act()
  }



  def debug(msg: String) {
//    println("["+hashCode()+"] "+msg)
  }

  def mark(msg: Char) {
    print(msg)
  }

}

object Worker {
  def apply[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
      (node:Node[S,C],deployer: OutputChannel[Any], strat:Str)
      (implicit builder: CBuilder[S,C]){
    val w = new Worker[S,C,Str](deployer,strat)
    w.work(node)
    w
  }
}

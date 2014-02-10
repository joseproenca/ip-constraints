package reopp.workers

import strategies.{Strategy, StrategyBuilder}
import reopp.common.{Constraints, Solution, CBuilder}
import reopp.common.Connector
import reopp.common.EmptySol
import scala.actors.Actor._
import java.util.concurrent.CountDownLatch


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:26
 * To change this template use File | Settings | File Templates.
 */

class Deployer[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
  (maxWorkers: Int)
  (implicit builder: CBuilder[S,C], sb: StrategyBuilder[S,C,Str])
  extends scala.actors.Actor {

  private val conflictManager = new ConflictManager(this) // self only used within actor behaviour.
  conflictManager.start

  val latch = new CountDownLatch(1)
  
//  val maxWorkers: Int
  var currentWorkers: Int = 0
  val pendingTasks = scala.collection.mutable.Queue[Node[S,C]]()
  private var tmpWorkers = List[scala.actors.Actor]()
//  private var counter = 0
  
  private var nodes: List[Node[S,C]] = Nil
  
  /** Creates a new worker (node), associated to this deployer.
   *  Keeps track of created nodes just to allow starting all of them in one go. */
  def add(con: => Connector[S,C]): Node[S,C] = {
    val res = Node[S,C](this, (uid:Int) => con )(builder)
    nodes ::= res
    res
  }

  /** Creates a new worker (node), associated to this deployer.
   *  Keeps track of created nodes just to allow starting all of them in one go.
   *  @param deps pairs of dependent port names, Used for hybrid strategy ([[strategies.HybridStrategy]]).
   *         For each (a,b), if 'a' is not on the border of the region, 'b' cannot be either.
   */
   def add(con: => Connector[S,C],deps: Iterable[(String,String)]): Node[S,C] = {
    val res = Node[S,C](this, deps, (uid:Int) => con )(builder)
    nodes ::= res
    res
  }

  /** Starts all nodes created with "add" in one go. */
//  def init() = for (n <- nodes) n.init
   def init() {
     debug("sending nodes to self?")
     for (n <- nodes) this ! n 
   }
  
  /** Checks if there are allowed workers and nodes ready to start, and create workers if needed.*/
  private def requestTasks() {
    debug(s"new worker? ($currentWorkers/$maxWorkers)"+(currentWorkers < maxWorkers)+"/"+ (!pendingTasks.isEmpty))
    if (currentWorkers < maxWorkers && !pendingTasks.isEmpty) {
      val nextTask = pendingTasks.dequeue
      if (nextTask.canStart) {
	      val w = new Worker[S,C,Str](self, conflictManager, sb.apply)
	      tmpWorkers ::= w
	      w.work(nextTask)
	      currentWorkers += 1
	      debug(s"added worker. Now: $currentWorkers (with ${pendingTasks.size} pending tasks)")
      }
      requestTasks()
    }
  }
  
  private def workerDone() {
    currentWorkers -= 1
//    println("worker done. current reopp.workers/pending tasks: "+currentWorkers+"/"+pendingTasks.size)
    requestTasks()
  }
  
  def act(): Nothing = {debug("waiting");self.react {
    // Sent by ConflictManager when a worker quits.
    case WorkerDone =>
      debugMsg("Worker done")
//      println("#######"+counter+"#######")
      workerDone()
      nextMessage()
    // Sent by nodes, indicating they are proactive (waiting to start). 
    case node: Node[S,C] => {
      debugMsg("Node requested task")
//      if (!(pendingTasks contains node))
      pendingTasks enqueue node
//      println(s"new node. pending tasks: ${pendingTasks.size}")
      requestTasks()
      act()
    }
    case Exit =>
      debug(s"exiting. workers: $currentWorkers, tasks: $pendingTasks")
      conflictManager ! Exit
      println(tmpWorkers.mkString(","))
      exit()
//      println(tmpWorkers.map(_.hashCode().toString.substring(5)).mkString(","))
//      for (w <- tmpWorkers) w ! 'STATUS // those alive will print their status...
//      exit()
  }}
  
  private def nextMessage() {
//	  requestTasks()
      if (currentWorkers > 0) {
    	debug(s"still has workers: $currentWorkers (with ${pendingTasks.size} pending tasks)")
        act()
      }
      else {
        debug("No more active workers")
        conflictManager ! Exit
        latch.countDown()
        exit()        
      }
  }
  
  private def debug(msg: String) {
//    println("[DEPL] "+msg)
  }
  private def debugMsg(msg:String) {
//    debug(s" <- [${sender.hashCode().toString.substring(5)}] $msg")
  }


}


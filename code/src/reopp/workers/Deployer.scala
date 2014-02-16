package reopp.workers

import strategies.{Strategy, StrategyBuilder}
import reopp.common.{Constraints, Solution, CBuilder}
import reopp.common.Connector
import reopp.common.EmptySol
import akka.actor.Actor
import java.util.concurrent.CountDownLatch
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import akka.event.Logging


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:26
 * To change this template use File | Settings | File Templates.
 */

class Deployer[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
  (maxWorkers: Int) //, conflictManager: ActorRef)
  (implicit builder: CBuilder[S,C], sb: StrategyBuilder[S,C,Str])
  extends Actor {

//  debug("before creating conflict manager")
  private val conflictManager: ActorRef =
  	context.actorOf(Props[ConflictManager], name = "conflictMng")
////new ConflictManager(this) // self only used within actor behaviour.
////  conflictManager.start
//  debug("after creating conflict manager")

  val latch = new CountDownLatch(1)
  
//  val maxWorkers: Int
  var currentWorkers: Int = 0
  val pendingTasks = scala.collection.mutable.Queue[Node[S,C]]()
  private var tmpWorkers = List[scala.actors.Actor]()
//  private var counter = 0
  

  
  /** Checks if there are allowed workers and nodes ready to start, and create workers if needed.*/
  private def requestTasks() {
    debug(s"new worker? ($currentWorkers/$maxWorkers)"+(currentWorkers < maxWorkers)+"/"+ (!pendingTasks.isEmpty))
    while (currentWorkers < maxWorkers && !pendingTasks.isEmpty) {
      val nextTask = pendingTasks.dequeue
      if (nextTask.canStart) {
//	      val w = new Worker[S,C,Str](conflictManager, sb.apply)
      	val w = context.actorOf(Worker.props[S,C,Str](conflictManager,sb.apply))
//	      tmpWorkers ::= w
	      w ! Claim(nextTask)
	      currentWorkers += 1
	      debug(s"added worker [${w.hashCode.toString.substring(5)}]. Now: $currentWorkers (with ${pendingTasks.size} pending tasks)")
      }
    }
  }
  
  private def workerDone() {
    currentWorkers -= 1
//    debug("worker done. current reopp.workers/pending tasks: "+currentWorkers+"/"+pendingTasks.size)
    requestTasks()
  }
  
  def receive = { //debug("waiting");self.react {
    // Sent by ConflictManager when a worker quits.
    case WorkerDone =>
      debugMsg("Worker done")
//      println("#######"+counter+"#######")
      workerDone()
      nextMessage()
    // Sent by nodes, indicating they are proactive (waiting to start). 
    case Task(node: Node[S,C]) => {
      debugMsg("Node requested task")
      pendingTasks enqueue node
//      debug(s"new node. pending tasks: ${pendingTasks.size}")
      requestTasks()
//      act()
    }
    case Status =>
      debug(s"exiting. workers: $currentWorkers, tasks: $pendingTasks")
      conflictManager ! Status
//      debug(tmpWorkers.mkString(","))
//      for (w <- tmpWorkers) w ! 'STATUS // those alive will print their status...
  }
  
  private def nextMessage() {
//	  requestTasks()
      if (currentWorkers > 0) {
        debug(s"still has workers: $currentWorkers (with ${pendingTasks.size} pending tasks)")
      }
      else {
        debug("No more active workers")
        // conflictManager ! Status
        // context.stop(self)
        context.system.shutdown
      }
  }
  
//  val log = Logging(context.system, this)

  private def debug(msg: String) {
//  	log.debug("[DDEEPPLL]"+msg)
//    println("[DEPL] "+msg)
  }
  private def debugMsg(msg:String) {
//    debug(s" <- [${sender.hashCode().toString.substring(5)}] $msg")
  }


}


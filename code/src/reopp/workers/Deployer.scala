package reopp.workers

import strategies.{Strategy, StrategyBuilder}
import reopp.common.{Constraints, Solution, CBuilder}
import reopp.common.Connector
import reopp.common.EmptySol


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
//  val maxWorkers: Int
  private var currentWorkers: Int = 0
  private val pendingTasks = scala.collection.mutable.Queue[Node[S,C]]()
//  private var tmpWorkers = List[scala.actors.Actor]()
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
  def init() = for (n <- nodes) n.init
  
  private def requestTasks() {
//    println("new worker? "+(currentWorkers < maxWorkers)+"/"+ (!pendingTasks.isEmpty))
    if (currentWorkers < maxWorkers && !pendingTasks.isEmpty) {
      val w = new Worker[S,C,Str](this, sb.apply)
//      tmpWorkers ::= w
      val started = w.work(pendingTasks.dequeue())
      if (started) {
        currentWorkers += 1
//        println(s"added worker. Now: $currentWorkers (with ${pendingTasks.size} pending tasks)")
      }
      requestTasks()
    }
  }
  
  private def workerDone() {
    currentWorkers -= 1
//    println("worker done. current reopp.workers/pending tasks: "+currentWorkers+"/"+pendingTasks.size)
    requestTasks()
  }
  
  def act(): Nothing = react {
    // Sent by nodes when a new step is found.
    case 'SOLVED =>
//      counter += 1
//      println("#######"+counter+"#######")
      workerDone()
      nextMessage()
    // Sent by nodes, indicating they are proactive (waiting to start). 
    case node: Node[S,C] =>
//      if (!(pendingTasks contains node))
        pendingTasks enqueue node
//      println(s"new node. pending tasks: ${pendingTasks.size}")
      requestTasks()
      act()
    case 'DONE =>
//      println("#####--"+counter+"--#####")
      workerDone()
      nextMessage()
    case 'STATUS =>
      println(s"workers: $currentWorkers, tasks: $pendingTasks")
//      println(tmpWorkers.map(_.hashCode().toString.substring(5)).mkString(","))
//      for (w <- tmpWorkers) w ! 'STATUS // those alive will print their status...
//      exit()
  }
  
  private def nextMessage() {
//	  requestTasks()
      if (currentWorkers > 0) {
//    	println(s"still has workers: $currentWorkers (with ${pendingTasks.size} pending tasks)")
        act()
      }
      else {
//        println("No more active... workers")
        exit()        
      }
  }
}


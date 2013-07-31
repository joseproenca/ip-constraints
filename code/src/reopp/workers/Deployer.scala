package reopp.workers

import strategies.{Strategy, StrategyBuilder}
import reopp.common.{Constraints, Solution, CBuilder}


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
  var currentWorkers: Int = 0
  val pendingTasks = scala.collection.mutable.Queue[Node[S,C]]()
  var counter = 0
  
  def requestTasks() {
//    println("new worker? "+(currentWorkers < maxWorkers)+"/"+ (!pendingTasks.isEmpty))
    if (currentWorkers < maxWorkers && !pendingTasks.isEmpty) {
      val w = new Worker[S,C,Str](this, sb.apply)
      val started = w.work(pendingTasks.dequeue())
      if (started) {
        currentWorkers += 1
//        println("added worker. current reopp.workers/pending tasks: "+currentWorkers+"/"+pendingTasks.size)
      }
      requestTasks()
    }
  }
  
  def workerDone() {
    currentWorkers -= 1
//    println("worker done. current reopp.workers/pending tasks: "+currentWorkers+"/"+pendingTasks.size)
    requestTasks()
  }
  
  def act(): Nothing = react {
    case 'SOLVED =>
      counter += 1
//      println("#######"+counter+"#######")
      workerDone()
      act()
    case node: Node[S,C] =>
//      println("new node")
//      if (!(pendingTasks contains node))
        pendingTasks enqueue node
      requestTasks()
      act()
    case 'DONE =>
//      println("#####--"+counter+"--#####")
      workerDone()
      act()
  }
}


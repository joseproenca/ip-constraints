package workers

import common.beh.{Solution, Constraints}


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:26
 * To change this template use File | Settings | File Templates.
 */

class Deployer[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str], SB<:StrategyBuilder[S,C,Str]]
  (maxWorkers: Int, sb: SB)
  extends scala.actors.Actor {
//  val maxWorkers: Int
  var currentWorkers: Int = 0
  val pendingTasks = scala.collection.mutable.Queue[Node[S,C]]()
  
  def requestTasks() {
    if (currentWorkers < maxWorkers && !pendingTasks.isEmpty) {
      val w = new Worker[S,C,Str](this, sb.apply)
      w.work(pendingTasks.dequeue())
      currentWorkers += 1
      requestTasks
    }
  }
  
  def workerDone() {
    currentWorkers -= 1
    requestTasks
  }
  
  def act: Nothing = react {
    case 'DONE => workerDone(); act
    case node: Node[S,C] => pendingTasks enqueue node; requestTasks(); act
  }
}

